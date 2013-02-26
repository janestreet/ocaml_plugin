open Core.Std
open Async.Std

type filename = string with sexp, compare

let parallel list ~f =
  Deferred.Or_error.List.iter ~how:`Parallel list ~f

module Digest : sig
  type t with compare, sexp
  val file : filename -> t Deferred.Or_error.t
  val to_string : t -> string
end = struct
  type t = string with compare, sexp
  let to_string t = t

  let file filename =
    let fct () =
      let raw = Digest.file filename in
      Digest.to_hex raw
    in
    Shell.Deferred.Or_error.try_with ~extract_exn:true (fun () -> In_thread.run fct)
end

module Key = struct
  module T = struct
    type t = filename list with sexp, compare
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make (T)
end

module Sources = struct
  type elt = filename * Digest.t with sexp, compare
  type t = elt list with sexp, compare
  let (=) a b = compare_t a b = 0
  let files t = t
  let key t = List.map ~f:fst t
end

module Plugin = struct
  type t = {
    cmxs_filename : filename; (* invariant: absolute *)
    sources : Sources.t;
    plugin_uuid : Plugin_uuid.t;
  } with fields, sexp

  let clean t =
    Shell.rm ~f:() [ t.cmxs_filename ]
end

module Info : sig
  type t

  val create :
    plugins: Plugin.t list
    -> unit
    -> t

  val plugins : t -> Plugin.t list

  val empty : t

  val load : dir:string -> t Deferred.Or_error.t
  val save : dir:string -> t -> unit Deferred.Or_error.t

  val cache_dir : string
  val cache_dir_perm : int
end = struct

  type t = {
    plugins : Plugin.t list;
  } with sexp, fields

  let create
      ~plugins
      () =
    {
      plugins;
    }

  let empty = create
    ~plugins:[]
    ()

  let cache_dir = "cmxs-cache"
  let info_file = "cache-info.sexp"

  let cache_dir_perm = 0o755
  let cache_files_perm = 0o644

  let info_file_pathname dir = dir ^/ cache_dir ^/ info_file

  let save ~dir t =
    Shell.Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      Writer.save_sexp ~perm:cache_files_perm (info_file_pathname dir) (sexp_of_t t)
    )

  let load ~dir =
    let pathname = info_file_pathname dir in
    (* do no check for `Read there, let the reader fail in that case *)
    Unix.access pathname [ `Exists ] >>= function
    | Ok () -> begin
      Shell.Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Reader.load_sexp_exn ~exclusive:true pathname t_of_sexp
      )
    end
    | Error _ ->
      Deferred.return (Ok empty)
end

module Config = struct
  module V1 = struct
    type t = {
      dir: string;
      max_files: int sexp_option;
      readonly: bool;
    } with fields, sexp
  end
  module V2 = struct
    type t = {
      dir: string;
      max_files: int with default(10);
      readonly: bool with default(false);
    } with fields, sexp
    let of_prev v1 =
      {
        dir = v1.V1.dir;
        max_files = Option.value v1.V1.max_files ~default:10;
        readonly = v1.V1.readonly;
      }
  end
  module V = struct
    type t =
    | V1 of V1.t
    | V2 of V2.t
    with sexp

    let to_current = function
      | V1 v1 -> V2.of_prev v1
      | V2 v2 -> v2
  end

  include V2

  let t_of_sexp sexp =
    let v =
      try V.t_of_sexp sexp with _ -> V.V1 (V1.t_of_sexp sexp)
    in
    V.to_current v

  let sexp_of_t t = V.sexp_of_t (V.V2 t)

  let create
      ~dir
      ?(max_files=10)
      ?(readonly=false)
      () =
    {
      dir;
      max_files;
      readonly;
    }

end

module State = struct

  type t = {
    config : Config.t;
    mutable index : int;
    mutable old_cache_with_new_exec : bool;
    mutable old_files_deleted : bool;
    table : (int * Plugin.t) Key.Table.t;
    deprecated_plugins : Plugin.t Queue.t;
  }

  let del_cmxs path filename =
    if Filename.check_suffix filename "cmxs" then
      Shell.rm ~f:() [ path ^/ filename ]
    else
      Deferred.Or_error.return ()

  let load_info t =
    let config_dir = Config.dir t.config in
    let refresh_behavior info =
      if Config.readonly t.config
      then
        Deferred.return (Ok ())
      else begin
        Info.save ~dir:config_dir Info.empty >>=? fun () ->
        parallel ~f:Plugin.clean (Info.plugins info) >>=? fun () ->
        let cache_dir = config_dir ^/ Info.cache_dir in
        Sys.readdir cache_dir >>= fun files ->
        Deferred.Or_error.List.iter (Array.to_list files) ~f:(del_cmxs cache_dir)
      end
    in
    Info.load ~dir:config_dir >>= function
    | Error _ ->
      (* the info could not be read, we clean the possibly existing cmxs files and then
         proceed as if there was no cache *)
      refresh_behavior Info.empty
    | Ok info ->
      t.old_cache_with_new_exec <- true;
      (* filtering the plugin if the file is available *)
      let iter plugin =
        Unix.access (Plugin.cmxs_filename plugin) [ `Exists ; `Read ] >>= function
        | Ok () ->
          let index = t.index in
          t.index <- succ index;
          let sources = Plugin.sources plugin in
          let key = Sources.key sources in
          Key.Table.replace t.table ~key ~data:(index, plugin);
          Deferred.return ()
        | Error _ ->
          Deferred.return ()
      in
      Deferred.List.iter ~f:iter (Info.plugins info) >>| fun () ->
      Ok ()

  let create config =
    let table = Key.Table.create () in
    let index = 0 in
    Shell.absolute_pathname (Config.dir config) >>=? fun config_dir ->
    let deprecated_plugins = Queue.create () in
    let config = { config with Config.dir = config_dir } in
    let old_cache_with_new_exec = false in
    let old_files_deleted = false in
    let state = {
      index;
      old_cache_with_new_exec;
      old_files_deleted;
      config;
      table;
      deprecated_plugins;
    } in
    load_info state >>=? fun () ->
    Deferred.return (Ok state)

  let info t =
    let plugins = Key.Table.data t.table in
    let plugins = List.rev_map ~f:snd plugins in
    Info.create ~plugins ()

  let save_info t =
    Info.save ~dir:(Config.dir t.config) (info t)

  let clean_old t =
    let max_files = Config.max_files t.config in
    let cut = t.index - max_files in
    let to_clean = ref [] in
    let clean plugin = to_clean := plugin :: ! to_clean in
    let f (index, plugin) =
      if index < cut then (clean plugin; false) else true
    in
    Key.Table.filter_inplace t.table ~f;
    Queue.iter ~f:clean t.deprecated_plugins;
    parallel !to_clean ~f:Plugin.clean >>=? fun () ->
    (* clean other old cmxs files that are no longer referenced by the info
       needs to be done only once *)
    if t.old_files_deleted then Deferred.Or_error.return ()
    else begin
      let current_cmxs_basename =
        let basenames = List.rev_map (Key.Table.data t.table) ~f:(fun (_, plugin) ->
          Filename.basename (Plugin.cmxs_filename plugin)
        ) in
        String.Hash_set.of_list basenames
      in
      let cache_dir = Config.dir t.config ^/ Info.cache_dir in
      Sys.readdir cache_dir >>= fun files ->
      Deferred.Or_error.List.iter (Array.to_list files) ~f:(fun file ->
        if not (Hash_set.mem current_cmxs_basename file)
        then
          del_cmxs cache_dir file
        else
          Deferred.Or_error.return ()
      ) >>|? fun r ->
      t.old_files_deleted <- true;
      r
    end

  let find t sources =
    let key = Sources.key sources in
    let table = t.table in
    match Key.Table.find table key with
    | Some (_, plugin) ->
      let plugin_sources = Plugin.sources plugin in
      if Sources.(=) plugin_sources sources
      then
        Some plugin
      else (
        (*
          this cache is now invalid, we should remove the file
          but this is very likely that a 'add' will be called,
          and we prefer writing the info file only once
        *)
        if not (Config.readonly t.config) then (
          Queue.enqueue t.deprecated_plugins plugin;
        );
        Key.Table.remove table key;
        None
      )
    | None ->
      None

  let add t sources plugin_uuid filename =
    if Config.readonly t.config
    then Deferred.return (Ok ())
    else begin
      let key = Sources.key sources in
      let dir = Config.dir t.config ^/ Info.cache_dir in
      Shell.mkdir_p ~perm:Info.cache_dir_perm dir >>=? fun () ->
      let uuid = Plugin_uuid.uuid plugin_uuid in
      let cmxs_filename = dir ^/ (Uuid.to_string uuid)^".cmxs" in
      Shell.cp ~source:filename ~dest:cmxs_filename >>=? fun () ->
      Shell.chmod cmxs_filename ~perm:Info.cache_dir_perm >>=? fun () ->
      let plugin = {
        Plugin.
        sources;
        plugin_uuid;
        cmxs_filename;
      } in
      let index = t.index in
      t.index <- succ index;
      Key.Table.set t.table ~key ~data:(index, plugin);
      clean_old t >>=? fun () ->
      save_info t
    end
end

include State

let filenames_from_ml_bundles lst =
  let f x = match Ml_bundle.to_pathnames x with
    | `ml ml -> [ ml ]
    | `pair (`ml ml, `mli mli) -> [ml ; mli]
  in List.concat_map lst ~f

let digest files =
  let files = filenames_from_ml_bundles files in
  Deferred.Or_error.List.map ~how:`Parallel files ~f:(fun file ->
    Digest.file file >>|? (fun digest -> file, digest)
  )

let old_cache_with_new_exec t = t.old_cache_with_new_exec
