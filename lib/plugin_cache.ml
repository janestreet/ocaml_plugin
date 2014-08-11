open Core.Std
open Async.Std
open Import

module Build_info : sig
  type t with sexp
  val equal : t -> t -> bool
  val current : t
end = struct
  type t = Sexp.t with sexp
  let current = Params.build_info_as_sexp
  let equal = Sexp.equal
end

type filename = string with sexp, compare

let parallel list ~f =
  Deferred.Or_error.List.iter ~how:`Parallel list ~f
;;

module Digest : sig
  type t with compare, sexp
  include Stringable with type t := t
  val file : filename -> t Deferred.Or_error.t
  val string : string -> t
end = struct
  include String

  let to_hex fct arg = Digest.to_hex (fct arg)
  ;;

  let file arg =
    let fct () = to_hex Digest.file arg in
    Deferred.Or_error.try_with ~extract_exn:true (fun () -> In_thread.run fct)
  ;;

  let string arg = to_hex Digest.string arg
  ;;
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
  ;;

  let key t = List.map ~f:fst t
  ;;
end

module Plugin = struct
  type t = {
    cmxs_filename : filename; (* invariant: absolute *)
    sources : Sources.t;
    plugin_uuid : Plugin_uuid.t;
  } with fields, sexp

  let clean t =
    Shell.rm ~f:() [ t.cmxs_filename ]
  ;;
end

module Info : sig
  type t with sexp_of

  val create :
    plugins: Plugin.t list
    -> unit
    -> t

  val plugins : t -> Plugin.t list
  val build_info : t -> Build_info.t

  val empty : t
  val is_empty : t -> bool

  val info_file_pathname : dir:string -> string
  val load : dir:string -> t Deferred.Or_error.t
  val save : dir:string -> t -> unit Deferred.Or_error.t

  val cache_dir : string
  val cache_dir_perm : int
end = struct

  type t = {
    version: Sexp.t;
    build_info : Build_info.t;
    plugins : Plugin.t list;
  } with sexp, fields

  let create
      ~plugins
      () =
    let version = sexp_of_string Params.version in
    let build_info = Build_info.current in
    {
      version;
      build_info;
      plugins;
    }
  ;;

  let empty = create
    ~plugins:[]
    ()
  ;;

  let is_empty t = List.is_empty t.plugins
  ;;

  let cache_dir = "cmxs-cache"
  let info_file = "cache-info.sexp"
  ;;

  let cache_dir_perm   = 0o755
  let cache_files_perm = 0o644
  ;;

  let info_file_pathname ~dir = dir ^/ cache_dir ^/ info_file
  ;;

  let save ~dir t =
    Deferred.Or_error.try_with ~extract_exn:true (fun () ->
      Writer.save_sexp ~perm:cache_files_perm (info_file_pathname ~dir) (sexp_of_t t)
    )
  ;;

  let load ~dir =
    let pathname = info_file_pathname ~dir in
    (* do no check for `Read there, let the reader fail in that case *)
    Unix.access pathname [ `Exists ] >>= function
    | Ok () -> begin
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Reader.load_sexp_exn ~exclusive:true pathname t_of_sexp
      )
    end
    | Error _ ->
      Deferred.return (Ok empty)
  ;;
end

module Config = struct
  let try_old_cache_with_new_exec_default = not Params.build_info_available

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
      try_old_cache_with_new_exec : bool with default(try_old_cache_with_new_exec_default);
    } with fields, sexp
    let of_prev v1 =
      {
        dir = v1.V1.dir;
        max_files = Option.value v1.V1.max_files ~default:10;
        readonly = v1.V1.readonly;
        try_old_cache_with_new_exec = try_old_cache_with_new_exec_default;
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
  ;;

  let sexp_of_t t = V.sexp_of_t (V.V2 t)
  ;;

  let create
      ~dir
      ?(max_files=10)
      ?(readonly=false)
      ?(try_old_cache_with_new_exec=try_old_cache_with_new_exec_default)
      () =
    {
      dir;
      max_files;
      readonly;
      try_old_cache_with_new_exec;
    }
  ;;
end

module State = struct

  type t = {
    config : Config.t;
    mutable has_write_lock : bool;
    mutable index : int;
    mutable old_cache_with_new_exec : bool;
    (* This field is true when the build info of the cache doesn't match and the config
       allows this option. But if the cache has the right build info, then this field
       stays false so that we don't try recompiling needlessly. *)
    mutable old_files_deleted : bool;
    table : (int * Plugin.t) Key.Table.t;
    (* At any given time (except maybe on startup and when the cache is readonly), the
       index in the table should be in [index - maxfiles, index[. [clean_old] is the one
       removing the old cmxs' from the table and from the filesystem. *)
    deprecated_plugins : Plugin.t Queue.t;
  }

  exception Read_only_info_file_exists_but_cannot_be_read_or_parsed of
      string * Error.t with sexp

  let del_cmxs path filename =
    if Filename.check_suffix filename "cmxs" then
      Shell.rm ~f:() [ path ^/ filename ]
    else
      Deferred.Or_error.return ()
  ;;

  let info t =
    let plugins = Key.Table.data t.table in
    let plugins = List.rev_map ~f:snd plugins in
    Info.create ~plugins ()
  ;;

  let save_info t =
    Info.save ~dir:(Config.dir t.config) (info t)
  ;;

  let lock_filename t =
    let config_dir = Config.dir t.config in
    config_dir ^/ Info.cache_dir ^ ".lock"
  ;;

  (* this lock is taken only if we actually need to modify the info. it is cleaned by the
     Nfs lock library at exit *)
  let take_write_lock t =
    if t.has_write_lock then Deferred.Or_error.return ()
    else
      let lock_filename = lock_filename t in
      Lock_file.Nfs.create lock_filename >>|? fun () ->
      t.has_write_lock <- true;
  ;;

  let load_info t =
    let config_dir = Config.dir t.config in
    let reset_cache_if_writable info =
      if_ (not (Config.readonly t.config)) (fun () ->
        let dir = Config.dir t.config ^/ Info.cache_dir in
        Shell.mkdir_p ~perm:Info.cache_dir_perm dir >>=? fun () ->
        take_write_lock t >>=? fun () ->
        Info.save ~dir:config_dir Info.empty >>=? fun () ->
        parallel ~f:Plugin.clean (Info.plugins info) >>=? fun () ->
        let cache_dir = config_dir ^/ Info.cache_dir in
        Shell.readdir cache_dir >>=? fun files ->
        Deferred.Or_error.List.iter (Array.to_list files) ~f:(del_cmxs cache_dir)
      )
    in
    Info.load ~dir:config_dir >>= function
    | Error error ->
      (* the info file exists but could not be read or could not be parsed *)
      if Config.readonly t.config then
        (* if the config is readonly, then we would rather make an error instead of
           working as if there was no cache, because applications could become slow all
           of a sudden, for reasons that could be hard to debug. *)
        Deferred.Or_error.of_exn
          (Read_only_info_file_exists_but_cannot_be_read_or_parsed
             (Info.info_file_pathname ~dir:config_dir, error))
      else
        (* If it is a permissions error, recreating the info file will fail and we will
           get a proper error. If it is a parsing error, we will just clean the possibly
           existing cmxs files and then proceed as if there was no cache. *)
        reset_cache_if_writable Info.empty
    | Ok info ->
      if (Params.build_info_available
          && Build_info.equal Build_info.current (Info.build_info info))
        || Info.is_empty info
        || (
          if Config.try_old_cache_with_new_exec t.config
          then (t.old_cache_with_new_exec <- true ; true)
          else false
        )
      then
        (* filtering the plugin if the file is available *)
        let iter plugin =
          Unix.access (Plugin.cmxs_filename plugin) [ `Exists ; `Read ] >>= function
          | Ok () ->
            let index = t.index in
            t.index <- succ index;
            let sources = Plugin.sources plugin in
            let key = Sources.key sources in
            Key.Table.set t.table ~key ~data:(index, plugin);
            Deferred.return ()
          | Error _ ->
            Deferred.return ()
        in
        Deferred.List.iter ~f:iter (Info.plugins info) >>| fun () ->
        Ok ()
      else
        reset_cache_if_writable info
  ;;

  let create config =
    let table = Key.Table.create () in
    let index = 0 in
    Shell.absolute_pathname (Config.dir config) >>=? fun config_dir ->
    let deprecated_plugins = Queue.create () in
    let config = { config with Config.dir = config_dir } in
    let old_cache_with_new_exec = false in
    let old_files_deleted = false in
    let has_write_lock = false in
    let state = {
      index;
      old_cache_with_new_exec;
      old_files_deleted;
      config;
      has_write_lock;
      table;
      deprecated_plugins;
    } in
    load_info state >>=? fun () ->
    Deferred.return (Ok state)
  ;;

  let clean_old t =
    assert (not (Config.readonly t.config));
    let max_files = Config.max_files t.config in
    let cut = t.index - max_files in
    let to_clean = ref [] in
    let clean plugin = to_clean := plugin :: ! to_clean in
    let f (index, plugin) =
      if index < cut then (clean plugin; false) else true
    in
    Key.Table.filter_inplace t.table ~f;
    Queue.iter ~f:clean t.deprecated_plugins;
    Queue.clear t.deprecated_plugins;
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
      Shell.readdir cache_dir >>=? fun files ->
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
  ;;

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
        (* This cache is now invalid, we should remove the file but this is very likely
           that a 'add' will be called, and we prefer writing the info file only once.
           We can't add the same plugin several times in the queue because it is removed
           from the hashtbl. If the config is read only, the file will stay in the
           queue. *)
        if not (Config.readonly t.config) then (
          Queue.enqueue t.deprecated_plugins plugin;
        );
        Key.Table.remove table key;
        None
      )
    | None ->
      None
  ;;

  let add t sources plugin_uuid filename =
    if_ (not (Config.readonly t.config)) (fun () ->
      let key = Sources.key sources in
      let dir = Config.dir t.config ^/ Info.cache_dir in
      Shell.mkdir_p ~perm:Info.cache_dir_perm dir >>=? fun () ->
      take_write_lock t >>=? fun () ->
      let uuid = Plugin_uuid.uuid plugin_uuid in
      let cmxs_filename = dir ^/ (Uuid.to_string uuid) ^ ".cmxs" in
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
    )
  ;;

  let clean t =
    let had_lock = t.has_write_lock in
    t.has_write_lock <- false;
    if_ had_lock (fun () -> Lock_file.Nfs.unlock (lock_filename t))
  ;;
end

include State

let filenames_from_ml_bundles lst =
  let f x =
    let `ml ml, `mli opt_mli, `module_name _ = Ml_bundle.to_pathnames x in
    ml :: Option.to_list opt_mli
  in List.concat_map lst ~f
;;

let digest files =
  let files = filenames_from_ml_bundles files in
  Deferred.Or_error.List.map ~how:`Parallel files ~f:(fun file ->
    Digest.file file >>|? (fun digest -> file, digest)
  )
;;

let old_cache_with_new_exec t = t.old_cache_with_new_exec
;;
