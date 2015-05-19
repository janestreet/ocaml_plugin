open Core.Std
open Async.Std
open Import

module Build_info : sig
  type t with sexp
  val is_current    : t -> bool
  val current       : t
  val not_available : t
end = struct
  type t = Sexp.t with sexp
  let not_available = <:sexp_of< string >> "(not available)"
  let current = Params.build_info_as_sexp
  let is_current a = Sexp.equal current a
end

type filename = string with sexp, compare
type basename = string with sexp, compare

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
    type t = (basename * Digest.t) list with sexp, compare
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make (T)

  let of_sources sources =
    List.map sources ~f:(fun (filename, digest) -> Filename.basename filename, digest)
  ;;
end

module Sources = struct
  type t = (filename * Digest.t) list with sexp
end

module Plugin = struct
  type t =
    { cmxs_filename : filename (* invariant: absolute *)
    ; sources       : Sources.t
    ; plugin_uuid   : Plugin_uuid.t
    ; build_info    : Build_info.t with default(Build_info.not_available)
    }
  with fields, sexp

  let t_of_sexp = Sexp.of_sexp_allow_extra_fields t_of_sexp

  let clean t =
    Shell.rm ~f:() [ t.cmxs_filename ]
  ;;

  let was_compiled_by_current_exec t =
    Build_info.is_current t.build_info
  ;;
end

module Make_count_by (M : sig
    val of_sources : Sources.t -> string list
  end) : sig

  module Key : sig
    type t
    val of_sources : Sources.t -> t
  end

  type t

  val create : unit -> t

  val incr : t -> Key.t -> unit
  val decr : t -> Key.t -> unit
  val find : t -> Key.t -> int
end = struct

  module Key = struct
    module T = struct
      type t = string list with sexp, compare
      let hash = Hashtbl.hash
    end
    include T
    include Hashable.Make (T)

    let of_sources = M.of_sources
  end

  type t = int Key.Table.t

  let create () = Key.Table.create ()

  let incr table key =
    Hashtbl.change table key (fun value ->
      Some (succ (Option.value value ~default:0)))
  ;;

  let decr table key =
    Hashtbl.change table key (function
      | None -> None
      | Some x when x <= 1 -> None
      | Some x -> Some (pred x))
  ;;

  let find table key =
    Option.value (Hashtbl.find table key) ~default:0
  ;;
end

module Count_by_basenames = Make_count_by(struct
    let of_sources sources =
      List.map sources ~f:(fun (filename, _digest) ->
        Filename.basename filename)
    ;;
  end)

module Count_by_filenames = Make_count_by(struct
    let of_sources sources = List.map sources ~f:fst
  end)
;;

module Info : sig
  type t with sexp_of

  val create :
    plugins: Plugin.t list
    -> unit
    -> t

  val plugins : t -> Plugin.t list

  val empty : t

  val info_file_pathname : dir:string -> string
  val load : dir:string -> t Deferred.Or_error.t
  val save : dir:string -> t -> unit Deferred.Or_error.t

  val cache_dir : string
  val cache_dir_perm : int
end = struct

  type t =
    { version    : Sexp.t
    ; build_info : Sexp.t
    ; plugins    : Plugin.t list
    }
  with sexp, fields

  let t_of_sexp = Sexp.of_sexp_allow_extra_fields t_of_sexp

  let create ~plugins () =
    { version    = sexp_of_string Params.version
    ; build_info = Params.build_info_as_sexp
    ; plugins
    }
  ;;

  let empty = create ~plugins:[] ()
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
        Reader.load_sexp_exn pathname t_of_sexp
      )
    end
    | Error _ ->
      Deferred.return (Ok empty)
  ;;
end

module Config = struct
  let try_old_cache_with_new_exec_default = false

  module V1 = struct
    type t =
      { dir       : string
      ; max_files : int sexp_option
      ; readonly  : bool
      }
    with fields, sexp
  end
  module V2 = struct
    type t =
      { dir       : string
      ; max_files : int with default(10)
      ; readonly  : bool with default(false)
      ; try_old_cache_with_new_exec : bool
        with default(try_old_cache_with_new_exec_default)
      }
    with fields, sexp
    let t_of_sexp = Sexp.of_sexp_allow_extra_fields t_of_sexp
    let of_prev v1 =
      { dir = v1.V1.dir
      ; max_files = Option.value v1.V1.max_files ~default:10
      ; readonly = v1.V1.readonly
      ; try_old_cache_with_new_exec = try_old_cache_with_new_exec_default
      }
    ;;
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
    { dir
    ; max_files
    ; readonly
    ; try_old_cache_with_new_exec
    }
  ;;
end

module State = struct

  module Plugin_in_table = struct
    type t =
      { creation_num : int
      ; plugin       : Plugin.t
      ; basenames    : Count_by_basenames.Key.t
      ; filenames    : Count_by_filenames.Key.t
      ; key          : Key.t
      }
    with fields

    let older_first t1 t2 = Int.compare t1.creation_num t2.creation_num
  end

  type t =
    { config                       : Config.t
    ; mutable has_write_lock       : bool
    ; mutable next_creation_num    : int
    ; mutable old_files_deleted    : bool
    ; table                        : Plugin_in_table.t Key.Table.t
    ; num_plugins_by_basenames     : Count_by_basenames.t
    ; num_plugins_by_filenames     : Count_by_filenames.t
    }

  exception Read_only_info_file_exists_but_cannot_be_read_or_parsed of
      string * Error.t with sexp

  let there_is_more_plugins_with_same what t p1 p2 =
    let num_plugins_with_same what t (p : Plugin_in_table.t) =
      match what with
      | `Basenames -> Count_by_basenames.find t.num_plugins_by_basenames p.basenames
      | `Filenames -> Count_by_filenames.find t.num_plugins_by_filenames p.filenames
    in
    Int.compare
      (num_plugins_with_same what t p2)
      (num_plugins_with_same what t p1)
  ;;

  let priority_heuristic_to_clean_plugins t =
    Comparable.lexicographic
      [ there_is_more_plugins_with_same `Filenames t
      ; there_is_more_plugins_with_same `Basenames t
      ; Plugin_in_table.older_first
      ]
  ;;

  let remove_internal t key =
    match Hashtbl.find_and_remove t.table key with
    | None -> ()
    | Some plugin ->
      Count_by_basenames.decr t.num_plugins_by_basenames plugin.basenames;
      Count_by_filenames.decr t.num_plugins_by_filenames plugin.filenames;
  ;;

  let get_and_clear_plugins_to_remove t =
    (* In practice this function is called each time we add a new plugin, so as soon as we
       reach the max capacity, which means the loop runs only once, in O(n).  In rare
       cases this might run several times in case a old plugin cache was loaded after
       decreasing the max values.  We tried as well an approach where we maintain a heap
       on addition incrementally but the code was more complex. *)
    let get_one t =
      let fold_data table ~init ~f =
        Hashtbl.fold table ~init ~f:(fun ~key:_ ~data acc -> f acc data)
      in
      Container.fold_min fold_data t.table
        ~cmp:(priority_heuristic_to_clean_plugins t)
    in
    let max_plugins = max 0 t.config.max_files in
    let rec loop acc =
      if Hashtbl.length t.table <= max_plugins
      then acc
      else
        match get_one t with
        | None -> acc
        | Some plugin ->
          remove_internal t plugin.key;
          loop (plugin :: acc)
    in
    loop []
  ;;

  let add_plugin_internal t (plugin : Plugin.t) =
    let sources = plugin.sources in
    let key = Key.of_sources sources in
    let basenames = Count_by_basenames.Key.of_sources sources in
    let filenames = Count_by_filenames.Key.of_sources sources in
    let plugin_in_table : Plugin_in_table.t =
      let creation_num = t.next_creation_num in
      t.next_creation_num <- succ creation_num;
      { plugin
      ; creation_num
      ; basenames
      ; filenames
      ; key
      }
    in
    remove_internal t key;
    Hashtbl.set t.table ~key ~data:plugin_in_table;
    Count_by_basenames.incr t.num_plugins_by_basenames basenames;
    Count_by_filenames.incr t.num_plugins_by_filenames filenames;
  ;;

  let del_cmxs path filename =
    if Filename.check_suffix filename "cmxs" then
      Shell.rm ~f:() [ path ^/ filename ]
    else
      Deferred.Or_error.return ()
  ;;

  let info t =
    let plugins =
      (* When creating the info the list is ordered from old to new so that when we
         deserialize the info, [creation_num] fields are assigned in the same order then
         before serialization. *)
      Hashtbl.data t.table
      |> List.sort ~cmp:Plugin_in_table.older_first
      |> List.map ~f:Plugin_in_table.plugin
    in
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
      (* filtering the plugin if the file is available *)
      let iter plugin =
        Unix.access (Plugin.cmxs_filename plugin) [ `Exists ; `Read ] >>| function
        | Ok ()   -> add_plugin_internal t plugin;
        | Error _ -> ()
      in
      Deferred.List.iter ~f:iter (Info.plugins info) >>| fun () ->
      Ok ()
  ;;

  let create (config : Config.t) =
    Shell.absolute_pathname (Config.dir config) >>=? fun config_dir ->
    let state =
      { config                   = { config with dir = config_dir }
      ; has_write_lock           = false
      ; next_creation_num        = 0
      ; old_files_deleted        = false
      ; table                    = Key.Table.create ()
      ; num_plugins_by_basenames = Count_by_basenames.create ()
      ; num_plugins_by_filenames = Count_by_filenames.create ()
      }
    in
    load_info state >>=? fun () ->
    Deferred.return (Ok state)
  ;;

  let clean_old t =
    assert (not (Config.readonly t.config));
    parallel (get_and_clear_plugins_to_remove t) ~f:(fun plugin_in_table ->
      Plugin.clean plugin_in_table.plugin)
    >>=? fun () ->
    (* clean other old cmxs files that are no longer referenced by the info
       needs to be done only once *)
    if t.old_files_deleted then Deferred.Or_error.return ()
    else begin
      let current_cmxs_basename =
        let basenames = List.rev_map (Hashtbl.data t.table) ~f:(fun plugin ->
          Filename.basename (Plugin.cmxs_filename (Plugin_in_table.plugin plugin))
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
    match Hashtbl.find t.table (Key.of_sources sources) with
    | None -> None
    | Some { plugin; _ } ->
      if Plugin.was_compiled_by_current_exec plugin
         || Config.try_old_cache_with_new_exec t.config
      then Some plugin
      else None
  ;;

  let add t sources plugin_uuid filename =
    if_ (not (Config.readonly t.config)) (fun () ->
      let dir = Config.dir t.config ^/ Info.cache_dir in
      Shell.mkdir_p ~perm:Info.cache_dir_perm dir >>=? fun () ->
      take_write_lock t >>=? fun () ->
      let uuid = Plugin_uuid.uuid plugin_uuid in
      let cmxs_filename = dir ^/ (Uuid.to_string uuid) ^ ".cmxs" in
      Shell.cp ~source:filename ~dest:cmxs_filename >>=? fun () ->
      Shell.chmod cmxs_filename ~perm:Info.cache_dir_perm >>=? fun () ->
      let plugin : Plugin.t =
        { sources
        ; plugin_uuid
        ; cmxs_filename
        ; build_info = Build_info.current
        }
      in
      add_plugin_internal t plugin;
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
