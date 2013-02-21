open Core.Std
open Async.Std
open Async_extra.Std

type connection_state = int ref

module Query =
struct
  type t = {
    x : string;
  } with bin_io
end

module Response =
struct
  type t = {
    y : string;
    z : int;
  } with bin_io
end

let toto_rpc = Rpc.Rpc.create
  ~name:"toto"
  ~version:1
  ~bin_query:Query.bin_t
  ~bin_response:Response.bin_t

let toto_handler state query =
  incr state;
  Deferred.return {
    Response.
    y = query.Query.x;
    z = !state;
  }

let implement_toto = Rpc.Rpc.implement toto_rpc toto_handler

let server_toto =
  Rpc.Implementations.create_exn
    ~implementations:[implement_toto]
    ~on_unknown_rpc:(`Ignore)


module Fork :
sig
  module Pipe :
  sig
    type t = {
      reader : Reader.t;
      writer : Writer.t;
    }
  end

  module Runner :
  sig
    type t = {
      pipe : Pipe.t;
    }
  end

  module Main :
  sig
    type t = {
      pipe : Pipe.t;
      child : Pid.t;
    }
  end

  type t =
  | Runner of Runner.t
  | Main of Main.t

  type fork_side

  val fork :
    implementations:'state Rpc.Implementations.t
    -> connection_state:(fork_side -> 'state)
    (* closure applied only once in the runner process *)
    -> on_handshake_error:[
    | `Raise
    | `Ignore
    | `Call of (Exn.t -> unit Deferred.t)
    ]
    -> (Rpc.Connection.t, Exn.t) Result.t Deferred.t
end =
struct
  module Pipe =
  struct
    type t = {
      reader : Reader.t;
      writer : Writer.t;
    }
  end

  module Runner =
  struct
    type t = {
      pipe : Pipe.t;
    }
  end

  module Main =
  struct
    type t = {
      pipe : Pipe.t;
      child : Pid.t;
    }
  end

  type t =
  | Runner of Runner.t
  | Main of Main.t

  type fork_side

  let fork ~implementations:_ ~connection_state:_ ~on_handshake_error:_ =
    assert false
end

(* in the ocaml_plugin loader:
   the module type of an argument for a new functor *)
module type Distant_module_arg =
sig
  type t
  val repr : string

  type query with bin_io
  type response with bin_io

  val implement : t -> query -> response

  (*
    see if we can use instead:
    t Rpc.Implementation.t list
  *)

end

module Reload =
struct
  module Query =
  struct
    type t = string (* filename *)
    with bin_io
  end
  module Response =
  struct
    type t = (unit, string) Result.t
    with bin_io
  end
  let rpc = Rpc.Rpc.create
    ~name:"reload"
    ~version:1
    ~bin_query:Query.bin_t
    ~bin_response:Response.bin_t
end

module Distant_module :
sig
  type ('query, 'response) t
  val dispatch : ('query, 'response) t -> 'query -> 'response Deferred.t

  val reload : (_, _) t -> string -> (unit, string) Result.t Deferred.t

  val private_make :
    rpc:(('query, 'response) Rpc.Rpc.t)
    -> connection:Rpc.Connection.t
    -> ('query, 'response) t
end =
struct
  type ('query, 'response) t = {
    rpc : ('query, 'response) Rpc.Rpc.t;
    connection : Rpc.Connection.t;
  }
  let dispatch t query =
    Rpc.Rpc.dispatch_exn t.rpc t.connection query

  let reload t filename =
    Rpc.Rpc.dispatch_exn Reload.rpc t.connection filename

  let private_make ~rpc ~connection = {
    rpc;
    connection;
  }

end

type loader

module Make_distant(X:Distant_module_arg) :
sig
  val load_ocaml_src_file :
    loader -> string
    -> ((X.query, X.response) Distant_module.t, Exn.t) Result.t Deferred.t
end =
struct

  type t = {
    mutable state : X.t;
    loader : loader;
  }

  (* module This_Loader = Make(X (\*down cast of X*\)) *)
  let really_load loader filename = (Obj.magic (ignore (loader,filename)) : X.t)
  let make_state loader filename =
    let state = really_load loader filename in
    {
      state;
      loader;
    }

  let reload t filename =
    let state = really_load t.loader filename in
    t.state <- state

  let load_ocaml_src_file loader filename =
    let rpc = Rpc.Rpc.create
      ~name:filename
      ~version:1
      ~bin_query:X.bin_query
      ~bin_response:X.bin_response
    in
    let implement t query = Deferred.return (X.implement t.state query) in
    let reload t filename =
      Deferred.return (
        try
          Result.Ok (reload t filename)
        with
        | e -> Result.Error (Exn.to_string e)
      )
    in
    let implementations = Rpc.Implementations.create_exn
      ~implementations:[
        Rpc.Rpc.implement rpc implement;
        Rpc.Rpc.implement Reload.rpc reload;
      ]
      ~on_unknown_rpc:`Raise
    in
    let fork_connection =
      Fork.fork
        ~implementations
        ~connection_state:(fun _ -> make_state loader filename)
        ~on_handshake_error:`Raise
    in
    fork_connection >>| fun fork_connection ->
    Result.bind fork_connection (fun connection ->
      Ok (Distant_module.private_make ~rpc ~connection)
    )

(*

 val fork :
    server:'state Rpc.Server.t
    -> connection_state:'state
    -> on_handshake_error:[
    | `Raise
    | `Ignore
    | `Call of (Exn.t -> unit Deferred.t)
    ]
    -> (Rpc.Connection.t, Exn.t) Result.t Deferred.t

*)

end

(*
  Example:
*)

module type M_intf =
sig
  val f1 : int -> int -> int
end

module X : Distant_module_arg =
struct
  type t = (module M_intf)
  let repr = "M_intf"

  type query = [
  | `f1 of int * int
  ] with bin_io

  type response = [
  | `f1 of int
  ] with bin_io

  let implement t query =
    let module M = (val t : M_intf) in
    match query with
    | `f1 (arg1, arg2) -> `f1 (M.f1 arg1 arg2)
end
