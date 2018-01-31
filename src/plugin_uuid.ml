open Core

module Repr = struct
  type t =
    { t           : string
    ; univ_constr : string
    }
  [@@deriving sexp, fields]

  let create ~t ~univ_constr =
    { t
    ; univ_constr
    }
  ;;
  let side_effect =
    { t           = "Ocaml_plugin.Dynloader.Side_effect"
    ; univ_constr = "Ocaml_plugin.Dynloader.side_effect_univ_constr"
    }
  ;;
end

module V1 = struct
  type t =
    { uuid       : Uuid.Stable.V1.t
    ; ml_bundles : Ml_bundle.t list
    ; repr       : Repr.t option
    }
  [@@deriving sexp, fields]
end

module V2 = struct
  module Prev = V1
  type t =
    { uuid       : Uuid.Stable.V1.t
    ; ml_bundles : Ml_bundle.t list
    ; repr       : Repr.t
    }
  [@@deriving sexp, fields]

  let t_of_sexp = Sexp.of_sexp_allow_extra_fields t_of_sexp

  let of_prev prev =
    let { Prev.
          uuid
        ; ml_bundles
        ; repr
        } = prev
    in
    let repr =
      match repr with
      | Some repr -> repr
      | None -> Repr.side_effect
    in
    { uuid
    ; ml_bundles
    ; repr
    }
  ;;
end

module Versioned = struct
  type t =
    | V1 of V1.t
    | V2 of V2.t
  [@@deriving sexp]
  let to_current = function
    | V1 v1 -> V2.of_prev v1
    | V2 v2 -> v2
  let of_current v2 = V2 v2
end

include V2

let of_v1 = V2.of_prev

let t_of_sexp sexp =
  match Versioned.t_of_sexp sexp with
  | versioned -> Versioned.to_current versioned
  | exception _ ->
    (* initially this was not versioned *)
    of_v1 (V1.t_of_sexp sexp)
;;

let sexp_of_t t = Versioned.sexp_of_t (Versioned.of_current t)

let create ~repr ~ml_bundles () =
  { uuid = Uuid.create ()
  ; ml_bundles
  ; repr
  }
;;
