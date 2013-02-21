open Core.Std

module Repr = struct
  type t = {
    t : string;
    univ_constr: string;
  } with sexp, bin_io, compare, fields
  let create ~t ~univ_constr = {
    t;
    univ_constr;
  }
end

module T = struct
  type t = {
    uuid : Uuid.t;
    ml_bundles : Ml_bundle.t list;
    repr : Repr.t option
  } with sexp, bin_io, compare, fields

  let hash t = Hashtbl.hash (Uuid.hash t.uuid, t.ml_bundles, t.repr)
end
include T

include Comparable.Make_binable(T)
include Hashable.Make_binable (T)

let create ~repr ~ml_bundles () =
  {
    uuid = Uuid.create ();
    ml_bundles;
    repr;
  }

let t_of_string s = t_of_sexp (Sexp.of_string s)
let string_of_t t = Sexp.to_string (sexp_of_t t)
