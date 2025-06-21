module Incr = Incremental.Make ()
module Incr_map = Incr_map.Make (Incr)
include Protocol

let print_s (sexp : Core.Sexp.t) : unit =
  let module Sexp_pp = Sexp_pretty in
  Sexp_pp.pp_out_channel Sexp_pp.Config.default stdout sexp
