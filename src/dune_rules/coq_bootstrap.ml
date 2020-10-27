(* This file is licensed under The MIT License           *)
(* (c) MINES ParisTech 2018-2019                         *)
(* (c) INRIA 2020                                        *)
(* Written by: Emilio Jesús Gallego Arias, Rudi Grinberg *)

open! Stdune

(* the internal boot flag determines if the Coq "standard library" is being
   built, in case we need to explitly tell Coq where the build artifacts are and
   add `Init.Prelude.vo` as a dependency; there is a further special case when
   compiling the prelude, in this case we also need to tell Coq not to try to
   load the prelude. *)
type t =
  | No_boot  (** Coq's stdlib is installed globally *)
  | Bootstrap of Coq_lib.t
      (** Coq's stdlib is in scope of the composed build *)
  | Bootstrap_prelude
      (** We are compiling the prelude itself
          [should be replaced with (per_file ...) flags] *)

let get ~boot_lib ~wrapper_name coq_module =
  match boot_lib with
  | None -> No_boot
  | Some (_loc, lib) -> (
    (* This is here as an optimization, TODO; replace with per_file flags *)
    let init =
      String.equal (Coq_lib.wrapper lib) wrapper_name
      && Option.equal String.equal
           (List.hd_opt (Coq_module.prefix coq_module))
           (Some "Init")
    in
    match init with
    | false -> Bootstrap lib
    | true -> Bootstrap_prelude )

let flags =
  let open Command in
  function
  | No_boot -> []
  | Bootstrap _lib -> [ Args.A "-boot" ]
  | Bootstrap_prelude -> [ Args.As [ "-boot"; "-noinit" ] ]
