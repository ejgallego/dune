(* This file is licensed under The MIT License           *)
(* (c) MINES ParisTech 2018-2019                         *)
(* (c) INRIA 2020                                        *)
(* Written by: Emilio Jesús Gallego Arias, Rudi Grinberg *)

type t =
  | No_boot  (** Coq's stdlib is installed globally *)
  | Bootstrap of Coq_lib.t
      (** Coq's stdlib is in scope of the composed build *)
  | Bootstrap_prelude
      (** We are compiling the prelude itself
          [should be replaced with (per_file ...) flags] *)

val get :
  boot_lib:(_ * Coq_lib.t) option -> wrapper_name:string -> Coq_module.t -> t

val flags : t -> 'a Command.Args.t list
