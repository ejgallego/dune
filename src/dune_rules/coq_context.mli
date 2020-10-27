(* This file is licensed under The MIT License           *)
(* (c) MINES ParisTech 2018-2019                         *)
(* (c) INRIA 2020                                        *)
(* Written by: Emilio Jesús Gallego Arias, Rudi Grinberg *)

open! Stdune
open! Dune_engine

(** Context for a Coq buildable *)
type 'a t

(** It seems taking a Coq_lib.t should be preferred here? *)
val create :
     coqc_dir:Path.Build.t
  -> Super_context.t
  -> dir:Path.Build.t
  -> wrapper_name:string
  -> theories_deps:(Coq_lib.t list, exn) result
  -> Coq_stanza.Buildable.t
  -> 'a t

(** Directory *)
val dir : 'a t -> Path.Build.t

(** Check if the buildable is a bootstrapped library *)
val boot_type : 'a t -> Coq_bootstrap.t

(** Compat rule for coqdep: to go away *)
val mlpack_rule : 'a t -> unit Build.t

(** (closed) dependencies of the buildable *)
val theories_deps : 'a t -> Coq_lib.t list Or_exn.t

(** path to [coqdep] *)
val coqdep : 'a t -> Action.Prog.t

(** run [coqc] *)
val coqc :
     ?stdout_to:Path.Build.t
  -> 'a t
  -> Command.Args.dynamic Command.Args.t list
  -> Action.t Build.With_targets.t

(** Static flags for [coqc] *)
val coqc_file_flags : 'a t -> 'a Command.Args.t list

(** User-specified [coqc] expanded flags from [(flags ...)] field *)
val coq_flags : 'a t -> string list Build.t

(** This basically adjusts boot type for some special modules; it is thus
    necessary to call for each module kinda defeating the purpose of
    Coq_context. Should go away. *)
val for_module : 'a t -> Coq_module.t -> 'a t
