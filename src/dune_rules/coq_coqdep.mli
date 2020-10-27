(* This file is licensed under The MIT License           *)
(* (c) MINES ParisTech 2018-2019                         *)
(* (c) INRIA 2020                                        *)
(* Written by: Emilio Jesús Gallego Arias, Rudi Grinberg *)

open! Stdune
open! Dune_engine

(** Dependencies for a particular module [foo], reads [foo.v.d] *)
val deps_of :
  dir:Path.Build.t -> boot_type:Coq_bootstrap.t -> Coq_module.t -> unit Build.t

(** Rule to generate [foo.v.d] for [foo] *)
val rule :
     'a Coq_context.t
  -> source_rule:unit Build.t
  -> file_flags:Command.Args.dynamic Command.Args.t list
  -> Coq_module.t
  -> Action.t Build.With_targets.t
