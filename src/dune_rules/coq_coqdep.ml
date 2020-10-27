(* This file is licensed under The MIT License           *)
(* (c) MINES ParisTech 2018-2019                         *)
(* (c) INRIA 2020                                        *)
(* Written by: Emilio Jesús Gallego Arias, Rudi Grinberg *)

open! Stdune
open! Dune_engine

let coqdep_debug = false

let parse ~dir ~(boot_type : Coq_bootstrap.t) ~coq_module (lines : string list)
    =
  if coqdep_debug then Format.eprintf "Parsing coqdep @\n%!";
  let source = Coq_module.source coq_module in
  let invalid phase =
    User_error.raise
      [ Pp.textf "coqdep returned invalid output for %s / [phase: %s]"
          (Path.Build.to_string_maybe_quoted source)
          phase
      ; Pp.verbatim (String.concat ~sep:"\n" lines)
      ]
  in
  let line =
    match lines with
    | []
    | _ :: _ :: _ :: _ ->
      invalid "line"
    | [ line ] -> line
    | [ l1; _l2 ] ->
      (* .vo is produced before .vio, this is fragile tho *)
      l1
  in
  match String.lsplit2 line ~on:':' with
  | None -> invalid "split"
  | Some (basename, deps) -> (
    let ff = List.hd @@ String.extract_blank_separated_words basename in
    let depname, _ = Filename.split_extension ff in
    let modname =
      String.concat ~sep:"/"
        Coq_module.(
          prefix coq_module @ [ Coq_module.Name.to_string (name coq_module) ])
    in
    if coqdep_debug then
      Format.eprintf "depname / modname: %s / %s@\n%!" depname modname;
    if depname <> modname then invalid "basename";
    let deps = String.extract_blank_separated_words deps in
    if coqdep_debug then
      Format.eprintf "deps for %s: %a@\n%!"
        (Path.Build.to_string source)
        (Format.pp_print_list Format.pp_print_string)
        deps;
    (* Add prelude deps for when stdlib is in scope and we are not actually
       compiling the prelude *)
    let deps = List.map ~f:(Path.relative (Path.build dir)) deps in
    match boot_type with
    | No_boot
    | Bootstrap_prelude ->
      deps
    | Bootstrap lib ->
      Path.relative (Path.build (Coq_lib.src_root lib)) "Init/Prelude.vo"
      :: deps )

let deps_of ~dir ~boot_type coq_module =
  let stdout_to = Coq_module.obj_file ~obj_dir:dir coq_module Dep in
  Build.dyn_paths_unit
    (Build.map
       (Build.lines_of (Path.build stdout_to))
       ~f:(parse ~dir ~boot_type ~coq_module))

let rule (cctx : _ Coq_context.t) ~source_rule ~file_flags coq_module =
  (* coqdep needs the full source + plugin's mlpack to be present :( *)
  let source = Coq_module.source coq_module in
  let file_flags =
    [ Command.Args.S file_flags
    ; As [ "-dyndep"; "opt" ]
    ; Dep (Path.build source)
    ]
  in
  let coqdep = Coq_context.coqdep cctx in
  let obj_dir = Coq_context.dir cctx in
  let mlpack_rule = Coq_context.mlpack_rule cctx in
  let stdout_to = Coq_module.obj_file ~obj_dir coq_module Dep in
  (* Coqdep has to be called in the stanza's directory *)
  let open Build.With_targets.O in
  Build.with_no_targets mlpack_rule
  >>> Build.with_no_targets source_rule
  >>> Command.run ~dir:(Path.build obj_dir) ~stdout_to coqdep file_flags
