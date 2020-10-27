(* This file is licensed under The MIT License           *)
(* (c) MINES ParisTech 2018-2019                         *)
(* (c) INRIA 2020                                        *)
(* Written by: Emilio Jesús Gallego Arias, Rudi Grinberg *)

open! Dune_engine
open! Stdune
open Coq_stanza
module SC = Super_context

let coq_debug = false

(* Coqdep / Coq expect the deps to the directory where the plugin cmxs file are.
   This seems to correspond to src_dir. *)
module Util = struct end

let resolve_program sctx ~loc ~dir prog =
  SC.resolve_program ~dir sctx prog ~loc:(Some loc) ~hint:"opam install coq"

(* get_libraries from Coq's ML dependencies *)
let libs_of_coq_deps ~lib_db = Result.List.map ~f:(Lib.DB.resolve lib_db)

let coqc_rule (cctx : _ Coq_context.t) ~file_flags coq_module =
  let source = Coq_module.source coq_module in
  let file_flags =
    let obj_dir = Coq_context.dir cctx in
    let object_to = Coq_module.obj_file ~obj_dir coq_module Obj in
    let aux = Coq_module.obj_file ~obj_dir coq_module Aux in
    let glob = Coq_module.obj_file ~obj_dir coq_module Glob in
    [ Command.Args.Hidden_targets [ object_to; aux; glob ]
    ; S file_flags
    ; Command.Args.Dep (Path.build source)
    ]
  in
  let open Build.With_targets.O in
  (* The way we handle the transitive dependencies of .vo files is not safe for
     sandboxing *)
  Build.with_no_targets
    (Build.dep (Dep.sandbox_config Sandbox_config.no_sandboxing))
  >>>
  let coq_flags = Coq_context.coq_flags cctx in
  Coq_context.coqc cctx (Command.Args.dyn coq_flags :: file_flags)

module Module_rule = struct
  type t =
    { coqdep : Action.t Build.With_targets.t
    ; coqc : Action.t Build.With_targets.t
    }
end

let setup_rule cctx ~source_rule coq_module =
  let open Build.With_targets.O in
  if coq_debug then
    Format.eprintf "gen_rule coq_module: %a@\n%!" Pp.to_fmt
      (Dyn.pp (Coq_module.to_dyn coq_module));

  let file_flags = Coq_context.coqc_file_flags cctx in

  let coqdep_rule = Coq_coqdep.rule cctx ~source_rule ~file_flags coq_module in

  (* Process coqdep and generate rules *)
  let dir = Coq_context.dir cctx in
  let boot_type = Coq_context.boot_type cctx in
  let deps_of = Coq_coqdep.deps_of ~dir ~boot_type coq_module in

  (* Rules for the files *)
  { Module_rule.coqdep = coqdep_rule
  ; coqc =
      Build.with_no_targets deps_of >>> coqc_rule cctx ~file_flags coq_module
  }

let coq_modules_of_theory ~sctx lib =
  let name = Coq_lib.name lib in
  let dir = Coq_lib.src_root lib in
  let dir_contents = Dir_contents.get sctx ~dir in
  let coq_sources = Dir_contents.coq dir_contents in
  Coq_sources.library coq_sources ~name

let source_rule ~sctx theories =
  (* sources for depending libraries coqdep requires all the files to be in the
     tree to produce correct dependencies, including those of dependencies *)
  Build.of_result_map theories ~f:(fun theories ->
      List.concat_map theories ~f:(coq_modules_of_theory ~sctx)
      |> List.rev_map ~f:(fun m -> Path.build (Coq_module.source m))
      |> Build.paths)

let setup_rules ~sctx ~dir ~dir_contents (s : Theory.t) =
  let name = snd s.name in
  let scope = SC.find_scope_by_dir sctx dir in
  let coq_lib_db = Scope.coq_libs scope in
  let theory = Coq_lib.DB.resolve coq_lib_db s.name |> Result.ok_exn in

  let cctx =
    let wrapper_name = Coq_lib.wrapper theory in
    (* Coq flags for depending libraries *)
    let theories_deps = Coq_lib.DB.requires coq_lib_db theory in
    let coqc_dir = (Super_context.context sctx).build_dir in
    Coq_context.create sctx ~coqc_dir ~dir ~wrapper_name ~theories_deps
      s.buildable
  in

  (* List of modules to compile for this library *)
  let coq_modules =
    let coq = Dir_contents.coq dir_contents in
    Coq_sources.library coq ~name
  in

  let source_rule =
    let theories =
      let open Result.O in
      let+ theories = Coq_context.theories_deps cctx in
      theory :: theories
    in
    source_rule ~sctx theories
  in
  List.concat_map coq_modules ~f:(fun m ->
      let cctx = Coq_context.for_module cctx m in
      let { Module_rule.coqc; coqdep } = setup_rule cctx ~source_rule m in
      [ coqc; coqdep ])

(******************************************************************************)
(* Install rules *)
(******************************************************************************)

(* This is here for compatibility with Coq < 8.11, which expects plugin files to
   be in the folder containing the `.vo` files *)
let coq_plugins_install_rules ~scope ~package ~dst_dir (s : Theory.t) =
  let lib_db = Scope.libs scope in
  let ml_libs =
    libs_of_coq_deps ~lib_db s.buildable.libraries |> Result.ok_exn
  in
  let rules_for_lib lib =
    let info = Lib.info lib in
    (* Don't install libraries that don't belong to this package *)
    if
      Option.equal Package.Name.equal (Lib_info.package info)
        (Some package.Package.name)
    then
      let loc = Lib_info.loc info in
      let plugins = Lib_info.plugins info in
      Mode.Dict.get plugins Mode.Native
      |> List.map ~f:(fun plugin_file ->
             (* Safe because all coq libraries are local for now *)
             let plugin_file = Path.as_in_build_dir_exn plugin_file in
             let plugin_file_basename = Path.Build.basename plugin_file in
             let dst =
               Path.Local.(to_string (relative dst_dir plugin_file_basename))
             in
             (Some loc, Install.Entry.make Section.Lib_root ~dst plugin_file))
    else
      []
  in
  List.concat_map ~f:rules_for_lib ml_libs

let install_rules ~sctx ~dir s =
  match s with
  | { Theory.package = None; _ } -> []
  | { Theory.package = Some package; _ } ->
    let loc = s.buildable.loc in
    let scope = SC.find_scope_by_dir sctx dir in
    let dir_contents = Dir_contents.get sctx ~dir in
    let name = snd s.name in
    (* This must match the wrapper prefix for now to remain compatible *)
    let dst_suffix = Coq_lib_name.dir (snd s.name) in
    (* These are the rules for now, coq lang 2.0 will make this uniform *)
    let dst_dir =
      if s.boot then
        (* We drop the "Coq" prefix (!) *)
        Path.Local.of_string "coq/theories"
      else
        let coq_root = Path.Local.of_string "coq/user-contrib" in
        Path.Local.relative coq_root dst_suffix
    in
    (* Also, stdlib plugins are handled in a hardcoded way, so no compat install
       is needed *)
    let coq_plugins_install_rules =
      if s.boot then
        []
      else
        coq_plugins_install_rules ~scope ~package ~dst_dir s
    in
    Dir_contents.coq dir_contents
    |> Coq_sources.library ~name
    |> List.concat_map ~f:(fun (vfile : Coq_module.t) ->
           let to_path f = Path.reach ~from:(Path.build dir) (Path.build f) in
           let to_dst f =
             Path.Local.to_string @@ Path.Local.relative dst_dir f
           in
           let vofile = Coq_module.obj_file ~obj_dir:dir vfile Obj in
           let vfile = Coq_module.source vfile in
           let make_entry file =
             ( Some loc
             , Install.(Entry.make Lib_root ~dst:(to_dst (to_path file)) file)
             )
           in
           [ make_entry vfile; make_entry vofile ])
    |> List.rev_append coq_plugins_install_rules

let coqpp_rules ~sctx ~dir (s : Coqpp.t) =
  let coqpp = resolve_program sctx ~dir ~loc:s.loc "coqpp" in
  let mlg_rule m =
    let source = Path.build (Path.Build.relative dir (m ^ ".mlg")) in
    let target = Path.Build.relative dir (m ^ ".ml") in
    let args = [ Command.Args.Dep source; Hidden_targets [ target ] ] in
    let build_dir = (Super_context.context sctx).build_dir in
    Command.run ~dir:(Path.build build_dir) coqpp args
  in
  List.map ~f:mlg_rule s.modules

let extraction_rules ~sctx ~dir ~dir_contents (s : Extraction.t) =
  let cctx =
    let wrapper_name = "DuneExtraction" in
    let theories_deps =
      let scope = SC.find_scope_by_dir sctx dir in
      let coq_lib_db = Scope.coq_libs scope in
      Coq_lib.DB.requires_for_user_written coq_lib_db s.buildable.theories
    in
    Coq_context.create sctx ~coqc_dir:dir ~dir ~wrapper_name ~theories_deps
      s.buildable
  in
  let coq_module =
    let coq = Dir_contents.coq dir_contents in
    Coq_sources.extract coq s
  in
  let ml_targets =
    Extraction.ml_target_fnames s |> List.map ~f:(Path.Build.relative dir)
  in
  let source_rule =
    let theories = source_rule ~sctx (Coq_context.theories_deps cctx) in
    let open Build.O in
    theories >>> Build.path (Path.build (Coq_module.source coq_module))
  in
  let { Module_rule.coqc; coqdep } = setup_rule cctx ~source_rule coq_module in
  let coqc = Build.With_targets.add coqc ~targets:ml_targets in
  [ coqdep; coqc ]
