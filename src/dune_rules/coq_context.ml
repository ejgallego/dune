(* This file is licensed under The MIT License           *)
(* (c) MINES ParisTech 2018-2019                         *)
(* (c) INRIA 2020                                        *)
(* Written by: Emilio Jesús Gallego Arias, Rudi Grinberg *)

open! Stdune
open! Dune_engine
open Coq_stanza
module SC = Super_context

module Util = struct
  let include_paths ts =
    Path.Set.of_list_map ts ~f:(fun t ->
        let info = Lib.info t in
        Lib_info.src_dir info)

  let include_flags ts = include_paths ts |> Lib.L.to_iflags

  (* coqdep expects an mlpack file next to the sources otherwise it
   * will omit the cmxs deps *)
  let ml_pack_files lib =
    let plugins =
      let info = Lib.info lib in
      let plugins = Lib_info.plugins info in
      Mode.Dict.get plugins Mode.Native
    in
    let to_mlpack file =
      [ Path.set_extension file ~ext:".mlpack"
      ; Path.set_extension file ~ext:".mllib"
      ]
    in
    List.concat_map plugins ~f:to_mlpack

  let resolve_program sctx ~loc ~dir prog =
    SC.resolve_program ~dir sctx prog ~loc:(Some loc) ~hint:"opam install coq"
end

type 'a t =
  { coqdep : Action.Prog.t
  ; coqc : Action.Prog.t * Path.Build.t
  ; wrapper_name : string
  ; dir : Path.Build.t
  ; expander : Expander.t
  ; buildable : Buildable.t
  ; theories_deps : Coq_lib.t list Or_exn.t
  ; mlpack_rule : unit Build.t
  ; ml_flags : 'a Command.Args.t
  ; scope : Scope.t
  ; boot_type : Coq_bootstrap.t
  ; build_dir : Path.Build.t
  ; profile_flags : Ordered_set_lang.Unexpanded.t
  }

let dir t = t.dir

let boot_type t = t.boot_type

let mlpack_rule t = t.mlpack_rule

let theories_deps t = t.theories_deps

let coqdep t = t.coqdep

let coqc ?stdout_to t args =
  let dir = Path.build (snd t.coqc) in
  Command.run ~dir ?stdout_to (fst t.coqc) args

let coq_flags t =
  Build.(
    map ~f:List.concat
      (all
         [ Expander.expand_and_eval_set t.expander t.profile_flags
             ~standard:(Build.return [])
         ; Expander.expand_and_eval_set t.expander t.buildable.flags
             ~standard:(Build.return [])
         ]))

let theories_flags =
  let setup_theory_flag lib =
    let wrapper = Coq_lib.wrapper lib in
    let dir = Coq_lib.src_root lib in
    let binding_flag =
      if Coq_lib.implicit lib then
        "-R"
      else
        "-Q"
    in
    [ Command.Args.A binding_flag; Path (Path.build dir); A wrapper ]
  in
  fun t ->
    Command.of_result_map t.theories_deps ~f:(fun libs ->
        Command.Args.S (List.concat_map libs ~f:setup_theory_flag))

let coqc_file_flags cctx =
  let file_flags =
    [ cctx.ml_flags
    ; theories_flags cctx
    ; Command.Args.A "-R"
    ; Path (Path.build cctx.dir)
    ; A cctx.wrapper_name
    ]
  in
  [ Command.Args.S (Coq_bootstrap.flags cctx.boot_type); S file_flags ]

(* get_libraries from Coq's ML dependencies *)
let libs_of_coq_deps ~lib_db = Result.List.map ~f:(Lib.DB.resolve lib_db)

(* compute include flags and mlpack rules *)
let setup_ml_deps ~lib_db libs theories =
  (* Pair of include flags and paths to mlpack *)
  let libs =
    let open Result.O in
    let* theories = theories in
    let libs = libs @ List.concat_map ~f:Coq_lib.libraries theories in
    let* libs = libs_of_coq_deps ~lib_db libs in
    Lib.closure ~linking:false libs
  in
  ( Command.of_result_map libs ~f:Util.include_flags
  , Build.of_result_map libs ~f:(fun libs ->
        (* If the mlpack files don't exist, don't fail *)
        Build.paths_existing (List.concat_map ~f:Util.ml_pack_files libs)) )

let create ~coqc_dir sctx ~dir ~wrapper_name ~theories_deps
    (buildable : Buildable.t) =
  let loc = buildable.loc in
  let rr = Util.resolve_program sctx ~dir ~loc in
  let expander = Super_context.expander sctx ~dir in
  let scope = SC.find_scope_by_dir sctx dir in
  let lib_db = Scope.libs scope in
  (* ML-level flags for depending libraries *)
  let ml_flags, mlpack_rule =
    setup_ml_deps ~lib_db buildable.libraries theories_deps
  in
  let build_dir = (Super_context.context sctx).build_dir in
  { coqdep = rr "coqdep"
  ; coqc = (rr "coqc", coqc_dir)
  ; wrapper_name
  ; dir
  ; expander
  ; buildable
  ; theories_deps
  ; mlpack_rule
  ; ml_flags
  ; scope
  ; boot_type = Coq_bootstrap.No_boot
  ; build_dir
  ; profile_flags = Super_context.coq sctx ~dir
  }

let for_module t coq_module =
  let boot_lib = t.scope |> Scope.coq_libs |> Coq_lib.DB.boot_library in
  let boot_type =
    Coq_bootstrap.get ~boot_lib ~wrapper_name:t.wrapper_name coq_module
  in
  { t with boot_type }
