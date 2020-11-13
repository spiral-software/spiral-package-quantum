(*
# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Some materials taken from CMU 15-411 Starter Code
#
# QASM Unparser
#
*)


open Core

type cmd_line_args =
  { filename : string }

let cmd_line_term : cmd_line_args Cmdliner.Term.t =
  let open Cmdliner in
  let module Let_syntax = struct
    let return = Term.pure
    let map ~f a = Term.(return f $ a)
  end
  in
  let%map filename =
    let doc = "The source file $(docv) to compile." in
    Arg.(required (pos 0 (some non_dir_file) None (info [] ~doc ~docv:"FILE")))
  in
  { filename }
;;

let compile (_cmd : cmd_line_args) : unit =
  let _qst = Parse.parse _cmd.filename in
  let ord = Qcodegen.get_init_order _qst in
  let _qasm = Qcodegen.qasm_codegen _qst ord in 
  let num = Qcodegen.get_num_qbits _qst in
  Qcodegen.pp_qasm _qasm;
  let strs = Qcodegen.qasm_to_string _qasm num in
  let file = _cmd.filename ^ ".qasm" in
  Out_channel.with_file file ~f:(fun out -> List.iter strs ~f:(fun line -> Out_channel.fprintf out "%s" line))
;;

let run (cmd : cmd_line_args) : unit =
  compile cmd
;;

let main () =
  let open Cmdliner in
  let cmd_line_info = Term.info "unparser" ~doc:"Compile a QSPIRAL source file." in
  match Term.eval (cmd_line_term, cmd_line_info) with
  | `Ok cmd_line -> run cmd_line
  | result -> Term.exit result
;;
