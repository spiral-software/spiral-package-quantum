(*
# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Some materials taken from CMU 15-411 Starter Code
#
# Parse
#
*)

open Core


let initialize_lexbuf (filename : string) : Lexing.lexbuf -> unit =
  let open Lexing in
  let pos = { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
  fun lexbuf ->
    lexbuf.lex_start_p <- pos;
    lexbuf.lex_curr_p <- pos
;;

let parse (filename : string) : Qst.circuit =
  try
    let ast =
      In_channel.with_file filename ~f:(fun chan ->
          let lexbuf = Lexing.from_channel chan in
          initialize_lexbuf filename lexbuf;
          try Q_parser.topcircuit Q_lexer.initial lexbuf with
          | _ ->
            let src_span =
              Mark.of_positions Lexing.(lexbuf.lex_start_p) Lexing.(lexbuf.lex_curr_p)
            in
            Error_msg.error Q_lexer.errors (Some src_span) ~msg:"Parse error.";
            raise Error_msg.Error)
    in
    if Error_msg.has_any_errors Q_lexer.errors
    then (
      Out_channel.prerr_endline "Lex error.";
      raise Error_msg.Error)
    else ast
  with
  | Sys_error s ->
    Out_channel.prerr_endline ("System error: " ^ s);
    raise Error_msg.Error
;;
