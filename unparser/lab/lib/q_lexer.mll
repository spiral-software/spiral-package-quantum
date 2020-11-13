{
(*
# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Some materials taken from CMU 15-411 Starter Code
#
# Lexer
#
*)

open Core

module T = Q_parser 
let errors = Error_msg.create ()

let text = Lexing.lexeme

let from_lexbuf : Lexing.lexbuf -> Mark.src_span option =
  fun lexbuf ->
    Mark.of_positions
      (Lexing.lexeme_start_p lexbuf)
      (Lexing.lexeme_end_p lexbuf)
    |> Option.some

let error lexbuf ~msg : unit =
  let src_span = from_lexbuf lexbuf in
  Error_msg.error errors src_span ~msg

let dec_constant s lexbuf =
  let handle_int_min str =
    if String.equal str "2147483648"
      then "0x80000000"
      else str
  in
  let i32 =
    try Int32.of_string (handle_int_min s)
    with Failure _ ->
      let src_span = from_lexbuf lexbuf in
      Error_msg.error errors src_span
        ~msg:(sprintf "Cannot parse decimal constant `%s`" (text lexbuf));
      Int32.zero
  in
  T.Dec_const i32

}

let dec_num = ("0" | ['1'-'9'](['0'-'9']*))

let ws = [' ' '\t' '\r' '\011' '\012']

rule initial = parse
  | ws+  { initial lexbuf }
  | '\n' { Lexing.new_line lexbuf;
           initial lexbuf
         }

  | "Tensor" { T.Tensor }
  | "Junc" { T.Junc }
  | '(' { T.L_paren }
  | ')' { T.R_paren }
  | '[' { T.L_brace }
  | ']' { T.R_brace }

  | '*' { T.Mult }

  | ','  { T.Comma }
  | "qH" { T.H }
  | "qT" { T.T }
  | "qTdag" { T.Tdag }
  | "qS" { T.S }
  | "qSdag" { T.Sdag }
  | "qX" { T.X }
  | "qY" { T.Y }
  | "qZ" { T.Z }
  | "CNOT" { T.CNOT }
  | "I" { T.I }

  | '-' { T.Minus }

  | dec_num as n { dec_constant n lexbuf }

  | eof { T.Eof }

  | _  { error lexbuf
           ~msg:(sprintf "Illegal character '%s'" (text lexbuf));
         initial lexbuf
       }


