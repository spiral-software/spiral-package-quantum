(*
# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Some materials taken from CMU 15-411 Starter Code
#
# Quantum Assembly Syntax
#
*)


open! Core

type gate =
  | I of Int32.t
  | H of Int32.t
  | X of Int32.t
  | Y of Int32.t
  | Z of Int32.t
  | S of Int32.t
  | Sdag of Int32.t
  | T of Int32.t
  | Tdag of Int32.t
  | CNOT of Int32.t*Int32.t

type stm =
  | Gate of gate
  | Tensor of (mstm list) list
  | Junc of Int32.t list * Int32.t

and mstm = stm Mark.t

type circuit = mstm list

module Print = struct
  let pp_gate = function
    | I i -> sprintf "I(%d)" (Int32.to_int_exn i)
    | H i -> sprintf "H(%d)" (Int32.to_int_exn i)
    | X i -> sprintf "X(%d)" (Int32.to_int_exn i)
    | Y i -> sprintf "Y(%d)" (Int32.to_int_exn i)
    | Z i -> sprintf "Z(%d)" (Int32.to_int_exn i)
    | S i -> sprintf "S(%d)" (Int32.to_int_exn i)
    | Sdag i -> sprintf "Sdag(%d)" (Int32.to_int_exn i)
    | T i -> sprintf "T(%d)" (Int32.to_int_exn i)
    | Tdag i -> sprintf "Tdag(%d)" (Int32.to_int_exn i)
    | CNOT (i, j) -> sprintf "CNOT(%d, %d)" (Int32.to_int_exn i) (Int32.to_int_exn j)

  let rec pp_stm (stm) (lvl:int) = 
    match stm with 
      | Tensor _exp -> sprintf "%sTensor{\n%s%s}\n" (tabs lvl "") (String.concat( List.map _exp ~f:(fun e -> (tabs lvl "")^">\n"^pp_intcircuit e (lvl+1)))) (tabs lvl "")
      | Junc (i, d) -> sprintf "%sJunc(%s, %d)\n" (tabs lvl "")  (String.concat (List.map i ~f:(Int32.to_string))) (Int32.to_int_exn d)
      | Gate g -> sprintf "%s%s\n" (tabs lvl "") (pp_gate g)
  and pp_mstm stm (lvl:int) = pp_stm (Mark.data stm) lvl
  and pp_stms stms (lvl:int) = String.concat (List.map ~f:(fun stm -> (pp_mstm stm lvl)) stms)
  and pp_intcircuit stms (lvl:int) = pp_stms stms lvl
  and tabs i acc = if i = 0 then acc else (tabs (i-1) ("  "^acc))

  let pp_circuit stms = "{\n" ^ (pp_intcircuit stms 0) ^ "\n}"
end
