(*
# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Some materials taken from CMU 15-411 Starter Code
#
# Quantum Code Generation
#
*)

module QS = Qst

type qasm_instr =
  | H of int
  | X of int
  | Y of int
  | Z of int
  | S of int
  | Sdag of int
  | T of int
  | Tdag of int
  | CNOT of int*int
  [@@deriving equal]
  
type program  = qasm_instr list

val qasm_codegen : Qst.circuit -> int list -> program 

val pp_qasm : program -> unit

val qasm_to_string : program -> int -> string list

val get_init_order : Qst.circuit -> int list 

val get_num_qbits : Qst.circuit -> int