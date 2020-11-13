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

module Print : sig
  val pp_stm : stm -> int -> string
  val pp_circuit : circuit -> string
end

