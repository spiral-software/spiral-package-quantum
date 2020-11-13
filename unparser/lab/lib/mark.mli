(*
# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Some materials taken from CMU 15-411 Starter Code
#
# Mark tokens with source lines
#
*)

type src_loc =
  { line : int
  ; col : int
  }

type src_span =
  { start_loc : src_loc
  ; end_loc : src_loc
  ; file : string
  }

val of_position : Lexing.position -> src_loc
val of_positions : Lexing.position -> Lexing.position -> src_span
val show : src_span -> string 

type 'a t 
val mark : 'a -> src_span -> 'a t
val data : 'a t -> 'a
val src_span : 'a t -> src_span option

