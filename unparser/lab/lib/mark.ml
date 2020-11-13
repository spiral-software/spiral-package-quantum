(*
# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Some materials taken from CMU 15-411 Starter Code
#
# Mark tokens with source lines
#
*)

open Core

type src_loc =
  { line : int
  ; col : int
  }
[@@deriving compare, sexp]

module Compare_src_loc = Comparable.Make (struct
  type t = src_loc [@@deriving compare, sexp]
end)

type src_span =
  { start_loc : src_loc
  ; end_loc : src_loc
  ; file : string
  }

let of_position (pos : Lexing.position) : src_loc =
  Lexing.{ line = pos.pos_lnum; col = pos.pos_cnum - pos.pos_bol + 1 }
;;

let of_positions (pos_start : Lexing.position) (pos_end : Lexing.position) =
  { start_loc = of_position pos_start
  ; end_loc = of_position pos_end
  ; file = Lexing.(pos_start.pos_fname)
  }
;;

let show : src_span -> string =
  let show_src_loc = function
    | { line; col = 0 } -> string_of_int line
    | { line; col } -> string_of_int line ^ "." ^ string_of_int col
  in
  fun span ->
    sprintf "%s:%s-%s" span.file (show_src_loc span.start_loc) (show_src_loc span.end_loc)
;;

type 'a t = 'a * src_span option

let mark (data : 'a) (span : src_span) : 'a t = data, Some span
let data : 'a t -> 'a = fst
let src_span : 'a t -> src_span option = snd
