(*
# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Some materials taken from CMU 15-411 Starter Code
#
# Error Messages
#
*)

open Core

type t = { mutable error_count : int }

let create () : t = { error_count = 0 }
let has_any_errors : t -> bool = fun t -> t.error_count > 0

let print_msg (variety : string) (span : Mark.src_span option) ~(msg : string) =
  Option.iter span ~f:(fun x -> Out_channel.output_string stderr (Mark.show x));
  Out_channel.fprintf stderr ":%s:%s\n" variety msg
;;

let error (t : t) (span : Mark.src_span option) ~(msg : string) : unit =
  t.error_count <- t.error_count + 1;
  print_msg "error" span ~msg
;;

exception Error
