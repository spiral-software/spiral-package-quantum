(*
# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Some materials taken from CMU 15-411 Starter Code
#
# Error Messages
#
*)

type t

val create : unit -> t
val has_any_errors : t -> bool

val error : t -> Mark.src_span option -> msg:string -> unit

exception Error
