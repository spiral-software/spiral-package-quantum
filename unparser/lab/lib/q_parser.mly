%{
(*
# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Some materials taken from CMU 15-411 Starter Code
#
# Parser
#
*)

let mark
  (data : 'a)
  (start_pos : Lexing.position)
  (end_pos : Lexing.position) : 'a Mark.t =
  let src_span = Mark.of_positions start_pos end_pos in
  Mark.mark data src_span

%}

%token Eof
%token Tensor
%token <Int32.t> Dec_const
%token Junc
%token H I CNOT T Tdag S Sdag X Y Z 
%token Mult
%token Minus 
%token L_brace R_brace
%token L_paren R_paren
%token Comma

%start topcircuit

 %type <Qst.mstm list> topcircuit
%type <Qst.mstm list> circuit
%type <Qst.stm> stm
%type <Qst.mstm> m(stm)
%type <Qst.gate> gate
%type <Int32.t list> intlist
%type <Int32.t list> restintlist
%type <Core.Int32.t> int_const


%%

topcircuit :
  | st = m(stm); Mult; c = circuit; Eof;
    { st :: c }
  | st = m(stm); Eof;
    { [st] }
  ;

circuit :
  | st = m(stm); Mult; c = circuit;
    { st :: c }
  | st = m(stm); 
    { [st] }
  ; 

circuitlist : 
  | g = circuit; gs = restcircuitlist
      { g :: gs }
  | g = circuit;
      { [g] }
  ; 

restcircuitlist : 
  | Comma; e = circuit; es = restcircuitlist
      { e :: es }
  | Comma; g = circuit;
      { [g] }
  ;


m(x) :
  | x = x;
      { mark x $startpos(x) $endpos(x) }
  ;

stm :
  | Tensor; L_paren; i = circuitlist; R_paren; 
      { Qst.Tensor(i) }
  | Junc; L_paren; L_brace; il = intlist; R_brace; Comma; c = int_const; R_paren;
      { Qst.Junc(il, c) }
  | Junc; L_paren; L_brace; il = intlist; R_brace; Comma; Minus; c = int_const; R_paren;
      { Qst.Junc(il, Int32.neg c) }
  | g = gate;
      { Qst.Gate(g) }
  ;


intlist : 
  | p = int_const; ps = restintlist;
    { p :: ps }
  | p = int_const;
    { [p] }
  | 
    { [] }
  ;

restintlist : 
  | Comma; p = int_const; rpl = restintlist
    { p :: rpl }
  | Comma; p = int_const; 
    { [p] }
  ; 

int_const :
  | c = Dec_const;
      { (c) }
  ;

gate :
  | H; L_paren; R_paren;
      { Qst.H(Int32.one) }
  | T; L_paren; R_paren;
      { Qst.T(Int32.one) }
  | Tdag; L_paren; R_paren;
      { Qst.Tdag(Int32.one) }
  | S; L_paren; R_paren;
      { Qst.S(Int32.one) }
  | Sdag; L_paren; R_paren;
      { Qst.Sdag(Int32.one) }
  | X; L_paren; R_paren;
      { Qst.X(Int32.one) }
  | Y; L_paren; R_paren;
      { Qst.Y(Int32.one) }
  | Z; L_paren; R_paren;
      { Qst.Z(Int32.one) }
  | I; L_paren; i = int_const; R_paren;
      { Qst.I(i) }
  | CNOT; L_paren; i = int_const; Comma; j = int_const; R_paren;
      { Qst.CNOT(i, j) }
  ;
