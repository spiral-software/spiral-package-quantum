(*
# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Some materials taken from CMU 15-411 Starter Code
#
# Quantum Code Generation
#
*)

open Core
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

let _cnot_cancellable (a) (b) : bool = 
  match (a,b) with 
    | (CNOT(i,j), CNOT(h,k)) -> (Int.equal i h) && (Int.equal j k)
    | _ -> false
;;


let rec _print_list (l : int list) : unit = 
  match l with 
    | i::is ->   printf "%d, " i; _print_list is 
    | [] -> ()
;;

let rec log2 (i : int) : int =
  (match i with 
    | 1 -> 0 
    | g -> (
      let j = g/2 in
      (1 + log2 (j))
    )
  )
;;

let get_gatesize (g : Qst.gate) : int = 
  match g with 
    | QS.CNOT (i,_) -> Int32.to_int_exn i+1 
    | QS.I i -> (log2 (Int32.to_int_exn i))
    | _ -> 1
;;

let rec get_size (e : Qst.stm) : int = 
    match e with 
      | QS.Gate(g) -> get_gatesize(g)
      | QS.Tensor l -> (
          let flat_stms = List.map l ~f:(fun subcirc -> match subcirc with i::_ -> i | [] -> failwith "empty subcirc") in 
          List.fold_left flat_stms ~init:0 ~f:(fun i stm -> i + get_size(Mark.data stm))
      )
      | QS.Junc (j,_) -> List.length j
;;

let get_init_order (_qst : Qst.circuit) : int list = 
  let rec canonical_qubit_order = fun (size : int) : int list -> (
    if size <= 0 then [] else
    (size-1)::(canonical_qubit_order (size-1))
  ) in
  let first_stm = match _qst with i::_ -> Mark.data i | [] -> failwith "empty program" in
  let first_size = (get_size first_stm) in 
  let revorder = canonical_qubit_order first_size in
  let order = List.rev(revorder) in
  order
;;

let get_num_qbits (_qst : Qst.circuit) : int  = 
  let first_stm = match _qst with i::_ -> Mark.data i | [] -> failwith "empty program" in
  let first_size = (get_size first_stm) in 
  first_size
;;

let rec qasm_codegen (_qst : Qst.circuit) (order : int list) :  program =
  let gengate (g : Qst.gate) (qubit_idx : int) (mapping : int list): qasm_instr list = (
    match g with 
      | QS.H _ -> [H(List.nth_exn mapping qubit_idx)]
      | QS.X _ -> [X(List.nth_exn mapping qubit_idx)]
      | QS.Y _ -> [Y(List.nth_exn mapping qubit_idx)]
      | QS.Z _ -> [Z(List.nth_exn mapping qubit_idx)]
      | QS.S _ -> [S(List.nth_exn mapping qubit_idx)]
      | QS.Sdag _ -> [Sdag(List.nth_exn mapping qubit_idx)]
      | QS.T _ -> [T(List.nth_exn mapping qubit_idx)]
      | QS.Tdag _ -> [Tdag(List.nth_exn mapping qubit_idx)]
      | QS.CNOT (i,d) -> if Int32.to_int_exn d = 0 then [CNOT(List.nth_exn mapping (qubit_idx), List.nth_exn mapping (qubit_idx+(Int32.to_int_exn i))  )]
                          else [CNOT(List.nth_exn mapping (qubit_idx+(Int32.to_int_exn i)), List.nth_exn mapping qubit_idx)]
      | QS.I _ -> []
  ) in
  let rec gentensor (stms : (QS.mstm list) list) (mapping : int list) (base : int): program = (
    match stms with
      | e :: es -> ( 
        let ord = get_init_order e in 
        let ord' = List.map ord ~f:(fun i -> List.nth_exn mapping (i+base)) in 
        let rhs = (qasm_codegen e ord') in
        rhs @ ( gentensor es mapping (base+List.length(ord)) )
      )
      | [] -> []
  ) in 
  let permute (ordering: int list) (dir: int) (mapping : int list) : int list = (
    if dir = 1 then 
      let mapp = List.fold_left ordering ~init:[] ~f:(fun acc i -> (List.nth_exn mapping i)::acc) in 
      List.rev(mapp)
    else (
      let indexed_map : (int*int) list = List.mapi mapping ~f:(fun i a -> (i,a)) in 
      let idx_map = List.map indexed_map ~f:(fun (i,a) -> (List.nth_exn ordering i,a)) in
      let sorted_list = List.sort idx_map ~compare:(fun (i,_) (j,_) -> Int.compare i j) in 
      let final = List.map sorted_list ~f:(fun (_,a) -> a) in
      final 
      )
  ) in
  let stm_to_qasm = fun (instr) (mapping) (acc) : (program * int list) ->
    match instr with
      | QS.Tensor l -> ( acc@(gentensor l mapping 0), mapping)
      | QS.Junc (j,d) -> (acc, permute (List.map j ~f:(Int32.to_int_exn)) (Int32.to_int_exn d) mapping)
      | QS.Gate g -> ( acc@(gengate g 0 mapping), mapping)
  in
  let (prog, _map) = List.fold_left _qst ~init:(([], order)) ~f:(fun (l, m) i -> stm_to_qasm (Mark.data i) m l ) in
  prog
;;


let pp_qasm (p : program) : unit = 
  let pp_stm (s : qasm_instr) : string = (
    match s with
      | H i -> sprintf "h\t[%d]" (i)
      | X i -> sprintf "x\t[%d]" (i)
      | Y i -> sprintf "y\t[%d]" (i)
      | Z i -> sprintf "z\t[%d]" (i)
      | S i -> sprintf "s\t[%d]" (i)
      | Sdag i -> sprintf "sdag\t[%d]" (i)
      | T i -> sprintf "t\t[%d]" (i)
      | Tdag i -> sprintf "tdag\t[%d]" (i)
      | CNOT (i,j) ->  sprintf "cnot\t[%d, %d]" (i) (j)
  )in
  printf "\nQASM Code: \n";
  List.iter p ~f:(fun s -> printf "%s\n" (pp_stm s))
;;

let rec gen_measure_all (c : int) (i : int) : string = (
  if Int.equal c i then "" else (sprintf "measure q[%d] -> out[%d];\n" (c) (c))^(gen_measure_all (c+1) i)
)
;;

let qasm_to_string (_program : program) (i : int) =
  let measure_all = gen_measure_all 0 i in
  (sprintf ("OPENQASM 2.0;\ninclude \"qelib1.inc\";\n"))::
  (sprintf "qreg q[%d];\n" (i))::
  (sprintf "creg out[%d];\n\n" (i))
  ::(List.map _program ~f:(fun instr -> match instr with
    | H i -> sprintf "h\tq[%d];\n" (i)
    | X i -> sprintf "x\tq[%d];\n" (i)
    | Y i -> sprintf "y\tq[%d];\n" (i)
    | Z i -> sprintf "z\tq[%d];\n" (i)
    | S i -> sprintf "s\tq[%d];\n" (i)
    | Sdag i -> sprintf "sdag\tq[%d];\n" (i)
    | T i -> sprintf "t\tq[%d];\n" (i)
    | Tdag i -> sprintf "tdag\tq[%d];\n" (i)
    | CNOT (i,j) -> sprintf "cx\tq[%d], q[%d];\n" (i) (j)
  ))@["barrier q;\n"; measure_all]
;;