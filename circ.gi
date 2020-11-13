# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Top-level qCirc object and associated helpers
#

##
#F CircTerm( <ops>, <n>, <arch> )
## 
#F given a list of operations to perform on  target qubits, the number of qubits, and the hardware architecture,
#F expand out the qCircuit object as a matrix product of qEmbed objects
##
CircTerm := function(ops, n, arch) 
    local mat, h, oper;
    mat := I(2^(n));
    for oper in ops do
        # oper is [[1,2,3], qHT], for example
        h := qEmbed(oper[1], n, arch, oper[2]).terminate();
        mat := mat * h;
    od;
    return mat;
end;


##
#F ArchUpdate( <oplist>, <arch> )
## 
#F updates all operations in oplist to use the new architecture
##
ArchUpdate := function(oplist, arch) 
    local newlist, ops, qubits, T;
    newlist := [];
    for ops in oplist do
        # op is [[qubits], Transform]
        qubits := ops[1];
        T := ops[2].recursive_def(arch);
        Add(newlist, [qubits, T]);
    od;
    return newlist;
end;

##
#F qCirc( <arch>, <n>, <ops>) Object
##
#F Given architecture arch and number of qubits n, embed each operation in ops
#F an operation is a [[qubit list], <Non-Terminal>] list
#F ops in an operation list
Class(qCirc, TaggedNonTerminal, rec(
  abbrevs   := [ (arch, n, ops) -> Checked(IsPosInt(n), [arch, n, ops]) ],
  dims      := self >> let(size := 2^self.params[2], [size, size]),
  terminate := self >> CircTerm(self.params[3], self.params[2], self.params[1]),
  isReal    := self >> false,
  recursive_def := (self, arch) >> self.__bases__[1](arch, self.params[2], ArchUpdate(self.params[3], arch) ),
  rChildren := self >> self.params,
  from_rChildren := (self, rch) >> self.__bases__[1](rch[1], rch[2], rch[3]),
  SmallRandom := () -> Random([2..5]),
  LargeRandom := () -> Random([6..15]),
  normalizedArithCost := self >> Error("ArithCost not implemented"),
  TType := T_Complex(64)
));

##
#F GenCircChildren( <arch>, <n>, <ops> )
## 
#F given a list of operations to perform on target qubits, the number of qubits, and the hardware architecture,
#F expand out the qCircuit object as a matrix product of qEmbed objects
##
GenCircChildren := function(arch, n, ops)
    local current_op, ops, h1, h, rest;
    current_op := ops[1];
    h1 := qEmbed(current_op[1], n, arch, current_op[2]);
    h := h1;
    if Length(ops) > 1 then 
        rest := [2..Length(ops)];
        rest := Map(rest, i -> ops[i]);
        h := h * qCirc(arch, n, rest);
    fi;
    return [h];
end;


##
#F qCirc Breakdown Rules
## 
NewRulesFor(qCirc, rec(

    # qCirc_Expand rule
    # qCirc_Expand qCirc(arch, n, ops) -> qEmbed(qubits, n, arch, <Non-Terminal>) * ...
    qCirc_Expand := rec (
        forTransposition := false,
        minSize          := 2, 
        applicable       := (self, nt) >> (nt.params[3] <> []),
        children         := nt -> List([GenCircChildren(nt.params[1], nt.params[2], nt.params[3])]),
        apply            := (nt, c, cnt) -> c[1],
        switch := true,
    ),

    # qCirc_Base rule
    # QCirc_Base qCirc(arch, n, []) -> I(2^n)
    qCirc_Base := rec(
        info             := "qCirc -> Identity",
        forTransposition := false,
        applicable       := (self, nt) >> (nt.params[3] = []),
        apply            := (nt, c, cnt) -> I(2^(nt.params[2])),
    )

));