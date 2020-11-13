# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Contains the CNOT and SWAP non-terminals and rewrite rules
#

##
#F qSWAPT( <qi>, <qj>, <n>,, <arch> ) Object
##
#F Awaps qi and qj in a system of n qubits, with architecture arch
#F Definition: (2^n x 2^n)-matrix  
#F that swaps qubit qi and qubit qj, with the identity operation applied to all others
Class(qSWAPT, TaggedNonTerminal, rec(
  abbrevs   := [ (qi, qj, n, arch) -> Checked(IsPosInt(n), [qi, qj, n, arch]) ],
  dims      := self >> let(size := 2^self.params[3], [size, size]),
  terminate := self >> GenSwapMat(self.params[1], self.params[2], self.params[3]), 
  isReal    := self >> true,
  rChildren := self >> self.params,
  from_rChildren := (self, rch) >> self.__bases__[1](rch[1], rch[2], rch[3], rch[4]),
  SmallRandom := () -> Random([2..5]),
  LargeRandom := () -> Random([6..15]),
  normalizedArithCost := self >> Error("ArithCost not implemented"),
  TType := T_Real(64)
));

##
#F CnotTerm( <i>, <j>, <n> )
##
## .terminate() function for the qCNOT Non-terminal. Expands qCNOT non-terminal into a 2^n by 2^n matrix
CnotTerm := function (i, j, n)
    return When(i < j, Tensor(Tensor(I(2^(i)), GenCnotMat(j, 0)), I(2^(n-j))), Tensor(Tensor(I(2^(j)), GenCnotMat(i, 1)), I(2^(n-i)))); # create the intended cnot matrix
end;

##
#F CnotSPL( <i>, <j>, <n> )
##
## Used for the qCNOT_Base breakdown rule. Represents a qCNOT Non-terminal as a tensor of CNOT and I SPL objects
CnotSPL := function (i, j, n)
    return When(i < j, Tensor(Tensor(I(2^(i)), CNOT(j-i, 0)), I(2^(n-1-j))), Tensor(Tensor(I(2^(j)), CNOT(i-j, 1)), I(2^(n-1-i)))); # create the intended cnot matrix
end;

##
#F qCNOT( <qj>, <dir>, <arch> ) Object
##
#F CNOT Non-terminal
#F Definition: (2^n x 2^n)-matrix  that applies a CNOT from qubit 0 to j 
Class(qCNOT, TaggedNonTerminal, rec(
  abbrevs   := [ (qj, dir, arch) -> Checked([qj, dir, arch]) ],
  dims      := self >> let(size := 2^(self.params[1]+1), [size, size]),
  terminate := self >> GenCnotMat(self.params[1], self.params[2]),
  isReal    := self >> true,
  groups    := self >> [[2]],
  recursive_def := (self, arch) >> self.__bases__[1](self.params[1], self.params[2], arch),
  rChildren := self >> self.params,
  from_rChildren := (self, rch) >> self.__bases__[1](rch[1], rch[2], rch[3]),
  connected := self >> true,
  SmallRandom := () -> Random([2..5]),
  LargeRandom := () -> Random([6..15]),
  normalizedArithCost := self >> Error("ArithCost not implemented"),
  TType := T_Real(64)
));


##
#F qCNOT Breakdown Rules
## 
NewRulesFor(qCNOT, rec(
 
    #F qCNOT_Base: qCNOT(_) =  CNOT() SPL object
    #F Directly represent as an implementable gate, if a proper edge exists
    qCNOT_Base := rec(
        info             := "qCNOT_(_) -> CNOT(_)",
        forTransposition := false,
        applicable       := (self, nt) >> (HasEdge(0, nt.params[1], nt.params[3]) = 1),
        apply            := (nt, c, cnt) -> CNOT(nt.params[1], nt.params[2]),
    )

));

##
#F CnotSPL( <i>, <j>, <n> )
##
## Used for the qSWAP_Base breakdown rule. Represents a qCNOT Non-terminal as a tensor of CNOT and I SPL objects
CnotSPL := function (i, j, n)
    return When(i < j, Tensor(Tensor(I(2^(i)), CNOT(j-i, 0)), I(2^(n-1-j))), Tensor(Tensor(I(2^(j)), CNOT(i-j, 1)), I(2^(n-1-i)))); # create the intended cnot matrix
end;

##
#F SwapGenChildren( <start>, <ending>, <n>, <arch> )
##
## decompose a qSWAPT transform into smaller qSWAPT transforms
SwapGenChildren := function (start, ending, n, arch)
    local move, children;
    move := Best_Move(start, ending, arch);
    children := [];
    Add(children, [qSWAPT(start, move, n, arch) * qSWAPT(move, ending, n, arch) * qSWAPT(start, move, n, arch)]);
    return children;
end;

##
#F qSWAPT Breakdown Rules
## 
NewRulesFor(qSWAPT, rec(

    #F qSWAPT_Base: qSWAPT(_) =  qCNOT() SPL objects
    #F SWAP to CNOT conversion, only if an edge exists
    #F represent a swap as a series of 3 CNOTs with the middle being an alternate direction
    qSWAPT_Base := rec(
        info             := "qSWAPT() -> CNOT * CNOT * CNOT ",
        forTransposition := false,
        applicable       := (self, nt) >> ( (HasEdge(nt.params[1], nt.params[2], nt.params[4]) = 1) and (nt.params[1] <> nt.params[2])),
        children         := nt -> List( [ [ (CnotSPL(nt.params[1], nt.params[2], nt.params[3]))*(CnotSPL(nt.params[2], nt.params[1], nt.params[3]))*(CnotSPL(nt.params[1], nt.params[2], nt.params[3])) ] , [ (CnotSPL(nt.params[2], nt.params[1], nt.params[3]))*(CnotSPL(nt.params[1], nt.params[2], nt.params[3]))*(CnotSPL(nt.params[2], nt.params[1], nt.params[3])) ]  ] ), 
        apply            := (nt, c, cnt) -> c[1],
    ),


    #F qSWAPT_ID: qSWAPT(_) =  I() SPL object
    #F SWAP to Identity conversion if the swap is trivial
    qSWAPT_Id := rec(
        info             := "qSWAPT() -> Identity ",
        forTransposition := false,
        applicable       := (self, nt) >> (nt.params[1] = nt.params[2]),
        apply            := (nt, c, cnt) -> I(2^(nt.params[3])),
    ),

    #F qSWAPT_Rec: qSWAPT(_) =   qSWAPT(_) *  qSWAPT(_) 
    #F Break down an unimplementable qSWAP into an implementable swap and a swap that is 1 step closer to being implementable
    #F TODO, make this a bit more intelligent, right now SwapGenChildren just does a random walk
    qSWAPT_Rec := rec(
        info             := "qSWAPT() -> qSWAPT() * qSWAPT()",
        forTransposition := false,
        applicable       := (self, nt) >> ((nt.params[1] <> nt.params[2]) and (HasEdge(nt.params[1], nt.params[2], nt.params[4]) = 0)),
        children         := nt -> SwapGenChildren(nt.params[1], nt.params[2], nt.params[3], nt.params[4]), 
        apply            := (nt, c, cnt) -> c[1],
    )

));

##
#F ShiftMat( <swap1>, <swap2>, <arch>, <n> )
##
## generates a martix of SWAP operations that swap the qubit groups swap1 and swap2 in parallel
## swap1 is the target position, and swap2 are original positions
ShiftMat := function (swap1, swap2, arch, n)
    local idx, format, backmat, targets, t, positions, e, sw1, sw2, s, i, test, already_swapped;
    idx := 1;
    format := I(2^(n));
    backmat := I(2^(n));
    positions := [];
    targets := [];
    for s in swap1 do 
        Add(positions, s);
    od;
    for t in swap2 do 
        Add(targets, t);
    od;
    for s in swap1 do
        if ((targets[idx] <> positions[idx])) then # just save the extra trouble, if the swap is trivial
            format := format * qSWAPT(positions[idx], targets[idx], n ,arch);
            backmat := qSWAPT(positions[idx], targets[idx], n , arch) * backmat;
            sw1 := targets[idx];
            sw2 := positions[idx];
            i := 1;
            for e in positions do 
                if e = sw1 then     
                    positions[i] := sw2;
                fi;
                if e = sw2 then     
                    positions[i] := sw1;
                fi;
                i := i + 1;
            od;
        fi;
        idx := idx + 1;
    od;
    return [format, backmat];
end;

