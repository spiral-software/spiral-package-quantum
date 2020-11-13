# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Quantum Hadamard transform
#


#F qHT( n ) - Hadamard Gate non-terminal
#F Definition: (2^n x 2^n)-matrix  that applies an n-point Hadamard Transform to the target qubits
#F Note:       qHT(1) denotes the matrix 1/sqrt(2)* [[1, 1], [1, -1]],
#F             qHT(_) is symmetric.
#F
#F qHT( n ) -> an n-qubit Hadamard transform
Class(qHT, TaggedNonTerminal, rec(
  abbrevs   := [ n -> Checked(IsPosInt(n), [n]) ],
  dims      := self >> let(size := 2^self.params[1], [size, size]),
  terminate := self >> Tensor(Replicate(self.params[1], qH())),
  isReal    := self >> true,
  groups    := self >> [Replicate(self.params[1], 1)],
  connected := self >> false,
  rChildren := self >> self.params,
  from_rChildren := (self, rch) >> self.__bases__[1](rch[1]),
  recursive_def := (self, arch) >> self.__bases__[1](self.params[1]),
  SmallRandom := () -> Random([2..5]),
  LargeRandom := () -> Random([6..15]),
  normalizedArithCost := self >> Error("ArithCost not implemented"),
  TType := T_Real(64)
));


##
#F qHT Breakdown Rules
## 
NewRulesFor(qHT, rec(

    # qHT_BinSplit rule
    # qHT_BinSplit qHT_(k) -> (qHT_(k1) tensor I) (I tensor qHT_(k2))
    # k1 + k2 = k
    qHT_BinSplit := rec (
        forTransposition := false,
        minSize          := 2, 
        applicable       := (self, nt) >> nt.params[1] > 1,
        children         := nt -> List( [1..nt.params[1] - 1],  i -> [ Tensor(qHT(i), qHT(nt.params[1]-i)).withTags(nt.getTags()) ]  ), 
        apply            := (nt, c, cnt) -> c[1],
        switch := true,
    ),

    #F qHT_Base: qHT(1) = qH() SPL object
    #F Directly represent as an implementable gate
    qHT_Base := rec(
        info             := "qHT_(1) -> qH()",
        forTransposition := false,
        applicable       := (self, nt) >> nt.params[1]=1,
        apply            := (nt, c, cnt) -> qH(),
    )


));