# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# n-qubit Z transforms
#


#F qZT( n ) - Z Gate non-terminal
#F Definition: (2^n x 2^n)-matrix  that applies an n-point Z Transform to the target qubits
#F Note:       qZT(1) denotes the matrix[[1, 0], [0, -1]],
#F             qZT(_) is symmetric.
#F
#F qZT( n ) -> an n-qubit Z transform
Class(qZT, TaggedNonTerminal, rec(
  abbrevs   := [ n -> Checked(IsPosInt(n), [n]) ],
  dims      := self >> let(size := 2^self.params[1], [size, size]),
  terminate := self >> Tensor(Replicate(self.params[1], qZ())),
  isReal    := self >> true,
  groups    := self >> [Replicate(self.params[1], 1)],
  rChildren := self >> self.params,
  from_rChildren := (self, rch) >> self.__bases__[1](rch[1]),
  recursive_def := (self, arch) >> self.__bases__[1](self.params[1]),
  SmallRandom := () -> Random([2..5]),
  LargeRandom := () -> Random([6..15]),
  normalizedArithCost := self >> Error("ArithCost not implemented"),
  TType := T_Complex(64)
));


NewRulesFor(qZT, rec(

    # qZT_BinSplit rule
    # qzT_BinSplit qZT_(k) -> (qZT_(k1) tensor I) (I tensor qZT_(k2))
    # k1 + k2 = k
    qZT_BinSplit := rec (
        forTransposition := false,
        minSize          := 2, 
        applicable       := (self, nt) >> nt.params[1] > 1,
        children         := nt -> List( [1..nt.params[1] - 1],  i -> [ Tensor(qZT(i), qZT(nt.params[1]-i)).withTags(nt.getTags()) ]  ), 
        apply            := (nt, c, cnt) -> c[1],
        switch := true,
    ),

    #F qZT_Base: qZT(1) = qZ() SPL object
    #F Directly represent as an implementable gate
    qZT_Base := rec(
        info             := "qZT_(1) -> qZ()",
        forTransposition := false,
        applicable       := (self, nt) >> nt.params[1]=1,
        apply            := (nt, c, cnt) -> qZ(),
    )

));