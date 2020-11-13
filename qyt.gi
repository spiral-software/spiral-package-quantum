# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# n-qubit Y transforms
#


#F qYT( n ) - Y Gate non-terminal
#F Definition: (2^n x 2^n)-matrix  that applies an n-point Y Transform to the target qubits
#F Note:       qYT(1) denotes the matrix[[0, -i], [i, 0]],
#F             qYT(_) is not symmetric.
#F
#F qYT( n ) -> an n-qubit Y transform
Class(qYT, TaggedNonTerminal, rec(
  abbrevs   := [ n -> Checked(IsPosInt(n), [n]) ],
  dims      := self >> let(size := 2^self.params[1], [size, size]),
  terminate := self >> Tensor(Replicate(self.params[1], qX())),
  isReal    := self >> true,
  groups    := self >> [Replicate(self.params[1], 1)],
  rChildren := self >> self.params,
  from_rChildren := (self, rch) >> self.__bases__[1](rch[1]),
  recursive_def := (self, arch) >> self.__bases__[1](self.params[1]),
  connected := self >> true,
  SmallRandom := () -> Random([2..5]),
  LargeRandom := () -> Random([6..15]),
  normalizedArithCost := self >> Error("ArithCost not implemented"),
  TType := T_Complex(64),
));


NewRulesFor(qYT, rec(

    # qYT_BinSplit rule
    # qYT_BinSplit qYT_(k) -> (qYT_(k1) tensor I) (I tensor qYT_(k2))
    # k1 + k2 = k
    qYT_BinSplit := rec (
        forTransposition := false,
        minSize          := 2, 
        applicable       := (self, nt) >> nt.params[1] > 1,
        children         := nt -> List( [1..nt.params[1] - 1],  i -> [ Tensor(qYT(i), qYT(nt.params[1]-i)).withTags(nt.getTags()) ]  ), 
        apply            := (nt, c, cnt) -> c[1],
        switch := true,
    ),

    #F qYT_Base: qYT(1) = qY() SPL object
    #F Directly represent as an implementable gate
    qYT_Base := rec(
        info             := "qYT_(1) -> qY()",
        forTransposition := false,
        applicable       := (self, nt) >> nt.params[1]=1,
        apply            := (nt, c, cnt) -> qY(),
    )

));