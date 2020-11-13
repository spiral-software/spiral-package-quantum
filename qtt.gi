# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# n-qubit T and Tdag transforms
#


#F qTT( n, i ) - T Gate non-terminal
#F Definition: (2^n x 2^n)-matrix  that applies an n-point T Transform to the target qubits
#F Note:       qTT(1, 1) denotes the matrix[[1, 0], [0, e^(-ipi/4)]], and qTT(1, -1) denotes the matrix[[1, 0], [0, e^(ipi/4)]]
#F             qTT(_) is symmetric.
#F
#F qTT(n , i) -> an n-qubit T transform, or Tdag transform if i = -1
Class(qTT, TaggedNonTerminal, rec(
  abbrevs   := [ (n, i) -> Checked(IsPosInt(n), [n, When(i <= -1, -1, 1)]) ],
  dims      := self >> let(size := 2^self.params[1], [size, size]),
  terminate := self >> When(self.params[2] = -1, Tensor(Replicate(self.params[1], qTdag())), Tensor(Replicate(self.params[1], qT()))),
  isReal    := self >> false,
  rChildren := self >> self.params,
  from_rChildren := (self, rch) >> self.__bases__[1](rch[1], rch[2]),
  groups    := self >> [Replicate(self.params[1], 1)],
  recursive_def := (self, arch) >> self.__bases__[1](self.params[1], self.params[2]),
  SmallRandom := () -> Random([2..5]),
  LargeRandom := () -> Random([6..15]),
  normalizedArithCost := self >> Error("ArithCost not implemented"),
  TType := T_Complex(64),
));


NewRulesFor(qTT, rec(

    # qTT_BinSplit rule
    # qTT_BinSplit qTT_(k) -> (qTT_(k1) tensor I) (I tensor qTT_(k2))
    # k1 + k2 = k
    qTT_BinSplit := rec (
        forTransposition := false,
        minSize          := 2, 
        applicable       := (self, nt) >> nt.params[1] > 1,
        children         := nt -> List( [1..nt.params[1] - 1],  i -> [ Tensor(qTT(i, nt.params[2]), qTT(nt.params[1]-i, nt.params[2])).withTags(nt.getTags()) ]  ), 
        apply            := (nt, c, cnt) -> c[1],
        switch := true,
    ),

    #F qTT_Base: qTT(1, _) = qT() or qT().inverse() (i.e. qTdag) SPL object
    #F Directly represent as an implementable gate
    qTT_Base := rec(
        info             := "qTT_(1, _) -> qT()",
        forTransposition := false,
        applicable       := (self, nt) >> nt.params[1]=1,
        apply            := (nt, c, cnt) -> When(nt.params[2] = -1, qTdag(), qT()),
    )

));