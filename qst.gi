# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# n-qubit S transform
#


#F qST( n, i ) - S Gate non-terminal
#F Definition: (2^n x 2^n)-matrix  that applies an n-point S Transform to the target qubits
#F Note:       qST(1) denotes the matrix[[1, 0], [0, e^(-ipi/2)]], or [[1,0], [0, -i]]
#F             qZT(_) is symmetric.
#F
#F qST(n , i) -> an n-qubit S transform, or Sdag transform if i = -1
Class(qST, TaggedNonTerminal, rec(
  abbrevs   := [ (n, i) -> Checked(IsPosInt(n), [n, When(i <= -1, -1, 1)]) ],
  dims      := self >> let(size := 2^self.params[1], [size, size]),
  terminate := self >> When(self.params[2] = -1, Tensor(Replicate(self.params[1], qSdag())), Tensor(Replicate(self.params[1], qS()))),
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


NewRulesFor(qST, rec(

    # qST_BinSplit rule
    # qST_BinSplit qST_(k) -> (qST_(k1) tensor I) (I tensor qST_(k2))
    # k1 + k2 = k
    qST_BinSplit := rec (
        forTransposition := false,
        minSize          := 2, 
        applicable       := (self, nt) >> nt.params[1] > 1,
        children         := nt -> List( [1..nt.params[1] - 1],  i -> [ Tensor(qST(i, nt.params[2]), qST(nt.params[1]-i, nt.params[2])).withTags(nt.getTags()) ]  ), 
        apply            := (nt, c, cnt) -> c[1],
        switch := true,
    ),

    #F qST_Base: qST(1, _) = qS() or qS().inverse() SPL object
    #F Directly represent as an implementable gate
    qST_Base := rec(
        info             := "qST_(1, _) -> qS()",
        forTransposition := false,
        applicable       := (self, nt) >> nt.params[1]=1,
        apply            := (nt, c, cnt) -> When(nt.params[2] = -1, qSdag(), qS()),
    )

));