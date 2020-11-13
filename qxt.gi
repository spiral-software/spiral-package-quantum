# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# n-qubit X transforms
#


#F qXT( n ) - X Gate non-terminal
#F Definition: (2^n x 2^n)-matrix  that applies an n-point X Transform to the target qubits
#F Note:       qXT(1) denotes the matrix[[0, 1], [1, 0]],
#F             qXT(_) is symmetric.
#F
#F qXT( n ) -> an n-qubit X transform
Class(qXT, TaggedNonTerminal, rec(
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


NewRulesFor(qXT, rec(

    # qXT_BinSplit rule
    # qXT_BinSplit qXT_(k) -> (qXT_(k1) tensor I) (I tensor qXT_(k2))
    # k1 + k2 = k
    qXT_BinSplit := rec (
        forTransposition := false,
        minSize          := 2, 
        applicable       := (self, nt) >> nt.params[1] > 1,
        children         := nt -> List( [1..nt.params[1] - 1],  i -> [ Tensor(qXT(i), qXT(nt.params[1]-i)).withTags(nt.getTags()) ]  ), 
        apply            := (nt, c, cnt) -> c[1],
        switch := true,
    ),

    #F qXT_Base: qXT(1) = qX() SPL object
    #F Directly represent as an implementable gate
    qXT_Base := rec(
        info             := "qXT_(1) -> qX()",
        forTransposition := false,
        applicable       := (self, nt) >> nt.params[1]=1,
        apply            := (nt, c, cnt) -> qX(),
    )

));