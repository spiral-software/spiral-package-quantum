# Copyright 2018-2019, Carnegie Mellon University
# See LICENSE for details
#F
#F SPL Objects for directly-implementable gates
#F


#F -----------------------------------------------------------------------------
#F H - 2x2 Hadamard SPL object
#F -----------------------------------------------------------------------------
Class(qH, Sym, rec(
    def := () ->  Sqrt(1/2) * Mat([[1,1], [1,-1]]),
    isReal := False,
    isPermutation := False,
    transpose := self >> self,
    conjTranspose := self >> self,
    inverse := self >> self,
    toAMat := self >> self.def(),
));


#F -----------------------------------------------------------------------------
#F X - 2x2 180 degree rotation along the X axis of the bloch sphere
#F Specific case of Rotx with theta = pi
#F -----------------------------------------------------------------------------
Class(qX, Sym, rec(
    def := () -> Mat( [[0, 1], [1, 0]] ),
    isReal := False,
    isPermutation := False,
    transpose := self >> self,
    conjTranspose := self >> self,
    inverse := self >> self,
    toAMat := self >> self.def(),
));


#F -----------------------------------------------------------------------------
#F Y - 2x2 180 degree rotation along the Y axis of the bloch sphere
#F Specific case of Roty with theta = pi
#F -----------------------------------------------------------------------------
Class(qY, Sym, rec(
    def := () -> Mat( [[0, E(2)], [-E(2), 0]] ), #Mat( [[0, Cplx(0, -1)], [Cplx(0, 1), 0]] ),
    isReal := False,
    isPermutation := False,
    transpose := self >> Mat( [[0, -E(2)], [E(2), 0]] ), #Mat( [[0, Cplx(0, 1)], [Cplx(0, -1), 0]] ),
    conjTranspose := self >> Mat( [[0, -E(2)], [E(2), 0]] ), #Mat( [[0, Cplx(0, 1)], [Cplx(0, -1), 0]] ),
    inverse := self >> self,
    toAMat := self >> self.def(),
));


#F -----------------------------------------------------------------------------
#F Z - 2x2 180 degree rotation along the Z axis of the bloch sphere
#F Specific case of Rotz with theta = pi
#F -----------------------------------------------------------------------------
Class(qZ, Sym, rec(
    def := () -> Mat( [[1, 0], [0, -1]] ),
    isReal := False,
    isPermutation := False,
    transpose := self >> self,
    conjTranspose := self >> self,
    inverse := self >> self,
    toAMat := self >> self.def(),
));

#F -----------------------------------------------------------------------------
#F S - 2x2 90 degree rotation along the Z axis of the bloch sphere
#F Specific case of Rotz with theta = pi/2, or commonly notated as sqrt(Z)
#F -----------------------------------------------------------------------------
Class(qS, Sym, rec(
    def := () -> Mat( [[1, 0], [0, -E(2)]] ), #Mat( [[1, 0], [0, Cplx(0, 1)]] ),
    isReal := False,
    isPermutation := False,
    transpose := self >> self,
    conjTranspose := self >> Mat( [[1, 0], [0, E(2)]] ), 
    inverse := self >> Mat( [[1, 0], [0, E(2)]] ), 
    toAMat := self >> self.def(),
));

#F -----------------------------------------------------------------------------
#F Sdag - 2x2 -90 degree rotation along the Z axis of the bloch sphere
#F Specific case of Rotz with theta = -pi/2, or commonly notated as -sqrt(Z)
#F -----------------------------------------------------------------------------
Class(qSdag, Sym, rec(
    def := () -> Mat( [[1, 0], [0, E(2)]] ), #Mat( [[1, 0], [0, Cplx(0, -1)]] ),
    isReal := False,
    isPermutation := False,
    transpose := self >> self,
    conjTranspose := self >> qS(), 
    inverse := self >> qS(), 
    toAMat := self >> self.def(),
));


#F -----------------------------------------------------------------------------
#F T - 2x2 45 degree rotation along the Z axis of the bloch sphere
#F Specific case of Rotz with theta = pi/4, or commonly notated as sqrt(S)
#F -----------------------------------------------------------------------------
Class(qT, Sym, rec(
    def := () -> Mat( [[1, 0], [0, E(8)]] ),
    isReal := False,
    isPermutation := False,
    transpose := self >> self,
    conjTranspose := self >> Mat( [[1, 0], [0, E(8)^3]] ), 
    inverse := self >> Mat( [[1, 0], [0, E(8)^3]] ), 
    toAMat := self >> self.def(),
));

#F -----------------------------------------------------------------------------
#F Tdag - 2x2 -90 degree rotation along the Z axis of the bloch sphere
#F Specific case of Rotz with theta = -pi/4, or commonly notated as sqrt(S)
#F -----------------------------------------------------------------------------
Class(qTdag, Sym, rec(
    def := () -> Mat( [[1, 0], [0, E(8)^3]] ),
    isReal := False,
    isPermutation := False,
    transpose := self >> self,
    conjTranspose := self >> qT(), 
    inverse := self >> qT(), 
    toAMat := self >> self.def(),
));



##
#F GenSwapMat( <i>, <j>, <n> )
## 
## Helper functioj to create a matrix swapping qubits i and j
GenSwapMat := function (i, j, n)
    local mi, mj, swap, pre;
    mi := Minimum([i, j]);
    mj := Maximum([i, j]);
    if (i = j) then 
        return I(2^(n));
    fi;
    pre := Tensor(   Tensor(I(2^(mj-1)), L(4,2)),    I(2^(n-mj-1)) );
    swap := pre * GenSwapMat(mi, mj-1, n) * pre;
    return swap;
end;

##
#F PerfSwap( <target>, <l>, <n> )
## 
## generates a 2^n by 2^n matrix that swaps the qubits in target with those in l
## *Not* conscious of the architecture, just generates the equivalent matrix for terminate()
PerfSwap := function (target, l, n)
    local swap, qub, e, a, ls, tmp_l, r, idx;
    swap := I(2^n);
    qub := 1;
    tmp_l := l;
    # Generate the forward swap matrix, 
    for e in tmp_l do 
        swap := swap * GenSwapMat(target[qub], e, n);
        ls := SplitAt(tmp_l, 1)[2];
        idx := 1;
        for r in ls do
            # if we affected another qubit in our list
            if r = qub then
                tmp_l[idx] := e;
            fi;
            idx := idx + 1;
        od;
        qub := qub + 1;
    od;
    return swap;
end;

##
#F JuncTerm( <l>, <n>, <dir> )
## 
## Terminate a Junction object into a permutation matrix
JuncTerm := function (l, n, dir)
    local  size, swap1, swap2, trans, tmp_l;
    size := Length(l);
    tmp_l := [1..Length(l)];
    tmp_l := Map(tmp_l, i -> l[i]);
    # Generate swap matrix
    swap1 := PerfSwap([0..size-1], tmp_l, n);
    # Generate the reverse swap matrix
    swap2 := PerfSwap(Reversed([0..size-1]), Reversed(tmp_l), n);
    if (dir >= 0) then 
        return swap1;
    fi;
    return swap2;
end;

#F -----------------------------------------------------------------------------
#F Junc - reordering of the logical qubit vector (l is the ordering, d is the direction)
#F -----------------------------------------------------------------------------
Class(Junc, Sym, rec(
    def := (l, d) -> JuncTerm(l, Length(l), d),
    isReal := False,
    isPermutation := False,
    inverse := self >> self.def(self.params[1], -self.params[2]), 
    toAMat := self >> self.def(self.params[1], self.params[2]),
));

#F -----------------------------------------------------------------------------
#F Reord - reordering of the physical qubit positions (l = ordering, arch = architecture, d = direction)
#F -----------------------------------------------------------------------------
Class(Reord, Sym, rec(
    def := (l, arch, d) -> JuncTerm(l, Length(l), d),
    isReal := False,
    isPermutation := False,
    inverse := self >> self.def(self.params[1], -self.params[2]), 
    toAMat := self >> self.def(self.params[1], self.params[2]),
));

