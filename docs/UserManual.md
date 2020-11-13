# spiral-package-quantum

User Manual. Visit the rest at: http://www.spiral.net/doc/usermanual/

- Quantum Circuits in SPIRAL
    - Quantum Gate Primitives
    - Quantum Transforms
        - Basic Transforms
        - Ongoing Work
    - Intermediate Transforms
        - qCirc
        - qEmbed
    - Rewrite Rules
    - Optimizing Circuits
    - Unparser




Quantum Gate Primitives
------------

#spiral-software\namespaces\packages\quantum\qspl.gi

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


Quantum Transforms
------------------

- Basic Transforms

#spiral-software\namespaces\packages\quantum\qht.gi

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

- Ongoing Work

Currently integrating Higher-level transforms like QFT into the SPIRAL Framework


Intermediate Transforms
-------------

- qCirc

#spiral-software\namespaces\packages\quantum\circ.gi

#
#F qCirc( <arch>, <n>, <ops>) Object
#
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
  TType := T_Complex(64)
));

- qEmbed

#spiral-software\namespaces\packages\quantum\embed.gi

#
#F qEmbed( <l>, <n>, <arch>, <Obj> ) Object
#
#F Embed the Obj Non-Terminal into the system of n qubits with given architecture arch
#F applied to the qubits in l
#F Represents a (2^n x 2^n)-matrix
Class(qEmbed, TaggedNonTerminal, rec(
  abbrevs   := [ (l, n, arch, nt) -> Checked(IsPosInt(n) , [l, n, arch, nt])],
  dims      := self >> let(size := 2^self.params[2], [size, size]),
  terminate := self >> (ExpandTens(self.params[1], self.params[2], self.params[4])),
  isReal    := self >> false,
  rChildren := self >> self.params,
  from_rChildren := (self, rch) >> self.__bases__[1](rch[1], rch[2], rch[3], rch[4]),
  recursive_def := (self, arch) >> self.__bases__[1](self.params[1], arch, self.params[2], arch, self.params[3]),
  SmallRandom := () -> Random([2..5]),
  LargeRandom := () -> Random([6..15]),
  normalizedArithCost := self >> Error("ArithCost not implemented"),
  TType := T_Complex(64)
));

Rewrite Rules
--------------

#spiral-software\namespaces\packages\quantum\qrewrite.gi

##
#F Quantum_Format ruleset
## 
Class(Quantum_Format, RuleSet);
RewriteRules(Quantum_Format, rec(
    # Remove Grp structure 
    remove_grp := ARule( Grp, [@(1)], x-> [(@(1).val)] ),

    # Flatten Tensors
    flatten_tensor_asoc := ARule(Tensor, [@(1), @(2).cond(e-> ObjId(@(1).val)=Tensor or ObjId(e)=Tensor)],
	    e -> let(
	           ch1 := Cond(ObjId(@(1).val)=Tensor, @(1).val._children, [@(1).val]),
	           ch2 := Cond(ObjId(@(2).val)=Tensor, @(2).val._children, [@(2).val]),
	           ch1::ch2
	         )),

    #Split identities into I(2)
    split_identity := ARule(Tensor, [@(1),  @(2).cond(e-> (CheckIdentitySplit(@(1).val, e)) )],
	    e -> let(
	           ch1 := Cond(ObjId(@(1).val)=I and @(1).val.params[1] > 2, Tensor(I(2), I(@(1).val.params[1]/2)), [@(1).val]),
	           ch2 := Cond(ObjId(@(2).val)=I and @(2).val.params[1] > 2, Tensor(I(2), I(@(2).val.params[1]/2)), [@(2).val]),
               [ch1]::[ch2]
	         )),
));

##
#F QuantumRewrite( <s>, <opts> )
##
## Simplifies an SPL expression representing a quantum circuit (s)
QuantumRewrite := function (s, opts)
    s := ApplyStrategy(s, [Quantum_Format], BUA, opts); 
    s := ApplyStrategy(s, [Quantum_Simplify], BUA, opts);
    s := ApplyStrategy(s, [Quantum_Reorder], BUA, opts);
    s := ApplyStrategy(s, [Quantum_Format], BUA, opts); 
    s := ApplyStrategy(s, [Quantum_Simplify], BUA, opts);
    s := ApplyStrategy(s, [Quantum_Terminate], BUA, opts);
    return s;
end;


Optimizing Quantum Circuits
-----------------------------

#spiral-software\namespaces\packages\quantum\qrewrite.gi

##
#F BestCircuit( <t>, <opts> )
##
## Finds the best circuit implementing transform t
## Hardcoded to count number CNOT gates as the cost function
BestCircuit := function (t, opts)
    local dpopts, best, s, cnot_list;
    dpopts := rec(verbosity := 1, hashTable := HashTableDP()); 
    dpopts.measureFunction := (rt, opts) ->
        let(c := SPLRuleTree(rt), # generate SPL Ruletree and then run a collect on CNOTs 
            c2 := QuantumRewrite(c, opts),
            Length(Collect(c2, @(1,CNOT)))
            ); 
    best := DP(t, dpopts, opts);
    Print("\n SPL \n");
    Print(SPLRuleTree(best[1].ruletree));
    Print("\n");
    s := QuantumRewrite(SPLRuleTree(best[1].ruletree), opts);
    Print("\ncost :");
    Print(Length(Collect(s, @(1,CNOT))));
    Print("\n");
    Print("---------------- BestCircuit: \n");
    return s;
end;

- Example:

Load(quantum);
Import(quantum);
opts := SpiralDefaults; 
arch := [[0, 1, 0], [1, 0, 1], [0, 1, 0]];
t := qCirc(arch, 3, [ [[0,1], qHT(2)], [[0,2], qCNOT(1, 0, arch)]  ] );
rt := RandomRuleTree(t, opts);
s := SPLRuleTree(rt);
circ := BestCircuit(t, opts);


Unparser
----------

#spiral-software\namespaces\packages\quantum\unparse.gi

UnparseQASM := function (spl)
    PrintTo("./qspiralout", spl);
    Exec( "./namespaces/packages/quantum/unparser/bin/unparser ./qspiralout"); 
end;

Build OCAML source separately in

#spiral-software\namespaces\packages\quantum\unparser\

Current software integration efforts are underway to port the unparser to SPIRAL’s existing framework, which would allow for the rewrite stage to be applied in the backend for additional optimizations.



