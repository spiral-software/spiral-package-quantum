
Load(quantum);
Import(quantum);
opts := SpiralDefaults;

arch := [ [ 0, 1, 1, 1, 1 ], [ 1, 0, 0, 1, 0 ], [ 1, 0, 0, 0, 1 ], [ 1, 1, 0, 0, 0 ], [ 1, 0, 1, 0, 0 ] ];
t := qCirc(arch, 5, [ [[2], qHT(1)], [[0,4], qCNOT(1, 0, arch)], [[2,4], qCNOT(1,1,arch)], [[0,3], qCNOT(1,1,arch)], [[0,2,4], qHT(3)], [[0,1,2], qHT(3)]]);

#generate random circuit
rt := RandomRuleTree(t, opts);
s := SPLRuleTree(rt);

#generate best circuit
circ := BestCircuit(t, SpiralDefaults);
UnparseQASM(circ);






