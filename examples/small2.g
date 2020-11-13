
Load(quantum);
Import(quantum);
opts := SpiralDefaults;


arch := [[0, 1, 0], [1, 0, 1], [0, 1, 0]];
t := qCirc(arch, 3, [ [[0,1], qHT(2)], [[0,2], qCNOT(1, 0, arch)], [[0,1], qCNOT(1, 0, arch)], [[2], qHT(1)] ] );

#generate random circuit
rt := RandomRuleTree(t, opts);
s := SPLRuleTree(rt);

#generate best circuit
circ := BestCircuit(t, opts);
UnparseQASM(circ);









