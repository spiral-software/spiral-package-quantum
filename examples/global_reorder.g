
Load(quantum);
Import(quantum);
opts := SpiralDefaults;

opts := SpiralDefaults;
arch := [[0,1,1,0,0], [1,0,1,0,0], [1,1,0,1,1], [0,0,1,0,1], [0,0,1,1,0]];
t := qCirc(arch, 5, [ [[4,2], qCNOT(1, 0, arch)], [[4,1], qCNOT(1, 0, arch)], [[3,4], qCNOT(1, 0, arch)], [[0,1], qCNOT(1, 0, arch)], [[1,2], qCNOT(1, 0, arch)], [[0,2,4], qHT(3)], [[3,1], qCNOT(1, 0, arch)] ]);

# will take a long time due to hash-tables not being shared, but explores globally reordered qubits
circ := BestCircuitReorder(arch, t, opts);
# in this circuit in particular, one extra CNOT remains uncancelled
# Porting the Unparser from OCAML into the SPIRAL framework will allow us to re-apply the rewrite stage after 
# Unrolling Junctions, which trivially eliminates this issue. This approach is not taken in this preliminary
# Version due to software engineering constraints.
UnparseQASM(circ);






