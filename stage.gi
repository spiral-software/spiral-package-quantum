# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Wrapper object for qCirc that explores global reordering of qubits by 
# permuting the input adjacency matrix
#

##
#F GenStages( <arch>, <circ>) Object
##
#F Given a circuit object and an architecture topology matrix,
#F for every reordering of the physical qubits
#F generate a top-level circuit with that topology
GenStages := function(arch, circ)
    local child, idcs, n, i, ar, c;
    n := Length(arch);
    idcs := [0..n-1];
    idcs := Arrangements(idcs, Length(idcs));
    child := [];
    for i in idcs do
        ar := PruneArch(arch, i);
        c := circ.recursive_def(ar);
        Add(child, [c, ar]);
    od;
    return child;
end;


##
#F BestCircuitReorder( <arch>, <t>, <opts>) Object
##
#F Finds the Best Circuit, but also explores all global reorderings of the
#F Adjacency matrix, skipping duplicate reorderings.
#F TODO: This is non-optimal because the hashtable is discarded across
#F DP calls, ideally we can feed the hashtable from the previous search into the next DP search
#F For research code that is bottlenecked by other reasons, this is acceptable for now
BestCircuitReorder := function(arch, t, opts)
    local gs, child, g, child_cnots, idx, min, c, same, a, arches_checked, i, j, row, bit, already;
    gs := GenStages(arch, t);
    child := [];
    arches_checked := [];
    for g in gs do 
        Print("\n Compiling arch: \n");
        Print(g[2]);
        Print("\n\n");
        already := 0;
        for a in arches_checked do 
            i := 1;
            same := 1;
            for row in a do 
                j := 1;
                for bit in row do
                    if bit <> g[2][i][j] then
                        same := 0;
                    fi;
                    j := j + 1;
                od;
                i := i + 1;
            od;
            if same = 1 then 
                already := 1;
            fi;
        od;
        if already = 1 then
            Print("\n already checked architecture! \n");
        fi;
        if already <> 1 then
            Add(child, BestCircuit(g[1], opts));
            Add(arches_checked, g[2]);
        fi;
    od;
    child_cnots := Map(child, i -> Length(Collect(i, @(1,CNOT))));
    min := Minimum(child_cnots);
    idx := 1;
    for c in child_cnots do
        if c = min then
            return child[idx];
        fi;
        idx := idx + 1;
    od;
    return null;
end;
