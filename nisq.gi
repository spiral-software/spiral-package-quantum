# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Utility functions for checking properties of the adjacency matrix NISQ topology
#

##
#F GetEdges( <arch>, <q> )
## 
## Gets a list of edges in arch for a target qubit q
GetEdges := function(arch, q)
    local row, inqub, edges, e, edges;
    row := arch[q+1];
    inqub := 0;
    edges := [];
    for e in row do 
        if e = 1 then
            Add(edges, inqub);
        fi;
        inqub := inqub + 1;
    od;
    # now have a list of adjacent qubits to the chosen qubit
    return edges;
end;

##
#F HasEdge( <i>, <j>, <arch> )
## 
#F Checks if there is an edge between qubits i and j in arch
HasEdge:= function (i, j, arch)
    local row;
    row := arch[i+1];
    return row[j+1];
end;

##
#F GetDist( <i>, <j>, <arch> )
## 
#F Returns the minimum distance in the architecture from qubit i to qubit j
GetDist := function (i, j, arch)
    local es, distances, dist, adjmat, n, inc;
    if (i = j) then
        return 0;
    fi;
    adjmat := arch;
    n := Length(arch);
    dist := 1;
    inc := 0;
    while (adjmat[i+1][j+1] <= 0 and inc <= n) do 
        adjmat := adjmat * adjmat;
        dist := dist + 1;
        inc := inc + 1;
    od;
    if inc > n then # not possible, maybe do something better than returning null
        return n+2; #psuedo-null
    fi;
    return dist;
end;

##
#F Best_Move( <i>, <j>, <arch> )
## 
#F Returns the most straightforward swap to perform to bring i and j closer
Best_Move := function (i, j, arch)
    local es, distances, shortest_path, e, best_move, l, tes;
    es := GetEdges(arch, i);
    tes := GetEdges(arch, j);
    if ContainsElement(es, j) then # if immediate neighbors
        return j;
    fi;
    l := Filtered(es, (e)-> ContainsElement(tes, e)); # find the immediate children they share, see if any match
    if l <> [] then 
        return l[1];
    fi;
    # have to calculate distances
    distances := Map(es, e -> (GetDist(e, j, arch)));
    shortest_path := Minimum(distances);
    shortest_path := PositionProperty(distances, (e) -> (e = shortest_path));
    best_move := es[shortest_path];
    return best_move;
end;

##
#F VerifyPath( <target>, <source>, <arch> )
## 
#F verifies that target and source are connected in arch
VerifyPath := function (target, source, arch)
    local i, q;
    i := 1;
    for q in target do 
        if(GetDist(target[i], source[i], arch) > Length(arch)) then 
            return false;
        fi;
        i := i + 1;
    od;
    return true;
end;