# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# qEmbed Object, qJunc Object
# All objects and helpers needed to embed a logical transform onto a physical mesh
#

##
#F qJunc( <qubit_order>, <n>, <dir>) Object
##
#F Given architecture ordering qubit_order and number of qubits n
#F reorder logical qubit vector to achieve the relative ordering
#F dir = 1 is a forward reordering, dir = -1 is a backward reordering
Class(qJunc, TaggedNonTerminal, rec(
  abbrevs   := [ (l, n, dir) -> Checked(IsPosInt(n) , [l, n, dir])],
  dims      := self >> let(size := 2^self.params[2], [size, size]),
  terminate := self >> (JuncTerm(self.params[1], self.params[2], self.params[3])),
  isReal    := self >> false,
  rChildren := self >> self.params,
  from_rChildren := (self, rch) >> self.__bases__[1](rch[1], rch[2], rch[3]),
  SmallRandom := () -> Random([2..5]),
  LargeRandom := () -> Random([6..15]),
  normalizedArithCost := self >> Error("ArithCost not implemented"),
  TType := T_Complex(64)
));

##
#F GrowGroup( <arch>, <groups>, <bad> )
## 
#F given an architecture and a list of qubit groupings, grow each grouping by 1, 
#F not using any qubits in the bad set. Used as a helper function to grab 
#F all sections of n connected qubits
##
GrowGroup := function(arch, groups, bad) 
    local newgroups, g, q, es, newqs, newlocalgroups, n, group;
    newgroups := [];
    for g in groups do
        # for each group
        for q in g do
            #for each qubit in each group
            #get a list of edges
            es := GetEdges(arch, q);
            newqs := ListDifference(es, g);
            #newqs is the list of new qubits we can add to this group
            newqs := ListDifference(newqs, bad);
            # dont use any bad qubits
            newlocalgroups := Map(newqs, i -> Concatenation(g, [i]));
            Append(newgroups, newlocalgroups);
        od;
    od;
    return newgroups;
end;

##
#F FindAllSectionsOfn( <arch>, <n>, <bad> )
## 
#F given an architecture and a set of off-limits qubits, 
#F finds all possible size-n connected components in the architecture 
#F using none of the bad qubits
FindAllSectionsOfn := function(arch, n, bad) 
    local l, qub, groups, count, g;
    l := [];
    qub := 0;
    groups := [0..(Length(arch)-1)];
    groups := ListDifference(groups, bad);
    groups := Map(groups, i -> [i]);
    count := 1;
    while count < n do 
        groups := GrowGroup(arch, groups, bad);
        count := count + 1;
    od;
    return groups;
end;

##
#F GenAllPartitions(<arch>, <S>)
## 
#F given a list of different partitions sizes, for example S = [[3,3], [3,2]]
#F returns a list of all possible partitionings 
#F for example S = [[3,3], [3,2]] will yield a list of 2 sublists, 
#F the first sublist contains all possible ways of getting two connected components of 3
#F the second sublist contains all possible ways of getting a connected component of size 3 and size 2
GenAllPartitions := function (arch, S)
    local parts, groupings, s, sects, r, fact, groups, ss, result, g, idx, newgroups, bad, k;
    parts := S.groups();
    groupings := [];
    for s in parts do 
        # now s is a list that contains all the needed connected subsets for the transform
        # a.k.a [3, 3]
        sects := FindAllSectionsOfn(arch, s[1], []);
        idx := 2;
        result := Map(sects, i ->[i]);
        while idx <= Length(s) do
            newgroups := [];
            # need to add a group of size g to every list in list groups
            for r in result do
                # add a size g grouping to this list
                # r is [ [1,2,3], ... ]
                bad := Flat(r);
                # don't use anything already used
                sects := FindAllSectionsOfn(arch, s[idx], bad);
                for k in sects do
                    Add(newgroups, Concatenation(r, [k]));
                od;
            od;
            result := newgroups;
            idx := idx + 1;
        od;
        #finished preparing all possible groups to satisfy the current partition
        Append(groupings, result);
    od;
    return groupings;
end;

##
#F PruneArch(<arch>, <in_use>)
## 
#F take all rows and columns out of the arch adjmatrix that are not in in_use
#F and reorder rows and columns so that in_use[1] is qubit 0, in_use[2] is qubit 1, etc...
#F to achieve our recursive reordering
PruneArch := function(arch, in_use)
    local idcs, idx, j, row, newarch, u, r, newrow;
    idcs := [0..(Length(in_use)-1)];
    idx := 1;
    newarch := [];
    for u in in_use do
        row := arch[u+1]; # keep only the rows we need
        newrow := [];
        j := [1..Length(in_use)];
        j := Map(j, i -> in_use[i]);
        for r in j do
            # need to add columns back in order of in_use, and only those columns
            Add(newrow, row[r+1]);
        od;
        Add(newarch, newrow); # add rows back in the order listed
    od;
    return newarch;
end;


##
#F ReordIdx(<t>, <o>, <n>)
## 
#F Helper to create all possible reorderings of the qubits, where o is the original ordering, and where
#F the qubits in list t are positioned at the upper indices of the new orderings
ReordIdx := function (t, o, n)
    local idcs, orderings, children, idx, e, diff, r, avail_idcs, reorderings, temp_orderings, tmp, child, a;
    idcs := [0..n-1];
    orderings := [];
    children := [];
    for i in idcs do # initialize
        idcs[i+1] := -1;
    od;
    idx := 1;
    for e in o do
        idcs[t[idx]+1] := o[idx];
        idx := idx + 1;
    od;
    # now need to generate all possible orderings of the rest of the qubits
    diff := ListDifference([0..n-1], o);
    avail_idcs := ListDifference([0..n-1], t);
    temp_orderings := Arrangements(diff, Length(diff));
    for tmp in temp_orderings do
        # fill in the remaining slots
        idx := 1;
        child := [];
        for e  in idcs do 
            Add(child, e);
        od;
        for a in avail_idcs do
            child[a+1] := tmp[idx];
            idx := idx + 1;
        od;
        Add(orderings, child);
    od;
    return orderings;
end;

##
#F GenChild(<l>, <n>, <arch>, <S>)
## 
#F Given a list of of affected qubits (l), the number of qubits in the system (n)
#F the architecture (arch) and the non-terminal to apply (S) 
#F generate all possible embeddings 
GenChild := function (l, n, arch, S)
    local children, possible_qubits, r, reorderings, possible, newarch, factor, mats, basequbit, forswapmat, backswapmat, subset, original_qubits, target_qubits, forjunction, backjunction, child, children;
    children := [];
    possible_qubits := GenAllPartitions(arch, S);
    # possible qubits is a list of all possible ways we can factor S into arch
    # so possible qubits, if need groups of 3 and 2 = [  [[0,1], [2,3,4]] , [[2,3], [7,8,9]], ... ]
    # for each of these factorizations, need to generate appropriate child
    # a proper child will be:
    # * SWAP proper qubits to get into the right position (canonical order)
    # * generate the proper Junction
    # * apply the transform
    # * reapply the proper Junction
    # * unSWAP the proper qubits to preserve ordering
    for factor in possible_qubits do
        # example factor = [[0, 1], [2,3,4]]
        original_qubits := [0..(Length(Flat(factor)) - 1)];
        original_qubits := Map(original_qubits, i -> l[i+1]);
        target_qubits := Flat(factor);
        reorderings := ReordIdx(target_qubits, original_qubits, n);
        # have swap matrices
        forjunction := qJunc(Flat(factor), n, 1);
        backjunction := qJunc(Flat(factor), n, -1);
        # have junctions
        # To prune arch, need to 1) eliminate all rows an columns of qubits not being used, i.e. in Flat(factor)
        # then need to rename the qubits to be 0,1,2,3,4, etc... canonical ordering with Flat(factor)
        newarch := PruneArch(arch, Flat(factor));
        for r in reorderings do
            child := (Reord(r, arch, 1) * forjunction * Tensor(S.recursive_def(newarch), I(2^(n-Length(l)))) * backjunction * Reord(r, arch, -1));
            if( VerifyPath(r, [0..n-1], arch) = true) then 
                Add(children, [child]);
            fi;
        od;
    od;
    return children;
end;

##
#F ExpandTens( <l>, <n>, <S> )
## 
## qEmbed terminate function. Generates a 2^n by 2^n matrix 
## representing the application of Non-terminal S on the qubits in
## list l. We use canonical ordering
ExpandTens := function (l, n, S)
    local  size, swap1, swap2, trans;
    size := Length(l);
    # Generate swap matrix
    swap1 := PerfSwap([0..size-1], l, n);
    # Generate the reverse swap matrix
    swap2 := PerfSwap(Reversed([0..size-1]), Reversed(l), n);
    trans := Tensor(S(size), I(2^(n-size)));
    return swap1 * trans * swap2;
end;


##
#F qEmbed( <l>, <n>, <arch>, <Obj> ) Object
##
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


##
#F PermIdx( <l>, <n> )
## 
#F return an ordering of qubits indexed 0 through n-1
#F where the top |l| indices match l
PermIdx := function (l, n)
    local idcs, idx, e, tmp, diff, perms;
    idcs := [0..n-1];
    diff := ListDifference(idcs, l);
    perms := Arrangements(diff, Length(diff));
    idcs := Concatenation(l, perms[1]);
    return idcs;
end;

##
#F IsPerm( <l> )
## 
#F verifies if the given ordering is a non-trivial permutation
IsPerm := function (l)
    local idcs;
    idcs := [0..(Length(l)-1)];
    if(l = idcs) then 
        return 0;
    fi;
    return 1;
end;


##
#F qJunc Breakdown Rules
## 
NewRulesFor(qJunc, rec(

    # Junc_Base rule
    # reduces a qJunc nonterminal to a Junc SPL object
    Junc_Base := rec (
        forTransposition := false,
        minSize          := 2, 
        applicable       := (self, nt) >> (IsPerm(nt.params[1]) = 1),
        apply            := (nt, c, cnt) -> Junc( PermIdx(nt.params[1], nt.params[2]) , nt.params[3]),
    ),
    
    # Junc_ID rule
    # reduces a trivial qJunc nonterminal to an Identity SPL object
    Junc_Id := rec (
        forTransposition := false,
        minSize          := 2, 
        applicable       := (self, nt) >> (IsPerm(nt.params[1]) = 0),
        apply            := (nt, c, cnt) -> I(2^(nt.params[2])),
    )

));


##
#F qEmbed Breakdown Rules
## 
NewRulesFor(qEmbed, rec(

    # Embed_TryAll rule    
    # Try every grouping possible placement on the given qubit architecture layout
    Embed_TryAll := rec (
        forTransposition := false,
        minSize          := 1, 
        applicable       := (self, nt) >> (nt.params[3] <> [] and nt.params[1] <> [] and nt.params[2] >= 1),
        children         := nt -> List( GenChild(nt.params[1], nt.params[2], nt.params[3], nt.params[4]) ), 
        apply            := (nt, c, cnt) -> c[1],
    )

));