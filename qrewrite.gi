# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details
#
# Contains the rewrite rulesets for SPL-expression simplification
# After the Breakdown Stage
# Current Demo version focuses on CNOTs, and so we do not cancel single-qubit gates
# Eventually we will convert all gates to generic rotations in Quantum_Format
# Combine rotations in Quantum_Simplify, and convert back into non-generic rotations in Quantum_Terminate
#
# Additionally, Junction steps should be factored out in this stage versus in the unparser to enable a 
# few more optimizations. A SWE effort is underway.
#

##
#F CheckIdentitySplit( <a>, <b> )
##
## return true if we can split an Identity transform into smaller matrices
CheckIdentitySplit := function(a, b)
    if(ObjId(a) = I) then 
        if a.params[1] > 2 then
            return true;
        fi;
    fi;
    if(ObjId(b) = I) then 
        if b.params[1] > 2 then
            return true;
        fi;
    fi;
    return false;
end;

##
#F CheckIdentity( <a>, <b> )
##
## are both objects identity
CheckIdentity := function(a, b)
    if(ObjId(a) = I) then 
        if(ObjId(b) = I) then 
            return true;
        fi;
    fi;
    return false;
end;

##
#F PullInTensor( <ch1>, <ch2> )
##
## compress two tensor expressions by pulling in symbols wherever sizes align
PullInTensor := function(ch1, ch2)
    local pos1, pos2, e1, e2, up1, up2, siz1, siz2;
    pos1 := 1;
    pos2 := 1;
    siz1 := 0;
    siz2 := 0;
    while pos1 <= Length(ch1) and pos2 <= Length(ch2) do
        e1 := ch1[pos1];
        e2 := ch2[pos2];
        if ( (e1.dims()[1] = e2.dims()[1]) and (pos1 = pos2) ) then 
            ch1[pos1] := ch1[pos1] * ch2[pos2];
            ch2[pos2] := I(e2.dims()[1]);
        fi;
        up1 := siz1 + log(e1.dims()[1], 2).v;
        up2 := siz2 + log(e2.dims()[1], 2).v;
        if (up1 <= up2) then 
            pos1 := pos1 + 1;
            siz1 := siz1 + log(e1.dims()[1], 2).v;
        fi;
        if (up1 > up2) then 
            pos2 := pos2 + 1;
            siz2 := siz2 + log(e2.dims()[1], 2).v;
        fi;
    od;
    return [ch1, ch2];
end;

##
#F CanPullIn( <ch1>, <ch2> )
##
## Similar to PullInTensor, but just checks to see if there is any simplification possible
CanPullIn := function(ch1, ch2)
    local pos1, pos2, e1, e2, up1, up2, siz1, siz2;
    pos1 := 1;
    pos2 := 1;
    siz1 := 0;
    siz2 := 0;
    while pos1 <= Length(ch1) and pos2 <= Length(ch2) do
        e1 := ch1[pos1];
        e2 := ch2[pos2];
        if ( (e1.dims()[1] = e2.dims()[1]) and (pos1 = pos2) and ObjId(e2) <> I) then 
            return true;
        fi;
        up1 := siz1 + log(e1.dims()[1], 2).v;
        up2 := siz2 + log(e2.dims()[1], 2).v;
        if (up1 <= up2) then 
            pos1 := pos1 + 1;
            siz1 := siz1 + log(e1.dims()[1], 2).v;
        fi;
        if (up1 > up2) then 
            pos2 := pos2 + 1;
            siz2 := siz2 + log(e2.dims()[1], 2).v;
        fi;
    od;
    return false;
end;

##
#F ValidTensSimplifyMatch( <e1>, <e2> )
##
## SWrapper function to deterrmine if the tensor rewrite rule can be applied
ValidTensSimplifyMatch := function(e1, e2)
    if ObjId(e1)=Tensor and ObjId(e2)=Tensor then 
        if CanPullIn(e1._children, e2._children) then 
            return true;
        fi;
    fi;
    return false;
end;

##
#F CombineReord( <ord1>, <dir1>, <ord2>, <dir2>, <arch> )
##
## Combine Reorder steps
CombineReord := function(ord1, dir1, ord2, dir2, arch)
    local idx, adj1, adj2, adj3, start, n, t1, t2, a, b, new_list, o1, o2;
    adj1 := [];
    adj2 := [];
    if dir1 = -1 then
        idx := 0;
        for o1 in ord1 do 
            Add(adj1, [idx, o1]);
            idx := idx + 1;
        od;
    fi;
    if dir1 = 1 then
        idx := 0;
        for o1 in ord1 do 
            Add(adj1, [o1, idx]);
            idx := idx + 1;
        od;
    fi;
    if dir2 = -1 then
        idx := 0;
        for o2 in ord2 do 
            Add(adj2, [idx, o2]);
            idx := idx + 1;
        od;
    fi;
    if dir2 = 1 then
        idx := 0;
        for o2 in ord2 do 
            Add(adj2, [o2, idx]);
            idx := idx + 1;
        od;
    fi;
    # now have adj1 and adj2
    adj3 := [];
    for a in adj1 do 
        start := a[1];
        t1 := a[2];
        t2 := -1;
        for b in adj2 do
            if b[1] = t1 then 
                t2 := b[2];
            fi;
        od;
        Add(adj3, [start, t2]);
    od;
    # full combine, and then check validity. If it isnt valid, then just dont simplify
    # TODO do a partial recombine if the entire reorder cannot be combined and be still implementable
    # not much harder, just a lot of code
    new_list := [0..Length(ord1)-1];
    idx := 1;
    for n in new_list do 
        for a in adj3 do
            if n = a[1] then 
                new_list[idx] := a[2];
            fi;
        od;
        idx := idx + 1;
    od;
    if VerifyPath(new_list, [0..Length(ord1)-1], arch) = true then 
        return Reord(new_list, arch, 1);
    fi;
    return (Reord(ord1, arch, dir1) * Reord(ord2, arch, dir2));
end;

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
#F Quantum_Simplify ruleset
## 
Class(Quantum_Simplify, RuleSet);
RewriteRules(Quantum_Simplify, rec(

    # Pull-In Tensors 
    pull_in_tensor := ARule(Compose, [@(1), @(2).cond(e-> ValidTensSimplifyMatch(@(1).val, e))],
	    e -> let(
               # both 1 ans 2 are tensors, can access a list of arguments via _children
               ch := PullInTensor(@(1).val._children, @(2).val._children),
	           [Tensor(ch[1])]::[Tensor(ch[2])]
	         )),

    # Remove Identity Multiplications
    remove_identity_right := ARule( Compose, [@(1), @(2).cond(e -> ObjId(e)=I)], x-> [(@(1).val)]),
    remove_identity_left := ARule( Compose, [@(1).cond(e -> ObjId(e)=I), @(2)], x-> [(@(2).val)]),

    # Cancel Composition of CNOTS
    cancel_cnot := ARule(Compose, [[CNOT, @(1), @(2)], [CNOT, @(3).cond(x -> (x = @(1).val)), @(4).cond(x -> (x = @(2).val))]], x->[I(2^(@(1).val + 1))] ),

    # Combine Reorder Compositions
    cancel_reorder := ARule(Compose, [[Reord, @(1), @(2), @(3)], [Reord, @(4), @(5), @(6)]], x-> [CombineReord(@(1).val, @(3).val, @(4).val, @(6).val, @(5).val)]),
));


##
#F Quantum_Terminate ruleset
## 
Class(Quantum_Terminate, RuleSet);
RewriteRules(Quantum_Terminate, rec(
    # Recombine Identities - Good
    recomb_identity := ARule(Tensor, [@(1),  @(2).cond(e-> (CheckIdentity(@(1).val, e) ))],
	    e -> [I(@(1).val.params[1] * @(2).val.params[1])]),

    # Remove Identity Multiplications - Good
    remove_identity_right := ARule( Compose, [@(1), @(2).cond(e -> ObjId(e)=I)], x-> [(@(1).val)]),
    remove_identity_left := ARule( Compose, [@(1).cond(e -> ObjId(e)=I), @(2)], x-> [(@(2).val)]),

));


##
#F QuantumRewriteInternal( <s>, <opts> )
##
## Rewrites a quantum SPL expression, for internal use
QuantumRewriteInternal := function (s, opts)
    s := ApplyStrategy(s, [Quantum_Format], BUA, opts); 
    s := ApplyStrategy(s, [Quantum_Simplify], BUA, opts);
    s := ApplyStrategy(s, [Quantum_Terminate], BUA, opts);
    return s;
end;

##
#F BestCircuitInternal( <t>, <opts> )
##
## Finds the best circuit implementing transform t, for internal use
BestCircuitInternal := function (t, opts)
    local dpopts, best, s;
    dpopts := rec(verbosity := 0, hashTable := HashTableDP()); 
    dpopts.measureFunction := (rt, opts) ->
        let(c := SPLRuleTree(rt), # generate SPL Ruletree and then run a collect on CNOTs 
            c2 := QuantumRewriteInternal(c, opts),
            Length(Collect(c2, @(1,CNOT)))
            ); 
    best := DP(t, dpopts, opts);
    s := QuantumRewriteInternal(SPLRuleTree(best[1].ruletree), opts);
    return s;
end;

##
#F TerminateReord( <l>, <arch>, <dir> )
##
## Convert a reorder object to CNOTs
TerminateReord := function (l, arch, dir)
    local mats, mat, dir, spl;
    mats := ShiftMat(l, [0..Length(arch)-1], arch, Length(arch));
    if dir = -1 then 
        mat := mats[2];
    fi;
    if dir = 1 then
        mat := mats[1];
    fi;
    # now mat in an unreduced Non-terminal expression
    # we need the SPL expression
    spl := BestCircuitInternal(mat, SpiralDefaults);
    return spl;
end;

##
#F Quantum_Reorder ruleset
## 
Class(Quantum_Reorder, RuleSet);
RewriteRules(Quantum_Reorder, rec(

    # Combine Reorder Compositions
    reorder_convert := Rule([Reord, @(1), @(2), @(3)], x-> TerminateReord(@(1).val, @(2).val, @(3).val)),
));


# ------------------------- For External Use ----------------------------------------

##
#F QuantumRewrite( <s>, <opts> )
##
## Simplifies an SPL expression representing a quantum circuit (s)
QuantumRewrite := function (s, opts)
    s := ApplyStrategy(s, [Quantum_Format], BUA, opts); 
    s := ApplyStrategy(s, [Quantum_Simplify], BUA, opts);
    s := ApplyStrategy(s, [Quantum_Reorder], BUA, opts);
    s := RulesSums(s);
    s := ApplyStrategy(s, [Quantum_Format], BUA, opts); 
    s := ApplyStrategy(s, [Quantum_Simplify], BUA, opts);
    s := ApplyStrategy(s, [Quantum_Terminate], BUA, opts);
    return s;
end;

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




