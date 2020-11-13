
# Copyright 2018-2019, Carnegie Mellon University
# See LICENSE for details
#
# CNOT Base object
#


##
#F GenCnotMat( <j>, <dir> )
## 
## Expands into a 2^i by 2^i CNOT matrix. CNOT-ing between i and 0 depending on dir
## if dir = 1 then i -> 0, if dir = 0 then 0 -> i
GenCnotMat:= function (j, dir)
    local m1, m2;
    # if 0 controlling j
    if j = 0 then 
        return I(1);
    fi;
    m1 := VStack( HStack(I(2^(j)), O(2^(j))), HStack(O(2^(j)), Tensor(I(2^(j-1)), qX())) );
    # if j controlling 0
    m2 := qHT(j+1) * m1 * qHT(j+1);
    return When(dir = 0, m1, m2);
end;

#F -----------------------------------------------------------------------------
#F CNOT(j, a) - CNOT gate size j, swapping qubit index 0 and index j
#F a takes on {0, 1} indicating which is the controlling qubit
#F affected qubit is implicitly the other qubit. acts on a 2-qubit state vector
#F -----------------------------------------------------------------------------
Class(CNOT, Sym, rec(
     def := (j, a) -> GenCnotMat(j, a),
     isReal := False,
     isPermutation := False,
     transpose := self >> self,
     conjTranspose := self >> self,
     inverse := self >> self,
     toAMat := self >> self.def(self.params[1], self.params[2]),
 ));


