# Copyright (c) 2018-2020, Carnegie Mellon University
# See LICENSE for details


UnparseQASM := function (spl)
    PrintTo("./qspiralout", spl);
    Exec( "./namespaces/packages/quantum/unparser/bin/unparser ./qspiralout"); 
end;

