# spiral-package-quantum
SPIRAL Package Supporting Quantum Computing

In the SPIRAL quantum package, we define the primitives and directives necessary to leverage SPIRAL’s code generation framework for quantum circuit optimization. The input to our system is a circuit description written in terms of qubit transform objects. SPIRAL outputs both a simplified tensor expression representing the optimized circuit and QASM code that can be executed on a quantum computer.

*DISCLAIMER: The contents of this package are a subset of current research efforts into the topic, and make no claim to be optimal or ready for industrial use. Please report any bugs to the SPIRAL team.

Installation
------------

Clone this repository into the namespaces/packages subdirectory of your SPIRAL installation tree and rename it to "quantum". For example, from the namespaces/packages directory:

```
git clone https://github.com/spiral-software/spiral-package-quantum.git quantum
```

OCAML Installation
------------------

In the Unparser directory is an OCAML project that will build an executable to perform unparsing. Current software development efforts are underway to incorporate this element with SPIRAL's existing unparsing infrastructure, which would allow for a few additional optimizations.

To install OCAML:

1) Install the package manager opam(https://opam.ocaml.org/doc/Install.html). The linked page includes a shell command that you can run to install opam directly.

2) Initialize opam with the correct compiler version: opam init --dot-profile=~/.bashrc --auto-setup

3) Install packages: opam install core.v0.14.0 ppx_jane cmdliner menhir yojson utop merlin ocamlformat

-- you may additionally have to install m4, which you can do with: sudo apt-get install -y m4 on an Ubuntu machine
-- you may additionally have to install bubblewrap, whcih you can do with: sudo apt-get install bubblewrap
-- some systems report needing to upgrade the default OCAML version. you can do this with opam install and the --unlock-base flag


Sample script
-------------

This is a simple SPIRAL script to test your installation.

```
Load(quantum);
Import(quantum);
opts := SpiralDefaults; 
arch := [[0, 1, 0], [1, 0, 1], [0, 1, 0]];
t := qCirc(arch, 3, [ [[0,1], qHT(2)], [[0,2], qCNOT(1, 0, arch)]  ] );
rt := RandomRuleTree(t, opts);
s := SPLRuleTree(rt);
circ := BestCircuit(t, opts);
UnparseQASM(circ);