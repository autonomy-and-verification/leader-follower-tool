# Download

Download the version for your platform from the `bin` folder. The binaries are statically linked and should run on both Windows and Linux with no dependencies; macOS is not supported currently. You may also want to download some test cases from the `tests` folder, e.g. `twopc.in`.

# Usage

Run `tool input_file output_file`, where `input_file` is the specification of the protocol you want to verify and `output_file` is the file to which the tool will write the translation. To check the translation run `z3 output_file`. If the result is `unsat`, the protocol has the specified property; if the result is `sat`, it doesn't. The translation uses some Z3-specific extensions to SMT-LIB and may not be compatible with other SMT solvers.

For example (on Linux, assuming `twopc.in` is in the same folder as the tool):

```
$ ./tool-x86_64-linux twopc.in twopc.out
$ z3 twopc.out
unsat
```

# Input syntax

The tool accepts protocol specifications in an SMT-LIB-like syntax (which itself is Lisp-like). Each specification is of the following form:

```
((leader
   (delta triple+)
   (initial state+))
 (follower
   (delta triple+)
   (initial state+)) 
 (property phi))
```

In the above, `delta` includes one more triples of the form `((state state_name) transition (state state_name))`, where, in the leader's case, `transition` is of the form `(in condition c mu_in)`, `(in condition c d mu_in)`, `(out mu_out)`, or `(local ell)`, corresponding to the leader's transitions ?⋉c μᵢₙ, ?⋉c/d μᵢₙ, !μₒᵤₜ, and ℓ respectively (`condition` is `=`, `<`, `>`, `<=`, or `>=`), and, in the follower's case, `transition` is of the form `(in mu_in)`, `(out mu_out)`, or `(local ell)`, corresponding to the leader's transitions ?μᵢₙ, !μₒᵤₜ, and ℓ respectively; `initial` includes one or more states written as above, i.e. `(state state_name)`; and `phi` is of the form `(always n omega)` or `(eventually n omega)` corresponding to □ⁿ ω or ⋄ⁿ ω respectively, where `omega` is an SMT-LIB formula (except for implication and logical equivalence, where, to simplify parsing, we use `implies` in place of `=>` and `equivalent` in place of `=`) featuring no terms other than (natural) numbers, the symbol `K` (standing for the number of followers), `(# (state state_name))` (standing for the number of followers in state `state_name`), and `(# (state state_name) transition)` (standing for the number of followers in state `state_name` taking transition `transition`, where `transition` is specified in the same format as above).

See the `tests` folder for some examples.