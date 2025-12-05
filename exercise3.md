# Exercise 3
Michael Kirkedal Thomsen

## RFUN

__Programmers introduction to RFUN__
https://github.com/kirkedal/rfun-interp/blob/master/README.md

__Online interpreter__
https://topps.diku.dk/pirc/?id=rfunT

__For a more general introduction (note that it is an older syntax)__

* [1] Interpretation and programming of the reversible functional language RFUN; Michael Kirkedal Thomsen, Holger Bock Axelsen; IFL '15: Proceedings of the 27th Symposium on the Implementation and Application of Functional Programming Languages; Article No.: 8, Pages 1-13
https://dl.acm.org/doi/abs/10.1145/2897336.2897345

__For a fundamental paper on the type system__

* [2] CoreFUN: a typed functional reversible core language; Petur Andrias Højgaard Jacobsen, Robin Kaarsgaard, Michael Kirkedal Thomsen; International Conference on Reversible Computation, Springer, Pages 304-321.

### Exercises

#### Arithmetic Exercises

* Implement a `minus` function
* Implement a function that multiplies by 2
* Implement a multiplication function **(hard)**
  * What embedding makes sense?
* Implement a function `even`, that checks if a natural number is even
  * What embedding of `even` makes sense?

#### Arithmetic List

* Implement a function that checks if all elements of a list is even
* Implement `splitAt` that splits a list in two after a given lenght
  * What is a good embedding?
  * You can find the Haskell definition here  http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:splitAt
* Implement an `append` function
  * Again, the embedding is not clear?
* Implement `scanl` and `scanr` **(hard)**
  * You can find the Haskell definition here  http://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:scanl
* Implement `foldl` and `foldr` **(even hard)**
  * What is a resonable embedding?
  * How does this differ from the scan's
* Continue with `interleave`, `intercalate`, `permutations`, `group`, `subsequences`, `transpose`, ...


#### Logic
We can define a bit and `not`-gate as

```
-- Don't mind that "Z" is translated to "0"
data Bit = True | False

not :: Bit <-> Bit
not True = False
not False = True
```

Implement the classical quantum gates: CNOT, Toffoli, Fredkin etc. Try to preserve the 


#### Quantum Circuits
Transformation (rewriting) of quantum curcuits is a classical (not very scaleable) technique for optimising quantum circuits.

We can implement James langauges like this. (You are welcome to simplify.)
```
data QC = I | X |  Y | Z | H | SX | S | T
data Operator = Atom(QC) | C (Operator) | Permute ([Int]) | Tensor (Operator,Operator) | Id (Nat) | Compose(Operator, Operator) | Adjoint(Operator)
```
[3] defines a complete equational theory for quantum circuits. Implementen the initial equational definitions (figures on the first few pages). You are also welcome to extend James' program in haskell with this.

* [3] A Complete Equational Theory for Quantum Circuits; Alexandre Clément, Nicolas Heurtel, Shane Mansfield, Simon Perdrix, Benoît Valiron; https://arxiv.org/pdf/2206.10577



## Jeopardy
A functional language bearing syntactic resemblance to you garden variety functional programming language and, consequently, exhibits the expected semantics for programs running in the conventional direction.

* [4] Jeopardy: An invertible functional programming language; Joachim Tilsted Kristensen, Robin Kaarsgaard, Michael Kirkedal Thomsen; International Conference on Reversible Computation, Springer. Pages 124-141
https://arxiv.org/pdf/2209.02422

__Source code__
https://github.com/jtkristensen/Jeopardy


### Exercise

Install `cabal install` and play.

What it the central differences.
