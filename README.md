# QuCLEAR Implementation in Haskell (HQPlayground)

A functional implementation of the QuCLEAR quantum circuit optimization framework. This project implements Clifford Extraction and Smart Synthesis to reduce CNOT gate counts in quantum circuits. Built upon the HQP framework provided in the course for ATPL (Advanced Topics in Programming Languages) block 4 2025.

## Requirements

**Haskell**: GHC and Cabal

**Python**: matplotlib/pandas

## Usage
The bash file `reproduce_benchmarks.sh` is what is being run for the experiments. It runs 500 iterations of random Pauli strings for 20 qubits with gate depth 20. After, it runs 100 iterations of random Pauli strings for 50 qubits with gate depth 50.

To run the program, simply run the following in the root directory:
- `chmod +x reproduce_benchmarks.sh
    ./reproduce_benchmarks.sh` 
