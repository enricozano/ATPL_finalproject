# QuCLEAR Implementation in Haskell (HQPlayground)

A functional implementation of the **QuCLEAR** quantum circuit optimization framework. This project implements **Clifford Extraction** and **Smart Synthesis** algorithms to significantly reduce CNOT gate counts in quantum circuits.

Built upon the **HQP (Haskell Quantum Playground)** framework provided for the *Advanced Topics in Programming Languages (ATPL)* course (Block 4, 2025).

## 📂 Project Structure

The project is organized as follows:

```text
.
├── HQPlayground/                 # Main Haskell Project Directory
│   ├── exe/                      # Executables and Benchmarking Scripts
│   │   ├── Benchmark.hs          # Haskell source for running benchmarks
│   │   ├── TestClifford.hs       # Unit tests & correctness showcase
│   │   └── plot_benchmark.py     # Python script to visualize results
│   ├── src/                      # Source Code (HQP Library)
│   │   └── HQP/
│   │       ├── Opt/              # Optimization Modules (QuCLEAR logic)
│   │       ├── QOp/              # Quantum Operator Syntax & Symplectic representation
│   │       └── PrettyPrint/      # Visualization utilities
│   └── hqp.cabal                 # Cabal build configuration
├── reproduce_benchmarks.sh       # Main automation script for benchmarks
└── README.md                     # Project documentation
```

## 🛠 Prerequisites & Dependencies

To build and run this software, your environment must meet the following requirements:

### 1. Haskell Environment

You need the Glasgow Haskell Compiler (GHC) and Cabal build tool.

**Recommended:** Install via **GHCup**.

**Haskell Packages** (handled automatically by Cabal):

- base
- random
- mtl

> This project relies purely on symbolic and algebraic manipulation of Clifford and Pauli operators and intentionally avoids heavy numerical dependencies.

### 2. Python Environment

Used only for benchmark visualization.

- Python 3.x
- `pandas`
- `matplotlib`

## 🚀 Installation

### Install Python Dependencies

```bash
pip install pandas matplotlib
```

### Update Cabal Package Database

```bash
cd HQPlayground
cabal update
cd ..
```

## 📊 Usage

The project includes an automated script `reproduce_benchmarks.sh` that compiles the Haskell code, runs benchmarks, and generates performance plots using Python.

### Running the Benchmarks

```bash
chmod +x reproduce_benchmarks.sh
./reproduce_benchmarks.sh
```

### What Happens?

- **Compilation:** Cabal builds the benchmark executable
- **Execution:** Random Pauli-rotation circuits are generated
- **Data Collection:** Results are written to  
  `HQPlayground/exe/benchmark_results.csv`
- **Visualization:** Plots are generated in  
  `HQPlayground/exe/benchmark_results/`

## 🧪 Correctness & Interactive Testing (`TestClifford.hs`)

The file `TestClifford.hs` serves as both a **correctness proof-of-functionality** and an **interactive test harness** for the QuCLEAR framework.

### Running All Tests

To execute all tests sequentially (including correctness checks and scalability benchmarks):

```bash
cd HQPlayground
cabal run TestClifford
```

This runs every test defined in `main` and prints:
- Clifford commutation validation
- Z-basis mapping checks
- Pauli gadget decomposition verification
- CNOT count comparisons between naive and QuCLEAR optimization
- Runtime and scalability benchmarks

### Interactive Exploration via `cabal repl`

You can also explore individual components interactively:

```bash
cd HQPlayground
cabal repl TestClifford
```

Within the REPL, the following functions can be invoked manually:

- **`testCommutationRules`**  
  Demonstrates correct Clifford–Pauli commutation on multi-qubit rotations, including tensor-product Hadamards and distant CNOT conjugation.

- **`testZBasisMapping`**  
  Verifies that arbitrary Pauli strings are correctly mapped to the Z-basis using Clifford basis changes.

- **`testNaiveDecomposition`**  
  Shows full Pauli-gadget decomposition into pre-, core-, and post-Clifford components and visualizes the resulting circuit.

- **`testOptimizationStrategies`**  
  Compares naive decomposition against QuCLEAR’s smart extraction strategy and reports CNOT count reductions.

- **`benchmarkScalability`**  
  Runs randomized circuit benchmarks at increasing qubit counts and depths to demonstrate scaling behavior.

Each function prints intermediate results and circuit visualizations, making the file suitable as both a test suite and a demonstration artifact.

## 🧩 Key Modules

- **HQP.Opt.SmartExtraction**  
  Implements QuCLEAR’s recursive CNOT-tree synthesis and look-ahead Pauli optimization.

- **HQP.Opt.CliffordExtraction**  
  Commutes Clifford gates and extracts them to canonical circuit positions.

- **HQP.QOp.Symplectic**  
  Provides a tableau-based symplectic representation for efficient Clifford conjugation of Pauli operators.
