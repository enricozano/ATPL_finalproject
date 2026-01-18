#!/bin/bash

# ==========================================
# ATPL Final Project - Benchmark Reproducer
# ==========================================

# 1. Setup: Exit if any command fails
set -e

# 2. Navigate to the Haskell project directory
if [ -d "HQPlayground" ]; then
    echo "📂 Entering HQPlayground directory..."
    cd HQPlayground
else
    echo "❌ Error: Could not find 'HQPlayground' directory."
    echo "   Make sure you run this script from the project root."
    exit 1
fi

# 3. Run Haskell Benchmark
# We use '-v0' to suppress build logs so strictly only the CSV data is output.
echo "🚀 Running Haskell Benchmark..."
echo "   (This may take some time depending on your CPU)"

# Ensure the executable is built first to avoid build logs mixing with output
cabal build Benchmark

# Run and redirect output to the CSV file expected by Python
cabal -v0 run Benchmark > exe/benchmark_results.csv

echo "✅ Benchmark data generated: HQPlayground/exe/benchmark_results.csv"

# 4. Run Python Visualization
echo "📊 Generating Python Plots..."
cd exe

# Check if python3 is available
if ! command -v python3 &> /dev/null; then
    echo "❌ Error: 'python3' is not installed or not in your PATH."
    exit 1
fi

python3 plot_benchmark.py

# 5. Finish
echo "=========================================="
echo "🎉 SUCCESS!"
echo "   Plots are available in: /benchmark_results/"
echo "=========================================="
