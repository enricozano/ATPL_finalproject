import pandas as pd
import matplotlib.pyplot as plt
import os
import sys
from io import StringIO

def main():
    # 1. Load Data
    filename = 'benchmark_results.csv'
    output_dir = '../../benchmark_results'

    if not os.path.exists(filename):
        print(f"Error: '{filename}' not found.")
        print("Please run the Haskell benchmark first using ./reproduce_benchmarks.sh")
        sys.exit(1)

    # --- Robust CSV Loading (skips build logs) ---
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    start_index = 0
    for i, line in enumerate(lines):
        if line.strip().startswith("Qubits,Depth"):
            start_index = i
            break
            
    if start_index > 0:
        print(f"Found {start_index} log lines. Skipping to data...")

    csv_content = "".join(lines[start_index:])
    
    try:
        df = pd.read_csv(StringIO(csv_content))
    except pd.errors.EmptyDataError:
        print("Error: CSV data is empty or malformed.")
        sys.exit(1)

    # 2. Create output directory if it doesn't exist
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        print(f"Created folder '{output_dir}' for plots.")

    # 3. Find all unique combinations of (Qubits, Depth)
    groups = df.groupby(['Qubits', 'Depth'])

    # Set plot style
    plt.style.use('ggplot')

    for (qubits, depth), group in groups:
        group = group.reset_index(drop=True)
        iterations = group.index

        print(f"Generating plots for Qubits={qubits}, Depth={depth}...")

        # --- PLOT 1: CNOT Comparison (Expanded vs Naive vs Smart) ---
        plt.figure(figsize=(12, 7))
        plt.plot(iterations, group['ExpandedCNOTs'], label='Expanded (Baseline)', color='gray', linestyle='--', alpha=0.5, linewidth=1)
        plt.plot(iterations, group['NaiveCNOTs'], label='Naive', color='red', alpha=0.7, linewidth=1.5)
        plt.plot(iterations, group['SmartCNOTs'], label='Smart (QuCLEAR)', color='blue', alpha=0.8, linewidth=1.5)
        
        plt.title(f'CNOT Count Comparison: {qubits} Qubits, Depth {depth}')
        plt.xlabel('Iteration Number')
        plt.ylabel('Number of CNOTs')
        plt.legend()
        plt.grid(True)
        
        path_comp = os.path.join(output_dir, f"Comparison_CNOT_Q{qubits}_D{depth}.png")
        plt.savefig(path_comp, dpi=300)
        plt.close()

        # --- PLOT 2: Combined Reduction Percentage ---
        plt.figure(figsize=(12, 7))
        # Line 1: Naive vs Smart (Green)
        plt.plot(iterations, group['ReductionNaive'], label='Naive vs Smart', color='green', alpha=0.8, linewidth=1)
        # Line 2: Baseline vs Smart (Teal)
        plt.plot(iterations, group['ReductionBaseline'], label='Baseline vs Smart', color='teal', alpha=0.8, linewidth=1)
        
        plt.title(f'CNOT Reduction Percentage: {qubits} Qubits, Depth {depth}')
        plt.xlabel('Iteration Number')
        plt.ylabel('Reduction (%)')
        plt.legend()
        plt.grid(True)
        
        path_red = os.path.join(output_dir, f"Reduction_Combined_Q{qubits}_D{depth}.png")
        plt.savefig(path_red, dpi=300)
        plt.close()

        # --- PLOT 3: Runtime Comparison ---
        plt.figure(figsize=(12, 7))
        plt.plot(iterations, group['ExpandedTime'], label='Expanded Time', color='gray', linestyle='--', alpha=0.5, linewidth=1)
        plt.plot(iterations, group['NaiveTime'], label='Naive Time', color='orange', alpha=0.7, linewidth=1.5)
        plt.plot(iterations, group['SmartTime'], label='Smart Time', color='purple', alpha=0.8, linewidth=1.5)
        
        plt.title(f'Runtime Comparison: {qubits} Qubits, Depth {depth}')
        plt.xlabel('Iteration Number')
        plt.ylabel('Time (seconds)')
        plt.legend()
        plt.grid(True)
        
        path_time = os.path.join(output_dir, f"Runtime_Q{qubits}_D{depth}.png")
        plt.savefig(path_time, dpi=300)
        plt.close()

    print(f"\nAll plots (CNOT Comp, Combined Reduction, Runtime) saved to: {output_dir}/")

if __name__ == "__main__":
    main()