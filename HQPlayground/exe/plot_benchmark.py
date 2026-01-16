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
    df = pd.read_csv(StringIO(csv_content))

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

        # --- PLOT 1: CNOT Comparison ---
        plt.figure(figsize=(10, 6))
        plt.plot(iterations, group['NaiveCNOTs'], label='Naive', color='red', alpha=0.7, linewidth=1)
        plt.plot(iterations, group['SmartCNOTs'], label='Smart (QuCLEAR)', color='blue', alpha=0.7, linewidth=1)
        plt.title(f'CNOT Comparison: {qubits} Qubits, Depth {depth}')
        plt.xlabel('Iteration Number')
        plt.ylabel('Number of CNOTs')
        plt.legend()
        plt.grid(True)
        
        path_comp = os.path.join(output_dir, f"Comparison_Q{qubits}_D{depth}.png")
        plt.savefig(path_comp, dpi=300)
        plt.close()

        # --- PLOT 2: Reduction Percentage ---
        plt.figure(figsize=(10, 6))
        plt.plot(iterations, group['ReductionPct'], label='Reduction %', color='green', alpha=0.8, linewidth=1)
        plt.title(f'CNOT Reduction Percentage: {qubits} Qubits, Depth {depth}')
        plt.xlabel('Iteration Number')
        plt.ylabel('Reduction (%)')
        plt.legend()
        plt.grid(True)
        
        path_red = os.path.join(output_dir, f"Reduction_Q{qubits}_D{depth}.png")
        plt.savefig(path_red, dpi=300)
        plt.close()

        # --- PLOT 3: Runtime Comparison (New) ---
        plt.figure(figsize=(10, 6))
        plt.plot(iterations, group['NaiveTime'], label='Naive Time', color='orange', alpha=0.7, linewidth=1)
        plt.plot(iterations, group['SmartTime'], label='Smart Time', color='purple', alpha=0.7, linewidth=1)
        plt.title(f'Runtime Comparison: {qubits} Qubits, Depth {depth}')
        plt.xlabel('Iteration Number')
        plt.ylabel('Time (seconds)')
        plt.legend()
        plt.grid(True)
        
        path_time = os.path.join(output_dir, f"Runtime_Q{qubits}_D{depth}.png")
        plt.savefig(path_time, dpi=300)
        plt.close()

    print(f"\nAll plots (CNOT, Reduction, Runtime) saved to: {output_dir}/")

if __name__ == "__main__":
    main()