import os
import subprocess
import sys

def main():
    print("Running master analysis pipeline...")
    
    print("\n--- Step 1: Executing Python Analysis (Topic Modeling & Tables) ---")
    subprocess.run([sys.executable, "analysis.py"], check=True)
    
    print("\n--- Step 2: Executing R Analysis (Statistics & Visualizations) ---")
    subprocess.run(["Rscript", "analysis.R"], check=True)
    
    print("\nPipeline execution complete.")

if __name__ == "__main__":
    main()

