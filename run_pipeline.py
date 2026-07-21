import os
import subprocess

def main():
    print("Running master analysis pipeline...")
    subprocess.run(["quarto", "render", "analysis.qmd"], check=True)
    print("Pipeline execution complete.")

if __name__ == "__main__":
    main()

