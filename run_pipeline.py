import subprocess
import sys
import os

print("=== Folk Models of Taste Pipeline ===")

def rebuild_qmd():
    content = """---
title: "Folk Models of Good and Bad Taste: Master Analysis Pipeline"
format: html
---

::: {.callout-note}
This file consolidates the entire data analysis pipeline (topic modeling, statistical tests, demographic models, and table conversion) into a single document.
:::

# 1. Topic Modeling (Python)
```{python, eval=FALSE}
__TOPIC_MODELING__
```

# 2. Chi-Square Associations (Python)
```{python, eval=FALSE}
__CHI_SQUARE__
```

# 3. Demographic Analysis and Visualizations (R)

## 3.1 Main R Analyses (Wald Tests, Demographics)
```{r, eval=FALSE}
__R_ANALYSES__
```

## 3.2 Domain Count ANOVA
```{r, eval=FALSE}
__DOMAIN_COUNT__
```

## 3.3 ANOVA for Distinction Domains
```{r, eval=FALSE}
__ANOVA__
```

## 3.4 Pearson Residuals and Heatmaps
```{r, eval=FALSE}
__PLOTS__
```

# 4. Utilities

## 4.1 Configuration and Tables
```{python, eval=FALSE}
__TABLES__
```
"""
    def read_file(path):
        if os.path.exists(path):
            with open(path, 'r', encoding='utf-8') as f:
                return f.read().strip()
        return ""
    
    content = content.replace("__TOPIC_MODELING__", read_file("scripts/run_topic_modeling.py"))
    content = content.replace("__CHI_SQUARE__", read_file("scripts/run_chi_square.py"))
    content = content.replace("__R_ANALYSES__", read_file("scripts/run_r_analyses.R"))
    content = content.replace("__DOMAIN_COUNT__", read_file("scripts/domain_count_anova.R"))
    content = content.replace("__ANOVA__", read_file("scripts/generate_anova.R"))
    content = content.replace("__PLOTS__", read_file("scripts/fix_plots.R"))
    content = content.replace("__TABLES__", read_file("scripts/generate_tables.py"))
    
    with open("analysis.qmd", "w", encoding="utf-8") as f:
        f.write(content)

steps = [
    ("Running Topic Modeling...", ["python", "scripts/run_topic_modeling.py"]),
    ("Running Chi-Square Tests...", ["python", "scripts/run_chi_square.py"]),
    ("Running R Analyses (Multinomial, Demographics)...", ["C:/Program Files/R/R-4.5.3/bin/Rscript.exe", "scripts/run_r_analyses.R"]),
    ("Running Domain Count ANOVA...", ["C:/Program Files/R/R-4.5.3/bin/Rscript.exe", "scripts/domain_count_anova.R"]),
    ("Generating ANOVA Tables...", ["C:/Program Files/R/R-4.5.3/bin/Rscript.exe", "scripts/generate_anova.R"]),
    ("Generating Plots and Heatmaps...", ["C:/Program Files/R/R-4.5.3/bin/Rscript.exe", "scripts/fix_plots.R"]),
    ("Generating TeX Summary Tables...", ["python", "scripts/generate_tables.py"]),
]

for desc, cmd in steps:
    print(f"\n--- {desc} ---")
    try:
        subprocess.run(cmd, check=True)
    except Exception as e:
        print(f"FAILED: {e}")
        if "Rscript" in cmd[0]:
            print("Note: If 'Rscript' is not found, ensure R is installed and added to your system PATH.")
        print("Note: You may need to run these commands manually.")

print("\n--- Rebuilding analysis.qmd ---")
try:
    rebuild_qmd()
    print("Updated analysis.qmd.")
except Exception as e:
    print(f"Could not rebuild analysis.qmd: {e}")

print("\n--- Compiling LaTeX Document ---")
try:
    subprocess.run(["pdflatex", "-interaction=nonstopmode", "draft_research_note.tex"], check=True)
    print("PDF Compiled Successfully.")
except Exception as e:
    print(f"LaTeX Compilation Failed (or pdflatex not found): {e}")
    print("Please compile draft_research_note.tex manually using your preferred LaTeX editor or install MiKTeX/TeXLive.")

print("\n=== Pipeline Complete! ===")
