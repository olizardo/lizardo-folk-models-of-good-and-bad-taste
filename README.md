# Folk Models of Good and Bad Taste

This repository contains the complete replication materials—data, code, and manuscript—for the research note investigating lay definitions of aesthetic taste. 

The paper leverages computational text analysis (Non-Negative Matrix Factorization) on open-ended survey responses to surface distinct cultural models of "good" and "bad" taste, exploring how they are socially patterned by demographic hierarchies such as age and gender.

---

## 📂 Repository Structure

The project directory is structured as follows:

```text
├── analysis.py                  # Python script for NMF topic modeling and LaTeX table generation
├── analysis.R                   # R script for multinomial regressions, ANOVA, and data visualization
├── run_pipeline.py              # Python wrapper to execute the full analysis pipeline sequentially
├── data/                        # Raw and processed survey data
├── good_taste_nmf_k4/           # NMF topic modeling outputs for "Good Taste" (k=4)
├── bad_taste_nmf_k4/            # NMF topic modeling outputs for "Bad Taste" (k=4)
├── draft_research_note.tex      # Source LaTeX file for the main research note
├── references.bib               # BibTeX file containing all references
├── Tabs/                        # Auto-generated LaTeX tables
├── Plots/                       # Auto-generated plots and visualizations
├── renv.lock                    # renv lockfile for exact R package versions
├── renv/                        # Project-local R library configuration
└── .venv/                       # Python virtual environment (if used locally)
```

---

## 🛠️ Prerequisites & Installation

To run the reproducibility workflow, you will need **R** and **Python 3**.

### 1. R Dependencies
This project uses `renv` to lock down R dependencies. You can install them by running the following command in your R console:

```R
renv::restore()
```

### 2. Python Dependencies
Ensure your Python environment has the necessary packages installed (e.g., `pandas`, `scikit-learn`, `matplotlib`, `seaborn` depending on the exact pipeline configuration). You can run this in an activated virtual environment if required by your setup.

---

## 🚀 How to Reproduce the Findings

1. **Clone the Repository**: Clone this repository to your local machine using git or download it as a ZIP file.
2. **Open the Project**: Open the repository folder in your editor (e.g., RStudio, Positron, or VS Code).
3. **Install Dependencies**: Run `renv::restore()` in R to install the locked package versions.
4. **Run the Computational Pipeline**:
   The replication pipeline is driven by `analysis.py` and `analysis.R`, which dynamically handle data cleaning, stats generation, and plot generation.
   
   To execute the entire project from start to finish, simply run the pipeline wrapper from your terminal:
   
   ```bash
   python run_pipeline.py
   ```

All auto-generated tables and figures are stored in the `Tabs/` and `Plots/` directories, respectively.

5. **Compile the Manuscript**: 
   Once the tables and plots are generated, you can compile the LaTeX manuscript `draft_research_note.tex` using `pdflatex` or your preferred LaTeX engine:
   
   ```bash
   pdflatex draft_research_note.tex
   bibtex draft_research_note
   pdflatex draft_research_note.tex
   pdflatex draft_research_note.tex
   ```

---

## 📝 License & Citation

If you use the materials or code in this repository, please cite the corresponding paper or research note when it is published.
