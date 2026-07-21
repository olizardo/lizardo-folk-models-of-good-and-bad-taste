# Agent Memory & Project Context

## Project Goal
Analyze survey data regarding sociological definitions of "good taste" and "bad taste" (`FolkTaste_BruteData.csv`, `FolkTaste_CleanData.csv`). 

## Key Technical Decisions & Analysis Pipeline
* **Scope Change**: The analysis has been restricted **exclusively to definitions** of good and bad taste. All legacy analysis of "examples" or "combined" responses has been permanently removed to focus the narrative.
* **Topic Modeling Algorithm**: NMF (Python) (previously BERTopic). Implemented robustness checks across multiple `K` components, optimizing for a 4-topic solution.
* **Pipeline Automation & Configuration**: 
    * The pipeline is entirely automated and dynamic. `config.json` dictates which models to use.
    * Use `python run_pipeline.py` from the root directory to re-execute the entire project (Topic Modeling -> Chi-Square -> R Stats/Plots -> LaTeX table generation). All analytical steps are consolidated into `analysis.py` and `analysis.R` standalone scripts.
* **R Environment**: Initialized a bare `renv` for R-based analysis. R is located at `C:/Program Files/R/R-4.5.3/bin/Rscript.exe`. Always use this explicit path when invoking Rscript from Python subprocesses (unless in a Positron Linux container where `Rscript` suffices).
* **Statistical Modeling & Visualizations**:
    * **Demographic Analysis**: Multinomial logistic regression (Wald tests) evaluating the probability of topic assignments across demographics. Predicted marginal probabilities for Age (faceted line plots with 95% CIs) and Gender (dodged bar plots) are generated using `ggeffects` dynamically in `analysis.R`.
    * **Correspondence Analysis (CA)**: CA plots mapping the schemas against each other.
    * **Domain Distinction**: PCA and ANOVAs of specific domains where respondents draw boundaries, visualized via heatmaps.
* **Reporting**: Findings are structured in `draft_research_note.tex`. The master analytical steps are consolidated in `analysis.py` and `analysis.R`, which are auto-rebuilt sequentially by `run_pipeline.py`.

## Data Structure
* Raw data is in `data/FolkTaste_BruteData.csv` (first row is variable names, second row is full question text).
* `Taste_Possibility` (Q1) determines if respondents distinguish between good and bad taste. Those who answered "YES" provided open-text definitions in `GoodTaste_Def` and `BadTaste_Def`.

## Operational Rules
* **Rendering & Installation Restrictions:** DO NOT render documents, execute pipelines, or run `pip install` commands that involve heavy dependencies (like PyTorch or downloading gigabytes of libraries) without explicitly asking the user for permission first. 
* **Transparency:** You must tell the user exactly what you are doing at each step, and you must not run any background processes that take long amounts of time without checking with the user first. If a command or render is expected to take a long time, time out, or hang, halt immediately and consult the user before proceeding.
