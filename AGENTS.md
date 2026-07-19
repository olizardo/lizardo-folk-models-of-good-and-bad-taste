# Agent Memory & Project Context

## Project Goal
Analyze survey data regarding sociological definitions of "good taste" and "bad taste" (`FolkTaste_BruteData.csv`, `FolkTaste_CleanData.csv`). 

## Key Technical Decisions & Analysis Pipeline
* **Scope Change**: The analysis has been restricted **exclusively to definitions** of good and bad taste. All legacy analysis of "examples" or "combined" responses has been permanently removed to focus the narrative.
* **Topic Modeling Algorithm**: BERTopic (Python) because it handles short survey responses (1-5 sentences) significantly better than LDA. Implemented robustness checks across `min_topic_size` parameters (5, 10, 15, 20).
* **Pipeline Automation & Configuration**: 
    * The pipeline is entirely automated and dynamic. `config.json` dictates which optimal models to use (currently `good_taste_def_model_min10` and `bad_taste_def_model_min5`). 
    * Python and R scripts (`run_chi_square.py`, `run_r_analyses.R`, `generate_anova.R`, `fix_plots.R`) parse `topic_info.csv` dynamically to fetch topic names—do not hardcode label names.
    * Use `python run_pipeline.py` to re-execute the entire project (Topic Modeling -> Chi-Square -> R Stats/Plots -> LaTeX table generation).
* **R Environment**: Initialized a bare `renv` for R-based analysis.
* **Statistical Modeling & Visualizations**:
    * **Demographic Analysis**: Multinomial logistic regression (Wald tests) evaluating the probability of topic assignments across demographics.
    * **Correspondence Analysis (CA)**: CA plots mapping the schemas against each other.
    * **Domain Distinction**: PCA and ANOVAs of specific domains where respondents draw boundaries, visualized via heatmaps.
* **Reporting**: Findings are structured in `draft_research_note.tex`. The master analytical steps are consolidated in `analysis.qmd`, which is auto-rebuilt by `run_pipeline.py`.

## Data Structure
* Raw data is in `data/FolkTaste_BruteData.csv` (first row is variable names, second row is full question text).
* `Taste_Possibility` (Q1) determines if respondents distinguish between good and bad taste. Those who answered "YES" provided open-text definitions in `GoodTaste_Def` and `BadTaste_Def`.

## Operational Rules
* **Rendering & Installation Restrictions:** DO NOT render documents, execute pipelines, or run `pip install` commands that involve heavy dependencies (like PyTorch or downloading gigabytes of libraries) without explicitly asking the user for permission first. 
* **Transparency:** You must tell the user exactly what you are doing at each step, and you must not run any background processes that take long amounts of time without checking with the user first. If a command or render is expected to take a long time, time out, or hang, halt immediately and consult the user before proceeding.
