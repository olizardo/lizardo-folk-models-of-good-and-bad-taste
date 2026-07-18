# Agent Memory & Project Context

## Project Goal
Analyze survey data regarding sociological definitions of "good taste" and "bad taste" (`FolkTaste_BruteData.csv`, `FolkTaste_CleanData.csv`). 

## Key Technical Decisions & Analysis Pipeline
* **Topic Modeling Algorithm**: BERTopic (Python) because it handles short survey responses (1-5 sentences) significantly better than LDA (which suffers from sparsity with short texts). We have expanded our modeling from two general models (good taste vs bad taste) to separate models for "definitions" and "examples" (`run_bertopic_four_models.py`), as well as "combined" definitions and examples (`run_bertopic_combined.py`).
* **Robustness Checks**: Implemented BERTopic robustness checks across various `min_topic_size` parameters (e.g., 5, 10, 15, 20) to ensure stable topic structures, with results stored in `topic_robustness_summary.json`, `topic_robustness_summary_four_models.json`, and `topic_robustness_summary_combined.json`.
* **R Environment**: Initialized a bare `renv` for R-based analysis.
* **Statistical Modeling & Visualizations**:
    * **Demographic Analysis**: Utilizes `chi_square_analysis.py` (Python), `analyze_demographics.R`, and `get_wald.R` (R) to examine demographic distributions. Logistic regression models evaluating the predicted probability of topic assignments across various demographics. Plotting includes baseline topic probabilities as dashed lines.
    * **Correspondence Analysis (CA)**: CA plots generated to map the relationship between topics and demographic categories.
    * **Domain Distinction**: Analysis of specific domains defining good vs. bad taste.
* **Reporting**: Initial compilation of findings is being structured into a LaTeX document (`draft_research_note.tex`).

## Data Structure
* Raw data is in `data/FolkTaste_BruteData.csv` (first row is variable names, second row is full question text).
* `Q1` determines if respondents have ever distinguished between good and bad taste. Those who answered "YES" provided open-text definitions in `GoodTaste` and `BadTaste`.

## Operational Rules
* **Rendering & Installation Restrictions:** DO NOT render documents, execute pipelines, or run `pip install` commands that involve heavy dependencies (like PyTorch or downloading gigabytes of libraries) without explicitly asking the user for permission first. 
* **Transparency:** You must tell the user exactly what you are doing at each step, and you must not run any background processes that take long amounts of time without checking with the user first. If a command or render is expected to take a long time, time out, or hang, halt immediately and consult the user before proceeding.
