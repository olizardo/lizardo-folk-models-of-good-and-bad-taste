# Agent Memory & Project Context

## Project Goal
Analyze survey data regarding sociological definitions of "good taste" and "bad taste" (`FolkTaste_BruteData.csv`, `FolkTaste_CleanData.csv`). 

## Key Technical Decisions
* **Topic Modeling Algorithm**: BERTopic (Python) because it handles short survey responses (1-5 sentences) significantly better than LDA (which suffers from sparsity with short texts).
* **R Environment**: Initialized a bare `renv` for any future R-based analysis (e.g., statistical modeling, ggplot2 visualizations).
* **Notebook**: Created `bertopic_good_taste.ipynb` to handle the text preprocessing and BERTopic pipeline.

## Data Structure
* Raw data is in `data/FolkTaste_BruteData.csv` (first row is variable names, second row is full question text).
* `Q1` determines if respondents have ever distinguished between good and bad taste. Those who answered "YES" provided open-text definitions in `GoodTaste` and `BadTaste`.

## Operational Rules
* **Rendering & Installation Restrictions:** DO NOT render documents, execute pipelines, or run `pip install` commands that involve heavy dependencies (like PyTorch or downloading gigabytes of libraries) without explicitly asking the user for permission first. 
* **Transparency:** You must tell the user exactly what you are doing at each step, and you must not run any background processes that take long amounts of time without checking with the user first. If a command or render is expected to take a long time, time out, or hang, halt immediately and consult the user before proceeding.
