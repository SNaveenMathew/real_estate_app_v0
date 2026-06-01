# House Data Processing Workspace

Brief overview of the repository and how to work with it locally.

## Purpose

This workspace contains data processing and analysis scripts for house-related datasets (crime, walkability, sold listings, census, flood, etc.) used to prepare inputs and visualizations for mapping and analysis.

## Key files and folders

- `global.R` — shared configuration for R scripts.
- `process_*.R` — collection of R scripts that ingest/clean specific datasets (crime, sold, flood, walkability, etc.).
- `prepare_sold.R`, `process_sold.R` — scripts related to sold listings processing.
- Many `.Rds` files and exported CSVs under the repo root are processed data artifacts (large/data-sensitive).

## Data and temporary files (ignored)

This repository intentionally keeps processed data and large raw dumps out of source control. See `.gitignore` for the full list of patterns; notable ignored items include:

- Data files: `*.csv`, `*.Rds`, `*.RData`, `*.rda`, `*.xlsx`
- Project data directories: `BikePGH/`, `Census Bureau/`, `Crime/`, `Flood/`, `Market Indices/`, `NRI/`, `Sold/`, `Walkability/`, `Zillow/`, `Smart Location/`
- R artifacts: `.Rhistory`, `.RData`, `.Rproj.user/`, `.Renviron`
- Python artifacts: `__pycache__/`, `venv/`, `.venv/`, `dist/`, `build/`
- Notebook checkpoints: `.ipynb_checkpoints/`

If you have local data you want to track, add a specific path to version control and remove it from `.gitignore` explicitly (not recommended for large/binary files).

## How to run

- R scripts: run interactively in RStudio or from the command line with:

```bash
Rscript path/to/script.R
```

- Python helper: run with your environment's Python, e.g.:

```bash
python path/to/script.py
```

Create and activate an isolated Python virtual environment or use your preferred package manager for reproducibility.

## Dependencies

- R: install required packages as used by the `process_*.R` scripts. Consider using `renv` to snapshot dependencies.
