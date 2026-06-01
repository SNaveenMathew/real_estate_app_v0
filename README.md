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

## Sequence of execution

Follow these steps to update house information from Redfin and refresh processed artifacts:

1. Download the latest favorites export from Redfin (CSV). The file is typically named like `redfin-favorites_YYYY-MM-DD.csv`.
2. Place the CSV file in the repository root (base folder) so the processing script can find it.
3. Run `process_redfin.R` (from RStudio or via command line):

```bash
Rscript process_redfin.R
```

4. `process_redfin.R` will read the CSV, normalize and clean the incoming rows, then update the local dataset artifacts (see the next section).

5. After `process_redfin.R` completes, run any downstream scripts that depend on updated Redfin data (for example, summary exports or mapping scripts).

## What `process_redfin.R` does (summary)

- Locates and reads the Redfin CSV placed in the repo root (auto-detects the latest matching file if multiple exist).
- Normalizes and cleans fields (addresses, prices, dates, URLs) and extracts identifiers such as a Redfin listing ID when present.
- For each row in the CSV, attempts to match it to an existing house record using Redfin ID, normalized address, and/or other heuristics (coordinates when available).
- If a match is found, updates the existing record's fields (price, status, last_seen timestamp, sold date, URL, and related metadata) while preserving other existing fields unless the CSV provides authoritative updates.
- If no match is found, adds the row as a new house record in the processed datasets.
- Writes updated processed artifacts (`latest_info.Rds`, and/or CSV exports), and may keep a processed copy of the input CSV and/or a changelog.
- Emits summary logs or messages indicating how many records were added, updated, or skipped.
 
### Additional implementation details (from `process_redfin.R`)

- File discovery: the script scans the repository root for files containing the substring `redfin` and ending with `.csv` (case-insensitive), sorts them, and processes each file in turn.
- Previously-processed filtering: it loads `previously_processed_rows.Rds` (if present) and filters out rows that match on several fields (address, city, state, zip, price, property type, square feet, status) to avoid re-processing unchanged rows.
- Date parsing: the script derives the CSV date from the filename (splits on `_` and parses the date-like token).
- Walkscore API: for rows that lack previously-stored walk/bike/transit scores, the script queries the Walkscore API using address plus `lat`/`lon` when available (it requests `transit=1&bike=1`). Responses are parsed for `walkscore`, `description`, `bike.score`, `bike.description`, `transit.score`, and `transit.description` and written into the processed records.
	- The script caches prior Walkscore lookups in `previously_processed_rows.Rds` and avoids repeated API calls for addresses already searched.
	- The Walkscore API key is currently set in the script (`walkscore_api_key`); consider moving the key into an environment variable or a local config file to avoid committing secrets.
- Error handling: JSON fields are accessed with `tryCatch` fallbacks so missing fields become `NA` instead of causing failures.
- Outputs: after processing, the script deduplicates and saves `previously_processed_rows.Rds`, generates `latest_info.Rds` which contains one latest snapshot per address/city/state/zip, and populates an HTML `popup_df` for use in map popups (via `utils.R` -> `table_to_popup_df`).
- Nearest-city mapping: the script reads `House.xlsx` sheet `City` to append `nearest_big_city` and `crime_city` to `latest_info`; it only saves `latest_info.Rds` when every row can be mapped to a nearest city.
- Post-processing: zip codes are truncated to 5 digits, rows without `lat`/`lon` are removed, and the script ensures uniqueness and ordering of historical rows before saving.
- Notes and quirks: the script contains a small hard-coded data correction (`714 Godwin Ct`), prints any cities missing from the `House.xlsx` mapping, and relies on `utils.R` for HTML popup generation.
