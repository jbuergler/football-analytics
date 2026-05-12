# Women's Euro 2025 Dashboard: Did England Deserve to Win?

A project for ULMS744: Sports Analytics in Practice

An interactive dashboard analysing England's victory at UEFA Women's Euro 2025 using StatsBomb open event data.

**Live dashboard:** <https://jbuergler.github.io/football-analytics/>

Built with R R 4.5.1, Shiny, and `bslib`, published to GitHub Pages via [Shinylive](https://shinylive.io/r/). The app runs entirely in the visitor's browser, with no server required. Data was pulled from the StatsBomb API in April 2026.

## Research Question

Did England deserve to win the Women's Euro 2025?

## Data

StatsBomb open event data pulled from the `StatsBombR` package. Competition ID 53 (UEFA Women's Euro), Season ID 315 (2025). 31 matches · 875 shots · excluding penalty shootouts.

## Repository Layout

- `scripts/` — data pipeline (`00_setup.R` through `05_visualise.R`)
- `app/`     — the Shiny app (`app.R` and all `.rds` data files)
- `docs/`    — the Shinylive build served by GitHub Pages
- `data/`    — raw and cleaned data and figures

## How to Reproduce

### Step 1: Install dependencies

If `devtools` is not already installed, run this in the R console first:

```r
install.packages("devtools")
```


Then open `football-analytics.Rproj` in RStudio and run:

```         
source("scripts/00_setup.R")
```

This installs and loads all required packages, including `StatsBombR`
(installed from GitHub via `devtools`).

### Step 2: Run the data pipeline in order

```         
source("scripts/01_data.R")   # pull raw data from StatsBomb API
source("scripts/02_explore.R") # explore event structure
source("scripts/03_clean.R")  # clean and engineer features
source("scripts/04_analyse.R") # build summary tables
source("scripts/05_visualise.R") # build static charts
```

Each script saves its outputs to data/ or data/figures/. Scripts 02 onwards can be run independently if the previous outputs already exist.

### Step 3: Run the app locally

```         
shiny::runApp("app")
```

### Step 4: Rebuild the deployed dashboard (optional)

To regenerate the Shinylive bundle served by GitHub Pages, run:

```         
shinylive::export("app", "docs")
```

Then commit and push the updated `docs/` folder. GitHub Pages serves from
`docs/` on the main branch. Allow a few minutes for the deployment to update.


------------------------------------------------------------------------

## Notes

- An internet connection is required for Step 1 (`01_data.R` pulls live data
  from the StatsBomb API).
- Built with R 4.5.1. Results should be reproducible on later versions, but
  package behaviour may differ.
- Penalty shootout events (period 5) and own goals are excluded from all xG
  and shot analyses.