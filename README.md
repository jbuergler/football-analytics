# Women's Euro 2025 Dashboard: Did England Deserve to Win?

A project for ULMS744: Sports Analytics in Practice

An interactive dashboard analysing England's victory at UEFA Women's Euro 2025 using StatsBomb open event data.

**Live dashboard:** <https://jbuergler.github.io/football-analytics/>

Built with R, Shiny, and `bslib`, published to GitHub Pages via [Shinylive](https://shinylive.io/r/). The app runs entirely in the visitor's browser, with no server required.

## Research Question

Did England deserve to win the Women's Euro 2025?

## Data

StatsBomb open event data pulled from the `StatsBombR` package. Competition ID 53 (UEFA Women's Euro), Season ID 315 (2025). 31 matches · 875 shots · excluding penalty shootouts.

## Repository Layout

-   `scripts/` - data pipeline (00_setup.R through 05_visualise.R)
-   `app/` - the Shiny app (app.R and all .rds data files)
-   `docs/` - the Shinylive build served by GitHub Pages
-   `data/` - raw and cleaned data

## How to Reproduce

1.  Open `football-analytics.Rproj` in RStudio
2.  Run `scripts/00_setup.R` to load libraries
3.  Run `scripts/01_data.R` to pull raw data from StatsBomb
4.  Run `scripts/02_explore.R` to explore the data
5.  Run `scripts/03_clean.R` to clean and engineer features
6.  Run `scripts/04_analyse.R` to build summary tables
7.  Run `scripts/05_visualise.R` to build static charts
8.  Run `shiny::runApp("app")` to launch the dashboard locally
