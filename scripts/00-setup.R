# ---- scripts/00_setup.R ----
# Purpose: Load libraries used for the project
# Run once at the start of a session before running any other script

# This project was built on:
# R Version: R version 4.5.2 (2025-10-31 ucrt)

if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

if (!requireNamespace("StatsBombR", quietly = TRUE)) {
  devtools::install_github("statsbomb/StatsBombR")
}

library(StatsBombR) # StatsBomb open event data
library(tidyverse) # data wrangling and visualisation (ggplot)
library(naniar) # missing variables
library(ggrepel) # non-overlapping labels in ggplot
library(ggsoccer) # football pitch plot
library(gt) # formatted tables
library(bslib) # shiny app theme
library(bsicons) # icons for shiny UI
library(plotly) # interactive charts for dashboard
library(shiny) # shiny app framework
library(shinylive) # deploy shiny app via WebAssembly



