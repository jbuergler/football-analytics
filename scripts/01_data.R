# ---- scripts/01_data.R ----
# Purpose: Pull Women's Euro 2025 data from StatsBomb and save raw files

# StatsBomb open data accessed via StatsBombR package
# Methodology reference: StatsBomb (2023) Working with R
# https://blogarchive.statsbomb.com/uploads/2022/08/Working-with-R.pdf

# Outputs: 
# data/raw/weuro2025_events.rds
# data/raw/weuro2025_matches.rds

# --- 1. Libraries ----
library(StatsBombR)
library(tidyverse)
library(naniar)

# --- 2. Pull competitions and confirm Euro 2025 IDs ----
competitions <- FreeCompetitions()

# Check what's available for competition_id 53 (UEFA Women's Euro)
competitions %>%
  filter(competition_id == 53) %>%
  select(competition_id, competition_name, season_id, season_name)

# --- 3. Filter to Women's Euro 2025 and pull matches ----
weuro_2025 <- competitions %>%
  filter(competition_id == 53, season_id == 315)

weuro_matches <- FreeMatches(Competitions = weuro_2025)

weuro_matches %>%
  select(match_id, match_date, 
         home_team.home_team_name, 
         away_team.away_team_name) %>%
  arrange(match_date)


# --- 4. Pull events for each match and combine into one dataframe ----
events_raw <- free_allevents(MatchesDF = weuro_matches, Parallel = TRUE)

# --- 5. Save raw data to data/raw/ ----
saveRDS(events_raw,    "data/raw/weuro2025_events.rds")
saveRDS(weuro_matches, "data/raw/weuro2025_matches.rds")

# ./_publish.sh "Add raw data pipeline and StatsBomb download script"

