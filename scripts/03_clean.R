# ---- scripts/03_clean.R ----
# Purpose: Clean the raw StatsBomb event data for Women's Euro 2025.
# Remove penalty shootout periods (period 5)
# Used allclean() to get coordinates and ElapsedTime 
# this also lets me use get.minutesplayed() later. get.opposingteam() 
# saves having to do a manual join for opponent names
# pitch zones: Based on StatsBomb's coordinate system, 
# the pitch runs from 0 to 120 on x and 0 to 80 on y
# so I split it into thirds at x = 40 and x = 80
# Select and rename columns needed for analysis
# Save cleaned outputs to data/cleaned/
#
# Input:  data/raw/weuro2025_events.rds
#         data/raw/weuro2025_matches.rds
# Output: data/cleaned/weuro2025_events_clean.rds
#         data/cleaned/weuro2025_shots_clean.rds

# 1. LOAD LIBRARIES ----
library(tidyverse)
library(naniar)   
library(StatsBombR)

# 2. LOAD RAW DATA ----
events_raw    <- readRDS("data/raw/weuro2025_events.rds")
weuro_matches <- readRDS("data/raw/weuro2025_matches.rds")

# 3. REMOVE PENALTY SHOOTOUTS ----
# StatsBomb records shootout kicks as period = 5.
# Three matches went to shootouts:
#   Sweden v England (match_id 4018355)
#   France v Germany (match_id 4018357)
#   England v Spain Final (match_id 4020846)
# All analysis should be based on 90 + extra time only.

events_filtered <- events_raw %>%
  filter(as.integer(period) <= 4)

# saved to raw/ rather than cleaned/ — this is a filtered but not yet
# fully cleaned version, used as an intermediate step only
saveRDS(events_filtered, "data/raw/events_filtered.rds")

# Quick check — period 5 should now be gone
# unique(events_filtered$period) # yes, only periods 1-4

# 4. CLEAN AND ENGINEER FEATURES ----
# allclean() unpacks the nested location columns into usable x/y coordinates
# and adds ElapsedTime — without this, location.x is still a list column
# methods from StatsBomb Working with R guide (StatsBomb, 2022).
# https://blogarchive.statsbomb.com/uploads/2022/08/Working-with-R.pdf
# get.opposingteam() adds OpposingTeam — avoids manual joins downstream.
# streamline all team names so it only shows the name of the country
# Pitch third variables engineered from StatsBomb coordinate system (120 x 80)
events_clean <- allclean(events_filtered) %>%
  get.opposingteam() %>%
  rename(
    location_x = location.x,
    location_y = location.y,
    opponent   = OpposingTeam,
    team = team.name,
    player = player.name
  ) %>%
  # adjust team names so it only shows the country, not "Women's" or "WNT" (Finland)
  mutate(
    team = str_remove(team, " Women's"),
    team = if_else(team == "WNT Finland", "Finland", team),
    opponent  = str_remove(opponent, " Women's"),
    opponent  = if_else(opponent == "WNT Finland", "Finland", opponent)
  ) %>%
  mutate(
    pitch_third = case_when(
      location_x <= 40 ~ "defensive_third",
      location_x <= 80 ~ "middle_third",
      location_x >  80 ~ "attacking_third",
      TRUE             ~ NA_character_
    )
  ) %>%
  left_join(
    weuro_matches %>%
      select(
        match_id,
        match_date,
        home_team = home_team.home_team_name,
        away_team = away_team.away_team_name,
        home_score,
        away_score,
        competition_stage = competition_stage.name
      ),
    by = "match_id"
  ) %>%
  # Clean home/away team names after the join brings them in
  mutate(
    home_team = str_remove(home_team, " Women's"),
    home_team = if_else(home_team == "WNT Finland", "Finland", home_team),
    away_team = str_remove(away_team, " Women's"),
    away_team = if_else(away_team == "WNT Finland", "Finland", away_team)
  )

# 5. BUILD A SHOTS TABLE ----
# Shot events only — columns needed for xG analysis, shot maps, etc.
# Penalty shootout shots already excluded by period filter
# stage_type values: "Group Stage", "Quarter-finals", "Semi-finals", "Final"
# The four competition stages are collapsed into a binary group/knockout split
# match_date also converted to Date type
shots_clean <- events_clean %>%
  filter(type.name == "Shot") %>%
  select(
    match_id,match_date,competition_stage,
    team, opponent,player,
    period,minute,
    location_x,location_y,pitch_third, 
    shot_outcome = shot.outcome.name,
    shot_type = shot.type.name,
    shot_body_part = shot.body_part.name,
    shot_technique  = shot.technique.name,
    xg = shot.statsbomb_xg,
    first_time = shot.first_time,
    one_on_one = shot.one_on_one,
    follows_dribble = shot.follows_dribble
  ) %>%
  mutate(
    # NA means flag was not set — replace with FALSE
    first_time = replace_na(first_time, FALSE),
    one_on_one = replace_na(one_on_one, FALSE),
    follows_dribble = replace_na(follows_dribble, FALSE),
    stage_type = case_when(
      competition_stage == "Group Stage" ~ "group",
      TRUE ~ "knockout"), # covers QF, SF and Final
    match_date = as.Date(match_date)
  )

# nrow(shots_clean) # 875 shots total
miss_var_summary(shots_clean) # only binary flags had NAs, replaced above

# 6. SAVE CLEANED DATA ----
dir.create("data/cleaned", recursive = TRUE, showWarnings = FALSE)

saveRDS(events_clean, "data/cleaned/weuro2025_events_clean.rds")
saveRDS(shots_clean,  "data/cleaned/weuro2025_shots_clean.rds")

# Confirm files are saved
list.files("data/cleaned")

