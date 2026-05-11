# ---- scripts/04_analyse.R ----

# Purpose: Build all summary tables for the Women's Euro 2025 dashboard.
# Sections map directly to dashboard tabs - build one section at a time

# Inputs:  
# data/cleaned/weuro2025_shots_clean.rds
# data/cleaned/weuro2025_events_clean.rds

# Outputs: 
# data/cleaned/tbl_team_xg_summary.rds           - Part 1: tournament overview
# data/cleaned/tbl_press_summary.rds             - Part 1
# data/cleaned/tbl_ball_progression.rds          - Part 1
# data/cleaned/tbl_stage_breakdown.rds           - Part 2: the journey
# data/cleaned/tbl_cumulative_xg.rds             - Part 2
# data/cleaned/tbl_final_shots.rds               - Part 3A: the final in detail: shots
# data/cleaned/tbl_final_timeline.rds            - Part 3A
# data/cleaned/tbl_final_player_actions.rds      - Part 3B: the final in detail: possession
# data/cleaned/tbl_final_pass_thirds.rds         - Part 3B
# data/cleaned/tbl_verdict_summary.rds           - Part 4: was it deserved?
# data/cleaned/tbl_final_pressures.rds           - Part 4

# Note: every saved .rds contains only the columns the dashboard needs
# The app never loads the full event log (Shinylive file-size constraint).

# Naming:
# tbl_ prefix for all saved data tables
# fig_ prefix for all plot objects (used in 05_visualise.R)

# LOAD LIBRARIES AND DATA ----
library(tidyverse)
library(StatsBombR)

shots_clean <- readRDS("data/cleaned/weuro2025_shots_clean.rds")
events_clean <- readRDS("data/cleaned/weuro2025_events_clean.rds")

# ---- TOURNAMENT OVERVIEW ----
# Tab 1: who performed best across the whole tournament?
# Build order: tbl_match_xg first; tbl_team_xg_summary derives from it

## tbl_match_xg ----
# One row per team per match
# opponent column comes directly from shots_clean
# Opponent goals/ xG/ shots = match total minus team's value
# only valid if exactly 2 teams per match
# The check below confirms this holds across all 31 games
tbl_match_xg <- shots_clean %>%
  group_by(match_id, team) %>%
  summarise(
    match_date = first(match_date),
    stage = first(competition_stage),
    stage_type = first(stage_type),
    opponent = first(opponent),
    goals = sum(shot_outcome == "Goal", na.rm = TRUE),
    xg_total = sum(xg,na.rm = TRUE),
    shots = n(),
    .groups  = "drop"
  ) %>%
  # opponent-relative metrics within each match
  group_by(match_id) %>%
  mutate(
    opp_xg  = sum(xg_total) - xg_total,
    xg_diff = xg_total - opp_xg,
    opp_goals = sum(goals) - goals,
    opp_shots = sum(shots) - shots
  ) %>%
  ungroup() %>%
  # finishing metrics per row
  mutate(
    goals_vs_xg = ifelse(xg_total > 0, goals / xg_total,  NA_real_),
    goals_minus_xg = goals - xg_total
  ) %>%
  select(
    match_id, match_date, team, opponent, stage, stage_type,
    goals, opp_goals, xg_total, opp_xg, xg_diff, 
    goals_vs_xg, goals_minus_xg, shots, opp_shots,
  )

# Integrity check - must return zero rows before proceeding
tbl_match_xg %>%
  count(match_id) %>%
  filter(n != 2) # zero rows confirmed

# Findings:
# Maximum total xG (5.71) is England vs Wales
# Happened in the group stage: 6-1 win with 24 shots 

## tbl_team_xg_summary ----
# One row per team, full-tournament aggregates
tbl_team_xg_summary <- tbl_match_xg %>%
  group_by(team) %>%
  summarise(
    matches = n(),
    total_goals = sum(goals),
    total_xg = sum(xg_total),
    total_xg_against = sum(opp_xg),
    avg_xg_per_match = mean(xg_total),
    avg_xg_against = mean(opp_xg),
    total_shots_faced = sum(opp_shots),
    avg_xg_diff = mean(xg_diff),
    goals_minus_xg = sum(goals_minus_xg),
    .groups = "drop"
  ) %>%
  mutate(
    goals_vs_xg = ifelse(total_xg > 0, total_goals / total_xg,NA_real_),
    shots_faced_per_match = round(total_shots_faced / matches, 1)
    ) %>%
  arrange(desc(avg_xg_diff)) %>%
  mutate(xg_diff_rank = row_number()) %>%
  select(
    team, matches, total_goals, total_xg, total_xg_against,
    avg_xg_per_match, avg_xg_against, avg_xg_diff,
    goals_vs_xg, goals_minus_xg, xg_diff_rank, shots_faced_per_match
  )

# Key finding:
# Spain 1st (avg_xg_diff 1.98), England 3rd (1.20).

## tbl_press_summary ----
# Pressing intensity by team and stage type 
# high_press_rate: proportion of pressures in the opponent half (location_x > 60).
# counterpress_rate: proportion of pressures flagged as counter-presses by StatsBomb.
# NA in counterpress recoded to FALSE so the full row count is the denominator
# Split by stage_type to capture any tactical shift from group to knockouts
# press summary uses events_clean directly so stage_type needs to be computed here separately

tbl_press_summary <- events_clean %>%
  filter(type.name == "Pressure") %>%
  mutate(
    stage_type = case_when(
      competition_stage == "Group Stage" ~ "group",
      TRUE ~ "knockout"
    ),
    high_press = location_x > 60,
    is_counterpress = ifelse(is.na(counterpress), FALSE, counterpress == TRUE)
  ) %>%
  group_by(team, stage_type) %>%
  summarise(
    total_pressures = n(),
    high_pressures = sum(high_press, na.rm = TRUE),
    counterpresses = sum(is_counterpress),
    high_press_rate = round(mean(high_press, na.rm = TRUE) * 100, 1),
    counterpress_rate = round(mean(is_counterpress) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(team, stage_type) %>%
  select(
    team, stage_type, total_pressures,
    high_pressures, high_press_rate,
    counterpresses, counterpress_rate
  ) %>% arrange(desc(high_press_rate))

# Key findings:
# Spain led on high press rate at every stage (76.6 group, 61.8 knockout vs 56.3 and 48.5 for England)
# Counter-press gap is smaller but consistent - Spain 32.3 group, England 20.

## tbl_ball_progression ----
# Pass and carry metrics showing how each team moved the ball forward
# Carries and pass end coordinates come from allclean()
# # final_third_carry_rate: proportion of carries ending beyond x = 80
# shows how the attacking third was penetrated with ball carries
# I excluded Unknown and Injury Clearance pass outcomes (138 and 7 rows respectively)
# as neither of them show successful or failed passes
# Progressive pass = at least 10 units forward
# Final Third: passes/ carries ending beyond x = 80 (attacking third boundary)

# Step 1: carry metrics per team
carry_stats <- events_clean %>%
  filter(type.name == "Carry") %>%
  mutate(
    carry_end_x = carry.end_location.x,
    final_third_carry = carry_end_x > 80,
  ) %>%
  group_by(team) %>%
  summarise(
    total_carries = n(),
    final_third_carries = sum(final_third_carry, na.rm = TRUE),
    final_third_carry_rate = round(mean(final_third_carry, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>% arrange(desc(final_third_carry_rate))

# Step 2: pass metrics per team
pass_stats <- events_clean %>%
  filter(
    type.name == "Pass",
    is.na(pass.outcome.name) |
      !pass.outcome.name %in% c("Unknown", "Injury Clearance")
  ) %>%
  mutate(
    pass_start_x = location_x,
    pass_end_x = pass.end_location.x, 
    progressive_pass = (pass_end_x - pass_start_x) >= 10, 
    final_third_pass = pass_end_x > 80
  ) %>%
  group_by(team) %>%
  summarise(
    total_passes = n(),
    completed_passes = sum(is.na(pass.outcome.name)),
    progressive_passes = sum(progressive_pass, na.rm = TRUE),
    final_third_passes = sum(final_third_pass, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    pass_completion = round(completed_passes / total_passes * 100, 1),
    progressive_pass_rate = round(progressive_passes / total_passes * 100, 1),
    final_third_pass_rate = round(final_third_passes / total_passes * 100, 1)
  )

# Step 3: join carries and passes into one table
tbl_ball_progression <- carry_stats %>%
  left_join(pass_stats, by = "team") %>%
  arrange(desc(final_third_pass_rate)) %>%
  select(
    team,
    total_carries, final_third_carries, final_third_carry_rate, 
    total_passes, completed_passes, pass_completion,
    progressive_passes, progressive_pass_rate,
    final_third_passes, final_third_pass_rate
  ) %>% arrange(desc(final_third_carry_rate))

# Key findings:
# Spain led on final third carries (35.7%) and pass completion (87.2%)
# England were 5th for final third carries (29.1%)

## Save part 1 tables ----
dir.create("data/cleaned", recursive = TRUE, showWarnings = FALSE)

saveRDS(tbl_team_xg_summary,        "data/cleaned/tbl_team_xg_summary.rds")
saveRDS(tbl_press_summary, "data/cleaned/tbl_press_summary.rds")
saveRDS(tbl_ball_progression, "data/cleaned/tbl_ball_progression.rds")

# ---- THE JOURNEY ----
# Tab 2: how did teams get to the final, match by match?

## tbl_stage_breakdown ----
# Adds result flags to tbl_match_xg
# won_dominant: won AND created more xG than the opponent (deserved win)
# won_fortunate: won BUT opponent created more xG (fortunate win)
tbl_stage_breakdown <- tbl_match_xg %>%
  mutate(
    match_result = case_when(
      goals > opp_goals ~ "win",
      goals < opp_goals ~ "loss",
      TRUE ~ "draw"
    ),
    won_dominant = match_result == "win" & xg_diff >  0,
    won_fortunate = match_result == "win" & xg_diff <  0
  ) %>%
  select(
    match_id, match_date, team, opponent, stage, stage_type,
    goals, opp_goals, xg_total, opp_xg, xg_diff,
    goals_minus_xg, match_result, won_dominant, won_fortunate
  ) 

## tbl_cumulative_xg ----
# Running xG and goal totals per team, in match order
# Used for the cumulative xG line chart on Tab 2
tbl_cumulative_xg <- tbl_match_xg %>%
  arrange(team, match_date) %>%
  group_by(team) %>%
  mutate(
    match_number = row_number(),
    cumulative_xg = cumsum(xg_total),
    cumulative_goals = cumsum(goals)
  ) %>%
  ungroup() %>%
  select(
    team, match_number, match_date, opponent, stage, stage_type,
    goals, opp_goals, xg_total, opp_xg,
    cumulative_xg, cumulative_goals
    )

## Save Part 2 Tables ----
saveRDS(tbl_stage_breakdown, "data/cleaned/tbl_stage_breakdown.rds")
saveRDS(tbl_cumulative_xg, "data/cleaned/tbl_cumulative_xg.rds")

# ---- THE FINAL PART A (SHOTS) ----
# Tab 3A: what happened in the England vs Spain final (shots)?

## tbl_final_shots ----
# Shot-level detail for the final match only - used for the shot map
final_match_id <- tbl_match_xg %>%
  filter(stage == "Final") %>%
  pull(match_id) %>%
  unique()

# Shot-level table for the final - one row per shot attempt
tbl_final_shots <- shots_clean %>%
  filter(match_id == final_match_id) %>%
  select(
    team, player,
    minute, period,
    location_x, location_y,
    xg,
    shot_outcome, shot_body_part, shot_technique
  ) %>%
  arrange(period, minute)

# Key finding: both goals were headers 
# England: Russo, min 56, 0.213 xG. Spain: Caldentey, min 24, 0.396 xG

## tbl_final_timeline ----
# Cumulative xG by shot, in chronological order - used for the xG timeline chart
tbl_final_timeline <- tbl_final_shots %>%
  arrange(period, minute) %>%
  group_by(team) %>%
  mutate(
    shot_number = row_number(),
    cumulative_xg = cumsum(xg)
  ) %>%
  ungroup() %>%
  select(
    team, shot_number, minute, period,
    player, xg, cumulative_xg, shot_outcome
  )

# ---- THE FINAL PART B (POSSESSION) ----
# Tab 3B: what happened in the England vs Spain final (possession)?

## tbl_final_player_actions ----
# Completed passes and progressive carries for Bonmatí and Hemp in the final.
# Used for the player pass map in Tab 3.
# Filtered to final-third actions only to keep the map readable.
tbl_final_player_actions <- events_clean %>%
  filter(
    match_id  == final_match_id,
    player %in% c("Aitana Bonmati Conca", "Lauren Hemp")
  ) %>%
  filter(
    (type.name == "Pass" & is.na(pass.outcome.name) &
       pass.end_location.x > 80) |
      (type.name == "Carry" & carry.end_location.x > 80 &
         (carry.end_location.x - location_x) >= 5)
  ) %>%
  mutate(
    player_label = if_else(
      player == "Aitana Bonmati Conca",
      "Bonmatí (Spain)", "Hemp (England)"
    ),
    action_type = type.name,
    end_x = if_else(type.name == "Pass",
                    pass.end_location.x, carry.end_location.x),
    end_y = if_else(type.name == "Pass",
                    pass.end_location.y, carry.end_location.y),
    tooltip = paste0(
      player_label, " · ", action_type, "\n",
      "From: (", round(location_x,1), ", ", round(location_y,1), ")\n",
      "To: (", round(end_x,1), ", ", round(end_y,1), ")\n",
      "Minute: ", minute, "'"
    )
  ) %>%
  select(player_label, action_type, minute,
         location_x, location_y, end_x, end_y, tooltip)

# Player selection rationale:
# Bonmatí: most active attacking player in the final by carries and passes into the final third
# Hemp: England's equivalent: highest volume of forward ball-carrying for her side
# Comparing them spatially follows StatsBomb Use Case 4 (pass plotting).

## tbl_final_pass_thirds ----
# Completed pass distribution by pitch third for the final
tbl_final_pass_thirds <- events_clean %>%
  filter(match_id == final_match_id,
         type.name == "Pass",
         is.na(pass.outcome.name)) %>%
  mutate(
    pitch_third = case_when(
      pitch_third == "defensive_third"  ~ "Defensive third",
      pitch_third == "middle_third"     ~ "Middle third",
      pitch_third == "attacking_third"  ~ "Attacking third"
    )
  ) %>%
  group_by(team, pitch_third) %>%
  summarise(
    passes = n(), 
    .groups = "drop") %>%
  group_by(team) %>%
  mutate(
    pct = round(passes / sum(passes) * 100, 1),
    tooltip = paste0(team, " · ", pitch_third, "\n",
                     "Passes: ", passes, " (", pct, "%)")
  ) %>%
  ungroup() %>%
  mutate(
    pitch_third = fct_relevel(pitch_third,
                              "Defensive third",
                              "Middle third",
                              "Attacking third")
  )

# Key Findings
# Spain dominated passes in the attacking third and less in the defensive third

## Save Part 3 Tables ----
saveRDS(tbl_final_shots, "data/cleaned/tbl_final_shots.rds")
saveRDS(tbl_final_timeline, "data/cleaned/tbl_final_timeline.rds")
saveRDS(tbl_final_player_actions,"data/cleaned/tbl_final_player_actions.rds")
saveRDS(tbl_final_pass_thirds, "data/cleaned/tbl_final_pass_thirds.rds")

# ---- EVIDENCE FOR THE VERDICT ----
# Tab 4: did England deserve to win?

## tbl_verdict_summary ----
# Static England vs Spain comparison
# used for tbl_verdict_gt in 05_visualise.R and static table in App Tab 4
tbl_verdict_summary <- tibble(
  Dimension = c(
    "Chance quality", "Defensive Solidity",
    "Game control", "Game control",
    "Attacking Threat", "Attacking Threat",
    "Defensive Solidity"
  ),
  Metric = c(
    "Avg xG per match",
    "Avg xG conceded per match",
    "High press rate (%)",
    "Pass completion (%)",
    "Final third carry rate (%)",
    "Final third pass rate (%)",
    "Shots faced per match"
  ),
  England = c(
    round(filter(tbl_team_xg_summary, team == "England")$avg_xg_per_match, 2),
    round(filter(tbl_team_xg_summary, team == "England")$avg_xg_against, 2),
    tbl_press_summary %>% filter(team == "England") %>%
      summarise(r = round(sum(high_pressures)/sum(total_pressures)*100,1)) %>% pull(r),
    filter(tbl_ball_progression, team == "England")$pass_completion,
    filter(tbl_ball_progression, team == "England")$final_third_carry_rate,
    filter(tbl_ball_progression, team == "England")$final_third_pass_rate,
    filter(tbl_team_xg_summary, team == "England")$shots_faced_per_match
  ),
  Spain = c(
    round(filter(tbl_team_xg_summary, team == "Spain")$avg_xg_per_match, 2),
    round(filter(tbl_team_xg_summary, team == "Spain")$avg_xg_against, 2),
    tbl_press_summary %>% filter(team == "Spain") %>%
      summarise(r = round(sum(high_pressures)/sum(total_pressures)*100,1)) %>% pull(r),
    filter(tbl_ball_progression, team == "Spain")$pass_completion,
    filter(tbl_ball_progression, team == "Spain")$final_third_carry_rate,
    filter(tbl_ball_progression, team == "Spain")$final_third_pass_rate,
    filter(tbl_team_xg_summary, team == "Spain")$shots_faced_per_match
  ),
  Spain_leads = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
)

## tbl_final_pressures ----
# Pressure counts by team in the final
# England out-pressed Spain (393 vs 253) - used for Tab 4 value box
# Surprising given Spain's xG dominance - suggests England's tactical
# approach was more aggressive than the scoreline implies
tbl_final_pressures <- events_clean %>%
  filter(
    match_id == final_match_id,
    type.name == "Pressure"
  ) %>%
  count(team)

# Key finding:
# England 393 pressures vs Spain 253
# England pressed significantly more despite being the lower xG team

## Save Part 4 Tables ----
saveRDS(tbl_verdict_summary, "data/cleaned/tbl_verdict_summary.rds")
saveRDS(tbl_final_pressures, "data/cleaned/tbl_final_pressures.rds")

# Copy all app data files to app/ folder
rds_files <- list.files("data/cleaned", pattern = "\\.rds$", full.names = TRUE)
file.copy(rds_files, "app/", overwrite = TRUE)

# add fig_xg_ranking as rds in app due to issues with shiny
file.copy("data/figures/fig_xg_ranking_app.rds", "app/", overwrite = TRUE)

# Confirm all files are in app/
list.files("app", pattern = "\\.rds$")

