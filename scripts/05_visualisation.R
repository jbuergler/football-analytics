# ---- scripts/05_visualisation.R ----
# Purpose: Build all static visualisation for the Women's Euro 2025 dashboard.
# Builds the foundation for the interactive plots for the app later on (with plotly, shiny)
# Static first — each chart confirmed here before going into Shiny
# fig_ prefix for all saved plot objects

# LOAD TABLES ----
tbl_team_xg_summary <- readRDS("data/cleaned/tbl_team_xg_summary.rds") # Fig lollipop, scatter, tbl verdict
tbl_press_summary   <- readRDS("data/cleaned/tbl_press_summary.rds") # fig press shift, tbl verdict
tbl_ball_progression <- readRDS("data/cleaned/tbl_ball_progression.rds") # tbl verdict
tbl_cumulative_xg <- readRDS("data/cleaned/tbl_cumulative_xg.rds") # fig cum xg
tbl_stage_breakdown <- readRDS("data/cleaned/tbl_stage_breakdown.rds") # fig match xg eng and esp
tbl_final_shots <- readRDS("data/cleaned/tbl_final_shots.rds") # fig shot map
tbl_final_timeline <- readRDS("data/cleaned/tbl_final_timeline.rds") # fig xg timeline
tbl_final_pass_thirds <- readRDS("data/cleaned/tbl_final_pass_thirds.rds") # fig final pass thirds
tbl_final_player_actions <- readRDS("data/cleaned/tbl_final_player_actions.rds") # fig passmap
tbl_verdict_summary <- readRDS("data/cleaned/tbl_verdict_summary.rds") # verdict gt table

# LIBRARIES ----
library(tidyverse)
library(ggrepel)
library(ggsoccer)
library(gt)
library(webshot2)

## COLOURS —---- 
# defined once, used in both ggplot charts and bslib theme
euro_colours <- c(
  "Spain"   = "#F1BF00",
  "England" = "#CE1124",
  "Other"   = "grey69"
)

# GGPLOT THEME ----
# Applied to every chart — call theme_euro() instead of theme_minimal() directly
theme_euro <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13,
                                      colour = "#1F2937"),
      plot.subtitle = element_text(size = 10, colour = "#6B7280",
                                      margin = margin(b = 10)),
      axis.title = element_text(size = 10, colour = "#1F2937"),
      axis.text = element_text(size = 9,  colour = "#6B7280"),
      panel.grid.major = element_line(colour = "#E5E7EB", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      legend.position = "none",
      plot.caption = element_text(size = 8, colour = "#9CA3AF", hjust = 0)
    )
}

# ---- TAB 1: TOURNAMENT OVERVIEW ----

## fig_xg_scatter ----
# One takeaway: both finalists' goals broadly matched their xG —
# the xG differential reflects genuine quality, not finishing luck.
fig_xg_scatter <- tbl_team_xg_summary %>%
  mutate(
    team_highlight = case_when(
      team == "Spain"   ~ "Spain",
      team == "England" ~ "England",
      TRUE ~ "Other"
    )
  ) %>%
  ggplot(aes(x = total_xg, y = total_goals, colour = team_highlight)) +
  geom_abline(slope = 1, intercept = 0,
              colour = "grey69", linewidth = 0.5, linetype = "dashed") +
  geom_point(aes(size = team_highlight == "Other"), show.legend = FALSE) +
  geom_text_repel(
    aes(label = team),
    size = 2.8, show.legend = FALSE,
    max.overlaps = 20
  ) +
  scale_colour_manual(values = euro_colours) +
  scale_size_manual(values = c("TRUE" = 2.5, "FALSE" = 3.8)) +
  scale_x_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  #annotate("text", x = 1, y = 19,
          # label = "above line = scored more than xG predicted",
           #size = 2.8, colour = "grey69", hjust = 0) +
  #annotate("text", x = 8, y = 1,
          # label = "below line = scored less than xG predicted",
           #size = 2.8, colour = "grey69", hjust = 0) +
  labs(
    title = "Goals scored vs expected goals — Women's Euro 2025",
    # subtitle = "Teams above the dashed line scored more than their chance quality predicted",
    x = "Total expected goals (xG)",
    y = "Total goals scored",
  ) +
  theme_euro()

## fig_xg_ranking ----
# Horizontal bar chart ranking all 16 teams by avg xG difference per match
# Spain and England highlighted in team colours, others in grey
# Reference lines at +1 and -1 to contextualise the rankings
# One takeaway: Spain dominated on chance quality; England ranked 3rd
fig_xg_ranking <- tbl_team_xg_summary %>%
  arrange(avg_xg_diff) %>%
  mutate(
    team_highlight = case_when(
      team == "Spain"   ~ "Spain",
      team == "England" ~ "England",
      TRUE ~ "Other"
    ),
    team = factor(team, levels = unique(team)),
    label = if_else(
      team %in% c("Spain", "England"),
      sprintf("%+.2f", avg_xg_diff),
      NA_character_
    )
  ) %>%
  ggplot(aes(x = avg_xg_diff, y = team, fill = team_highlight)) +
  geom_col(colour = NA) +
  geom_vline(xintercept =  0, colour = "black", alpha = 0.5,
             linewidth = 0.4, linetype = "dashed") +
  geom_vline(xintercept =  1, colour = "#2E7D32", alpha = 0.7,
             linewidth = 0.8, linetype = "dotted") +
  geom_vline(xintercept = -1, colour = "#CE1124", alpha = 0.7,
             linewidth = 0.8, linetype = "dotted") +
  annotate("text", x =  1, y = 0.4, label = "+1",
           colour = "#2E7D32", size = 4, fontface = "bold", hjust = -0.2) +
  annotate("text", x = -1, y = 0.4, label = "-1",
           colour = "#CE1124", size = 4, fontface = "bold", hjust =  1.2) +
  geom_text(
    aes(label = label,
        hjust = if_else(avg_xg_diff >= 0, -0.2, 1.2)),
    size = 4, fontface = "bold", na.rm = TRUE
  ) +
  scale_fill_manual(values = euro_colours) +
  scale_x_continuous(expand = c(0.2, 0.25)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "xG Dominance — Tournament Ranking",
    x = "Avg xG Difference per Match", 
    y = NULL
  ) +
  theme_euro() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

# ---- TAB 2: THE JOURNEY ----

## fig_cumulative_xg ----
# One takeaway: England's xG line spikes in the group stage then flattens —
# Spain's rises steadily throughout. The arcs tell different stories
# about how each team built their tournament.
fig_cumulative_xg <- tbl_cumulative_xg %>%
  mutate(
    team_highlight = case_when(
      team == "Spain" ~ "Spain",
      team == "England" ~ "England",
      TRUE ~ "Other"
    ),
  ) %>%
  ggplot(aes(x = match_number, y = cumulative_xg,
             group = team, colour = team_highlight)) +
  geom_line(
    data = ~ filter(.x, team_highlight == "Other"),
    linewidth = 0.5, alpha = 0.25
  ) +
  geom_line(
    data = ~ filter(.x, team_highlight != "Other"),
    linewidth = 1.8
  ) +
  geom_point(
    data = ~ filter(.x, team_highlight != "Other"),
    size = 2.5
  ) +
  geom_vline(xintercept = 3.5, colour = "grey69",
             linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = 2, y = max(tbl_cumulative_xg$cumulative_xg) * 0.95,
           label = "Group Stage", size = 4, colour = "grey69", hjust = 0.5) +
  annotate("text", x = 4.5, y = max(tbl_cumulative_xg$cumulative_xg) * 0.95,
           label = "Knockout", size = 4, colour = "grey69", hjust = 0.5) +
  geom_text(
    data = ~ filter(.x, team_highlight != "Other") %>%
      group_by(team) %>%
      slice_max(match_number, n = 1),
    aes(label = team),
    hjust = -0.15, size = 4, fontface = "bold"
  ) +
  scale_colour_manual(values = euro_colours) +
  scale_x_continuous(
    breaks = 1:6,
    labels = paste("Match", 1:6),
    expand = expansion(mult = c(0.05, 0.2))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Cumulative xG across the Tournament",
    # subtitle = "Spain builds up consistently · England's line is inflated by a 5.71 xG group stage match against Wales.",
    x = NULL,
    y = "Cumulative expected goals (xG)",
    # caption = "grey lines = other teams"
  ) +
  theme_euro() +
  theme(legend.position = "none")

## fig_match_xg_bars ----
# One takeaway: England's xG advantage was concentrated in the group stage
# the knockouts were tight, and the final shows Spain with more xG

### England ----
fig_match_xg_bars_eng <- tbl_stage_breakdown %>%
  filter(team == "England") %>%
  arrange(match_date) %>%
  mutate(
    match_label = paste0(
      "vs ", opponent, "\n", stage
    ),
    match_label = fct_inorder(match_label)
  ) %>%
  select(match_label, stage_type, xg_total, opp_xg) %>%
  pivot_longer(
    cols = c(xg_total, opp_xg),
    names_to  = "metric",
    values_to = "xg"
  ) %>%
  mutate(
    bar_colour = if_else(metric == "xg_total", "England", "Opponent")
  ) %>%
  ggplot(aes(x = match_label, y = xg, fill = bar_colour)) +
  annotate("rect",
           xmin = 0.5, xmax = 3.5,
           ymin = 0, ymax = 7,
           fill = "#F9FAFB", alpha = 0.8) +
  annotate("rect",
           xmin = 3.5, xmax = 6.5,
           ymin = 0, ymax = 7,
           fill = "#FFF0F0", alpha = 0.5) +
  annotate("text", x = 2, y = 5.4,
           label = "Group Stage", size = 3,
           colour = "grey69", hjust = 0.5) +
  annotate("text", x = 5, y = 5.4,
           label = "Knockout", size = 3,
           colour = "grey69", hjust = 0.5) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = round(xg, 2)),
    position = position_dodge(width = 0.75),
    vjust = -0.4, size = 2.5, fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("England" = "#CE1124", "Opponent" = "grey69"),
    labels = c("England" = "England xG", "Opponent" = "Opponent xG")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Match-by-match xG — England",
    #subtitle = "Red = England xG · Grey = Opponent xG · Red Background = Knockout Stage",
    x = NULL,
    y = "Expected Goals (xG)",
    fill = NULL,
  ) +
  scale_x_discrete() +
  theme_euro() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 8)
  )

### Spain ----
fig_match_xg_bars_esp <- tbl_stage_breakdown %>%
  filter(team == "Spain") %>%
  arrange(match_date) %>%
  mutate(
    match_label = paste0("vs ", opponent, "\n", stage),
    match_label = fct_inorder(match_label)
  ) %>%
  select(match_label, stage_type, xg_total, opp_xg) %>%
  pivot_longer(
    cols = c(xg_total, opp_xg),
    names_to  = "metric",
    values_to = "xg"
  ) %>%
  mutate(
    bar_colour = if_else(metric == "xg_total", "Spain", "Opponent")
  ) %>%
  ggplot(aes(x = match_label, y = xg, fill = bar_colour))+
  annotate("rect",
           xmin = 0.5, xmax = 3.5,
           ymin = 0, ymax = 7,
           fill = "#F9FAFB", alpha = 0.8) +
  annotate("rect",
           xmin = 3.5, xmax = 6.5,
           ymin = 0, ymax = 7,
           fill = "#FEF9EC", alpha = 0.5) +
  annotate("text", x = 2, y = 5.4,
           label = "Group Stage", size = 3,
           colour = "grey69", hjust = 0.5) +
  annotate("text", x = 5, y = 5.4,
           label = "Knockout", size = 3,
           colour = "grey69", hjust = 0.5) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = round(xg, 2)),
    position = position_dodge(width = 0.75),
    vjust = -0.4, size = 2.5, fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Spain" = "#F1BF00", "Opponent" = "grey69"),
    labels = c("Spain" = "Spain xG", "Opponent" = "Opponent xG")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Match-by-match xG — Spain",
    #subtitle = "Yellow = Spain xG · Grey = Opponent xG · Yellow background = Knockout Stage",
    x = NULL,
    y = "Expected Goals (xG)",
    fill = NULL
  ) +
  scale_x_discrete() +
  theme_euro() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 8)
  )

## fig_press_shift ----
# One takeaway: Spain pressed higher than England at every stage
# Spain's knockout press rate (61.8%) exceeded England's group stage rate (56.3%)
fig_press_shift <- tbl_press_summary %>%
  filter(team %in% c("Spain", "England")) %>%
  mutate(
    stage_label = if_else(stage_type == "group",
                          "Group stage", "Knockout"),
    stage_label = fct_relevel(stage_label, "Group stage", "Knockout")
  ) %>%
  pivot_longer(
    cols = c(high_press_rate, counterpress_rate),
    names_to = "metric",
    values_to = "rate"
  ) %>%
  mutate(
    metric = case_when(
      metric == "high_press_rate" ~ "High-press rate",
      metric == "counterpress_rate" ~ "Counter-press rate"
    ),
    metric = fct_relevel(metric, "High-press rate", "Counter-press rate")
  ) %>%
  ggplot(aes(x = stage_label, y = rate, fill = team))+
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = paste0(round(rate), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.5, size = 2.8, fontface = "bold"
  ) +
  facet_wrap(~ metric) +
  scale_fill_manual(values = euro_colours, name = NULL) +
  scale_y_continuous(
    limits = c(0, 90),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Pressing Intensity — Group Stage vs Knockout",
   # subtitle = "Spain pressed higher and counter-pressed more aggressively than England at every stage",
    x = NULL,
    y = "Rate (%)"
  ) +
  theme_euro() +
  theme(
    legend.position  = "top",
    strip.text = element_text(face = "bold", size = 10,
                                    colour = "#1F2937"),
    panel.spacing = unit(1.5, "lines")
  )

# ---- TAB 3A: THE FINAL (SHOTS) ----

## fig_shot_map ----
# One takeaway: Spain generated far more shots and from better positions —
# 23 shots and 2.14 xG vs England's 8 shots and 0.88 xG
fig_shot_map <- tbl_final_shots %>%
  mutate(
    # Flip England so they attack left, Spain stays attacking right
    plot_x = if_else(team == "England", 120 - location_x, location_x),
    plot_y = if_else(team == "England", 80  - location_y, location_y),
  ) %>%
  ggplot(aes(x = plot_x, y = plot_y)) +
  annotate_pitch(dimensions = pitch_statsbomb,
                 colour = "white", fill = "#4a7c3f") +
  geom_point(
    aes(size = xg,
        colour = team),
    alpha = 0.9
  ) +
  geom_point(
    data = ~ filter(.x, shot_outcome == "Goal"),
    aes(size = xg),
    shape = 21,
    fill = NA,
    colour = "white",
    stroke = 1.5
  ) +
  scale_colour_manual(values = euro_colours) +
  scale_size_continuous(range = c(2, 8), name = "xG Value",
                        breaks = c(0.05, 0.2, 0.5),
                        labels = c("0.05", "0.20", "0.50")) +
  guides(
    colour = guide_legend(
      override.aes = list(size = 5, alpha = 1)),
    size = guide_legend(
      override.aes = list(alpha = 1))
  ) +
  coord_cartesian(xlim = c(0, 120), ylim = c(0, 80)) +
  labs(
    title = "Shot map — Women's Euro 2025 Final",
   # subtitle = "England attack left · Spain attack right · circle size = xG · white ring = goal"
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold", size = 13,
                                   colour = "#1F2937",
                                   margin = margin(b = 4)),
    #plot.subtitle = element_text(size = 10, colour = "#6B7280",
                                  # margin = margin(b = 8)),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.spacing.x = unit(1, "cm"),
    legend.text = element_text(size = 11, colour = "#1F2937"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

## fig_xg_timeline ----
# One takeaway: Spain's xG accumulated steadily throughout
# England's line barely moved after the first half.

# create end-point labels (last row per team) - added for the report
tbl_timeline_labels <- tbl_final_timeline %>%
  group_by(team) %>%
  slice_max(minute, n = 1) %>%
  ungroup()

fig_xg_timeline <- tbl_final_timeline %>%
  ggplot(aes(x = minute, y = cumulative_xg,
             colour = team, group = team)) +
  geom_vline(xintercept = 45,  colour = "grey69", 
             linewidth = 0.4, linetype = "dashed") +
  geom_vline(xintercept = 90,  colour = "grey69", 
             linewidth = 0.4, linetype = "dashed") +
  geom_vline(xintercept = 105, colour = "grey69", 
             linewidth = 0.4, linetype = "dashed") +
  annotate("text", x = 22,  y = 2.2, label = "1st half",
           size = 2.8, colour = "grey69") +
  annotate("text", x = 67,  y = 2.2, label = "2nd half",
           size = 2.8, colour = "grey69") +
  annotate("text", x = 100, y = 2.2, label = "ET",
           size = 2.8, colour = "grey69") +
  geom_step(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_point(
    data = ~ filter(.x, shot_outcome == "Goal"),
    size = 3, shape  = 21,
    fill = NA, colour = "#2E7D32", stroke = 1.5
  ) +
  geom_text(
    data = tbl_timeline_labels,
    aes(label = paste0(team, " ", round(cumulative_xg, 2))),
    hjust = -0.15, fontface = "bold", size = 3
  ) +
  scale_colour_manual(values = euro_colours, name = NULL) +
  scale_x_continuous(
    breaks = c(0, 45, 90, 105, 120),
    labels = c("0'", "45'", "90'", "105'", "120'"),
    expand = expansion(mult = c(0.02, 0.18))  # increase from 0.12 to 0.18
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Cumulative xG Timeline — Women's Euro 2025 Final",
    x = "Minute",
    y = "Cumulative xG"
  ) +
  theme_euro() +
  theme(legend.position = "top")

# ---- TAB 3B: THE FINAL (POSSESSION) ----
## fig_final_pass_thirds ----
# how the passes were distributed by pitch third (def, mid, att)
# takeaway: Spain in attacking third (29.2%) vs England's 15.2%
fig_final_pass_thirds <- tbl_final_pass_thirds %>%
  ggplot(aes(x = pitch_third, y = pct,
             fill = team)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = paste0(round(pct), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.5, size = 2.8, fontface = "bold"
  ) +
  scale_fill_manual(values = euro_colours, name = NULL) +
  scale_y_continuous(
    limits = c(0, 60),
    expand = expansion(mult = c(0, 0.05)),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Pass Distribution by Pitch Third",
    x = NULL,
    y = "Share of completed Passes (%)"
  ) +
  theme_euro() +
  theme(legend.position = "top")

## fig_player_passmap ----
# Method: StatsBomb Working with R (StatsBomb, 2022), Use Case 4 — plotting passes
# takeaway: Bonmatí drove forward across the full width of the pitch 
# Hemp mainly came through the left hand side
# Note: 
# as a facet wrap for a static visual, but will be 2 individual ones in the app
# colours here are generic. In the app each player gets their team colour (Spain yellow for Bonmatí, England red for Hemp)
fig_player_passmap <- tbl_final_player_actions %>%
  mutate(
    player_label = fct_relevel(player_label, "Hemp (England)", "Bonmatí (Spain)")
  ) %>%
  ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb,
                 colour = "white", fill = "#4a7c3f") +
  geom_segment(
    aes(x = location_x, y = location_y,
        xend = end_x, yend = end_y,
        colour = action_type),
    linewidth = 0.6, alpha = 0.7,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed")
  ) +
  facet_wrap(~ player_label) +
  scale_colour_manual(
    values = c("Pass" = "#1F2937", "Carry" = "grey69"),
    name   = NULL,
    labels = c("Pass" = "Final-third pass", "Carry" = "Final-third carry")
  ) +
  coord_cartesian(xlim = c(60, 121), ylim = c(0, 80)) +
  labs(
    title = "Final-third actions — Women's Euro 2025 Final",
  ) +
  theme_pitch() +
  theme(
    plot.title = element_text(face = "bold", size = 13,
                                   colour = "#1F2937",
                                   margin = margin(b = 4)),
    strip.text = element_text(face = "bold", size = 11,
                                   colour = "#1F2937"),
    strip.background = element_rect(fill = "#F3F4F6", colour = NA),
    legend.position = "bottom",
    legend.text = element_text(size = 10, colour = "#1F2937"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# ---- TAB 4: WAS IT DESERVED? ----
## tbl_verdict_gt ----
# gt summary table for Tab 4 — rendered via gt_output() in the Shiny app.
# Colour bands highlight which team led each metric
# Spain leads in all 7 metrics — the table closes the analytical verdict
tbl_verdict_gt <- tbl_verdict_summary %>%
  gt(groupname_col = "Dimension") %>%
  tab_header(
    title = md("**England vs Spain — Key Metrics**"),
    subtitle = "Tournament Averages · Women's Euro 2025"
  ) %>%
  tab_spanner(
    label = "Team",
    columns = c(England, Spain)
  ) %>%
  tab_style(
    style = cell_fill(color = "#FEF9EC"),
    locations = cells_body(
      columns = Spain,
      rows = Spain_leads == TRUE
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#FFF0F0"),
    locations = cells_body(
      columns = England,
      rows = Spain_leads == FALSE
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Spain,
      rows = Spain_leads == TRUE
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = England,
      rows = Spain_leads == FALSE
    )
  ) %>%
  tab_style(
    style = cell_text(color = "#6B7280", size = "small"),
    locations = cells_row_groups()
  ) %>%
  cols_hide(columns = Spain_leads) %>%
  cols_align(align = "center", columns = c(England, Spain)) %>%
  opt_table_font(font = "system-ui") %>%
  tab_options(
    table.border.top.color = "white",
    heading.border.bottom.color = "#E5E7EB",
    row_group.border.top.color = "#E5E7EB",
    column_labels.font.weight = "bold",
    table.width = pct(80)
  )

# ---- SAVE FIGURES FOR REPORT ----
dir.create("data/figures", recursive = TRUE, showWarnings = FALSE)

saveRDS(fig_xg_ranking, "data/figures/fig_xg_ranking.rds")
saveRDS(fig_xg_scatter, "data/figures/fig_xg_scatter.rds")
saveRDS(fig_cumulative_xg, "data/figures/fig_cumulative_xg.rds")
saveRDS(fig_match_xg_bars_eng, "data/figures/fig_match_xg_bars_eng.rds")
saveRDS(fig_match_xg_bars_esp, "data/figures/fig_match_xg_bars_esp.rds")
saveRDS(fig_press_shift, "data/figures/fig_press_shift.rds")
saveRDS(fig_shot_map, "data/figures/fig_shot_map.rds")
saveRDS(fig_xg_timeline, "data/figures/fig_xg_timeline.rds")
saveRDS(fig_final_pass_thirds, "data/figures/fig_final_pass_thirds.rds")
saveRDS(fig_player_passmap, "data/figures/fig_player_passmap.rds")
gtsave(tbl_verdict_gt, "data/figures/fig_verdict_table.png", 
       vwidth = 900, vheight = 400, zoom = 2)
