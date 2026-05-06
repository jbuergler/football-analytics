# ---- app/app.R ----
# Women's Euro 2025 — Did England deserve to win?

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(ggrepel)
library(ggsoccer)
library(gt)

# LOAD DATA ----
tbl_team_xg_summary <- readRDS("tbl_team_xg_summary.rds")
tbl_press_summary <- readRDS("tbl_press_summary.rds")
tbl_cumulative_xg <- readRDS("tbl_cumulative_xg.rds")
tbl_stage_breakdown <- readRDS("tbl_stage_breakdown.rds")
tbl_final_shots <- readRDS("tbl_final_shots.rds")
tbl_final_timeline <- readRDS("tbl_final_timeline.rds")
tbl_final_player_actions <- readRDS("tbl_final_player_actions.rds")
tbl_final_pass_thirds <- readRDS("tbl_final_pass_thirds.rds")
tbl_verdict_summary <- readRDS("tbl_verdict_summary.rds")

# COLOURS ----
euro_colours <- c(
  "Spain"   = "#F1BF00",
  "England" = "#CE1124",
  "Other"   = "grey69"
)

team_colours <- c(
  "England"     = "#CE1124",
  "Spain"       = "#F1BF00",
  "Sweden"      = "#006AA7",
  "France"      = "#21304D",
  "Germany"     = "#000000",
  "Norway"      = "#BA0C2F",
  "Netherlands" = "#F36C21",
  "Denmark"     = "#C8102E",
  "Italy"       = "#007CC3",
  "Portugal"    = "#0D6938",
  "Belgium"     = "#ED2939",
  "Iceland"     = "#02529C",
  "Switzerland" = "#DA291C",
  "Finland"     = "#002F6C",
  "Wales"       = "#174A3F",
  "Poland"      = "#DC143C"
)

# THEME ----
# controls Shiny layout, sidebar, value boxes, tabs
euro_theme <- bs_theme(
  version   = 5,
  primary   = "#F1BF00", 
  secondary = "#CE1124",   
  success   = "#2E7D32",
  info = "#4E2CA3",
  warning = "#4DFFFF",
  danger = "#006AA7",
  base_font = font_collection(
    "-apple-system",
    "BlinkMacSystemFont",
    "Segoe UI",
    "Helvetica Neue",
    "Arial",
    "sans-serif"
  )
)

# GGPLOT THEME ----
theme_euro <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, colour = "#1F2937"),
      plot.subtitle    = element_text(size = 10, colour = "#6B7280", margin = margin(b = 10)),
      axis.title       = element_text(size = 10, colour = "#1F2937"),
      axis.text        = element_text(size = 9,  colour = "#6B7280"),
      panel.grid.major = element_line(colour = "#E5E7EB", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      legend.position  = "none",
      plot.caption     = element_text(size = 8, colour = "#9CA3AF", hjust = 0)
    )
}

# UI ----
ui <- page_navbar(
  title = "Women's Euro 2025",
  theme = euro_theme,
  
  # TAB 1: Tournament Picture ----
  nav_panel(
    "Tournament Picture",
    
    layout_sidebar(
      
      sidebar = sidebar(
        width = 210,
        bg    = "#F9FAFB",
        open  = TRUE,
        tags$p(
          tags$strong("Dashboard Question", style = "color: #1F2937;"),
          style = "margin-bottom: 0.4rem;"
        ),
        tags$p(
          "Did England deserve to win Women's Euro 2025?",
          style = "font-style: italic; font-size: 0.85rem; color: #374151;"
        ),
        tags$hr(),
        tags$p(
          "This Dashboard compares England's Journey to and at the Final against Spain.
          It analyses their Journey from four analytical angles:
          Tournament Picture ➜ 
          England and Spain's Journeys to the Final ➜
          The Final itself ➜ 
          Analytical Decision: Did England deserve to win?",
          style = "font-size: 0.78rem; color: #6B7280;"
        ),
        tags$hr(),
        tags$p(
          "Based on StatsBomb open event data
          31 matches · excluding Penalty Shootouts",
          style = "font-size: 0.78rem; color: #6B7280;"
        )
      ),
      
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(
          title    = "Teams",
          value    = n_distinct(tbl_team_xg_summary$team),
          showcase = bsicons::bs_icon("people-fill"),
          theme    = "primary"
        ),
        value_box(
          title    = "Matches played",
          value    = "31",
          showcase = bsicons::bs_icon("calendar-event"),
          theme    = "secondary"
        ),
        value_box(
          title    = "Goals scored",
          value    = sum(tbl_team_xg_summary$total_goals),
          showcase = bsicons::bs_icon("bullseye"),
          theme    = "primary"
        ),
        value_box(
          title    = "Shots taken",
          value    = 875,
          showcase = bsicons::bs_icon("crosshair"),
          theme    = "secondary"
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header("xG Dominance - Tournament Ranking"),
          p("Spain top the tournament in xG dominance (+1.98), ahead of England (+1.20) and the rest of the field.",
            style = "font-size:0.85rem; color:#555; padding: 4px 12px 0 12px;"),
          plotlyOutput("xg_lollipop", height = "380px")
        ),
        card(
          full_screen = TRUE,
          card_header("Goals vs Expected Goals"),
          p("Spain and England both outscored their xG, with Spain generating slightly more overall chance quality.",
            style = "font-size:0.85rem; color:#555; padding: 4px 12px 0 12px;"),
          plotlyOutput("xg_scatter", height = "380px")
        )
      )
    )
  ),
  
  # TAB 2: The Journey ----
  nav_panel(
    "The Journey",
    
    card(
      full_screen = TRUE,
      card_header("Cumulative xG across the Tournament"),
      p("England's xG built steadily but Spain pulled clear in the knockout rounds.",
        style = "font-size:0.85rem; color:#555; padding: 4px 12px 0 12px;"),
      layout_columns(
        col_widths = c(3, 9),
        selectInput(
          inputId  = "compare_team",
          label    = "Compare England against:",
          choices  = tbl_cumulative_xg %>%
            filter(team != "England") %>%
            distinct(team) %>%
            arrange(team) %>%
            pull(team),
          selected = "Spain"
        ),
        plotlyOutput("cumulative_xg", height = "360px")
      )
    ),
    
    layout_columns(
      col_widths = c(6, 6),
      navset_card_tab(
        title       = "Match-by-Match xG",
        full_screen = TRUE,
        nav_panel("England", 
                  p("Spain's per-match xG was more consistent; England had bigger variation across games.",
                    style = "font-size:0.85rem; color:#555; padding: 4px 12px 0 12px;"),
                  plotlyOutput("match_bars_eng")),
        nav_panel("Spain",
                  p("Spain's per-match xG was more consistent; England had bigger variation across games.",
                    style = "font-size:0.85rem; color:#555; padding: 4px 12px 0 12px;"),
                  plotlyOutput("match_bars_esp"))
      ),
      card(
        full_screen = TRUE,
        card_header("Pressing intensity — Group Stage vs Knockout"),
        p("Spain pressed consistently higher and counter-pressed more throughout the tournament.",
          style = "font-size:0.85rem; color:#555; padding: 4px 12px 0 12px;"),
        plotlyOutput("press_shift", height = "320px")
      )
    )
  ),
  
  # TAB 3: The Final ----
  nav_panel(
    "The Final",
    
    # Row 1 — shot map and xG timeline
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header("Shot Map - Final"),
        p("Spain registered 23 shots to England's 8, with higher-quality chances concentrated centrally.",
          style = "font-size:0.85rem; color:#555; padding: 4px 12px 0 12px;"),
        plotlyOutput("shot_map", height = "400px")
      ),
      card(
        full_screen = TRUE,
        card_header("Cumulative xG Timeline - Final"),
        p("Spain dominated xG from the beginning. England's last shot of the game was in the 68th minute.",
          style = "font-size:0.85rem; color:#555; padding: 4px 12px 0 12px;"),
        plotlyOutput("xg_timeline", height = "400px")
      )
    ),
    
    # Row 2 — two cards side by side
    layout_columns(
      col_widths = c(6, 6),
      
      # Left — Pass Distribution by Pitch Third
      card(
        full_screen = TRUE,
        card_header("Pass Distribution by Pitch Third - Final"),
        p("Spain played more passes into the final third, showing attempts to create chances in the attacking end.",
          style = "font-size:0.85rem; color:#555; padding: 4px 12px 0 12px;"),
        plotlyOutput("pass_thirds", height = "380px")
      ),
      
      # Right — player passmap with player selector
      navset_card_tab(
        title = "Top Player actions — Final Third",
        full_screen = TRUE,
        nav_panel(
          "Bonmatí (Spain)",
          p("Bonmatí generated significantly more final-third actions than Hemp despite playing the same minutes.",
            style = "font-size:0.85rem; color:#555; padding: 4px 12px 0 12px;"),
          plotOutput("passmap_bonmati", height = "420px", width = "100%")
        ),
        nav_panel(
          "Hemp (England)",
          p("Bonmatí generated significantly more final-third actions than Hemp despite playing the same minutes.",
            style = "font-size:0.85rem; color:#555; padding: 4px 12px 0 12px;"),
          plotOutput("passmap_hemp", height = "420px", width = "100%")
        )
      )
    )
  ),
  
  # TAB 4: Was It Deserved? ----
  nav_panel(
    "Was It Deserved?",
    
    # Row 1 — headline value boxes
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title    = "Spain xG in the final",
        value    = "2.14",
        showcase = bsicons::bs_icon("bullseye"),
        theme    = "primary"
      ),
      value_box(
        title    = "England xG in the final",
        value    = "0.88",
        showcase = bsicons::bs_icon("bullseye"),
        theme    = "secondary"
      ),
      value_box(
        title    = "Spain led on metrics",
        value    = "7 of 7",
        showcase = bsicons::bs_icon("trophy-fill"),
        theme    = "primary"
      )
    ),
    
    # Row 2 — verdict text left, metric table right
    layout_columns(
      col_widths = c(4, 8),
      
      card(
        card_header("Analytical Outcome"),
        p("England won Women's Euro 2025 on penalties after a 1–1 draw,
        but the data tells a different story."),
        p("Spain dominated on every key metric in the tournament. They
        created higher quality chances, pressed higher,
        completed more passes, and progressed the ball further
        into the attacking third."),
        p("In the final, Spain generated 2.14 xG from 23 shots
        against England's 0.88 xG from 8 shots."),
        p(tags$em("The Table shows how England compared to Spain
                  across seven key analytical metrics."))
      ),
      
      card(
        full_screen = TRUE,
        card_header("Key Performance Metrics - England vs Spain"),
        p("Spain led England on all seven performance metrics. The data supports Spain as the deserving winner.",
          style = "font-size:0.85rem; color:#555; padding: 4px 12px 0 12px;"),
        gt_output("verdict_table")
      )
    )
    )
  )


# SERVER ----
server <- function(input, output, session) {
  # TAB 1 OUTPUTS ----
  
  ## xG lollipop ----
  output$xg_lollipop <- renderPlotly({
    p <- tbl_team_xg_summary %>%
      mutate(
        team_highlight = case_when(
          team == "Spain" ~ "Spain",
          team == "England" ~ "England",
          TRUE ~ "Other"
        ),
        team = fct_reorder(team, avg_xg_diff),
        tooltip    = paste0(
          team, "\n",
          "Avg xG diff: ", sprintf("%+.2f", avg_xg_diff)
        )
      ) %>%
      ggplot(aes(x = avg_xg_diff, y = team,
                 colour = team_highlight, text = tooltip)) +
      geom_vline(xintercept = 0, colour = "#9CA3AF",
                 linewidth = 0.5, linetype = "dashed") +
      geom_segment(aes(x = 0, xend = avg_xg_diff,
                       y = team, yend = team),
                   linewidth = 0.8, alpha = 0.6) +
      geom_point(size = 3.5) +
      scale_colour_manual(values = euro_colours) +
      scale_x_continuous(expand = expansion(mult = c(0.2, 0.25))) +
      labs(x = "Avg xG difference per match", y = NULL) +
      theme_euro() +
      theme(panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  ## xG scatter ----
  output$xg_scatter <- renderPlotly({
    p <- tbl_team_xg_summary %>%
      mutate(
        team_highlight = case_when(
          team == "Spain" ~ "Spain",
          team == "England" ~ "England",
          TRUE                      ~ "Other"
        ),
        tooltip    = paste0(
          team, "\n",
          "Total xG: ", round(total_xg, 2), "\n",
          "Goals: ", total_goals
        )
      ) %>%
      ggplot(aes(x = total_xg, y = total_goals,
                 colour = team_highlight, text = tooltip)) +
      geom_abline(slope = 1, intercept = 0,
                  colour = "#9CA3AF", linewidth = 0.5, linetype = "dashed") +
      geom_point(size = 3.5) +
      geom_text(
        data = ~ filter(.x, team_highlight %in% c("Spain", "England")),
        aes(label = team),
        nudge_y = 1.2,
        size = 3.1,
        fontface = "bold",
        show.legend = FALSE
      ) +
      scale_colour_manual(values = euro_colours) +
      scale_x_continuous(limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.1))) +
      scale_y_continuous(limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Total xG", y = "Total goals scored") +
      theme_euro()
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  # TAB 2 OUTPUTS ----
  ## Cumulative xG ----
  output$cumulative_xg <- renderPlotly({
    selected <- input$compare_team
    
    p <- tbl_cumulative_xg %>%
      mutate(
        is_visible = team == "England" | team == selected,
        tooltip = paste0(
          team, " · Match ", match_number,
          " vs ", opponent, "\n",
          stage, "\n",
          "xG this match: ", round(xg_total, 2),
          " | Goals: ", goals, "–", opp_goals, "\n",
          "Cumulative xG: ", round(cumulative_xg, 2)
        )
      ) %>%
      ggplot(aes(x = match_number, y = cumulative_xg,
                 group = team, colour = team)) +
      geom_line(
        data      = ~ filter(.x, !is_visible),
        linewidth = 0,
        alpha     = 0
      ) +
      geom_line(
        data      = ~ filter(.x, is_visible),
        linewidth = 1.8
      ) +
      geom_point(
        data = ~ filter(.x, is_visible),
        aes(text = tooltip),
        size = 2.5
      ) +
      geom_vline(xintercept = 3.5, colour = "#9CA3AF",
                 linewidth = 0.5, linetype = "dashed") +
      annotate("text", x = 2,
               y = max(tbl_cumulative_xg$cumulative_xg) * 0.95,
               label = "Group Stage", size = 3.5,
               colour = "#9CA3AF", hjust = 0.5) +
      annotate("text", x = 4.5,
               y = max(tbl_cumulative_xg$cumulative_xg) * 0.95,
               label = "Knockout", size = 3.5,
               colour = "#9CA3AF", hjust = 0.5) +
      scale_colour_manual(values = team_colours) +
      scale_x_continuous(
        breaks = 1:6,
        labels = paste("Match", 1:6),
        expand = expansion(mult = c(0.05, 0.1))
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(x = NULL, y = "Cumulative xG") +
      theme_euro() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  ## Match bars — England ----
  output$match_bars_eng <- renderPlotly({
    eng_data <- tbl_stage_breakdown %>%
      filter(team == "England") %>%
      arrange(match_date) %>%
      mutate(
        match_label = paste0("vs ", opponent),
        match_label = fct_inorder(match_label)
      ) %>%
      pivot_longer(
        cols      = c(xg_total, opp_xg),
        names_to  = "metric",
        values_to = "xg"
      ) %>%
      mutate(
        bar_colour = if_else(metric == "xg_total", "England", "Opponent"),
        bar_colour = fct_relevel(bar_colour, "England", "Opponent"),
        tooltip    = paste0(bar_colour, " xG: ", round(xg, 2))
      )
    
    p <- eng_data %>%
      ggplot(aes(x = match_label, y = xg,
                 fill = bar_colour, text = tooltip)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.65) +
      scale_fill_manual(
        values = c("England" = "#CE1124", "Opponent" = "#9CA3AF")
      ) +
      scale_y_continuous(
        limits = c(0, 7),
        expand = expansion(mult = c(0, 0.05))
      ) +
      labs(x = NULL, y = "xG", fill = NULL) +
      theme_euro() +
      theme(
        legend.position = "top",
        axis.text.x     = element_text(size = 7)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        yaxis = list(range = c(0, 7))
      )
  })
  
  ## Match bars — Spain ----
  output$match_bars_esp <- renderPlotly({
    esp_data <- tbl_stage_breakdown %>%
      filter(team == "Spain") %>%
      arrange(match_date) %>%
      mutate(
        match_label = paste0("vs ", opponent),
        match_label = fct_inorder(match_label)
      ) %>%
      pivot_longer(
        cols      = c(xg_total, opp_xg),
        names_to  = "metric",
        values_to = "xg"
      ) %>%
      mutate(
        bar_colour = if_else(metric == "xg_total", "Spain", "Opponent"),
        bar_colour = fct_relevel(bar_colour, "Spain", "Opponent"),
        tooltip    = paste0(bar_colour, " xG: ", round(xg, 2))
      )
    
    p <- esp_data %>%
      ggplot(aes(x = match_label, y = xg,
                 fill = bar_colour, text = tooltip)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.65) +
      scale_fill_manual(
        values = c("Spain" = "#F1BF00", "Opponent" = "#9CA3AF")
      ) +
      scale_y_continuous(
        limits = c(0, 7),
        expand = expansion(mult = c(0, 0.05))
      ) +
      labs(x = NULL, y = "xG", fill = NULL) +
      theme_euro() +
      theme(
        legend.position = "top",
        axis.text.x     = element_text(size = 7)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        yaxis = list(range = c(0, 7))
      )
  })
  
  ## Press shift ----
  output$press_shift <- renderPlotly({
    p <- tbl_press_summary %>%
      filter(team %in% c("Spain", "England")) %>%
      mutate(
        team_clean  = if_else(str_detect(team, "Spain"), "Spain", "England"),
        stage_label = if_else(stage_type == "group",
                              "Group Stage", "Knockout"),
        stage_label = fct_relevel(stage_label, "Group Stage", "Knockout")
      ) %>%
      pivot_longer(
        cols      = c(high_press_rate, counterpress_rate),
        names_to  = "metric",
        values_to = "rate"
      ) %>%
      mutate(
        metric  = if_else(metric == "high_press_rate",
                          "High-press rate", "Counter-press rate"),
        metric  = fct_relevel(metric, "High-press rate",
                              "Counter-press rate"),
        tooltip = paste0(team_clean, " · ", stage_label, "\n",
                         metric, ": ", rate, "%")
      ) %>%
      ggplot(aes(x = stage_label, y = rate,
                 fill = team_clean, text = tooltip)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      facet_wrap(~ metric) +
      scale_fill_manual(values = euro_colours, name = NULL) +
      scale_y_continuous(limits = c(0, 90),
                         expand = expansion(mult = c(0, 0.05))) +
      labs(x = NULL, y = "Rate (%)") +
      theme_euro() +
      theme(
        legend.position = "none",
        strip.text      = element_text(face = "bold", size = 10,
                                       colour = "#1F2937"),
        panel.spacing   = unit(1.5, "lines")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x           = 0.5,
          xanchor     = "center",
          y           = -0.15,
          yanchor     = "top"
        )
      )
  })
  
  # TAB 3 OUTPUTS ----
  ## Shot map ----
  output$shot_map <- renderPlotly({
    p <- tbl_final_shots %>%
      mutate(
        plot_x      = if_else(team == "England",
                              120 - location_x, location_x),
        plot_y      = if_else(team == "England",
                              80  - location_y, location_y),
        tooltip = paste0(
          team, " · ", player, "\n",
          "Minute: ", minute, "'\n",
          "xG: ", round(xg, 3), "\n",
          "Outcome: ", shot_outcome, "\n",
          "Technique: ", shot_body_part
        )
      ) %>%
      ggplot(aes(x = plot_x, y = plot_y)) +
      annotate_pitch(dimensions = pitch_statsbomb,
                     colour = "white", fill = "#4a7c3f") +
      geom_point(
        aes(size   = xg,
            colour = team,
            text   = tooltip),
        alpha = 0.9
      ) +
      geom_point(
        data   = ~ filter(.x, shot_outcome == "Goal"),
        aes(size = xg, text = tooltip),
        shape  = 21,
        fill   = NA,
        colour = "white",
        stroke = 1.5
      ) +
      scale_colour_manual(values = euro_colours, name = NULL) +
      scale_size_continuous(
        range  = c(2, 8),
        breaks = c(0.05, 0.2, 0.5),
        labels = c("0.05", "0.20", "0.50")
      ) +
      scale_size_continuous(
        range = c(2, 8),
        guide = "none"
      ) +
      guides(
        colour = guide_legend(
          override.aes = list(size = 5, alpha = 1))
      ) +
      coord_cartesian(xlim = c(0, 120), ylim = c(0, 80)) +
      labs(
        subtitle = "Circle size = xG value · England attack left · Spain attack right · white ring = goal"
      ) +
      theme_pitch() +
      theme(
        plot.subtitle   = element_text(size = 9, colour = "#6B7280",
                                       margin = margin(b = 6)),
        legend.position = "bottom",
        legend.text     = element_text(size = 10, colour = "#1F2937"),
        plot.background = element_rect(fill = "white", colour = NA)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = TRUE,
             legend = list(
               orientation = "h",
               x = 0.5, xanchor = "center",
               y = 1.05, yanchor = "bottom"
             ))
  })
  
  ## xG timeline ----
  output$xg_timeline <- renderPlotly({
    p <- tbl_final_timeline %>%
      mutate(
        tooltip    = paste0(
          team, " · ", player, "\n",
          "Minute: ", minute, "'\n",
          "xG: ", round(xg, 3), "\n",
          "Outcome: ", shot_outcome, "\n",
          "Cumulative xG: ", round(cumulative_xg, 3)
        )
      ) %>%
      ggplot(aes(x = minute, y = cumulative_xg,
                 colour = team, group = team)) +
      geom_vline(xintercept = 45,  colour = "#9CA3AF",
                 linewidth = 0.4, linetype = "dashed") +
      geom_vline(xintercept = 90,  colour = "#9CA3AF",
                 linewidth = 0.4, linetype = "dashed") +
      geom_vline(xintercept = 105, colour = "#9CA3AF",
                 linewidth = 0.4, linetype = "dashed") +
      annotate("text", x = 22,  y = 2.2,
               label = "1st half", size = 2.8, colour = "#9CA3AF") +
      annotate("text", x = 67,  y = 2.2,
               label = "2nd half", size = 2.8, colour = "#9CA3AF") +
      annotate("text", x = 100, y = 2.2,
               label = "ET", size = 2.8, colour = "#9CA3AF") +
      geom_step(linewidth = 1.2) +
      geom_point(aes(text = tooltip), size = 2) +
      geom_point(
        data   = ~ filter(.x, shot_outcome == "Goal"),
        size   = 3, shape = 21,
        fill   = NA, colour = "#2E7D32", stroke = 1.5
      ) +
      scale_colour_manual(values = euro_colours, name = NULL) +
      scale_x_continuous(
        breaks = c(0, 45, 90, 105, 120),
        labels = c("0'", "45'", "90'", "105'", "120'"),
        expand = expansion(mult = c(0.02, 0.05))
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
      labs(x = "Minute", y = "Cumulative xG") +
      theme_euro() +
      theme(legend.position = "top")
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = 1.05, yanchor = "bottom"
        )
      )
  })
  
  ## Pitch Thirds ----
  output$pass_thirds <- renderPlotly({
    p <- tbl_final_pass_thirds %>%
      ggplot(aes(x = pitch_third, y = pct,
                 fill = team, text = tooltip)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      scale_fill_manual(values = euro_colours, name = NULL) +
      scale_y_continuous(
        limits = c(0, 60),
        expand = expansion(mult = c(0, 0.05)),
        labels = function(x) paste0(x, "%")
      ) +
      labs(x = NULL, y = "Share of completed passes (%)") +
      theme_euro() +
      theme(legend.position = "top")
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x           = 0.5,
          xanchor     = "center",
          y           = 1.05,
          yanchor     = "bottom"
        )
      )
  })
 
  ## Player passmap — Bonmatí ----
  output$passmap_bonmati <- renderPlot({
    tbl_final_player_actions %>%
      filter(player_label == "Bonmatí (Spain)") %>%
      ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb,
                     colour = "white", fill = "#4a7c3f") +
      geom_segment(
        aes(x = location_x, y = location_y,
            xend = end_x, yend = end_y,
            colour = action_type),
        linewidth = 0.7, alpha = 0.7,
        arrow = arrow(length = unit(0.12, "cm"), type = "closed")
      ) +
      scale_colour_manual(
        values = c("Pass" = "#F1BF00", "Carry" = "grey69"),
        name   = NULL,
        labels = c("Pass" = "Final-third pass",
                   "Carry" = "Progressive carry")
      ) +
      coord_cartesian(xlim = c(60, 121), ylim = c(0, 80)) +
      theme_pitch() +
      theme(
        legend.position = "bottom",
        legend.text     = element_text(size = 10, colour = "#1F2937"),
        plot.background = element_rect(fill = "white", colour = NA)
      )
  }, bg = "white")
  
  ## Player passmap — Hemp ----
  output$passmap_hemp <- renderPlot({
    tbl_final_player_actions %>%
      filter(player_label == "Hemp (England)") %>%
      ggplot() +
      annotate_pitch(dimensions = pitch_statsbomb,
                     colour = "white", fill = "#4a7c3f") +
      geom_segment(
        aes(x = location_x, y = location_y,
            xend = end_x, yend = end_y,
            colour = action_type),
        linewidth = 0.7, alpha = 0.7,
        arrow = arrow(length = unit(0.12, "cm"), type = "closed")
      ) +
      scale_colour_manual(
        values = c("Pass" = "#CE1124", "Carry" = "grey69"),
        name   = NULL,
        labels = c("Pass" = "Final-third pass",
                   "Carry" = "Progressive carry")
      ) +
      coord_cartesian(xlim = c(60, 121), ylim = c(0, 80)) +
      theme_pitch() +
      theme(
        legend.position = "bottom",
        legend.text     = element_text(size = 10, colour = "#1F2937"),
        plot.background = element_rect(fill = "white", colour = NA)
      )
  }, bg = "white")

  # TAB 4 OUTPUTS ----
  ## Verdict table — static ----
  output$verdict_table <- render_gt({
    tbl_verdict_summary %>%
      gt(groupname_col = "Dimension") %>%
      tab_header(
        title    = md("**England vs Spain — Key Metrics**"),
        subtitle = "Tournament averages · Women's Euro 2025"
      ) %>%
      tab_spanner(
        label   = "Team",
        columns = c(England, Spain)
      ) %>%
      tab_style(
        style     = cell_fill(color = "#FEF9EC"),
        locations = cells_body(
          columns = Spain,
          rows    = Spain_leads == TRUE
        )
      ) %>%
      tab_style(
        style     = cell_fill(color = "#FFF0F0"),
        locations = cells_body(
          columns = England,
          rows    = Spain_leads == FALSE
        )
      ) %>%
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body(
          columns = Spain,
          rows    = Spain_leads == TRUE
        )
      ) %>%
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body(
          columns = England,
          rows    = Spain_leads == FALSE
        )
      ) %>%
      tab_style(
        style     = cell_text(color = "#6B7280", size = "small"),
        locations = cells_row_groups()
      ) %>%
      cols_hide(columns = Spain_leads) %>%
      cols_align(
        align   = "center",
        columns = c(England, Spain)
      ) %>%
      opt_table_font(font = "system-ui") %>%
      tab_options(
        table.border.top.color      = "white",
        heading.border.bottom.color = "#E5E7EB",
        row_group.border.top.color  = "#E5E7EB",
        column_labels.font.weight   = "bold",
        table.width                 = pct(100)
      )
  })

}

shinyApp(ui, server)
