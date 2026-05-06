# ---- scripts/02_explore.R ----
# Purpose: Explore the raw StatsBomb event data for Euro 2025
# to understand available variables, event types,
# and data quality before cleaning.

# NOTE: Raw data is already loaded in environment from 01_data.R
# If starting fresh, uncomment these two lines:
# events_raw    <- readRDS("data/raw/weuro2025_events.rds")
# weuro_matches <- readRDS("data/raw/weuro2025_matches.rds")

# 1. DATA STRUCTURE OVERVIEW ----
# All column names (143 columns total)
names(events_raw)

# Missing data summary — most NAs are expected because columns
# only apply to specific event types (e.g. shot columns are NA
# for pass events and vice versa)
miss_var_summary(events_raw) %>% print(n=150)

# All unique event types in the dataset
unique(events_raw$type.name)

# 2. EVENT TYPE COUNTS ----
# How many of each event type across all 31 matches?
# This gives a sense of scale and what's available to analyse.

events_raw %>%
  summarise(
    half.start = sum(type.name == "Half Start", na.rm = TRUE), # 150
    starting11 = sum(type.name == "Starting XI", na.rm = TRUE), # 62
    passes = sum(type.name == "Pass", na.rm = TRUE), # 29,108
    ball.receipt = sum(type.name == "Ball Receipt*", na.rm = TRUE), # 24,646
    carry = sum(type.name == "Carry", na.rm = TRUE), # 22,960
    pressure = sum(type.name == "Pressure", na.rm = TRUE), # 11,784
    duel = sum(type.name == "Duel", na.rm = TRUE), # 2,070
    miscontrol = sum(type.name == "Miscontrol", na.rm = TRUE), # 946
    ball.recovery = sum(type.name == "Ball Recovery", na.rm = TRUE), # 3,512
    clearance = sum(type.name == "Clearance", na.rm = TRUE), # 1,280
    block = sum(type.name == "Block", na.rm = TRUE), # 1,494
    goalkeeper = sum(type.name == "Goal Keeper", na.rm = TRUE), # 1,184
    shot = sum(type.name == "Shot", na.rm = TRUE), # 912
    interception = sum(type.name == "Interception", na.rm = TRUE), # 516
    dispossessed = sum(type.name == "Dispossessed", na.rm = TRUE), # 812
    dribble = sum(type.name == "Dribble", na.rm = TRUE), # 942
    dribbled.past = sum(type.name == "Dribbled Past", na.rm = TRUE), # 486
    foul.committed = sum(type.name == "Foul Committed", na.rm = TRUE), # 721
    foul.won = sum(type.name == "Foul Won", na.rm = TRUE), # 681
    error = sum(type.name == "Error", na.rm = TRUE), # 28
    shield = sum(type.name == "Shield", na.rm = TRUE), # 50
    fifty.fifty = sum(type.name == "50/50", na.rm = TRUE), # 228
    injury.stoppage = sum(type.name == "Injury Stoppage", na.rm = TRUE), # 226
    substitution = sum(type.name == "Substitution", na.rm = TRUE), # 289
    ref.ball.drop = sum(type.name == "Referee Ball-Drop", na.rm = TRUE), # 102
    half.end = sum(type.name == "Half End", na.rm = TRUE), # 150
    tact.shift = sum(type.name == "Tactical Shift", na.rm = TRUE), # 126
    player.off = sum(type.name == "Player Off", na.rm = TRUE), # 56
    player.on = sum(type.name == "Player On", na.rm = TRUE), # 56
    offside = sum(type.name == "Offside", na.rm = TRUE), # 17
    bad.behaviour = sum(type.name == "Bad Behaviour", na.rm = TRUE), # 11
    own.goal.for = sum(type.name == "Own Goal For", na.rm = TRUE), # 3
    own.goal.against = sum(type.name == "Own Goal Against", na.rm = TRUE), # 3
    counterpress = sum(counterpress == 1, na.rm = TRUE) # 3,663
    )

# The amount of carries (22,960) is almost as high as passes (29,108) 
# carries are far more common than I expected and worth looking at 
# for the ball progression analysis
# Complete and incomplete dribbles are almost exactly split (455 vs 487) —
# slight edge to defenders across the tournament

# 11,784 pressure events across 31 matches — roughly 380 per match,
# enough for a meaningful pressing analysis per team

# 3. PLAY PATTERN COUNTS ----
# play_pattern.name describes what situation led to each event
# e.g. open play, corner, free kick, counter attack etc.
# NOTE: high "from.throw.in" count just means the next action
# followed a throw-in, not that throw-ins were used tactically.

events_raw %>%
  summarise(
    regular.play = sum(play_pattern.name == "Regular Play", na.rm = TRUE), # 45,902
    from.kick.off = sum(play_pattern.name == "From Kick Off", na.rm = TRUE), # 2,955
    from.throw.in = sum(play_pattern.name == "From Throw In", na.rm = TRUE), # 27,052
    from.keeper = sum(play_pattern.name == "From Keeper", na.rm = TRUE), # 4,775
    from.goal.kick = sum(play_pattern.name == "From Goal Kick", na.rm = TRUE), # 7.707
    from.corner = sum(play_pattern.name == "From Corner", na.rm = TRUE), # 3,695
    from.free.kick = sum(play_pattern.name == "From Free Kick", na.rm = TRUE), # 12,077
    other = sum(play_pattern.name == "Other", na.rm = TRUE), # 637
    from.counter = sum(play_pattern.name == "From Counter", na.rm = TRUE) # 811
  )

# The vast majority are regular plays
# set pieces (corners and free kicks) are a small

# 4. SHOTS AND OFFENSIVE EVENTS ----
# Available categories within shot columns
unique(events_raw$shot.body_part.name)  
# Head, Right Foot, Left Foot, Other

unique(events_raw$shot.type.name)       
# Open Play, Penalty, Free Kick

unique(events_raw$shot.technique.name)  
# Normal, Volley, Half Volley, etc.

# Shot breakdown by body part, type and technique
events_raw %>%
  summarise(
    # shot context
    shot.one_on_one = sum(shot.one_on_one == TRUE, na.rm = TRUE), # 29
    shot.first_time = sum(shot.first_time == TRUE, na.rm = TRUE), # 308
    # body part
    shot.head = sum(shot.body_part.name == "Head", na.rm = TRUE), # 151
    shot.right = sum(shot.body_part.name == "Right Foot", na.rm = TRUE), # 501
    shot.left = sum(shot.body_part.name == "Left Foot", na.rm = TRUE), # 256
    shot.other = sum(shot.body_part.name == "Other", na.rm = TRUE), # 4
    # shot type
    shot.open_play = sum(shot.type.name == "Open Play", na.rm = TRUE), # 851
    shot.penalty = sum(shot.type.name == "Penalty", na.rm = TRUE), # 51
    shot.freekick = sum(shot.type.name == "Freekick", na.rm = TRUE), # 0
    # shot technique
    shot.normal = sum(shot.technique.name == "Normal", na.rm = TRUE), # 709
    shot.half_volley = sum(shot.technique.name == "Half Volley", na.rm = TRUE), # 119
    shot.backheel = sum(shot.technique.name == "Backheel", na.rm = TRUE), # 10
    shot.lob = sum(shot.technique.name == "Lob", na.rm = TRUE), # 13
    shot.overhead = sum(shot.technique.name == 'Overhead Kick', na.rm = TRUE), # 3
    shot.volley = sum(shot.technique.name == "Volley", na.rm = TRUE) # 58
  )

# Additional shot flags (all TRUE/NA columns)
# These are binary flags — either TRUE or NA, no FALSE values
# more shot event summary
events_raw %>%
  summarise(
    shot.aerial = sum(shot.aerial_won, na.rm = TRUE), # 79
    shot.defelcted = sum(shot.deflected, na.rm = TRUE), # 12
    foul.won.pen = sum(foul_won.penalty, na.rm = TRUE), # 12
    shot.saved.post = sum(shot.saved_to_post, na.rm = TRUE), # 6
    shot.open.goal = sum(shot.open_goal, na.rm = TRUE), # 7
    shot.redirect = sum(shot.redirect, na.rm = TRUE), # 2
    shot.follow.dribble = sum(shot.follows_dribble, na.rm = TRUE) # 1
  )


# 5. PASSING EVENTS ----
# Available categories within pass columns
unique(events_raw$pass.type.name)     
# Kick Off, Corner, Free Kick etc.

unique(events_raw$pass.outcome.name)  
# Incomplete, Out, Pass Offside etc.

unique(events_raw$pass.technique.name) 
# Straight, Through Ball, Inswinging etc.

# Pass type, outcome, and technique breakdown
events_raw %>%
  summarise(
    # pass type 
    kickoff = sum(pass.type.name == "Kick Off", na.rm = TRUE), # 178
    throw.in = sum(pass.type.name == "Throw-In", na.rm = TRUE), # 0
    recovery = sum(pass.type.name == "Recovery", na.rm = TRUE), # 2,663
    interception = sum(pass.type.name == "Interception", na.rm = TRUE), # 160
    goalkick = sum(pass.type.name == "Goal Kick", na.rm = TRUE), # 569
    corner = sum(pass.type.name == "Corner", na.rm = TRUE), # 305
    freekick = sum(pass.type.name == "Free Kick", na.rm = TRUE), # 719
    # pass outcome: 6574, 105,611
    incomplete = sum(pass.outcome.name == "Incomplete", na.rm = TRUE), # 5,651
    out = sum(pass.outcome.name == "Out", na.rm = TRUE), # 690
    pass.offisde = sum(pass.outcome.name == "Pass Offside", na.rm = TRUE), # 88
    unknown = sum(pass.outcome.name == "Unknown", na.rm = TRUE), # 138
    injury.clearance = sum(pass.outcome.name == "Injury Clearance", na.rm = TRUE), # 7
    nas = sum(is.na(pass.outcome.name)), # 99,037
    # pass technique: 354
    straight = sum(pass.technique.name == "Straight", na.rm = TRUE), # 45
    through.ball = sum(pass.technique.name == "Through Ball", na.rm = TRUE), # 94
    inswinging = sum(pass.technique.name == "Inswinging", na.rm = TRUE), # 161
    outswinging = sum(pass.technique.name == "Outswinging", na.rm = TRUE), # 54
    # body part pass: 27,066
    bodypart_rightfoot = sum(pass.body_part.name == "Right Foot", na.rm = TRUE), # 18,097
    bodypart_head = sum(pass.body_part.name == "Head", na.rm = TRUE), # 985
    bodypart_leftfoot = sum(pass.body_part.name == "Left Foot", na.rm = TRUE), # 7,564
    bodypart_keeperarm = sum(pass.body_part.name == "Keeper Arm", na.rm = TRUE), # 216
    bodypart_other = sum(pass.body_part.name == "Other", na.rm = TRUE), # 78
    bodypart_dropkick = sum(pass.body_part.name == "Drop Kick", na.rm = TRUE), # 111
    bodypart_notouch = sum(pass.body_part.name == "No Touch", na.rm = TRUE) # 15
    )

# 99,037 passes have NA outcome — StatsBomb's convention is that 
# NA = completed, only failed passes get an outcome name recorded.
# That's important for pass completion calculations later.

# Pass quality and creation flags
# These are particularly relevant for player/team analysis
events_raw %>%
  summarise(
    aerial_passes_won = sum(pass.aerial_won == TRUE, na.rm = TRUE), # 508
    pass_switch = sum(pass.switch == TRUE, na.rm = TRUE), # 602
    pass_shot_assist = sum(pass.shot_assist == TRUE, na.rm = TRUE), # 548
    pass.deflected = sum(pass.deflected == TRUE, na.rm = TRUE), # 21
    pass.cross = sum(pass.cross == TRUE, na.rm = TRUE), # 785
    pass.straight = sum(pass.straight == TRUE, na.rm = TRUE), # 45
    pass.through_ball = sum(pass.through_ball == TRUE, na.rm = TRUE), # 94
    pass.inswinging = sum(pass.inswinging == TRUE, na.rm = TRUE), # 161
    pass.goal_assist = sum(pass.goal_assist == TRUE, na.rm = TRUE), # 70
    pass.cut_back = sum(pass.cut_back == TRUE, na.rm = TRUE), # 86
    pass.outswing = sum(pass.outswinging == TRUE, na.rm = TRUE), # 54
    right_pass = sum(pass.body_part.name == "Right Foot", na.rm = TRUE), # 18,097
    left_pass = sum(pass.body_part.name == "Left Foot",  na.rm = TRUE), # 7,564
    head_pass = sum(pass.body_part.name == "Head",       na.rm = TRUE), # 985
    pass.no.touch = sum(pass.no_touch == TRUE, na.rm = TRUE), # 15
    pass.misscom = sum(pass.miscommunication == TRUE, na.rm = TRUE) # 5
  )

# 6. DRIBBLES ----
unique(events_raw$dribble.outcome.name) # Complete or Incomplete

# Dribble completion rate
events_raw %>%
  summarise(
    complete.dribble = sum(dribble.outcome.name == "Complete", na.rm = TRUE), # 455
    incomplete.dribble = sum(dribble.outcome.name == "Incomplete", na.rm = TRUE) # 487
  )

# Complete and incomplete dribbles are almost exactly split (455 vs 487) —
# slight edge to defenders across the tournament

# 7. DEFENSIVE EVENTS ----
## 7a. Duels ----
unique(events_raw$duel.type.name)
# Aerial Lost, Tackle

unique(events_raw$duel.outcome.name)
# # Won, Lost, Success, In Play etc.

# duel type and outcome summary
events_raw %>%
  summarise(
    # duel type
    duel.aerial.lost = sum(duel.type.name == "Aerial Lost", na.rm = TRUE), # 803
    duel.tackle = sum(duel.type.name == "Tackle", na.rm = TRUE), # 1,267
    # duel outcome
    duel.success.in.play = sum(duel.outcome.name == "Success In Play", na.rm = TRUE), # 376
    duel.lost.in.play = sum(duel.outcome.name == "Lost In Play", na.rm = TRUE), # 282
    duel.won = sum(duel.outcome.name == "Won", na.rm = TRUE), # 301
    duel.lost.out = sum(duel.outcome.name == "Lost Out", na.rm = TRUE), # 252
    duel.success.out = sum(duel.outcome.name == "Success Out", na.rm = TRUE) # 56
  )

## 7b. Clearances ----
events_raw %>%
  summarise(
    clearance_head = sum(clearance.body_part.name == "Head", na.rm = TRUE), # 563
    clearance_right = sum(clearance.body_part.name == "Right Foot", na.rm = TRUE), # 455
    clearance_left = sum(clearance.body_part.name == "Left Foot", na.rm = TRUE), # 250
    clearance.other = sum(clearance.body_part.name == "Other", na.rm = TRUE), # 12
    aerial.won = sum(clearance.aerial_won == TRUE, na.rm = TRUE) # 191
  )

## 7c. Interceptions ----
# interception outcome summary
unique(events_raw$interception.outcome.name)
# Won, Lost In Play, Lost Out, Success In Play, Success Out

events_raw %>%
  summarise(
    won.interception = sum(interception.outcome.name == "Won", na.rm = TRUE), # 225
    lost.in.play.interception = sum(interception.outcome.name == "Lost In Play", na.rm = TRUE), # 110
    lost.out.interception = sum(interception.outcome.name == "Lost Out", na.rm = TRUE), # 89
    success.in.play.interception = sum(interception.outcome.name == "Success In Play", na.rm = TRUE), # 87
    success.out.interception = sum(interception.outcome.name == "Success Out", na.rm = TRUE) # 5
  )

## 7d. Fouls----
events_raw %>%
  summarise(
    foul.committed = sum(type.name == "Foul Committed", na.rm = TRUE), # 721
    foul.won = sum(type.name == "Foul Won", na.rm = TRUE)  # 681
  )

# Roughly balanced — neither team had a notable foul count advantage overall

# 8. PENALTY SHOOTOUT CHECK ----
# StatsBomb records penalty shootouts as period = 5
# These must be excluded from all analytical tables
# Affected matches: Sweden v England QF (4018355),
# France v Germany QF (4018357),
# England v Spain Final (4020846)

# Confirm period 5 exists
unique(events_raw$period)

# Check which matches have period 5 penalty shots
events_raw %>%
  filter(type.name == "Shot", shot.type.name == "Penalty") %>%
  count(match_id, period, team.name)
# 3 Matches affected
# Confirmed: period 5 = shootout. All cleaned tables will use:
# filter(as.integer(period) <= 4)

# Three matches went to shootouts — one of them is the final
# This means England's penalty win is literally in the data (period 5) and 
# needs to be stripped out before any xG comparison

# Key decisions for 03_clean.R:
# - Filter period 5 (penalty shootouts) — affects 3 matches
# - NA in pass.outcome.name = completed pass (not missing data)
# - counterpress column is TRUE or NA, never FALSE — recode before analysis
# - allclean() needed to extract x/y coordinates and ElapsedTime from nested columns

# ./_publish.sh "02_explore: event type counts, shot/pass breakdowns, penalty shootout check"