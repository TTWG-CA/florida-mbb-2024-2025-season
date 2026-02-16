library(hoopR)
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(stringr)
library(ggrepel)
library(readr)
#NCAA MBB 2025 Data
pbp_2025 = load_mbb_pbp(seasons = 2025)

#All Florida's Games
florida_pbp = pbp_2025 %>%
  filter(home_team_name == "Florida" | away_team_name == "Florida")
view(florida_pbp)


#March Madness Tournament Games
tourney_pbp = pbp_2025 %>%
  filter(game_date >= "2025-03-20")

#Florida's Tournament Runs
florida = tourney_pbp %>%
  filter(home_team_name == "Florida" | away_team_name == "Florida")

#Florida vs Houston Championship Game
title_game_pbp = pbp_2025 %>%
  filter((home_team_name == "Florida" & away_team_name == "Houston") |
           (home_team_name == "Houston" & away_team_name == "Florida"))

#Analysis of Florida vs Houston Championship Game
view(title_game_pbp)

title_game_pbp = title_game_pbp %>%
  mutate(
    clock_sec = ms(clock_display_value) %>% as.numeric(),
    clock_minutes = clock_sec / 60,
    plot_minutes = case_when(
      period_display_value == "1st Half" ~ clock_minutes + 20,
      period_display_value == "2nd Half" ~ clock_minutes
    ),score_diff = home_score - away_score, lead_color = case_when(
      score_diff > 0 ~ "Houston",
      score_diff < 0 ~ "Florida",
      TRUE ~ "Tied"
    )
  )

ggplot(title_game_pbp, aes(
  x = plot_minutes,
  y = score_diff,
  color = score_diff
)) +
  geom_line(size = 1.6) +
  scale_color_steps2(
    low = "#0021A5",      # Florida BLUE
    mid = "gray80",
    high = "#C8102E",     # Houston red
    midpoint = 0,
    n = 9                 # keeps blue strong
  ) +
  scale_x_reverse(
    breaks = c(40, 30, 20, 10, 0),
    labels = c("20:00", "10:00", "0:00", "10:00", "0:00")
  ) +
  labs(
    title = "Score Differential (Florida vs Houston)",
    x = "Game Time (Minutes)",
    y = "Houston Lead (+) / Florida Lead (-)",
    color = "Score Diff"
  ) +
  theme_minimal()

#Clutch Performance (last 5 minutes)
clutch = title_game_pbp %>%
  filter(plot_minutes <= 5)
range(clutch$plot_minutes, na.rm = TRUE)


clutch_scoring = clutch %>%
  arrange(period_number, start_game_seconds_remaining, game_play_number) %>%
  mutate(
    d_home = home_score - lag(home_score, default = first(home_score)), 
    d_away = away_score - lag(away_score, default = first(away_score))
  ) %>%
  summarise(
    houston_points_clutch = sum(d_home, na.rm = TRUE), 
    florida_points_clutch = sum(d_away, na.rm = TRUE), 
    net_florida = florida_points_clutch - houston_points_clutch)
  

clutch_scoring

clutch_events = clutch %>%
  mutate(
    is_turnover = str_detect(type_text, regex("turnover", ignore_case = TRUE)),
    is_foul = str_detect(type_text, regex("foul", ignore_case = TRUE)),
    is_ft = str_detect(type_text, regex("free throw", ignore_case = TRUE)),
    is_oreb = str_detect(type_text, regex("offensive rebound", ignore_case = TRUE)),
    is_shot = shooting_play == TRUE
  ) %>%
  summarise(
  turnovers = sum(is_turnover, na.rm = TRUE),
  fouls = sum(is_foul, na.rm = TRUE),
  ft_attemps = sum(is_ft, na.rm = TRUE),
  offensive_rebounds = sum(is_oreb, na.rm = TRUE),
  made_shots = sum(scoring_play == TRUE & is_shot, na.rm = TRUE)
)

clutch_events

clutch_plot = clutch %>%
  mutate(score_diff = home_score - away_score)

ggplot(clutch_plot, aes(x = plot_minutes, y = score_diff, color = score_diff)) +
  geom_line(size = 1.6) +
  scale_color_steps2(
    low = "#0021A5",      # Florida BLUE
    mid = "gray80",
    high = "#C8102E",     # Houston red
    midpoint = 0,
    n = 9,
  ) +
  scale_x_reverse()+
  labs(
    title = "Clutch Time (Last 5 Minutes)", 
    x = "Minutes Remaining",
    y = "Houston Lead (+) / Florida Lead (-)"
  )

#Player Clutch Performance (last 5 minutes)
clutch_scorers= clutch %>%
  filter(scoring_play == TRUE, score_value > 0, !is.na(athlete_id_1)) %>%
  group_by(athlete_id_1) %>%
  summarise(
    clutch_points = sum(score_value, na.rm = TRUE),
    scoring_plays = n(),
    example_text  = first(text),
    .groups = "drop"
  ) %>%
  arrange(desc(clutch_points))

print(clutch_scorers)

name_map = tibble::tribble(
  ~athlete_id_1, ~player_name,
  4897262, "Will Richard",
  4896372, "Walter Clayton Jr.",
  4433149, "L.J. Cryer",
  4684301, "Ja'Vier Francis",
  4702656, "Alijah Martin",
  5060700, "Joseph Tugler",
  5105527, "Denzel Aberdeen"
)

clutch_player_impact = clutch %>%
  filter(!is.na(athlete_id_1)) %>%
  mutate(
    is_turnover = str_detect(type_text, regex("turnover", ignore_case = TRUE)),
    is_shot = shooting_play == TRUE,
    team = case_when(
      team_id == home_team_id ~ home_team_name,
      team_id == away_team_id ~ away_team_name,
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(athlete_id_1, team) %>%
  summarise(
    clutch_points = sum(score_value[scoring_play == TRUE], na.rm = TRUE),
    clutch_turnovers = sum(is_turnover, na.rm = TRUE),
    clutch_shot_attempts = sum(is_shot, na.rm = TRUE),
    example_text = first(text),
    .groups = "drop"
  ) %>%
  filter(clutch_points > 0) %>%   # scorers only
  arrange(desc(clutch_points))

missing_ids= setdiff(clutch_player_impact$athlete_id_1, name_map$athlete_id_1)
print(missing_ids)

clutch_player_impact_named <- clutch_player_impact %>%
  left_join(name_map, by = "athlete_id_1")


ggplot(clutch_player_impact_named, aes(
  x = clutch_points,
  y = clutch_turnovers,
  size = clutch_shot_attempts,
  color = team
)) +
  geom_point(alpha = 0.65) +
  ggrepel::geom_text_repel(
    aes(label = player_name),
    size = 4,
    box.padding = 0.6,
    point.padding = 0.5,
    force = 2,
    max.overlaps = Inf,
    segment.alpha = 0.4
  ) +
  scale_color_manual(values = c(
    "Florida" = "#0021A5",
    "Houston" = "#C8102E"
  )) +
  scale_y_continuous(breaks = 0:4) +
  labs(
    title = "Clutch Player Impact (Last 5 Minutes)",
    subtitle = "Only players who scored in clutch time are shown",
    x = "Clutch Points Scored",
    y = "Clutch Turnovers",
    size = "Clutch Shot Attempts",
    color = "Team"
  ) +
  theme_minimal()


#Shooting % (FT, 2PT, 3PT)
game_shots = title_game_pbp %>%
  filter(shooting_play == TRUE)

game_shots = game_shots %>%
  mutate(
    shot_type = case_when(
      score_value == 1 ~ "FT",
      score_value == 2 ~ "2PT", 
      score_value == 3 ~ "3PT",
      TRUE ~ NA_character_
    ),
    made = scoring_play == TRUE,
    team = case_when(
      team_id == home_team_id ~ home_team_name,
      team_id == away_team_id ~ away_team_name
    )
  )

shooting_summary = game_shots %>%
  filter(!is.na(shot_type)) %>%
  group_by(team, shot_type) %>%
  summarise(
    makes = sum(made), 
    attempts = n(), 
    pct = makes / attempts, 
    .groups = "drop"
  )

shooting_summary


ggplot(shooting_summary, aes(
  x = shot_type,
  y = pct, 
  fill = team
))+
  geom_col(position =  "dodge", alpha = .7) +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c(
    "Florida" = "#0021A5",
    "Houston" = "#C8102E"
  ))+
  labs(
    title = "Shooting Efficiency by Shot Type",
    x = "Shot Type", 
    y = "Shooting Percentage", 
    fill = "Team"
  )+
  theme_minimal()



#Florida Offensive and Defensive Efficiency
team_possessions= title_game_pbp %>%
  filter(!is.na(team_id)) %>%
  mutate(
    is_ft  = shooting_play == TRUE & points_attempted == 1,
    is_fga = shooting_play == TRUE & points_attempted %in% c(2, 3),
    is_to   = str_detect(type_text, regex("Turnover", ignore_case = TRUE)),
    is_oreb = str_detect(type_text, regex("Offensive Rebound", ignore_case = TRUE)),
    team = case_when(
      team_id == home_team_id ~ home_team_name,
      team_id == away_team_id ~ away_team_name,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(team)) %>%
  group_by(team) %>%
  summarise(
    FGA = sum(is_fga, na.rm = TRUE),
    FTA = sum(is_ft,  na.rm = TRUE),
    TO  = sum(is_to,  na.rm = TRUE),
    OREB = sum(is_oreb, na.rm = TRUE),
    possessions = FGA + 0.475 * FTA - OREB + TO,
    .groups = "drop"
  )

team_possessions

possessions_game = mean(round(team_possessions$possessions))
possessions_game

final_score = title_game_pbp %>%
  summarise(
    home_points = max(home_score, na.rm = TRUE),
    away_points = max(away_score, na.rm = TRUE))
florida_points = final_score$away_points
houston_points = final_score$home_points  

florida_off_eff = 100 * florida_points / possessions_game
florida_def_eff = 100 * houston_points / possessions_game

