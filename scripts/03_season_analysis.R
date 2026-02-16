#Florida's ID 
f1_team_id = pbp_2025 %>%
  filter(home_team_name == "Florida" | away_team_name == "Florida") %>%
  summarise(
    f1_id = first(c(home_team_id[home_team_name == "Florida"],
                    away_team_id[away_team_name == "Florida"]))
  ) %>%
  pull(f1_id)

#Florida's Regular Season Games 
f1_all_games = pbp_2025 %>% 
  filter(home_team_id == f1_team_id | away_team_id == f1_team_id) %>%
  mutate(game_date = as.Date(wallclock)) %>%
  filter(game_date < as.Date("2025-03-20")) %>%
  distinct(game_id) %>%
  pull(game_id)

#Florida's Regular Season PBP Games
f1_regular_games = pbp_2025 %>%
  filter(
    game_id %in% f1_all_games,
    team_id == f1_team_id
  )
#Opponent's Regular Season PBP Games
f1_regular_all_plays = pbp_2025 %>%
  filter(game_id %in% f1_all_games)

#Season Summary Metrics
#Florida
f1_team_totals_reg = f1_regular_games %>%
  group_by(game_id) %>%
  summarise(
    points = sum(if_else(scoring_play, score_value, 0), na.rm = TRUE),
    fga = sum(shooting_play == TRUE, na.rm = TRUE),
    fgm = sum(shooting_play == TRUE & scoring_play == TRUE, na.rm = TRUE), 
    x3pa = sum(shooting_play == TRUE & points_attempted == 3, na.rm = TRUE), 
    x3pm = sum(shooting_play == TRUE & scoring_play == TRUE & points_attempted == 3, na.rm = TRUE), 
    fta = sum(shooting_play == TRUE & points_attempted == 1, na.rm =  TRUE), 
    to = sum(grepl("turnover", type_text, ignore.case = TRUE), na.rm = TRUE), 
    orb = sum(type_text == "Offensive Rebound", na.rm = TRUE), 
    drb = sum(type_text == "Defensive Rebound", na.rm = TRUE), 
  ) %>%
  mutate(
    trb = orb + drb, 
    poss = fga + 0.44 * fta + to - orb, 
    efg = (fgm + 0.5 * x3pm) / pmax(fga, 1))
  

#Opponents
opp_team_totals_regular = f1_regular_all_plays %>%
  filter(!is.na(team_id), team_id != f1_team_id) %>%
  group_by(game_id) %>%
  summarise(
    opp_points = sum(if_else(scoring_play, score_value, 0), na.rm = TRUE), 
    opp_fga = sum(shooting_play == TRUE, na.rm = TRUE), 
    opp_fta = sum(shooting_play == TRUE & points_attempted == 1, na.rm = TRUE), 
    opp_to = sum(grepl("turnover", type_text, ignore.case = TRUE), na.rm = TRUE), 
    opp_orb =sum(type_text == "Offensive Rebound", na.rm = TRUE), 
    opp_drb = sum(type_text == "Defensive Rebound", na.rm = TRUE), 
  ) %>%
  mutate(
    opp_trb = opp_orb + opp_drb, 
    opp_poss = opp_fga + 0.44 * opp_fta + opp_to - opp_orb
  )
    
#Florida's Regular Season Efficiencies
f1_regular_game_eff = f1_team_totals_reg %>%
  left_join(opp_team_totals_regular, by = "game_id") %>%
  mutate(
    off_eff = 100 * points / pmax(poss, 1), 
    def_eff = 100 * opp_points / pmax(opp_poss, 1), 
    tov_pct = to / pmax(poss, 1), 
    reb_pct = trb / pmax(poss, 1)
  )
view(f1_regular_game_eff)
#Comparison of Tournament vs. Regular Season 
season_avg = f1_regular_game_eff%>%
  summarise(
    period = "Season", 
    off_eff = mean(off_eff), 
    def_eff = mean(def_eff), 
    tov_pct = mean(tov_pct) * 100,
    reb_pct = mean(reb_pct) * 100,
    efg = mean(efg) * 100,
    pace = mean(poss)
  )
view(season_avg)
tourney_avg = f1_game_eff %>%
  summarise(
    period = "Tournament", 
    off_eff = mean(off_eff), 
    def_eff = mean(def_eff), 
    tov_pct = mean(tov_pct) * 100, 
    reb_pct = mean(reb_pct) * 100, 
    efg = mean(efg) * 100, 
    pace = mean(poss)
  )
view(tourney_avg)
compare_avg = rbind(season_avg, tourney_avg)
compare_avg


#Offensive Efficiency Comparison Graph
ggplot(compare_avg, aes(x = period, y = off_eff)) + 
  geom_col(fill = "#0021A5", width = 0.5) +
  labs(title = "Florida Offensive Efficiency: Season vs. Tournament", 
       x = "",
       y = "Points per 100 Possessions") + 
  theme_minimal()


#Defensive Efficiency Comparison Graph
ggplot(compare_avg, aes(x = period, y = def_eff)) +
  geom_col(fill = "#0021A5", width = 0.5) +
  labs(
    title = "Florida Defensive Efficiency: Season vs Tournament",
    x = "",
    y = "Points Allowed per 100 Possessions"
  ) +
  theme_minimal()

#EfG Comparison Graph
ggplot(compare_avg, aes(x = period, y = efg)) +
  geom_col(fill = "#0021A5", width = 0.5) +
  labs(
    title = "Florida eFG%: Season vs Tournament",
    x = "",
    y = "Effective Field Goal %"
  ) +
  theme_minimal()

#Rebound Comparison Graph
ggplot(compare_avg, aes(x = period, y = reb_pct)) +
  geom_col(fill = "#0021A5", width = 0.5) +
  labs(
    title = "Florida Rebound Percentage: Season vs Tournament",
    x = "",
    y = "Rebound %"
  ) +
  theme_minimal()

#Pace Comparison Graph
ggplot(compare_avg, aes(x = period, y = pace)) +
  geom_col(fill = "#0021A5", width = 0.5) +
  labs(
    title = "Florida Pace: Season vs Tournament",
    x = "",
    y = "Estimated Possessions per Game"
  ) +
  theme_minimal()

