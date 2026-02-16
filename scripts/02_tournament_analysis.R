#Data of Florida Tournament Games

f1_team_id = pbp %>%
  filter(home_team_name == "Florida" | away_team_name == "Florida") %>%
  filter(team_id  %in% c(home_team_id, away_team_id)) %>%
  count(team_id, sort =  TRUE) %>%
  slice(1) %>%
  pull(team_id)

#Florida Metrics
f1_team_totals = pbp %>%
  filter(team_id == f1_team_id) %>%
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


view(f1_team_totals)
f1_team_totals['poss']


#Opponent Metrics
opp_team_totals = pbp %>%
  filter(team_id !=f1_team_id, !is.na(team_id)) %>%
  group_by(game_id)%>%
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

#Florida's 6-Game Efficencies
f1_game_eff = f1_team_totals %>%
  left_join(opp_team_totals, by = "game_id") %>%
  mutate(
    off_eff = 100 * points / pmax(poss, 1), 
    def_eff = 100 * opp_points / pmax(opp_poss, 1),
    tov_pct = to / pmax(poss, 1), 
    reb_pct = trb / pmax(trb + opp_trb, 1), 
    game_num = row_number()
  ) %>%
  arrange(game_num)
view(f1_game_eff)


#Plots of 6-Game Tournament Run
plot_df = f1_game_eff %>%
  transmute(
    game_num,
    `Off Eff` = off_eff,
    `Def Eff` = def_eff,
    `TOV%`    = 100 * tov_pct,
    `Reb%`    = 100 * reb_pct,
    `eFG%`    = 100 * efg
  ) %>%
  pivot_longer(-game_num, names_to = "metric", values_to = "value")

ggplot(plot_df, aes(x = game_num, y = value)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~metric, scales = "free_y", ncol = 3) +
  labs(
    title = "Florida Tournament Progression (Game-by-Game)",
    x = "Tournament Game (in order)",
    y = "Value"
  ) +
  theme_minimal(base_size = 12)


