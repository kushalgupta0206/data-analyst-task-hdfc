
#load the required libraries
library(dplyr)
library(tidyr)
library(stringr)

#load all the tables
player_season_wy <- read.csv("../data/wyscout_2024_mls.csv")
player_match_runs_sc <- read.csv("../data/skillcorner_gi_2024_mls.csv") 
player_match_physical_sc <- read.csv("../data/skillcorner_physical_2024_mls.csv", sep = ";")
player_id_map <- read.csv("../data/player_id_map.csv")

#define a center forward and calculate the number of matches and minutes played in the season
player_season_data_sc <- player_match_physical_sc %>% 
  mutate(
    Position.Group.Refined = if_else(str_detect(Position, "LF|RF"), "Center Forward", Position.Group)
  ) %>% 
  group_by(Player, Player.ID, Position.Group.Refined) %>% 
  summarise(
    n_matches = n(),
    n_minutes = sum(Minutes),
    .groups = "drop"
  ) %>% 
  filter(Position.Group.Refined == "Center Forward" & n_minutes > 800) %>% 
  pull(Player.ID)

#player-season df of most relevant physical metrics and a season minute threshold of 800
player_season_physical_df <- player_match_physical_sc %>%
  mutate(
    Position.Group.Refined = if_else(str_detect(Position, "LF|RF"), "Center Forward", Position.Group)
  ) %>%
  group_by(Player, Player.ID, Position.Group.Refined) %>%
  summarise(
    n_minutes = sum(Minutes, na.rm = TRUE),
    Running.Distance.P90 = weighted.mean(Running.Distance.P90, Minutes, na.rm = TRUE),
    HI.Count.P90 = weighted.mean(HI.Count.P90, Minutes, na.rm = TRUE),
    COD.Count.P90 = weighted.mean(Change.of.Direction.Count.P90, Minutes, na.rm = TRUE),
    PSV.99.Top5 = mean(head(sort(PSV.99, decreasing = TRUE), 5), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Position.Group.Refined == "Center Forward", n_minutes > 800)

#player-season df of most relevant off-ball runs metrics and a season minute threshold of 800
player_season_runs_df <- player_match_runs_sc %>% 
  mutate(
    position_group_refined = if_else(str_detect(position, "LF|RF"), "Center Forward", position_group),
    offballrun_count_dangerous_P30TIP = (offballrun_count_dangerous/minutes_tip) * 30,
    offballrun_count_total_P30TIP = (offballrun_count_total/minutes_tip) * 30,
    offballrun_count_received_P30TIP = (offballrun_count_received/minutes_tip) * 30
  ) %>%
  group_by(player_name, player_id, position_group_refined) %>%
  summarise(
    n_minutes = sum(minutes, na.rm = TRUE),
    offballrun_count_dangerous_P30TIP = weighted.mean(offballrun_count_dangerous_P30TIP, minutes_tip, na.rm = TRUE),
    offballrun_count_total_P30TIP = weighted.mean(offballrun_count_total_P30TIP, minutes_tip, na.rm = TRUE),
    offballrun_count_received_P30TIP = weighted.mean(offballrun_count_received_P30TIP, minutes_tip, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  filter(position_group_refined == "Center Forward" & n_minutes > 800)

#player-team df
player_team_df <- player_match_runs_sc %>% 
  distinct(player_id, team_id, team_name)

#combining player-season physical, off-ball runs and wyscout datasets and renaming for uniformity 
player_season_complete <- player_season_runs_df %>% 
  select(-c(player_name, n_minutes)) %>% 
  left_join(player_season_physical_df, by = c("player_id" = "Player.ID")) %>% 
  left_join(player_id_map, by = c("player_id" = "sk_player_id")) %>% 
  left_join(player_season_wy %>%
              filter(src_grouped_position == "#9") %>% 
              select(wy_player_id, src_grouped_position, minutes_played, goals, assists, xg_shot, shots, xg_assist, shot_assists, head_shots, received_pass),
            by = c("wy_player_id")) %>% 
  filter(minutes_played > 800) %>% 
  left_join(player_team_df, by = c("player_id")) %>% 
  select(-c(position_group_refined, Position.Group.Refined, src_grouped_position)) %>% 
  rename(sk_player_id = player_id,
         sk_player_name = Player,
         sk_minutes = n_minutes,
         wy_minutes = minutes_played,
         running_distance_P90 = Running.Distance.P90,
         hi_count_P90 = HI.Count.P90,
         cod_count_P90 = COD.Count.P90,
         psv_99_Top5 = PSV.99.Top5, 
         received_pass_P90 = received_pass, 
         goals_P90  = goals,
         assists_P90 = assists,
         xg_shot_P90 = xg_shot, 
         shots_P90 = shots,
         xg_assist_P90 = xg_assist,
         shot_assists_P90 = shot_assists,
         head_shots_P90 = head_shots,
         sk_team_id = team_id, 
         sk_team_name = team_name) %>% 
  select(
    sk_player_id, sk_player_name, sk_team_id, sk_team_name, sk_minutes, wy_player_id, wy_minutes,
    goals_P90, assists_P90, xg_shot_P90, shots_P90, xg_assist_P90, shot_assists_P90,
    running_distance_P90, hi_count_P90, cod_count_P90, psv_99_Top5, head_shots_P90,
    offballrun_count_dangerous_P30TIP, offballrun_count_total_P30TIP, offballrun_count_received_P30TIP,received_pass_P90)

#categorizing metrics
production_cols  <- c("xg_shot_P90", "shots_P90", "xg_assist_P90", "shot_assists_P90", "goals_P90", "assists_P90")
athleticism_cols <- c("running_distance_P90", "hi_count_P90", "cod_count_P90", "psv_99_Top5")
movement_cols    <- c("offballrun_count_dangerous_P30TIP", "offballrun_count_total_P30TIP", "offballrun_count_received_P30TIP", "received_pass_P90")
physicality_cols <- c("head_shots_P90")

#setting the weights of each metric within its respective category and calculating an example composite score 
final_scouting_df <- player_season_complete %>%
  mutate(
    across(all_of(c(production_cols, athleticism_cols, movement_cols, physicality_cols)), 
           ~ as.numeric(scale(.)), 
           .names = "z_{.col}"),
    production_z_score = 
      (z_xg_shot_P90 * 0.30) + 
      (z_shots_P90 * 0.20) + 
      (z_xg_assist_P90 * 0.20) + 
      (z_shot_assists_P90 * 0.15) + 
      (z_goals_P90 * 0.075) + 
      (z_assists_P90 * 0.075),
    
    athleticism_z_score = 
      (z_psv_99_Top5 * 0.40) + 
      (z_hi_count_P90 * 0.25) + 
      (z_running_distance_P90 * 0.20) + 
      (z_cod_count_P90 * 0.15),
    
    movement_z_score = 
      (z_offballrun_count_dangerous_P30TIP * 0.50) + 
      (z_offballrun_count_total_P30TIP * 0.20) + 
      (z_offballrun_count_received_P30TIP * 0.20) +
      (z_received_pass_P90 * 0.10),
    
    physicality_z_score = (z_head_shots_P90 * 1.00),
    
    composite_score = (production_z_score * 0.65) + 
      (athleticism_z_score * 0.15) + 
      (movement_z_score * 0.15) + 
      (physicality_z_score * 0.05)
  ) %>%
  arrange(desc(composite_score)) %>% 
  select(-starts_with("z")) %>% 
  select(sk_player_name, sk_team_name, sk_minutes, ends_with("_score"))

#creating a grid of all possible sets of weights for each category, given certain conditions
weight_grid <- expand.grid(
  w_prod = seq(0.55, 0.75, by = 0.025),
  w_ath  = seq(0.10, 0.20, by = 0.025),
  w_mov  = seq(0.10, 0.20, by = 0.025),
  w_phys = seq(0.025, 0.075, by = 0.025)
) %>%
  filter(w_prod + w_ath + w_mov + w_phys == 1.0)

#applying the weight grid search and calculating average score, sd, and best and worst ranks for each player
simulation_results <- weight_grid %>%
  rowwise() %>%
  do({
    w <- .
    final_scouting_df %>%
      mutate(
        temp_composite = (production_z_score * w$w_prod) + 
          (athleticism_z_score * w$w_ath) + 
          (movement_z_score * w$w_mov) + 
          (physicality_z_score * w$w_phys)
      ) %>%
      mutate(rank = rank(-temp_composite)) %>%
      select(sk_player_name, sk_team_name, sk_minutes, production_z_score, athleticism_z_score, movement_z_score, physicality_z_score, rank, temp_composite)
  }) %>%
  group_by(sk_player_name, sk_team_name, sk_minutes, production_z_score, athleticism_z_score, movement_z_score, physicality_z_score) %>%
  summarise(
    avg_score = mean(temp_composite),
    variability = sd(temp_composite),
    best_rank = min(rank),
    worst_rank = max(rank)
  ) %>%
  arrange(-avg_score)

#saving to a .csv for conditional formatting purposes on google sheets
# write.csv(simulation_results, "simulation_results_2.csv", row.names = FALSE)

#getting the top five prospects and changing the shape of the df
top_5_prospects <- player_season_complete %>% 
  filter(str_detect(sk_player_name, "Juan Camilo Hernández Suárez|Kelvin Yeboah|Dejan|Willy|Tai")) %>% 
  select(sk_player_name, sk_team_name, sk_minutes, goals_P90, assists_P90, xg_shot_P90, shots_P90, xg_assist_P90, shot_assists_P90, psv_99_Top5, running_distance_P90, hi_count_P90, offballrun_count_dangerous_P30TIP, head_shots_P90) %>% 
  pivot_longer( 
    cols = -c(sk_player_name, sk_team_name,sk_minutes), 
    names_to = "metric", 
    values_to = "value"
  ) %>% 
  select(-c(sk_team_name, sk_minutes)) %>% 
  pivot_wider(
    names_from = sk_player_name, 
    values_from = value
  ) %>% 
  select(metric, "Juan Camilo Hernández Suárez", "Kelvin Yeboah", "Dejan Joveljić", "Willy Agada", "Tai Baribo")

#saving to a .csv for conditional formatting purposes on google sheets
# write.csv(top_5_prospects, "top_5_prospects.csv", row.names = FALSE)

#getting the minimum, avg and maximum for each chosen metric for conditional formatting purposes on google sheets
metric_range_df <- player_season_complete %>% 
  select(
    goals_P90, assists_P90, xg_shot_P90, shots_P90, xg_assist_P90, shot_assists_P90, 
    psv_99_Top5, running_distance_P90, hi_count_P90, 
    offballrun_count_dangerous_P30TIP,
    head_shots_P90
  ) %>% 
  # 2. Calculate stats for every column
  summarise(across(everything(), list(
    minimum = ~min(., na.rm = TRUE),
    avg     = ~mean(., na.rm = TRUE),
    maximum = ~max(., na.rm = TRUE)
  ))) %>% 
  pivot_longer(
    cols = everything(),
    names_to = c("metric", ".value"),
    names_pattern = "(.*)_(minimum|avg|maximum)"
  )
