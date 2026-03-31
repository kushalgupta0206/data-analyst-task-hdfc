#load all libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(lme4)
library(tidyverse)

#load all the tables
player_season_wy <- read.csv("../data/wyscout_2024_mls.csv")
player_match_runs_sc <- read.csv("../data/skillcorner_gi_2024_mls.csv") 
player_match_physical_sc <- read.csv("../data/skillcorner_physical_2024_mls.csv", sep = ";")
player_id_map <- read.csv("../data/player_id_map.csv")

#a dataset of number of matches played and total minutes for players playing in the "center forward" position
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
  filter(Position.Group.Refined == "Center Forward") %>% 
  arrange(-n_minutes)

#filter player-match physical data by cf
player_match_physical_sc_cf <- player_match_physical_sc %>% 
  mutate(
    Position.Group.Refined = if_else(str_detect(Position, "LF|RF"), "Center Forward", Position.Group)
  ) %>% 
  filter(Position.Group.Refined == "Center Forward")

#vector of count physical metrics
physical_metrics_count <- c(
  "HSR.Count", "Sprint.Count", "HI.Count", 
  "Medium.Acceleration.Count", "High.Acceleration.Count", 
  "Medium.Deceleration.Count", "High.Deceleration.Count", 
  "Explosive.Acceleration.to.HSR.Count", 
  "Explosive.Acceleration.to.Sprint.Count", 
  "Change.of.Direction.Count"
)
#vector of physical metrics other than count metrics
physical_metrics_rest <- c( "Distance",
                            "Running.Distance", "HSR.Distance", "Sprint.Distance", "HI.Distance", 
                            "PSV.99", 
                            "Distance.P90", "Running.Distance.P90", "HSR.Distance.P90", 
                            "HSR.Count.P90", "Sprint.Distance.P90", "Sprint.Count.P90", "HI.Distance.P90", 
                            "HI.Count.P90", "Medium.Acceleration.Count.P90", "High.Acceleration.Count.P90", 
                            "Medium.Deceleration.Count.P90", "High.Deceleration.Count.P90", 
                            "Explosive.Acceleration.to.HSR.Count.P90", 
                            "Explosive.Acceleration.to.Sprint.Count.P90", "Change.of.Direction.Count.P90"
)
#vector of per-90 physical metrics
physical_metrics_P90 <- c(
  "Distance.P90", "Running.Distance.P90", "HSR.Distance.P90", 
  "HSR.Count.P90", "Sprint.Distance.P90", "Sprint.Count.P90", "HI.Distance.P90", 
  "HI.Count.P90", "Medium.Acceleration.Count.P90", "High.Acceleration.Count.P90", 
  "Medium.Deceleration.Count.P90", "High.Deceleration.Count.P90", 
  "Explosive.Acceleration.to.HSR.Count.P90", 
  "Explosive.Acceleration.to.Sprint.Count.P90", "Change.of.Direction.Count.P90"
)

#a helper function to calculate the signal to noise ratio using linear-mixed effects regression
#there is an assumption that metric values are continuous and follow a Gaussian distribution
get_snr_gaussian <- function(metric_name, data, player_id_col) {
  form <- as.formula(paste(metric_name, "~ 1 + (1 | " , player_id_col, ")"))
  
  model <- lmer(form, data = data, REML = TRUE)
  vc <- as.data.frame(VarCorr(model))
  
  var_player <- vc$vcov[vc$grp == player_id_col]
  var_resid  <- vc$vcov[vc$grp == "Residual"]
  
  if(length(var_player) == 0) var_player <- 0
  
  tibble(
    metric = metric_name,
    model_type = "Gaussian",
    player_variance = var_player,
    residual_variance = var_resid,
    snr = var_player / var_resid
  )
}

#a helper function to calculate the signal to noise ratio using linear-mixed effects regression
#there is an assumption that metric values are continuous and follow a Poisson distribution weighted by minutes played
get_snr_poisson <- function(metric_name, data, player_id_col, minute_id_col) {
  form <- as.formula(paste(metric_name, "~ 1 + (1 | ", player_id_col, ") + offset(log(", minute_id_col, "))"))

  model <- glmer(form, data = data, family = poisson)
  vc <- as.data.frame(VarCorr(model))
  
  var_player <- vc$vcov[vc$grp == player_id_col]
  #the residual variance is often fixed to 1 in a standard Poisson model
  var_resid <- 1 
  
  tibble(
    metric = metric_name,
    model_type = "Poisson",
    player_variance = var_player,
    residual_variance = var_resid,
    snr = var_player / var_resid
  )
}

#running the gaussian helper function on continuous metrics like distance and per-90
results_snr_gaussian <- map_dfr(physical_metrics_rest, 
                                get_snr_gaussian, 
                                data = player_match_physical_sc_cf,
                                player_id_col = "Player.ID")

#running the poisson helper function on count metrics
results_snr_poisson <- map_dfr(physical_metrics_count, 
                               get_snr_poisson, 
                               data = player_match_physical_sc_cf, 
                               player_id_col = "Player.ID", 
                               minute_id_col = "Minutes")

#combine the dfs from the two helper functions
snr_results_physical <- bind_rows(results_snr_gaussian, results_snr_poisson) %>% 
  arrange(desc(snr))

#physical metrics to consider for analysis
physical_metrics_to_consider <- c("Running.Distance.P90","HI.Count.P90", "PSV.99")

#helper function to run the snr sensitivity analysis for a specific season minute threshold
run_snr_sensitivity_physical <- function(s_min) {
  ids <- player_season_data_sc %>% 
    filter(n_minutes > s_min) %>% 
    pull(Player.ID)
  
  df_filtered <- player_match_physical_sc %>%
    mutate(Position.Group.Refined = if_else(str_detect(Position, "LF|RF"), "Center Forward", Position.Group)) %>%
    filter(Position.Group.Refined == "Center Forward",
           Player.ID %in% ids)
  
  results <- map_dfr(physical_metrics_to_consider, 
                     ~get_snr_gaussian(.x, data = df_filtered, player_id_col = "Player.ID"))
  
  results %>%
    mutate(season_threshold = s_min, n_players = length(ids)) %>%
    select(metric, season_threshold, n_players, player_variance, residual_variance, snr)
}

#grid of thresholds
threshold_grid <- expand.grid(
  season_min = c(200, 300, 400, 500, 600, 700, 800, 900, 1000)
)

#calling the snr sensitivity helper function for physical metrics
sensitivity_results_physical <- pmap_dfr(list(threshold_grid$season_min), 
                                         ~run_snr_sensitivity_physical(..1))

#helper function to calculate reliability for each metric given player and residual variance
calculate_curve_practical <- function(metric_name, sig_mu, sig_eps, minute_threshold, n_players) {
  data.frame(
    minutes = minute_threshold,
    metric = metric_name,
    reliability = sig_mu / (sig_mu + (sig_eps / (minute_threshold / 90))),
    n_players = n_players
  )
}

#calling the above helper function for each physical metric and season minute threshold
reliability_plotting_df_physical <- sensitivity_results_physical %>%
  filter(metric %in% physical_metrics_to_consider) %>%
  select(metric, player_variance, residual_variance, season_threshold, n_players) %>% 
  pmap_dfr(~calculate_curve_practical(..1, ..2, ..3, ..4, ..5))

#vector of off-ball run count metrics
runs_metrics_count <- c(
  "offballrun_count_total", "offballrun_count", 
  "offballrun_count_shotwithin10s", "offballrun_count_goalwithin10s",
  "offballrun_count_targeted", "offballrun_count_received", 
  "offballrun_count_abovehsr", "offballrun_count_penaltyarea", 
  "offballrun_count_dangerous", "offballrun_count_dangerous_targeted", 
  "offballrun_count_dangerous_received"
)
#vector of runs per-30 team in possession count metrics
runs_metrics_P30TIP <- paste0(runs_metrics_count, "_P30TIP")
#all run metrics that are not count metrics
runs_metrics_rest <- c("offballrun_avgdistance", runs_metrics_P30TIP)

#player-match runs dataset, with per 30 tip, filtered by cf
player_match_runs_sc_cf <- player_match_runs_sc %>% 
  mutate(
    across(all_of(runs_metrics_count), 
           ~ (.x / minutes_tip) * 30, 
           .names = "{.col}_P30TIP"),
    
    Position.Group.Refined = if_else(str_detect(position, "LF|RF"), "Center Forward", position_group)
  ) %>% 
  filter(
    Position.Group.Refined == "Center Forward"
  )

#call the gaussian helper function to calculate snr for per 30 tip runs metrics
results_snr_gaussian_runs <- map_dfr(runs_metrics_rest, 
                                     get_snr_gaussian, 
                                     data = player_match_runs_sc_cf, 
                                     player_id_col = "player_id")

#call the poisson helper function to calculate snr for count runs metrics
results_snr_poisson_runs <- map_dfr(runs_metrics_count, 
                                    get_snr_poisson, 
                                    data = player_match_runs_sc_cf, 
                                    player_id_col = "player_id",
                                    minute_id_col = "minutes_tip")

#combine the dfs from the two helper functions
snr_results_runs <- bind_rows(results_snr_gaussian_runs, results_snr_poisson_runs) %>% 
  arrange(desc(snr))

#run metrics to consider for minute threshold analysis
run_metrics_to_consider <- c(
  "offballrun_count_dangerous_P30TIP", "offballrun_count_total_P30TIP","offballrun_count_received_P30TIP")

#set up player-match runs dataset for helper function below 
player_match_runs_sc_setup <- player_match_runs_sc %>% 
  mutate(
    across(all_of(runs_metrics_count), 
           ~ (.x / minutes_tip) * 30, 
           .names = "{.col}_P30TIP"),
    Position.Group.Refined = if_else(str_detect(position, "LF|RF"), "Center Forward", position_group)
  )

#helper function to run the snr sensitivity analysis for a specific season minute threshold
run_snr_sensitivity_runs <- function(s_min) {
  ids <- player_season_data_sc %>% 
    filter(n_minutes > s_min) %>% 
    pull(Player.ID)
  
  df_filtered <- player_match_runs_sc_setup %>%
    filter(Position.Group.Refined == "Center Forward",
           player_id %in% ids)
  
  results <- map_dfr(run_metrics_to_consider, 
                     ~get_snr_gaussian(.x, data = df_filtered, player_id_col = "player_id"))
  
  results %>%
    mutate(season_threshold = s_min, n_players = length(ids)) %>%
    select(metric, season_threshold, player_variance, residual_variance, n_players, snr)
}

#calling the snr sensitivity helper function for runs metrics
sensitivity_results_runs <- pmap_dfr(list(threshold_grid$season_min), 
                                     ~run_snr_sensitivity_runs(..1))
#calling the above helper function for each off ball runs metric and season minute threshold
reliability_plotting_df_runs <- sensitivity_results_runs %>%
  select(metric, player_variance, residual_variance, season_threshold, n_players) %>% 
  pmap_dfr(~calculate_curve_practical(..1, ..2, ..3, ..4, ..5))

#combining the reliability dfs for the chosen physical and off-ball runs metrics  
reliability_plotting_df_complete <- reliability_plotting_df_physical %>% 
  bind_rows(reliability_plotting_df_runs) %>% 
  mutate(
    metric_clean = case_when(
      metric == "Running.Distance.P90" ~ "Running Distance",
      metric == "HI.Count.P90" ~ "High-Intensity Run Count",
      metric == "offballrun_count_dangerous_P30TIP" ~ "Dangerous Off-Ball Runs",
      metric == "offballrun_count_total_P30TIP" ~ "Total Off-Ball Runs",
      metric == "offballrun_count_received_P30TIP" ~ "Off-Ball Runs Received",
      TRUE ~ metric
    )
  ) %>% 
  mutate(metric_clean = factor(metric_clean, levels = c(
    "Running Distance",  
    "High-Intensity Run Count",
    "PSV.99",
    "Dangerous Off-Ball Runs", 
    "Total Off-Ball Runs",
    "Off-Ball Runs Received"
  )))

#getting the number of unique players for each season-minute threshold
label_data_runs <- reliability_plotting_df_complete %>%
  distinct(minutes, n_players)

#plotting metric reliability over different season-minute thresholds
ggplot(reliability_plotting_df_complete, 
       aes(x = minutes, y = reliability, color = metric_clean, group = metric_clean)) +
  geom_vline(xintercept = 800, linetype = "dashed", color = "indianred", size = 1) +
  geom_line(size = 1.3) +
  geom_point(size = 3) +
  geom_text(data = label_data_runs, 
            aes(x = minutes, y = 0.52, label = paste0("n=", n_players)),
            inherit.aes = FALSE, 
            vjust = 0, 
            size = 3.5, 
            color = "gray30", 
            fontface = "italic") +
  scale_color_brewer(palette = "Set2", name = "Metric") +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, 0.1)) +
  scale_x_continuous(limits = c(200, 1000), breaks = seq(200, 1000, 100)) +
  labs(
    title = "Metric Reliability Over Minutes Thresholds",
    subtitle = "Number of players remaining at each threshold indicated by n",
    x = "Minimum Season Minutes Threshold",
    y = "Reliability"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 15))
  ) +
  guides(
    linetype = "none", 
    color = guide_legend(nrow = 2, byrow = TRUE)
  )

#combining the snr results for physical and off-ball runs metrics 
complete_snr_plot_df <- snr_results_physical %>% 
  slice(1:10) %>% 
  mutate(
    metric_type = "Physical Metric",
    metric = reorder(metric, snr)
  ) %>% 
  bind_rows(
    snr_results_runs %>% 
      slice(1:6) %>% 
      mutate(
        metric_type = "Off-Ball Runs Metric",
        metric = reorder(metric, snr)
      )
  ) %>% 
  filter(metric != "offballrun_count_P30TIP") %>% 
  mutate(metric = reorder(metric, snr))

#plotting the snr for physical and off-ball runs metrics on the same plot
ggplot(complete_snr_plot_df, aes(x = snr, y = metric, fill = metric_type)) +
  geom_col() +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "red", size = 0.8) +
  scale_fill_manual(values = c("Physical Metric" = "steelblue", 
                               "Off-Ball Runs Metric" = "#E69F00")) +
  scale_x_continuous(breaks = seq(0, max(complete_snr_plot_df$snr) + 0.2, by = 0.2)) +
  labs(
    title = "Signal to Noise Ratio (SNR): Physical vs. Off-Ball Runs",
    x = "Signal-to-Noise Ratio (SNR)",
    y = NULL,
    fill = "Metric Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9)
  )

#vector of player ids of center forwards with more than 800 minutes 
#(~1/4 of the season, season-minute threshold identified) 
center_forward_players_sc <- player_season_data_sc %>% 
  filter(n_minutes > 800) %>% 
  pull(Player.ID)

#filter player-match physical data by cf and season-minute threshold
player_match_physical_sc_cf_filtered <- player_match_physical_sc %>% 
  mutate(
    Position.Group.Refined = if_else(str_detect(Position, "LF|RF"), "Center Forward", Position.Group)
  ) %>% 
  filter(Position.Group.Refined == "Center Forward" & Player.ID %in% center_forward_players_sc)

#split the filtered player-match physical dataset by season half and calculate per-90 metrics on a player-half-season level
player_match_physical_sc_cf_split <- player_match_physical_sc_cf_filtered %>%
  group_by(Player.ID) %>%
  arrange(Date) %>%
  mutate(
    match_num = row_number(),
    total_matches = n(),
    season_half = if_else(match_num <= (total_matches / 2), "H1", "H2")
  ) %>%
  group_by(Player.ID, season_half) %>% 
  summarise(
    across(all_of(physical_metrics_P90), 
           ~ weighted.mean(.x, w = Minutes, na.rm = TRUE)),
    PSV.99.AVG = mean(PSV.99, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = season_half, 
    values_from = c(all_of(physical_metrics_P90), "PSV.99.AVG"),
    names_glue = "{.value}_{season_half}"
  )

#calculate the Pearson correlation for each per-90 physical metric across the two halfs
stability_results_physical <- map_dfr(c(physical_metrics_P90, "PSV.99.AVG"), function(m) {
  h1_col <- paste0(m, "_H1")
  h2_col <- paste0(m, "_H2")
  r_value <- cor(player_match_physical_sc_cf_split[[h1_col]], 
                 player_match_physical_sc_cf_split[[h2_col]], 
                 use = "complete.obs")
  tibble(
    metric = m,
    pearson_r = r_value
  )
}) %>% 
  arrange(desc(pearson_r))

#player-match runs dataset, with per 30 tip, filtered by cf and season-minute threshold
player_match_runs_sc_cf_filtered <- player_match_runs_sc %>% 
  mutate(
    across(all_of(runs_metrics_count), 
           ~ (.x / minutes_tip) * 30, 
           .names = "{.col}_P30TIP"),
    
    Position.Group.Refined = if_else(str_detect(position, "LF|RF"), "Center Forward", position_group)
  ) %>% 
  filter(
    Position.Group.Refined == "Center Forward",
    player_id %in% center_forward_players_sc
  )

#split the filtered player-match runs dataset by season half and calculate per-30 tip metrics on a player-half-season level
player_match_runs_sc_cf_split <- player_match_runs_sc_cf_filtered %>%
  group_by(player_id) %>%
  arrange(match_date) %>%
  mutate(
    match_num = row_number(),
    total_matches = n(),
    season_half = if_else(match_num <= (total_matches / 2), "H1", "H2")
  ) %>%
  group_by(player_id, season_half) %>% 
  summarise(
    across(all_of(runs_metrics_P30TIP), 
           ~ weighted.mean(.x, w = minutes_tip, na.rm = TRUE)),
    offballrun_avgdistance_avg = mean(offballrun_avgdistance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = season_half, 
    values_from = c(all_of(runs_metrics_P30TIP), "offballrun_avgdistance_avg"),
    names_glue = "{.value}_{season_half}"
  )

#calculate the Pearson correlation for each per-30 tip runs metric across the two halfs
stability_results_runs <- map_dfr(c(all_of(runs_metrics_P30TIP), "offballrun_avgdistance_avg"), function(m) {
  h1_col <- paste0(m, "_H1")
  h2_col <- paste0(m, "_H2")
  r_value <- cor(player_match_runs_sc_cf_split[[h1_col]], 
                 player_match_runs_sc_cf_split[[h2_col]], 
                 use = "complete.obs")
  tibble(
    metric = m,
    pearson_r = r_value
  )
}) %>% 
  arrange(desc(pearson_r))

#combine the stability dfs of physical and off-ball runs metrics
complete_stability_plot_df <- stability_results_physical %>% 
  slice(1:10) %>% 
  mutate(
    metric_type = "Physical Metric",
    metric = reorder(metric, pearson_r)) %>% 
  bind_rows(
    stability_results_runs %>% 
      slice(1:6) %>% 
      mutate(
        metric_type = "Off-Ball Runs Metric",
        metric = reorder(metric, pearson_r)
      )
  ) %>% 
  filter(metric != "offballrun_count_P30TIP") %>%
  mutate(metric = reorder(metric, pearson_r))

#plot the stability of physical and off-ball runs metrics on the same plot
ggplot(complete_stability_plot_df, aes(x = pearson_r, y = metric, fill = metric_type)) +
  geom_col() +
  scale_fill_manual(values = c("Physical Metric" = "steelblue", 
                               "Off-Ball Runs Metric" = "#E69F00")) +
  scale_x_continuous(breaks = seq(0.7, 0.95, by = 0.1)) +
  coord_cartesian(xlim = c(0.7, 0.95)) +
  labs(
    title = "Stability: Physical vs. Off-Ball Runs",
    subtitle = "Correlation between the two halves of the season",
    x = "Pearson's r",
    y = NULL,
    fill = "Metric Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10)
  )
