#' Returns long data frame with time average results for each team
#'
#' @param year a year of interest
#'
#' @return a long data frame with time average results for each game for every team
#' @export
nfl_mvt_season <- function(year = 2021){
  pbp_df <- load_pbp(year) %>%
    filter(season_type == "REG") %>%
    select(game_seconds_remaining, posteam,
           score_differential_post, week,
           game_half, sp,
           home_team, away_team,
           result)

  team_vec <- pbp_df %>%
    distinct(home_team) %>%
    arrange(home_team) %>%
    pull(home_team)

  max_week <- max(pbp_df$week)

  results_df <- data.frame(
      week = 1:max_week,
      team = NA_character_,
      opponent = rep(NA_character_,
                     max_week*length(team_vec)),
      home_away = rep(NA_character_,
                      max_week*length(team_vec)),
      team_result = rep(NA_real_,
                        max_week*length(team_vec)),
      time_avg_lead = rep(NA_real_,
                          max_week*length(team_vec)),
      time_avg_sd = rep(NA_real_,
                        max_week*length(team_vec))
    )

  for(j in 1:length(team_vec)){
    team = team_vec[j]
    for(k in 1:max_week){
      results_df$team[(j-1)*max_week + k] = team
    }
  }

  for(i in 1:length(team_vec)){
    team = team_vec[i]
    results_df <- rows_update(results_df,
                              pbp_df %>%
                                filter(home_team == team | away_team == team) %>%
                                mvt_season(team = team),
                              by = c("week", "team"))
  }

  results_df <- results_df %>%
    mutate(season = year) %>%
    select(season, everything())

  return(results_df)
}
