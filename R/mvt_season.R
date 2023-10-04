#' For a given team and season, returns regulation time average lead for each game played in the season.
#'
#' @param team_pbp_df a data frame with play-by-play data for season of interest
#' @param team a team of interest
#'
#' @return a data frame with time average lead results for each game played by team in a season
#' @export
mvt_season <- function(team_pbp_df, team)
{
#  df <- load_pbp( year ) %>%
#    filter( home_team == team | away_team == team) %>%
#    filter( season_type == "REG" )  %>%
#    select(game_seconds_remaining, posteam, score_differential_post, week,
#           game_half, sp, home_team, away_team)

  week_vec <- team_pbp_df %>%
    distinct(week) %>%
    pull(week)
  max_week <- max(week_vec)

  # Create data frame to be updated in for loop below
  season_df <- data.frame(
    week = 1:max_week,
    team = team,
    opponent = rep(NA_character_, max_week),
    home_away = rep(NA_character_, max_week),
    time_avg_lead = rep(NA_real_, max_week),
    team_result = rep(NA_real_, max_week),
    team_spread = rep(NA_real_, max_week),
    total_line = rep(NA_real_, max_week),
    team_score = rep(NA_real_, max_week),
    opponent_score = rep(NA_real_, max_week)
  )

  for(i in 1:max_week){
    # Leave NAs for bye week; o/w, update with time_avg_lead results
    if(i %in% week_vec){
      season_df <- rows_update(season_df,
                                team_pbp_df %>%
                                 filter(week == i) %>%
                                 mvt_game(team),
                               by = c("week", "team"))
    }
  }

  return(season_df)
}
