#' Returns aggregate results for regular season (e.g., wins, losses, total points, pythagorean win expectation)
#'
#' @param year a year of interest
#'
#' @return a data frame with aggregate regular season results
#' @export
nfl_reg_season_summary <- function(year = 2020){
  df <- fast_scraper_schedules(year) %>%
    filter(game_type == "REG")

  nfl_teams <- df %>%
    distinct(away_team) %>%
    arrange(away_team) %>%
    pull(away_team)

  results_df = data.frame(team = nfl_teams,
                          wins = NA_integer_,
                          losses = NA_integer_,
                          ties = NA_integer_,
                          season_point_for = NA_integer_,
                          season_point_against = NA_integer_,
                          season_point_diff = NA_integer_,
                          pythag_wins = NA_real_)

  for(i in 1:length(nfl_teams)){
    results_df <- rows_update(
      results_df,
      df %>% team_reg_season_summary(nfl_teams[i]),
      by = "team"
    )
  }

  results_df <- results_df %>%
    mutate(season = year) %>%
    select(season, everything())

  return(results_df)
}
