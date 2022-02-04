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
                          wins = NA,
                          losses = NA,
                          ties = NA,
                          season_point_for = NA,
                          season_point_against = NA,
                          season_point_diff = NA,
                          pythag_wins = NA)

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
