#' Returns strength of schedule measure for all teams based on opponent performance against other teams in regular season
#'
#' @param df a long data frame with time average point and possession differential for each game in a season
#'
#' @return a data frame with a average strength of schedule measures for all teams
#' @export
nfl_mvt_sos <- function(df){
  nfl_teams <- df %>%
    distinct(team) %>%
    arrange(team) %>%
    pull(team)

  results_df <- data.frame(
    team = nfl_teams,
    sos = NA_real_
  )

  for(i in 1:length(nfl_teams)){
    results_df <- rows_update(
      results_df,
      df %>% mvt_sos(nfl_teams[i]),
      by = "team"
    )
  }

  return(results_df)
}
