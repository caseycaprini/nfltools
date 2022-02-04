#' For a given team, returns strength of schedule measure based on opponent performance against other teams in regular season
#'
#' @param df a long data frame with
#' @param team_interest a team of interest
#'
#' @return a data frame with a average strength of schedule measure for each opponent
#' @export
mvt_sos <- function(df, team_interest){
  # Make vector of teams which team of interest played against
  team_opponents <- df %>%
    filter(team == team_interest) %>%
    pull(opponent)

  df %>%
    filter(team %in% team_opponents) %>%
    filter(opponent != team_interest) %>%
    group_by(team) %>%
    summarize(opp_quality = mean(time_avg_lead,
                                 na.rm = TRUE)) %>%
    summarize(sos = mean(opp_quality)) %>%
    mutate(team = team_interest) %>%
    return()
}
