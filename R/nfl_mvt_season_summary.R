#' Returns an aggregate summary of time average results with adjustments based on time average opponent quality metrics
#'
#' @param df a long data frame with time average results for each game for every team
#'
#' @return a data frame with aggregate time average results with adjustments based on time average opponent quality metrics
#' @export
nfl_mvt_season_summary <- function(df){
  df %>%
    group_by(team) %>%
    summarize(time_avg_lead = mean(time_avg_lead,
                                   na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(df %>% nfl_mvt_sos(),
              by = c("team" = "team")) %>%
    mutate(adj_time_avg_lead = time_avg_lead + sos) %>%
    arrange(team) %>%
    return()
}
