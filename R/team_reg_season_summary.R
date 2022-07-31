#' For a given team and data frame of regular season results data, returns data frame with season summary results and pythagorean win expectation
#'
#' @param df a data frame with regular season game results
#' @param team a team of interest
#'
#' @return a data frame with season summary results and pythagorean win expectation
#' @export
team_reg_season_summary <- function(df, team){
  sched_df <- df %>%
    filter( away_team == team | home_team == team ) %>%
    filter( is.na(away_score) == FALSE )

  pf_vector <- rep( NA_integer_ , nrow( sched_df ) )
  pa_vector <- rep( NA_integer_ , nrow( sched_df ) )
  win_vector <- rep( 0 , nrow( sched_df ) )
  loss_vector <- rep( 0 , nrow( sched_df ) )
  tie_vector <- rep( 0 , nrow( sched_df ) )

  for(i in 1:nrow(sched_df)){
    #Team of interest home
    if( team == sched_df$home_team[i] ){
      pf_vector[i] = sched_df$home_score[i]
      pa_vector[i] = sched_df$away_score[i]

      if( sched_df$home_score[i] > sched_df$away_score[i] ){
        win_vector[i] = 1
      }
      else{
        if( sched_df$home_score[i] < sched_df$away_score[i] ){
          loss_vector[i] = 1
        }
        else{
          tie_vector[i] = 1
        }
      }

    }
    #Team of interest is away
    else{
      pa_vector[i] = sched_df$home_score[i]
      pf_vector[i] = sched_df$away_score[i]

      if( sched_df$home_score[i] > sched_df$away_score[i] ){
        loss_vector[i] = 1
      }
      else{
        if( sched_df$home_score[i] < sched_df$away_score[i] ){
          win_vector[i] = 1
        }
        else{
          tie_vector[i] = 1
        }
      }
    }
  }

  results_df = data.frame(team = team,
                          wins = sum(win_vector),
                          losses = sum(loss_vector),
                          ties = sum(tie_vector),
                          season_point_for = sum(pf_vector),
                          season_point_against = sum(pa_vector),
                          season_point_diff = sum(pf_vector) - sum(pa_vector))

  total_games = sum(win_vector, loss_vector, tie_vector)
  results_df = results_df %>%
    mutate(pythag_wins = ((season_point_for ^ 2.37)/((season_point_for ^ 2.37) + (season_point_against ^ 2.37)))*total_games)

  return(results_df)
}
