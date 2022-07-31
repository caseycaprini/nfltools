#' Returns long data frame documenting final result of each regular season game
#'
#' @param year a season of interest
#'
#' @return a long data frame with results of each game in a season
#' @export
nfl_reg_season_results <- function(year = 2020){
  sched_df <- fast_scraper_schedules( year ) %>%
    filter( game_type == "REG" ) %>%
    filter( is.na(away_score) == FALSE )

  teams = sched_df %>%
    distinct(home_team) %>%
    pull(home_team)

  for(k in 1:length(teams)){
    team = teams[k]
    weeks = sched_df %>%
      filter(home_team == team | away_team == team) %>%
      summarize(n = n()) %>%
      pull(n)

    team_vector <- rep( team , weeks )
    week_vector <- rep( NA_integer_ , weeks )
    home_away_vector <- rep( NA_character_ , weeks )
    opponent_vector <- rep( NA_character_ , weeks )
    pf_vector <- rep( NA_integer_ , weeks )
    pa_vector <- rep( NA_integer_ , weeks )
    win_vector <- rep( 0 , weeks )
    loss_vector <- rep( 0 , weeks )
    tie_vector <- rep( 0 , weeks )

    j = 1
    team_sched_df = sched_df %>%
      filter(away_team == team | home_team == team)

    for(i in 1:nrow(team_sched_df)){
      #Team of interest home
      if( team == team_sched_df$home_team[i] ){
        week_vector[j] = team_sched_df$week[i]
        home_away_vector[j] = "home"
        opponent_vector[j] = team_sched_df$away_team[i]
        pf_vector[j] = team_sched_df$home_score[i]
        pa_vector[j] = team_sched_df$away_score[i]

        if( team_sched_df$home_score[i] > team_sched_df$away_score[i] ){
          win_vector[j] = 1
        }

        else{
          if( team_sched_df$home_score[i] < team_sched_df$away_score[i] ){
            loss_vector[j] = 1
          }
          else{
            tie_vector[j] = 1
          }
        }
        j = j + 1
      }
      #Team of interest is away
      else{
        week_vector[j] = team_sched_df$week[i]
        home_away_vector[j] = "away"
        opponent_vector[j] = team_sched_df$home_team[i]
        pa_vector[j] = team_sched_df$home_score[i]
        pf_vector[j] = team_sched_df$away_score[i]

        if( team_sched_df$home_score[i] > team_sched_df$away_score[i] ){
          loss_vector[j] = 1
        }
        else{
          if( team_sched_df$home_score[i] < team_sched_df$away_score[i] ){
            win_vector[j] = 1
          }
          else{
            tie_vector[j] = 1
          }
        }
        j = j + 1
      }
    }
    if(k == 1){
      result_df = data.frame(team = team_vector,
                             week = week_vector,
                             home_away = home_away_vector,
                             points_for = pf_vector,
                             points_against = pa_vector,
                             win = win_vector,
                             loss = loss_vector,
                             tie = tie_vector)
    }
    else{
      temp_df = data.frame(team = team_vector,
                           week = week_vector,
                           home_away = home_away_vector,
                           points_for = pf_vector,
                           points_against = pa_vector,
                           win = win_vector,
                           loss = loss_vector,
                           tie = tie_vector)
      result_df = rbind(result_df, temp_df)
    }
  }

  result_df <- result_df %>%
    mutate(season = year) %>%
    select(season, everything())

  return(result_df %>% arrange(team))
}
