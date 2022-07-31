#' Given game play-by-play data, returns finite time average point differential (i.e., time_avg_lead) for regulation time
#'
#' @param game_pbp a play-by-play data file via nflfastR
#' @param team a team of interest
#'
#' @return a data frame containing game info and time average lead for regulation
#' @export
mvt_game <- function(game_pbp, team)
{
  # Reduce to only scoring plays that occur in regulation (i.e., not in overtime)
  game_pbp <- game_pbp %>%
    filter( game_half %in% c( "Half1" , "Half2" ) ) %>%
    filter( sp == 1 )

  # One more interval than scoring plays
  m <- nrow( game_pbp ) + 1
  time_int <- rep( NA_integer_ , m )
  point_diff <- rep( NA_integer_ , m )

  for(i in 1:m){
    time_int[i] <- ifelse(i == 1,
                          3600 - game_pbp$game_seconds_remaining[1],
                          ifelse(i == m,
                                 game_pbp$game_seconds_remaining[m - 1] - 0,
                                 game_pbp$game_seconds_remaining[i - 1] - game_pbp$game_seconds_remaining[i]) )

    point_diff[i] <- ifelse(i == 1,
                            0,
                            ifelse(game_pbp$posteam[i - 1] == team,
                                   game_pbp$score_differential_post[i - 1],
                                   game_pbp$score_differential_post[i - 1] * -1) )

  }

  week = game_pbp$week[1]
  team = team
  opponent = ifelse(game_pbp$home_team[1] == team,
                    game_pbp$away_team[1],
                    game_pbp$home_team[1])
  home_away = ifelse(game_pbp$home_team[1] == team,
                     "home",
                     "away")
  time_avg_lead = sum((time_int * point_diff))/3600

  df = data.frame(week = week,
                  team = team,
                  opponent = opponent,
                  home_away = home_away,
                  time_avg_lead = time_avg_lead
                  )

  return(df)
}
