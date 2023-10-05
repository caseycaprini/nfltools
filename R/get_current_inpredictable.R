#' Returns current inpredictable nfl rankings (scraped)
#'
#' @return a dataframe containing current inpredictable nfl vegas power rankings
#' @export
get_current_inpredictable <- function(){

scraped_list <-
  rvest::read_html(
    "https://stats.inpredictable.com/rankings/nfl.php"
  ) %>%
  rvest::html_elements("table") %>%
  rvest::html_table()

scraped_df <-
  scraped_list[[1]]

names(scraped_df) = NULL

team <-
  scraped_df[-1,2] %>%
  as_vector()
gpf <-
  scraped_df[-1, 5] %>%
  as_vector()
offense_gpf <-
  scraped_df[-1, 7] %>%
  as_vector()
defense_gpf <-
  scraped_df[-1, 10] %>%
  as_vector()
played_sos <-
  scraped_df[-1, 17] %>%
  as_vector()
remaining_sos <-
  scraped_df[-1, 19] %>%
  as_vector()

df <-
  data.frame(
    team = team,
    gpf = as.numeric(gpf),
    offense_gpf = as.numeric(offense_gpf),
    defense_gpf = as.numeric(defense_gpf),
    played_sos = as.numeric(played_sos),
    remaining_sos = as.numeric(remaining_sos)
  )

df <-
  df %>%
  mutate(team = str_remove(team, "&nbsp"),
         team = case_when(
           team == "JAC" ~ "JAX",
           team == "ARZ" ~ "ARI",
           T ~ team))

return(df)

}
