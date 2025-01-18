


join_innings_data <- function(match_results, innings_results) {

  checkmate::check_tibble(match_results)
  checkmate::check_tibble(innings_results)

  df <- match_results |>
    dplyr::filter(is.na(result), gender == 'male') |>
    dplyr::count(matchid, dates) |>
    dplyr::inner_join(
      innings_results[, c("matchid", "over", "innings", "team", "batsman", "bowler", "runs.total", "wicket.player_out")],
      by = c('matchid'),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      over_split = stringr::str_split_fixed(over, "\\.", 2)
    ) |>
    dplyr::mutate(
      over = as.integer(over_split[, 1]),
      ball = as.integer(over_split[, 2]),
      wicket.player_out = as.integer(!is.na(wicket.player_out))
    ) |>
    dplyr::group_by(matchid, over, innings, team) |>
    dplyr::arrange(wicket.player_out) |>
    dplyr::mutate(rn = dplyr::row_number()) |>
    dplyr::filter(rn == 1) |>
    dplyr::select(-over_split, -rn) |>
    dplyr::ungroup() |>
    dplyr::rename(wickets = wicket.player_out)

  assertthat::assert_that(max(df$wickets) <= 10)

  df
}



aggregate_innings_data <- function(df_joined_data) {

  checkmate::check_tibble(df)
  df |>
    dplyr::group_by(team, dates, matchid, innings, over, ball) |>
    dplyr::summarise(
      runs = sum(runs.total, na.rm = TRUE),
      wickets = sum(wickets, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    dplyr::group_by(team, matchid) |>
    dplyr::arrange(over, ball) |>
    dplyr::mutate(
      total_runs = cumsum(runs),
      total_wickets = cumsum(wickets),
      overs_remaining = 50 - over - 1,
      wickets_remaining = 10 - total_wickets
    ) |>
    dplyr::ungroup() |>
    dplyr::select(team, innings, overs_remaining, wickets_remaining, runs)

}

transform_data <- function(data_list) {

  match_results <- data_list$match_results
  innings_results <- data_list$innings_results

  df <- join_inning_data(match_results = match_results, innings_results = innings_results) |>
    aggregate_innings_data()

  df
}





