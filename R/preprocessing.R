


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

split_data <- function(df) {
  checkmate::check_data_frame(df)
  n <- nrow(df)

  train_idx <- sample(1:n, size = 0.6 * n)  # 60% for training
  remaining_idx <- setdiff(1:n, train_idx)
  val_idx <- sample(remaining_idx, size = 0.2 * n)  # 20% for validation
  test_idx <- setdiff(remaining_idx, val_idx)  # Remaining 20% for testing

  df_transformed[, -which(colnames(df_transformed) == 'team')]

  # Split the data
  train_data <- df[train_idx, ]
  val_data <- df[val_idx, ]
  test_data <- df[test_idx, ]

  # Check sizes
  message("Training data: ", nrow(train_data))  # 60 rows
  message("Validation data: ", nrow(val_data))    # 20 rows
  message("Testing data: ", nrow(test_data))   # 20 rows

  return(
    list(
      train_data = train_data,
      val_data = val_data,
      test_data = test_data
    )
  )
}

poisson_model <- function(df) {
  glm(runs ~ ., data = df, family = poisson)
}

model_data <- function(data_splits) {
  checkmate::assert_list(data_splits)

  train_data <- data_splits$train_data
  val_data <- data_splits$val_data
  test_data <- data_splits$test_data

  purrr::map(.x = data_splits, .f = poisson_model)

  # TODO: wrap into a dataframe or list that contains eval metric and data
}



df_transformed <- transform_data(datasets2)
data_splits <- split_data(df_transformed)


df_transformed

mod <- glm(runs ~ ., data = df_transformed[, -which(colnames(df_transformed) == 'team')], family = poisson)

df_transformed |>
  dplyr::mutate(
    pred = predict(mod, newdata = df_transformed, type = 'response')
  ) |>
  dplyr::pull(pred) |> summary()

df_transformed


















