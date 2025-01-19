#' Join Match and Innings Data
#'
#' This function joins match results and innings results, performs data transformation, and calculates derived fields.
#'
#' @param match_results A tibble containing match results. Must include the columns \code{matchid}, \code{result}, \code{gender}, and \code{dates}.
#' @param innings_results A tibble containing innings results. Must include the columns \code{matchid}, \code{over}, \code{innings}, \code{team}, \code{batsman}, \code{bowler}, \code{runs.total}, and \code{wicket.player_out}.
#' @return A tibble with the joined and processed data, including calculated fields such as \code{over}, \code{ball}, and \code{wickets}.
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' match_data <- tibble::tibble(
#'   matchid = 1:5,
#'   result = c(NA, "Win", NA, "Loss", NA),
#'   gender = c("male", "male", "female", "male", "male"),
#'   dates = Sys.Date() + 1:5
#' )
#' innings_data <- tibble::tibble(
#'   matchid = c(1, 1, 2, 3, 5),
#'   over = c("1.1", "1.2", "2.1", "3.1", "5.1"),
#'   innings = c(1, 1, 2, 1, 1),
#'   team = c("Team A", "Team A", "Team B", "Team C", "Team D"),
#'   batsman = c("Player 1", "Player 2", "Player 3", "Player 4", "Player 5"),
#'   bowler = c("Bowler 1", "Bowler 2", "Bowler 3", "Bowler 4", "Bowler 5"),
#'   runs.total = c(4, 6, 2, 3, 1),
#'   wicket.player_out = c(NA, "Player 1", NA, NA, NA)
#' )
#' result <- join_innings_data(match_data, innings_data)
#' }
#' @export
join_innings_data <- function(match_results, innings_results) {
  checkmate::check_tibble(match_results)
  checkmate::check_tibble(innings_results)

  df <- match_results |>
    dplyr::filter(is.na(.data$result), .data$gender == "male") |>
    dplyr::count(.data$matchid, .data$dates) |>
    dplyr::inner_join(
      innings_results[, c("matchid", "over", "innings", "team", "batsman", "bowler", "runs.total", "wicket.player_out")],
      by = c("matchid"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      over_split = stringr::str_split_fixed(.data$over, "\\.", 2)
    ) |>
    dplyr::mutate(
      over = as.integer(.data$over_split[, 1]),
      ball = as.integer(.data$over_split[, 2]),
      wicket.player_out = as.integer(!is.na(.data$wicket.player_out))
    ) |>
    dplyr::group_by(.data$matchid, .data$over, .data$innings, .data$team) |>
    dplyr::arrange(.data$wicket.player_out) |>
    dplyr::mutate(rn = dplyr::row_number()) |>
    dplyr::filter(.data$rn == 1) |>
    dplyr::select(-.data$over_split, -.data$rn) |>
    dplyr::ungroup() |>
    dplyr::rename(wickets = .data$wicket.player_out)

  assertthat::assert_that(max(df$wickets) <= 10)

  df
}


#' Aggregate Innings Data
#'
#' This function aggregates cricket innings data, calculating cumulative runs,
#' wickets, and remaining overs and wickets for each team in a match.
#'
#' @param df_joined_data A tibble containing joined match and innings data with columns:
#'   `team`, `dates`, `matchid`, `innings`, `over`, `ball`, `runs.total`, and `wickets`.
#'
#' @return A tibble with aggregated data for each team's innings, including:
#'   `team`, `innings`, `overs_remaining`, `wickets_remaining`, and `runs`.
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' # Assuming `df_joined_data` is a tibble with the required structure:
#' # result <- aggregate_innings_data(df_joined_data)
#' }
#' @export
aggregate_innings_data <- function(df_joined_data) {

  checkmate::check_tibble(df_joined_data)

  df_joined_data |>
    dplyr::group_by(.data$team, .data$dates, .data$matchid, .data$innings, .data$over, .data$ball) |>
    dplyr::summarise(
      runs = sum(.data$runs.total, na.rm = TRUE),
      wickets = sum(.data$wickets, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$team, .data$matchid) |>
    dplyr::arrange(.data$over, .data$ball) |>
    dplyr::mutate(
      total_runs = cumsum(.data$runs),
      total_wickets = cumsum(.data$wickets),
      overs_remaining = 50 - .data$over - 1,
      wickets_remaining = 10 - .data$total_wickets
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      .data$team, .data$innings, .data$overs_remaining, .data$wickets_remaining, .data$runs
    )
}

#' Transform Match and Innings Data
#'
#' Processes the input datasets containing match and innings data to create a transformed dataset
#' by joining and aggregating the information.
#'
#' @param datasets A list containing:
#'   \itemize{
#'     \item `match_results`: A tibble of match results.
#'     \item `innings_results`: A tibble of innings results.
#'   }
#' @return A tibble containing the aggregated innings data with the following columns:
#'   \itemize{
#'     \item `team`: Team name.
#'     \item `innings`: Inning number.
#'     \item `overs_remaining`: Overs remaining for the team.
#'     \item `wickets_remaining`: Wickets remaining for the team.
#'     \item `runs`: Runs scored by the team.
#'   }
#' @export
transform_data <- function(datasets) {
  match_results <- datasets$match_results
  innings_results <- datasets$innings_results

  df <- join_innings_data(match_results = match_results, innings_results = innings_results) |>
    aggregate_innings_data()

  df
}


#' Split Data into Training, Validation, and Test Sets
#'
#' This function splits a given data frame into three subsets: training (60%), validation (20%), and test (20%).
#' It removes the 'team' column from the data frame before performing the split. If the `prediction` argument is set to TRUE,
#' it returns the modified data frame without performing the split.
#'
#' @param df A data frame to be split.
#' @param prediction A logical flag indicating whether to return the modified data frame without splitting. Default is FALSE.
#'
#' @return A list containing three elements:
#'   - `train_data`: Data frame for training.
#'   - `val_data`: Data frame for validation.
#'   - `test_data`: Data frame for testing.
#'   If `prediction` is TRUE, the modified data frame is returned instead.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' result <- split_data(my_data)
#' training_set <- result$train_data
#' validation_set <- result$val_data
#' testing_set <- result$test_data
#' }
#' @export
split_data <- function(df, prediction = FALSE) {
  checkmate::check_data_frame(df)

  n <- nrow(df)
  df <- df[, -which(colnames(df) == "team")]  # Remove 'team' column

  if (prediction) {
    return(df)
  }

  train_idx <- sample(1:n, size = 0.6 * n)  # 60% for training
  remaining_idx <- setdiff(1:n, train_idx)
  val_idx <- sample(remaining_idx, size = 0.2 * n)  # 20% for validation
  test_idx <- setdiff(remaining_idx, val_idx)  # Remaining 20% for testing

  train_data <- df[train_idx, ]
  val_data <- df[val_idx, ]
  test_data <- df[test_idx, ]

  message("Training data: ", nrow(train_data))
  message("Validation data: ", nrow(val_data))
  message("Testing data: ", nrow(test_data))

  return(
    list(
      train_data = train_data,
      val_data = val_data,
      test_data = test_data
    )
  )
}



















