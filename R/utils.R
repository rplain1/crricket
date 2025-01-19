#' Initialize Temporary Workspace
#'
#' Creates a temporary directory structure with "staging" and "model" subdirectories
#' inside the base temporary directory. The function returns a list containing the paths
#' to these directories for further use in model storage and staging purposes.
#'
#' @return A list with two elements: `staging` and `model`, each being the path to
#'   the respective directory.
#' @examples
#' \dontrun{
#' temp_dirs <- initialize_temp_workspace()
#' temp_dirs$staging  # Path to the staging directory
#' temp_dirs$model    # Path to the model directory
#' }
#' @export
initialize_temp_workspace <- function() {
  temp_base <- tempdir()

  staging_dir <- file.path(temp_base, "staging")
  model_dir <- file.path(temp_base, "model")

  dir.create(staging_dir, showWarnings = FALSE)
  dir.create(model_dir, showWarnings = FALSE)

  list(staging = staging_dir, model = model_dir)
}

#' Generate Ireland Data Subset
#'
#' Filters the input data frame to return only the rows where the `team` column equals "Ireland".
#' It then returns the first few rows of this subset for quick inspection.
#'
#' @param df A data frame containing a column named `team`.
#' @return A data frame containing rows where `team` is "Ireland".
#' @examples
#' \dontrun{
#' ireland_data <- generate_ireland_data(df)
#' head(ireland_data)
#' }
generate_ireland_data <- function(df) {
  df |>
    dplyr::filter(.data$team == 'Ireland') |>  # Use .data for tidy evaluation
    utils::head()
}


