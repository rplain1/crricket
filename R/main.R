#' Main Entry Point for Model Training and Prediction
#'
#' This function is the entry point for the entire project workflow. It:
#' - Builds the Poisson regression model by calling `build_model()`,
#' - Uses the trained model to make predictions on the `ireland` dataset by calling `predict_runs()`.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' main()
#' }
main <- function() {
  # Build the model and get the path to the saved model
  model_path <- build_model()

  # Use the model to predict runs on the ireland dataset
  predictions <- crricket::ireland |>
    predict_runs(model_path = model_path)

  message("Predictions for Ireland dataset: ")
  print(predictions)
}
