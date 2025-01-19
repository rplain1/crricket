#' Build and Train Poisson Model, Save to Disk
#'
#' This function orchestrates the entire process of model building:
#' - Initializes the temporary workspace,
#' - Extracts and transforms the data,
#' - Splits the data into training, validation, and test sets,
#' - Trains the model using the training pipeline,
#' - Saves the trained model to disk.
#'
#' @return The path to the saved model file.
#'
#' @examples
#' \dontrun{
#' Example usage:
#' model_path <- build_model()
#' }
#' @export
build_model <- function() {
  temp_locations <- initialize_temp_workspace()  # Set up temporary file locations
  datasets <- extract_data()  # Extract the datasets
  df_transformed <- transform_data(datasets)  # Transform the data
  data_splits <- split_data(df_transformed)  # Split the data into training, validation, and test sets

  # Train the model
  model <- training_pipeline(data_splits)

  # Save the model to disk
  model_output_path <- paste0(temp_locations$model, "poisson_model.rds")
  saveRDS(model$train_results$model_object, model_output_path)

  # Return the path to the saved model
  model_output_path
}

#' Predict Runs Using a Trained Poisson Model
#'
#' This function loads a previously saved Poisson regression model and generates predictions
#' for the given dataset.
#'
#' @param df A data frame containing the data for which to generate predictions.
#' @param model_path The path to the saved model file (RDS format).
#'
#' @return A data frame with an additional column `pred` containing the predicted values.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' predictions <- predict_runs(my_data, "path/to/poisson_model.rds")
#' }
#' @export
predict_runs <- function(df, model_path) {
  # Load the model from the specified path
  model <- readRDS(model_path)

  # Generate predictions
  df |>
    dplyr::mutate(pred = stats::predict(model, newdata = df, type = "response"))
}



