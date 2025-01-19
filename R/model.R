
#' Fit a Poisson Regression Model and Evaluate Performance
#'
#' This function fits a Poisson regression model to predict `runs` using all other variables in the provided data frame.
#' It also calculates the Mean Squared Error (MSE) of the model's predictions.
#'
#' @param df A data frame containing the response variable `runs` and the predictor variables.
#'
#' @return A list containing:
#'   - `mse`: The Mean Squared Error of the model's predictions.
#'   - `model_object`: The fitted Poisson regression model.
#'   - `preds`: The original data frame with an additional column `preds` containing the predicted values.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' result <- poisson_model(my_data)
#' mse_value <- result$mse
#' model <- result$model_object
#' predictions <- result$preds
#' }
poisson_model <- function(df) {
  checkmate::check_data_frame(df)

  # Fit Poisson regression model
  mod <- stats::glm(runs ~ ., data = df, family = stats::poisson())

  # Generate predictions
  df$preds <- stats::predict(mod, newdata = df, type = "response")

  # Calculate Mean Squared Error (MSE)
  mse <- mean((df$runs - df$preds)^2)

  # Return results
  list(
    mse = mse,
    model_object = mod,
    preds = df
  )
}



#' Train a Model on Training and Validation Data and Evaluate on Test Data
#'
#' This function trains a model using specified training and validation data, with optional evaluation on test data.
#' It allows for the use of the full dataset for training if `train_on_full` is set to TRUE.
#'
#' @param data_splits A list containing `train_data`, `val_data`, and `test_data` data frames.
#' @param model_fn A function that takes a data frame and returns model results.
#' @param evaluate_test A logical flag indicating whether to evaluate the model on the test data. Default is FALSE.
#' @param train_on_full A logical flag indicating whether to train on the full dataset (train + validation + test). Default is FALSE.
#'
#' @return A list containing:
#'   - `train_results`: The results from training (output of `model_fn` on training data).
#'   - `test_results`: The results from testing (output of `model_fn` on test data), or NULL if `evaluate_test` is FALSE.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' result <- train_model(data_splits, model_fn, evaluate_test = TRUE)
#' training_results <- result$train_results
#' test_results <- result$test_results
#' }
#' @export
train_model <- function(data_splits, model_fn, evaluate_test = FALSE, train_on_full = FALSE) {
  checkmate::assert_list(data_splits)
  checkmate::assert_function(model_fn)

  # Determine the data to use for training and testing
  if (train_on_full) {
    train_data <- dplyr::bind_rows(data_splits$train_data, data_splits$val_data, data_splits$test_data)
    test_data <- NULL
  } else {
    if (evaluate_test) {
      train_data <- dplyr::bind_rows(data_splits$train_data, data_splits$val_data)
      test_data <- data_splits$test_data
    } else {
      train_data <- data_splits$train_data
      test_data <- data_splits$val_data
    }
  }

  # Train the model
  train_results <- model_fn(train_data)

  # Test the model (if applicable)
  test_results <- if (!is.null(test_data)) {
    model_fn(test_data)
  } else {
    NULL
  }

  # Return results
  list(
    train_results = train_results,
    test_results = test_results
  )
}


#' Train and Evaluate Poisson Model on Data Splits
#'
#' This function runs a training pipeline for the Poisson regression model. It trains the model, evaluates performance on training and validation data,
#' then tests the model on the test set. A final model is trained on the full dataset (train + validation + test).
#'
#' @param data_splits A list containing `train_data`, `val_data`, and `test_data` data frames.
#'
#' @return The final model trained on the full dataset (train + validation + test).
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' final_model <- training_pipeline(data_splits)
#' }
#' @export
training_pipeline <- function(data_splits) {

  # Train model and evaluate on validation data
  validation_metrics <- train_model(data_splits, poisson_model)
  message(sprintf("\nTrain MSE: %.3f", validation_metrics$train_results$mse))
  message(sprintf("Validation MSE: %.3f", validation_metrics$train_results$mse))

  # Test model and evaluate on test data
  test_metrics <- train_model(data_splits, poisson_model, evaluate_test = TRUE)
  message("\nModel finalized...\nRunning against test data")
  message(sprintf("Train MSE: %.3f", test_metrics$train_results$mse))
  message(sprintf("Test MSE: %.3f", test_metrics$train_results$mse))

  # Train final model on the full dataset (train + validation + test)
  final_model <- train_model(data_splits, poisson_model, train_on_full = TRUE)

  # Return final model
  final_model
}


