% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{main}
\alias{main}
\title{Main Entry Point for Model Training and Prediction}
\usage{
main()
}
\description{
This function is the entry point for the entire project workflow. It:
- Builds the Poisson regression model by calling `build_model()`,
- Uses the trained model to make predictions on the `ireland` dataset by calling `predict_runs()`.
}
\examples{
\dontrun{
# Example usage:
main()
}
}
