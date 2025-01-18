initialize_temp_workspace <- function() {
  temp_base <- tempdir()

  # Create subdirectories
  staging_dir <- file.path(temp_base, "staging")
  model_dir <- file.path(temp_base, "model")

  # Ensure they exist
  dir.create(staging_dir, showWarnings = FALSE)
  dir.create(model_dir, showWarnings = FALSE)

  list(staging = staging_dir, model = model_dir)
}
