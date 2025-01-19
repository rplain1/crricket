#' Export JSON to Temporary Directory as Parquet
#'
#' This function connects to DuckDB, installs and loads necessary extensions,
#' and converts a remote JSON file to a Parquet file saved in the temporary directory.
#'
#' @param file_name A string representing the base name of the file to process. The JSON file is expected to be located in the S3 bucket with the given name.
#' @return A string containing the file path of the generated Parquet file in the temporary directory.
#' @examples
#' \dontrun{
#' elt_to_temp_dir("match_results")
#' }
#' @export
elt_to_temp_dir <- function(file_name) {

  con <- DBI::dbConnect(duckdb::duckdb())

  DBI::dbExecute(con, "INSTALL json;")
  DBI::dbExecute(con, "LOAD json;")
  DBI::dbExecute(con, "INSTALL httpfs;")
  DBI::dbExecute(con, "LOAD httpfs;")

  # Create a temp file path for the Parquet output
  tmp_file <- file.path(tempdir(), glue::glue("{file_name}.parquet"))

  # Execute the SQL query to read JSON and write Parquet
  query <- glue::glue('
    COPY (
      SELECT *
      FROM read_json(\'https://cricketwireless.s3.us-east-2.amazonaws.com/raw-data/{file_name}.json\',maximum_depth = -1, sample_size = -1)

    )
    TO \'{tmp_file}\' (FORMAT PARQUET);
  ')

  DBI::dbExecute(con, query)
  message("File ", file_name, " written to: ", tmp_file)
  DBI::dbDisconnect(con)

  return(tmp_file)
}


#' Extract Data from S3 and Return Datasets
#'
#' This function extracts datasets from remote JSON files, processes them into
#' Parquet format, reads the data, and removes temporary files. It returns
#' the data as a list of tibbles.
#'
#' @return A named list containing:
#' \describe{
#'   \item{match_results}{A tibble with match results.}
#'   \item{innings_results}{A tibble with innings results.}
#' }
#' @examples
#' \dontrun{
#' data <- extract_data()
#' }
#' @export
extract_data <- function() {
  matches_path <- elt_to_temp_dir('match_results')
  matches <- arrow::read_parquet(matches_path)
  unlink(matches_path, recursive = TRUE)


  innings_path <- elt_to_temp_dir('innings_results')
  innings <- arrow::read_parquet(innings_path)
  unlink(innings_path, recursive = TRUE)

  return(
    list(
      match_results = matches,
      innings_results = innings
    )
  )
}



