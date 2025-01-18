
elt_to_temp_dir <- function(file_name) {

  con <- DBI::dbConnect(duckdb::duckdb())

  DBI::dbExecute(con, "INSTALL json;")
  DBI::dbExecute(con, "LOAD json;")
  DBI::dbExecute(con, "INSTALL httpfs;")
  DBI::dbExecute(con, "LOAD httpfs;")

  # Create a temp file path for the Parquet output
  tmp_file <- file.path(tempdir(), glue::glue("{file_name}.csv"))

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


extract_data <- function() {
  matches_path <- elt_to_temp_dir('match_results')
  matches <- arrow::read_parquet(matches_path)

  innings_path <- elt_to_temp_dir('innings_results')
  innings <- arrow::read_parquet(innings_path)
  unlink(matches_path, recursive = TRUE)

  return(
    list(
      match_results = matches,
      innings_results = innings
    )
  )
}



