initialize_temp_duckdb <- function() {
  return(con)
}



# Function to perform ETL task
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
      FROM read_json(\'https://cricketwireless.s3.us-east-2.amazonaws.com/raw-data/{file_name}.json\')
    )
    TO \'{tmp_file}\' (FORMAT CSV);
  ')

  # Execute the query
  DBI::dbExecute(con, query)
  message("File ", file_name, " written to: ", tmp_file)
  DBI::dbDisconnect(con)

  return(tmp_file)
}


main <- function() {
  # Run the function
  matches_path <- elt_to_temp_dir('match_results')
  matches <- readr::read_csv(matches_path)

  innings_path <- elt_to_temp_dir('innings_results')
  innings <- readr::read_csv(innings_path)
  unlink(matches_path, recursive = TRUE)

  return(
    list(
      match_results = matches,
      innings_results = innings
    )
  )
}



