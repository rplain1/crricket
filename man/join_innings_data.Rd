% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{join_innings_data}
\alias{join_innings_data}
\title{Join Match and Innings Data}
\usage{
join_innings_data(match_results, innings_results)
}
\arguments{
\item{match_results}{A tibble containing match results. Must include the columns \code{matchid}, \code{result}, \code{gender}, and \code{dates}.}

\item{innings_results}{A tibble containing innings results. Must include the columns \code{matchid}, \code{over}, \code{innings}, \code{team}, \code{batsman}, \code{bowler}, \code{runs.total}, and \code{wicket.player_out}.}
}
\value{
A tibble with the joined and processed data, including calculated fields such as \code{over}, \code{ball}, and \code{wickets}.
}
\description{
This function joins match results and innings results, performs data transformation, and calculates derived fields.
}
\examples{
\dontrun{
match_data <- tibble::tibble(
  matchid = 1:5,
  result = c(NA, "Win", NA, "Loss", NA),
  gender = c("male", "male", "female", "male", "male"),
  dates = Sys.Date() + 1:5
)
innings_data <- tibble::tibble(
  matchid = c(1, 1, 2, 3, 5),
  over = c("1.1", "1.2", "2.1", "3.1", "5.1"),
  innings = c(1, 1, 2, 1, 1),
  team = c("Team A", "Team A", "Team B", "Team C", "Team D"),
  batsman = c("Player 1", "Player 2", "Player 3", "Player 4", "Player 5"),
  bowler = c("Bowler 1", "Bowler 2", "Bowler 3", "Bowler 4", "Bowler 5"),
  runs.total = c(4, 6, 2, 3, 1),
  wicket.player_out = c(NA, "Player 1", NA, NA, NA)
)
result <- join_innings_data(match_data, innings_data)
}
}
