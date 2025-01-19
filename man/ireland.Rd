\name{ireland}
\alias{ireland}
\title{Ireland Cricket Match Data}
\description{
  A dataset containing cricket match data for the Ireland team, including innings,
  overs remaining, wickets remaining, and runs scored.
}
\usage{
  data(ireland)
}
\format{
  A tibble with 6 rows and 5 variables:
  \describe{
    \item{team}{Character vector indicating the team (all entries are "Ireland").}
    \item{innings}{Integer indicating the inning number (1 or 2).}
    \item{overs_remaining}{Numeric indicating the number of overs remaining.}
    \item{wickets_remaining}{Numeric indicating the number of wickets remaining.}
    \item{runs}{Integer indicating the runs scored in the current inning.}
  }
}
\details{
  The `ireland_data.RData` file is stored in the `data/` directory of the package and contains
  performance data for Ireland in a series of cricket innings.
}
\examples{
  # Load the dataset
  data(ireland)

  # View the data
  head(ireland)
}
