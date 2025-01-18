
match_results <- datasets2$match_results
innings_results <- datasets2$innings_results

'
df = (
  match_results_df.filter((pl.col("result").is_null()) & (pl.col("gender") == "male"))
  .group_by(["matchid", "dates"])
  .len()
  .join(
    innings_results_df.select(
      ["matchid", "over", "innings", "team", "batsman", "bowler", "runs.total", "wicket.player_out"]
    ),
    on="matchid",
  )
  .with_columns(pl.col("over").str.split_exact(".", 1).struct.rename_fields(["over", "ball"]))
  .unnest("over")
  .with_columns(
    dates=pl.col("dates").cast(pl.Date),
    over=pl.col("over").cast(pl.Int64),
    ball=pl.col("ball").cast(pl.Int64),
    wicket=pl.col("wicket.player_out")
    .is_not_null()
    .cast(pl.Int64),  # unnesting like I did caused multiple rows for wicket player
    rn=pl.col("wicket.player_out").rank("ordinal").over(["matchid", "over", "innings", "team"]),
  )
  .filter(pl.col("rn") == 1)
  .drop("rn")
)
'


df <- match_results |>
  dplyr::filter(is.na(result), gender == 'male') |>
  dplyr::count(matchid, dates) |>
  dplyr::inner_join(
    innings_results[, c("matchid", "over", "innings", "team", "batsman", "bowler", "runs.total", "wicket.player_out")],
    by = c('matchid'),
    relationship = "many-to-many"
  ) |>
  dplyr::mutate(
    over_split = stringr::str_split_fixed(over, "\\.", 2)
  ) |>
  dplyr::mutate(
    over = as.integer(over_split[, 1]),
    ball = as.integer(over_split[, 2]),
    wicket.player_out = as.integer(!is.na(wicket.player_out))
  ) |>
  dplyr::select(-over_split) # TODO: come back and handle the duplicate rows via dplyr::count()

  df |> dplyr::select(dates, over, ball, wicket.player_out)
