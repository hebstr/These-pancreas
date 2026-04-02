tbl_baseline <- df |>
  discard(is.Date) |>
  use_vars() |>
  tbl_summary(
    # by = centre,
    statistic = opts$vars$stat,
    type = list(where(is.numeric) ~ "continuous"),
    digits = opts$digits,
    missing = "ifany",
    missing_text = opts$labs$missing
  ) |>
  add_stat_label(label = opts$vars$label) |>
  gtsum_format() |>
  gt_format()

easy_out(tbl_baseline, width = 500)
