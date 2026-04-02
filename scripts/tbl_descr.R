tbl_baseline <- df |>
  select(-matches("date"), -surv_pfs) |>
  use_vars() |>
  tbl_summary(
    # by = centre,
    statistic = opts$vars$stat,
    digits = opts$digits,
    missing = "ifany",
    missing_text = opts$labs$missing
  ) |>
  add_stat_label(label = opts$vars$label) |>
  gtsum_format() |>
  gt_format()

easy_out(tbl_baseline, width = 500)
