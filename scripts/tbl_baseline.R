tbl_baseline <- df |>
  select(
    groupe,
    centre,
    sexe,
    age,
    age_cat,
    ps_diag,
    dl_diag,
    ca_diag,
    ca_diag_bin,
    tm_loc,
    starts_with("tm_stade")
  ) |>
  use_vars() |>
  tbl_summary(
    by = groupe,
    statistic = opts$vars$stat,
    value = binary_value(df),
    digits = opts$digits,
    missing = "ifany",
    missing_text = opts$labs$row_missing
  ) |>
  add_stat_label(label = opts$vars$label) |>
  gtsum_format() |>
  tbl_format(width = 750)

easy_out(tbl_baseline)
