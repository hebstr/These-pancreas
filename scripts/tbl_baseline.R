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
    matches("tm_stade_(t|n)")
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
  add_p(
    pvalue_fun = opts$pvalue$format,
    test.args = .test_args
  ) |>
  gtsum_format() |>
  tbl_format(width = 800)

easy_out(tbl_baseline)

### QMD ------------------------------------------------------------------------

.baseline <- qmd_only(lst(
  age = tbl_med_iqr(tbl_baseline, "age"),
  homme = tbl_n_pct(tbl_baseline, "sexe", level = "Homme")
))
