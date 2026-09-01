tbl_ttt_induc <- .induc$df |>
  filter(induc_nb > 0) |>
  select(
    groupe,
    matches("induc_dose"),
    induc_adapt,
    induc_adapt_pct,
    starts_with("induc_adapt_c")
  ) |>
  strip_label("^(Induction|Adjuvant)\\s*: (dose)?\\s*") |>
  use_vars() |>
  tbl_summary(
    by = groupe,
    statistic = opts$vars$stat,
    type = matches("induc_dose") ~ "continuous",
    value = binary_value(df),
    digits = opts$digits,
    missing = "ifany",
    missing_text = opts$labs$row_missing
  ) |>
  add_stat_label(label = opts$vars$label) |>
  gtsum_format() |>
  add_variable_group_header(
    header = "Dernière dose reçue",
    variables = matches("induc_dose")
  ) |>
  modify_column_hide(columns = stat_0) |>
  modify_header(all_stat_cols(stat_0 = FALSE) ~ "**{level}<br>(n={n})**") |>
  add_note(
    vars = "induc_adapt_pct",
    note = "Adaptation maximale parmi les trois molécules, à la dernière cure."
  ) |>
  tbl_format(width = 500)

easy_out(tbl_ttt_induc)
