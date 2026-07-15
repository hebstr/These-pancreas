tbl_tox_induc <- .induc$df |>
  filter(induc_nb > 0) |>
  select(
    groupe,
    induc_ei,
    all_of(.tox_vars$induc)
  ) |>
  strip_label(str_glue("^Induction\\s*:\\s*|(\\s*grade\\s*3-4)$")) |>
  use_vars() |>
  tbl_summary(
    by = groupe,
    statistic = opts$vars$stat,
    value = binary_value(df),
    digits = opts$digits,
    missing = "ifany",
    missing_text = opts$labs$missing
  ) |>
  add_stat_label(label = opts$vars$label) |>
  gtsum_format() |>
  modify_indent(
    columns = label,
    rows = variable %in% .tox_vars$induc & row_type %in% "label"
  ) |>
  modify_column_hide(columns = stat_0) |>
  modify_header(all_stat_cols(stat_0 = FALSE) ~ "**{level}<br>(n={n})**") |>
  # col_missing() |>
  gt_format() |>
  add_note(
    vars = "induc_ei",
    note = "Selon la classification CTCAE.",
  )

easy_out(tbl_tox_induc, width = 450)
