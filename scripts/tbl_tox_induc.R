.tox_induc <- lst(
  data = .induc$df |> filter(induc_nb > 0),
  level = binary_value(data)$induc_ei,
  n = nrow(data),
  n_ei = sum(data$induc_ei == level, na.rm = TRUE),
  pct = style_pct(n_ei / sum(!is.na(data$induc_ei)))
)

tbl_tox_induc <- .tox_induc$data |>
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
    missing_text = opts$labs$row_missing
  ) |>
  add_stat_label(label = opts$vars$label) |>
  gtsum_format() |>
  modify_indent(
    columns = label,
    rows = variable %in% .tox_vars$induc & row_type %in% "label"
  ) |>
  modify_column_hide(columns = stat_0) |>
  modify_header(all_stat_cols(stat_0 = FALSE) ~ "**{level}<br>(n={n})**") |>
  add_note(
    vars = "induc_ei",
    note = "Selon la Common Terminology Criteria for Adverse Events (CTCAE) v5.0."
  ) |>
  tbl_format(width = 450)

easy_out(tbl_tox_induc)
