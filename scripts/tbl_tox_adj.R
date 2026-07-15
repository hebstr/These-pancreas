.tox_adj <- lst(
  data = df |> filter(adj_nb > 0),
  level = binary_value(data)$adj_ei,
  n = nrow(data),
  n_ei = sum(data$adj_ei == level, na.rm = TRUE),
  pct = style_percent(
    n_ei / sum(!is.na(data$adj_ei)),
    symbol = TRUE,
    digits = 1
  )
)

tbl_tox_adj <- .tox_adj$data |>
  select(
    groupe,
    adj_ei,
    all_of(.tox_vars$adj)
  ) |>
  strip_label(str_glue("^(Induction|Adjuvant)\\s*:\\s*|(\\s*grade\\s*3-4)$")) |>
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
  col_missing() |>
  modify_indent(
    columns = label,
    rows = variable %in% .tox_vars$adj & row_type %in% "label"
  ) |>
  gt_format() |>
  add_note(
    vars = "adj_ei",
    note = "Selon la classification CTCAE.",
  ) |>
  tab_style(
    style = cell_text(size = px(11)),
    locations = cells_body(columns = dm)
  )

easy_out(tbl_tox_adj, width = 750)
