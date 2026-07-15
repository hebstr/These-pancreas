tbl_ttt_adj <- df |>
  filter(adj_nb > 0) |>
  select(
    groupe,
    matches("adj_dose"),
    adj_adapt,
    adj_adapt_pct,
    starts_with("adj_adapt_c"),
  ) |>
  strip_label("^(Induction|Adjuvant)\\s*: (dose)?\\s*") |>
  use_vars() |>
  tbl_summary(
    by = groupe,
    statistic = opts$vars$stat,
    type = matches("adj_dose") ~ "continuous",
    value = binary_value(df),
    digits = opts$digits,
    missing = "ifany"
  ) |>
  add_stat_label(label = opts$vars$label) |>
  gtsum_format() |>
  add_label(
    name = "Dernière dose reçue",
    levels = c("adj_dose_5fu", "adj_dose_irino", "adj_dose_oxali")
  ) |>
  col_missing() |>
  gt_format() |>
  tab_style(
    style = cell_text(size = px(11)),
    locations = cells_body(columns = dm)
  )

easy_out(tbl_ttt_adj, width = 700)

# pct : adaptation max parmi les 3 mol (dernière cure)
