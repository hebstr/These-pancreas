# PHASE INDUCTION

tbl_ttt_induc <- .induc$df |>
  filter(induc_nb > 0) |>
  select(
    matches("induc_dose"),
    induc_adapt,
    induc_adapt_pct,
    starts_with("induc_adapt_c"),
  ) |>
  strip_label("^(Induction|Adjuvant)\\s*: (dose)?\\s*") |>
  use_vars() |>
  tbl_summary(
    statistic = opts$vars$stat,
    type = matches("induc_dose") ~ "continuous",
    value = binary_value(df),
    digits = opts$digits,
    missing = "ifany",
    missing_text = opts$labs$missing
  ) |>
  add_stat_label(label = opts$vars$label) |>
  add_label(
    name = "Dernière dose reçue",
    levels = c("induc_dose_5fu", "induc_dose_irino", "induc_dose_oxali")
  ) |>
  gtsum_format(
    label_stat = str_glue(
      "Groupe<br>{tolower(.induc$level)}<br>
      <span style='display:inline-block; margin-top:6px'>(N={.induc$n})</span>"
    )
  ) |>
  col_missing() |>
  gt_format() |>
  tab_style(
    style = cell_text(size = px(11)),
    locations = cells_body(columns = dm)
  )

easy_out(tbl_ttt_induc, width = 450)
