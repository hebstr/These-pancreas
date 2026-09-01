tbl_ttt_adj <- df |>
  filter(adj_nb > 0) |>
  select(
    groupe,
    matches("adj_dose"),
    adj_adapt,
    adj_adapt_pct,
    starts_with("adj_adapt_c")
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
  add_variable_group_header(
    header = "Dernière dose reçue",
    variables = matches("adj_dose")
  ) |>
  add_note(
    vars = "adj_adapt_pct",
    note = "Adaptation maximale parmi les trois molécules, à la dernière cure."
  ) |>
  tbl_format(
    note_global = paste(
      "Patients ayant reçu au moins une cure de chimiothérapie adjuvante",
      str_glue("({sum(df$adj_nb > 0)} sur {nrow(df)} patients inclus).")
    )
  )

easy_out(tbl_ttt_adj)
