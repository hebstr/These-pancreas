tbl_event <- df |>
  select(groupe, os_event, pfs_cause) |>
  mutate(
    pfs_cause = fct_relevel(
      pfs_cause,
      "Récidive",
      "Décès sans récidive",
      "Censure"
    )
  ) |>
  use_vars() |>
  tbl_summary(
    by = groupe,
    statistic = opts$vars$stat,
    digits = opts$digits,
    missing = "ifany",
    missing_text = opts$labs$row_missing
  ) |>
  add_stat_label(label = opts$vars$label) |>
  gtsum_format(
    label_header = opts$labs$header,
    label_overall = opts$labs$overall
  ) |>
  tbl_format()

easy_out(tbl_event)
