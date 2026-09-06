.tox <- lst(
  data = df |> filter(n_cures > 0),
  level = binary_value(data)$total_ei,
  n = nrow(data),
  n_eval = sum(!is.na(data$total_ei)),
  n_ei = sum(data$total_ei == level, na.rm = TRUE),
  pct = style_pct(n_ei / n_eval)
)

tbl_tox <- .tox$data |>
  select(
    groupe,
    total_ei,
    all_of(.tox_vars)
  ) |>
  strip_label("\\s*grade\\s*3-4$") |>
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
  add_p(pvalue_fun = opts$pvalue$format) |>
  gtsum_format() |>
  modify_indent(
    columns = label,
    rows = variable %in% .tox_vars & row_type %in% "label"
  ) |>
  add_note(
    vars = "total_ei",
    note = "Selon la Common Terminology Criteria for Adverse Events (CTCAE) v5.0."
  ) |>
  tbl_format(
    note_global = str_glue(
      "Patients ayant reçu au moins une cure de chimiothérapie d'induction \\
      ou adjuvante ({sum(df$n_cures > 0)} sur {nrow(df)} patients inclus)."
    )
  )

easy_out(tbl_tox)

### QMD ------------------------------------------------------------------------

.tox <- c(
  .tox,
  qmd_only(lst(
    ei = map(
      lst(
        vom = "total_ei_vom",
        neuro = "total_ei_neuro",
        dig = "total_ei_dig",
        asth = "total_ei_asthenie",
        neutrop = "total_ei_neutrop"
      ),
      ~ tbl_n_pct(tbl_tox, .x)
    )
  ))
)
