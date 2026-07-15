tbl_outcome <- df |>
  select(
    groupe,
    induc_chimio,
    induc_nb,
    adj_chimio,
    adj_nb,
    n_cures_outcome,
    n_cures_outcome_sub,
  ) |>
  strip_label(str_glue("^(Induction|Adjuvant)\\s*:\\s*")) |>
  strip_label(str_glue("^{var_label(df$n_cures_outcome)}\\s*:\\s*"), to_upper = FALSE) |>
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
    rows = variable %in% c("induc_nb", "adj_nb", "n_cures_outcome_sub") & row_type %in% 'label'
  ) |>
  modify_table_body(
    ~ .x |>
      mutate(across(
        c(stat_0, stat_1),
        \(x) if_else(variable %in% c("induc_chimio", "induc_nb"), "–", x)
      ))
  ) |>
  gt_format() |>
  add_note(
    vars = c("induc_chimio", "adj_chimio"),
    note = "Nombre de patients ayant reçu au moins une cure.",
  )

easy_out(tbl_outcome, width = 750)
