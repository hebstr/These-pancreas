tbl_ttt <- .chimio$data |>
  select(
    groupe,
    matches("total_dose"),
    total_adapt,
    total_adapt_pct
  ) |>
  strip_label(str_glue("^{.grp_lab$dose} : ")) |>
  use_vars() |>
  tbl_summary(
    by = groupe,
    statistic = opts$vars$stat,
    type = matches("total_dose") ~ "continuous",
    value = binary_value(df),
    digits = opts$digits,
    missing = "ifany",
    missing_text = opts$labs$row_missing
  ) |>
  add_stat_label(label = opts$vars$label) |>
  add_p(
    pvalue_fun = opts$pvalue$format,
    test.args = .test_args
  ) |>
  gtsum_format() |>
  add_variable_group_header(
    header = .grp_lab$dose,
    variables = matches("total_dose")
  ) |>
  add_note(
    vars = "total_adapt_pct",
    note = "Adaptation maximale parmi les trois molécules, à la dernière cure."
  ) |>
  tbl_format(note_global = .chimio$note, width = 800)

easy_out(tbl_ttt)

### QMD ------------------------------------------------------------------------

.ttt <- c(
  lst(n = .chimio$n),
  qmd_only(lst(
    adapt = tbl_n_pct(tbl_ttt, "total_adapt"),
    adapt_sup = tbl_n_pct(tbl_ttt, "total_adapt_pct", level = ">20%")
  ))
)
