tbl_tumor <- df |>
  select(
    groupe,
    chir_resec,
    chir_hosp,
    chir_complic_class,
    chir_marges,
    recidive_none,
    recidive_loc,
    recidive_meta
  ) |>
  mutate(chir_marges = fct_drop(chir_marges)) |>
  strip_label(str_glue("^{.grp_lab$recidive} : ")) |>
  use_vars() |>
  tbl_summary(
    by = groupe,
    statistic = opts$vars$stat,
    value = binary_value(df),
    digits = opts$digits,
    missing = "ifany"
  ) |>
  add_stat_label(label = opts$vars$label) |>
  add_p(
    pvalue_fun = opts$pvalue$format,
    test.args = .test_args
  ) |>
  gtsum_format() |>
  add_note(
    vars = "chir_complic_class",
    note = "Selon la classification de Clavien-Dindo."
  ) |>
  add_note(
    vars = "chir_resec",
    note = "Selon les critères du National Comprehensive Cancer Network (NCCN)."
  ) |>
  add_variable_group_header(
    header = .grp_lab$recidive,
    variables = c(recidive_none, recidive_loc, recidive_meta)
  ) |>
  add_note(
    vars = c("recidive_loc", "recidive_meta"),
    note = "Une récidive à la fois locale et métastatique est comptée dans les deux catégories."
  ) |>
  tbl_format(width = 800)

easy_out(tbl_tumor)

### QMD ------------------------------------------------------------------------

.tumor <- qmd_only(lst(
  delai = df$time_diag_chir |>
    (\(x) c(lst(total = x), split(x, df$groupe)))() |>
    set_names("total", "adj", "periop") |>
    map(med_iqr),
  recidive = lst(
    n = sum(df$recidive_type != "Aucune"),
    pct = style_pct(mean(df$recidive_type != "Aucune")),
    locale = tbl_n_pct(tbl_tumor, "recidive_loc"),
    meta = tbl_n_pct(tbl_tumor, "recidive_meta")
  )
))
