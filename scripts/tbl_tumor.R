.site_meta_vars <- names(df) |>
  str_subset("site_meta_") |>
  set_names() |>
  imap_dbl(~ sum(as.numeric(df[[.]]))) |>
  sort(decreasing = TRUE) |>
  names()

.site_meta_note <- "Parmi les patients ayant récidivé au niveau métastatique ou local + métastatique."

tbl_tumor <- df |>
  select(
    groupe,
    time_diag_chir,
    chir_resec,
    chir_hosp,
    chir_complic_class,
    chir_marges,
    recidive_type,
    n_recidive_site_meta,
    all_of(.site_meta_vars)
  ) |>
  mutate(chir_marges = fct_drop(chir_marges)) |>
  use_vars() |>
  tbl_summary(
    by = groupe,
    statistic = opts$vars$stat,
    value = binary_value(df),
    digits = opts$digits,
    missing = "ifany"
  ) |>
  add_stat_label(label = opts$vars$label) |>
  gtsum_format() |>
  remove_row_type(variables = "n_recidive_site_meta", type = "missing") |>
  add_label(name = "Site métastatique", levels = .site_meta_vars) |>
  add_note(
    vars = "chir_complic_class",
    note = "Selon la classification de Clavien-Dindo."
  ) |>
  add_note(
    vars = "chir_resec",
    note = "Selon les critères du National Comprehensive Cancer Network (NCCN)."
  ) |>
  add_note(vars = "n_recidive_site_meta", note = .site_meta_note) |>
  add_note(levels = "Site métastatique", note = .site_meta_note) |>
  tbl_format(width = 750)

easy_out(tbl_tumor)
