checklist <- list()

checklist$ttt <- df_init |>
  check_fun(
    induc_nb = coalesce(induc_nb, 0),
    adj_nb = coalesce(adj_nb, 0),
    n_cures = induc_nb + adj_nb,
    induc_detail = pmax(induc_adapt_c4, induc_adapt_c8, induc_adapt_c12, na.rm = TRUE),
    adj_detail = pmax(adj_adapt_c4, adj_adapt_c8, adj_adapt_c12, na.rm = TRUE),
    check_birth_missing = is.na(date_birth),
    check_diag_missing = is.na(date_diag),
    check_chir_missing = is.na(date_chir),
    check_lastnews_missing = is.na(date_lastnews),
    check_chir_avant_diag = date_chir < date_diag,
    check_recidive_avant_diag = date_recidive < date_diag,
    check_death_avant_diag = date_death < date_diag,
    check_recidive_avant_chir = date_recidive < date_chir,
    check_death_avant_chir = date_death < date_chir,
    check_recidive_after_death = date_recidive > date_death,
    check_diag_after_lastnews = date_diag > date_lastnews,
    check_chir_after_lastnews = date_chir > date_lastnews,
    check_death_after_lastnews = date_death > date_lastnews,
    check_recidive_after_lastnews = date_recidive > date_lastnews,
    check_recidive_sans_date = recidive_type != 0 & is.na(date_recidive),
    check_date_sans_recidive = !is.na(date_recidive) & recidive_type == 0,
    check_pat_sans_chimio = induc_chimio == 0 & adj_chimio == 0,
    check_pat_sans_cure = n_cures == 0,
    check_groupe_adj_avec_induc = groupe == 1 & (induc_chimio == 1 | induc_nb > 0),
    check_groupe_periop_sans_induc = groupe == 2 &
      (induc_chimio == 0 | induc_nb == 0),
    check_induc_chimio_sans_cure = induc_chimio == 1 & induc_nb == 0,
    check_adj_chimio_sans_cure = adj_chimio == 1 & adj_nb == 0,
    check_induc_cure_sans_chimio = induc_nb > 0 & induc_chimio == 0,
    check_adj_cure_sans_chimio = adj_nb > 0 & adj_chimio == 0,
    check_induc_adapt_sans_detail = induc_adapt == 1 &
      (is.na(induc_detail) | induc_detail == 0),
    check_induc_detail_sans_adapt = induc_adapt == 0 & induc_detail %in% c(1, 2),
    check_adj_adapt_sans_detail = adj_adapt == 1 & (is.na(adj_detail) | adj_detail == 0),
    check_adj_detail_sans_adapt = adj_adapt == 0 & adj_detail %in% c(1, 2),
    .vars = c("centre", "groupe")
  ) |>
  easy_label()

checklist$adapt_apres_derniere_cure <- df_init |>
  mutate(
    across(c(induc_nb, adj_nb), as.numeric),
    induc_nb = coalesce(induc_nb, 0),
    adj_nb = coalesce(adj_nb, 0),
    total_nb = induc_nb + adj_nb
  ) |>
  select(rowname, Nom, centre, groupe, induc_nb, adj_nb, total_nb, matches("adapt")) |>
  pivot_longer(
    cols = matches("adapt"),
    names_to = c("phase", "checkpoint"),
    names_pattern = "(induc|adj)_adapt_c(\\d+)",
    values_to = "adapt"
  ) |>
  mutate(
    adapt = as.numeric(adapt),
    checkpoint = as.numeric(checkpoint),
    derniere_cure = if_else(phase == "induc", induc_nb, adj_nb),
    ecart = checkpoint - derniere_cure
  ) |>
  filter(!is.na(adapt), ecart > 0) |>
  mutate(
    check = "adapt_apres_derniere_cure",
    adapt = recode_values(
      x = adapt,
      from = dict$val$adj_adapt_c4,
      to = names(dict$val$adj_adapt_c4)
    )
  ) |>
  relocate(check, .after = 1) |>
  easy_label()

# openxlsx2::wb_save(wb = get_xlsx(checklist), file = "check.xlsx")
