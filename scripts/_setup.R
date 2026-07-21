### DF -------------------------------------------------------------------------

df_recode <- df_init |>
  mutate(
    age = trunc(as.numeric(date_diag - date_birth) / 365.25 * 10) / 10,
    time_diag_chir = time_length(interval(date_diag, date_chir), "months"),
    across(starts_with("tm_stade"), factor),
    tm_stade_t = fct_collapse(tm_stade_t, "T1-T2" = 1:2, "T3-T4" = 3:4),
    tm_stade_n = tm_stade_n |> fct_relabel(~ str_glue("N{.x}")),
    tm_stade_m = tm_stade_m |> fct_relabel(~ str_glue("M{.x}")),
    chir_complic_class_maj = chir_complic_class >= 3,
    # chir_complic_class = fct_collapse(factor(chir_complic_class), "III-IV-V" = 3:5),
    induc_nb = coalesce(induc_nb, 0),
    adj_nb = coalesce(adj_nb, 0),
    n_cures = induc_nb + adj_nb,
    n_cures_outcome = n_cures < 12,
    n_cures_outcome_sub = n_cures_outcome |
      induc_adapt_pct %in% 2 |
      adj_adapt_pct %in% 2,
    total_ei = coalesce(induc_ei, adj_ei),
    n_recidive_site_meta = rowSums(across(starts_with("recidive_site_meta"))) |>
      na_if(0),
    n_recidive_site_meta = if_else(n_recidive_site_meta == 1, 1, 2),
    os_date = date_death,
    os_event = complete.cases(os_date),
    os_tte = time_length(
      x = interval(date_diag, coalesce(os_date, date_lastnews)),
      unit = "months"
    ),
    pfs_date = pmin(date_recidive, date_death, na.rm = TRUE),
    pfs_event = complete.cases(pfs_date),
    pfs_tte = time_length(
      x = interval(date_chir, coalesce(pfs_date, date_lastnews)),
      unit = "months"
    ),
    pfs_cause = case_when(
      !is.na(date_recidive) &
        (is.na(date_death) | date_recidive <= date_death) ~ 1,
      !is.na(date_death) ~ 2,
      .default = 0
    )
  ) |>
  easy_cut(
    var = age,
    values = c(60, 70),
    labels = c("<60", "60-69", "≥70")
  ) |>
  easy_cut(
    var = age,
    incr = TRUE,
    from = 40,
    to = 85,
    by = 5
  ) |>
  easy_label(
    variable = list(
      age = "Âge au diagnostic, années",
      age_cat = "Âge au diagnostic, années",
      age_incr = "Âge au diagnostic, années",
      time_diag_chir = "Délai entre diagnostic et chirurgie, mois",
      chir_complic_class_maj = "Complication post-opératoire majeure",
      n_cures = "Nombre total de cures",
      n_cures_outcome = "Nombre total de cures < 12",
      n_cures_outcome_sub = "Nombre total de cures < 12 : ou dose relative moyenne < 80%",
      total_ei = "Toxicité de grade III-IV",
      n_recidive_site_meta = "Nombre de sites métastatiques",
      os_event = "Décès toutes causes",
      os_tte = "Délai entre le diagnostic et le décès toutes causes, mois",
      pfs_event = "Récidive ou décès toutes causes",
      pfs_tte = "Délai entre la chirurgie et la récidive ou le décès toutes causes, mois",
      pfs_cause = "Premier évènement"
    ),
    value = list(
      n_recidive_site_meta = c("1" = 1, ">=2" = 2),
      pfs_cause = c("Censure" = 0, "Récidive" = 1, "Décès sans récidive" = 2)
    )
  ) |>
  discard(~ is.null(var_label(.x)))

df <- df_recode |>
  rownames_to_column() |>
  mutate(rowname = as.numeric(rowname) + 1)

.induc <- lst(
  label = var_label(df$groupe),
  level = levels(df$groupe)[2],
  n = sum(df$groupe == level, na.rm = TRUE),
  df = df |>
    filter(groupe == level) |>
    mutate(groupe = fct_drop(groupe))
)

.tox_vars <- map(
  set_names(c("induc", "adj")),
  ~ names(df) |>
    str_subset(str_glue("{.x}_ei_")) |>
    set_names() |>
    imap_dbl(~ sum(as.numeric(df[[.]]), na.rm = TRUE)) |>
    sort(decreasing = TRUE) |>
    names()
)

.check_df <- lst(
  data_recode = df |> select(-rowname),
  names = names(data_recode),
  data_init = df_init |> select(any_of(names)) |> easy_label(),
  vars = easy_view(data_recode)$output,
  distrib_recode = check_distrib(data_recode),
  distrib_init = check_distrib(data_init),
)

### FIT ------------------------------------------------------------------------

.followup <- build_model(
  data = df,
  tte = os_tte,
  event = 1 - os_event,
  strata = groupe,
  estimate_label = "récidive ou décès",
  tbl_label = "Probabilité d'être encore suivi",
  type = "surv"
)

.surv <- lst(
  os = build_model(
    data = df,
    tte = os_tte,
    event = os_event,
    strata = groupe,
    estimate_label = "le décès toutes causes",
    tbl_label = "Probabilité de survie globale",
    type = "surv"
  ),
  pfs = build_model(
    data = df,
    tte = pfs_tte,
    event = pfs_event,
    strata = groupe,
    estimate_label = "la récidive ou le décès toutes causes",
    tbl_label = "Probabilité de survie sans récidive",
    type = "surv"
  )
) |>
  list_transpose()

.surv_labs <- list(
  os = list(
    x = "Durée depuis le diagnostic (mois)",
    y = "Probabilité de survie globale (%)"
  ),
  pfs = list(
    x = "Durée depuis la chirurgie (mois)",
    y = "Probabilité de survie sans récidive (%)"
  )
)

.cmp <- build_model(
  data = df,
  tte = pfs_tte,
  event = pfs_cause,
  strata = groupe,
  estimate_label = "la récidive",
  tbl_label = "Probabilité de survie sans récidive (%)",
  type = "cmp"
)

.fig_palette <- c(opts$color$base, opts$color$cold[2])

### MODEL ----------------------------------------------------------------------

.model <- lst()

.model$default <- lst(
  data = df |> mutate(ps_diag = ps_diag |> fct_collapse("1-2" = c(1, 2))),
  vars = set_model_vars(
    y = "n_cures_outcome",
    x_uv = c(
      "groupe",
      "centre",
      "age_incr",
      "ps_diag",
      "chir_resec"
    ),
    x_mv_exclude = "chir_resec"
  ),
  args = lst(
    pvalue_fun = opts$pvalue$format,
    exponentiate = TRUE
  )
)

### AUTO EXEC ------------------------------------------------------------------

# auto_exec()
