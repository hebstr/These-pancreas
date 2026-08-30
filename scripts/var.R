### VAR ------------------------------------------------------------------------

var_dict <- get_vars_dict(df)

easy_out(var_dict)

### DISTRIB --------------------------------------------------------------------

var_distrib <- list(
  initial = df_init |>
    select(any_of(names(df))) |>
    easy_label() |>
    get_var_distrib(
      title = "Distribution des variables **avant** recodage",
      width = 800
    ),
  final = df |>
    get_var_distrib(
      title = "Distribution des variables **après** recodage",
      width = 800
    )
)

easy_out(var_distrib)

### CHECK ----------------------------------------------------------------------

# .check <- list()

# .check$ttt <- df_init |>
#   easy_check(
#     birth_missing = is.na(date_birth),
#     diag_missing = is.na(date_diag),
#     chir_missing = is.na(date_chir),
#     datepoint_missing = is.na(date_point),
#     chir_avant_diag = date_chir < date_diag,
#     recidive_avant_diag = date_recidive < date_diag,
#     death_avant_diag = date_death < date_diag,
#     recidive_avant_chir = date_recidive < date_chir,
#     death_avant_chir = date_death < date_chir,
#     recidive_after_death = date_recidive > date_death,
#     diag_after_datepoint = date_diag > date_point,
#     chir_after_datepoint = date_chir > date_point,
#     death_after_datepoint = date_death > date_point,
#     recidive_after_datepoint = date_recidive > date_point,
#     recidive_sans_date = recidive_type != 0 & is.na(date_recidive),
#     date_sans_recidive = !is.na(date_recidive) & recidive_type == 0,
#     pat_sans_chimio = induc_chimio == 0 & adj_chimio == 0,
#     pat_sans_cure = n_cures == 0,
#     groupe_adj_avec_induc = groupe == 1 &
#       (induc_chimio == 1 | induc_nb > 0),
#     groupe_periop_sans_induc = groupe == 2 &
#       (induc_chimio == 0 | induc_nb == 0),
#     induc_chimio_sans_cure = induc_chimio == 1 & induc_nb == 0,
#     adj_chimio_sans_cure = adj_chimio == 1 & adj_nb == 0,
#     induc_cure_sans_chimio = induc_nb > 0 & induc_chimio == 0,
#     adj_cure_sans_chimio = adj_nb > 0 & adj_chimio == 0,
#     induc_adapt_sans_detail = induc_adapt == 1 &
#       (is.na(induc_detail) | induc_detail == 0),
#     induc_detail_sans_adapt = induc_adapt == 0 &
#       induc_detail %in% c(1, 2),
#     adj_adapt_sans_detail = adj_adapt == 1 &
#       (is.na(adj_detail) | adj_detail == 0),
#     adj_detail_sans_adapt = adj_adapt == 0 & adj_detail %in% c(1, 2),
#     .with = list(
#       centre,
#       groupe,
#       induc_nb = coalesce(induc_nb, 0),
#       adj_nb = coalesce(adj_nb, 0),
#       n_cures = induc_nb + adj_nb,
#       induc_detail = pmax(induc_adapt_c4, induc_adapt_c8, induc_adapt_c12, na.rm = TRUE),
#       adj_detail = pmax(adj_adapt_c4, adj_adapt_c8, adj_adapt_c12, na.rm = TRUE)
#     )
#   ) |>
#   easy_label()

# var_check <- get_xlsx(.check)

# easy_out(var_check)

### TEMP -----------------------------------------------------------------------

.limits <- lst(
  cures = map(
    set_names(levels(df$groupe), c("adj", "periop")),
    ~ df |>
      filter(groupe == .x) |>
      summarise(
        max = max(n_cures, na.rm = TRUE),
        over = sum(n_cures > 12, na.rm = TRUE)
      )
  ),
  followup = survfit(Surv(os_tte, 1 - os_event) ~ centre, data = df) |>
    summary() |>
    pluck("table") |>
    as_tibble(rownames = "centre") |>
    transmute(
      centre = str_remove(centre, "^centre="),
      med = round(median, 1)
    ) |>
    arrange(med)
)
