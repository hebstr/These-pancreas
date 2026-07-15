### INIT -----------------------------------------------------------------------

source("rv/scripts/rvr.R")
source("rv/scripts/activate.R")
.rv$sync()
.rv$summary()

options(
  repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"),
  gargle_oauth_email = Sys.getenv("GARGLE_OAUTH_EMAIL", unset = NA)
  # openxlsx2.maxWidth = 60,
)

library(conflicted)
library(tidyverse)
library(googlesheets4)
library(rlang)
library(broom)
library(labelled)
library(gtsummary)
library(ggsurvfit)
library(survival)
library(tidycmprsk)
library(ggrepel)
library(gt)
library(ggswim)
library(patchwork)
library(Gmisc)
library(grid)
library(officer)
library(rvg)
library(hebstr)

print(conflict_scout())

conflicts_prefer(dplyr::filter, Gmisc::coords, .quiet = TRUE)

auto_exec("config", quiet = TRUE)

lang_fr()

set_opts(
  acro = acro(
    DM ~ "données manquantes par groupe"
  ),
  font = "luciole"
)

opts <- get_opts()

update_geom_defaults("text", list(family = opts$font$alpha))

### GS IMPORT ------------------------------------------------------------------

sheets <- map(
  set_names(1:3, "variables", "inclusions", "exclusions"),
  ~ read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/11yV-hnL3OBmuUa5EOKfJylzl1yeg4WrleTKy8CKc2yI/edit?gid=102661300#gid=102661300",
    sheet = .x,
    col_types = "c"
  )
)

dict <- extract_from_dict(
  data = sheets$variables,
  var_name = var_name,
  var_label = var_label,
  type = type,
  level = level
)

easy_label <- \(data, variable = list(), value = list()) {
  data |>
    modify_if(is.logical, as.numeric) |>
    set_variable_labels(!!!dict$var, !!!variable, .strict = FALSE) |>
    set_value_labels(!!!dict$val, !!!value, .strict = FALSE) |>
    mutate(across(any_of(dict$type$int) | where(is.labelled), labelled::to_factor))
}

df_init <- sheets$inclusions |>
  rownames_to_column() |>
  mutate(
    rowname = as.numeric(rowname) + 1,
    across(any_of(c(dict$type$dbl, dict$type$int)), as.numeric),
    across(all_of(dict$type$date), dmy)
  )

### CHECKS ---------------------------------------------------------------------

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
    # check_recidive_sans_date = recidive_type != 0 & is.na(date_recidive),
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
