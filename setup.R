### SETUP ----------------------------------------------------------------------

library(conflicted)
library(tidyverse)
library(googlesheets4)
library(rlang)
library(broom)
library(labelled)
library(gtsummary)
library(ggsurvfit)
library(survival)
library(ggrepel)
library(patchwork)
library(Gmisc)
library(grid)
library(hebstr)

if (interactive()) {
  print(conflict_scout())
}

conflicts_prefer(dplyr::filter, .quiet = TRUE)

auto_exec("lib", quiet = TRUE)

lang_fr()

source("_extensions/hebstr/hebstr-doc/fonts/register.R")

set_opts(
  font = "Luciole",
  .default_font = "Helvetica"
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

### DF -------------------------------------------------------------------------

df_init <- sheets$inclusions |>
  rownames_to_column() |>
  mutate(
    rowname = as.numeric(rowname) + 1,
    across(any_of(c(dict$type$dbl, dict$type$int)), as.numeric),
    across(all_of(dict$type$date), dmy)
  )

df_recode <- df_init |>
  mutate(
    age = trunc(as.numeric(date_diag - date_birth) / 365.25 * 10) / 10,
    time_diag_chir = time_length(interval(date_diag, date_chir), "months"),
    across(starts_with("tm_stade"), factor),
    tm_stade_t = fct_collapse(tm_stade_t, "T1-T2" = 1:2, "T3-T4" = 3:4),
    tm_stade_n = tm_stade_n |> fct_relabel(~ str_glue("N{.x}")),
    tm_stade_m = tm_stade_m |> fct_relabel(~ str_glue("M{.x}")),
    chir_complic_class_maj = chir_complic_class >= 3,
    chir_complic_class = pmin(chir_complic_class, 3),
    ca_diag_bin = ca_diag >= 500,
    induc_nb = coalesce(induc_nb, 0),
    adj_nb = coalesce(adj_nb, 0),
    n_cures = induc_nb + adj_nb,
    n_cures_complete = n_cures >= 12,
    n_cures_complete_sub = n_cures_complete &
      !(induc_adapt_pct %in% 2) &
      !(adj_adapt_pct %in% 2),
    total_ei = coalesce(induc_ei, adj_ei),
    n_recidive_site_meta = rowSums(across(starts_with("recidive_site_meta"))) |>
      na_if(0),
    n_recidive_site_meta = if_else(n_recidive_site_meta == 1, 1, 2),
    os_date = date_death,
    os_event = complete.cases(os_date),
    os_tte = time_length(
      x = interval(date_chir, coalesce(os_date, date_point)),
      unit = "months"
    ),
    os_tte_diag = time_length(
      x = interval(date_diag, coalesce(os_date, date_point)),
      unit = "months"
    ),
    pfs_date = pmin(date_recidive, date_death, na.rm = TRUE),
    pfs_event = complete.cases(pfs_date),
    pfs_tte = time_length(
      x = interval(date_chir, coalesce(pfs_date, date_point)),
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
  )

df <- df_recode |>
  easy_label(
    variable = list(
      age = "Âge au diagnostic, années",
      age_cat = "Âge au diagnostic, années",
      age_incr = "Âge au diagnostic, années",
      time_diag_chir = "Délai entre diagnostic et chirurgie, mois",
      chir_complic_class_maj = "Complication post-opératoire majeure",
      ca_diag_bin = "CA 19-9 au diagnostic >= 500 UI/L",
      n_cures = "Nombre total de cures",
      n_cures_complete = "Nombre total de cures >= 12",
      n_cures_complete_sub = "Nombre total de cures >= 12 : avec dose relative moyenne >= 80 %",
      total_ei = "Toxicité de grade III-IV",
      n_recidive_site_meta = "Nombre de sites métastatiques",
      os_event = "Décès toutes causes",
      os_tte = "Délai entre la chirurgie et le décès toutes causes, mois",
      os_tte_diag = "Délai entre le diagnostic et le décès toutes causes, mois",
      pfs_event = "Récidive ou décès toutes causes",
      pfs_tte = "Délai entre la chirurgie et la récidive ou le décès toutes causes, mois",
      pfs_cause = "Premier évènement"
    ),
    value = list(
      chir_complic_class = c("Pas de complication" = 0, "I" = 1, "II" = 2, "III-IV-V" = 3),
      n_recidive_site_meta = c("1" = 1, ">=2" = 2),
      pfs_cause = c("Censure" = 0, "Récidive" = 1, "Décès sans récidive" = 2)
    ),
    drop = TRUE
  )

### FIT ------------------------------------------------------------------------

.followup <- build_model(
  data = df,
  tte = os_tte,
  event = 1 - os_event,
  strata = groupe,
  estimate_label = "la fin de suivi",
  tbl_label = "Probabilité d'être encore suivi"
)

.followup_alive <- df |>
  filter(os_event == 0) |>
  summarise(
    n = n(),
    min = round(min(os_tte), 1),
    max = round(max(os_tte), 1)
  ) |>
  as.list()

.followup_median <- lst(
  total = style_median(.followup$total$data$tte$median),
  strata = style_median_strata(.followup$strata$data$tte$median)
)

.surv <- lst(
  os = build_model(
    data = df,
    tte = os_tte,
    event = os_event,
    strata = groupe,
    estimate_label = "le décès toutes causes",
    tbl_label = "Probabilité de survie globale"
  ),
  pfs = build_model(
    data = df,
    tte = pfs_tte,
    event = pfs_event,
    strata = groupe,
    estimate_label = "la récidive ou le décès toutes causes",
    tbl_label = "Probabilité de survie sans récidive"
  )
) |>
  list_transpose()

.surv_labs <- list(
  os = list(
    x = "Durée depuis la chirurgie (mois)",
    y = "Probabilité de survie globale (%)"
  ),
  pfs = list(
    x = "Durée depuis la chirurgie (mois)",
    y = "Probabilité de survie sans récidive (%)"
  )
)

.fig_palette <- c(opts$color$base, opts$color$cold[2])

### MODEL ----------------------------------------------------------------------

.model <- lst()

.model$default <- lst(
  data = df |> modify_if(is.factor, fct_drop),
  vars = set_model_vars(
    y = "n_cures",
    x_uv = c(
      "groupe",
      "centre",
      "age_incr",
      "chir_marges",
      "chir_complic_class_maj"
    ),
    x_mv_exclude = NULL
  ),
  args = lst(
    pvalue_fun = opts$pvalue$format,
    exponentiate = TRUE
  )
)

### VARS -----------------------------------------------------------------------

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

### RUN ------------------------------------------------------------------------

# source("backup.R")
# auto_exec()
# export_docx()
