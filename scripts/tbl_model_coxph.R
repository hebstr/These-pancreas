### DATA ------------------------------------------------------------------------

.model$coxph <- map(set_names(names(.surv$total)), \(outcome) {
  lst(
    data = .model$default$data,
    vars = set_model_vars(
      y = paste0(outcome, c("_tte", "_event")),
      x_uv = .model$default$vars$x$uv,
      x_mv_exclude = c("centre", "chir_complic_class_maj")
    ),
    args = .model$default$args,
    descr = \() {
      check_model_vars(
        data = data[, c(vars$y[2], vars$x$uv)],
        by = vars$y[2]
      )
    },
    y_fun = call2("Surv", rlang::splice(syms(vars$y))),
    strata_term = "strata(centre)",
    fit = coxph(reformulate(c(vars$x$mv, strata_term), y_fun), data),
    check = cox.zph(fit)
  )
})

### TBL ------------------------------------------------------------------------

get_tbl_coxph <- \(outcome) {
  model <- .model$coxph[[outcome]]

  surv_def <- df[[model$vars$y[1]]] |>
    var_label() |>
    str_remove(",\\s*mois$") |>
    str_replace("^.", str_to_lower)

  event_def <- df[[model$vars$y[2]]] |>
    var_label() |>
    str_replace("^.", str_to_lower)

  lst(
    uv = tbl_uvregression(
      data = model$data,
      y = !!model$y_fun,
      method = coxph,
      hide_n = TRUE,
      include = model$vars$x$uv,
      !!!model$args
    ),
    mv = tbl_regression(
      x = model$fit,
      !!!model$args
    )
  ) |>
    map(
      ~ . |>
        gtsum_format() |>
        add_global_p()
    ) |>
    tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
    add_note(
      vars = "age_incr",
      note = "Incrémentation par tranches de 5 ans."
    ) |>
    add_note(
      vars = "chir_complic_class_maj",
      note = paste(
        "Définie par un grade supérieur ou égal à III",
        "selon la classification de Clavien-Dindo."
      )
    ) |>
    tbl_format(
      note_global = paste(
        str_glue("Le critère de jugement est le {surv_def}."),
        "Les patients sans évènement sont censurés à la date de point du centre.",
        str_glue("Un hazard ratio > 1 est en faveur d'un risque plus élevé de {event_def}"),
        "comparé au groupe de référence."
      ),
      note_pvalue = paste(
        "Modèle de régression de Cox multivariable stratifié sur le centre,",
        str_glue("incluant {model$fit$nevent} évènements pour {model$fit$n} observations"),
        str_glue("complètes ({nrow(model$data) - model$fit$n} observations supprimées"),
        "pour cause de données manquantes)."
      ),
      width = 800
    )
}

tbl_coxph <- map(set_names(names(.model$coxph)), get_tbl_coxph)

### CHECK -------------------------------------------------------------------

.coxph_check_tbl <- map(.model$coxph, \(model) {
  labels <- model$vars$x$mv |>
    set_names() |>
    map_chr(~ var_label(df[[.x]])) |>
    c(GLOBAL = "Ensemble du modèle")

  model$check$table |>
    as_tibble(rownames = "term") |>
    transmute(
      variable = labels[term],
      chi2 = round(chisq, 2),
      ddl = df,
      p = style_pvalue(p, digits = 2)
    )
})

### OUTPUT ---------------------------------------------------------------------

easy_out_map(tbl_coxph)
