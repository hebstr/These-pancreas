### DATA ------------------------------------------------------------------------

.model$glm$pois <- lst(
  data = .model$default$data,
  vars = set_model_vars(
    y = "n_cures",
    x_uv = .model$default$vars$x$uv,
    x_mv_exclude = "chir_resec"
  ),
  args = .model$default$args,
  descr = \() {
    check_model_vars(
      data = data[, c(vars$y, vars$x$uv)],
    )
  },
  fit = glm(
    formula = reformulate(vars$x$mv, vars$y),
    family = quasipoisson,
    data = data
  )
)

### TBL ----------------------------------------------------------------------

.model$glm$pois$tbls <- lst(
  uv = tbl_uvregression(
    data = .model$glm$pois$data,
    y = .model$glm$pois$vars$y,
    method = glm,
    method.args = list(family = quasipoisson),
    hide_n = TRUE,
    include = .model$glm$pois$vars$x$uv,
    !!!.model$glm$pois$args
  ),
  mv = tbl_regression(
    x = .model$glm$pois$fit,
    !!!.model$glm$pois$args
  )
)

tbl_model_glm_pois <- .model$glm$pois$tbls |>
  map(
    ~ . |>
      gtsum_format(
        label_n = "N",
        stat_n = "{n_obs}",
        estim_acro = "IRR",
        estim_label = "rapport du nombre moyen"
      ) |>
      add_global_p()
  ) |>
  tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
  add_note(
    vars = "age_incr",
    note = "Incrémentation par tranches de 5 ans."
  ) |>
  tbl_format(
    note_global = str_glue(
      "Le critère de jugement est le nombre total de cures reçues,
      modélisé en comptage."
    ),
    note_pvalue = str_glue(
      "Modèle de régression de quasi-Poisson multivariable
      portant sur {nobs(.model$glm$pois$fit)} observations complètes
      ({nrow(.model$glm$pois$data) - nobs(.model$glm$pois$fit)} observations
      supprimées pour cause de données manquantes)."
    )
  )

# .pois_check <- glm(
#   formula = formula(.model_pois$fit),
#   family = poisson,
#   data = .model_pois$data
# )
# performance::check_overdispersion(.pois_check)
# performance::check_collinearity(.pois_check)
# performance::check_outliers(.pois_check)
# performance::model_performance(.pois_check)

easy_out(tbl_model_glm_pois, width = 680)
