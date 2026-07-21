### DATA ------------------------------------------------------------------------

.model$glm$bin <- lst(
  data = .model$default$data,
  vars = set_model_vars(
    y = "n_cures_outcome",
    x_uv = .model$default$vars$x$uv,
    x_mv_exclude = "chir_resec"
  ),
  args = .model$default$args,
  descr = \() {
    check_model_vars(
      data = data[, c(vars$y, vars$x$uv)],
      by = vars$y
    )
  },
  fit = glm(
    formula = reformulate(vars$x$mv, vars$y),
    family = binomial,
    data = data
  )
)

### TBL ----------------------------------------------------------------------

.model$glm$bin$tbls <- lst(
  uv = tbl_uvregression(
    data = .model$glm$bin$data,
    y = .model$glm$bin$vars$y,
    method = glm,
    method.args = list(family = binomial),
    hide_n = TRUE,
    include = .model$glm$bin$vars$x$uv,
    !!!.model$glm$bin$args
  ),
  mv = tbl_regression(
    x = .model$glm$bin$fit,
    !!!.model$glm$bin$args
  )
)

tbl_model_glm_bin <- .model$glm$bin$tbls |>
  map(
    ~ . |>
      gtsum_format() |>
      add_global_p()
  ) |>
  tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
  add_note(
    vars = "age_incr",
    note = "Incrémentation par tranches de 5 ans.",
  ) |>
  tbl_format(
    note_global = str_glue(
      "Le critère de jugement est la réalisation d'un schéma incomplet de
      chimiothérapie, défini par un nombre total de cures reçues
      strictement inférieur à 12."
    ),
    note_pvalue = str_glue(
      "Modèle de regression logistique multivariable incluant
      {sum(.model$glm$bin$fit$y)} évènements pour {nobs(.model$glm$bin$fit)}
      observations complètes ({nrow(.model$glm$bin$data) - nobs(.model$glm$bin$fit)}
      observations supprimées pour cause de données manquantes)."
    )
  )

performance::check_collinearity(.model$glm$bin$fit)
performance::binned_residuals(.model$glm$bin$fit)
performance::check_outliers(.model$glm$bin$fit)
performance::model_performance(.model$glm$bin$fit)

easy_out(tbl_model_glm_bin, width = 680)
