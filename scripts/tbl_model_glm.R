### DATA ------------------------------------------------------------------------

.model <- lst(
  data = df |>
    mutate(ps_diag = ps_diag |> fct_collapse("1-2" = c(1, 2))),
  y = "n_cures_outcome",
  x = lst(
    uv = c(
      "groupe",
      "centre",
      "chir_hosp",
      "age_incr",
      "ps_diag",
      "ca_diag",
      "chir_resec"
    ),
    mv = c(
      "groupe",
      "centre",
      "chir_hosp",
      "age_incr",
      "ps_diag"
      # "ca_diag",
      # "chir_resec"
    )
  ),
  args = lst(
    pvalue_fun = opts$pvalue$format,
    exponentiate = TRUE
  ),
  descr = check_model_vars(
    data = data[, c(y, x$uv)],
    by = y
  ),
  fit = glm(
    formula = reformulate(x$mv, y),
    family = binomial,
    data = data
  )
)

### TBL ----------------------------------------------------------------------

.glm_models <- lst(
  uv = tbl_uvregression(
    data = .model$data[c(.model$y, .model$x$uv)],
    y = .model$y,
    method = glm,
    method.args = list(family = binomial),
    !!!.model$args,
    hide_n = TRUE
  ),
  mv = tbl_regression(
    x = .model$fit,
    !!!.model$args
  )
)

tbl_model_glm <- .glm_models |>
  map(
    ~ . |>
      gtsum_format(
        ci = opts$ci,
        model_mv = .model$fit,
        label_header = opts$labs$header,
        estim_sep = opts$sep$int
      ) |>
      add_global_p()
  ) |>
  tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
  gt_format(
    note_pvalue = str_glue(
      "Modèle de regression logistique multivariable incluant
      {sum(.model$fit$y)} évènements pour {nobs(.model$fit)}
      observations complètes ({nrow(.model$data) - nobs(.model$fit)}
      observations supprimées pour cause de données manquantes)."
    )
  ) |>
  add_note(
    vars = "age_incr",
    note = "Par tranches de 5 ans.",
  )

# performance::check_collinearity(.model$fit)
# performance::binned_residuals(.model$fit)
# performance::check_outliers(.model$fit)
# performance::model_performance(.model$fit)

easy_out(tbl_model_glm, width = 680)
