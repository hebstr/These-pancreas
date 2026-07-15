### DATA ------------------------------------------------------------------------

.model_glm <- lst(
  data = df |>
    mutate(ps_diag = ps_diag |> fct_collapse("1-2" = c(1, 2))),
  y = "n_cures_outcome",
  x = lst(
    uv = c(
      "groupe",
      "centre",
      "age_incr",
      "ps_diag",
      "chir_resec"
    ),
    mv = setdiff(uv, "chir_resec")
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
    data = .model_glm$data[c(.model_glm$y, .model_glm$x$uv)],
    y = .model_glm$y,
    method = glm,
    method.args = list(family = binomial),
    !!!.model_glm$args,
    hide_n = TRUE
  ),
  mv = tbl_regression(
    x = .model_glm$fit,
    !!!.model_glm$args
  )
)

tbl_model_glm <- .glm_models |>
  map(
    ~ . |>
      gtsum_format(
        ci = opts$ci,
        model_mv = .model_glm$fit,
        label_header = opts$labs$header,
        estim_sep = opts$sep$int
      ) |>
      add_global_p()
  ) |>
  tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
  gt_format(
    note_global = str_glue(
      "Le critère de jugement est la réalisation d'un schéma incomplet de
      chimiothérapie, défini par un nombre total de cures reçues
      strictement inférieur à 12."
    ),
    note_pvalue = str_glue(
      "Modèle de regression logistique multivariable incluant
      {sum(.model_glm$fit$y)} évènements pour {nobs(.model_glm$fit)}
      observations complètes ({nrow(.model_glm$data) - nobs(.model_glm$fit)}
      observations supprimées pour cause de données manquantes)."
    )
  ) |>
  add_note(
    vars = "age_incr",
    note = "Incrémentation par tranches de 5 ans.",
  )

# performance::check_collinearity(.model_glm$fit)
# performance::binned_residuals(.model_glm$fit)
# performance::check_outliers(.model_glm$fit)
# performance::model_performance(.model_glm$fit)

easy_out(tbl_model_glm, width = 680)
