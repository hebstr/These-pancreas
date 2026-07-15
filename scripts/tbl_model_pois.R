### DATA ------------------------------------------------------------------------

.model_pois <- lst(
  data = df |>
    mutate(ps_diag = ps_diag |> fct_collapse("1-2" = c(1, 2))),
  y = "n_cures",
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
    data = data[, c(y, x$uv)]
  ),
  fit = glm(
    formula = reformulate(x$mv, y),
    family = quasipoisson,
    data = data
  )
)

### TBL ----------------------------------------------------------------------

.pois_models <- lst(
  uv = tbl_uvregression(
    data = .model_pois$data[c(.model_pois$y, .model_pois$x$uv)],
    y = .model_pois$y,
    method = glm,
    method.args = list(family = quasipoisson),
    !!!.model_pois$args,
    hide_n = TRUE
  ),
  mv = tbl_regression(
    x = .model_pois$fit,
    !!!.model_pois$args
  )
)

tbl_model_pois <- .pois_models |>
  map(
    ~ . |>
      gtsum_format(
        ci = opts$ci,
        model_mv = .model_pois$fit,
        label_header = opts$labs$header,
        estim_sep = opts$sep$int,
        label_n = "N",
        stat_n = "{n_obs}",
        estim_acro = "IRR",
        estim_label = "rapport du nombre moyen"
      ) |>
      add_global_p()
  ) |>
  tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
  gt_format(
    note_global = str_glue(
      "Le critère de jugement est le nombre total de cures reçues,
      modélisé en comptage."
    ),
    note_pvalue = str_glue(
      "Modèle de régression de quasi-Poisson multivariable
      portant sur {nobs(.model_pois$fit)} observations complètes
      ({nrow(.model_pois$data) - nobs(.model_pois$fit)} observations
      supprimées pour cause de données manquantes)."
    )
  ) |>
  add_note(
    vars = "age_incr",
    note = "Incrémentation par tranches de 5 ans."
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

easy_out(tbl_model_pois, width = 680)
