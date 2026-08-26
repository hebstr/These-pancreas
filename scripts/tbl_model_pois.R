### DATA -----------------------------------------------------------------------

.model$pois <- lst(
  data = .model$default$data,
  vars = set_model_vars(
    y = "n_cures",
    x_uv = .model$default$vars$x$uv,
    x_mv_exclude = NULL
  ),
  args = .model$default$args,
  descr = \() {
    check_model_vars(
      data = data[, c(vars$y, vars$x$uv)]
    )
  },
  fit_with = \(family) {
    glm(
      formula = reformulate(vars$x$mv, vars$y),
      family = family,
      data = data
    )
  },
  fit = map(
    c(pois = "poisson", qpois = "quasipoisson"),
    fit_with
  )
)

### TBL ------------------------------------------------------------------------

.model_pois_fit <- .model$pois$fit$qpois

.model$pois$tbls <- lst(
  uv = tbl_uvregression(
    data = .model$pois$data,
    y = .model$pois$vars$y,
    method = glm,
    method.args = list(family = quasipoisson),
    hide_n = TRUE,
    include = .model$pois$vars$x$uv,
    !!!.model$pois$args
  ),
  mv = tbl_regression(
    x = .model_pois_fit,
    !!!.model$pois$args
  )
)

tbl_model_pois <- .model$pois$tbls |>
  map(
    ~ . |>
      gtsum_format(
        label_n = "N",
        stat_n = "{n_obs}",
        estim_acro = list(irr = "RM"),
        estim_label = list(irr = "rapport du nombre moyen de cures")
      ) |>
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
      "Le critère de jugement est le nombre total de cures de chimiothérapie",
      "reçues, toutes phases confondues, modélisé en comptage."
    ),
    note_pvalue = paste(
      "Modèle de régression de quasi-Poisson multivariable portant sur",
      str_glue("{nobs(.model_pois_fit)} observations complètes"),
      str_glue("({nrow(.model$pois$data) - nobs(.model_pois_fit)} observations"),
      "supprimées pour cause de données manquantes)."
    ),
    width = 750
  )

### CHECK ----------------------------------------------------------------------

.model$pois$check <- lst(
  fit = .model$pois$fit$pois,
  overdispersion = performance::check_overdispersion(fit),
  collinearity = performance::check_collinearity(fit),
  outliers = performance::check_outliers(fit),
  performance = performance::model_performance(fit)
)

.pois_check_label <- .model$pois$vars$x$mv |>
  set_names() |>
  map_chr(~ var_label(df[[.x]]))

.pois_check_vif <- .model$pois$check$collinearity |>
  as_tibble() |>
  transmute(
    variable = .pois_check_label[Term],
    vif = round(VIF, 2),
    vif_adj = round(SE_factor, 2),
    tolerance = round(Tolerance, 2)
  )

.pois_check_perf <- .model$pois$check$performance |>
  as_tibble() |>
  transmute(
    aic = round(AIC, 1),
    aicc = round(AICc, 1),
    bic = round(BIC, 1),
    r2 = round(R2_Nagelkerke, 3),
    rmse = round(RMSE, 2)
  )

.pois_check_cook <- lst(
  attrs = attributes(.model$pois$check$outliers),
  n = sum(attrs$data$Outlier),
  max = round(max(attrs$data$Distance_Cook), 3),
  threshold = round(attrs$threshold$cook, 2)
)

### OUTPUT ---------------------------------------------------------------------

easy_out(tbl_model_pois)
