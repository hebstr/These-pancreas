### DATA ------------------------------------------------------------------------

.model <- lst(
  data = df,
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
    mv = uv
  ),
  args = lst(
    pvalue_fun = opts$pvalue$format,
    exponentiate = TRUE
  ),
  descr = check_model_vars(
    data = data[, c(y, x$uv)],
    by = y
  ),
  sep = glm(
    formula = reformulate(x$mv, y),
    family = binomial,
    data = data,
    method = detectseparation::detect_separation
  ),
  fit = glm(
    formula = reformulate(x$mv, y),
    family = binomial,
    data = data,
    method = brglm2::brglmFit,
    type = "AS_mean"
  )
)

### TBL ----------------------------------------------------------------------

.plr_models <- lst(
  uv = tbl_uvregression(
    data = .model$data[c(.model$y, .model$x$uv)],
    y = .model$y,
    method = glm,
    method.args = list(
      family = binomial,
      method = brglm2::brglmFit,
      type = "AS_mean"
    ),
    !!!.model$args,
    hide_n = TRUE
  ),
  mv = tbl_regression(
    x = .model$fit,
    !!!.model$args
  )
)

tbl_model_plr <- .plr_models |>
  map(
    ~ . |>
      gtsum_format(
        ci = opts$ci,
        model_mv = .model$fit,
        label_header = opts$labs$header,
        estim_sep = opts$sep$int
      ) |>
      add_global_p(anova_fun = anova_wald)
  ) |>
  tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
  gt_format(
    note_pvalue = str_glue(
      "Multivariable Firth-penalized logistic regression (brglm2, bias-reduced
      score, equivalent to Jeffreys-prior penalization) included
      {sum(.model$fit$y)} events for {nobs(.model$fit)}
      complete observations ({nrow(.model$data) - nobs(.model$fit)}
      observations deleted due to missing values). Global p-values are Wald
      tests; confidence intervals are profile-likelihood."
    )
  )

easy_out(tbl_model_plr, width = 680)
