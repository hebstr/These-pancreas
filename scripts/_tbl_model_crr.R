### DATA ------------------------------------------------------------------------

.model <- lst(
  data = df,
  y = c("pfs_tte", "pfs_cause"),
  x = lst(
    uv = c(
      "n_cures_outcome",
      "groupe",
      "centre",
      "chir_hosp",
      "age",
      # "ps_diag",
      # "ca_diag",
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
    by = y[2]
  )
)

.model$crr <- lst(
  y = expr(Surv(!!!syms(.model$y))),
  fit = crr(reformulate(.model$x$mv, y), .model$data)
)

### TBL ------------------------------------------------------------------------

.crr_models <- lst(
  uv = tbl_uvregression(
    data = .model$data[c(.model$y, .model$x$uv)],
    y = !!.model$crr$y,
    method = crr,
    !!!.model$args,
    hide_n = TRUE
  ),
  mv = tbl_regression(
    x = .model$crr$fit,
    !!!.model$args
  )
)

tbl_model_crr <- .crr_models |>
  map(
    ~ . |>
      gtsum_format(
        ci = opts$ci,
        model_mv = .model$crr$fit,
        label_header = opts$labs$header,
        estim_sep = opts$sep$int
      ) |>
      add_global_p()
  ) |>
  tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
  tbl_format(
    note_pvalue = str_glue(
      "Fine-Gray subdistribution hazard model for recurrence,
      treating death without recurrence as a competing risk
      ({.model$crr$fit$cmprsk$n} observations)."
    )
  )

easy_out(tbl_model_crr, width = 680)

### CHECK ----------------------------------------------------------------------

crr_zph <- \(data, y, x) {
  data <- .model$data |>
    select(all_of(c(.model$y, .model$x$mv))) |>
    drop_na()

  fg_model <- finegray(
    reformulate(".", str_glue("Surv({.model$y[[1]]}, {.model$y[[2]]})")),
    data = data,
    etype = levels(data[[.model$y[[2]]]])[2]
  )

  cox_model <- coxph(
    reformulate(.model$x$mv, "Surv(fgstart, fgstop, fgstatus)"),
    weights = fgwt,
    data = fg_model
  )

  cox.zph(cox_model)
}

crr_zph_plot <- .model$data |>
  crr_zph(y = .model$y, x = .model$x$mv) |>
  ggcoxzph()
