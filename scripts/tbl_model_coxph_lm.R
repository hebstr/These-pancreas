### DATA ------------------------------------------------------------------------

.landmark <- 9

.model <- lst(
  data = df |>
    filter(os_tte >= .landmark) |>
    mutate(os_tte_lm = os_tte - .landmark),
  y = c("os_tte_lm", "os_event"),
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

.model$coxph <- lst(
  y = expr(Surv(!!!syms(.model$y))),
  fit = coxph(reformulate(.model$x$mv, y), .model$data)
)

### TBL ------------------------------------------------------------------------

.tbl_coxph_models <- lst(
  uv = tbl_uvregression(
    data = .model$data[c(.model$y, .model$x$uv)],
    y = !!.model$coxph$y,
    method = coxph,
    !!!.model$args,
    hide_n = TRUE
  ),
  mv = tbl_regression(
    x = .model$coxph$fit,
    !!!.model$args
  )
)

tbl_coxph_lm <- .tbl_coxph_models |>
  map(
    ~ . |>
      gtsum_format(
        ci = opts$ci,
        model_mv = .model$coxph$fit,
        label_header = opts$labs$header,
        estim_sep = opts$sep$int
      ) |>
      add_global_p() |>
      modify_table_body(
        ~ mutate(., across(c(estimate, ci), ~ if_else(n_event == 0, NA, .)))
      )
  ) |>
  tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
  gt_format(
    note_pvalue = str_glue(
      "Landmark analysis at {.landmark} months from diagnosis: only patients
      at risk at the landmark are included and the follow-up clock is reset to
      the landmark, conditioning on survival to that point to remove immortal
      time. Multivariable Cox regression model included
      {.model$coxph$fit$nevent} events for {.model$coxph$fit$n} complete
      observations ({nrow(.model$data) - .model$coxph$fit$n} observations
      deleted due to missing values). n_cures_outcome is classified on the final
      cure total, so this landmark removes early deaths but not person-time
      misclassification: it under-corrects the bias (see the time-dependent
      model)."
    )
  )

coxph_lm_ph <- cox.zph(.model$coxph$fit)
coxph_lm_ph_plot <- ggcoxzph(coxph_lm_ph)

easy_out(tbl_coxph_lm, width = 680)
