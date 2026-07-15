### DATA ------------------------------------------------------------------------

.model <- lst(
  data = df |>
    mutate(ps_diag = ps_diag |> fct_collapse("1-2" = c(1, 2))),
  y = c("os_tte", "os_event"),
  x = lst(
    uv = c(
      "groupe",
      "n_cures_outcome",
      "centre",
      "chir_hosp",
      "age_incr",
      "ps_diag",
      "ca_diag",
      "chir_resec"
    ),
    mv = c(
      "groupe",
      "n_cures_outcome",
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

tbl_coxph <- .tbl_coxph_models |>
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
      "Modèle de regression de Cox multivariable incluant
      {.model$coxph$fit$nevent} évènements pour {.model$coxph$fit$n}
      observations complètes ({nrow(.model$data) - .model$coxph$fit$n}
      observations supprimées pour cause de données manquantes)."
    )
  ) |>
  add_note(
    vars = "age_incr",
    note = "Par tranches de 5 ans.",
  )

coxph_ph <- cox.zph(.model$coxph$fit)
coxph_ph_plot <- ggcoxzph(coxph_ph)

easy_out(tbl_coxph, width = 680)
