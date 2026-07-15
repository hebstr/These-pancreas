### DATA ------------------------------------------------------------------------

.model_coxph <- lst(
  data = df |>
    mutate(ps_diag = ps_diag |> fct_collapse("1-2" = c(1, 2))),
  y = c("os_tte", "os_event"),
  x = lst(
    uv = c(
      "groupe",
      "n_cures_outcome",
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
    data = data[, c(y[2], x$uv)],
    by = y[2]
  )
)

.model_coxph$coxph <- lst(
  y = expr(Surv(!!!syms(.model_coxph$y))),
  fit = coxph(reformulate(.model_coxph$x$mv, y), .model_coxph$data)
)

### TBL ------------------------------------------------------------------------

.tbl_coxph_models <- lst(
  uv = tbl_uvregression(
    data = .model_coxph$data[c(.model_coxph$y, .model_coxph$x$uv)],
    y = !!.model_coxph$coxph$y,
    method = coxph,
    !!!.model_coxph$args,
    hide_n = TRUE
  ),
  mv = tbl_regression(
    x = .model_coxph$coxph$fit,
    !!!.model_coxph$args
  )
)

tbl_coxph <- .tbl_coxph_models |>
  map(
    ~ . |>
      gtsum_format(
        ci = opts$ci,
        model_mv = .model_coxph$coxph$fit,
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
      {.model_coxph$coxph$fit$nevent} évènements pour {.model_coxph$coxph$fit$n}
      observations complètes ({nrow(.model_coxph$data) - .model_coxph$coxph$fit$n}
      observations supprimées pour cause de données manquantes)."
    )
  ) |>
  add_note(
    vars = "age_incr",
    note = "Incrémentation par tranches de 5 ans.",
  )

coxph_ph <- cox.zph(.model_coxph$coxph$fit)
coxph_ph_plot <- ggcoxzph(coxph_ph)

easy_out(tbl_coxph, width = 680)
