### DATA ------------------------------------------------------------------------

# date_cure12 simulée pour test !!!

.cox_td_data <- df |>
  mutate(
    date_cure12 = if_else(
      n_cures >= 12,
      pmin(
        date_chir + weeks(2 * pmax(12 - induc_nb, 1)),
        date_diag + days(floor(os_tte * 30.44)) - days(1)
      ),
      as.Date(NA)
    ),
    t_cross = time_length(interval(date_diag, date_cure12), "months")
  )

.model <- lst(
  data = .cox_td_data |>
    tmerge(.cox_td_data, id = rowname, death = event(os_tte, os_event)) |>
    tmerge(.cox_td_data, id = rowname, reached12 = tdc(t_cross)),
  y = c("tstart", "tstop", "death"),
  x = lst(
    uv = c(
      "reached12",
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
    data = .cox_td_data[c("os_event", setdiff(x$uv, "reached12"))],
    by = "os_event"
  )
)

.model$coxph <- lst(
  y = expr(Surv(!!!syms(.model$y))),
  fit = coxph(reformulate(.model$x$mv, y), .model$data)
)

### TBL ----------------------------------------------------------------------

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

tbl_coxph_td <- .tbl_coxph_models |>
  map(
    ~ . |>
      gtsum_format(
        ci = opts$ci,
        model_mv = .model$coxph$fit,
        label_header = opts$labs$header,
        estim_sep = opts$sep$int
      ) |>
      add_global_p()
  ) |>
  tbl_merge(tab_spanner = str_glue("**{opts$labs$spanner}**")) |>
  tbl_format(
    note_pvalue = str_glue(
      "Régression de Cox à covariable dépendante du temps : reached12 est une
        covariable variant dans le temps, 0 avant la 12e cure du patient et 1 à
        partir de date_cure12, ce qui supprime le biais de temps immortel en
        attribuant le person-time au bon état d'exposition. Le modèle incluait
        {.model$coxph$fit$nevent} évènements pour
        {length(unique(.model$data$rowname))} patients sur
        {nrow(.model$data)} intervalles à risque."
    )
  )

coxph_td_ph <- cox.zph(.model$coxph$fit)
coxph_td_ph_plot <- ggcoxzph(coxph_td_ph)

easy_out(tbl_coxph_td, width = 680)
