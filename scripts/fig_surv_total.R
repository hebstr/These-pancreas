get_fig_surv <- \(data, outcome) {
  .surv_data <- data[["data"]]
  .labs <- .surv_labs[[outcome]]
  .surv_time <- .surv_data$tte$obs$time
  .surv_brks <- auto_ceiling(time = max(.surv_time), by = 6)
  .surv_med <- .surv_data$tte$median
  .surv_med_lab <- str_remove(data[["tbl_label"]], "^Probabilité de ")

  .surv_data$tte$model |>
    ggsurvfit() +
    add_confidence_interval(alpha = 0.08) +
    add_censor_mark(shape = "\U0131", size = 3) +
    add_quantile(
      y_value = 0.5,
      linetype = "dashed",
      linewidth = 0.3
    ) +
    annotate(
      geom = "label",
      label = str_glue(
        "Médiane de {.surv_med_lab}, mois {opts$ci$label}{opts$sep$int}{.surv_med}"
      ),
      x = 0,
      y = 0.03,
      size = 2.5,
      hjust = 0,
      vjust = 0,
      family = opts$font$alpha,
      fill = "white",
      border.colour = NA,
      label.padding = unit(0.15, "lines")
    ) +
    add_risktable(
      risktable_stats = c("{n.risk} ({cum.event}/{cum.censor})"),
      stats_label = c("Nombre à risque (évènements/censures)"),
      risktable_group = "risktable_stat",
      size = 2,
      family = opts$font$alpha,
      theme = theme_risktable()
    ) +
    add_risktable_strata_symbol(symbol = "") +
    scale_ggsurvfit(
      x_scales = list(
        name = .labs$x,
        breaks = .surv_brks,
        limits = range(.surv_brks, .surv_time),
        expand = expansion(mult = 0.03)
      ),
      y_scales = list(
        name = .labs$y,
        breaks = seq(0, 1, by = 0.2),
        labels = label_style_percent()
      )
    ) +
    theme_tte(vjust_y = 1.5)
}

fig_surv_total <- imap(.surv$total, get_fig_surv)

easy_out(fig_surv_total, height = 3.75, width = 7)
