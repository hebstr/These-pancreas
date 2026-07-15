get_fig_surv <- \(data, outcome) {
  .surv_data <- data[["data"]]
  .labs <- .surv_labs[[outcome]] %||%
    cli::cli_abort("No axis labels defined for outcome {.val {outcome}}.")

  .surv_data$tte$model |>
    ggsurvfit() +
    add_confidence_interval(alpha = 0.08) +
    add_censor_mark(shape = "\U0131", size = 3) +
    add_risktable(
      risktable_stats = c("n.risk", "{cum.event} ({cum.censor})"),
      stats_label = c("**Nombre à risque**", "**Évènements (Censures)**"),
      size = 2,
      family = opts$font$alpha,
      theme = theme_risktable()
    ) +
    scale_ggsurvfit(
      x_scales = list(
        name = .labs$x,
        breaks = auto_ceiling(time = max(.surv_data$tte$obs$time), by = 6),
        limits = c(0, max(.surv_data$tte$obs$time) * 1.02)
      ),
      y_scales = list(
        name = .labs$y,
        breaks = seq(0, 1, by = 0.2),
        labels = label_style_percent()
      )
    ) +
    theme_tte(vjust_y = 12.5)
}

fig_surv_total <- imap(.surv$total, get_fig_surv)

easy_out_map(fig_surv_total, height = 3.75, width = 7)
