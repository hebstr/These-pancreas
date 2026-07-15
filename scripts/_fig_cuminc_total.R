get_fig_cuminc <- \(data) {
  .cmp_events <- levels(data[["vars"]][[2]])

  data$crr |>
    ggcuminc(outcome = tail(.cmp_events, 2)) +
    add_confidence_interval(alpha = 0.05) +
    add_censor_mark(shape = "\U0131", size = 3) +
    add_risktable(
      risktable_stats = "{n.risk} ({cum.event}/{cum.censor})",
      stats_label = "Nombre à risque (évènements/censures)",
      risktable_group = "risktable_stats",
      size = 2,
      family = opts$font$alpha,
      theme = theme_risktable()
    ) +
    add_risktable_strata_symbol(symbol = "\U2014", size = 8) +
    scale_ggsurvfit(
      x_scales = list(
        name = "Months since surgery",
        breaks = auto_ceiling(time = max(data$tte$obs$time), by = 6),
        limits = c(-0.8, max(data$tte$obs$time) * 1.02)
      ),
      y_scales = list(
        name = "Cumulative incidence (%)",
        breaks = seq(0, 1, by = 0.2),
        labels = label_style_percent()
      )
    ) +
    theme_tte() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.02, 0.95),
      legend.justification = c(0, 1),
      legend.key.width = unit(18, "pt"),
      legend.text = element_text(size = 7)
    )
}

fig_cuminc_total <- get_fig_cuminc(.cmp$total$data)

easy_out(fig_cuminc_total, height = 3.75, width = 7)
