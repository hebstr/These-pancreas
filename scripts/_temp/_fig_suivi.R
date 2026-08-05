### DATA -----------------------------------------------------------------------

.origin <- sym("date_diag")
.tte <- sym("os_tte")
.event <- sym("os_event")

.suivi_data <- df |>
  mutate(
    fu_max = time_length(interval(!!.origin, date_lastnews), "months"),
    death = factor(!!.event, levels = c(0, 1), labels = c("Non", "Oui"))
  ) |>
  drop_na(!!.origin, !!.tte)

.suivi_labels <- .suivi_data |>
  slice_max(!!.origin, n = 1, by = centre) |>
  select(centre, !!.origin, fu_max)

.suivi_palette <- .suivi_data$centre |>
  levels() |>
  set_names(x = c("#999999", "#0099FF", "#D55E00", "#009E73"))

### FIGURE ---------------------------------------------------------------------

fig_suivi <-
  .suivi_data |>
  ggplot() +
  aes(x = !!.origin, y = !!.tte) +
  geom_line(
    mapping = aes(y = fu_max, group = centre, color = centre),
    linewidth = 0.4
  ) +
  geom_point(
    mapping = aes(shape = death, fill = centre),
    size = 1.5,
    alpha = 0.6,
    color = "#111"
  ) +
  geom_text_repel(
    data = .suivi_labels,
    mapping = aes(y = fu_max, label = centre, color = centre),
    size = 2.5,
    direction = "y",
    hjust = 0,
    nudge_x = 30,
    min.segment.length = Inf,
    family = opts$font$alpha,
    seed = 0
  ) +
  labs(
    x = "Date du diagnostic",
    y = "Durée depuis le diagnostic (mois)",
    shape = "Décès"
  ) +
  scale_x_date(
    date_breaks = "year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.14))
  ) +
  scale_y_continuous(n.breaks = 10) +
  scale_shape_manual(values = c(Non = 21, Oui = 24)) +
  scale_color_manual(
    values = .suivi_palette,
    aesthetics = c("color", "fill")
  ) +
  guides(
    color = "none",
    fill = "none",
    shape = guide_legend(override.aes = list(fill = "#111", alpha = 1))
  ) +
  theme_bar(grid = FALSE, legend_position = "top")

easy_out(fig_suivi, height = 4, width = 7)
