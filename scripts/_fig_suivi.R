### DATA -----------------------------------------------------------------------

.origin <- sym("date_diag")
.tte <- sym("os_tte")
.event <- sym("os_event")

.suivi_data <- df |>
  mutate(
    date_lastnews = .lastnews[as.integer(centre)],
    fu_max = time_length(interval(!!.origin, date_lastnews), "months")
  ) |>
  drop_na(!!.origin, !!.tte)

.suivi_labels <- .suivi_data |>
  slice_max(!!.origin, n = 1, by = centre) |>
  select(centre, !!.origin, fu_max)

### FIGURE ---------------------------------------------------------------------

fig_suivi <-
  .suivi_data |>
  ggplot() +
  aes(x = !!.origin, y = !!.tte) +
  geom_line(
    mapping = aes(y = fu_max, group = centre, color = centre)
  ) +
  geom_point(
    mapping = aes(shape = !!.event, fill = centre),
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
    family = opts$font$alpha,
    seed = 0
  ) +
  labs(
    x = "Date du diagnostic",
    y = "Recul de suivi (mois)",
    shape = "Décès",
    color = "Centre"
  ) +
  scale_x_date(
    date_breaks = "year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(n.breaks = 10) +
  theme_bar(grid = FALSE)

easy_out(fig_suivi, width = 6.5)
