get_label <- \(data, ..., sort = TRUE, indent = "  ") {
  vars <- enquos(...)
  recurse <- \(data, vars, depth) {
    var <- vars[[1]]
    pad <- strrep(indent, depth)
    .n <- count(data, !!var, name = ".n", sort = sort)
    lines <- str_glue("{pad}- {.n[[1]]} (n = {.n$.n})")
    if (length(vars) == 1L) {
      return(str_flatten(lines, "\n"))
    }
    children <- map_chr(
      .n[[1]],
      \(value) recurse(filter(data, !!var == value), vars[-1], depth + 1)
    )
    str_flatten(str_glue("{lines}\n{children}"), "\n")
  }
  recurse(data, vars, 0)
}

auto_ceiling <- \(time, by) {
  seq(0, ceiling(time / by - 1e-3) * by, by = by)
}

ggcoxzph <- \(x) {
  x$y |>
    as_tibble() |>
    mutate(time = x$time) |>
    pivot_longer(
      cols = -time,
      names_to = "covariate",
      values_to = "residual"
    ) |>
    ggplot(aes(time, residual)) +
    geom_point(
      size = 0.8,
      alpha = 0.4,
      color = opts$color$warm[2]
    ) +
    geom_smooth(
      method = "lm",
      formula = y ~ splines::ns(x, df = 4),
      color = opts$color$cold[2],
      fill = opts$color$cold[2],
      alpha = 0.15,
      linewidth = 0.5
    ) +
    facet_wrap(
      facets = ~covariate,
      scales = "free_y"
    ) +
    labs(
      x = "Months",
      y = "Scaled Schoenfeld residual"
    )
}
