### DATA -----------------------------------------------------------------------

df_flow <- sheets$exclusions |>
  select(centre, motif_exclusion) |>
  bind_rows(sheets$inclusions[, "centre"]) |>
  mutate(
    across(c(centre, motif_exclusion), as.numeric),
    motif_exclusion = replace_na(motif_exclusion, 0)
  ) |>
  set_value_labels(!!!dict$val, .strict = FALSE) |>
  modify_if(is.labelled, labelled::to_factor)

label_centre <- get_label(df_recode, centre)

label_exclus <- df_flow |>
  filter(motif_exclusion != 0) |>
  get_label(centre, motif_exclusion)

n_groupe <- fct_count(df_recode$groupe)

n <- lst(
  eligible = nrow(df_flow),
  exclus = sum(df_flow$motif_exclusion != 0),
  inclus = eligible - exclus,
  chimio_adj = n_groupe$n[1],
  chimio_periop = n_groupe$n[2]
)

### GROBS ----------------------------------------------------------------------

col_blue <- "#08F"
col_dark <- "#111"
col_grey <- "grey95"
base_fontsize <- 11
base_textcolor <- "#333"

options(
  connectGrob = gpar(col = "#09F", lwd = 1.5),
  connectGrobArrow = arrow(length = unit(0, "mm"))
)

txt_gp <- \(fontsize = base_fontsize, col = base_textcolor) {
  gpar(
    fontsize = fontsize,
    col = col,
    fontfamily = opts$font$alpha
  )
}

flow_box <- \(
  label,
  x,
  y,
  just = "center",
  col = col_blue,
  fill = "#FFF",
  lwd = 1.5,
  txt = txt_gp()
) {
  boxGrob(
    label = label,
    x = x,
    y = y,
    just = just,
    box_gp = gpar(fill = fill, col = col, lwd = lwd),
    txt_gp = txt
  )
}

eligible_box <- flow_box(
  str_glue(
    "Patients éligibles (N = {n$eligible})
    - Hommes et femmes >= 18 ans
    - Opérés d'un cancer du pancréas
    - Entre 01/2022 et 12/2025"
  ),
  x = 0.4,
  y = 0.9,
  just = "left",
  col = col_dark
)

exclus_box <- flow_box(
  str_glue("Exclus (n = {n$exclus})\n{label_exclus}"),
  x = 0.825,
  y = 0.6,
  just = "left",
  col = col_grey,
  fill = col_grey,
  lwd = 1.2,
  txt = txt_gp(base_fontsize - 1.5, col = "#555")
)

coords_exclus_box <- coords(exclus_box)
anchor_exclus_box <- boxGrob(
  label = "",
  x = coords_exclus_box$left,
  y = coords_exclus_box$y * 1.3,
  width = unit(0, "mm"),
  height = unit(0, "mm"),
  box_gp = gpar(col = NA, fill = NA)
)
anchor_exclus_connect <- connectGrob(
  eligible_box,
  anchor_exclus_box,
  type = "L",
  lty_gp = gpar(col = "grey90", lwd = 2.5)
)

inclus_box <- flow_box(
  str_glue("Inclus (n = {n$inclus})\n{label_centre}"),
  x = 0.4,
  y = 0.50,
  just = "left"
)
inclus_connect <- connectGrob(
  eligible_box,
  inclus_box,
  type = "vertical",
  lty_gp = gpar(col = base_textcolor, lwd = 1.5)
)

adjuvant_box <- flow_box(
  label = str_glue("Chimiothérapie adjuvante seule\n(n = {n$chimio_adj})"),
  x = 0.60,
  y = 0.15
)
adjuvant_connect <- connectGrob(inclus_box, adjuvant_box, type = "N")

periop_box <- flow_box(
  label = str_glue("Chimiothérapie péri-opératoire\n(n = {n$chimio_periop})"),
  x = 0.20,
  y = 0.15
)
periop_connect <- connectGrob(inclus_box, periop_box, type = "N")

### FLOWCHART ------------------------------------------------------------------

fig_flowchart <- grobTree(
  eligible_box,
  exclus_box,
  inclus_box,
  adjuvant_box,
  periop_box,
  anchor_exclus_connect,
  inclus_connect,
  adjuvant_connect,
  periop_connect
)

### OUTPUT ---------------------------------------------------------------------

easy_out(fig_flowchart, width = 9, height = 8)

svg_flowchart <- here::here("output/fig_flowchart.svg")

svg_flowchart |>
  read_file() |>
  str_replace(fixed("<svg "), "<svg xml:space='preserve' ") |>
  write_file(svg_flowchart)

svg_flowchart |>
  magick::image_read_svg(height = 1200) |>
  magick::image_write(here::here("output/fig_flowchart.png"), format = "png")

# export_office(fig_flowchart, width = 9, height = 8)
