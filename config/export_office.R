### OFFICE EXPORT --------------------------------------------------------------

export_office <- function(
  x,
  filename = NULL,
  dir = getOption("easy_out.dir", default = "output"),
  width = 9,
  height = 8
) {
  if (is.null(filename)) {
    filename <- as_label(enexpr(x))
  }
  is_supported <- is_ggplot(x) || grid::is.grob(x)
  if (!is_supported) {
    cli::cli_abort(c(
      "{.arg x} must be a ggplot or grid grob object",
      i = "Received object of class: {.cls {class(x)}}"
    ))
  }

  fs::dir_create(dir)
  to_pptx <- fs::path(dir, filename, ext = "pptx")

  draw <- if (is_ggplot(x)) {
    \() print(x)
  } else {
    \() {
      grid::grid.newpage()
      grid::grid.draw(x)
    }
  }

  doc <- read_pptx() |>
    add_slide(layout = "Blank", master = "Office Theme")

  slide <- slide_size(doc)
  scale <- min(slide$width / width, slide$height / height)
  w <- width * scale
  h <- height * scale
  loc <- ph_location(
    left = (slide$width - w) / 2,
    top = (slide$height - h) / 2,
    width = w,
    height = h
  )

  doc |>
    ph_with(value = dml(code = draw()), location = loc) |>
    print(target = to_pptx)

  invisible(to_pptx)
}
