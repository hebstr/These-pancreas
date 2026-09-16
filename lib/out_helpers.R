export_tables <- \(
  qmd = .report_qmd(),
  dir = .output_dir(quarto, qmd),
  stem = "tables",
  crossref = .crossref(quarto),
  quarto = .inspect(qmd)
) {
  report <- .read_floats(qmd, "tbl")
  tables <- report$floats

  force(crossref)

  path <- .export_path(dir, report$date, stem)

  inline <- mget(ls(globalenv(), all.names = TRUE, pattern = "^\\."), globalenv())
  withr::defer(list2env(inline, globalenv()))

  withr::with_options(
    list(hebstr.docx = TRUE, easy_out.export = TRUE, easy_out.quiet = TRUE),
    auto_exec(include = "^tbl", quiet = TRUE)
  )

  ft <- imap(tables, \(x, i) {
    obj <- eval(x$expr, globalenv())

    if (!inherits(obj, "flextable")) {
      cli::cli_abort(c(
        "{.file {qmd}}: chunk {.val {x$label}} does not publish a table.",
        i = "{.code {as_label(x$expr)}} is {.obj_type_friendly {obj}}."
      ))
    }

    flextable::set_caption(
      obj,
      caption = str_glue("{.float_label(crossref, i)} {x$cap}") |>
        flextable::as_chunk(props = .caption_text()) |>
        flextable::as_paragraph(),
      fp_p = officer::fp_par(
        text.align = "center",
        padding.top = 12,
        padding.bottom = 12,
        padding.left = 3,
        padding.right = 3
      )
    )
  })

  ft |>
    unname() |>
    map(\(x) \(doc) flextable::body_add_flextable(doc, x, align = NULL)) |>
    .save_docx(path)

  cli::cli_alert_success("{length(ft)} table{?s} written to {.file {path}}.")

  invisible(path)
}

export_figures <- \(
  qmd = .report_qmd(),
  dir = .output_dir(quarto, qmd),
  stem = "figures",
  crossref = .crossref(quarto, type = "fig"),
  quarto = .inspect(qmd),
  section = .docx_section()
) {
  report <- .read_floats(qmd, "fig")
  figures <- report$floats

  force(crossref)

  path <- .export_path(dir, report$date, stem)

  pages <- imap(figures, \(x, i) {
    cap <- .split_caption(x$cap, x$label, qmd)
    top <- identical(x$location %||% crossref$location, "top")

    caption <- .figure_caption(
      str_glue("{.float_label(crossref, i)} {cap$title}"),
      cap$note,
      top = top,
      spacing = if (top) 12 else 6
    )

    image <- .figure_image(.figure_path(x, qmd), section, keep = !top)

    blocks <- if (top) c(caption, list(image)) else c(list(image), caption)

    \(doc) reduce(blocks, \(d, b) officer::body_add_fpar(d, b), .init = doc)
  })

  .save_docx(unname(pages), path, section)

  cli::cli_alert_success(
    "{length(pages)} figure{?s} written to {.file {path}}."
  )

  invisible(path)
}

.read_floats <- \(qmd, type = c("tbl", "fig")) {
  type <- arg_match(type)
  noun <- c(tbl = "tables", fig = "figures")[[type]]

  if (!fs::file_exists(qmd)) {
    cli::cli_abort("{.file {qmd}}: no such file to read the {noun} from.")
  }

  lines <- readLines(qmd)

  floats <- lines |>
    .chunk_lines(qmd) |>
    map(.chunk_float, qmd = qmd, type = type) |>
    compact()

  if (!length(floats)) {
    cli::cli_abort("{.file {qmd}}: no {.field {type}-} chunk to export.")
  }

  list(floats = floats, date = .front_date(lines, qmd))
}

.float_label <- \(crossref, i) {
  str_glue("{crossref$prefix}\u00a0{i}{crossref$delim}")
}

.export_path <- \(dir, date, stem) {
  fs::dir_create(dir)

  fs::path(dir, str_glue("{date}_{stem}"), ext = "docx")
}

.figure_path <- \(x, qmd) {
  path <- eval(x$call, globalenv())

  if (
    !inherits(path, "knit_image_paths") ||
      length(path) != 1 ||
      !identical(fs::path_ext(path), "png")
  ) {
    cli::cli_abort(c(
      "{.file {qmd}}: chunk {.val {x$label}} does not publish a single PNG figure.",
      i = "{.code {as_label(x$call)}} must resolve to an exported PNG outside HTML."
    ))
  }

  unclass(path)
}

.split_caption <- \(cap, label, qmd) {
  parts <- cap |>
    str_split_1("<br\\s*/?>") |>
    str_remove_all("</?span[^>]*>") |>
    str_squish()

  if (length(parts) > 2 || any(str_detect(parts, "<[^>]*>"))) {
    cli::cli_abort(c(
      "{.file {qmd}}: the {.field fig-cap} of {.val {label}} carries markup beyond a title and a note.",
      i = "A caption is read as {.fun str_fig} writes it, {.code title<br><span>note</span>}."
    ))
  }

  list(
    title = parts[[1]],
    note = if (length(parts) == 2 && nzchar(parts[[2]])) parts[[2]]
  )
}

# Mirrors the flextable caption hebstr writes, so both assembled files read alike.
.caption_text <- \(size = 10, bold = TRUE, color = "#111111") {
  officer::fp_text(
    font.family = "Aptos",
    font.size = size,
    bold = bold,
    italic = FALSE,
    color = color
  )
}

# Keeps one Image Caption style and reproduces by direct formatting what the report gets from Table Caption above a figure: centred, kept with the image.
.figure_caption <- \(title, note, top, spacing = 6) {
  par <- \(text, props, keep_next, before, after) {
    officer::ftext(text, props) |>
      officer::fpar(
        fp_p = officer::fp_par(
          text.align = if (top) "center" else "left",
          padding.top = before,
          padding.bottom = after,
          padding.left = 0,
          padding.right = 0,
          keep_with_next = keep_next,
          word_style = "Image Caption"
        )
      )
  }

  if (is.null(note)) {
    return(list(par(title, .caption_text(), top, spacing, spacing)))
  }

  list(
    par(title, .caption_text(), TRUE, spacing, 0),
    par(
      note,
      .caption_text(size = 9, bold = FALSE, color = "#555555"),
      top,
      0,
      spacing
    )
  )
}

.figure_image <- \(src, section, keep, max_height = 0.8, spacing = 12) {
  dims <- dim(png::readPNG(src))

  width <- section$page_size$width -
    section$page_margins$left -
    section$page_margins$right

  height <- width * dims[[1]] / dims[[2]]

  limit <- max_height *
    (section$page_size$height -
      section$page_margins$top -
      section$page_margins$bottom)

  scale <- min(1, limit / height)

  officer::fpar(
    officer::external_img(src, width = width * scale, height = height * scale),
    fp_p = officer::fp_par(
      text.align = "center",
      padding.top = spacing,
      padding.bottom = spacing,
      keep_with_next = keep
    )
  )
}

.docx_section <- \() officer::prop_section(type = "continuous")

.save_docx <- \(pages, path, section = .docx_section()) {
  pages |>
    seq_along() |>
    reduce(
      \(doc, i) {
        doc <- if (i > 1) officer::body_add_break(doc) else doc
        pages[[i]](doc)
      },
      .init = officer::read_docx()
    ) |>
    officer::body_set_default_section(section) |>
    print(target = path)

  invisible(path)
}

.chunk_lines <- \(lines, qmd) {
  open <- str_which(lines, "^```+\\s*\\{r[ ,}]")
  close <- str_which(lines, "^```+\\s*$")

  map(open, \(i) {
    end <- close[close > i][1]

    if (is.na(end)) {
      cli::cli_abort("{.file {qmd}}: unclosed code chunk at line {i}.")
    }

    if (end > i + 1) lines[(i + 1):(end - 1)] else character()
  })
}

.chunk_float <- \(x, qmd, type = c("tbl", "fig")) {
  type <- arg_match(type)

  chunk <- knitr::partition_chunk("r", x)

  label <- chunk$options$label

  if (
    !is_string(label) ||
      !str_starts(label, str_glue("{type}-")) ||
      str_starts(label, str_glue("{type}-anx-"))
  ) {
    return(NULL)
  }

  option <- str_glue("{type}-cap")

  cap <- chunk$options[[option]]

  if (is.expression(cap)) {
    cap <- tryCatch(
      eval(cap, globalenv()),
      error = \(e) {
        cli::cli_abort(
          c(
            "{.file {qmd}}: the {.field {option}} of {.val {label}} cannot be evaluated.",
            i = "A caption is built by a script before the report runs, never in its own chunk."
          ),
          parent = e
        )
      }
    )
  }

  if (!is_string(cap)) {
    cli::cli_abort(c(
      "{.file {qmd}}: chunk {.val {label}} carries no {.field {option}}.",
      i = "A float published by the report is captioned by its own chunk."
    ))
  }

  calls <- chunk$code |>
    parse(text = _) |>
    as.list() |>
    keep(\(e) is.call(e) && identical(e[[1]], quote(out_qmd)))

  if (length(calls) != 1) {
    cli::cli_abort(c(
      "{.file {qmd}}: chunk {.val {label}} holds {length(calls)} {.fun out_qmd} call{?s}.",
      i = "A {.field {type}-} chunk publishes exactly one output, by name."
    ))
  }

  list(
    label = label,
    cap = cap,
    location = chunk$options[[str_glue("{type}-cap-location")]],
    call = calls[[1]],
    expr = calls[[1]][[2]]
  )
}

.front_date <- \(lines, qmd) {
  fence <- str_which(lines, "^(---|\\.\\.\\.)\\s*$")

  date <- if (length(fence) >= 2 && fence[1] == 1) {
    yaml12::parse_yaml(lines[2:(fence[2] - 1)])$date
  }

  if (
    !is_string(date) ||
      !str_detect(date, "^\\d{4}-\\d{2}-\\d{2}$") ||
      is.na(as.Date(date, format = "%Y-%m-%d"))
  ) {
    cli::cli_abort(c(
      "{.file {qmd}}: {.field date} must hold an ISO date, {.val YYYY-MM-DD}.",
      i = "It names this output, as it names the rendered report."
    ))
  }

  date
}

.inspect <- \(qmd) {
  out <- suppressWarnings(
    system2("quarto", c("inspect", qmd), stdout = TRUE, stderr = FALSE)
  )

  if (!length(out) || !is.null(attr(out, "status"))) {
    cli::cli_abort(c(
      "{.code quarto inspect} failed on {.file {qmd}}.",
      i = "It resolves the output directory and the {.field crossref} of the theme."
    ))
  }

  jsonlite::fromJSON(out, simplifyVector = FALSE)
}

.output_dir <- \(quarto, qmd) {
  root <- quarto$project$dir %||% fs::path_dir(qmd)

  fs::path_abs(quarto$project$config$project$`output-dir` %||% root, start = root)
}

.crossref <- \(quarto, type = c("tbl", "fig"), format = "docx") {
  type <- arg_match(type)

  formats <- quarto$formats

  target <- formats |>
    keep(\(x) identical(x$identifier$`base-format`, format)) |>
    pluck(1) %||%
    pluck(formats, 1)

  if (is.null(target)) {
    cli::cli_abort(c(
      "{.code quarto inspect} names no format to caption from.",
      i = "The report's {.field format} front matter declares none."
    ))
  }

  list(
    prefix = target$metadata$crossref[[str_glue("{type}-title")]] %||%
      target$language[[str_glue("crossref-{type}-title")]] %||%
      c(tbl = "Table", fig = "Figure")[[type]],
    delim = target$metadata$crossref$`title-delim` %||% ":",
    location = target$metadata[[str_glue("{type}-cap-location")]] %||%
      c(tbl = "top", fig = "bottom")[[type]]
  ) |>
    map(\(x) str_replace_all(as.character(x), "\\\\(.)", "\\1"))
}

.report_qmd <- \(config = here::here("_quarto.yml")) {
  render <- yaml12::read_yaml(config)$project$render

  if (!is_string(render)) {
    cli::cli_abort(c(
      "{.file {config}}: {.field project.render} must list exactly one document.",
      i = "It names the report the Word exports read."
    ))
  }

  here::here(render)
}

auto_build <- \(
  docx = knitr::pandoc_to("docx"),
  quiet = isTRUE(getOption("knitr.in.progress")),
  qmd = .report_qmd()
) {
  withr::with_options(
    list(easy_out.quiet = quiet),
    auto_exec()
  )

  if (docx) {
    quarto <- .inspect(qmd)
    export_tables(qmd = qmd, quarto = quarto)
    export_figures(qmd = qmd, quarto = quarto)
  }
}
