export_tables <- \(
  qmd = here::here("index.qmd"),
  dir = .output_dir(quarto, qmd),
  stem = "tables",
  crossref = .crossref(quarto),
  quarto = .inspect(qmd)
) {
  if (!fs::file_exists(qmd)) {
    cli::cli_abort("{.file {qmd}}: no such file to read the tables from.")
  }

  lines <- readLines(qmd)

  tables <- lines |>
    .chunk_lines(qmd) |>
    map(.chunk_table, qmd = qmd) |>
    compact()

  if (!length(tables)) {
    cli::cli_abort("{.file {qmd}}: no {.field tbl-} chunk to export.")
  }

  date <- .front_date(lines, qmd)

  force(crossref)

  fs::dir_create(dir)

  path <- fs::path(dir, str_glue("{date}_{stem}"), ext = "docx")

  inline <- mget(ls(globalenv(), all.names = TRUE, pattern = "^\\."), globalenv())

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
      str_glue("{crossref$prefix}\u00a0{i}{crossref$delim} {x$cap}")
    )
  })

  .save_docx(unname(ft), path)

  list2env(inline, globalenv())

  cli::cli_alert_success("{length(ft)} table{?s} written to {.file {path}}.")

  invisible(path)
}

.save_docx <- \(ft, path, section = officer::prop_section(type = "continuous")) {
  ft |>
    seq_along() |>
    reduce(
      \(doc, i) {
        doc <- if (i > 1) officer::body_add_break(doc) else doc
        flextable::body_add_flextable(doc, ft[[i]], align = NULL)
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

.chunk_table <- \(x, qmd) {
  chunk <- knitr::partition_chunk("r", x)

  label <- chunk$options$label

  if (
    !is_string(label) ||
      !str_starts(label, "tbl-") ||
      str_starts(label, "tbl-anx-")
  ) {
    return(NULL)
  }

  cap <- chunk$options[["tbl-cap"]]

  if (is.expression(cap)) {
    cap <- eval(cap, globalenv())
  }

  if (!is_string(cap)) {
    cli::cli_abort(c(
      "{.file {qmd}}: chunk {.val {label}} carries no {.field tbl-cap}.",
      i = "A table published by the report is captioned by its own chunk."
    ))
  }

  calls <- chunk$code |>
    parse(text = _) |>
    as.list() |>
    keep(\(e) is.call(e) && identical(e[[1]], quote(out_qmd)))

  if (length(calls) != 1) {
    cli::cli_abort(c(
      "{.file {qmd}}: chunk {.val {label}} holds {length(calls)} {.fun out_qmd} call{?s}.",
      i = "A {.field tbl-} chunk publishes exactly one table, by name."
    ))
  }

  list(label = label, cap = cap, expr = calls[[1]][[2]])
}

.front_date <- \(lines, qmd) {
  fence <- str_which(lines, "^(---|\\.\\.\\.)\\s*$")

  date <- if (length(fence) >= 2 && fence[1] == 1) {
    yaml12::parse_yaml(lines[2:(fence[2] - 1)])$date
  }

  if (!is_string(date) || !str_detect(date, "^\\d{4}-\\d{2}-\\d{2}$")) {
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

.crossref <- \(quarto, format = "docx") {
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
    prefix = target$metadata$crossref$`tbl-title` %||%
      target$language$`crossref-tbl-title` %||%
      "Table",
    delim = target$metadata$crossref$`title-delim` %||% ":"
  ) |>
    map(\(x) str_replace_all(as.character(x), "\\\\(.)", "\\1"))
}

auto_build <- \(
  docx = knitr::pandoc_to("docx"),
  quiet = isTRUE(getOption("knitr.in.progress"))
) {
  withr::with_options(
    list(easy_out.quiet = quiet),
    auto_exec()
  )

  if (docx) export_tables()
}
