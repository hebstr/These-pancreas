.git_lines <- \(...) {
  suppressWarnings(
    system2(
      "git",
      shQuote(c("-C", here::here(), ...)),
      stdout = TRUE,
      stderr = FALSE
    )
  )
}

.git_state <- \(config = here::here("_quarto.yml")) {
  commit <- .git_lines("rev-parse", "--short", "HEAD")

  if (length(commit) == 0) {
    return(list(commit = NA_character_, uncommitted = NA))
  }

  generated <- c("logs", "output", yaml12::read_yaml(config)$project$`output-dir`)

  changed <- .git_lines(
    "status",
    "--porcelain",
    "--",
    ".",
    paste0(":(exclude)", generated)
  )

  list(
    commit = commit,
    uncommitted = if (is.null(attr(changed, "status"))) length(changed) > 0 else NA
  )
}

.sheets_state <- \(sheets) {
  list(
    source = attr(sheets, "provenance"),
    tabs = map(sheets, \(x) list(rows = nrow(x), hash = hash(x)))
  )
}

.svg_hashes <- \(dir = here::here("output")) {
  files <- fs::dir_ls(dir, recurse = TRUE, glob = "*.svg")

  as.list(set_names(hash_file(files), fs::path_rel(files, here::here())))
}

log_render <- \(
  pass,
  warnings,
  seconds,
  sheets = get("sheets", envir = globalenv()),
  file = here::here("logs", "render.jsonl")
) {
  entry <- list(
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    pass = pass,
    report = rmarkdown::metadata$date %||% NA_character_,
    git = .git_state(),
    data = .sheets_state(sheets),
    svg = .svg_hashes(),
    warnings = I(warnings),
    seconds = round(seconds, 1)
  )

  fs::dir_create(fs::path_dir(file))

  cat(
    jsonlite::toJSON(entry, auto_unbox = TRUE, na = "null", null = "null"),
    "\n",
    sep = "",
    file = file,
    append = TRUE
  )
}
