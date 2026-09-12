qmd <- yaml12::read_yaml(here::here("_quarto.yml"))$project$render
stem <- "rapport-stat"

lines <- readLines(qmd)
fence <- grep("^(---|\\.\\.\\.)\\s*$", lines)

if (length(fence) < 2 || fence[1] != 1) {
  cli::cli_abort("{.file {qmd}}: no YAML front matter to read the date from.")
}

doc_date <- yaml12::parse_yaml(lines[2:(fence[2] - 1)])$date

if (
  !rlang::is_string(doc_date) ||
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", doc_date) ||
    is.na(as.Date(doc_date, format = "%Y-%m-%d"))
) {
  cli::cli_abort(c(
    "{.file {qmd}}: {.field date} must hold an ISO date, {.val YYYY-MM-DD}.",
    i = if (is.null(doc_date)) {
      "The front matter carries no {.field date} field."
    } else {
      "Read: {.val {doc_date}}."
    }
  ))
}

yaml12::write_yaml(
  value = list(`output-file` = paste0(doc_date, "_", stem)),
  path = here::here("_metadata.yml")
)
