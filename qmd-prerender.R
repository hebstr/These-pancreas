project <- yaml12::read_yaml(here::here("_quarto.yml"))$project
qmd <- project$render
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

report <- paste0(doc_date, "_", stem)

yaml12::write_yaml(
  value = list(`output-file` = report),
  path = here::here("_metadata.yml")
)

target <- paste(c(project$`output-dir`, paste0(report, ".html")), collapse = "/")

writeLines(
  c(
    "<!doctype html>",
    "<html lang=\"fr\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    sprintf("<meta http-equiv=\"refresh\" content=\"0; url=%s\">", target),
    sprintf("<link rel=\"canonical\" href=\"%s\">", target),
    "<title>Rapport d'analyse statistique</title>",
    "</head>",
    "<body>",
    sprintf("<p><a href=\"%s\">Rapport d'analyse statistique</a></p>", target),
    "</body>",
    "</html>"
  ),
  here::here("index.html")
)
