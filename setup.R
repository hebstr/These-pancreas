### SETUP ----------------------------------------------------------------------

library(conflicted)
library(tidyverse)
library(googlesheets4)
library(rlang)
library(broom)
library(labelled)
library(gtsummary)
library(ggsurvfit)
library(survival)
library(ggrepel)
library(patchwork)
library(Gmisc)
library(grid)
library(hebstr)

if (interactive()) {
  print(conflict_scout())
}

conflicts_prefer(dplyr::filter, .quiet = TRUE)

auto_exec("lib", quiet = TRUE)

lang_fr()

.log_dir <- fs::dir_create(here::here("logs"))

# system2("rv", "summary", stdout = here::here(.log_dir, "rv.txt"))

source("_extensions/hebstr/hebstr-doc/fonts/register.R")

set_opts(
  font = "Luciole",
  .default_font = "Helvetica"
)

opts <- get_opts()

.test_args <- all_tests("wilcox.test") ~ list(exact = FALSE)

update_geom_defaults("text", list(family = opts$font$alpha))

.fig_palette <- c(opts$color$base, opts$color$cold[2])

source("scripts/_common.R")
