### INIT -----------------------------------------------------------------------

source("rv/scripts/rvr.R")
source("rv/scripts/activate.R")

options(
  repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"),
  tidyverse.quiet = TRUE,
  tidymodels.quiet = TRUE,
  openxlsx2.maxWidth = 60,
  digits = 3,
  gargle_oauth_email = Sys.getenv("GARGLE_OAUTH_EMAIL", unset = NA)
)

library(tidyverse)
library(googlesheets4)
library(labelled)
library(gtsummary)
library(hebstr)

no_conflict <- \() conflicted::conflicts_prefer(dplyr::filter(), .quiet = TRUE)
here_src <- \(file = "") here::here("scripts", file)

lang_fr()
set_opts()

### GOOGLE SHEETS IMPORT -------------------------------------------------------

url <- "https://docs.google.com/spreadsheets/d/1iL6LS6ZXJpctP8I0Rj0HLSVGK3KoClZcX1EcIOkduUI/edit?pli=1&gid=2126636750#gid=2126636750"

variables <- read_sheet(
  ss = url,
  sheet = "Variables"
)

recueil_ano <- read_sheet(
  ss = url,
  sheet = "Recueil anonymisé Nantes + LRSY",
  col_types = "c"
)

### GOOGLE SHEETS BACKUP -------------------------------------------------------

# fs::dir_create(".backup")
# openxlsx2::write_xlsx(variables, ".backup/variables.xlsx")
# openxlsx2::write_xlsx(recueil_ano, ".backup/recueil_ano.xlsx")
