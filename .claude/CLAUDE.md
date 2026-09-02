# Project instructions

R analysis project for a retrospective multicentre study on resected pancreatic cancer, producing a statistical analysis report as HTML. The report source sits at the repository root under a dated name, `<date>_rapport-stat.qmd`, one file per version; `_quarto.yml` picks it up through the glob `*_rapport-stat.qmd`, so the render list survives a re-dating. Refer to it as "the report", never by a filename that a new version invalidates.

Global conventions (tone, R idioms, gate ordering, prose rules) come from `~/.claude/CLAUDE.md` and `rules/r.md`. This file carries only what the repository does not state on its own.

## Data source

Data are stored in a Google Sheets workbook with three sheets, imported into a list `sheets`:

- **Sheet 1** (`variables`): data dictionary with variable names (`var_name`), human-readable labels (`var_label`), types (`type`), and coded values (`level`, format `code = label`)
- **Sheet 2** (`inclusions`): anonymized patient data (included cohort), all columns imported as character strings
- **Sheet 3** (`exclusions`): excluded patients

The sheet is read live on every run, so any figure quoted from a past run describes that draw, not the current cohort.

The import pipeline is already configured. `setup.R` attaches the packages, imports the three sheets, and builds the raw dataframe `df_init`, then the analysis-ready dataframe `df`. It is sourced explicitly: manually in an interactive session, or at the top of the report via `source("setup.R")` immediately before `auto_exec()`. `.Rprofile` only bootstraps `rv` and sets the gargle OAuth email.

Do not write import or setup code unless asked.

## What `df` contains

- All variables from the data dictionary, already labelled (variable labels and value labels applied via the `labelled` package)
- Numeric variables already coerced to `numeric`
- Date variables already parsed to `Date`
- `age`: age at diagnosis, computed from birth and diagnosis dates
- Factors derived from labelled integers via `labelled::to_factor()`
- Only variables with a defined label are kept

The main grouping variable is `groupe` (adjuvant only vs peri-operative). It codes the **planned strategy**, not the treatment received: patients in the adjuvant arm with zero cycles belong there, on an intention-to-treat basis.

## Project structure

```
setup.R            attaches packages, imports the sheets, builds df (sourced explicitly, before auto_exec)
lib/               project-local helpers, sourced by setup.R via auto_exec("lib")
lib/misc_helpers.R export_docx(), the only path to the Word tables: it sources the tbl_*.R scripts alone under hebstr.docx = TRUE. No caller in the repo, it is run by hand
scripts/           R scripts, one script per table or figure
scripts/var.R      variable dictionary and distribution tables, written to output/ only
output/            HTML/SVG and PNG exports, plus the JSON and XLSX get_vars_dict() feeds and the PPTX easy_out(pptx = TRUE) writes; one folder per output, kebab-named
<date>_rapport-stat.qmd  the report, the only Quarto document (renders to HTML)
_quarto.yml        project config: render list (glob *_rapport-stat.qmd), lang, date
.gitattributes     puts the HTML outputs and the report HTML through the gtid clean filter
```

New scripts go in `scripts/`. New outputs go in `output/`. Use `here::here()` for paths.

## Script execution

The report calls `auto_exec()` at render time, which sources every `.R` file in `scripts/` in alphabetical order **except** those whose name starts with `_` (`auto_exec()` defaults to `exclude = "^_"`, a regular expression matched on the filename).

The underscore prefix is the mechanism for making a script dormant while keeping it in the tree, and it is also the scope boundary of the report: a `_`-prefixed script is neither rendered, nor annexed, nor mentioned. No script is dormant today.

A script without an underscore runs on every render even if the report never displays its object: it still executes and `easy_out()` still writes to `output/`. A render is therefore a write operation on `output/`, not only a read.

Each non-dormant script is expected to produce a named object (e.g. `tbl_baseline`) that is then referenced in the Quarto document.

## Available packages

Attached by `setup.R`: `conflicted`, `tidyverse`, `googlesheets4`, `rlang`, `broom`, `labelled`, `gtsummary`, `ggsurvfit`, `survival`, `ggrepel`, `patchwork`, `Gmisc`, `grid`, `hebstr` (https://github.com/hebstr/hebstr, declared as a local path in `rproject.toml:10`, the git entry staying commented out on line 11).

Declared in `rproject.toml` and used via `::` without being attached: `fs` (`backup.R`), `performance` (`scripts/tbl_pois.R`), `car` (`scripts/tbl_coxph.R:63,80`, and the `pois-inline` chunk of the report, where `.pois_p()` calls `car::Anova(type = "III")`, which makes `car` a render dependency of the document and not only of the scripts), `cli` (`lib/model_helpers.R`, `lib/tbl_helpers.R`), `openxlsx2` (`backup.R`), `sessioninfo` and `gt` (the report), `knitr` (the report and `_extensions/hebstr/hebstr-doc/fonts/register.R:75`), `svglite` (set as the knitr device by the extension, `_extension.yml:55`, and called at `register.R:58`), `systemfonts` (`register.R:21,23`) and `xfun` (`register.R:54`), the last three reached through `setup.R:28` on every session and every render. `grid` is declared despite being a base-priority package.

`rvg` is declared but never called by the project: `easy_out(pptx = TRUE)` reaches it, and hebstr only carries it as a `Suggests`, so the entry has to stay. Packages hebstr carries as `Imports` arrive transitively and are deliberately **not** declared, whether the project never touches them (`officer`, `httpuv`, `broom.helpers`, `jsonlite`, `here`) or calls them by `::` like any declared one (`withr` in `lib/misc_helpers.R`): an undeclared `pkg::` call is the convention here, not an omission.

**Package-usage greps must run on the raw package name**, never on a `pkg::` or a call-shaped pattern. `tidycmprsk` once survived a removal audit wrongly, because `lib/model_helpers.R` reached it through strings that a lexical search does not match: `do.call("cuminc", ...)`, `do.call("crr", ...)`, `fun = "tbl_cuminc"`.

## hebstr helpers

Attached by `setup.R`.

```
| Function                               | Role                                                                                                                                                      |
| -------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `set_opts(.default_font, .vars_envir, .assign, .name, ...)` | Builds the project option set, assigned to `opts` by default. Table defaults (`opts$vars$stat`, `opts$digits`, `opts$labs$row_missing`) are entries of that set, passed through `...`, not parameters. The project calls it at `setup.R:30-33` with `font = "Luciole"` and `.default_font = "Helvetica"`; the global `opts` binding comes from `get_opts()` at `setup.R:35` |
| `lang_fr()`                            | Sets French locale for R and gtsummary (number formatting, labels)                                                                                        |
| `use_vars(data, .parametric, ...)`     | Caches a variable classification of `data` in the package store, for later use by the table helpers. Returns `data` unchanged: it selects nothing         |
| `gtsum_format()`                       | Applies standard formatting to a `gtsummary` table, summary or regression. Regression tables can pass the model arguments (`estim_acro`, `estim_label`, `label_reference`, `model_mv`, `adj_acro`, `bold_p`); the project only ever passes the first two, plus `label_n` and `stat_n`, at `tbl_pois.R:52-57`. Summary tables call it bare, and so does `tbl_coxph.R:118` |
| `easy_out()`                           | Saves a `ggplot`, `ggmatrix`, `gt`, `gtsummary` or `grid` grob object to `output/` as PNG, plus SVG or HTML depending on the type. A `wbWorkbook` goes to XLSX instead. `pptx = TRUE` adds an editable DrawingML slide beside the SVG, for a plot or a grob only, as in `fig_flowchart.R`. Each output gets a kebab-case folder of its own inside `output/`. Handed a named list, it writes one file per element into the shared folder, suffixed by the element name (`tbl_surv$os` becomes `output/tbl-surv/tbl-surv-os.*`). It writes nothing under `options(hebstr.docx = TRUE)`, its `export` default being `!.is_docx()` |
| `get_vars_dict(x, cols, font_size, font_family, strip_color, ...)` | Builds the variable dictionary of `x` (name, label, type, levels) as a formatted table. Called bare by `var.R:3`, exported to `output/var-dict/` as HTML, JSON and XLSX |
| `easy_check(.data, ..., .id, .with, .name, .na)` | Flags row-level inconsistencies: each `...` expression is a named logical test, the result is one row per `.id`×failed check, `.with` supplying the derived columns the tests need |
| `get_xlsx(sheets, ...)`                | Turns a named list of data frames into a `wbWorkbook`, which `easy_out()` then writes as XLSX                                                             |
| `with_fig_device(width, height, code)` | Evaluates `code` on a graphics device of the given size, so a `grid`/`Gmisc` grob is built with the geometry it will be exported at                        |
| `auto_exec(dir, include, exclude, ext, quiet)` | Sources the R scripts in `dir` (default `scripts/`) in alphabetical order. `include` and `exclude` are regular expressions matched on the filename: `exclude` defaults to `"^_"` and carries the underscore convention, `include` defaults to `NULL` and narrows the sweep to one family when set, as `export_docx()` does with `"^tbl"`. `exclude` applies after `include`, so a script matching both is skipped. `ext` defaults to `".R"`; `quiet = TRUE` silences the banner, as at `setup.R:24`               |
```

## What to produce

- **Descriptive tables**: `gtsummary`, exported via `hebstr::easy_out()`
- **Figures**: `ggplot2`, exported with `hebstr::easy_out()`

Default to the packages listed above. Only suggest an additional package if none of them can handle the task, and explain why it is necessary.

## Data rules

- Joins: always use explicit key columns, and check for NAs on the keys afterwards.
- Calculated approximations (age computed from a date difference, for instance): flag the approximation, do not auto-correct. The convention is usually intentional.
- Domain-specific variable codings and clinical rules: never rewrite or tighten without explicit confirmation.
- **`induc_adapt_pct` and `adj_adapt_pct` are collected, not derived**, and mean the **maximum dose adjustment across the three molecules, at the last cycle**: neither an average nor a relative dose over the whole phase. Level 2 is `>20%`, not `<=20%`. `setup.R` only consumes them. Since the `notes` column of `variables.xlsx` is `#N/A` on all 74 variables, the footnotes of `tbl_ttt_adj.R:26-29` and `tbl_ttt_induc.R:29-32` are the only place in the project that defines them: do not delete those notes as redundant. Background in point 33 of `NOTE-RAPPORT-STAT.md`.

## Project pitfalls

**A table footnote never contains a line break.** Any newline inside a footnote string, literal or interpolated, is swallowed **without a space** in the Word conversion, producing `classificationde Clavien-Dindo` or `95observations`. Beyond one line, build the note with `paste()` and one segment per line, each interpolation fitting on a single `str_glue()` line. HTML re-glues the space and shows nothing, so the check only happens on the `output/tbl-*/*.docx` files `export_docx()` writes. Figure labels are out of scope: there the break is wanted.

**A `Gmisc` grob freezes its geometry at construction, not at drawing.** `boxGrob()` queries the current device to convert text height, and `connectGrob(type = "N")` places its horizontal bar against that same device. A grob built in the Positron plot pane and exported on another canvas comes out deformed. `hebstr::with_fig_device(width, height, code)` is the fix; `scripts/fig_flowchart.R` declares the size once in `fig_size` (`:59`) and passes it to the wrapper (`:178-182`) then to `easy_out()`. The construction device must come from `svglite`, whose font metrics match the export; `pdf(NULL)` and `cairo_pdf` measure differently.

**Never pipe an `Rscript` that writes outputs into `grep` or `head`.** SIGPIPE kills R mid-write and leaves inconsistent files (HTML from one state, PNG from another), which yields contradictory checks on the same file.

**`gtsummary` pre-formats numeric fields before passing them to glue**, so a value arrives as `"34,0"` and any formatting function called inside a `statistic` template fails on receiving a string. Clean up afterwards with `modify_table_body()`.

**The `gtid` filter is not versioned, and two binaries escape it.** `.gitattributes` routes `output/**/*.html` and the report HTML through `filter=gtid`, whose driver lives in the local git config (`git config filter.gtid.clean`) and travels with the machine, not with the repository. It renumbers the random ten-letter ids `gt` assigns to its tables, so a re-render diffs on content alone; a clone without the driver sees every HTML change on every render. `git check-attr filter <file>` and `git config --get filter.gtid.clean` settle whether a diff is real. The PNG, SVG and JSON outputs regenerate byte-identically, but `output/fig-flowchart/fig-flowchart.pptx` and `output/var-dict/var-dict.xlsx` carry OOXML creation timestamps and so churn on every run: their diff never means the content moved.

## Gate

- After editing a `.qmd`: `panache` format, `quarto render`, `prose-lint`.
- After editing a script: `air format` then `jarl check`, then re-render.

## What not to touch

- Do not suggest modifying `.Rprofile`, `rproject.toml`, `rv.lock`, or any package management file.
- Do not suggest `install.packages()`: package management goes through `rv add`.
- Do not suggest modifying the Google Sheets import logic unless explicitly asked.
- Scripts are edited on explicit request only.

## Positron shortcuts (Linux)

```
| Shortcut           | Action                                        |
| ------------------ | --------------------------------------------- |
| `Ctrl+Shift+F10`   | Restart R session                             |
| `Ctrl+S`           | Save                                          |
| `Ctrl+Enter`       | Run current line/block or selection           |
| `Ctrl+Shift+Enter` | Run entire script                             |
| `Ctrl+Shift+M`     | Insert `|>` (native pipe)                     |
| `Ctrl+Shift+C`     | Comment / uncomment selected lines            |
| `Ctrl+Shift+K`     | Render the report                             |
| `Tab`              | Autocomplete                                  |
```

## Working files in `.claude/`

```
| File                   | Role                                                                 |
| ---------------------- | -------------------------------------------------------------------- |
| `CONTINUE.md`          | Session handoff: current state, settled scope rules, open items       |
| `DEFERRED.md`          | Deferred findings backlog                                             |
| `NOTE-RAPPORT-STAT.md` | Design note for the report: corpus, sourced guidelines, plan, point-by-point status (section 9) |
| `MODEL_GLM.md`         | Audit of the count model on `n_cures`, including the ceiling asymmetry that decides the headline result (section 8) |
| `EVENTS.md`            | Survival endpoint definitions and immortal time bias                  |
| `LIMITES.md`           | Archived prose of the `## Limites` section, awaiting rewrite by the author |
| `exemples-rapports-stat/` | Corpus of 7 reference statistical reports (PDF), source of sections 1-2 of NOTE-RAPPORT-STAT |
```

Read `NOTE-RAPPORT-STAT.md` section 9 before touching the report: it carries the open and settled methodological points.

`.claude/archive/` holds settled designs and superseded instructions. **Nothing there describes the current state of the repository: read it for the reasoning behind a decision, never as a description of what the code does today.** It contains the two instruction files this one replaces, plus five notes on work already settled or code already deleted: `DOC_PLR.md` (Firth exploration, summarised in section 7 of `MODEL_GLM.md`, but it keeps the `method = "PLR"` trap that returns p-values of exactly 1.0), `RECO-FLOWCHART.md` (flowchart tool comparison, decided in favour of `Gmisc`), `NOTE-FLOWCHART-DEVICE.md` (the device-height measurements behind the `with_fig_device()` pitfall above), `NOTE-FLOWCHART-CHUNK.md` (documented alternative: drawing the grob in the chunk instead of including the SVG, nothing to do while the current state holds) and `PLAN-SWIM.md` (swimmer plot design, script deleted, integration withdrawn).
