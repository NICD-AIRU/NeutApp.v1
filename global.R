# global.R — loaded once before ui/server
# Sets up rhandsontable stubs + SQLite database initialisation

if (!requireNamespace("rhandsontable", quietly = TRUE)) {
  renderRHandsontable <- function(...) shiny::renderUI({
    shiny::tags$div(
      class = "alert alert-info", style = "margin:10px;",
      shiny::icon("info-circle"),
      " Install rhandsontable for inline table editing: ",
      shiny::tags$code("install.packages('rhandsontable')")
    )
  })
  rHandsontableOutput <- function(id, ...) shiny::uiOutput(id)
} else {
  library(rhandsontable)
}

suppressMessages({
  library(DBI)
  library(RSQLite)
})

# ── Database path (created next to the app) ───────────────────────────────────
DB_PATH <- file.path(getwd(), "neut_assay_db.sqlite")

init_db <- function(path = DB_PATH) {
  con <- dbConnect(SQLite(), path)
  on.exit(dbDisconnect(con))

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS analysis_runs (
      run_id       INTEGER PRIMARY KEY AUTOINCREMENT,
      run_date     TEXT NOT NULL,
      analyst      TEXT,
      titer_id     INTEGER,
      plates       TEXT,
      n_plates     INTEGER,
      n_samples    INTEGER,
      n_titers     INTEGER,
      created_at   TEXT DEFAULT (datetime('now'))
    )")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS inhibition_records (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id        INTEGER REFERENCES analysis_runs(run_id),
      plate_id      TEXT,
      bnab_id       TEXT,
      virus_id      TEXT,
      sample_type   TEXT,
      dilution      REAL,
      rep1_RLU      REAL,
      rep2_RLU      REAL,
      perc_inhibition REAL,
      perc_std_dev  REAL
    )")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS titer_records (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id      INTEGER REFERENCES analysis_runs(run_id),
      plate_id    TEXT,
      bnab_id     TEXT,
      virus_id    TEXT,
      sample_type TEXT,
      K38         REAL,
      IC80        REAL
    )")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS control_records (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      run_id    INTEGER REFERENCES analysis_runs(run_id),
      plate_id  TEXT,
      control   TEXT,
      avg       REAL,
      sd        REAL,
      cv_pct    REAL
    )")

  invisible(path)
}

# Initialise on load
init_db()
