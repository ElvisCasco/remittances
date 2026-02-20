library(duckdb)
library(readxl)
library(here)

FILE <- here("Project", "Ingresos de Divisas por Remesas Familiares_0303_Departamento_2017-2026_Municipios_2020_2026.xlsx")
cat("FILE:", FILE, "\n")
cat("exists:", file.exists(FILE), "\n")

con <- dbConnect(duckdb())
tryCatch(
  dbExecute(con, "INSTALL excel; LOAD excel;"),
  error = function(e) message("INSTALL/LOAD error: ", e$message)
)

sheets <- excel_sheets(FILE)
cat("Sheets:", paste(sheets, collapse = ", "), "\n")

# Escape single quotes in path for SQL
file_sql <- gsub("'", "''", FILE)

read_sheet_clean <- function(con, file_sql, sheet, max_scan = 20) {
  # Read everything as raw (no header)
  sql <- sprintf(
    "SELECT * FROM read_xlsx('%s', sheet='%s', header=false, stop_at_empty=false)",
    file_sql, sheet
  )
  raw <- tryCatch(
    dbGetQuery(con, sql),
    error = function(e) { message("  read error [", sheet, "]: ", e$message); NULL }
  )
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  # Find first row where >=2 cells are non-empty strings
  header_row <- 1L
  for (i in seq_len(min(max_scan, nrow(raw)))) {
    n_str <- sum(sapply(raw[i, ], function(v) {
      !is.na(v) && is.character(v) && nchar(trimws(v)) > 0
    }))
    if (n_str >= 2) { header_row <- i; break }
  }

  # Set column names from header row, slice data rows below it
  col_names <- trimws(as.character(unlist(raw[header_row, ])))
  df <- raw[(header_row + 1):nrow(raw), , drop = FALSE]
  names(df) <- col_names
  rownames(df) <- NULL

  # Drop fully empty rows/cols
  df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]
  df
}

dfs <- lapply(setNames(sheets, sheets), function(sheet) {
  cat("\n--- Sheet:", sheet, "---\n")
  df <- read_sheet_clean(con, file_sql, sheet)
  if (is.null(df)) return(NULL)
  cat("  shape:", nrow(df), "x", ncol(df), "\n")
  cat("  cols:", paste(names(df), collapse = ", "), "\n")
  df
})

dbDisconnect(con)
cat("\nDone. Sheets loaded:", paste(names(Filter(Negate(is.null), dfs)), collapse = ", "), "\n")
