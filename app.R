##############################################################################
#  Neutralisation Assay - Shiny Dashboard
#  * Processes luminometer plate data with helper file layout definitions
#  * Computes IC50/IC80 titers -- 4PL (drc LL.4) preferred; point-based log-linear fallback
#  * Generates XLSX workbook
##############################################################################

# Encoding guard: ensure UTF-8 locale for shinyapps.io (Linux servers)
if (.Platform$OS.type == "unix") {
  Sys.setlocale("LC_ALL", "C.UTF-8")
}

# Increase max upload size to 100 MB
options(shiny.maxRequestSize = 100 * 1024^2)

suppressMessages({
  library(shiny)
  library(shinydashboard)
  library(readxl)
  library(openxlsx)
  # Load only the tidyverse packages actually used (avoids memory crash on shinyapps.io)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(scales)
  library(DT)
  library(gridExtra)
  library(grid)
})

# Load drc gracefully - if unavailable, point-based fallback is used automatically
DRC_AVAILABLE <- tryCatch({
  suppressMessages(library(drc))
  TRUE
}, error = function(e) {
  message("[INFO] drc package not available - using point-based IC50/IC80 fallback only.")
  FALSE
})

# -- Colour palette ------------------------------------------------------------
SAMPLE_COLOURS <- c(
  P1="#F0C419", P2="#2196F3", P3="#9E9E9E", P4="#0D3B8E",
  P5="#E53935", P6="#43A047", P7="#8E24AA", P8="#FB8C00",
  P9="#00ACC1", P10="#6D4C41", P11="#F06292", P12="#546E7A"
)

FALLBACK_COLOURS <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00",
                      "#A65628","#F781BF","#999999","#1B9E77","#D95F02")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# -- Instrument plate-layout definitions ---------------------------------------
INSTRUMENT_LAYOUTS <- list(
  `victor-5` = list(sheet = "Plate_Page1", range = "A7:L14",  use_first = FALSE),
  synergy    = list(sheet = "Sheet1",      range = "C16:N23", use_first = FALSE),
  glomax     = list(sheet = "Results",     range = "F11:Q18", use_first = FALSE),
  ensight    = list(sheet = "Sheet1",      range = "B11:M18", use_first = TRUE)
)

# -- Utility functions ---------------------------------------------------------
normalize_instrument <- function(x) tolower(trimws(as.character(x)))

get_layout <- function(instrument) {
  layout <- INSTRUMENT_LAYOUTS[[normalize_instrument(instrument)]]
  if (is.null(layout)) {
    warning("Unknown instrument: ", instrument, " - defaulting to victor-5")
    layout <- INSTRUMENT_LAYOUTS[["victor-5"]]
  }
  layout
}

get_instrument_read_params <- function(instrument, file_stem = NULL) {
  layout <- get_layout(instrument)
  list(
    sheet     = if (isTRUE(layout$use_first)) 1L else layout$sheet,
    range     = layout$range,
    use_first = isTRUE(layout$use_first)
  )
}

get_plate_instrument <- function(helper_table, pid) {
  if (is.null(helper_table)) return("victor-5")
  rows <- helper_table[trimws(toupper(as.character(helper_table$plate_id))) ==
                          trimws(toupper(as.character(pid))), , drop = FALSE]
  if (nrow(rows) == 0 || !"instrument" %in% colnames(rows)) return("victor-5")
  inst_raw <- tolower(trimws(as.character(rows$instrument[1])))
  if (is.na(inst_raw) || nchar(inst_raw) == 0) return("victor-5")
  if (grepl("glomax",  inst_raw)) return("glomax")
  if (grepl("synergy", inst_raw)) return("synergy")
  if (grepl("ensight", inst_raw)) return("ensight")
  if (grepl("victor",  inst_raw)) return("victor-5")
  "victor-5"
}

col_letter_to_num <- function(s) {
  s <- toupper(s)
  sum(sapply(seq_len(nchar(s)), function(i)
    (utf8ToInt(substr(s,i,i)) - 64L) * 26L^(nchar(s)-i)))
}

col_to_letter <- function(n) {
  s <- ""
  while (n > 0) { s <- paste0(LETTERS[((n-1) %% 26)+1], s); n <- (n-1) %/% 26 }
  s
}

plate_to_excel_coords <- function(plate_row, plate_col, instrument) {
  layout <- get_layout(instrument)
  tl <- strsplit(layout$range, ":")[[1]][1]
  tl_col <- gsub("[0-9]", "", tl)
  tl_row <- as.integer(gsub("[A-Za-z]", "", tl))
  excel_row <- tl_row + (match(toupper(plate_row), LETTERS) - 1L)
  excel_col <- col_letter_to_num(tl_col) + (as.integer(plate_col) - 1L)
  paste0(col_to_letter(excel_col), excel_row)
}

# -- Shared well-range parser (used by heatmap, plate map, and XLSX export) ----
# Parses range strings like "1:2", "A1:H12", "3,5:7" into a data.frame(r, c)
parse_wells <- function(rstr, n_r, n_c) {
  rstr <- trimws(as.character(rstr))
  if (is.na(rstr) || nchar(rstr) == 0)
    return(data.frame(r = integer(0), c = integer(0)))

  lr <- function(s) {
    s <- toupper(trimws(s))
    sum(sapply(seq_len(nchar(s)), function(i)
      (utf8ToInt(substr(s,i,i)) - 64L) * 26L^(nchar(s)-i)))
  }
  pc <- function(cell) {
    cell <- trimws(toupper(cell))
    m <- regmatches(cell, regexpr("^([A-Z]+)([0-9]+)$", cell))
    if (length(m) == 0 || nchar(m) == 0) return(NULL)
    list(row = lr(gsub("[0-9]","",m)), col = as.integer(gsub("[A-Z]","",m)))
  }

  # Numeric-only range: "1:2" or "3,5:7"
  if (isTRUE(grepl("^[0-9]+(:[0-9]+)?(,[0-9]+(:[0-9]+)?)*$", rstr))) {
    tokens <- strsplit(rstr, ",")[[1]]; col_ids <- integer(0)
    for (tok in tokens) {
      p <- as.integer(strsplit(trimws(tok),":")[[1]])
      col_ids <- c(col_ids, seq(min(p), max(p)))
    }
    return(expand.grid(r = seq_len(n_r), c = sort(unique(col_ids))))
  }

  # Cell-ref range: "A1:H12", "A1,B3:C5"
  tokens <- strsplit(rstr, ",")[[1]]; res <- list()
  for (tok in tokens) {
    tok <- trimws(tok)
    if (isTRUE(grepl(":", tok))) {
      pts <- strsplit(tok,":")[[1]]; if (length(pts) < 2) next
      c1 <- pc(trimws(pts[1])); c2 <- pc(trimws(pts[2]))
      if (is.null(c1) || is.null(c2)) next
      res[[length(res)+1]] <- expand.grid(
        r = seq(min(c1$row,c2$row), max(c1$row,c2$row)),
        c = seq(min(c1$col,c2$col), max(c1$col,c2$col)))
    } else {
      c1 <- pc(tok)
      if (!is.null(c1)) res[[length(res)+1]] <- data.frame(r=c1$row, c=c1$col)
    }
  }
  if (length(res) == 0) return(data.frame(r = integer(0), c = integer(0)))
  unique(bind_rows(res))
}

# =============================================================================
#  ANALYSIS FUNCTIONS
# =============================================================================

make_dilutions <- function(starting_conc, dil_factor, n = 8, sample_type = "mAb") {
  # Convention: position 1 (first in plate_range) = MOST DILUTE; last position = start_concentration.
  # This matches the physical plate layout: most diluted well loaded first.
  #
  # For mAb:   [sc/df^(n-1), ..., sc/df, sc]  -- ascending concentration
  #   On log x-axis: left = low conc (low inh) -> right = high conc (high inh)  up-right
  #
  # For Serum: [sc*df^(n-1), ..., sc*df, sc]  -- descending dilution denominator
  #   On log x-axis: left = low denominator (concentrated, high inh) -> right = high denom (diluted, low inh)  down-right
  sc <- as.numeric(starting_conc); df <- as.numeric(dil_factor)
  if (sample_type == "Serum") rev(sc * df^(0:(n-1))) else rev(sc / df^(0:(n-1)))
}

calc_inhibition <- function(rep1_vals, rep2_vals, virus_avg, cell_avg) {
  denom      <- virus_avg - cell_avg
  avg_sample <- (rep1_vals + rep2_vals) / 2
  # Clamp to [0, 1] -- matches corrected_percent_inhibition logic in reference Rmd
  perc_inh   <- pmin(1, pmax(0, (virus_avg - avg_sample) / denom))
  std_sample <- apply(cbind(rep1_vals, rep2_vals), 1, sd)
  perc_sd    <- std_sample / abs(denom)
  list(perc_inhibition = round(perc_inh, 6), perc_std_dev = round(perc_sd, 6))
}

# -- Outcome classification ----------------------------------------------------
# inhibitions_pct: vector in [0, 100] scale
# Returns: "FLAT" | "Titered" | "Not-Titered"
determine_outcome <- function(inhibitions_pct) {
  mx <- suppressWarnings(max(inhibitions_pct, na.rm = TRUE))
  mn <- suppressWarnings(min(inhibitions_pct, na.rm = TRUE))
  if (!is.finite(mx)) return("FLAT")
  if (mx < 50) return("FLAT")
  if (mn > 50) return("Not-Titered")
  "Titered"
}

# -- Point-based log-linear interpolation -------------------------------------
# Matches calculate_ic_log_interpolation() from the reference Rmd.
# Used as primary method and as 4PL fallback.
# dilutions: numeric > 0; inhibitions_pct: [0,100]; target_pct: e.g. 50 or 80
calc_ic_pointbased <- function(dilutions, inhibitions_pct, target_pct) {
  keep <- !is.na(dilutions) & dilutions > 0 &
          !is.na(inhibitions_pct) & is.finite(inhibitions_pct)
  dilutions <- dilutions[keep]; inhibitions_pct <- inhibitions_pct[keep]
  if (length(dilutions) < 2) return(NA_real_)
  if (target_pct < min(inhibitions_pct) || target_pct > max(inhibitions_pct)) return(NA_real_)

  # Sort by log10(dilution) ascending. After sorting:
  #   mAb   - inhibition is ascending  (low conc to high conc)
  #   Serum - inhibition is descending (low denom/concentrated to high denom/diluted)
  # Scan for the first pair of ADJACENT points that straddle the target.
  # The old max(below)/min(above) approach assumed inhibition was always ascending
  # and for Serum returned non-adjacent endpoints spanning the whole curve.
  df <- data.frame(lc = log10(dilutions), inh = inhibitions_pct) %>% arrange(lc)
  n  <- nrow(df)
  lo <- NA_integer_; hi <- NA_integer_
  for (i in seq_len(n - 1L)) {
    a <- df$inh[i]; b <- df$inh[i + 1L]
    if ((a <= target_pct && b >= target_pct) ||
        (a >= target_pct && b <= target_pct)) {
      lo <- i; hi <- i + 1L; break
    }
  }
  if (is.na(lo)) return(NA_real_)
  if (df$inh[hi] == df$inh[lo]) return(NA_real_)
  round(10^(df$lc[lo] + (target_pct - df$inh[lo]) *
              (df$lc[hi] - df$lc[lo]) / (df$inh[hi] - df$inh[lo])), 4)
}

# -- 4PL dose-response fit (drc LL.4) -----------------------------------------
# Returns list: converged, ic50_4pl, ic80_4pl, slope, lower, upper
# Uses drc::LL.4 -- the standard log-logistic 4-parameter model.
# IC50/IC80 derived via drc::ED(type="absolute") = conc at which inhibition = 50/80%.
fit_4pl <- function(dilutions, inhibitions_pct) {
  empty <- list(converged = FALSE, ic50_4pl = NA_real_, ic80_4pl = NA_real_,
                slope = NA_real_, lower = NA_real_, upper = NA_real_)
  if (!exists("DRC_AVAILABLE") || !DRC_AVAILABLE) return(empty)
  keep <- !is.na(dilutions) & dilutions > 0 &
          !is.na(inhibitions_pct) & is.finite(inhibitions_pct) &
          inhibitions_pct >= 0 & inhibitions_pct <= 100
  dilutions <- dilutions[keep]; inhibitions_pct <- inhibitions_pct[keep]
  if (length(dilutions) < 4) return(empty)
  tryCatch({
    m  <- drc::drm(inhibitions_pct ~ dilutions,
                   fct = drc::LL.4(names = c("Slope","Lower","Upper","IC50")))
    cf <- coef(m)
    ed <- tryCatch(
      suppressWarnings(drc::ED(m, c(50, 80), type = "absolute", display = FALSE)),
      error = function(e) NULL)
    # IC80: only report if the observed data actually reach 80% inhibition.
    # If max(inhibitions_pct) < 80 the 4PL model extrapolates beyond the data --
    # suppress the value to avoid reporting an unanchored estimate.
    max_inh_obs <- max(inhibitions_pct, na.rm = TRUE)
    list(
      converged = TRUE,
      ic50_4pl  = if (!is.null(ed) && is.finite(ed[1,1])) round(ed[1,1], 4) else NA_real_,
      ic80_4pl  = if (!is.null(ed) && is.finite(ed[2,1]) && max_inh_obs >= 80)
                    round(ed[2,1], 4) else NA_real_,
      slope     = round(cf["Slope:(Intercept)"], 4),
      lower     = round(cf["Lower:(Intercept)"], 4),
      upper     = round(cf["Upper:(Intercept)"], 4)
    )
  }, error = function(e) empty)
}


# -- 2D-aware well range helpers -----------------------------------------------

# Parse a single cell reference "A3" -> list(row = 1, col = 3); NULL on failure
parse_cell_ref <- function(ref) {
  ref <- toupper(trimws(as.character(ref)))
  m   <- regmatches(ref, regexec("^([A-Z]+)([0-9]+)$", ref))[[1]]
  if (length(m) < 3) return(NULL)
  letter <- m[2]
  row_i  <- sum(sapply(seq_len(nchar(letter)), function(i)
    (utf8ToInt(substr(letter, i, i)) - 64L) * 26L^(nchar(letter) - i)))
  list(row = row_i, col = as.integer(m[3]))
}

# Parse a range string "A3:H4" -> list with all bounds + geometry.
# The larger dimension is treated as the dilution axis; the smaller = replicates.
# Ties (e.g. single-row range) -> dilution by row.
parse_range_2d <- function(rstr) {
  rstr <- trimws(as.character(rstr))
  if (is.na(rstr) || nchar(rstr) == 0) return(NULL)
  pts <- strsplit(rstr, ":")[[1]]
  if (length(pts) != 2) return(NULL)
  c1 <- parse_cell_ref(pts[1]); c2 <- parse_cell_ref(pts[2])
  if (is.null(c1) || is.null(c2)) return(NULL)
  row1 <- min(c1$row, c2$row); row2 <- max(c1$row, c2$row)
  col1 <- min(c1$col, c2$col); col2 <- max(c1$col, c2$col)
  row_span <- row2 - row1 + 1L; col_span <- col2 - col1 + 1L
  dil_by_row <- row_span >= col_span          # rows = dilution axis
  list(row1 = row1, row2 = row2, col1 = col1, col2 = col2,
       row_span = row_span, col_span = col_span,
       dil_by_row = dil_by_row,
       n_dil = if (dil_by_row) row_span else col_span)
}

# Extract numeric values from raw_plate[row1:row2, col1:col2] as a flat vector
extract_wells <- function(raw_plate, row1, row2, col1, col2) {
  r_idx <- seq(row1, row2)
  c_chr <- as.character(seq(col1, col2))
  r_idx <- r_idx[r_idx >= 1L & r_idx <= nrow(raw_plate)]
  c_chr <- c_chr[c_chr %in% colnames(raw_plate)]
  if (!length(r_idx) || !length(c_chr)) return(numeric(0))
  as.numeric(as.matrix(raw_plate[r_idx, c_chr, drop = FALSE]))
}



# -- Full plate processor -----------------------------------------------------
process_plate_full <- function(raw_mat, pid, helper_table,
                               titer_id = 50, instrument = "victor-5") {
  plate_id <- pid

  # Build numeric plate matrix with letter row names (A-H) and numeric col names (1-12)
  raw_plate <- as.data.frame(raw_mat)
  raw_plate[] <- lapply(raw_plate, function(x) round(as.numeric(x)))
  rownames(raw_plate) <- LETTERS[seq_len(nrow(raw_plate))]
  colnames(raw_plate) <- as.character(seq_len(ncol(raw_plate)))

  plate_layout <- helper_table[trimws(toupper(as.character(helper_table$plate_id))) ==
                                trimws(toupper(as.character(pid))), , drop = FALSE]

  # Normalise concentration column name
  for (old_nm in c("dilution_start", "concentration_start")) {
    if (old_nm %in% colnames(plate_layout) && !"start_concentration" %in% colnames(plate_layout))
      colnames(plate_layout)[colnames(plate_layout) == old_nm] <- "start_concentration"
  }

  # -- Cell controls -----------------------------------------------------------
  distinct_cc_ranges <- unique(trimws(as.character(plate_layout$cell_control_range)))
  distinct_cc_ranges <- distinct_cc_ranges[!is.na(distinct_cc_ranges) & nchar(distinct_cc_ranges) > 0]
  cc_vals <- numeric(0)
  for (rstr in distinct_cc_ranges) {
    r2d <- parse_range_2d(rstr)
    if (!is.null(r2d))
      cc_vals <- c(cc_vals, extract_wells(raw_plate, r2d$row1, r2d$row2, r2d$col1, r2d$col2))
  }
  cell_avg <- mean(cc_vals, na.rm = TRUE)
  cell_sd  <- sd(cc_vals,   na.rm = TRUE)
  cell_ctrl <- list(avg = cell_avg, sd = cell_sd, n = length(cc_vals), ranges = distinct_cc_ranges)

  # -- Virus controls ----------------------------------------------------------
  viral_avgs <- list()
  for (v in unique(plate_layout$virus_id)) {
    vrows <- plate_layout[plate_layout$virus_id == v, , drop = FALSE]
    vc_rstr   <- trimws(as.character(vrows$virus_control_range[1]))
    ctrl_name <- paste0(v, " Virus Control")
    r2d <- parse_range_2d(vc_rstr)
    if (!is.null(r2d)) {
      vals <- extract_wells(raw_plate, r2d$row1, r2d$row2, r2d$col1, r2d$col2)
      viral_avgs[[ctrl_name]] <- list(avg = mean(vals, na.rm = TRUE),
                                       sd  = sd(vals,   na.rm = TRUE),
                                       n   = length(vals))
    }
  }
  virus_avg_vec <- sapply(viral_avgs, function(x) x$avg)

  # -- Inhibition & titers per experiment row ----------------------------------
  bnab_exps <- plate_layout %>%
    dplyr::select(sample_id, virus_id, experiment_id, plate_range, virus_control_range,
                  sample_type, start_concentration, dilution_factor) %>% distinct()

  inh_rows <- titer_rows <- list()

  for (k in seq_len(nrow(bnab_exps))) {
    row   <- bnab_exps[k, ]
    bnab  <- as.character(row$sample_id)
    virus <- as.character(row$virus_id)
    exp   <- as.character(row$experiment_id)
    stype <- as.character(row$sample_type)
    sc_v  <- as.numeric(row$start_concentration)
    df_v  <- as.numeric(row$dilution_factor)

    r2d <- parse_range_2d(as.character(row$plate_range))
    if (is.null(r2d)) next

    # Extract rep1 and rep2 along the dilution axis.
    # Convention: first position in range = most dilute; last = start_concentration.
    if (r2d$dil_by_row) {
      # Dilution steps run down rows; replicates are the two columns.
      row_idx   <- seq(r2d$row1, r2d$row2)
      rep1_vals <- as.numeric(raw_plate[row_idx, as.character(r2d$col1)])
      rep2_vals <- as.numeric(raw_plate[row_idx, as.character(r2d$col2)])
    } else {
      # Dilution steps run across columns; replicates are the two rows.
      col_chars <- as.character(seq(r2d$col1, r2d$col2))
      rep1_vals <- as.numeric(as.vector(unlist(raw_plate[r2d$row1, col_chars])))
      rep2_vals <- as.numeric(as.vector(unlist(raw_plate[r2d$row2, col_chars])))
    }

    if (!length(rep1_vals) || !length(rep2_vals)) next

    dilutions <- make_dilutions(sc_v, df_v, n = r2d$n_dil, sample_type = stype)

    ctrl_name <- paste0(virus, " Virus Control")
    virus_avg <- if (ctrl_name %in% names(virus_avg_vec)) virus_avg_vec[[ctrl_name]] else NA

    inh_out <- calc_inhibition(rep1_vals, rep2_vals, virus_avg, cell_avg)

    inh_rows[[k]] <- data.frame(
      plate_id = plate_id, sample_id = bnab, virus_id = virus, sample_type = stype,
      start_concentration = sc_v, dilution_factor = df_v,
      dilution = dilutions, rep1_RLU = rep1_vals, rep2_RLU = rep2_vals,
      perc_inhibition = inh_out$perc_inhibition, perc_std_dev = inh_out$perc_std_dev,
      stringsAsFactors = FALSE)

    # -- Unified IC calculation ------------------------------------------------
    inh_pct     <- inh_out$perc_inhibition * 100          # [0, 100] scale
    outcome_val <- determine_outcome(inh_pct)

    # Point-based IC (log-linear interpolation -- matches reference Rmd)
    ic50_pb <- calc_ic_pointbased(dilutions, inh_pct, 50)
    ic80_pb <- calc_ic_pointbased(dilutions, inh_pct, 80)

    # 4PL fit (only attempted for Titered curves)
    fit4 <- if (outcome_val == "Titered") {
      fit_4pl(dilutions, inh_pct)
    } else {
      list(converged = FALSE, ic50_4pl = NA_real_, ic80_4pl = NA_real_,
           slope = NA_real_, lower = NA_real_, upper = NA_real_)
    }

    # Best IC = 4PL if converged and finite, else point-based
    best_ic50 <- if (!is.na(fit4$ic50_4pl) && is.finite(fit4$ic50_4pl)) fit4$ic50_4pl else ic50_pb
    best_ic80 <- if (!is.na(fit4$ic80_4pl) && is.finite(fit4$ic80_4pl)) fit4$ic80_4pl else ic80_pb

    titer_rows[[k]] <- data.frame(
      plate_id = plate_id, sample_id = bnab, virus_id = virus, sample_type = stype,
      n_points            = r2d$n_dil,
      start_concentration = sc_v,
      ic50_pb             = ic50_pb,
      ic80_pb             = ic80_pb,
      ic50_4pl            = fit4$ic50_4pl,
      ic80_4pl            = fit4$ic80_4pl,
      slope_4pl           = fit4$slope,
      lower_4pl           = fit4$lower,
      upper_4pl           = fit4$upper,
      model_converged     = fit4$converged,
      outcome             = outcome_val,
      titer               = best_ic50,    # best available IC50 -- used by plots
      titer_id            = 50L,
      stringsAsFactors = FALSE
    )
  }

  list(
    renamed    = raw_plate,          # raw numeric plate (rows = A-H, cols = 1-12)
    cell_ctrl  = cell_ctrl,
    viral_avgs = viral_avgs,
    inhibition = bind_rows(inh_rows),
    titers     = bind_rows(titer_rows),   # unified: ic50_pb, ic80_pb, ic50_4pl, ic80_4pl, outcome, titer
    instrument = instrument
  )
}

# -- Shared ggplot theme for neutralisation curves -----------------------------
neut_theme <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(plot.title    = element_text(face = "bold", size = base_size + 1),
          plot.subtitle = element_text(colour = "grey40", size = 8),
          legend.position  = "right",
          legend.text      = element_text(size = 8),
          legend.title     = element_text(size = 9, face = "bold"),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_line(colour = "grey90", linewidth = 0.25),
          axis.text.x        = element_text(angle = 30, hjust = 1))
}

# Build series colour + shape maps from plot data
build_series_aesthetics <- function(plot_data) {
  all_series  <- unique(plot_data$series)
  pal_names   <- sub(":.*", "", all_series)
  colour_vals <- SAMPLE_COLOURS[pal_names]
  colour_vals[is.na(colour_vals)] <- rep_len(FALLBACK_COLOURS, sum(is.na(colour_vals)))
  colours <- setNames(as.character(colour_vals), all_series)

  shape_vals <- plot_data %>% dplyr::select(series, sample_type) %>% distinct() %>%
    mutate(sh = ifelse(sample_type == "Serum", 17L, 16L)) %>%
    { setNames(.$sh, .$series) }

  list(colours = colours, shapes = shape_vals)
}

# Core neut curve ggplot builder
build_neut_ggplot <- function(plot_data, titer_annot, colours, shape_vals,
                              titer_id, x_min, x_max, title, subtitle = NULL, x_label) {
  p <- ggplot(plot_data, aes(x = dilution, y = pct_neut, colour = series,
                              shape = series, group = series)) +
    geom_smooth(method = "loess", formula = y~x, se = FALSE, span = 0.75,
                linewidth = 0.9, show.legend = FALSE) +
    geom_line(linetype = "dotted", linewidth = 0.55, alpha = 0.75,
              show.legend = FALSE) +
    geom_point(size = 2.8, alpha = 0.92) +
    geom_hline(yintercept = titer_id, linetype = "dashed", colour = "grey25", linewidth = 0.5) +
    annotate("text", x = x_min*1.6, y = titer_id+4, label = "50%",
             hjust = 0, size = 2.8, colour = "grey25") +
    geom_hline(yintercept = 80, linetype = "dotted", colour = "grey40", linewidth = 0.5) +
    annotate("text", x = x_min*1.6, y = 84, label = "80%",
             hjust = 0, size = 2.6, colour = "grey40")

  maj_br <- 10^seq(floor(log10(x_min)), ceiling(log10(x_max)))
  min_br <- log_minor_breaks(x_min, x_max)

  p + scale_x_log10(limits       = c(x_min, x_max),
                     breaks       = maj_br,
                     minor_breaks = min_br,
                     labels       = smart_log_labels) +
    scale_y_continuous(limits = c(-5, 110), breaks = seq(0, 100, 25),
                       labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = colours, name = "Sample") +
    scale_shape_manual(values = shape_vals, name = "Sample") +
    guides(colour = guide_legend(override.aes = list(linewidth = 1.2, size = 3.5), ncol = 1),
           shape  = guide_legend(ncol = 1)) +
    labs(title = title, subtitle = subtitle, x = x_label, y = "Inhibition (%)") +
    neut_theme()
}

# Prepare plot data from inhibition + layout
prepare_neut_plot_data <- function(pid_inh, pid_layout) {
  bnab_order  <- unique(pid_layout$sample_id)
  pos_labels  <- setNames(paste0("S", seq_along(bnab_order)), bnab_order)
  pid_inh %>%
    filter(!is.na(dilution), dilution > 0, !is.na(perc_inhibition)) %>%
    mutate(
      pct_neut  = perc_inhibition * 100,
      pos_label = pos_labels[sample_id],
      series    = paste0(pos_label, ": ", sample_id, " / ", virus_id)
    )
}

get_x_label <- function(sample_types) {
  if (length(sample_types) == 1 && sample_types == "Serum") "Dilution"
  else if (length(sample_types) == 1 && sample_types == "mAb") "Concentration (\u03bcg/mL)"
  else "Concentration / Dilution"
}

# Tight x range: small padding (0.15 log units) so axes hug the actual data
get_x_range <- function(dilutions) {
  x_pos <- dilutions[is.finite(dilutions) & dilutions > 0]
  if (length(x_pos) == 0) return(c(0.001, 10000))
  lo <- log10(min(x_pos, na.rm = TRUE))
  hi <- log10(max(x_pos, na.rm = TRUE))
  c(10^(lo - 0.15), 10^(hi + 0.15))
}

# Smart x-axis labels: no "0" for sub-1 values, minimal decimals, comma thousands
smart_log_labels <- function(x) {
  sapply(x, function(v) {
    if (!is.finite(v) || v <= 0) return("")
    prettyNum(signif(v, 3), big.mark = ",")
  })
}

# Generate intermediate (2-9x) log10 minor break positions within [x_min, x_max]
log_minor_breaks <- function(x_min, x_max) {
  decades <- seq(floor(log10(x_min)) - 1L, ceiling(log10(x_max)) + 1L)
  mb <- as.vector(outer(2:9, 10^decades))
  sort(mb[mb > x_min & mb < x_max])
}

# -- Main neutralisation plot --------------------------------------------------
make_neut_plot_gg <- function(pid_inh, pid_titer, titer_id, pid_layout) {
  plot_data <- prepare_neut_plot_data(pid_inh, pid_layout)
  if (nrow(plot_data) == 0) return(NULL)

  aes_maps <- build_series_aesthetics(plot_data)
  bnab_order <- unique(pid_layout$sample_id)
  pos_labels <- setNames(paste0("S", seq_along(bnab_order)), bnab_order)

  titer_annot <- pid_titer %>% filter(!is.na(titer)) %>%
    mutate(pos_label   = pos_labels[sample_id],
           series      = paste0(pos_label, ": ", sample_id, " / ", virus_id),
           titer_label = sprintf("%s\nIC%d=%.1f", sample_id, titer_id, titer))

  xr <- get_x_range(plot_data$dilution)
  build_neut_ggplot(
    plot_data, titer_annot, aes_maps$colours, aes_maps$shapes,
    titer_id, xr[1], xr[2],
    title    = "Inhibition Curves",
    subtitle = "\u25CF = mAb   \u25B2 = Serum",
    x_label  = get_x_label(unique(plot_data$sample_type))
  )
}

# -- Per-plate inhibition curves split by sample_type --------------------------
make_inhibition_curves_plots <- function(results, helper_table, titer_id) {
  out <- list()
  for (pid in names(results)) {
    pid_inh    <- results[[pid]]$inhibition
    pid_titer  <- results[[pid]]$titers %>% mutate(plate_id = pid)
    pid_layout <- helper_table %>% filter(plate_id == pid)

    plot_data <- prepare_neut_plot_data(pid_inh, pid_layout)
    if (nrow(plot_data) == 0) { out[[pid]] <- list(serum = NULL, mab = NULL); next }

    aes_maps <- build_series_aesthetics(plot_data)
    bnab_order <- unique(pid_layout$sample_id)
    pos_labels <- setNames(paste0("S", seq_along(bnab_order)), bnab_order)

    titer_annot <- pid_titer %>% filter(!is.na(titer)) %>%
      mutate(pos_label   = pos_labels[sample_id],
             series      = paste0(pos_label, ": ", sample_id, " / ", virus_id),
             titer_label = sprintf("%s\nIC%d=%.1f", sample_id, titer_id, titer))

    xr <- get_x_range(plot_data$dilution)   # kept for reference; panels use their own sub_xr
    types_present <- unique(plot_data$sample_type)

    make_panel <- function(stype) {
      sub_data  <- plot_data  %>% filter(sample_type == stype)
      sub_annot <- titer_annot %>% filter(sample_id %in%
                     (pid_inh %>% filter(sample_type == stype) %>% pull(sample_id) %>% unique()))
      sub_series <- unique(sub_data$series)
      sub_xr <- get_x_range(sub_data$dilution)   # range from this panel's data only
      build_neut_ggplot(
        sub_data, sub_annot,
        aes_maps$colours[sub_series], aes_maps$shapes[sub_series],
        titer_id, sub_xr[1], sub_xr[2],
        title   = paste0(pid, " Inhibition Curves \u2014 ", stype),
        x_label = if (stype == "Serum") "Dilution" else "Concentration (\u03bcg/mL)"
      )
    }

    out[[pid]] <- list(
      serum = if ("Serum" %in% types_present) make_panel("Serum") else NULL,
      mab   = if ("mAb"   %in% types_present) make_panel("mAb")   else NULL
    )
  }
  out
}


# =============================================================================
#  UI
# =============================================================================
ui <- dashboardPage(
  skin = "blue",

  # Logo note: Shiny serves static files from a www/ folder placed next to this app file.
  # To display the AIRU logo, create: <app_directory>/www/airu_logo.png
  # The onerror handler hides the img gracefully if the file is missing.
  dashboardHeader(
    title = tags$span(
      tags$img(src = "airu_logo.png",
               height = "38px",
               style = "margin-right:8px; vertical-align:middle;",
               onerror = "this.style.display='none'"),
      "Neut Assay Dashboard"
    ),
    titleWidth = 340
  ),

  dashboardSidebar(
    width = 270,
    tags$head(tags$style(HTML("
      .skin-blue .main-header .logo { background-color:#1a3a6b; }
      .skin-blue .main-header .navbar { background-color:#1a3a6b; }
      .skin-blue .main-sidebar { background-color:#1e2d40; }
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li:hover > a { background:#2c4a7c; border-left:3px solid #F0C419; }
      .box.box-primary { border-top-color:#1a3a6b; }
      .box-header { background:#f7f9fc; }
      .shiny-notification { top:60px; right:10px; }
    "))),

    sidebarMenu(
      id = "tabs",
      menuItem("Overview",          tabName = "overview",  icon = icon("home")),
      menuItem("Helper Setup",      tabName = "setup",     icon = icon("cog")),
      menuItem("Plate Data Upload", tabName = "upload",    icon = icon("upload")),
      menuItem("Plate Review",      tabName = "controls",  icon = icon("flask")),
      menuItem("Inhibition Curves", tabName = "curves",    icon = icon("chart-line")),
      menuItem("Titer Results",     tabName = "titers",    icon = icon("table")),
      menuItem("Export",            tabName = "export",    icon = icon("download"))
    ),

    hr(),

    div(style = "padding:0 15px;",
      fileInput("helper_file", "Upload Helper File (.xlsx)",
                accept = c(".xlsx",".xls"), placeholder = "universal_helper_file.xlsx"),
      conditionalPanel("output.helper_loaded",
        tags$div(class = "alert alert-success", style = "padding:6px 10px; font-size:12px;",
          icon("check-circle"), " Helper file loaded"))
    ),

    div(style = "padding:0 15px;",
      h5("Run Parameters", style = "color:#aaa; font-size:12px; text-transform:uppercase; margin-top:10px;"),
      textInput("run_date", "Run Date (YYYYMMDD)",
                value = format(Sys.Date(), "%Y%m%d"), width = "100%"),
      textInput("user_name", "Analyst Name", placeholder = "First Last", width = "100%"),
      actionButton("run_analysis", "Run Analysis", class = "btn-primary btn-block",
                   style = "margin-top:8px;")
    )
  ),

  dashboardBody(
    tabItems(

      # -- OVERVIEW --------------------------------------------------------------
      tabItem("overview",
        fluidRow(
          valueBoxOutput("vb_plates",  width = 3),
          valueBoxOutput("vb_samples", width = 3),
          valueBoxOutput("vb_viruses", width = 3),
          valueBoxOutput("vb_titers",  width = 3)
        ),
        fluidRow(
          box(title = "Workflow Guide", width = 6, status = "primary", solidHeader = TRUE,
            tags$ol(style = "padding-left:20px; line-height:2;",
              tags$li("Upload your ", tags$b("Helper File"), " in the sidebar"),
              tags$li("Review the ", tags$b("Helper Setup"), " tab"),
              tags$li("Upload ", tags$b("plate .xlsx files"), " in the Plate Data tab"),
              tags$li("Click ", tags$b("Run Analysis")),
              tags$li("Explore ", tags$b("Inhibition Curves"), ", ", tags$b("Titer Results"), ", ", tags$b("Plate Review")),
              tags$li("Download XLSX from the ", tags$b("Export"), " tab")
            )
          ),
          box(title = "About This Dashboard", width = 6, status = "info", solidHeader = TRUE,
            tags$ul(style = "line-height:1.8;",
              tags$li("Range-based 2D plate layout (standard and split-virus plates)"),
              tags$li("Cell & virus control QC (mean & SD)"),
              tags$li("% Inhibition with rep1/rep2 SD"),
              tags$li("IC50/IC80: 4PL (drc LL.4) preferred; point-based log-linear fallback; outcome classified as Titered / FLAT / Not-Titered"),
              tags$li("Interactive neutralisation curves")
            )
          )
        ),
        fluidRow(
          box(title = "Recent Run Summary", width = 12, status = "warning", solidHeader = TRUE,
            uiOutput("run_summary_ui"))
        )
      ),

      # -- HELPER SETUP --------------------------------------------------------
      tabItem("setup",
        fluidRow(
          box(title = "Plate Setup (plate_setup sheet)", width = 12, status = "primary", solidHeader = TRUE,
            DTOutput("hot_experiments"))
        ),
        fluidRow(
          box(title = "Helper File Format", width = 6, status = "primary", solidHeader = TRUE,
            tags$p(style = "font-size:12px; color:#555;",
              "The ", tags$b("plate_setup"), " sheet defines all plate layouts. ",
              "Ranges such as ", tags$code("A3:H4"), " set the dilution axis: ",
              tags$b("first position = most dilute"), ", last position = ",
              tags$code("start_concentration"), ". Two columns = rep1/rep2."),
            DTOutput("hot_setup")),
          box(title = "Sample Types Available", width = 3, status = "info", solidHeader = TRUE,
            DTOutput("dt_sample_types")),
          box(title = "Plates Available", width = 3, status = "info", solidHeader = TRUE,
            DTOutput("dt_plate_numbers"))
        )
      ),

      # -- PLATE DATA UPLOAD ---------------------------------------------------
      tabItem("upload",
        fluidRow(
          box(title = "Upload Plate XLS/XLSX Files", width = 6, status = "primary", solidHeader = TRUE,
            fileInput("plate_files", "Select ALL plate files",
                      multiple = TRUE, accept = c(".xls",".xlsx",".XLS",".XLSX"),
                      placeholder = "Select multiple files..."),
            tags$div(class = "alert alert-info", style = "font-size:12px; padding:8px;",
              icon("info-circle"), tags$b(" Naming: "),
              tags$code("YYYYMMDD_Study_ScientistID_PlateID.(xls|xlsx)"),
              tags$br(), "4th token = plate_id (e.g. ",
              tags$code("20260304_RENEW_KP_P1.xls"), " -> ", tags$b("P1"), ")")
          ),
          box(title = "Uploaded Plates", width = 6, status = "info", solidHeader = TRUE,
            DTOutput("dt_uploaded_plates"), br(), uiOutput("upload_match_ui"))
        ),
        fluidRow(
          box(title = "Raw Plate Preview", width = 12, status = "info",
            selectInput("preview_plate", "Select plate to preview:", choices = NULL),
            DTOutput("dt_raw_preview"))
        )
      ),

      # -- INHIBITION CURVES ---------------------------------------------------
      tabItem("curves",
        fluidRow(
          box(width = 3, status = "primary",
            selectInput("curve_plate",  "Plate:",   choices = NULL),
            selectInput("curve_virus",  "Virus:",   choices = NULL),
            selectInput("curve_sample_type", "Sample Type:",
                        choices = c("All", "mAb", "Serum"), selected = "All"),
            selectInput("curve_sample", "Sample (sample_id):", choices = NULL, multiple = TRUE),
            hr(),
            downloadButton("dl_curve_png", "Save PNG", class = "btn-sm btn-default btn-block")
          ),
          box(width = 9, status = "primary", solidHeader = TRUE,
            title = uiOutput("curve_title"),
            uiOutput("neut_curve_plot_ui"))
        ),
        fluidRow(
          box(title = "Inhibition Data Table", width = 12, DTOutput("dt_inhibition")))
      ),

      # -- TITER RESULTS -------------------------------------------------------
      tabItem("titers",
        fluidRow(
          box(title = "IC50 & IC80 Titer Summary (long format)", width = 12,
              status = "primary", solidHeader = TRUE, collapsible = TRUE,
            p(style = "font-size:12px; color:#666;",
              "IC50_4PL / IC80_4PL = titer from 4-parameter logistic (4PL) fit -- primary method.  IC50_pb / IC80_pb = titer from point-based log-linear interpolation -- fallback when 4PL does not converge.  Converged = whether the 4PL model converged.  Outcome: Titered = IC threshold crossed; FLAT = max inhibition < 50%; Not-Titered = insufficient data to estimate titer."),
            DTOutput("dt_titers"))
        ),
        fluidRow(
          box(title = "Wide Titer Heatmap -- Virus x Sample", width = 12,
              status = "warning", solidHeader = TRUE,
            fluidRow(
              column(4,
                selectInput("wide_titer_metric", "Select metric:",
                            choices = c("IC50" = "IC50", "IC80" = "IC80"),
                            selected = "IC50", width = "100%")
              ),
              column(8,
                p(style = "font-size:12px; color:#555; margin-top:26px;",
                  "Rows = Virus | Columns = Sample | ",
                  tags$b("Geomean"), " = geometric mean across samples.")
              )
            ),
            DTOutput("dt_wide_titers"),
            br(),
            uiOutput("wide_titer_legend_ui")
          )
        )
      ),

      # -- PLATE REVIEW --------------------------------------------------------
      tabItem("controls",
        fluidRow(
          box(title = "Plate Layout & Plate Map", width = 12, status = "info", solidHeader = TRUE,
            selectInput("heatmap_plate", "Select Plate:", choices = NULL),
            plotOutput("rlu_heatmap", height = "440px"))
        ),
        fluidRow(
          box(title = "Control Summary -- Mean RLU (all plates)", width = 12,
              status = "primary", solidHeader = TRUE,
            DTOutput("dt_controls"))
        )
      ),

      # -- EXPORT --------------------------------------------------------------
      tabItem("export",
        fluidRow(
          box(title = "Export Results", width = 6, status = "primary", solidHeader = TRUE,
            h4("XLSX Workbook"),
            p("Sheets: 01_plate_setup, 02_raw_RLUs, 03_plate_maps, 04_plate_data, ",
              "05_control_summary, 06_percent_inhibition, 07_inhibition_curves, 07_titers, IC50_wide, IC80_wide."),
            downloadButton("dl_xlsx", "Download Results.xlsx", class = "btn-success btn-lg"),
            hr(),
            h4("Run Metadata"),
            verbatimTextOutput("export_meta")
          ),
          box(title = "Fields Legend -- 07_titers", width = 6, status = "info",
            tags$table(class = "table table-condensed", style = "font-size:12px;",
              tags$thead(tags$tr(tags$th("Field"), tags$th("Description"))),
              tags$tbody(
                tags$tr(tags$td("scientist_id"),    tags$td("Scientist identifier from the helper file")),
                tags$tr(tags$td("plate_id"),        tags$td("Plate identifier")),
                tags$tr(tags$td("experiment_date"), tags$td("Date of the experiment")),
                tags$tr(tags$td("sample_id"),       tags$td("Sample identifier")),
                tags$tr(tags$td("virus_id"),        tags$td("Virus identifier")),
                tags$tr(tags$td("sample_type"),     tags$td("Serum or mAb")),
                tags$tr(tags$td("n_points"),        tags$td("Number of dilution points used")),
                tags$tr(tags$td("outcome"),         tags$td("Titered = IC threshold crossed; FLAT = max inhibition < 50%; Not-Titered = insufficient data")),
                tags$tr(tags$td("ic50_pointbased"), tags$td("IC50 via point-based log-linear interpolation (fallback method)")),
                tags$tr(tags$td("ic80_pointbased"), tags$td("IC80 via point-based log-linear interpolation")),
                tags$tr(tags$td("model_converged"), tags$td("Whether the 4PL model converged (Yes / No)")),
                tags$tr(tags$td("slope_4pl"),       tags$td("Hill slope from 4PL fit")),
                tags$tr(tags$td("lower_4pl"),       tags$td("Lower asymptote from 4PL fit")),
                tags$tr(tags$td("upper_4pl"),       tags$td("Upper asymptote from 4PL fit")),
                tags$tr(tags$td("ic50_4pl"),        tags$td("IC50 from 4PL fit (primary method)")),
                tags$tr(tags$td("ic80_4pl"),        tags$td("IC80 from 4PL fit"))
              )
            )
          )
        )
      )
    )
  )
)

# =============================================================================
#  SERVER
# =============================================================================
server <- function(input, output, session) {

  rv <- reactiveValues(
    helper_table     = NULL,
    sample_types_df  = NULL,
    plate_numbers_df = NULL,
    plate_files_df   = NULL,
    raw_plates       = list(),
    results          = list(),
    analysis_done    = FALSE
  )

  # Helper: collect a field across all plate results
  collect_results <- function(field) {
    bind_rows(lapply(rv$results, `[[`, field))
  }

  # -- Helper file -------------------------------------------------------------
  observeEvent(input$helper_file, {
    req(input$helper_file)
    tryCatch({
      path        <- input$helper_file$datapath
      sheet_names <- readxl::excel_sheets(path)

      if (!"plate_setup" %in% sheet_names)
        stop("Sheet 'plate_setup' not found. Expected new helper file format.")

      helper_raw <- read_excel(path, sheet = "plate_setup")
      if (colnames(helper_raw)[1] %in% c("", "...1")) helper_raw <- helper_raw[, -1]
      helper_raw <- as.data.frame(helper_raw, stringsAsFactors = FALSE) %>%
        filter(!is.na(plate_id))

      # Normalise concentration column
      for (old_nm in c("dilution_start","concentration_start")) {
        if (old_nm %in% colnames(helper_raw) && !"start_concentration" %in% colnames(helper_raw))
          helper_raw <- helper_raw %>% rename(start_concentration = !!old_nm)
      }
      if (!"start_concentration" %in% colnames(helper_raw))
        stop("Helper file must have a 'dilution_start' or 'start_concentration' column.")
      helper_raw$start_concentration <- as.numeric(helper_raw$start_concentration)
      helper_raw$dilution_factor     <- as.numeric(helper_raw$dilution_factor)
      helper_raw$sample_id           <- as.character(helper_raw$sample_id)

      rv$helper_table    <- helper_raw
      rv$sample_types_df <- tryCatch(
        as.data.frame(read_excel(path, sheet = "sample_type"), stringsAsFactors = FALSE),
        error = function(e) NULL)
      rv$plate_numbers_df <- tryCatch(
        as.data.frame(read_excel(path, sheet = "plate_number"), stringsAsFactors = FALSE),
        error = function(e) NULL)

      showNotification(
        paste0("Helper file loaded: ", nrow(helper_raw), " rows, ",
               length(unique(helper_raw$plate_id)), " plate(s)."),
        type = "message", duration = 5)

    }, error = function(e) {
      showNotification(paste("Error loading helper file:", e$message),
                       type = "error", duration = 10)
    })
  })

  output$helper_loaded <- reactive({ !is.null(rv$helper_table) })
  outputOptions(output, "helper_loaded", suspendWhenHidden = FALSE)

  output$hot_experiments  <- renderDT({ req(rv$helper_table)
    datatable(rv$helper_table, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE)) })
  output$hot_setup <- renderDT({ req(rv$helper_table)
    datatable(rv$helper_table %>% dplyr::select(plate_id, sample_id, virus_id,
                plate_range, cell_control_range, virus_control_range,
                start_concentration, dilution_factor) %>% distinct(),
              rownames = FALSE, options = list(pageLength = 10, dom = "t", scrollX = TRUE)) })
  output$dt_sample_types  <- renderDT({ req(rv$sample_types_df)
    datatable(rv$sample_types_df, rownames = FALSE, options = list(pageLength = 10, dom = "t")) })
  output$dt_plate_numbers <- renderDT({ req(rv$plate_numbers_df)
    datatable(rv$plate_numbers_df, rownames = FALSE, options = list(pageLength = 15, dom = "t")) })

  # -- Plate upload ------------------------------------------------------------
  observeEvent(input$plate_files, {
    req(input$plate_files)
    finfo <- input$plate_files
    raw_list <- list(); rows <- list()

    for (i in seq_len(nrow(finfo))) {
      fname <- finfo$name[i]; fpath <- finfo$datapath[i]
      ext   <- tolower(tools::file_ext(fname))
      if (!ext %in% c("xls","xlsx")) {
        rows[[i]] <- data.frame(file = fname, plate_id = NA_character_,
                                rows = NA, cols = NA, status = "Skipped",
                                stringsAsFactors = FALSE); next
      }
      parts     <- strsplit(tools::file_path_sans_ext(fname), "_")[[1]]
      pid       <- if (length(parts) >= 4) trimws(toupper(parts[4])) else NA_character_
      file_stem <- tools::file_path_sans_ext(basename(fname))
      tryCatch({
        instrument <- if (!is.null(rv$helper_table) && !is.na(pid))
                        get_plate_instrument(rv$helper_table, pid) else "victor-5"
        rp  <- get_instrument_read_params(instrument, file_stem)
        mat <- tryCatch(
          read_excel(fpath, sheet = rp$sheet, range = rp$range,
                     col_types = "text", col_names = FALSE),
          error = function(e) tryCatch(
            read_excel(fpath, sheet = 1L, range = rp$range,
                       col_types = "text", col_names = FALSE),
            error = function(e2) read_excel(fpath, sheet = 1L, range = "A7:L14",
                                            col_types = "text", col_names = FALSE)))
        if (ncol(mat) > 0) colnames(mat) <- as.character(seq_len(ncol(mat)))

        exp_date <- if (length(parts) >= 1) {
          d <- trimws(parts[1])
          if (nchar(d) == 8 && grepl("^[0-9]{8}$", d))
            paste0(substr(d,1,4),"/",substr(d,5,6),"/",substr(d,7,8)) else d
        } else NA_character_
        scientist_id_f <- if (length(parts) >= 3) tolower(trimws(parts[3])) else NA_character_

        raw_list[[fname]] <- list(data = mat, plate_id = pid,
                                  experiment_date = exp_date,
                                  scientist_id = scientist_id_f)
        rows[[i]] <- data.frame(file = fname, plate_id = pid %||% "unknown",
                                experiment_date = exp_date %||% "unknown",
                                scientist_id = scientist_id_f %||% "unknown",
                                rows = nrow(mat), cols = ncol(mat), status = "Loaded",
                                stringsAsFactors = FALSE)
      }, error = function(e) {
        rows[[i]] <<- data.frame(file = fname, plate_id = pid %||% "unknown",
                                 experiment_date = NA_character_,
                                 scientist_id = NA_character_,
                                 rows = NA, cols = NA, status = paste("Error:", e$message),
                                 stringsAsFactors = FALSE)
      })
    }
    rv$raw_plates     <- raw_list
    rv$plate_files_df <- bind_rows(rows)
    plates_found <- unique(na.omit(rv$plate_files_df$plate_id))
    updateSelectInput(session, "preview_plate", choices = plates_found)
    showNotification(paste(nrow(finfo), "file(s) -",
                           sum(rv$plate_files_df$status == "Loaded"), "loaded."),
                     type = "message", duration = 5)
  })

  output$upload_match_ui <- renderUI({
    req(rv$plate_files_df)
    uploaded_ids <- unique(na.omit(rv$plate_files_df$plate_id))
    if (!length(uploaded_ids)) return(tags$div(class = "alert alert-warning", "No valid plate IDs."))
    helper_ids <- if (!is.null(rv$helper_table)) unique(rv$helper_table$plate_id) else character(0)
    matched   <- intersect(uploaded_ids, helper_ids)
    unmatched <- setdiff(uploaded_ids, helper_ids)
    no_file   <- setdiff(helper_ids, uploaded_ids)
    tags$div(
      if (length(matched) > 0)
        tags$div(class = "alert alert-success", style = "padding:7px; font-size:12px;",
          icon("check-circle"), tags$b(paste0(" Matched (", length(matched), "): ")),
          paste(matched, collapse = ", ")),
      if (length(unmatched) > 0)
        tags$div(class = "alert alert-warning", style = "padding:7px; font-size:12px;",
          icon("exclamation-triangle"), tags$b(" Not in helper: "), paste(unmatched, collapse = ", ")),
      if (length(no_file) > 0)
        tags$div(class = "alert alert-danger", style = "padding:7px; font-size:12px;",
          icon("times-circle"), tags$b(" No file for: "), paste(no_file, collapse = ", "))
    )
  })

  output$dt_uploaded_plates <- renderDT({ req(rv$plate_files_df)
    datatable(rv$plate_files_df, rownames = FALSE, options = list(pageLength = 10, dom = "t")) })

  output$dt_raw_preview <- renderDT({
    req(rv$raw_plates, input$preview_plate)
    match_entry <- Filter(function(x) identical(x$plate_id, input$preview_plate), rv$raw_plates)
    if (!length(match_entry)) return(NULL)
    datatable(as.data.frame(match_entry[[1]]$data), rownames = TRUE,
              options = list(dom = "t", scrollX = TRUE))
  })

  # -- Run Analysis ------------------------------------------------------------
  observeEvent(input$run_analysis, {
    req(rv$helper_table, length(rv$raw_plates) > 0)
    withProgress(message = "Running neutralisation analysis...", value = 0, {
      results <- list()
      plates  <- rv$raw_plates
      n       <- length(plates)
      for (idx in seq_along(plates)) {
        entry <- plates[[idx]]; pid <- entry$plate_id
        incProgress(1/n, detail = paste("Plate", pid))
        if (is.na(pid) || !pid %in% rv$helper_table$plate_id) next
        tryCatch({
          instrument_p <- get_plate_instrument(rv$helper_table, pid)
          results[[pid]] <- process_plate_full(entry$data, pid, rv$helper_table,
                                               50, instrument_p)
        }, error = function(e) {
          showNotification(paste("Plate", pid, ":", e$message), type = "error", duration = 10)
        })
      }
      rv$results       <- results
      rv$analysis_done <- TRUE
    })

    showNotification(paste("Analysis complete:", length(rv$results), "plate(s)."),
                     type = "message", duration = 6)

    plates_done <- names(rv$results)
    updateSelectInput(session, "curve_plate",   choices = c("All Plates", plates_done), selected = "All Plates")
    updateSelectInput(session, "heatmap_plate", choices = plates_done, selected = plates_done[1])

    all_inh <- collect_results("inhibition")
    if (nrow(all_inh) > 0) {
      all_viruses <- unique(all_inh$virus_id)
      updateSelectInput(session, "curve_virus", choices = c("All Viruses", all_viruses), selected = "All Viruses")
    }
  })

  # -- Curve filter cascade ----------------------------------------------------
  inh_for_cascade <- function() {
    if (identical(input$curve_plate, "All Plates"))
      collect_results("inhibition")
    else if (input$curve_plate %in% names(rv$results))
      rv$results[[input$curve_plate]]$inhibition
    else NULL
  }

  apply_curve_filters <- function(inh) {
    if (!identical(input$curve_virus, "All Viruses") && !is.null(input$curve_virus))
      inh <- inh %>% filter(virus_id == input$curve_virus)
    stype <- input$curve_sample_type %||% "All"
    if (!identical(stype, "All")) inh <- inh %>% filter(sample_type == stype)
    inh
  }

  observeEvent(input$curve_plate, {
    req(rv$results); inh <- inh_for_cascade(); req(!is.null(inh))
    updateSelectInput(session, "curve_virus",
                      choices = c("All Viruses", unique(inh$virus_id)), selected = "All Viruses")
  })

  observeEvent(c(input$curve_virus, input$curve_sample_type), {
    req(rv$results); inh <- inh_for_cascade(); req(!is.null(inh))
    inh <- apply_curve_filters(inh)
    updateSelectInput(session, "curve_sample",
                      choices = unique(inh$sample_id), selected = unique(inh$sample_id))
  })

  # -- Value boxes -------------------------------------------------------------
  output$vb_plates  <- renderValueBox({
    valueBox(length(rv$results), "Plates Processed", icon = icon("vials"), color = "blue") })
  output$vb_samples <- renderValueBox({
    n <- if (rv$analysis_done) length(unique(collect_results("inhibition")$sample_id)) else 0
    valueBox(n, "Unique Samples", icon = icon("flask"), color = "green") })
  output$vb_viruses <- renderValueBox({
    n <- if (rv$analysis_done) length(unique(collect_results("inhibition")$virus_id)) else 0
    valueBox(n, "Viruses Tested", icon = icon("virus"), color = "yellow") })
  output$vb_titers  <- renderValueBox({
    n <- if (rv$analysis_done) sum(!is.na(collect_results("titers")$titer)) else 0
    valueBox(n, paste0("IC", 50, " Titers (best available)"), icon = icon("chart-bar"), color = "red") })

  output$run_summary_ui <- renderUI({
    if (!rv$analysis_done) return(p("No analysis run yet."))
    all_t <- collect_results("titers")
    tags$div(
      tags$p(strong("Run date: "), input$run_date,
             tags$span(style = "margin-left:20px;"), strong("Analyst: "), input$user_name),
      tags$p(strong("Plates: "), paste(names(rv$results), collapse = ", ")),
      tags$p(strong(paste0("IC50 titers reached (best available): ")),
             sum(!is.na(all_t$titer)), " / ", nrow(all_t))
    )
  })

  # -- Neutralisation curve ----------------------------------------------------
  output$curve_title <- renderUI({
    plate_lbl <- input$curve_plate %||% "-"
    virus_lbl <- input$curve_virus %||% "-"
    stype <- input$curve_sample_type %||% "All"
    stype_label <- switch(stype,
      "mAb"   = "mAb - x: Concentration (ug/mL)",
      "Serum" = "Serum - x: Dilution",
      "All sample types")
    tags$span(paste0("Plate: ", plate_lbl, "  |  Virus: ", virus_lbl, "  |  ", stype_label))
  })

  selected_inh <- reactive({
    req(rv$results)
    inh <- if (identical(input$curve_plate, "All Plates")) {
      collect_results("inhibition")
    } else {
      req(input$curve_plate %in% names(rv$results))
      rv$results[[input$curve_plate]]$inhibition
    }
    if (!is.null(input$curve_virus) && !identical(input$curve_virus, "All Viruses") && nchar(input$curve_virus) > 0)
      inh <- inh %>% filter(virus_id == input$curve_virus)
    if (!is.null(input$curve_sample_type) && !identical(input$curve_sample_type, "All") && nchar(input$curve_sample_type) > 0)
      inh <- inh %>% filter(sample_type == input$curve_sample_type)
    if (!is.null(input$curve_sample) && length(input$curve_sample) > 0)
      inh <- inh %>% filter(sample_id %in% input$curve_sample)
    inh
  })

  output$neut_curve_plot_ui <- renderUI({
    req(rv$results)
    inh <- tryCatch(selected_inh(), error = function(e) NULL)
    if (is.null(inh) || nrow(inh) == 0) {
      tags$div(class = "alert alert-warning", style = "margin:20px; padding:14px; font-size:13px;",
               icon("exclamation-triangle"), " No data found for the current filter selection.")
    } else {
      plotOutput("neut_curve_plot", height = "500px")
    }
  })

  get_titer_for_plot <- function() {
    if (identical(input$curve_plate, "All Plates")) {
      bind_rows(lapply(names(rv$results), function(p)
        rv$results[[p]]$titers %>% mutate(plate_id = p)))
    } else {
      rv$results[[input$curve_plate]]$titers
    }
  }

  output$neut_curve_plot <- renderPlot({
    inh <- selected_inh(); req(nrow(inh) > 0)
    tit <- get_titer_for_plot()
    if (!is.null(input$curve_virus) && !identical(input$curve_virus, "All Viruses") && nchar(input$curve_virus) > 0)
      tit <- tit %>% filter(virus_id == input$curve_virus)
    layout <- if (identical(input$curve_plate, "All Plates")) rv$helper_table
              else rv$helper_table %>% filter(plate_id == input$curve_plate)
    print(make_neut_plot_gg(inh, tit, 50, layout))
  })

  output$dt_inhibition <- renderDT({
    inh <- selected_inh(); req(nrow(inh) > 0)
    inh %>%
      mutate(perc_inhibition = paste0(round(perc_inhibition*100, 1), "%"),
             perc_std_dev    = paste0(round(perc_std_dev*100, 1), "%"),
             dilution        = round(dilution, 3)) %>%
      mutate(across(any_of(c("plate_id","sample_id","virus_id","sample_type","pos_label")), as.factor)) %>%
      datatable(rownames = FALSE, filter = "top",
                options = list(pageLength = 16, scrollX = TRUE,
                               columnDefs = list(list(className = "dt-center", targets = "_all"))))
  })

  output$dl_curve_png <- downloadHandler(
    filename = function() paste0("NeutCurve_", input$curve_plate, "_",
                                 input$curve_sample_type, "_", input$run_date, ".png"),
    content = function(file) {
      inh <- selected_inh(); req(nrow(inh) > 0)
      tit <- get_titer_for_plot()
      if (!is.null(input$curve_virus) && !identical(input$curve_virus, "All Viruses") && nchar(input$curve_virus) > 0)
        tit <- tit %>% filter(virus_id == input$curve_virus)
      layout <- if (identical(input$curve_plate, "All Plates")) rv$helper_table
                else rv$helper_table %>% filter(plate_id == input$curve_plate)
      ggsave(file, plot = make_neut_plot_gg(inh, tit, 50, layout),
             width = 10, height = 6, dpi = 150)
    })

  # -- Titer results -----------------------------------------------------------
  titer_combined_long <- reactive({
    req(rv$analysis_done)
    collect_results("titers")
  })

  output$dt_titers <- renderDT({
    combined <- titer_combined_long() %>%
      mutate(
        IC50_pb  = dplyr::case_when(
          outcome == "FLAT"        ~ as.character(round(start_concentration, 2)),
          outcome == "Not-Titered" ~ "NA",
          !is.na(ic50_pb)          ~ as.character(round(ic50_pb, 2)),
          TRUE                     ~ "NA"),
        IC80_pb  = dplyr::case_when(
          outcome == "FLAT"        ~ as.character(round(start_concentration, 2)),
          outcome == "Not-Titered" ~ "NA",
          !is.na(ic80_pb)          ~ as.character(round(ic80_pb, 2)),
          TRUE                     ~ "NA"),
        IC50_4PL = dplyr::case_when(
          outcome == "FLAT"        ~ as.character(round(start_concentration, 2)),
          outcome == "Not-Titered" ~ "NA",
          !is.na(ic50_4pl)         ~ as.character(round(ic50_4pl, 2)),
          TRUE                     ~ "NA"),
        IC80_4PL = dplyr::case_when(
          outcome == "FLAT"        ~ as.character(round(start_concentration, 2)),
          outcome == "Not-Titered" ~ "NA",
          !is.na(ic80_4pl)         ~ as.character(round(ic80_4pl, 2)),
          TRUE                     ~ "NA"),
        Converged = ifelse(model_converged, "Yes", "No")
      ) %>%
      dplyr::select(Plate = plate_id, Sample = sample_id, Virus = virus_id,
                    Type = sample_type, Outcome = outcome,
                    IC50_pb, IC80_pb, IC50_4PL, IC80_4PL, Converged) %>%
      mutate(across(any_of(c("Plate","Sample","Virus","Type","Outcome","Converged")), as.factor))
    datatable(combined, rownames = FALSE, filter = "top",
              options = list(pageLength = 20, scrollX = TRUE)) %>%
      formatStyle("IC50_4PL",
        backgroundColor = styleEqual(
          setdiff(unique(combined$IC50_4PL), "NA"),
          rep("#DDEEFF", sum(unique(combined$IC50_4PL) != "NA")))) %>%
      formatStyle("IC50_pb",
        backgroundColor = styleEqual(
          setdiff(unique(combined$IC50_pb),  "NA"),
          rep("#E8F5E9", sum(unique(combined$IC50_pb)  != "NA")))) %>%
      formatStyle("Outcome",
        backgroundColor = styleEqual(
          c("Titered","FLAT","Not-Titered"),
          c("#D4EDDA","#F8D7DA","#FFF3CD")))
  })

  # -- Wide heatmap ------------------------------------------------------------
  output$dt_wide_titers <- renderDT({
    req(rv$analysis_done, input$wide_titer_metric)

    raw_titers <- collect_results("titers") %>%
      group_by(virus_id, sample_id) %>%
      filter(plate_id == first(plate_id)) %>%
      summarise(
        metric_val = {
          oc <- outcome[1]
          if (oc %in% c("FLAT", "Not-Titered")) {
            NA_real_
          } else {
            if (input$wide_titer_metric == "IC50") {
              v4 <- ic50_4pl[1]; vpb <- ic50_pb[1]
            } else {
              v4 <- ic80_4pl[1]; vpb <- ic80_pb[1]
            }
            if (!is.na(v4) && is.finite(v4)) v4 else vpb
          }
        },
        # Pre-compute display label per outcome rule
        disp_label = {
          oc <- outcome[1]; sc <- start_concentration[1]
          if      (oc == "FLAT")         as.character(round(sc, 2))
          else if (oc == "Not-Titered")  "NA"
          else                           NA_character_   # filled from metric_val below
        },
        .groups = "drop"
      ) %>%
      # Fill Titered rows: if disp_label is NA, derive from metric_val
      mutate(disp_label = ifelse(
        is.na(disp_label),
        ifelse(is.na(metric_val) | !is.finite(metric_val), "NA",
               as.character(round(metric_val, 2))),
        disp_label))

    # Numeric wide table -- for Geomean and colour scale only
    wide_num <- raw_titers %>%
      dplyr::select(virus_id, sample_id, metric_val) %>%
      pivot_wider(names_from = sample_id, values_from = metric_val) %>%
      arrange(virus_id)

    # Label wide table -- for cell display text
    wide_lbl <- raw_titers %>%
      dplyr::select(virus_id, sample_id, disp_label) %>%
      pivot_wider(names_from = sample_id, values_from = disp_label) %>%
      arrange(virus_id) %>%
      # Any NA from pivot_wider (missing sample/virus combos) -> "NA"
      mutate(across(-virus_id, ~ ifelse(is.na(.), "NA", .)))

    bnab_cols    <- setdiff(colnames(wide_num), "virus_id")
    all_num_cols <- c(bnab_cols, "Geomean")

    wide_num <- wide_num %>% rowwise() %>%
      mutate(Geomean = {
        vals <- c_across(all_of(bnab_cols))
        vals <- vals[!is.na(vals) & is.finite(vals) & vals > 0]
        if (length(vals) == 0) NA_real_ else exp(mean(log(vals)))
      }) %>% ungroup()

    all_vals <- unlist(wide_num[, all_num_cols], use.names = FALSE)
    all_vals <- all_vals[!is.na(all_vals) & is.finite(all_vals) & all_vals > 0]
    if (length(all_vals) < 2) all_vals <- c(1, 100)
    log_min <- log10(min(all_vals)); log_max <- log10(max(all_vals))
    if (isTRUE(all.equal(log_min, log_max))) { log_min <- log_min - 0.5; log_max <- log_max + 0.5 }

    cramp <- colorRamp(c("lightgrey", "orange", "red", "darkred"))
    make_hex <- function(vals) sapply(vals, function(v) {
      if (is.na(v) || !is.finite(v) || v <= 0) return("#EEEEEE")
      frac <- max(0, min(1, (log10(v) - log_min) / (log_max - log_min)))
      m <- cramp(frac); rgb(m[1], m[2], m[3], maxColorValue = 255)
    })

    # Build display table from label pivot; add formatted Geomean column
    wide_disp <- wide_lbl %>% rename(Virus = virus_id)
    wide_disp[["Geomean"]] <- ifelse(
      is.na(wide_num[["Geomean"]]) | !is.finite(wide_num[["Geomean"]]),
      "NA", as.character(round(wide_num[["Geomean"]], 2)))

    dt <- datatable(wide_disp, rownames = FALSE,
      options = list(pageLength = 30, scrollX = TRUE, dom = "tip",
                     columnDefs = list(list(className = "dt-center", targets = "_all"))))

    for (col in all_num_cols) {
      dt <- dt %>% formatStyle(col,
        backgroundColor = styleEqual(wide_disp[[col]], make_hex(wide_num[[col]])),
        color      = "white",
        fontWeight = if (col == "Geomean") "bold" else "normal")
    }
    dt %>% formatStyle("Geomean", borderLeft = "2px solid #fff", fontWeight = "bold", color = "white")
  })

  output$wide_titer_legend_ui <- renderUI({
    tags$div(
      style = "display:flex; align-items:center; gap:8px; font-size:11px; color:#555; margin-top:4px;",
      tags$span("Colour scale (log10):"),
      tags$span(style = "background:lightgrey; padding:3px 14px; border:1px solid #bbb;", "Low"),
      tags$span("\u2192"),
      tags$span(style = "background:orange; padding:3px 14px; border:1px solid #bbb;"),
      tags$span("\u2192"),
      tags$span(style = "background:red; color:white; padding:3px 14px;"),
      tags$span("\u2192"),
      tags$span(style = "background:darkred; color:white; padding:3px 14px;", "High"),
      tags$span(style = "margin-left:12px;", "| FLAT = max inhibition < 50% (value shown = highest conc / lowest dilution tested)  |  NA = not titered  |  4PL preferred; point-based fallback  |  Geomean = geometric mean")
    )
  })

  # -- Plate Review ------------------------------------------------------------
  output$dt_controls <- renderDT({
    req(rv$analysis_done)
    rows <- list()
    for (pid in names(rv$results)) {
      res <- rv$results[[pid]]
      cc_label <- if (length(res$cell_ctrl$cols) > 0)
                    paste(res$cell_ctrl$cols, collapse = " + ") else "CC-1"
      cc_avg <- res$cell_ctrl$avg; cc_sd <- res$cell_ctrl$sd
      rows[[length(rows)+1]] <- data.frame(plate_id = pid, control = cc_label,
        mean_RLU     = round(cc_avg, 1),
        perc_std_dev = paste0(round((cc_sd / cc_avg) * 100, 1), "%"),
        n_wells      = res$cell_ctrl$n %||% NA_integer_,
        check.names = FALSE)
      for (col in names(res$viral_avgs)) {
        v_avg <- res$viral_avgs[[col]]$avg; v_sd <- res$viral_avgs[[col]]$sd
        rows[[length(rows)+1]] <- data.frame(plate_id = pid, control = col,
          mean_RLU     = round(v_avg, 1),
          perc_std_dev = paste0(round((v_sd / v_avg) * 100, 1), "%"),
          n_wells      = res$viral_avgs[[col]]$n %||% NA_integer_,
          check.names = FALSE)
      }
    }
    datatable(bind_rows(rows), rownames = FALSE, options = list(pageLength = 20)) %>%
      formatRound("mean_RLU", digits = 1)
  })

  # Global RLU range for consistent heatmap colour scale
  global_rlu_range <- reactive({
    req(rv$analysis_done, length(rv$results) > 0)
    all_vals <- unlist(lapply(rv$results, function(res)
      as.numeric(as.matrix(apply(res$renamed, 2, as.numeric)))))
    all_vals <- all_vals[is.finite(all_vals)]
    if (length(all_vals) == 0) return(c(0, 1))
    range(all_vals, na.rm = TRUE)
  })

  output$rlu_heatmap <- renderPlot({
    req(rv$results, input$heatmap_plate %in% names(rv$results))
    pid          <- input$heatmap_plate
    renamed      <- rv$results[[pid]]$renamed
    plate_layout <- rv$helper_table %>% filter(plate_id == pid)

    rng      <- global_rlu_range()
    rlu_min  <- rng[1]; rlu_max <- rng[2]
    rows_r   <- rownames(renamed)
    n_rows   <- length(rows_r); n_cols <- length(colnames(renamed))

    run_subtitle <- paste0(
      if (!is.null(input$user_name) && nchar(trimws(input$user_name)) > 0)
        paste0("Scientist: ", trimws(input$user_name)) else "",
      if (!is.null(input$run_date) && nchar(trimws(input$run_date)) > 0)
        paste0(" | Date: ", input$run_date) else "")

    # -- LEFT: Plate Layout (RLU heatmap) --------------------------------------
    sci_lbl <- function(x) ifelse(is.na(x), "",
      ifelse(abs(x) >= 1000,
        sub("e\\+0*(\\d+)", "e+\\1", formatC(x, format = "e", digits = 1)),
        as.character(round(x, 0))))

    mat <- matrix(as.numeric(as.matrix(renamed)), nrow = n_rows, ncol = n_cols,
                  dimnames = list(rows_r, seq_len(n_cols)))

    df_rlu <- as.data.frame(mat) %>%
      tibble::rownames_to_column("row_lbl") %>%
      pivot_longer(-row_lbl, names_to = "col_pos", values_to = "rlu") %>%
      mutate(
        col_pos = factor(col_pos, levels = as.character(seq_len(n_cols))),
        row_lbl = factor(row_lbl, levels = rev(rows_r)),
        lbl     = sci_lbl(rlu),
        txt_col = ifelse(!is.na(rlu) & rlu > (rlu_min + (rlu_max - rlu_min) * 0.35),
                         "white", "grey20"))

    p_layout <- ggplot(df_rlu, aes(x = col_pos, y = row_lbl, fill = rlu)) +
      geom_tile(colour = "white", linewidth = 0.4) +
      geom_text(aes(label = lbl, colour = txt_col), size = 3.0, fontface = "bold") +
      scale_colour_identity() +
      scale_fill_gradientn(
        colors = c("lightgrey","orange","red","darkred"), na.value = "white",
        values = scales::rescale(c(rlu_min, rlu_min + (rlu_max - rlu_min)*0.45, rlu_max)),
        limits = c(rlu_min, rlu_max), name = "RLU values", oob = scales::squish) +
      scale_x_discrete(expand = expansion(0)) +
      scale_y_discrete(expand = expansion(0)) +
      labs(title = paste0("Plate RLUs: ", pid), subtitle = run_subtitle,
           x = "Column", y = "Row") +
      theme_minimal(base_size = 9) +
      theme(panel.grid = element_blank(),
            plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
            plot.subtitle = element_text(size = 7.5, colour = "grey40", hjust = 0.5),
            legend.position = "right",
            legend.key.height = unit(1.5, "cm"), legend.key.width = unit(0.35, "cm"),
            plot.margin = margin(4,4,4,4,"mm"),
            panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5))

    # -- RIGHT: Plate Map ------------------------------------------------------
    trunc8 <- function(s) if (nchar(s) > 8) paste0(substr(s,1,7), ".") else s

    n_phys_rows <- n_rows
    n_phys_cols <- n_cols
    well_type_mat <- matrix("Neutralization", nrow = n_phys_rows, ncol = n_phys_cols)
    well_lbl_mat  <- matrix("", nrow = n_phys_rows, ncol = n_phys_cols)

    distinct_cc <- unique(trimws(as.character(plate_layout$cell_control_range)))
    distinct_cc <- distinct_cc[!is.na(distinct_cc) & nchar(distinct_cc) > 0]
    cc_nm_map   <- setNames(paste0("CC-", seq_along(distinct_cc)), distinct_cc)

    # Neutralization wells
    for (li in seq_len(nrow(plate_layout))) {
      lrow <- plate_layout[li,]
      pw   <- parse_wells(lrow$plate_range, n_phys_rows, n_phys_cols)
      if (nrow(pw) > 0) {
        lbl <- paste0(trunc8(as.character(lrow$sample_id)),"\n",trunc8(as.character(lrow$virus_id)))
        for (wi in seq_len(nrow(pw))) {
          r <- pw$r[wi]; cc <- pw$c[wi]
          if (r >= 1 && r <= n_phys_rows && cc >= 1 && cc <= n_phys_cols) {
            well_type_mat[r, cc] <- "Neutralization"; well_lbl_mat[r, cc] <- lbl
          }
        }
      }
    }
    # Virus controls
    for (v in unique(plate_layout$virus_id)) {
      vrows <- plate_layout %>% filter(virus_id == v)
      vw    <- parse_wells(vrows$virus_control_range[1], n_phys_rows, n_phys_cols)
      if (nrow(vw) > 0) {
        lbl <- paste0("VC:\n", trunc8(as.character(v)))
        for (wi in seq_len(nrow(vw))) {
          r <- vw$r[wi]; cc <- vw$c[wi]
          if (r >= 1 && r <= n_phys_rows && cc >= 1 && cc <= n_phys_cols) {
            well_type_mat[r, cc] <- "Virus Control"; well_lbl_mat[r, cc] <- lbl
          }
        }
      }
    }
    # Cell controls
    for (cc_rstr in names(cc_nm_map)) {
      nm <- cc_nm_map[[cc_rstr]]
      cw <- parse_wells(cc_rstr, n_phys_rows, n_phys_cols)
      if (nrow(cw) > 0) {
        for (wi in seq_len(nrow(cw))) {
          r <- cw$r[wi]; cc <- cw$c[wi]
          if (r >= 1 && r <= n_phys_rows && cc >= 1 && cc <= n_phys_cols) {
            well_type_mat[r, cc] <- "Cell Control"; well_lbl_mat[r, cc] <- nm
          }
        }
      }
    }

    df_map <- expand.grid(phys_row = seq_len(n_phys_rows),
                          phys_col = seq_len(n_phys_cols), stringsAsFactors = FALSE) %>%
      mutate(well_type = mapply(function(r,c) well_type_mat[r,c], phys_row, phys_col),
             label     = mapply(function(r,c) well_lbl_mat[r,c],  phys_row, phys_col))

    phys_row_labels <- if (n_phys_rows <= 26) LETTERS[seq_len(n_phys_rows)]
                       else as.character(seq_len(n_phys_rows))
    phys_col_labels <- as.character(seq_len(n_phys_cols))

    x_title <- "Column"
    y_title <- "Row"

    df_map <- df_map %>% mutate(
      x_pos = factor(phys_col, levels = seq_len(n_phys_cols)),
      y_pos = factor(phys_row, levels = rev(seq_len(n_phys_rows))))

    well_colours <- c("Cell Control" = "#9E9E9E", "Neutralization" = "#F4A020",
                      "Virus Control" = "#CC2222", "unknown" = "white")

    p_map <- ggplot(df_map, aes(x = x_pos, y = y_pos, fill = well_type)) +
      geom_tile(colour = "white", linewidth = 0.4) +
      geom_text(aes(label = label), size = 3, lineheight = 0.82,
                colour = "black", fontface = "bold") +
      scale_fill_manual(values = well_colours, name = "Well Type", na.value = "grey90", drop = FALSE) +
      scale_x_discrete(expand = expansion(0), labels = phys_col_labels) +
      scale_y_discrete(expand = expansion(0), labels = rev(phys_row_labels)) +
      labs(title = paste0("Plate Map: ", pid), subtitle = run_subtitle,
           x = x_title, y = y_title) +
      theme_minimal(base_size = 9) +
      theme(panel.grid = element_blank(),
            plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
            plot.subtitle = element_text(size = 7.5, colour = "grey40", hjust = 0.5),
            legend.position = "right",
            legend.key.height = unit(0.5, "cm"), legend.key.width = unit(0.35, "cm"),
            plot.margin = margin(4,4,4,4,"mm"),
            panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5))

    gridExtra::grid.arrange(
      p_layout, p_map, ncol = 2, widths = c(1, 1),
      bottom = grid::textGrob(
        paste0("Global colour scale: ",
               format(round(rlu_min, 0), big.mark = ","), " - ",
               format(round(rlu_max, 0), big.mark = ",")),
        gp = grid::gpar(fontsize = 7, col = "grey55")))
  })

  # -- Export metadata ---------------------------------------------------------
  output$export_meta <- renderText({
    paste0("Run date : ", input$run_date, "\n",
           "Analyst  : ", input$user_name, "\n",
           "IC thresh: ", 50, "%\n",
           "Plates   : ", paste(names(rv$results), collapse = ", "))
  })

  # -- Helper: look up experiment_date and scientist_id from raw_plates --------
  get_plate_meta <- function(pid) {
    entry <- Filter(function(x) identical(x$plate_id, pid), rv$raw_plates)
    if (length(entry) == 0) return(list(experiment_date = NA_character_, scientist_id = NA_character_))
    list(experiment_date = entry[[1]]$experiment_date %||% NA_character_,
         scientist_id    = entry[[1]]$scientist_id    %||% NA_character_)
  }

  # -- Export XLSX -------------------------------------------------------------
  output$dl_xlsx <- downloadHandler(
    filename = function() paste0("NeutResults_", input$run_date, ".xlsx"),
    content = function(file) {
      req(rv$analysis_done)
      wb <- createWorkbook()

      # Common styles
      hdr_style  <- createStyle(textDecoration = "bold", fgFill = "#1F4E79",
                                fontColour = "#FFFFFF", border = "Bottom",
                                halign = "center", wrapText = FALSE)
      data_style <- createStyle(halign = "left")
      num_style  <- createStyle(numFmt = "#,##0", halign = "right")
      title_dark <- createStyle(textDecoration = "bold", fgFill = "#1a3a6b",
                                fontColour = "#FFFFFF", fontSize = 11)
      title_plain <- createStyle(textDecoration = "bold", fontSize = 11)

      write_styled_table <- function(ws_name, df, start_row = 1, start_col = 1) {
        writeData(wb, ws_name, df, startRow = start_row, startCol = start_col,
                  colNames = TRUE, rowNames = FALSE)
        nc <- ncol(df); nr <- nrow(df)
        addStyle(wb, ws_name, hdr_style,
                 rows = start_row, cols = start_col:(start_col+nc-1), gridExpand = TRUE)
        if (nr > 0)
          addStyle(wb, ws_name, data_style,
                   rows = (start_row+1):(start_row+nr), cols = start_col:(start_col+nc-1),
                   gridExpand = TRUE)
        setColWidths(wb, ws_name, cols = start_col:(start_col+nc-1), widths = "auto")
      }

      # -- 01_plate_setup ------------------------------------------------------
      plate_setup_xl <- rv$helper_table %>%
        mutate(analyst_id = input$user_name %||% "") %>%
        rename_with(~ case_when(. == "start_concentration" ~ "dilution_start", TRUE ~ .))
      addWorksheet(wb, "01_plate_setup")
      write_styled_table("01_plate_setup", plate_setup_xl)

      # -- 02_raw_RLUs ---------------------------------------------------------
      addWorksheet(wb, "02_raw_RLUs")
      cur_row <- 1
      for (pid in names(rv$results)) {
        res  <- rv$results[[pid]]
        mat_p <- as.data.frame(res$renamed)
        nr_p <- nrow(mat_p); nc_p <- ncol(mat_p)
        rownames(mat_p) <- if (nr_p <= 26) LETTERS[seq_len(nr_p)] else as.character(seq_len(nr_p))
        colnames(mat_p) <- as.character(seq_len(nc_p))
        mat_p[] <- lapply(mat_p, function(x) suppressWarnings(as.numeric(x)))
        plate_df <- cbind(Row = rownames(mat_p), mat_p); rownames(plate_df) <- NULL

        writeData(wb, "02_raw_RLUs", data.frame(V1 = paste0("Plate: ", pid)),
                  startRow = cur_row, startCol = 1, colNames = FALSE)
        addStyle(wb, "02_raw_RLUs", title_dark, rows = cur_row, cols = 1:ncol(plate_df), gridExpand = TRUE)
        cur_row <- cur_row + 1
        writeData(wb, "02_raw_RLUs", plate_df, startRow = cur_row, startCol = 1, colNames = TRUE, rowNames = FALSE)
        addStyle(wb, "02_raw_RLUs", hdr_style, rows = cur_row, cols = 1:ncol(plate_df), gridExpand = TRUE)
        addStyle(wb, "02_raw_RLUs", num_style,
                 rows = (cur_row+1):(cur_row+nr_p), cols = 2:ncol(plate_df), gridExpand = TRUE)
        setColWidths(wb, "02_raw_RLUs", cols = 1:ncol(plate_df), widths = "auto")
        cur_row <- cur_row + nr_p + 2
      }

      # -- 03_plate_maps -------------------------------------------------------
      addWorksheet(wb, "03_plate_maps")
      pm_row <- 1
      pm_hdr   <- createStyle(textDecoration = "bold", border = "Bottom", halign = "center")
      pm_title <- createStyle(textDecoration = "bold", fgFill = "#E8E8E8", fontSize = 10)
      trunc6   <- function(s) if (nchar(s) > 6) paste0(substr(s,1,5), ".") else s

      for (pid in names(rv$results)) {
        pl    <- rv$helper_table %>% filter(trimws(toupper(plate_id)) == trimws(toupper(pid)))
        res_p <- rv$results[[pid]]
        n_r   <- nrow(res_p$renamed)
        n_c   <- ncol(res_p$renamed)
        row_lbls <- if (n_r <= 26) LETTERS[seq_len(n_r)] else as.character(seq_len(n_r))
        col_lbls <- as.character(seq_len(n_c))
        wt_mat   <- matrix("", nrow = n_r, ncol = n_c)

        for (li in seq_len(nrow(pl))) {
          pw <- parse_wells(pl$plate_range[li], n_r, n_c)
          if (nrow(pw) > 0) for (wi in seq_len(nrow(pw))) {
            r <- pw$r[wi]; cc <- pw$c[wi]
            if (r >= 1 && r <= n_r && cc >= 1 && cc <= n_c)
              wt_mat[r, cc] <- paste0(trunc6(as.character(pl$sample_id[li])), "/",
                                      trunc6(as.character(pl$virus_id[li])))
          }
        }
        for (v in unique(pl$virus_id)) {
          vr <- pl %>% filter(virus_id == v)
          pw <- parse_wells(vr$virus_control_range[1], n_r, n_c)
          if (nrow(pw) > 0) for (wi in seq_len(nrow(pw))) {
            r <- pw$r[wi]; cc <- pw$c[wi]
            if (r >= 1 && r <= n_r && cc >= 1 && cc <= n_c)
              wt_mat[r, cc] <- paste0("VC:", trunc6(as.character(v)))
          }
        }
        dcc <- unique(trimws(as.character(pl$cell_control_range)))
        dcc <- dcc[!is.na(dcc) & nchar(dcc) > 0]
        cc_nm <- setNames(paste0("CC-", seq_along(dcc)), dcc)
        for (cc_r in names(cc_nm)) {
          pw <- parse_wells(cc_r, n_r, n_c)
          if (nrow(pw) > 0) for (wi in seq_len(nrow(pw))) {
            r <- pw$r[wi]; cc <- pw$c[wi]
            if (r >= 1 && r <= n_r && cc >= 1 && cc <= n_c)
              wt_mat[r, cc] <- cc_nm[[cc_r]]
          }
        }
        writeData(wb, "03_plate_maps", data.frame(V1 = paste0("Plate Map: ", pid)),
                  startRow = pm_row, startCol = 1, colNames = FALSE)
        addStyle(wb, "03_plate_maps", pm_title, rows = pm_row, cols = 1:(n_c+1))
        pm_row <- pm_row + 1
        hdr_df <- as.data.frame(t(c("", col_lbls)), stringsAsFactors = FALSE); colnames(hdr_df) <- NULL
        writeData(wb, "03_plate_maps", hdr_df, startRow = pm_row, startCol = 1, colNames = FALSE)
        addStyle(wb, "03_plate_maps", pm_hdr, rows = pm_row, cols = 1:(n_c+1), gridExpand = TRUE)
        pm_row <- pm_row + 1
        for (ri in seq_len(n_r)) {
          rd <- as.data.frame(t(c(row_lbls[ri], wt_mat[ri,])), stringsAsFactors = FALSE); colnames(rd) <- NULL
          writeData(wb, "03_plate_maps", rd, startRow = pm_row, startCol = 1, colNames = FALSE)
          pm_row <- pm_row + 1
        }
        setColWidths(wb, "03_plate_maps", cols = 1:(n_c+1), widths = "auto")
        pm_row <- pm_row + 2
      }

      addWorksheet(wb, "04_plate_data")
      pd_rows <- list()
      for (pid in names(rv$results)) {
        res    <- rv$results[[pid]]
        meta   <- get_plate_meta(pid)
        pl     <- rv$helper_table %>% filter(trimws(toupper(plate_id)) == trimws(toupper(pid)))
        inst   <- get_plate_instrument(rv$helper_table, pid)
        n_r_pd <- nrow(res$renamed)
        n_c_pd <- ncol(res$renamed)

        # Build well metadata grid (A-H rows, 1-12 cols, physical plate coordinates)
        wm_df <- expand.grid(plate_row = LETTERS[seq_len(n_r_pd)],
                             plate_col = seq_len(n_c_pd), stringsAsFactors = FALSE) %>%
          mutate(plate_coord = paste0(plate_row, plate_col),
                 data_type = "neutralization", sample_id = NA_character_,
                 virus_id = NA_character_, sample_dilution = NA_real_,
                 replicate = NA_character_)

        # Mark cell controls
        dcc <- unique(trimws(as.character(pl$cell_control_range)))
        dcc <- dcc[!is.na(dcc) & nchar(dcc) > 0]
        for (cc_r in dcc) {
          pw <- parse_wells(cc_r, n_r_pd, n_c_pd)
          if (nrow(pw) > 0) for (wi in seq_len(nrow(pw))) {
            mask <- wm_df$plate_row == LETTERS[pw$r[wi]] & wm_df$plate_col == pw$c[wi]
            wm_df$data_type[mask] <- "cell_control"
            wm_df$sample_id[mask] <- "NA"; wm_df$virus_id[mask] <- "NA"
          }
        }
        # Mark virus controls
        for (v in unique(pl$virus_id)) {
          vr <- pl %>% filter(virus_id == v)
          pw <- parse_wells(vr$virus_control_range[1], n_r_pd, n_c_pd)
          if (nrow(pw) > 0) for (wi in seq_len(nrow(pw))) {
            mask <- wm_df$plate_row == LETTERS[pw$r[wi]] & wm_df$plate_col == pw$c[wi]
            wm_df$data_type[mask] <- "virus_control"
            wm_df$sample_id[mask] <- "NA"; wm_df$virus_id[mask] <- v
          }
        }
        # Mark neutralization wells - use parse_range_2d to resolve rep/dilution axes
        for (li in seq_len(nrow(pl))) {
          lrow  <- pl[li,]
          sc    <- as.numeric(lrow$start_concentration %||% lrow$dilution_start %||% NA)
          df_v  <- as.numeric(lrow$dilution_factor %||% NA)
          stype <- as.character(lrow$sample_type)
          r2d   <- parse_range_2d(as.character(lrow$plate_range))
          if (is.null(r2d)) next

          dils <- if (!is.na(sc) && !is.na(df_v))
            make_dilutions(sc, df_v, n = r2d$n_dil, sample_type = stype)
          else rep(NA_real_, r2d$n_dil)

          if (r2d$dil_by_row) {
            # rows = dilution axis; cols = rep1/rep2
            for (ri in seq(r2d$row1, r2d$row2)) {
              dil_idx <- ri - r2d$row1 + 1L
              dil_val <- if (dil_idx <= length(dils)) dils[dil_idx] else NA_real_
              mask1 <- wm_df$plate_row == LETTERS[ri] & wm_df$plate_col == r2d$col1
              mask2 <- wm_df$plate_row == LETTERS[ri] & wm_df$plate_col == r2d$col2
              for (mask in list(mask1, mask2)) {
                wm_df$data_type[mask]       <- "neutralization"
                wm_df$sample_id[mask]       <- as.character(lrow$sample_id)
                wm_df$virus_id[mask]        <- as.character(lrow$virus_id)
                wm_df$sample_dilution[mask] <- dil_val
              }
              wm_df$replicate[mask1] <- "rep1"
              wm_df$replicate[mask2] <- "rep2"
            }
          } else {
            # cols = dilution axis; rows = rep1/rep2
            for (ci_offset in seq_len(r2d$col2 - r2d$col1 + 1L)) {
              ci      <- r2d$col1 + ci_offset - 1L
              dil_val <- if (ci_offset <= length(dils)) dils[ci_offset] else NA_real_
              mask1   <- wm_df$plate_row == LETTERS[r2d$row1] & wm_df$plate_col == ci
              mask2   <- wm_df$plate_row == LETTERS[r2d$row2] & wm_df$plate_col == ci
              for (mask in list(mask1, mask2)) {
                wm_df$data_type[mask]       <- "neutralization"
                wm_df$sample_id[mask]       <- as.character(lrow$sample_id)
                wm_df$virus_id[mask]        <- as.character(lrow$virus_id)
                wm_df$sample_dilution[mask] <- dil_val
              }
              wm_df$replicate[mask1] <- "rep1"
              wm_df$replicate[mask2] <- "rep2"
            }
          }
        }

        # Get RLU values directly from the (already letter-rowed) renamed matrix
        rlu_mat <- as.matrix(apply(res$renamed, 2, as.numeric))
        rownames(rlu_mat) <- LETTERS[seq_len(nrow(rlu_mat))]
        colnames(rlu_mat) <- as.character(seq_len(ncol(rlu_mat)))

        wm_df$rlu_value <- mapply(function(r, cc) {
          if (r %in% rownames(rlu_mat) && as.character(cc) %in% colnames(rlu_mat))
            as.numeric(rlu_mat[r, as.character(cc)]) else NA_real_
        }, wm_df$plate_row, wm_df$plate_col)

        wm_df$file_coord <- mapply(function(pr, pc) plate_to_excel_coords(pr, pc, inst),
                                   wm_df$plate_row, wm_df$plate_col)

        wm_df$scientist_id    <- meta$scientist_id %||% NA_character_
        wm_df$plate_id        <- pid
        wm_df$experiment_date <- meta$experiment_date %||% NA_character_

        wm_df$replicate[wm_df$data_type %in% c("cell_control","virus_control")] <- "NA"
        wm_df$sample_id[is.na(wm_df$sample_id)] <- "NA"
        wm_df$virus_id[is.na(wm_df$virus_id)]   <- "NA"

        # Convert sample_dilution to character so controls display "NA" (not blank) in Excel
        wm_df$sample_dilution <- as.character(wm_df$sample_dilution)
        wm_df$sample_dilution[wm_df$data_type %in% c("cell_control","virus_control")] <- "NA"

        pd_rows[[pid]] <- wm_df %>%
          dplyr::select(scientist_id, plate_id, experiment_date,
                        plate_coord, plate_row, plate_col, file_coord,
                        data_type, sample_id, virus_id,
                        sample_dilution, replicate, rlu_value)
      }
      write_styled_table("04_plate_data", bind_rows(pd_rows))

      # -- 05_control_summary --------------------------------------------------
      ctrl_rows <- list()
      for (pid in names(rv$results)) {
        res  <- rv$results[[pid]]; meta <- get_plate_meta(pid)
        ctrl_nm <- if (length(res$cell_ctrl$ranges) > 0)
          paste(res$cell_ctrl$ranges, collapse = "+") else "cell_ctrl"
        cc_avg <- res$cell_ctrl$avg
        cc_sd  <- res$cell_ctrl$sd
        cc_n   <- res$cell_ctrl$n %||% NA_integer_
        ctrl_rows[[length(ctrl_rows)+1]] <- data.frame(
          plate_id = pid, experiment_date = meta$experiment_date,
          scientist_id = meta$scientist_id, virus_id = "NA", control = ctrl_nm,
          mean_RLUs     = round(cc_avg, 2),
          perc_std_dev  = round((cc_sd / cc_avg), 4),   # numeric proportion; displayed as % via numFmt
          n_wells       = cc_n, check.names = FALSE)
        for (col in names(res$viral_avgs)) {
          v_avg <- res$viral_avgs[[col]]$avg
          v_sd  <- res$viral_avgs[[col]]$sd
          v_n   <- res$viral_avgs[[col]]$n %||% NA_integer_
          ctrl_rows[[length(ctrl_rows)+1]] <- data.frame(
            plate_id = pid, experiment_date = meta$experiment_date,
            scientist_id = meta$scientist_id,
            virus_id = sub(" Virus Control$", "", col), control = col,
            mean_RLUs    = round(v_avg, 2),
            perc_std_dev = round((v_sd / v_avg), 4),    # numeric proportion; displayed as % via numFmt
            n_wells      = v_n, check.names = FALSE)
        }
      }
      addWorksheet(wb, "05_control_summary")
      ctrl_summary_df <- bind_rows(ctrl_rows)
      write_styled_table("05_control_summary", ctrl_summary_df)
      # Display as %, highlight values > 30% with red font
      if (nrow(ctrl_summary_df) > 0) {
        pct_rows_05 <- 2:(nrow(ctrl_summary_df) + 1)
        addStyle(wb, "05_control_summary",
                 createStyle(numFmt = "0.00%", halign = "left"),
                 rows = pct_rows_05, cols = 7, gridExpand = TRUE, stack = TRUE)
        conditionalFormatting(
          wb, "05_control_summary",
          cols  = 7,
          rows  = pct_rows_05,
          rule  = ">0.3",
          style = createStyle(fontColour = "#FF0000"),
          type  = "expression"
        )
      }

      # -- 06_percent_inhibition -----------------------------------------------
      all_inh <- collect_results("inhibition")
      if (nrow(all_inh) > 0) {
        ctrl_lookup <- lapply(names(rv$results), function(pid) {
          res <- rv$results[[pid]]
          list(cell = res$cell_ctrl$avg, viral = sapply(res$viral_avgs, function(x) x$avg))
        })
        names(ctrl_lookup) <- names(rv$results)

        all_inh_xl <- all_inh %>% rowwise() %>%
          mutate(
            mean_RLUs          = (rep1_RLU + rep2_RLU) / 2,
            cell_control_mean  = { lk <- ctrl_lookup[[plate_id]]; if (!is.null(lk)) lk$cell else NA_real_ },
            virus_control_mean = {
              lk <- ctrl_lookup[[plate_id]]; v_key <- paste0(virus_id, " Virus Control")
              if (!is.null(lk) && v_key %in% names(lk$viral)) lk$viral[[v_key]] else NA_real_
            },
            perc_inhibition = paste0(round(perc_inhibition * 100, 2), "%"),
            perc_std_dev    = round(perc_std_dev, 4)   # numeric proportion; displayed as % via numFmt
          ) %>% ungroup() %>%
          dplyr::select(plate_id, sample_id, virus_id, sample_type, dilution,
                        rep1_RLU, rep2_RLU, mean_RLUs,
                        cell_control_mean, virus_control_mean,
                        perc_inhibition, perc_std_dev)
        addWorksheet(wb, "06_percent_inhibition")
        write_styled_table("06_percent_inhibition", all_inh_xl)
        # Display as %, highlight values > 30% with red font
        pct_rows_06 <- 2:(nrow(all_inh_xl) + 1)
        addStyle(wb, "06_percent_inhibition",
                 createStyle(numFmt = "0.00%", halign = "left"),
                 rows = pct_rows_06, cols = 12, gridExpand = TRUE, stack = TRUE)
        conditionalFormatting(
          wb, "06_percent_inhibition",
          cols  = 12,
          rows  = pct_rows_06,
          rule  = ">0.3",
          style = createStyle(fontColour = "#FF0000"),
          type  = "expression"
        )
      }

      # -- 07_inhibition_curves ------------------------------------------------
      addWorksheet(wb, "07_inhibition_curves")
      plot_tmp <- file.path(tempdir(), paste0("plots_", format(Sys.time(), "%Y%m%d%H%M%S")))
      dir.create(plot_tmp, recursive = TRUE, showWarnings = FALSE)

      curve_plots <- make_inhibition_curves_plots(rv$results, rv$helper_table, 50)
      PLOT_H <- 5.5; PLOT_W <- 8.0; ROWS_PER <- 28
      img_row <- 1

      for (pid in names(curve_plots)) {
        panels <- curve_plots[[pid]]
        has_s <- !is.null(panels$serum); has_m <- !is.null(panels$mab)
        if (!has_s && !has_m) next

        save_and_insert <- function(plot_obj, col_start, suffix) {
          path <- file.path(plot_tmp, paste0(suffix, "_", pid, ".png"))
          tryCatch({
            # Add explicit left margin to prevent y-axis title ("Inhibition (%)") clipping
            plot_obj <- plot_obj +
              theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 15, unit = "pt"),
                    axis.title.y = element_text(size = 10, margin = margin(r = 8, unit = "pt")))
            ggsave(path, plot = plot_obj, width = PLOT_W, height = PLOT_H, dpi = 150)
            insertImage(wb, "07_inhibition_curves", path,
                        startRow = img_row, startCol = col_start,
                        width = PLOT_W, height = PLOT_H, units = "in")
          }, error = function(e) warning("Plot failed for ", pid, ": ", e$message))
        }

        if (has_s && has_m) {
          save_and_insert(panels$serum, 1,  "serum")
          save_and_insert(panels$mab,   11, "mab")
        } else {
          save_and_insert(if (has_s) panels$serum else panels$mab, 1, "only")
        }
        img_row <- img_row + ROWS_PER
      }

      # -- 07_titers ----------------------------------------------------------
      # Helper: FLAT -> start_concentration; Not-Titered -> "NA"; Titered -> numeric or "NA"
      fmt_ic <- function(value, outcome, start_conc) {
        dplyr::case_when(
          outcome == "FLAT"                              ~ as.character(round(start_conc, 2)),
          outcome == "Not-Titered"                       ~ "NA",
          !is.na(value) & is.finite(value)              ~ as.character(round(value, 4)),
          TRUE                                           ~ "NA"
        )
      }

      titer_rows_xl <- list()
      for (pid in names(rv$results)) {
        res  <- rv$results[[pid]]; meta <- get_plate_meta(pid)
        tdf  <- res$titers

        for (si in seq_len(nrow(tdf))) {
          row_t <- tdf[si, ]
          oc    <- row_t$outcome
          sc    <- row_t$start_concentration
          titer_rows_xl[[length(titer_rows_xl)+1]] <- data.frame(
            scientist_id    = meta$scientist_id %||% NA_character_,
            plate_id        = pid,
            experiment_date = meta$experiment_date %||% NA_character_,
            sample_id       = row_t$sample_id,
            virus_id        = row_t$virus_id,
            sample_type     = row_t$sample_type,
            n_points        = row_t$n_points,
            outcome         = oc,
            # Point-based log-linear interpolation
            ic50_pointbased = fmt_ic(row_t$ic50_pb,   oc, sc),
            ic80_pointbased = fmt_ic(row_t$ic80_pb,   oc, sc),
            # 4PL model results
            model_converged = ifelse(isTRUE(row_t$model_converged), "Yes", "No"),
            slope_4pl       = ifelse(is.na(row_t$slope_4pl) | !is.finite(row_t$slope_4pl),
                                     "NA", as.character(round(row_t$slope_4pl, 4))),
            lower_4pl       = ifelse(is.na(row_t$lower_4pl) | !is.finite(row_t$lower_4pl),
                                     "NA", as.character(round(row_t$lower_4pl, 4))),
            upper_4pl       = ifelse(is.na(row_t$upper_4pl) | !is.finite(row_t$upper_4pl),
                                     "NA", as.character(round(row_t$upper_4pl, 4))),
            ic50_4pl        = fmt_ic(row_t$ic50_4pl,  oc, sc),
            ic80_4pl        = fmt_ic(row_t$ic80_4pl,  oc, sc),
            stringsAsFactors = FALSE, check.names = FALSE)
        }
      }
      addWorksheet(wb, "07_titers")
      write_styled_table("07_titers", bind_rows(titer_rows_xl))

      # -- IC50_wide / IC80_wide -----------------------------------------------
      # Numeric value for Titered rows (4PL preferred, point-based fallback).
      # FLAT -> highest concentration/lowest dilution assessed (start_concentration).
      # Not-Titered -> "NA".  Missing sample/virus combos after pivot -> "NA".
      titer_src_xl <- collect_results("titers") %>%
        group_by(virus_id, sample_id) %>%
        filter(plate_id == first(plate_id)) %>%
        summarise(
          outcome_val = outcome[1],
          start_conc  = start_concentration[1],
          IC50 = {
            oc <- outcome[1]; v4 <- ic50_4pl[1]; vpb <- ic50_pb[1]
            if (oc %in% c("FLAT","Not-Titered")) NA_real_
            else if (!is.na(v4) && is.finite(v4)) v4
            else if (!is.na(vpb) && is.finite(vpb)) vpb
            else NA_real_
          },
          IC80 = {
            oc <- outcome[1]; v4 <- ic80_4pl[1]; vpb <- ic80_pb[1]
            if (oc %in% c("FLAT","Not-Titered")) NA_real_
            else if (!is.na(v4) && is.finite(v4)) v4
            else if (!is.na(vpb) && is.finite(vpb)) vpb
            else NA_real_
          },
          .groups = "drop")

      make_wide_xl <- function(src, value_col) {
        # Numeric pivot -- used only for Geomean calculation
        num_wide <- src %>%
          dplyr::select(virus_id, sample_id, val = all_of(value_col)) %>%
          group_by(virus_id, sample_id) %>%
          summarise(val = { v <- val[!is.na(val)]; if (length(v) == 0) NA_real_ else v[1] },
                    .groups = "drop") %>%
          pivot_wider(names_from = sample_id, values_from = val) %>% arrange(virus_id) %>%
          rowwise() %>%
          mutate(Geomean = {
            vs <- c_across(-virus_id); vs <- vs[!is.na(vs) & is.finite(vs) & vs > 0]
            if (length(vs) == 0) NA_real_ else round(exp(mean(log(vs))), 4)
          }) %>% ungroup() %>% rename(Virus = virus_id)

        # Label pivot: FLAT -> start_conc, Not-Titered -> "NA", Titered -> numeric or "NA"
        lbl_wide <- src %>%
          mutate(disp = dplyr::case_when(
            outcome_val == "FLAT"        ~ as.character(round(start_conc, 4)),
            outcome_val == "Not-Titered" ~ "NA",
            !is.na(.data[[value_col]]) & is.finite(.data[[value_col]]) ~
              as.character(round(.data[[value_col]], 4)),
            TRUE ~ "NA")) %>%
          dplyr::select(virus_id, sample_id, disp) %>%
          pivot_wider(names_from = sample_id, values_from = disp) %>% arrange(virus_id) %>%
          rename(Virus = virus_id) %>%
          # Any NA from pivot_wider (missing sample/virus combos) -> "NA"
          mutate(across(everything(), ~ ifelse(is.na(.), "NA", .)))

        # Geomean: numeric -> formatted string; NA -> "NA"
        lbl_wide[["Geomean"]] <- ifelse(
          is.na(num_wide[["Geomean"]]) | !is.finite(num_wide[["Geomean"]]),
          "NA", as.character(round(num_wide[["Geomean"]], 4)))

        lbl_wide
      }

      for (metric in c("IC50","IC80")) {
        wide <- make_wide_xl(titer_src_xl, metric)
        ws <- paste0(metric, "_wide")
        addWorksheet(wb, ws)
        writeData(wb, ws,
                  data.frame(V1 = paste0(metric, " -- 4PL preferred; point-based fallback; FLAT = highest conc/lowest dilution assessed; NA = Not-Titered")),
                  startRow = 1, startCol = 1, colNames = FALSE)
        addStyle(wb, ws, title_plain, rows = 1, cols = 1)
        write_styled_table(ws, wide, start_row = 2)
      }

      saveWorkbook(wb, file, overwrite = TRUE)
    })

}

shinyApp(ui, server)
