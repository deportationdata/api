# plumber.R ------------------------------------------------------------------
library(plumber)
library(dplyr)
library(readr)
library(writexl)
library(haven)
library(arrow)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------
# 1.  Load data once at start-up
# ---------------------------------------------------------------------------
data <- arrow::read_feather(
  "https://github.com/deportationdata/ice/releases/latest/download/arrests-latest.feather"
)

# ---------------------------------------------------------------------------
# 2.  Pre-compute distinct values for each column (used for the filters list)
#     This runs once at launch; no heavy work happens per request.
# ---------------------------------------------------------------------------
filters_cache <- lapply(data, function(x){
  vals <- unique(x)
  vals[!is.na(vals)]
})
names(filters_cache) <- names(data)

# ---------------------------------------------------------------------------
# 3.  CORS filter (unchanged)
# ---------------------------------------------------------------------------
#* @filter cors
function(req, res){
  res$setHeader("Access-Control-Allow-Origin",  "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")

  if (req$REQUEST_METHOD == "OPTIONS"){
    res$status <- 200
    return(list())
  }

  forward()
}

# ---------------------------------------------------------------------------
# 4.  Helper — apply per-column search, ordering, paging
# ---------------------------------------------------------------------------
apply_query <- function(df, q, do_page = TRUE){
  # -- column search ---------------------------------------------------------
  for (i in seq_along(df)){
    col_query <- q[[sprintf("columns[%d][search][value]", i - 1)]]
    if (!is.null(col_query) && nzchar(col_query)){
      col_query <- gsub("^\\^|\\$$", "", col_query)   # drop anchors
      cn <- names(df)[i]
      df <- df |>
        filter(grepl(col_query, as.character(.data[[cn]]), ignore.case = TRUE))
    }
  }

  # -- ordering (first instruction only) ------------------------------------
  if (!is.null(q[["order[0][column]"]])){
    col_idx <- as.integer(q[["order[0][column]"]]) + 1L
    if (!is.na(col_idx) && col_idx <= ncol(df)){
      cn  <- names(df)[col_idx]
      dir <- q[["order[0][dir]"]]
      df  <- if (identical(dir, "desc")){
        arrange(df, desc(.data[[cn]]))
      } else {
        arrange(df, .data[[cn]])
      }
    }
  }

  # -- paging ---------------------------------------------------------------
  if (do_page){
    start  <- as.integer(q[["start"]]  %||% 0L)
    length <- as.integer(q[["length"]] %||% 10L)
    if (!is.na(start) && !is.na(length) && length > 0){
      end <- min(start + length, nrow(df))
      df  <- if (start <= end) df[(start + 1L):end, ] else df[0, ]
    }
  }

  df
}

# ---------------------------------------------------------------------------
# 5.  Data endpoint (now with `filters` on draw == 1)
# ---------------------------------------------------------------------------
#* @serializer unboxedJSON list(dataframe = "rows", auto_unbox = TRUE, na = "null")
#* @get /data
function(req){
  q      <- req$args
  draw_i <- as.integer(q[["draw"]] %||% 0L)

  df_page     <- apply_query(data, q, do_page = TRUE)
  filtered_n  <- nrow(apply_query(data, q, do_page = FALSE))

  out <- list(
    draw            = draw_i,
    recordsTotal    = nrow(data),
    recordsFiltered = filtered_n,
    data            = df_page
  )

  # Send the filter lists only on the very first draw
  if (identical(draw_i, 1L)){
    out$filters <- filters_cache
  }

  out
}

# ---------------------------------------------------------------------------
# 6.  Download endpoint (unchanged)
# ---------------------------------------------------------------------------
#* Download filtered dataset as CSV, Excel, or Stata
#* @serializer contentType list(type = "application/octet-stream")
#* @get /download
function(req, res){
  q      <- req$args
  format <- tolower(q[["format"]] %||% "csv")
  df_out <- apply_query(data, q, do_page = FALSE)

  fn_base <- sprintf("filtered_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))

  if (format == "xlsx"){
    tmp <- tempfile(fileext = ".xlsx")
    writexl::write_xlsx(df_out, tmp)
    res$setHeader("Content-Disposition",
                  paste0('attachment; filename="', fn_base, '.xlsx"'))
    res$setHeader("Content-Type",
                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    readBin(tmp, "raw", n = file.info(tmp)$size)

  } else if (format == "dta"){
    tmp <- tempfile(fileext = ".dta")
    haven::write_dta(df_out, tmp)
    res$setHeader("Content-Disposition",
                  paste0('attachment; filename="', fn_base, '.dta"'))
    res$setHeader("Content-Type", "application/x-stata-dta")
    readBin(tmp, "raw", n = file.info(tmp)$size)

  } else {                  # default = CSV
    res$setHeader("Content-Disposition",
                  paste0('attachment; filename="', fn_base, '.csv"'))
    res$setHeader("Content-Type", "text/csv")
    paste(capture.output(write.csv(df_out, row.names = FALSE, na = "")),
          collapse = "\n")
  }
}

#* @get /
function(){
  list(status = "ok")
}

# ---------------------------------------------------------------------------
# 7.  Run when invoked directly (unchanged)
# ---------------------------------------------------------------------------
if (sys.nframe() == 0L){
  port <- as.integer(Sys.getenv("PORT", 8000))
  plumber::plumb()$run(host = "0.0.0.0", port = port)
}