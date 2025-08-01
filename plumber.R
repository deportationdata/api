# plumber.R
library(plumber)
library(dplyr)
library(readr)
library(writexl)
library(haven)

# Load data from CSV at startup (replace 'path/to/data.csv' with your file path)

data <- arrow::read_feather("https://github.com/deportationdata/ice/releases/latest/download/arrests-latest.feather")

#* @apiTitle DataTables Server-Side API
#* @apiDescription Provides paged, filtered, sorted data for DataTables

#* @filter cors
function(req, res) {
  # Specify the allowed origin (adjust to your Quarto page's origin or use "*" for all)
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  
  # If this is a preflight OPTIONS request, respond and end filter chain
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200 
    return(list())
  }
  
  plumber::forward()  # forward to next handler for real requests
}

# ---- shared helper ----------------------------------------------------------
apply_query <- function(df, q, do_page = TRUE) {
  # --- global search ---------------------------------------------------------
  if (!is.null(q[["search[value]"]]) && nzchar(q[["search[value]"]])) {
    term <- q[["search[value]"]]
    df <- 
      df |> 
      filter(if_any(everything(), 
             ~ grepl(term, as.character(.x), ignore.case = TRUE)))
  }

  # --- ordering (first instruction only) ------------------------------------
  if (!is.null(q[["order[0][column]"]])) {
    col_idx <- as.integer(q[["order[0][column]"]]) + 1L
    if (!is.na(col_idx) && col_idx <= ncol(df)) {
      col_nm <- names(df)[col_idx]
      dir    <- q[["order[0][dir]"]]
      df     <- if (identical(dir, "desc"))
                  df |> arrange(desc(.data[[col_nm]]))
               else df |> arrange(     .data[[col_nm]])
    }
  }

  # --- optional paging -------------------------------------------------------
  if (do_page) {
    start  <- as.integer(q[["start"]]  %||% 0L)
    length <- as.integer(q[["length"]] %||% 10L)
    if (!is.na(start) && !is.na(length) && length > 0) {
      end <- min(start + length, nrow(df))
      df  <- if (start <= end) df[(start + 1L):end, ] else df[0, ]
    }
  }
  df
}

#* @serializer unboxedJSON list(dataframe = "values", auto_unbox = TRUE, na = "null")
#* @get /data
function(req) {
  q      <- req$args
  df_out <- apply_query(data, q, do_page = TRUE)

  list(
    draw            = as.integer(q[["draw"]] %||% 0),
    recordsTotal    = nrow(data),
    recordsFiltered = nrow(apply_query(data, q, do_page = FALSE)),
    data            = df_out
  )
}

#* Download filtered dataset as CSV, Excel, or Stata
#* @serializer contentType list(type = "application/octet-stream")
#* @get /download
function(req, res) {
  q      <- req$args
  format <- tolower(q[["format"]] %||% "csv")
  df_out <- apply_query(data, q, do_page = FALSE)

  filename_base <- sprintf("filtered_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))

  if (format == "xlsx") {
    tmp <- tempfile(fileext = ".xlsx")
    writexl::write_xlsx(df_out, tmp)
    res$setHeader("Content-Disposition", paste0('attachment; filename="', filename_base, '.xlsx"'))
    res$setHeader("Content-Type", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    return(readBin(tmp, what = "raw", n = file.info(tmp)$size))

  } else if (format == "dta") {
    tmp <- tempfile(fileext = ".dta")
    haven::write_dta(df_out, tmp)
    res$setHeader("Content-Disposition", paste0('attachment; filename="', filename_base, '.dta"'))
    res$setHeader("Content-Type", "application/x-stata-dta")
    return(readBin(tmp, what = "raw", n = file.info(tmp)$size))

  } else {
    # default = CSV
    res$setHeader("Content-Disposition", paste0('attachment; filename="', filename_base, '.csv"'))
    res$setHeader("Content-Type", "text/csv")
    return(paste(capture.output(write.csv(df_out, row.names = FALSE, na = "")), collapse = "\n"))
  }
}

#* @get /
function() {
  list(status = "ok")
}

# Start the API only if this file is the main entrypoint
if (sys.nframe() == 0L) {
  port <- as.integer(Sys.getenv("PORT", 8000))
  host <- "0.0.0.0"
  pr <- plumber::plumb()
  pr$run(host = host, port = port)
}