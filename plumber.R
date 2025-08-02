# plumber.R ---------------------------------------------------------------
library(plumber)
library(dplyr)
library(lubridate)
library(writexl)
library(haven)
library(arrow)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# 1. Load dataset once -----------------------------------------------------
data <- arrow::read_feather(
  "https://github.com/deportationdata/ice/releases/latest/download/arrests-latest.feather"
)

# 2. CORS filter -----------------------------------------------------------
#* @filter cors
function(req, res){
  res$setHeader("Access-Control-Allow-Origin",  "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS"){ res$status <- 200; return(list()) }
  forward()
}

# 3.  Data endpoint ---------------------------------------------------------
#* @serializer unboxedJSON list(dataframe = "rows", auto_unbox = TRUE, na = "string", rownames   = FALSE)
#* @get /data
function(req){
  q <- req$args
  filtered <- data                               # ① start with full set

  # ---- per-column search from DataTables ------------------------------
  for (i in seq_along(filtered)) {
    key <- sprintf("columns[%d][search][value]", i - 1)
    val <- q[[key]]
    if (!is.null(val) && nzchar(val)) {
      pattern  <- gsub("^\\^|\\$$", "", val)     # strip ^…$
      col_name <- names(filtered)[i]
      filtered <- filter(filtered,
                         grepl(pattern, as.character(.data[[col_name]]),
                               ignore.case = TRUE))
    }
  }

  # ---- ordering --------------------------------------------------------
  if (!is.null(q[["order[0][column]"]])) {
    col_idx <- as.integer(q[["order[0][column]"]]) + 1L
    dir     <- q[["order[0][dir]"]]
    ord_col <- names(filtered)[col_idx]
    filtered <- if (identical(dir, "desc"))
        arrange(filtered, desc(.data[[ord_col]])) else arrange(filtered, .data[[ord_col]])
  }

  # ---- paging ----------------------------------------------------------
  start   <- as.integer(q$start  %||% 0)
  length  <- as.integer(q$length %||% 25)
  end     <- min(start + length, nrow(filtered))
  page    <- if (length > 0 && start < end) slice(filtered, (start + 1):end) else filtered[0, ]

  list(
    draw            = as.integer(q$draw %||% 0),
    recordsTotal    = nrow(data),
    recordsFiltered = nrow(filtered),
    data            = mutate(page, across(where(is.Date), as.character))
  )
}

# 4. Download endpoint -----------------------------------------------------
#* @serializer contentType list(type="application/octet-stream")
#* @get /download
function(req, res){
  q      <- req$args
  format <- tolower(q$format %||% "csv")

  df_out <- get("/data", req)$data |> as_tibble()   # reuse same logic

  fname <- sprintf("filtered_%s_%s", format, format(Sys.time(), "%Y%m%d_%H%M%S"))

  if (format == "xlsx"){
    tmp <- tempfile(fileext=".xlsx"); write_xlsx(df_out, tmp)
    res$setHeader("Content-Type", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    res$setHeader("Content-Disposition", sprintf('attachment; filename="%s.xlsx"', fname))
    readBin(tmp, "raw", n=file.info(tmp)$size)

  } else if (format == "dta"){
    tmp <- tempfile(fileext=".dta"); write_dta(df_out, tmp)
    res$setHeader("Content-Type", "application/x-stata-dta")
    res$setHeader("Content-Disposition", sprintf('attachment; filename="%s.dta"', fname))
    readBin(tmp, "raw", n=file.info(tmp)$size)

  } else {
    res$setHeader("Content-Type", "text/csv")
    res$setHeader("Content-Disposition", sprintf('attachment; filename="%s.csv"', fname))
    paste(capture.output(write.csv(df_out, row.names=FALSE, na="")), collapse="\n")
  }
}

# 5. Health ping -----------------------------------------------------------
#* @get /
function(){ list(status="ok") }

# 6. Run if script is called directly -------------------------------------
if (sys.nframe() == 0){
  plumber::plumb()$run(host="0.0.0.0", port=as.integer(Sys.getenv("PORT", 8000)))
}