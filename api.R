# plumber.R
library(plumber)
library(dplyr)
library(readr)

# Load data from CSV at startup (replace 'path/to/data.csv' with your file path)
data <- penguins

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

#* @serializer unboxedJSON list("dataframe" = "rows", "auto_unbox" = TRUE, "na" = "null")
#* @get /data
function(req, res) {
  query <- req$args  # query parameters sent by DataTables

  total_records <- nrow(data)

  if (!is.null(query[["search[value]"]]) && query[["search[value]"]] != "") {
    search_term <- query[["search[value]"]]
    data_filtered <- data |> 
      filter(if_any(everything(), ~ grepl(search_term, as.character(.x), ignore.case = TRUE)))
  } else {
    data_filtered <- data
  }

  filtered_records <- nrow(data_filtered)

  order_col_index <- query[["order[0][column]"]]
  order_dir <- query[["order[0][dir]"]]
  if (!is.null(order_col_index) && order_col_index != "") {
    col_index <- as.integer(order_col_index) + 1
    if (!is.na(col_index) && col_index >= 1 && col_index <= ncol(data_filtered)) {
      col_name <- names(data_filtered)[col_index]
      data_filtered <- if (order_dir == "desc") {
        data_filtered |> arrange(desc(.data[[col_name]]))
      } else {
        data_filtered |> arrange(.data[[col_name]])
      }
    }
  }

  start <- ifelse(!is.null(query[["start"]]), as.integer(query[["start"]]), 0)
  length <- ifelse(!is.null(query[["length"]]), as.integer(query[["length"]]), 10)
  if (!is.na(start) && !is.na(length)) {
    end <- min(start + length, nrow(data_filtered))
    data_page <- if (start <= end) data_filtered[(start + 1):end, ] else data_filtered[0, ]
  } else {
    data_page <- data_filtered
  }

  list(
    draw            = as.integer(query[["draw"]] %||% 0),
    recordsTotal    = total_records,
    recordsFiltered = filtered_records,
    data            = data_page
  )
}