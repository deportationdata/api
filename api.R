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

#* @serializer unboxedJSON
#* @get /data
function(req, res) {
  query <- req$args  # query parameters sent by DataTables

  # Total records before filtering
  total_records <- nrow(data)

  # **Global Search Filter**: filter across all columns if a search term is provided
  if (!is.null(query[["search[value]"]]) && query[["search[value]"]] != "") {
    search_term <- query[["search[value]"]]
    data_filtered <- data |> 
      filter(if_any(everything(), ~ grepl(search_term, as.character(.x), ignore.case = TRUE)))
  } else {
    data_filtered <- data
  }

  # Number of records after filtering
  filtered_records <- nrow(data_filtered)

  # **Sorting**: apply ordering if specified (DataTables may send multiple orders; handle first for simplicity)
  order_col_index <- query[["order[0][column]"]]
  order_dir <- query[["order[0][dir]"]]
  if (!is.null(order_col_index) && order_col_index != "") {
    col_index <- as.integer(order_col_index) + 1  # DataTables index is 0-based, R is 1-based
    if (!is.na(col_index) && col_index >= 1 && col_index <= ncol(data_filtered)) {
      col_name <- names(data_filtered)[col_index]
      data_filtered <- if (order_dir == "desc") {
        data_filtered |> arrange(desc(.data[[col_name]]))
      } else {
        data_filtered |> arrange(.data[[col_name]])
      }
    }
  }

  # **Paging**: extract the requested page of data
  start <- ifelse(!is.null(query[["start"]]), as.integer(query[["start"]]), 0)
  length <- ifelse(!is.null(query[["length"]]), as.integer(query[["length"]]), 10)
  if (!is.na(start) && !is.na(length)) {
    # Ensure the slice indices are within bounds
    end <- min(start + length, nrow(data_filtered))
    if (start <= end) {
      data_page <- data_filtered[(start + 1):end, ]  # +1 because R indices start at 1
    } else {
      data_page <- data_filtered[0, ]  # empty slice if out of bounds
    }
  } else {
    data_page <- data_filtered
  }

  # **Prepare JSON response using dataframe = "rows"**
  jsonlite::toJSON(
    list(
      draw            = as.integer(query[["draw"]] %||% 0),      # draw counter
      recordsTotal    = total_records,
      recordsFiltered = filtered_records,
      data            = data_page
    ),
    dataframe = "rows",   # << Key fix: each row becomes a named object
    auto_unbox = TRUE
  )
}
