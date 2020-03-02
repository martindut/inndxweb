# constants
theme <- "dark"
#' Retrieve Config
#' 
#' Retrieves config file.
#' 
#' @keywords internal
get_config <- function(){
  has_config()
  config <- yaml::read_yaml(config_file)
  check_config(config)
  return(config)
}

check_config <- function(config){
  if(config$database$user == "me" && config$database$password == "password" && config$database$name == "name")
    stop("Complete the config file: _inndxweb.yml")
  
  # if(!length(config$newsapi))
  #   warning("No newsapi key found in config not crawling news.")
  
  invisible()
}

#' Has Config
#' 
#' Ensure config file is present.
#' 
#' @keywords internal
has_config <- function(){
  has_config <- file.exists(config_file)
  if(!has_config)
    stop(
      "Missing config file, see `create_config`", call. = FALSE
    )
  
  invisible()
}


#' Connect
#' 
#' Connect to database.
#' 
#' @param con Output of \code{connect}.
#' 
#' @rdname connect
#' @keywords internal
connect <- function(){
  config <- get_config()
  
  has_vars <- all(c("user", "password", "host") %in% names(config$database))
  
  if(!has_vars)
    stop("Missing variables in config file, see `create_config`", call. = FALSE)
  
  pool::dbPool(
    RPostgres::Postgres(),
    host = config$database$host,
    user = config$database$user,
    password = config$database$password,
    dbname = config$database$name,
    port = 5432
  )
}

#' @rdname connect
#' @keywords internal
disconnect <- function(con = NULL){
  if(!is.null(con))
    pool::poolClose(con)
}


#' Pivot
#' 
#' Change data from wide to long.
#' 
#' @param df Sheet.
#' 
#' @keywords internal
pivot <- function(df){
  tidyr::pivot_longer(
    df, 
    tidyselect::contains("/"),
    names_to = c("date"),
    values_to = c("cases"),
    values_ptypes = list(cases = "character")
  )
}

#' Table
#' 
#' Create shinyMobile table.
#' 
#' @param df Data.frame.
#' @param card Whether to use as card.
#' 
#' @keywords internal
as_f7_table <- function(df, card = FALSE){
  headers <- purrr::map(df, class2f7)
  colnames <- names(headers)
  
  headers <- purrr::map2(headers, colnames, function(x, y){
    tags$th(class = x, y)
  }) 
  
  df_list <- purrr::transpose(df)
  
  table <- purrr::map(df_list, function(row){
    r <- purrr::map(row, function(cell){
      tags$th(class = class2f7(cell), cell)
    })
    tags$tr(r)
  })
  
  cl <- "data-table"
  
  if(card)
    cl <- paste(cl, "card")
  
  div(
    class = cl,
    tags$table(
      tags$thead(
        tags$tr(headers)
      ),
      tags$tbody(table)
    )
  )
}

#' Get CSS class based on cell class
#' 
#' @param x Value.
#' 
#' @keywords internal
class2f7 <- function(x){
  if(inherits(x, "numeric"))
    return("numeric-cell")
  
  return("label-cell")
}


#' Round up
#' 
#' Nicely round up numbers for pieces in maps.
#' 
#' @param x Number.
#' @param nice Vector of ending.
#' 
#' @keywords internal
round_up <- function(x, nice=1:10) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}