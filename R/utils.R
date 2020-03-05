# constants
theme <- "dark"

deaths_pal <- c("#263238", "#455a64", "#607d8b", "#90a4ae", "#cfd8dc") %>% rev()

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
  
  # pool::dbPool(
  #   RPostgres::Postgres(),
  #   host = config$database$host,
  #   user = config$database$user,
  #   password = config$database$password,
  #   dbname = config$database$name,
  #   port = 5432
  # )
}

#' @rdname connect
#' @keywords internal
disconnect <- function(con = NULL){
  if(!is.null(con))
    pool::poolClose(con)
}


get_data <- function(){
  
  cdate <- Sys.Date()
  idate <- inndxtdr::datetoint(cdate)
  pbday <- inndxtdr::businessdaylast(cdate)
  
  inndxtdr::pins_connect()
  
  df_dailyfiles_all <- pins::pin_get(name = paste0("dailyfiles/dailyfile_summary"), board = "azure", cache = FALSE)
  df_dailyfiles_today <- df_dailyfiles_all %>% dplyr::filter(valuationdate == max(valuationdate))
  df_dailyfiles_sum <- df_dailyfiles_all %>% dplyr::group_by(valuationdate, type) %>% dplyr::summarise(cases = sum(n, na.rm = TRUE))
  
  df_dailyfiles_today_sum <- df_dailyfiles_today %>% 
    dplyr::rename(
      type_x = type,
      type = root,
      cases = n
    ) %>%
    dplyr::group_by(valuationdate, type) %>% 
    dplyr::summarise(
      cases = sum(cases, na.rm = TRUE)
    )
  
  
  df_reconitems <- pins::pin_get(name = paste0("reconitems/recon_summary"), board = "azure", cache = FALSE)
  
  df_ur <- pins::pin_get(name = paste0("unrealised/unrealised_summary"), board = "azure", cache = FALSE)
  
  obxdbs <- df_ur %>% 
    dplyr::select(obelixdbname) %>% 
    dplyr::mutate(obelixdbname = stringr::str_to_lower(obelixdbname)) %>% 
    dplyr::distinct()
  
  ri_dbs_today <- df_reconitems %>% 
    dplyr::filter(valuationdate == pbday, recon_status != "ReconOK") %>% 
    dplyr::group_by(obelixdbname) %>% 
    dplyr::summarise(cases = sum(positions, na.rm = TRUE)) %>% 
    dplyr::mutate(type = "ri") %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(obelixdbname = stringr::str_to_lower(obelixdbname))      
  
  ri_dbs_today <- obxdbs %>%
    dplyr::left_join(
      ri_dbs_today, by = "obelixdbname"
    ) %>% 
    dplyr::mutate(
      type = "ri",
      cases = dplyr::if_else(is.na(cases), 0L, cases)
    )
  
  ur_dbs_today <- df_ur %>% 
    dplyr::filter(valuationdate == pbday) %>% 
    dplyr::group_by(obelixdbname) %>% 
    dplyr::summarise(cases = sum(positions, na.rm = TRUE)) %>% 
    dplyr::mutate(type = "ur") %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(obelixdbname = stringr::str_to_lower(obelixdbname))
  
  ur_dbs_today <- obxdbs %>%
    dplyr::left_join(
      ur_dbs_today, by = "obelixdbname"
    ) %>% 
    dplyr::mutate(
      type = "ur",
      cases = dplyr::if_else(is.na(cases), 0L, cases)
    )
  
  
  dbs_today <- dplyr::bind_rows(ur_dbs_today, ri_dbs_today) 
  #%>% tidyr::pivot_wider(names_from = c(type), values_from = c(cases))
  
  obxdbs <- obxdbs %>% 
    dplyr::left_join(
      dbs_today, by = "obelixdbname"
    )
  
  df_ur_sum <- df_ur %>% 
    dplyr::group_by(valuationdate, companyname, assetclass) %>% 
    dplyr::summarise(
      positions = sum(positions, na.rm = TRUE),
      aua = sum(marketvalue, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup()
  
  df1 <- df_dailyfiles_today %>% 
    dplyr::group_by(valuationdate) %>% 
    dplyr::summarise(cases = sum(n, na.rm = TRUE)) %>% 
    dplyr::mutate(type = "dailyfiles")  
  
  df2 <- df_reconitems %>% 
    dplyr::filter(recon_status != "ReconOK", valuationdate == max(valuationdate)) %>% 
    dplyr::group_by(valuationdate) %>% 
    dplyr::summarise(cases = sum(positions, na.rm = TRUE)) %>% 
    dplyr::mutate(type = "recons")  
  
  df3 <- df_ur_sum %>% 
    dplyr::filter(valuationdate == max(valuationdate)) %>% 
    dplyr::group_by(valuationdate) %>% 
    dplyr::summarise(cases = sum(positions, na.rm = TRUE)) %>% 
    dplyr::mutate(type = "positions")  
  
  
  df4 <- df_ur_sum %>% 
    dplyr::filter(valuationdate == max(valuationdate)) %>% 
    dplyr::group_by(valuationdate) %>% 
    dplyr::summarise(cases = sum(aua, na.rm = TRUE)) %>% 
    dplyr::mutate(type = "marketvalue")  
  
  df <- dplyr::bind_rows(df1, df2, df3, df4)
  
  data <- list(df = df, df_dailyfiles_all = df_dailyfiles_all, df_dailyfiles_sum = df_dailyfiles_sum, 
               df_dailyfiles_today_sum = df_dailyfiles_today_sum)
  
  return(data)
  
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