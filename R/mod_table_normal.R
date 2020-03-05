# Module UI

#' @title   mod_table_normal_ui and mod_table_normal_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_table
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_table_normal_ui <- function(id, label){
  ns <- NS(id)
  
  tableOutput(ns("data"))
}

# Module Server

#' @rdname mod_table
#' @export
#' @keywords internal

mod_table_normal_server <- function(input, output, session, df){
  ns <- session$ns
  
  # datatable
  output$data <- renderTable({
    df
  }, rownames = TRUE)
}
