# Module UI

#' @title   mod_table_ui and mod_table_server
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
mod_table_ui <- function(id, label){
  ns <- NS(id)

    f7ExpandableCard(
      title = label,
      id = paste0(id, "_card"),
      color = "green",
      #subtitle = "Cases by provinces in China",
      uiOutput(ns("table"))
  )
}

# Module Server

#' @rdname mod_table
#' @export
#' @keywords internal

mod_table_server <- function(input, output, session, df){
  ns <- session$ns
  
  output$table <- renderUI({
    df %>% 
      as_f7_table(card = TRUE)
  })
}
