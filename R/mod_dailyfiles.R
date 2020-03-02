# Module UI
  
#' @title   mod_dailyfiles_ui and mod_dailyfiles_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dailyfiles
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_dailyfiles_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_dailyfiles
#' @export
#' @keywords internal
    
mod_dailyfiles_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_dailyfiles_ui("dailyfiles_ui_1")
    
## To be copied in the server
# callModule(mod_dailyfiles_server, "dailyfiles_ui_1")
 
