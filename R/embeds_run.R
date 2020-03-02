#' Run embeds app
#' 
#' @import shiny
#' 
#' @export 
run_embeds <- function(){
  addResourcePath(
    'www', system.file('app/www', package = 'inndxweb')
  )
  shinyApp(ui = embeds_ui, server = embeds_server)
}