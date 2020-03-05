# Module UI

#' @title   mod_dailyfiles_trend_ui and mod_dailyfiles_trend_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_new_cases
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_dailyfiles_trend_ui <- function(id){
  ns <- NS(id)
  
  f7Card(
    title = "Daily Files",
    echarts4r::echarts4rOutput(ns("plot"), height = 250)
    #, footer = uiOutput(ns("copy"))
  )
}

# Module Server

#' @rdname mod_new_cases
#' @export
#' @keywords internal

mod_dailyfiles_trend_server <- function(input, output, session, df){
  ns <- session$ns
  
  #embed_url <- golem::get_golem_options("embed_url")
  
  # output$copy <- renderUI({
  #   copy(embed_url, "jhu", "&chart=cases-added")
  # })
  
  output$plot <- echarts4r::renderEcharts4r({
    mod_dailyfiles_trend_echarts(df)
  })
}

mod_dailyfiles_trend_echarts <- function(df){
  df %>% 
    dplyr::group_by(valuationdate) %>%
    dplyr::summarise(cases = sum(n, na.rm = TRUE)) %>%  
    #dplyr::group_by(type) %>% 
    dplyr::arrange(valuationdate) %>% 
    dplyr::mutate(
      cases_lag = dplyr::lag(cases),
      diff = cases - cases_lag
    ) %>% 
    #dplyr::group_by(type) %>% 
    echarts4r::e_charts(valuationdate) %>% 
    echarts4r::e_bar(diff) %>% 
    # echarts4r::e_color(
    #   c(confirmed_pal[3], deaths_pal[4], recovered_pal[4])
    # ) %>% 
    echarts4r::e_tooltip(
      trigger = "axis",
      axisPointer = list(
        type = "shadow"
      )
    ) %>% 
    echarts4r::e_legend(
      selectedMode = "single",
      selected = list(
        "confirmed" = TRUE
      )
    ) %>% 
    echarts4r::e_hide_grid_lines("x") %>% 
    #echarts4r::e_group("JHU") %>%
    echarts4r::e_theme(theme)
}
