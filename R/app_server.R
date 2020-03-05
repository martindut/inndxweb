#' @import shiny
app_server <- function(input, output,session) {
  data <- golem::get_golem_options("data")
  
  sever::cleave("Sorry, I've encountered an error", bg_color = "#000", color = "#298614")
  
  sever::sever(
    tagList(
      h1("Whoops!"),
      p("It looks like you were disconnected"),
      shiny::tags$button(
        "Reload",
        style = "color:#000;background-color:#fff;",
        class = "button button-raised",
        onClick = "location.reload();"
      )
    ),
    bg_color = "#000"
  )
  
  echarts4r::e_common(
    font_family = "Quicksand",
    theme = theme
  )
  
  
  if(is.null(data)){
    data <- get_data()
    
  } else {
    
  }
  
  #df_reconitems %>% dplyr::count(ReconStatus)
  
  # r <- reactiveValues(
  #   mod_grid = reactiveValues(playing = "onload", start = FALSE),
  #   mod_timer = reactiveValues(),
  #   mod_bomb = reactiveValues(),
  #   mod_welcome = reactiveValues(firstVisit = TRUE),
  #   mod_scores = reactiveValues(refresh = NULL),
  #   click = reactiveValues(counter = 0),
  #   currentTab = reactiveValues(val = NULL),
  #   warrior = reactiveValues(mode = FALSE),
  #   cookies = reactiveValues(),
  #   device = reactiveValues(info = NULL)
  # )
  
  # welcome module
  #callModule(mod_welcome_server, "welcome_ui_1", r = r)
  
  
  # df load logic

  callModule(mod_count_server, "count_ui_1", df = data$df, type_filter = "positions")
  callModule(mod_count_server, "count_ui_2", df = data$df, type_filter = "recons")
  callModule(mod_count_server, "count_ui_4", df = data$df, type_filter = "marketvalue")
  
  
  # -------------------- Load tab by tab for more responsiveness
  
  # track initialised tabs
  df_init <- rc_init <- TRUE
  
  w <- waiter::Waiter$new(html = loader, color = "#000")
  
  observeEvent(input$tabs, {
    
    if(all(input$tabs == "DailyFiles", df_init)){
      
      w$show()
      
      df_init <- FALSE
      # dxy tab
      
      callModule(
        mod_dailyfiles_trend_server, "dailyfiles_trend",
        df = data$df_dailyfiles_all
      )
      
      callModule(
        mod_dailyfiles_rate_server, "dailyfiles_rate_ui_1",
        df = data$df_dailyfiles_sum
      )
      
      callModule(mod_count_server, "count_ui_1_df", df = data$df_dailyfiles_today_sum, type_filter = "rawdata")
      callModule(mod_count_server, "count_ui_2_df", df = data$df_dailyfiles_today_sum, type_filter = "snapshot")
      callModule(mod_count_server, "count_ui_3_df", df = data$df_dailyfiles_today_sum, type_filter = "other")
      
      #table
      callModule(mod_table_normal_server, "table_dailyfiles", df = data$df_dailyfiles_today_sum)
      
      w$hide()
      
    } else if(all(input$tabs == "Recons", rc_init)){
      w$show()
      rc_init <- FALSE
      w$hide()
      
    } 

  })
  
}
