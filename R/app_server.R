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
  
  cdate <- Sys.Date()
  idate <- inndxtdr::datetoint(cdate)
  
  if(is.null(data)){
    library(magrittr)
    inndxtdr::pins_connect()

    df_dailyfiles_all <- pins::pin_get(name = paste0("dailyfiles/dailyfile_summary"), board = "azure", cache = FALSE)
    
    df_dailyfiles <- df_dailyfiles_all %>% dplyr::filter(date == as.Date(cdate))
    
    df_reconitems <- pins::pin_get(name = paste0("reconitems/recon_summary"), board = "azure", cache = FALSE)
    
    df_reconitems <- df_reconitems %>% dplyr::mutate(date = cdate)
    
  } else {
    
  }
  
  #df_reconitems %>% dplyr::count(ReconStatus)
  
  # df load logic

  df1 <- df_dailyfiles %>% dplyr::group_by(date) %>% dplyr::summarise(cases = sum(n, na.rm = TRUE)) %>% dplyr::mutate(type = "dailyfiles")  
  
  df2 <- df_reconitems %>% dplyr::filter(ReconStatus != "ReconOK") %>% dplyr::group_by(date) %>% dplyr::summarise(cases = sum(n, na.rm = TRUE)) %>% dplyr::mutate(type = "recons")  
  
  #df3 <- df_reconitems %>% dplyr::count(date, ObelixDatabaseName) %>% dplyr::select(-n) %>% dplyr::count(date, name = "cases") %>% dplyr::mutate(type = "dbs")  
  
  df4 <- df_reconitems %>% dplyr::group_by(date) %>% dplyr::summarise(cases = sum(HP_Value, na.rm = TRUE)) %>% dplyr::mutate(type = "marketvalue")  
    
  df <- dplyr::bind_rows(df1, df2, df3, df4)
  
  callModule(mod_count_server, "count_ui_1", df = df, type_filter = "dailyfiles")
  callModule(mod_count_server, "count_ui_2", df = df, type_filter = "recons")
  callModule(mod_count_server, "count_ui_3", df = df, type_filter = "dbs")
  callModule(mod_count_server, "count_ui_4", df = df, type_filter = "marketvalue")
  
  
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
        df = df_dailyfiles_all
      )
      
      df_dailyfiles_sum <- df_dailyfiles %>% 
        dplyr::rename(
          type_x = type,
          cases = n
        ) %>% 
        dplyr::mutate(
          type = dplyr::case_when(
            grepl("rawdata_", type_x) ~ "rawdata",
            grepl("snapshot_", type_x) ~ "snapshot",
            TRUE ~ "other"
          )
        )
      
      callModule(mod_count_server, "count_ui_1_df", df = df_dailyfiles_sum, type_filter = "rawdata")
      callModule(mod_count_server, "count_ui_2_df", df = df_dailyfiles_sum, type_filter = "snapshot")
      callModule(mod_count_server, "count_ui_3_df", df = df_dailyfiles_sum, type_filter = "other")
      
      # table
      #callModule(mod_dxy_table_server, "dxy_table_ui_1", df = dxy)
      
      w$hide()
      
    } else if(all(input$tabs == "Recons", rc_init)){
      w$show()
      rc_init <- FALSE
      w$hide()
      
    } 

  })
  
}
