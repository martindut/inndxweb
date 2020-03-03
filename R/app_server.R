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
    inndxtdr::pins_connect()

    df_dailyfiles_all <- pins::pin_get(name = paste0("dailyfiles/", idate, "_summary"), board = "azure", cache = FALSE)
    
    df_dailyfiles <- df_dailyfiles_all %>% dplyr::filter(date == as.Date(cdate))
    
    df_reconitems <- pins::pin_get(name = paste0("reconitems/", idate), board = "azure", cache = FALSE)
    
    df_reconitems <- df_reconitems %>% dplyr::mutate(date = cdate)

  } else {
    
  }
  
  df_reconitems %>% dplyr::count(ReconStatus)
  
  # df load logic

  df1 <- df_dailyfiles %>% dplyr::group_by(date) %>% dplyr::summarise(cases = sum(n, na.rm = TRUE)) %>% dplyr::mutate(type = "dailyfiles")  
  
  df2 <- df_reconitems %>% dplyr::filter(ReconStatus != "ReconOK") %>%  dplyr::count(date, name = "cases") %>% dplyr::mutate(type = "recons")  
  
  df3 <- df_reconitems %>% dplyr::count(date, ObelixDatabaseName) %>% dplyr::select(-n) %>% dplyr::count(date, name = "cases") %>% dplyr::mutate(type = "dbs")  
  
  df4 <- df_reconitems %>% dplyr::group_by(date) %>% dplyr::summarise(cases = sum(HP_Value, na.rm = TRUE)) %>% dplyr::mutate(type = "marketvalue")  
    
  df <- dplyr::bind_rows(df1, df2, df3, df4)
  
  callModule(mod_count_server, "count_ui_1", df = df, type_filter = "dailyfiles")
  callModule(mod_count_server, "count_ui_2", df = df, type_filter = "recons")
  callModule(mod_count_server, "count_ui_3", df = df, type_filter = "dbs")
  callModule(mod_count_server, "count_ui_4", df = df, type_filter = "marketvalue")
  
  
  # -------------------- Load tab by tab for more responsiveness
  
  # track initialised tabs
  dxy_init <- jhu_init <- wx_init <- TRUE
  
  w <- waiter::Waiter$new(html = loader, color = "#000")
  
  observeEvent(input$tabs, {
    
    if(all(input$tabs == "DailyFiles", dxy_init)){
      
      w$show()
      
      dxy_init <- FALSE
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
      
    } else if(all(input$tabs == "John Hopkins", jhu_init)){
      
    } else if(all(input$tabs == "John Hopkins", jhu_init)){
      jhu_init <- FALSE
      
      w$show()
      
      # cases added daily
      callModule(mod_new_cases_server, "new_cases", df = df)
      
      # china vs world
      callModule(mod_china_others_server, "china_others", df = df)
      
      # jhu tab
      callModule(mod_count_server, "count_ui_1_jhu", df = df, type_filter = "confirmed")
      callModule(mod_count_server, "count_ui_2_jhu", df = df, type_filter = "death")
      callModule(mod_count_server, "count_ui_3_jhu", df = df, type_filter = "recovered")
      
      # trend
      callModule(mod_trend_server, "trend_ui_1", df = df)
      
      # maps
      callModule(mod_map_server, "map_ui_1", df = df)
      callModule(mod_world_server, "world_ui_1", df = df)
      callModule(mod_time_provinces_server, "time_provinces_1", df = df)
      
      # tables
      callModule(mod_china_server, "table_china", df = df)
      callModule(mod_table_world_server, "table_world", df = df)
      
      # death rate
      callModule(mod_jhu_death_rate_server, "jhu_death_rate_ui_1", df = df)
      
      w$hide()
      
    } else if(all(input$tabs == "Weixin", wx_init)){
      wx_init <- FALSE
      
      w$show()
      
      # weixin tab
      callModule(
        mod_count_weixin_server, "count_weixin_ui_1_wx", 
        df = china_total, column = "confirm"
      )
      callModule(
        mod_count_weixin_server, "count_weixin_ui_2_wx", 
        df = china_total, column = "dead"
      )
      callModule(
        mod_count_weixin_server, "count_weixin_ui_3_wx", 
        df = china_total, column = "heal"
      )
      callModule(
        mod_count_weixin_server, "count_weixin_ui_4_wx", 
        df = china_total, column = "suspect"
      )
      
      # weixin tab chart
      callModule(mod_china_trend_server, "china_trend_ui_confirm", df = china_daily, column = "confirm")
      callModule(mod_china_trend_server, "china_trend_ui_heal", df = china_daily, column = "heal")
      callModule(mod_china_trend_server, "china_trend_ui_dead", df = china_daily, column = "dead")
      callModule(
        mod_china_trend_server, 
        "china_trend_ui_suspect", 
        df = china_daily, 
        column = "suspect",
        connect = TRUE
      )
      
      w$hide()
    }
  })
  
}
