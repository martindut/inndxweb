#' @import shiny
app_ui <- function() {
  version <- paste0("v", packageVersion("inndxweb"))
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    f7Page(
      title = "Inndx Web",
      dark_mode = TRUE,
      init = f7Init(
        skin = "md", 
        theme = "dark"
      ),
      f7TabLayout(
        navbar = f7Navbar(
          title = "Inndx Operations",
          hairline = FALSE,
          shadow = TRUE,
          left_panel = TRUE,
          right_panel = FALSE
        ),
        panels = tagList(
          f7Panel(
            title = "About", 
            side = "left", 
            theme = "dark",
            effect = "cover",
            p("Write something."),
            f7Link(label = "Author", src = "https://inndx.net", external = TRUE),
            tags$pre(tags$code(version))
          )
        ),
        f7Tabs(
          animated = TRUE,
          id = 'tabs',
          f7Tab(
            tabName = "Home",
            icon = f7Icon("info_circle", old = FALSE),
            active = TRUE,
            swipeable = TRUE,
            waiter::waiter_show_on_load(loader, color = "#000"),
            #h2("Home Page", class = "center"),
            #p(tags$small("Data on China and the rest of the World."), class = "center"),
            #mod_welcome_ui("welcome_ui_1"),
            f7Row(
              f7Col(
                mod_count_ui("count_ui_1", "Positions"),
              ),
              f7Col(
                mod_count_ui("count_ui_2", "Recon Items")
              ),
              f7Col(
                mod_count_ui("count_ui_4", "AUA")
              )
            )
          ),
          f7Tab(
            tabName = "Recons",
            icon = f7Icon("waveform_path", old = FALSE),
            swipeable = TRUE,
            active = FALSE,
            h2("Recon Items", class = "center"),
            #p(Sys.info()["user"], class = "center")
            
          ),
          f7Tab(
            tabName = "DailyFiles",
            icon = f7Icon("graph_circle", old = FALSE),
            swipeable = TRUE,
            active = FALSE,
            h2("Daily Files", class = "center"),
            #p(tags$small("Data on China."), class = "center"),
            waiter::waiter_hide_on_render("count_ui_1-cnt"),
            f7Row(
              f7Col(
                mod_count_ui("count_ui_1_df", "Raw Data"),
              ),
              f7Col(
                mod_count_ui("count_ui_2_df", "Snapshots")
              ),
              f7Col(
                mod_count_ui("count_ui_3_df", "Other")
              )
            ),
            f7Row(
              f7Col(mod_dailyfiles_trend_ui("dailyfiles_trend")),
              f7Col(mod_dailyfiles_rate_ui("dailyfiles_rate_ui_1"))
            ),            
            mod_table_normal_ui("table_dailyfiles", "Daily Files")
          ),
          f7Tab(
            tabName = "News",
            icon = f7Icon("quote_bubble", old = FALSE),
            swipeable = TRUE,
            active = FALSE,
            #mod_news_ui("news")
          )
        )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  #logger::log_info("golem_add_external_resources")
  
  addResourcePath(
    'www', system.file('app', package = 'inndxweb')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    waiter::use_waiter(include_js = FALSE),
    tags$link(rel="stylesheet", type="text/css", href="www/style.css"),
    tags$script(src="www/copy.js"),
    sever::use_sever(),
    shinyscroll::use_shinyscroll()
    )#,
#     HTML(
#       "
# <!-- Global site tag (gtag.js) - Google Analytics -->
# <script async src='https://www.googletagmanager.com/gtag/js?id=UA-74544116-1'></script>
# <script>
#   window.dataLayer = window.dataLayer || [];
#   function gtag(){dataLayer.push(arguments);}
#   gtag('js', new Date());
# 
#   gtag('config', 'UA-74544116-1');
# </script>"
#     ),
#     tags$meta(property="og:title", content="Coronavirus Tracker"),
#     tags$meta(property="og:type", content="article"),
#     tags$meta(property="og:url", content="https://shiny.john-coene.com/coronavirus"),
#     tags$meta(property="og:image", content="https://shiny.john-coene.com/coronavirus/www/coronavirus.png"),
#     tags$meta(property="og:description", content="A Coronavirus tracker app using John Hopkins and Weixin Data"),
#     tags$meta(name="twitter:card", content="summary_large_image"),
#     tags$meta(name="twitter:site", content="@jdatap"),
#     tags$meta(name="twitter:title", content="Coronavirus Tracker"),
#     tags$meta(name="twitter:description", content="A Coronavirus tracker app using John Hopkins and Weixin Data"),
#     tags$meta(name="twitter:image:src", content="https://shiny.john-coene.com/coronavirus/www/coronavirus.png"),
#     shiny::tags$link(rel = "apple-touch-icon", href = "www/icons/apple-touch-icon.png"),
#     
#     # pwa materials (this is not a permanent fix). shinyMobile will
#     # handle this better soon!
#     tags$link(rel = "manifest", href = "www/manifest.json"),
#     tags$meta(name="theme-color", content="#000000")
#     
#     # Launch screen for IOS
#     # Below is just an example in case you would like to try
#     # This is done by default for Android but not iOS :(
#     # Also you would have to handle as many devices as possible ...
#     # shiny::tags$link(href = "www/splashscreens/iphone5_splash.png", media = "(device-width: 320px) and (device-height: 568px) and (-webkit-device-pixel-ratio: 2)", rel = "apple-touch-startup-image")
#     
#   )
}

loader <- tagList(
  waiter::spin_loaders(42),
  br(),
  h3("Loading data")
)