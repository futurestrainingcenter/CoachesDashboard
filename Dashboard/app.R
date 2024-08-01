library(shiny)
library(shinyglide)
library(bs4Dash)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyverse)
library(lubridate)
library(DT)
library(plotly)
library(echarts4r)
library(shinyWidgets)

current_date <- Sys.Date()
current_month <- format(as.Date(current_date), "%B")
current_year <- format(current_date, "%Y")

last_month_date <- current_date %m-% months(1)
last_month <- format(as.Date(last_month_date), "%B")

# Load client data
client_data <- read_csv("/Users/watts/Downloads/FullClientList.csv") %>% 
  filter(`Client Type` %in% c("Active Member", "Active Pass Holder")) %>% 
  mutate(
    Birthdate = as.Date(`field-general-7.dl_date`),
    Age = as.numeric(difftime(Sys.Date(), Birthdate, units = "weeks")) / 52.25,
    Age = floor(Age) # Make age a whole number without rounding up
  )

# Load speed data
speed_data <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/SpeedFacilityData.csv")
speed_data$Date <- as.Date(speed_data$Date)

# Load strength data
strength_data <- read_csv("/Users/watts/Documents/Futures Performance Center/Data/Facility Data/StrengthFacilityData.csv")
strength_data$Date <- as.Date(strength_data$Date)

# Get the min and max dates from speed_data
min_date <- min(speed_data$Date, na.rm = TRUE)
max_date <- max(speed_data$Date, na.rm = TRUE)

# Get the min and max dates from speed_data
strength_min_date <- min(strength_data$Date, na.rm = TRUE)
strength_max_date <- max(strength_data$Date, na.rm = TRUE)

ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  dashboardHeader(
    status = "primary",
    title = dashboardBrand(
      title = "Coaching Dashboard",
      color = "primary",
      image = "Untitled design.png"
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("table-columns")),
      menuItem("Athlete Profiles", tabName = "profiles", icon = icon("user")),
      menuItem("Correlations", tabName = "correlations", icon = icon("chart-line")),
      menuItem("Reports", tabName = "reports", icon = icon("file"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(".datepicker {z-index:1050 !important;}"))
    ),
    tabItems(
      tabItem(tabName = "overview",
              tabBox(
                id = "overview_tabbox", width = 12,
                tabPanel("Speed Data",
                         box(title = "Summary Statistics", status = "primary", solidHeader = TRUE, width = 12,
                             fluidRow(
                               column(width = 3, dateRangeInput("speed_dateRange", "Date Range", start = min_date, end = max_date, min = min_date, max = max_date)),
                               column(width = 2, pickerInput("speed_level", "Level", choices = unique(speed_data$Level), selected = unique(speed_data$Level), options = pickerOptions(actionsBox = TRUE, selectedTextFormat = "count > 3"),  multiple = TRUE)),
                               column(width = 2, pickerInput("speed_gender", "Gender", choices = unique(speed_data$Gender), selected = unique(speed_data$Gender), options = pickerOptions(actionsBox = TRUE),  multiple = TRUE)),
                               column(width = 2, pickerInput("speed_serviceName", "Service Name", choices = unique(speed_data$ServiceName), selected = unique(speed_data$ServiceName), options = pickerOptions(actionsBox = TRUE, selectedTextFormat = "count > 3"),  multiple = TRUE))
                             ), br(),
                             fluidRow(
                               column(width = 4,
                                      value_box(
                                        title = tags$div(
                                          style = "display: flex; justify-content: space-between; width: 100%;",
                                          tags$span("Acceleration"),
                                          tags$span(uiOutput("accelerationMoM"))
                                        ),
                                        value = textOutput("accelerationValue_MoM"),
                                        showcase = plotlyOutput("accelerationPlot_MoM"),
                                        showcase_layout = "bottom",
                                        full_screen = TRUE
                                      )),
                               column(width = 4,
                                      value_box(
                                        title = tags$div(
                                          style = "display: flex; justify-content: space-between; width: 100%;",
                                          tags$span("Hard 90"),
                                          tags$span(uiOutput("hardNinetyMoM"))
                                        ),
                                        value = textOutput("hardNinetyValue_MoM"),
                                        showcase = plotlyOutput("hardNinetyPlot_MoM"),
                                        showcase_layout = "bottom",
                                        full_screen = TRUE
                                      )),
                               column(width = 4,
                                      value_box(
                                        title = tags$div(
                                          style = "display: flex; justify-content: space-between; width: 100%;",
                                          tags$span("40 Yard Dash"),
                                          tags$span(uiOutput("fortyYardMoM"))
                                        ),
                                        value = textOutput("fortyYardValue_MoM"),
                                        showcase = plotlyOutput("fortyYardPlot_MoM"),
                                        showcase_layout = "bottom",
                                        full_screen = TRUE
                                      ))
                             ),
                             fluidRow(
                               column(width = 2),
                               column(width = 4,
                                      value_box(
                                        title = tags$div(
                                          style = "display: flex; justify-content: space-between; width: 100%;",
                                          tags$span("Max Velocity"),
                                          tags$span(uiOutput("maxvelocityMoM"))
                                        ),
                                        value = textOutput("maxvelocityValue_MoM"),
                                        showcase = plotlyOutput("maxvelocityPlot_MoM"),
                                        showcase_layout = "bottom",
                                        full_screen = TRUE
                                      )),
                               column(width = 4,
                                      value_box(
                                        title = tags$div(
                                          style = "display: flex; justify-content: space-between; width: 100%;",
                                          tags$span("RSI"),
                                          tags$span(uiOutput("RSI_MoM"))
                                        ),
                                        value = textOutput("RSI_value_MoM"),
                                        showcase = plotlyOutput("RSI_plot_MoM"),
                                        showcase_layout = "bottom",
                                        full_screen = TRUE
                                      )),
                               column(width = 2)
                             )
                         ),
                         fluidRow(
                           box(title = "Attendance Trend", status = "primary", solidHeader = TRUE, width = 6,
                               echarts4rOutput("speed_attendancePlot")
                           ),
                           box(title = "Service Count", status = "primary", solidHeader = TRUE, width = 6,
                               echarts4rOutput("speed_serviceCount")
                           )
                         ),
                         box(title = "Recent Performances", status = "primary", solidHeader = TRUE, width = 12,
                             glide(id = "recent_glide", controls_position = "bottom",
                                   next_label = paste("Next Exercise", shiny::icon("chevron-right", lib = "glyphicon")),
                                   previous_label = paste(shiny::icon("chevron-left", lib = "glyphicon"), "Previous Exercise"),
                                   screen(
                                     tags$p("Acceleration", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                     DTOutput("acceleration_recent")
                                   ),
                                   screen(
                                     tags$p("Hard 90", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                     DTOutput("hardNinety_recent")
                                   ),
                                   screen(
                                     tags$p("40 Yard Dash", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                     DTOutput("fortyYard_recent")
                                   ),
                                   screen(
                                     tags$p("Max Velocity", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                     DTOutput("maxVelocity_recent")
                                   ),
                                   screen(
                                     tags$p("RSI", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                     DTOutput("RSI_recent")
                                   )
                             )
                         )
                ),
                tabPanel("Strength Data",
                         box(title = "Summary Statistics", status = "primary", solidHeader = TRUE, width = 12,
                             fluidRow(
                               column(width = 3, dateRangeInput("strength_dateRange", "Date Range", start = strength_min_date, end = strength_max_date, min = strength_min_date, max = strength_max_date)),
                               column(width = 2, pickerInput("strength_level", "Level", choices = unique(strength_data$Level), selected = unique(strength_data$Level), options = pickerOptions(actionsBox = TRUE, selectedTextFormat = "count > 3"),  multiple = TRUE)),
                               column(width = 2, pickerInput("strength_gender", "Gender", choices = unique(strength_data$Gender), selected = unique(strength_data$Gender), options = pickerOptions(actionsBox = TRUE),  multiple = TRUE)),
                               column(width = 2, pickerInput("strength_serviceName", "Service Name", choices = unique(strength_data$ServiceName), selected = unique(strength_data$ServiceName), options = pickerOptions(actionsBox = TRUE, selectedTextFormat = "count > 3"),  multiple = TRUE))
                             ), br(),
                             fluidRow(
                               column(width = 4,
                                      value_box(
                                        title = tags$div(
                                          style = "display: flex; justify-content: space-between; width: 100%;",
                                          tags$span("CMJ"),
                                          tags$span(uiOutput("CMJ_MoM"))
                                        ),
                                        value = textOutput("CMJ_Value_MoM"),
                                        showcase = plotlyOutput("CMJ_Plot_MoM"),
                                        showcase_layout = "bottom",
                                        full_screen = TRUE
                                      )),
                               column(width = 4,
                                      value_box(
                                        title = tags$div(
                                          style = "display: flex; justify-content: space-between; width: 100%;",
                                          tags$span("ISO Belt Squat"),
                                          tags$span(uiOutput("IBSQT_MoM"))
                                        ),
                                        value = textOutput("IBSQT_Value_MoM"),
                                        showcase = plotlyOutput("IBSQT_Plot_MoM"),
                                        showcase_layout = "bottom",
                                        full_screen = TRUE
                                      )),
                               column(width = 4,
                                      value_box(
                                        title = tags$div(
                                          style = "display: flex; justify-content: space-between; width: 100%;",
                                          tags$span("Squat Jump"),
                                          tags$span(uiOutput("SJ_MoM"))
                                        ),
                                        value = textOutput("SJ_Value_MoM"),
                                        showcase = plotlyOutput("SJ_Plot_MoM"),
                                        showcase_layout = "bottom",
                                        full_screen = TRUE
                                      ))
                             ),
                             fluidRow(
                               column(width = 1),
                               column(width = 5,
                                      value_box(
                                        title = tags$div(
                                          style = "display: flex; justify-content: space-between; width: 100%;",
                                          tags$span("Proteus Power"),
                                          tags$span(uiOutput("proteusPower_MoM"))
                                        ),
                                        value = textOutput("proteusPower_Value_MoM"),
                                        showcase = plotlyOutput("proteusPower_Plot_MoM"),
                                        showcase_layout = "bottom",
                                        full_screen = TRUE
                                      )),
                               column(width = 5,
                                      value_box(
                                        title = tags$div(
                                          style = "display: flex; justify-content: space-between; width: 100%;",
                                          tags$span("Proteus Acceleration"),
                                          tags$span(uiOutput("proteusAcc_MoM"))
                                        ),
                                        value = textOutput("proteusAcc_Value_MoM"),
                                        showcase = plotlyOutput("proteusAcc_Plot_MoM"),
                                        showcase_layout = "bottom",
                                        full_screen = TRUE
                                      )),
                               column(width = 1)
                             )
                         ),
                         fluidRow(
                           box(title = "Attendance Trend", status = "primary", solidHeader = TRUE, width = 6,
                               echarts4rOutput("strength_attendancePlot")
                           ),
                           box(title = "Service Count", status = "primary", solidHeader = TRUE, width = 6,
                               echarts4rOutput("strength_serviceCount")
                           )
                         ),
                         box(title = "Recent Performances", status = "primary", solidHeader = TRUE, width = 12,
                             glide(id = "recent_glide", controls_position = "bottom",
                                   next_label = paste("Next Exercise", shiny::icon("chevron-right", lib = "glyphicon")),
                                   previous_label = paste(shiny::icon("chevron-left", lib = "glyphicon"), "Previous Exercise"),
                                   screen(
                                     tags$p("ForceDeck: CMJ", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                     DTOutput("CMJ_recent")
                                   ),
                                   screen(
                                     tags$p("ForceDeck: ISO Belt Squat", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                     DTOutput("IBSQT_recent")
                                   ),
                                   screen(
                                     tags$p("ForceDeck: ShoulderISO", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                     DTOutput("SJ_recent")
                                   ),
                                   screen(
                                     tags$p("Proteus Power", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                     DTOutput("proteusPower_recent")
                                   ),
                                   screen(
                                     tags$p("Proteus Acceleration", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                     DTOutput("proteusAcc_recent")
                                   )
                             )
                         )
                )
              )
      ),
      tabItem(tabName = "profiles",
              fluidRow(
                column(width = 4,
                       box(title = "Select Athlete", status = "primary", solidHeader = TRUE, width = NULL,
                           selectInput("athlete", "Athlete", choices = sort(unique(client_data$Client))),
                           boxProfile(
                             image = "Generic-Profile-Image.png",
                             title = uiOutput("athlete_position"),
                             subtitle = uiOutput("athlete_level"),
                             bordered = TRUE,
                             boxProfileItem(
                               title = "Age",
                               description = uiOutput("athlete_age")
                             ),
                             boxProfileItem(
                               title = "Birthdate",
                               description = uiOutput("athlete_birthdate")
                             ),
                             boxProfileItem(
                               title = "Height",
                               description = uiOutput("athlete_height")
                             ),
                             boxProfileItem(
                               title = "Weight",
                               description = uiOutput("athlete_weight")
                             )
                           ),
                           br(),
                           uiOutput("athlete_physicality"),
                           uiOutput("athlete_recovery"),
                           uiOutput("athlete_nutrition"),
                           br(),
                           uiOutput("objective_one"),
                           uiOutput("objective_two")
                       ),                           
                       box(title = "Weight Trend", status = "primary", solidHeader = TRUE, width = NULL,
                           echarts4rOutput("weight_trend", height = "250px")
                       ),
                       box(title = "Percentiles", status = "primary", solidHeader = TRUE, width = NULL,
                           echarts4rOutput("radar_plot")
                       )
                ),
                column(width = 8,
                       tabBox(
                         id = "athlete_performance_tabbox", width = 12,
                         tabPanel("Speed Data",
                                  box(title = "Summary Statistics", status = "primary", solidHeader = TRUE, width = 12,
                                      fluidRow(
                                        column(width = 4,
                                               value_box(
                                                 title = tags$div(
                                                   style = "display: flex; justify-content: space-between; width: 100%;",
                                                   tags$span("Acceleration"),
                                                   tags$span(uiOutput("athlete_accelerationMoM"))
                                                 ),
                                                 value = textOutput("athlete_accelerationValue"),
                                                 showcase = plotlyOutput("athlete_accelerationPlot"),
                                                 showcase_layout = "bottom",
                                                 full_screen = TRUE
                                               )),
                                        column(width = 4,
                                               value_box(
                                                 title = tags$div(
                                                   style = "display: flex; justify-content: space-between; width: 100%;",
                                                   tags$span("Hard 90"),
                                                   tags$span(uiOutput("athlete_hardNinetyMoM"))
                                                 ),
                                                 value = textOutput("athlete_hardNinetyValue"),
                                                 showcase = plotlyOutput("athlete_hardNinetyPlot"),
                                                 showcase_layout = "bottom",
                                                 full_screen = TRUE
                                               )),
                                        column(width = 4,
                                               value_box(
                                                 title = tags$div(
                                                   style = "display: flex; justify-content: space-between; width: 100%;",
                                                   tags$span("40 Yard Dash"),
                                                   tags$span(uiOutput("athlete_fortyYardMoM"))
                                                 ),
                                                 value = textOutput("athlete_fortyYardValue"),
                                                 showcase = plotlyOutput("athlete_fortyYardPlot"),
                                                 showcase_layout = "bottom",
                                                 full_screen = TRUE
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 2),
                                        column(width = 4,
                                               value_box(
                                                 title = tags$div(
                                                   style = "display: flex; justify-content: space-between; width: 100%;",
                                                   tags$span("Max Velocity"),
                                                   tags$span(uiOutput("athlete_maxvelocityMoM"))
                                                 ),
                                                 value = textOutput("athlete_maxvelocityValue"),
                                                 showcase = plotlyOutput("athlete_maxvelocityPlot"),
                                                 showcase_layout = "bottom",
                                                 full_screen = TRUE
                                               )),
                                        column(width = 4,
                                               value_box(
                                                 title = tags$div(
                                                   style = "display: flex; justify-content: space-between; width: 100%;",
                                                   tags$span("RSI"),
                                                   tags$span(uiOutput("athlete_RSI_MoM"))
                                                 ),
                                                 value = textOutput("athlete_RSI_value"),
                                                 showcase = plotlyOutput("athlete_RSI_plot"),
                                                 showcase_layout = "bottom",
                                                 full_screen = TRUE
                                               )),
                                        column(width = 2)
                                      )
                                  ),
                                  box(title = "Attendance Trend", status = "primary", solidHeader = TRUE, width = 12,
                                      echarts4rOutput("athlete_speed_attendancePlot")
                                  ),
                                  box(title = "Athlete Performances", status = "primary", solidHeader = TRUE, width = 12,
                                      glide(id = "recent_glide_athlete_speed", controls_position = "bottom",
                                            next_label = paste("Next Exercise", shiny::icon("chevron-right", lib = "glyphicon")),
                                            previous_label = paste(shiny::icon("chevron-left", lib = "glyphicon"), "Previous Exercise"),
                                            screen(
                                              tags$p("Acceleration", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                              DTOutput("athlete_acceleration_recent")
                                            ),
                                            screen(
                                              tags$p("Hard 90", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                              DTOutput("athlete_hardNinety_recent")
                                            ),
                                            screen(
                                              tags$p("40 Yard Dash", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                              DTOutput("athlete_fortyYard_recent")
                                            ),
                                            screen(
                                              tags$p("Max Velocity", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                              DTOutput("athlete_maxVelocity_recent")
                                            ),
                                            screen(
                                              tags$p("RSI", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                              DTOutput("athlete_RSI_recent")
                                            )
                                      )
                                  )
                         ),
                         tabPanel("Strength Data",
                                  box(title = "Summary Statistics", status = "primary", solidHeader = TRUE, width = 12,
                                      fluidRow(
                                        column(width = 4,
                                               value_box(
                                                 title = tags$div(
                                                   style = "display: flex; justify-content: space-between; width: 100%;",
                                                   tags$span("CMJ"),
                                                   tags$span(uiOutput("athlete_CMJ_MoM"))
                                                 ),
                                                 value = textOutput("athlete_CMJ_Value"),
                                                 showcase = plotlyOutput("athlete_CMJ_Plot_MoM"),
                                                 showcase_layout = "bottom",
                                                 full_screen = TRUE
                                               )),
                                        column(width = 4,
                                               value_box(
                                                 title = tags$div(
                                                   style = "display: flex; justify-content: space-between; width: 100%;",
                                                   tags$span("ISO Belt Squat"),
                                                   tags$span(uiOutput("athlete_IBSQT_MoM"))
                                                 ),
                                                 value = textOutput("athlete_IBSQT_Value"),
                                                 showcase = plotlyOutput("athlete_IBSQT_Plot_MoM"),
                                                 showcase_layout = "bottom",
                                                 full_screen = TRUE
                                               )),
                                        column(width = 4,
                                               value_box(
                                                 title = tags$div(
                                                   style = "display: flex; justify-content: space-between; width: 100%;",
                                                   tags$span("Squat Jump"),
                                                   tags$span(uiOutput("athlete_SJ_MoM"))
                                                 ),
                                                 value = textOutput("athlete_SJ_Value"),
                                                 showcase = plotlyOutput("athlete_SJ_Plot_MoM"),
                                                 showcase_layout = "bottom",
                                                 full_screen = TRUE
                                               ))
                                      ),
                                      fluidRow(
                                        column(width = 1),
                                        column(width = 5,
                                               value_box(
                                                 title = tags$div(
                                                   style = "display: flex; justify-content: space-between; width: 100%;",
                                                   tags$span("Proteus Power"),
                                                   tags$span(uiOutput("athlete_proteusPower_MoM"))
                                                 ),
                                                 value = textOutput("athlete_proteusPower_Value"),
                                                 showcase = plotlyOutput("athlete_proteusPower_Plot_MoM"),
                                                 showcase_layout = "bottom",
                                                 full_screen = TRUE
                                               )),
                                        column(width = 5,
                                               value_box(
                                                 title = tags$div(
                                                   style = "display: flex; justify-content: space-between; width: 100%;",
                                                   tags$span("Proteus Acceleration"),
                                                   tags$span(uiOutput("athlete_proteusAcc_MoM"))
                                                 ),
                                                 value = textOutput("athlete_proteusAcc_Value"),
                                                 showcase = plotlyOutput("athlete_proteusAcc_Plot_MoM"),
                                                 showcase_layout = "bottom",
                                                 full_screen = TRUE
                                               )),
                                        column(width = 1)
                                      )
                                  ),
                                  box(title = "Attendance Trend", status = "primary", solidHeader = TRUE, width = 12,
                                      echarts4rOutput("athlete_strength_attendancePlot")
                                  ),
                                  box(title = "Athlete Performances", status = "primary", solidHeader = TRUE, width = 12,
                                      glide(id = "recent_glide_athlete_strength", controls_position = "bottom",
                                            next_label = paste("Next Exercise", shiny::icon("chevron-right", lib = "glyphicon")),
                                            previous_label = paste(shiny::icon("chevron-left", lib = "glyphicon"), "Previous Exercise"),
                                            screen(
                                              tags$p("ForceDeck: CMJ", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                              DTOutput("athlete_CMJ_recent")
                                            ),
                                            screen(
                                              tags$p("ForceDeck: ISO Belt Squat", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                              DTOutput("athlete_IBSQT_recent")
                                            ),
                                            screen(
                                              tags$p("ForceDeck: ShoulderISO", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                              DTOutput("athlete_SJ_recent")
                                            ),
                                            screen(
                                              tags$p("Proteus Power", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                              DTOutput("athlete_proteusPower_recent")
                                            ),
                                            screen(
                                              tags$p("Proteus Acceleration", style = "text-align: center; font-size: 24px; font-weight: bold;"),
                                              DTOutput("athlete_proteusAcc_recent")
                                            )
                                      )
                                  )
                         )
                       )
                )
              )
      ),
      tabItem(tabName = "attendance",
              box(status = "primary", solidHeader = TRUE, width = 6,
                  ribbon(
                    text = "Under Construction",
                    color = "warning"
                  )
              )
      ),
      tabItem(tabName = "correlations",
              box(status = "primary", solidHeader = TRUE, width = 6,
                  ribbon(
                    text = "Under Construction",
                    color = "warning"
                  )
              )
      ),
      tabItem(tabName = "reports",
              fluidRow(
                box(title = "Generate Report", status = "primary", solidHeader = TRUE, width = 6,
                    ribbon(
                      text = "Under Construction",
                      color = "warning"
                    ),
                    downloadButton("download_report", "Download Report"))
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  selected_athlete_info <- reactive({
    req(input$athlete)
    client_data[client_data$Client == input$athlete, ]
  })
  
  update_objectives <- function() {
    athlete_info <- selected_athlete_info()
    
    output$objective_one <- renderUI({
      req(input$athlete_performance_tabbox)
      if (input$athlete_performance_tabbox == "Speed Data") {
        HTML(paste("<b>Movement Objective:</b>", athlete_info$`Movement Objective (Speed)`))
      } else if (input$athlete_performance_tabbox == "Strength Data") {
        HTML(paste("<b>Core Objective:</b>", athlete_info$`Core Objective (Weight Room)`))
      }
    })
    
    output$objective_two <- renderUI({
      req(input$athlete_performance_tabbox)
      if (input$athlete_performance_tabbox == "Speed Data") {
        HTML(paste("<b>Speed-Strength Objective:</b>", athlete_info$`Speed-Strength Objective (Speed)`))
      } else if (input$athlete_performance_tabbox == "Strength Data") {
        HTML(paste("<b>Lower Body Objective:</b>", athlete_info$`Lower Body Objective (Weight Room)`))
      }
    })
    
    output$radar_plot <- renderEcharts4r({
      req(input$athlete_performance_tabbox)
      if (input$athlete_performance_tabbox == "Speed Data") {
        speed_percentiles <- speed_data %>%
          filter(Name == input$athlete) %>% 
          group_by(`Exercise Name`) %>% 
          summarise(
            Percentile = round(mean(PercentileRank, na.rm = TRUE), 0)
          )
        
        if (nrow(speed_percentiles) < 4) {
          # Display an empty radar chart if no data is available
          blank_data <- data.frame(
            x = numeric(0),
            y = numeric(0)
          )
          
          blank_data %>% 
            e_charts(x) %>% 
            e_x_axis(show = FALSE) %>% 
            e_y_axis(show = FALSE) %>% 
            e_draft(text = "No Data", size = "50px")
        } else {
          # Display the radar chart with the athlete's data
          speed_percentiles %>% 
            e_charts(`Exercise Name`) %>% 
            e_radar(Percentile, max = 100, legend = FALSE, areaStyle = list(opacity = 0.35)) %>% 
            e_tooltip(trigger = "item")
        }
      } else if (input$athlete_performance_tabbox == "Strength Data") {
        strength_percentiles <- strength_data %>%
          filter(Name == input$athlete, `Exercise Name` != "Proteus Full Test") %>% 
          group_by(`Exercise Name`) %>% 
          summarise(
            Percentile = round(mean(PercentileRank, na.rm = TRUE), 0)
          )
        
        if (nrow(strength_percentiles) < 2) {
          # Display an empty radar chart if no data is available
          blank_data <- data.frame(
            x = numeric(0),
            y = numeric(0)
          )
          
          blank_data %>% 
            e_charts(x) %>% 
            e_x_axis(show = FALSE) %>% 
            e_y_axis(show = FALSE) %>% 
            e_draft(text = "No Data", size = "50px")
        } else {
          # Display the radar chart with the athlete's data
          strength_percentiles %>% 
            e_charts(`Exercise Name`) %>% 
            e_radar(Percentile, max = 100, legend = FALSE, areaStyle = list(opacity = 0.35)) %>% 
            e_tooltip(trigger = "item")
        }
      }
    })
  }
  
  observeEvent(input$athlete, {
    update_objectives()
  })
  
  observeEvent(input$athlete_performance_tabbox, {
    update_objectives()
  })
  
  # Line Plot Visual for the Value Boxes
  plotly_time_series <- function(d, x, y) {
    info <- getCurrentOutputInfo()
    large <- isTRUE(info$height() > 200)
    
    plot_ly(d, x = x, y = y) %>%
      add_lines(
        color = I("#5470c6"),
        span = I(1),
        #hoverinfo = if (!large) "none",
        fill = 'tozeroy',
        alpha = 0.2
      ) %>%
      layout(
        hovermode = "x+y",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = info$fg()),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        xaxis = list(
          title = "",
          visible = large,
          showgrid = FALSE
        ),
        yaxis = list(
          title = "",
          visible = large,
          showgrid = FALSE
        )
      ) %>%
      config(displayModeBar = FALSE)
  }
  
  # Monthly Plot Data for Overview Tab
  output$maxvelocityPlot_MoM <- renderPlotly({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    plot_data <- speed_data %>%
      filter(`Exercise Name` == "Max Velocity",
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>%
      group_by(Date) %>%
      summarise(MPH = round(mean(MPH, na.rm = TRUE), 2))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~MPH
    )
  })
  
  output$accelerationPlot_MoM <- renderPlotly({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    plot_data <- speed_data %>%
      filter(`Exercise Name` == "Acceleration", 
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>%
      group_by(Date) %>%
      summarise(Acceleration = round(mean(`Split1`, na.rm = TRUE), 2))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~Acceleration
    )
  })
  
  output$hardNinetyPlot_MoM <- renderPlotly({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    plot_data <- speed_data %>%
      filter(`Exercise Name` == "Hard 90",  
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>%
      group_by(Date) %>%
      summarise(HardNinety = round(mean(`Cumulative2`, na.rm = TRUE), 2))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~HardNinety
    )
  })
  
  output$fortyYardPlot_MoM <- renderPlotly({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    plot_data <- speed_data %>%
      filter(`Exercise Name` == "40 Yard Dash",  
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>%
      group_by(Date) %>%
      summarise(FortyTime = round(mean(`Cumulative3`, na.rm = TRUE), 2))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~FortyTime
    )
  })
  
  output$RSI_plot_MoM <- renderPlotly({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    plot_data <- speed_data %>%
      filter(`Exercise Name` == "RSI",   
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>%
      group_by(Date) %>%
      summarise(RSI = round(mean(`Mean RSI (Jump Height/Contact Time) [m/s]`, na.rm = TRUE), 2))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~RSI
    )
  })
  
  # Plot data for Athlete Profiles
  output$athlete_maxvelocityPlot <- renderPlotly({
    plot_data <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Max Velocity") %>%
      group_by(Date) %>%
      summarise(MPH = round(mean(MPH, na.rm = TRUE), 2))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~MPH
    )
  })
  
  output$athlete_accelerationPlot <- renderPlotly({
    plot_data <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Acceleration") %>%
      group_by(Date) %>%
      summarise(Acceleration = round(mean(`Split1`, na.rm = TRUE), 2))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~Acceleration
    )
  })
  
  output$athlete_hardNinetyPlot <- renderPlotly({
    plot_data <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Hard 90") %>%
      group_by(Date) %>%
      summarise(HardNinety = round(mean(`Cumulative2`, na.rm = TRUE), 2))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~HardNinety
    )
  })
  
  output$athlete_fortyYardPlot <- renderPlotly({
    plot_data <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "40 Yard Dash") %>%
      group_by(Date) %>%
      summarise(FortyTime = round(mean(`Cumulative3`, na.rm = TRUE), 2))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~FortyTime
    )
  })
  
  output$athlete_RSI_plot <- renderPlotly({
    plot_data <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "RSI") %>%
      group_by(Date) %>%
      summarise(RSI = round(mean(`Mean RSI (Jump Height/Contact Time) [m/s]`, na.rm = TRUE), 2))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~RSI
    )
  })
  
  # Monthly Plot Data for Overview Tab (Strength)
  output$CMJ_Plot_MoM <- renderPlotly({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    plot_data <- strength_data %>%
      filter(`Exercise Name` == "CMJ",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>%
      group_by(Date) %>%
      summarise(CMJ = round(mean(`Concentric Peak Force [N]`, na.rm = TRUE)))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~CMJ
    )
  })
  
  output$IBSQT_Plot_MoM <- renderPlotly({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    plot_data <- strength_data %>%
      filter(`Exercise Name` == "IBSQT",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>%
      group_by(Date) %>%
      summarise(IBSQT = round(mean(`Peak Vertical Force [N]`, na.rm = TRUE)))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~IBSQT
    )
  })
  
  output$SJ_Plot_MoM <- renderPlotly({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    plot_data <- strength_data %>%
      filter(`Exercise Name` == "SJ",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>%
      group_by(Date) %>%
      summarise(SJ = round(mean(`Takeoff Peak Force [N]`, na.rm = TRUE)))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~SJ
    )
  })
  
  output$proteusPower_Plot_MoM <- renderPlotly({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    plot_data <- strength_data %>%
      filter(`Exercise Name` == "Proteus Full Test",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>%
      group_by(Date) %>%
      summarise(Proteus_Power = round(mean(`power - high`, na.rm = TRUE)))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~Proteus_Power
    )
  })
  
  output$proteusAcc_Plot_MoM <- renderPlotly({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    plot_data <- strength_data %>%
      filter(`Exercise Name` == "Proteus Full Test",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>%
      group_by(Date) %>%
      summarise(Proteus_Acc = round(mean(`acceleration - high`, na.rm = TRUE)))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~Proteus_Acc
    )
  })
  
  # Monthly Plot Data for Athlete Tab (Strength)
  output$athlete_CMJ_Plot_MoM <- renderPlotly({
    plot_data <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "CMJ") %>%
      group_by(Date) %>%
      summarise(CMJ = round(mean(`Concentric Peak Force [N]`, na.rm = TRUE)))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~CMJ
    )
  })
  
  output$athlete_IBSQT_Plot_MoM <- renderPlotly({
    plot_data <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "IBSQT") %>%
      group_by(Date) %>%
      summarise(IBSQT = round(mean(`Peak Vertical Force [N]`, na.rm = TRUE)))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~IBSQT
    )
  })
  
  output$athlete_SJ_Plot_MoM <- renderPlotly({
    plot_data <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "SJ") %>%
      group_by(Date) %>%
      summarise(SJ = round(mean(`Takeoff Peak Force [N]`, na.rm = TRUE)))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~SJ
    )
  })
  
  output$athlete_proteusPower_Plot_MoM <- renderPlotly({
    plot_data <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Proteus Full Test") %>%
      group_by(Date) %>%
      summarise(Proteus_Power = round(mean(`power - high`, na.rm = TRUE)))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~Proteus_Power
    )
  })
  
  output$athlete_proteusAcc_Plot_MoM <- renderPlotly({
    plot_data <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Proteus Full Test") %>%
      group_by(Date) %>%
      summarise(Proteus_Acc = round(mean(`acceleration - high`, na.rm = TRUE), 1))
    
    plotly_time_series(
      plot_data, x = ~Date, y = ~Proteus_Acc
    )
  })
  
  # Monthly Value Box Data for Overview Tab (Speed)
  output$maxvelocityValue_MoM <- renderText({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    maxVelo_summary <- speed_data %>%
      filter(`Exercise Name` == "Max Velocity",
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>% 
      summarise(
        `Max Velocity` = round(mean(MPH, na.rm = TRUE), 2)
      )
    maxVelo_output <- maxVelo_summary$`Max Velocity`
    paste0(maxVelo_output, " mph")
  })
  
  output$accelerationValue_MoM <- renderText({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    summary_data <- speed_data %>%
      filter(`Exercise Name` == "Acceleration",
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>% 
      summarise(
        Acceleration = round(mean(`Split1`, na.rm = TRUE), 3)
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$Acceleration)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$Acceleration, " sec")
    }
    
  })
  
  output$hardNinetyValue_MoM <- renderText({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    summary_data <- speed_data %>%
      filter(`Exercise Name` == "Hard 90",
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>% 
      summarise(
        `Hard 90` = round(mean(`Cumulative2`, na.rm = TRUE), 3)
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$`Hard 90`)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$`Hard 90`, " sec")
    }
    
  })
  
  output$fortyYardValue_MoM <- renderText({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    summary_data <- speed_data %>%
      filter(`Exercise Name` == "40 Yard Dash", 
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>% 
      summarise(
        `40 Yard Dash` = round(mean(`Cumulative3`, na.rm = TRUE), 3)
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$`40 Yard Dash`)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$`40 Yard Dash`, " sec")
    }
    
  })
  
  output$RSI_value_MoM <- renderText({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    summary_data <- speed_data %>%
      filter(`Exercise Name` == "RSI",  
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>% 
      summarise(
        RSI = round(mean(`Mean RSI (Jump Height/Contact Time) [m/s]`, na.rm = TRUE), 2)
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$RSI)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$RSI, " m/s")
    }
    
  })
  
  # Monthly Value Box Data for Overview Tab (Strength)
  output$CMJ_Value_MoM <- renderText({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    summary_data <- strength_data %>%
      filter(`Exercise Name` == "CMJ",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>% 
      summarise(
        CMJ = round(mean(`Concentric Peak Force [N]`, na.rm = TRUE))
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$CMJ)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$CMJ, " N")
    }
    
  })
  
  output$IBSQT_Value_MoM <- renderText({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    summary_data <- strength_data %>%
      filter(`Exercise Name` == "IBSQT",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>% 
      summarise(
        IBSQT = round(mean(`Peak Vertical Force [N]`, na.rm = TRUE))
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$IBSQT)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$IBSQT, " N")
    }
    
  })
  
  output$SJ_Value_MoM <- renderText({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    summary_data <- strength_data %>%
      filter(`Exercise Name` == "SJ",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>% 
      summarise(
        SJ = round(mean(`Takeoff Peak Force [N]`, na.rm = TRUE))
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$SJ)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$SJ, " N")
    }
    
  })
  
  output$proteusPower_Value_MoM <- renderText({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    summary_data <- strength_data %>%
      filter(`Exercise Name` == "Proteus Full Test",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>% 
      summarise(
        proteusPower = round(mean(`power - high`, na.rm = TRUE))
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$proteusPower)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$proteusPower, " N")
    }
    
  })
  
  output$proteusAcc_Value_MoM <- renderText({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    summary_data <- strength_data %>%
      filter(`Exercise Name` == "Proteus Full Test",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>% 
      summarise(
        proteusAcc = round(mean(`acceleration - high`, na.rm = TRUE))
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$proteusAcc)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$proteusAcc, " m/s")
    }
    
  })

  # Value Box Data for Athlete Profiles (Speed)
  output$athlete_maxvelocityValue <- renderText({
    maxVelo_summary <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Max Velocity") %>% 
      summarise(
        `Max Velocity` = round(mean(MPH, na.rm = TRUE), 2)
      )
    
    if(nrow(maxVelo_summary) == 0 || is.na(maxVelo_summary$`Max Velocity`)){
      maxVelo_output <- "No Data"
    } else {
      maxVelo_output <- paste0(maxVelo_summary$`Max Velocity`, " mph")
    }
    
    maxVelo_output
  })
  
  output$athlete_accelerationValue <- renderText({
    acceleration_summary <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Acceleration") %>% 
      summarise(
        Acceleration = round(mean(`Split1`, na.rm = TRUE), 3)
      )
    
    if(nrow(acceleration_summary) == 0 || is.na(acceleration_summary$Acceleration)){
      acceleration_output <- "No Data"
    } else {
      acceleration_output <- paste0(acceleration_summary$Acceleration, " sec")
    }
    
    acceleration_output
  })
  
  output$athlete_hardNinetyValue <- renderText({
    hardNinety_summary <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Hard 90") %>% 
      summarise(
        `Hard 90` = round(mean(`Cumulative2`, na.rm = TRUE), 3)
      )
    
    if(nrow(hardNinety_summary) == 0 || is.na(hardNinety_summary$`Hard 90`)){
      hardNinety_output <- "No Data"
    } else {
      hardNinety_output <- paste0(hardNinety_summary$`Hard 90`, " sec")
    }
    
    hardNinety_output
  })
  
  output$athlete_fortyYardValue <- renderText({
    fortyYard_summary <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "40 Yard Dash") %>% 
      summarise(
        `40 Yard Dash` = round(mean(`Cumulative3`, na.rm = TRUE), 3)
      )
    
    if(nrow(fortyYard_summary) == 0 || is.na(fortyYard_summary$`40 Yard Dash`)){
      fortyYard_output <- "No Data"
    } else {
      fortyYard_output <- paste0(fortyYard_summary$`40 Yard Dash`, " sec")
    }
    
    fortyYard_output
  })
  
  output$athlete_RSI_value <- renderText({
    RSI_summary <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "RSI") %>% 
      summarise(
        RSI = round(mean(`Mean RSI (Jump Height/Contact Time) [m/s]`, na.rm = TRUE), 2)
      )
    
    if(nrow(RSI_summary) == 0 || is.na(RSI_summary$RSI)){
      RSI_output <- "No Data"
    } else {
      RSI_output <- paste0(RSI_summary$RSI, " m/s")
    }
    
    RSI_output
  })

  
  # Value Box Data for Athlete Profiles (Strength)
  output$athlete_CMJ_Value <- renderText({
    summary_data <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "CMJ") %>% 
      summarise(
        CMJ = round(mean(`Concentric Peak Force [N]`, na.rm = TRUE))
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$CMJ)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$CMJ, " N")
    }
    
    output_data
  })
  
  output$athlete_IBSQT_Value <- renderText({
    summary_data <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "IBSQT") %>% 
      summarise(
        IBSQT = round(mean(`Peak Vertical Force [N]`, na.rm = TRUE))
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$IBSQT)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$IBSQT, " N")
    }
    
    output_data
  })
  
  output$athlete_SJ_Value <- renderText({
    summary_data <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "SJ") %>% 
      summarise(
        SJ = round(mean(`Takeoff Peak Force [N]`, na.rm = TRUE))
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$SJ)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$SJ, " N")
    }
    
    output_data
  })
  
  output$athlete_proteusPower_Value <- renderText({
    summary_data <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Proteus Full Test") %>% 
      summarise(
        Proteus_Power = round(mean(`power - high`, na.rm = TRUE))
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$Proteus_Power)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$Proteus_Power, " N")
    }
    
    output_data
  })

  output$athlete_proteusAcc_Value <- renderText({
    summary_data <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Proteus Full Test") %>% 
      summarise(
        Proteus_Acc = round(mean(`acceleration - high`, na.rm = TRUE), 1)
      )
    
    if(nrow(summary_data) == 0 || is.na(summary_data$Proteus_Acc)){
      output_data <- "No Data"
    } else {
      output_data <- paste0(summary_data$Proteus_Acc, " N")
    }
    
    output_data
  })
  
  
  # Value Box Data for Overview tab (Speed)
  output$maxvelocityMoM <- renderUI({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    athlete_progression <- speed_data %>% 
      filter(`Exercise Name` == "Max Velocity",
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(MPH), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  output$accelerationMoM <- renderUI({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)

    athlete_progression <- speed_data %>% 
      filter(`Exercise Name` == "Acceleration",
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(Split1), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent <= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  output$hardNinetyMoM <- renderUI({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    athlete_progression <- speed_data %>% 
      filter(`Exercise Name` == "Hard 90",
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(Cumulative2), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent <= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  output$fortyYardMoM <- renderUI({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    athlete_progression <- speed_data %>% 
      filter(`Exercise Name` == "40 Yard Dash",
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(Cumulative3), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent <= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  output$RSI_MoM <- renderUI({
    req(input$speed_dateRange, input$speed_level, input$speed_gender, input$speed_serviceName)
    
    athlete_progression <- speed_data %>% 
      filter(`Exercise Name` == "RSI",
             Date >= input$speed_dateRange[1] & Date <= input$speed_dateRange[2],
             Level %in% input$speed_level,
             Gender %in% input$speed_gender,
             ServiceName %in% input$speed_serviceName) %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(`Mean RSI (Jump Height/Contact Time) [m/s]`), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  # Value Box Data for Overview tab (Strength)
  output$CMJ_MoM <- renderUI({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    athlete_progression <- strength_data %>% 
      filter(`Exercise Name` == "CMJ",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(`Concentric Peak Force [N]`), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })

  output$IBSQT_MoM <- renderUI({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    athlete_progression <- strength_data %>% 
      filter(`Exercise Name` == "IBSQT",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(`Peak Vertical Force [N]`), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  output$SJ_MoM <- renderUI({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    athlete_progression <- strength_data %>% 
      filter(`Exercise Name` == "SJ",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(`Takeoff Peak Force [N]`), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })

  output$proteusPower_MoM <- renderUI({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    athlete_progression <- strength_data %>% 
      filter(`Exercise Name` == "Proteus Full Test",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(`power - high`), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  output$proteusAcc_MoM <- renderUI({
    req(input$strength_dateRange, input$strength_level, input$strength_gender, input$strength_serviceName)
    
    athlete_progression <- strength_data %>% 
      filter(`Exercise Name` == "Proteus Full Test",
             Date >= input$strength_dateRange[1] & Date <= input$strength_dateRange[2],
             Level %in% input$strength_level,
             Gender %in% input$strength_gender,
             ServiceName %in% input$strength_serviceName) %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(`acceleration - high`), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  # Value Box Data for Athlete Profiles (Speed)
  output$athlete_maxvelocityMoM <- renderUI({
    athlete_progression <- speed_data %>% 
      filter(Name == input$athlete, `Exercise Name` == "Max Velocity") %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(MPH), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  output$athlete_accelerationMoM <- renderUI({
    athlete_progression <- speed_data %>% 
      filter(Name == input$athlete, `Exercise Name` == "Acceleration") %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(Split1), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent <= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  output$athlete_hardNinetyMoM <- renderUI({
    athlete_progression <- speed_data %>% 
      filter(Name == input$athlete, `Exercise Name` == "Hard 90") %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(Cumulative2), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent <= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  
  output$athlete_fortyYardMoM <- renderUI({
    athlete_progression <- speed_data %>% 
      filter(Name == input$athlete, `Exercise Name` == "40 Yard Dash") %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(Cumulative3), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent <= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  output$athlete_RSI_MoM <- renderUI({
    athlete_progression <- speed_data %>% 
      filter(Name == input$athlete, `Exercise Name` == "RSI") %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(`Mean RSI (Jump Height/Contact Time) [m/s]`), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }

    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  # Value Box Data for Athlete Profiles (Strength)
  output$athlete_CMJ_MoM <- renderUI({
    athlete_progression <- strength_data %>% 
      filter(Name == input$athlete, `Exercise Name` == "CMJ") %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(`Concentric Peak Force [N]`), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  output$athlete_IBSQT_MoM <- renderUI({
    athlete_progression <- strength_data %>% 
      filter(Name == input$athlete, `Exercise Name` == "IBSQT") %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(`Peak Vertical Force [N]`), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
   
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })

  output$athlete_SJ_MoM <- renderUI({
    athlete_progression <- strength_data %>% 
      filter(Name == input$athlete, `Exercise Name` == "SJ") %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(`Takeoff Peak Force [N]`), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  output$athlete_proteusPower_MoM <- renderUI({
    athlete_progression <- strength_data %>% 
      filter(Name == input$athlete, `Exercise Name` == "Proteus Full Test") %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(`power - high`), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  output$athlete_proteusAcc_MoM <- renderUI({
    athlete_progression <- strength_data %>% 
      filter(Name == input$athlete, `Exercise Name` == "Proteus Full Test") %>%
      group_by(Name, Date) %>%
      summarise(Value = mean(`acceleration - high`), .groups = 'drop') %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      filter(n() > 1) %>%
      mutate(First = first(Value),
             Last = last(Value)) %>%
      ungroup() %>%
      mutate(Improvement = Last - First,
             Percent = (Improvement / First) * 100) %>%
      filter(!is.na(Improvement), !is.na(Percent)) %>%
      summarise(AvgImprovement = round(mean(Improvement, na.rm = TRUE), 3),
                AvgPercent = round(mean(Percent, na.rm = TRUE), 2), .groups = 'drop') %>%
      ungroup()
    
    if (is.nan(athlete_progression$AvgImprovement)) {
      return(NULL)
    }
    
    icon_name <- ifelse(athlete_progression$AvgPercent >= 0, "arrow-up", "arrow-down")
    icon_color <- ifelse(athlete_progression$AvgPercent >= 0, "green", "red")
    
    HTML(paste0(
      "<div style='color:", icon_color, ";'>",
      icon(icon_name), " ", athlete_progression$AvgPercent, "%",
      "</div>"
    ))
  })
  
  # Overview Tab - Recent Performances (Speed)
  output$acceleration_recent <- renderDT({
    recent_performances <- speed_data %>%
      filter(`Exercise Name` == "Acceleration") %>% 
      select(Date, Name, Level, Gender, ServiceName, `Split1`, PercentileRank) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 100)
    
    recent_performances
  })
  
  output$hardNinety_recent <- renderDT({
    recent_performances <- speed_data %>%
      filter(`Exercise Name` == "Hard 90") %>% 
      select(Date, Name, Level, Gender, ServiceName, `Cumulative2`, PercentileRank) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 100)
    
    recent_performances
  })
  
  output$fortyYard_recent <- renderDT({
    recent_performances <- speed_data %>%
      filter(`Exercise Name` == "40 Yard Dash") %>% 
      select(Date, Name, Level, Gender, ServiceName, `Cumulative3`, PercentileRank) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 100)
    
    recent_performances
  })
  
  output$maxVelocity_recent <- renderDT({
    recent_performances <- speed_data %>%
      filter(`Exercise Name` == "Max Velocity") %>% 
      select(Date, Name, Level, Gender, ServiceName, MPH, PercentileRank) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 100)
    
    recent_performances
  })
  
  output$RSI_recent <- renderDT({
    recent_performances <- speed_data %>%
      filter(`Exercise Name` == "RSI") %>% 
      rename(RSI = "Mean RSI (Jump Height/Contact Time) [m/s]") %>% 
      select(Date, Name, Level, Gender, ServiceName, RSI, PercentileRank) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 100)
    
    recent_performances
  })
  
  # Overview Tab - Recent Performances (Strength)
  output$CMJ_recent <- renderDT({
    recent_performances <- strength_data %>%
      filter(`Exercise Name` == "CMJ") %>% 
      select(Date, Name, Level, Gender, ServiceName, `Concentric Peak Force [N]`, PercentileRank) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 100)
    
    recent_performances
  })
  
  output$IBSQT_recent <- renderDT({
    recent_performances <- strength_data %>%
      filter(`Exercise Name` == "IBSQT") %>% 
      select(Date, Name, Level, Gender, ServiceName, `Peak Vertical Force [N]`, PercentileRank) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 100)
    
    recent_performances
  })
  
  output$SJ_recent <- renderDT({
    recent_performances <- strength_data %>%
      filter(`Exercise Name` == "SJ") %>% 
      select(Date, Name, Level, Gender, ServiceName, `Takeoff Peak Force [N]`, PercentileRank) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 100)
    
    recent_performances
  })
  
  output$proteusPower_recent <- renderDT({
    recent_performances <- strength_data %>%
      filter(`Exercise Name` == "Proteus Full Test") %>% 
      select(Date, Name, Level, Gender, ServiceName, `power - high`, PowerPercentileRank) %>%
      mutate(`power - high` = round(`power - high`)) %>%
      rename(Power = `power - high`, Percentile = PowerPercentileRank) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 100)
    
    recent_performances
  })
  
  output$proteusAcc_recent <- renderDT({
    recent_performances <- strength_data %>%
      filter(`Exercise Name` == "Proteus Full Test") %>% 
      select(Date, Name, Level, Gender, ServiceName, `acceleration - high`, AccelerationPercentileRank) %>%
      mutate(`acceleration - high` = round(`acceleration - high`, 1)) %>%
      rename(Accel = `acceleration - high`, Percentile = AccelerationPercentileRank) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 100)
    
    recent_performances
  })
  
  # Athlete Tab - Recent Performances (Speed)
  output$athlete_acceleration_recent <- renderDT({
    recent_performances <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Acceleration") %>% 
      select(Date, ServiceName, `Split1`, PercentileRank) %>%
      arrange(desc(Date))
    
    recent_performances
  })
  
  output$athlete_hardNinety_recent <- renderDT({
    recent_performances <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Hard 90") %>% 
      select(Date, ServiceName, `Cumulative2`, PercentileRank) %>%
      arrange(desc(Date))
    
    recent_performances
  })
  
  output$athlete_fortyYard_recent <- renderDT({
    recent_performances <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "40 Yard Dash") %>% 
      select(Date, ServiceName, `Cumulative3`, PercentileRank) %>%
      arrange(desc(Date)) 
    
    recent_performances
  })
  
  output$athlete_maxVelocity_recent <- renderDT({
    recent_performances <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Max Velocity") %>% 
      select(Date, ServiceName, MPH, PercentileRank) %>%
      arrange(desc(Date)) 
    
    recent_performances
  })
  
  output$athlete_RSI_recent <- renderDT({
    recent_performances <- speed_data %>%
      filter(Name == input$athlete, `Exercise Name` == "RSI") %>% 
      rename(RSI = "Mean RSI (Jump Height/Contact Time) [m/s]") %>% 
      select(Date, ServiceName, RSI, PercentileRank) %>%
      arrange(desc(Date))
    
    recent_performances
  })
  
  # Athlete Tab - Recent Performances (Strength)
  output$athlete_CMJ_recent <- renderDT({
    recent_performances <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "CMJ") %>% 
      select(Date, ServiceName, `Concentric Peak Force [N]`, PercentileRank) %>%
      arrange(desc(Date))
    
    recent_performances
  })
  
  output$athlete_IBSQT_recent <- renderDT({
    recent_performances <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "IBSQT") %>% 
      select(Date, ServiceName, `Peak Vertical Force [N]`, PercentileRank) %>%
      arrange(desc(Date))
    
    recent_performances
  })
  
  output$athlete_SJ_recent <- renderDT({
    recent_performances <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "SJ") %>% 
      select(Date, ServiceName, `Takeoff Peak Force [N]`, PercentileRank) %>%
      arrange(desc(Date)) 
    
    recent_performances
  })
  
  output$athlete_proteusPower_recent <- renderDT({
    recent_performances <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Proteus Full Test") %>% 
      select(Date, Name, Level, Gender, ServiceName, `power - high`, PowerPercentileRank) %>%
      mutate(`power - high` = round(`power - high`)) %>%
      rename(Power = `power - high`, Percentile = PowerPercentileRank) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 100)
    
    recent_performances
  })
  
  output$athlete_proteusAcc_recent <- renderDT({
    recent_performances <- strength_data %>%
      filter(Name == input$athlete, `Exercise Name` == "Proteus Full Test") %>% 
      select(Date, Name, Level, Gender, ServiceName, `acceleration - high`, AccelerationPercentileRank) %>%
      mutate(`acceleration - high` = round(`acceleration - high`, 1)) %>%
      rename(Accel = `acceleration - high`, Percentile = AccelerationPercentileRank) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 100)
    
    recent_performances
  })
  
  # Reactive expression to fetch the selected athlete's info
  selected_athlete_info <- reactive({
    req(input$athlete)
    client_data %>% filter(Client == input$athlete)
  })
  
  # Render athlete information
  output$athlete_height <- renderText({
    athlete_info <- selected_athlete_info()
    HTML(paste(athlete_info$Height))
  })
  
  output$athlete_weight <- renderText({
    athlete_info <- selected_athlete_info()
    HTML(paste(athlete_info$Weight))
  })
  
  output$athlete_position <- renderText({
    athlete_info <- selected_athlete_info()
    paste0(athlete_info$`Position (Baseball/Softball)`)
  })
  
  output$athlete_level <- renderUI({
    athlete_info <- selected_athlete_info()
    div(style = "text-align: center; color: grey;", 
        paste0(athlete_info$`Reporting Level (Age-Dependent)`)
    )
  })
  
  output$athlete_age <- renderText({
    athlete_info <- selected_athlete_info()
    HTML(paste(athlete_info$Age))
  })
  
  output$athlete_birthdate <- renderText({
    athlete_info <- selected_athlete_info()
    HTML(paste(athlete_info$`field-general-7.dl_date`))
  })
  
  output$athlete_physicality <- renderText({
    athlete_info <- selected_athlete_info()
    HTML(paste("<b>Physicality Objective:</b>", athlete_info$`Physicality Objective (All)`))
  })
  
  output$athlete_recovery <- renderText({
    athlete_info <- selected_athlete_info()
    HTML(paste("<b>Recovery Objective:</b>", athlete_info$`Recovery Objective (All)`))
  })
  
  output$athlete_nutrition <- renderText({
    athlete_info <- selected_athlete_info()
    HTML(paste("<b>Nutrition Objective:</b>", athlete_info$`Nutrition Objective (All)`))
  })
  
  output$weight_trend <- renderEcharts4r({
    weight_data <- strength_data %>% 
      filter(Name == input$athlete, !is.na(Weight)) %>% 
      arrange(Date) %>% 
      group_by(Date, Name) %>% 
      summarise(Weight = mean(Weight, na.rm = TRUE)) %>% 
      ungroup()
    
    if (nrow(weight_data) == 0) {
      blank_data <- data.frame(
        x = numeric(0),
        y = numeric(0)
      )
      
      blank_data %>% 
        e_charts(x) %>% 
        e_x_axis(show = FALSE) %>% 
        e_y_axis(show = FALSE) %>% 
        e_draft(text = "No Data", size = "50px")
      
    } else {
      min_weight <- round(min(weight_data$Weight) / 10) * 10
      
      first_weight <- weight_data %>% 
        slice(1) %>% 
        pull(Weight)
      
      most_recent_weight <- weight_data %>% 
        slice(n()) %>% 
        pull(Weight)
      
      weight_difference <- round(most_recent_weight - first_weight, 1)
      
      icon_color <- ifelse(weight_difference >= 0, "green", "red")
      plus_minus <- ifelse(weight_difference >= 0, "+", "")
      
      weight_data %>% 
        e_charts(Date) %>% 
        e_line(Weight) %>% 
        e_legend(FALSE) %>% 
        e_tooltip(trigger = "axis") %>% 
        e_y_axis(min = min_weight - 10) %>% 
        e_title(paste0(plus_minus, weight_difference), left = "right", textStyle = list(color = icon_color))
    }
  })
  
  output$speed_attendancePlot <- renderEcharts4r({
    attendance_data <- speed_data %>% 
      group_by(Month, Year) %>% 
      summarise(Attendance = round(mean(Attendance, na.rm = TRUE), 1), .groups = 'drop') %>% 
      ungroup() %>% 
      mutate(Date = ym(paste(Year, Month, sep = " "))) %>% 
      arrange(Date) %>% 
      mutate(Date = format(Date, "%b-%Y"))
    
      attendance_data %>% 
        e_charts(Date) %>% 
        e_bar(Attendance) %>% 
        e_legend(FALSE) %>% 
        e_tooltip(trigger = "item") %>% 
        e_mark_line(data = list(yAxis = 6), title = "Goal", lineStyle = list(color = "#3ba272", width = 3))
  })
  
  output$strength_attendancePlot <- renderEcharts4r({
    attendance_data <- strength_data %>% 
      group_by(Month, Year) %>% 
      summarise(Attendance = round(mean(Attendance, na.rm = TRUE), 1), .groups = 'drop') %>% 
      ungroup() %>% 
      mutate(Date = ym(paste(Year, Month, sep = " "))) %>% 
      arrange(Date) %>% 
      mutate(Date = format(Date, "%b-%Y"))
    
      attendance_data %>% 
        e_charts(Date) %>% 
        e_bar(Attendance) %>% 
        e_legend(FALSE) %>% 
        e_tooltip(trigger = "item") %>% 
        e_mark_line(data = list(yAxis = 9), title = "Goal", lineStyle = list(color = "#3ba272", width = 3))
  })
  
  output$athlete_speed_attendancePlot <- renderEcharts4r({
    attendance_data <- speed_data %>% 
      filter(Name == input$athlete, !is.na(Attendance)) %>% 
      group_by(Month, Year) %>% 
      summarise(Attendance = round(mean(Attendance, na.rm = TRUE), 1), .groups = 'drop') %>% 
      ungroup() %>% 
      mutate(Date = ym(paste(Year, Month, sep = " "))) %>% 
      arrange(Date) %>% 
      mutate(Date = format(Date, "%b-%Y"))
    
    if (nrow(attendance_data) == 0) {
      blank_data <- data.frame(
        x = numeric(0),
        y = numeric(0)
      )
      
      blank_data %>% 
        e_charts(x) %>% 
        e_x_axis(show = FALSE) %>% 
        e_y_axis(show = FALSE) %>% 
        e_draft(text = "No Data", size = "50px")
      
    } else {
      max_attendance <- max(attendance_data$Attendance, na.rm = TRUE)
      y_axis_max <- max(max_attendance, 7)
      
      attendance_data %>% 
        e_charts(Date) %>% 
        e_bar(Attendance) %>% 
        e_legend(FALSE) %>% 
        e_tooltip(trigger = "item") %>% 
        e_y_axis(min = 0, max = y_axis_max) %>%
        e_mark_line(data = list(yAxis = 6), title = "Goal", lineStyle = list(color = "#3ba272", width = 3))
    }
  })
  
  output$athlete_strength_attendancePlot <- renderEcharts4r({
    attendance_data <- strength_data %>% 
      filter(Name == input$athlete, !is.na(Attendance)) %>% 
      group_by(Month, Year) %>% 
      summarise(Attendance = round(mean(Attendance, na.rm = TRUE), 1), .groups = 'drop') %>% 
      ungroup() %>% 
      mutate(Date = ym(paste(Year, Month, sep = " "))) %>% 
      arrange(Date) %>% 
      mutate(Date = format(Date, "%b-%Y"))
    
    if (nrow(attendance_data) == 0) {
      blank_data <- data.frame(
        x = numeric(0),
        y = numeric(0)
      )
      
      blank_data %>% 
        e_charts(x) %>% 
        e_x_axis(show = FALSE) %>% 
        e_y_axis(show = FALSE) %>% 
        e_draft(text = "No Data", size = "50px")
      
    } else {
      max_attendance <- max(attendance_data$Attendance, na.rm = TRUE)
      y_axis_max <- max(max_attendance, 10)
      
      attendance_data %>% 
        e_charts(Date) %>% 
        e_bar(Attendance) %>% 
        e_legend(FALSE) %>% 
        e_tooltip(trigger = "item") %>% 
        e_y_axis(min = 0, max = y_axis_max) %>%
        e_mark_line(data = list(yAxis = 9), title = "Goal", lineStyle = list(color = "#3ba272", width = 3))
    }
  })
  
  output$speed_serviceCount <- renderEcharts4r({
    cleaned_data <- speed_data %>%
      distinct(Name, Date, .keep_all = TRUE) %>% 
      mutate(ServiceName = replace_na(ServiceName, "Not Checked In"))
    
    service_counts <- cleaned_data %>%
      group_by(Year, Month, ServiceName) %>%
      summarise(EntryCount = n()) %>%
      ungroup() %>%
      mutate(Date = ym(paste(Year, Month, sep = " "))) %>% 
      arrange(Date) %>%
      select(Date, ServiceName, EntryCount) %>%  # Select only the necessary columns
      pivot_wider(names_from = ServiceName, values_from = EntryCount, values_fill = 0) 
    
    service_counts %>% 
      e_charts(Date) %>% 
      e_bar(`Speed L1`, stack = "grp") %>%  
      e_bar(`Speed L2`, stack = "grp") %>% 
      e_bar(`Speed L3`, stack = "grp") %>% 
      e_bar(`Summer Camp G1`, stack = "grp") %>% 
      e_bar(`Summer Camp G2`, stack = "grp") %>% 
      e_bar(`Summer Camp G3`, stack = "grp") %>% 
      e_bar(`Learning Academy - Attended`, stack = "grp") %>% 
      e_bar(`Not Checked In`, stack = "grp") %>% 
      e_tooltip(trigger = "axis") %>% 
      e_legend(type = "scroll")
  })
  
  output$strength_serviceCount <- renderEcharts4r({
    cleaned_data <- strength_data %>%
      distinct(Name, Date, .keep_all = TRUE) %>% 
      mutate(ServiceName = replace_na(ServiceName, "Not Checked In"))
    
    service_counts <- cleaned_data %>%
      group_by(Year, Month, ServiceName) %>%
      summarise(EntryCount = n()) %>%
      ungroup() %>%
      mutate(Date = ym(paste(Year, Month, sep = " "))) %>% 
      arrange(Date) %>%
      select(Date, ServiceName, EntryCount) %>%  # Select only the necessary columns
      pivot_wider(names_from = ServiceName, values_from = EntryCount, values_fill = 0) 
    
    service_counts %>% 
      e_charts(Date) %>% 
      e_bar(`Strength Pro L3`, stack = "grp") %>%  
      e_bar(`Strength Pro L2/L3`, stack = "grp") %>% 
      e_bar(`Strength Base L3`, stack = "grp") %>% 
      e_bar(`Strength Base L2`, stack = "grp") %>% 
      e_bar(`Strength Base L2/L3`, stack = "grp") %>% 
      e_bar(`Summer Camp G1`, stack = "grp") %>% 
      e_bar(`Summer Camp G2`, stack = "grp") %>% 
      e_bar(`Summer Camp G3`, stack = "grp") %>% 
      e_bar(`Learning Academy - Attended`, stack = "grp") %>% 
      e_bar(`Professional - Facility Access`, stack = "grp") %>% 
      e_bar(`Not Checked In`, stack = "grp") %>% 
      e_tooltip(trigger = "axis") %>% 
      e_legend(type = "scroll")
  })
  
  output$correlation_plot <- renderEcharts4r({
  })

  # Download Report
  output$download_report <- downloadHandler(
    filename = function() {
      paste("performance_report", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Code to generate and save the PDF report
    }
  )
}

shinyApp(ui, server)
