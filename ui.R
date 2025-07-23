# ui.R

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(reactable)
library(webshot2)
library(shinyWidgets)
library(waiter)
library(gt)

ui <- tagList(
  useWaiter(),
  useWaitress(),
  waiterPreloader(
    html = tagList(
      spin_loaders(id = 19, color = "black", style = NULL),
      tags$h3("Loading data...", style = "margin-top: 20px; color: black;")
    ),
    color = "white"
  ),

  tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"),
  
  # ───── Main UI (hidden until data loads) ─────
  tags$div(
    page_navbar(
      title = tags$a(
        href = "#",
        tags$img(src = "futures.png", height = "15px")
      ),
      
      tags$head(
        tags$meta(name = "viewport", content = "width=1400")
      ),
      
      navbar_options = navbar_options(collapsible = TRUE, bg = "#2D89C8"),
      
      nav_panel("Facility",
                div(
                  style = "padding: 20px 10px; max-width: 1400px; margin: 0 auto; text-align: center;",
                  
                  # Title + pickerInput
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 30px;",
                    tags$h2("Facility Overview", style = "margin: 0;"),
                    div(
                      style = "margin-top: 20px;",
                      pickerInput("facility_metric",
                                  label = NULL,
                                  choices = list(
                                    "Hitting"   = c("HitTrax", "Blast"),
                                    "Pitching"  = c("Trackman", "Armcare"),
                                    "Strength"  = c("ForceDecks", "Dynamo", "Proteus"),
                                    "Speed"     = c("VALD")
                                  ),
                                  multiple = FALSE,
                                  selected = "HitTrax"
                      )
                    )
                  ),
                  
                  # Explanation Card
                  div(
                    style = "background-color: #f9f9f9; border: 1px solid #e0e0e0; border-radius: 10px; padding: 20px; margin-bottom: 30px; text-align: left;",
                    p("Each table displays athlete performance across key metrics within the selected filters."),
                    p("To ensure meaningful comparisons, athletes must meet a minimum session threshold within the selected date range:"),
                    tags$ul(
                      tags$li("Strength: At least 2 sessions"),
                      tags$li("Hitting, Pitching, and Speed: At least 6 sessions")
                    ),
                    p("Improvement calculations differ slightly based on the type of data:"),
                    tags$ul(
                      tags$li(tags$b("Strength:"), " Due to the limited number of strength assessments, improvement is calculated using the athlete's first and most recent recorded values."),
                      tags$li(tags$b("Hitting, Pitching, and Speed:"), " Improvement is calculated using the average of the first 3 and last 3 recorded values.")
                    ),
                    tags$ul(
                      tags$li(tags$b("First:"), " Starting performance (either first value for Strength or average of first 3 for others)"),
                      tags$li(tags$b("Last:"), " Most recent performance (either most recent value for Strength or average of last 3 for others)"),
                      tags$li(tags$b("Overall Average:"), " Mean of all recorded values"),
                      tags$li(tags$b("Average Improvement:"), " Difference between Last and First"),
                      tags$li(tags$b("Average % Improvement:"), " Percentage change from First to Last")
                    ),
                    p(tags$b("Note:"), " The Attendance filter is based on athletes' monthly attendance records.")
                  ),
                  
                  # Filters
                  div(
                    style = "display: flex; justify-content: center; gap: 20px; flex-wrap: wrap; margin-bottom: 30px;",
                    uiOutput("facility_date_range_ui"),
                    uiOutput("facility_level_ui"),
                    uiOutput("facility_service_ui"),
                    uiOutput("facility_gender_ui"),
                    uiOutput("facility_attendance_ui")
                  ),
                  
                  # GT Table
                  div(
                    style = "display: flex; justify-content: center; margin-top: 20px;",
                    gt_output("facility_progression_table")
                  )
                )
      ),
      
      
      nav_menu("Athlete",
               
               # ───── HITTING ─────
               nav_panel("Hitting",
                         div(
                           style = "margin-left: auto; margin-right: auto; max-width: 1400px;",
                           
                           layout_columns(
                             # Always-visible cards
                             card(
                               full_screen = FALSE,
                               style = "width: 400px; height: 650px;",
                               div(
                                 style = "display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 20px; width: 100%;",
                                 dateRangeInput("date_range_hitting", "Date Range:",
                                                start = Sys.Date() %m-% months(3), end = Sys.Date()),
                                 selectInput("selected_athlete_hitting", "Athlete:", choices = NULL),
                                 actionButton("get_profile_hitting", "Get Profile", class = "btn-primary"),
                                 uiOutput("athlete_profile_hitting")
                               )
                             ),
                             card(
                               full_screen = FALSE,
                               style = "width: 400px; height: 650px;",
                               div(
                                 style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                 div(style = "height: 60px; display: flex; align-items: center; justify-content: center;",
                                     tags$h4("Percentile Rankings", style = "margin: 0; text-align: center;")
                                 ),
                                 div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; width: 100%;",
                                     plotOutput("hitting_percentile_plot", height = "400px")
                                 )
                               )
                             ),
                             card(
                               full_screen = FALSE,
                               style = "width: 400px; height: 650px;",
                               div(
                                 style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                 div(style = "height: 60px; display: flex; align-items: center; justify-content: center;",
                                     tags$h4("Futures Score", style = "margin: 0; text-align: center;")
                                 ),
                                 div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; width: 100%;",
                                     uiOutput("futures_score_hitting")
                                 )
                               )
                             )
                           ),
                           
                           # ── Standard Summary Table ──
                           layout_columns(
                             div(
                               style = "margin: 0 auto 30px auto;",
                               gt_output("standard_summary_table")
                             )
                           ),
                           
                           layout_columns(
                             card(
                               full_screen = FALSE,
                               style = "width: 1260px; height: 725px; margin-top: 30px;",
                               div(
                                 style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                 div(style = "margin-top: 20px;",
                                     plotlyOutput("hitting_trend_plot", height = "550px")
                                 ),
                                 div(style = "display: flex; flex-direction: row; gap: 20px; justify-content: center; align-items: center; margin-top: 20px;",
                                     pickerInput("trend_metric", "Select a Metric:", choices = NULL,
                                                 options = list(dropupAuto = FALSE, dropdownAlignRight = FALSE, size = 6)),
                                     pickerInput("trend_grouping", "Group By:", choices = c("Date" = "date", "Month" = "month"),
                                                 selected = "date",
                                                 options = list(dropupAuto = FALSE, dropdownAlignRight = FALSE, size = 6))
                                 )
                               )
                             )
                           ),
                           
                           layout_columns(
                             div(style = "text-align: center;",
                                 actionButton("show_hittrax_table", "HitTrax"),
                                 actionButton("show_blast_table", "Blast")
                             )
                           ),
                           
                           uiOutput("hitting_summary_tables"),
                           
                           layout_columns(
                             card(
                               full_screen = FALSE,
                               style = "width: 1260px; height: 500px; margin-top: 30px;", # Match width and height to the others
                               div(
                                 style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                 div(
                                   style = "flex-grow: 1; display: flex; flex-direction: row; justify-content: space-around; align-items: center;",
                                   
                                   # Early Connection
                                   div(
                                     style = "display: flex; flex-direction: column; align-items: center;",
                                     h4("Early Connection"),
                                     textOutput("early_connection_value"),
                                     uiOutput("early_connection_img")
                                   ),
                                   
                                   # On Plane Efficiency
                                   div(
                                     style = "display: flex; flex-direction: column; align-items: center;",
                                     h4("On Plane Efficiency"),
                                     textOutput("ope_value"),
                                     uiOutput("ope_img")
                                   ),
                                   
                                   # Connection at Impact
                                   div(
                                     style = "display: flex; flex-direction: column; align-items: center;",
                                     h4("Connection at Impact"),
                                     textOutput("connection_impact_value"),
                                     uiOutput("connection_impact_img")
                                   )
                                 )
                               )
                             )
                           )
                         )
               ),
               
               # ───── PITCHING ─────
               nav_panel("Pitching",
                         div(
                           style = "margin-left: auto; margin-right: auto; max-width: 1400px;",
                           layout_columns(
                             card(
                               full_screen = FALSE,
                               style = "width: 400px; height: 650px;",
                               div(style = "display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 20px; width: 100%;",
                                   dateRangeInput("date_range_pitching", "Date Range:",
                                                  start = Sys.Date() %m-% months(3), end = Sys.Date()),
                                   selectInput("selected_athlete_pitching", "Athlete:", choices = NULL),
                                   actionButton("get_profile_pitching", "Get Profile", class = "btn-primary"),
                                   uiOutput("athlete_profile_pitching")
                               )
                             ),
                             card(
                               full_screen = FALSE,
                               style = "width: 400px; height: 650px;",
                               div(style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                   div(style = "height: 60px; display: flex; align-items: center; justify-content: center;",
                                       tags$h4("Percentile Rankings", style = "margin: 0; text-align: center;")
                                   ),
                                   div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; width: 100%;",
                                       plotOutput("pitching_percentile_plot", height = "400px")
                                   )
                               )
                             ),
                             card(
                               full_screen = FALSE,
                               style = "width: 400px; height: 650px;",
                               div(style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                   div(style = "height: 60px; display: flex; align-items: center; justify-content: center;",
                                       tags$h4("Futures Score", style = "margin: 0; text-align: center;")
                                   ),
                                   div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; width: 100%;",
                                       uiOutput("futures_score_pitching")
                                   )
                               )
                             )
                           ),
                           
                           layout_columns(
                             div(style = "text-align: center;",
                                 actionButton("show_trackman_table",   "Trackman"),
                                 actionButton("show_armcare_table",    "Armcare")
                             )
                           ),
                           
                           uiOutput("pitching_summary_tables"),
                           
                           layout_columns(
                             card(
                               full_screen = FALSE,
                               style = "width: 1260px; height: 725px; margin-top: 30px;",
                               div(
                                 style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                 div(style = "margin-top: 20px;",
                                     plotlyOutput("pitching_trend_plot", height = "550px")
                                 ),
                                 div(style = "display: flex; flex-direction: row; gap: 20px; justify-content: center; align-items: center; margin-top: 20px;",
                                     pickerInput("pitching_trend_metric", "Select a Metric:", choices = NULL,
                                                 options = list(dropupAuto = FALSE, dropdownAlignRight = FALSE, size = 6)),
                                     pickerInput("pitching_trend_grouping", "Group By:", choices = c("Date" = "date", "Month" = "month"),
                                                 selected = "date",
                                                 options = list(dropupAuto = FALSE, dropdownAlignRight = FALSE, size = 6))
                                 )
                               )
                             )
                           ),
                           
                           # ── Advanced Summary Table ──
                           layout_columns(
                             div(
                               style = "margin: 0 auto 30px auto;",
                               gt_output("advanced_summary_table")
                             )
                           ),
                           
                           layout_columns(
                             card(
                               full_screen = FALSE,
                               style = "width: 1260px; height: 725px; margin-top: 30px;",
                               div(
                                 style = "display: flex; flex-direction: row; justify-content: space-between; align-items: center; height: 100%; width: 100%; padding: 20px;",
                                 
                                 div(
                                   style = "flex: 1; margin-right: 10px;",
                                   plotlyOutput("release_trend_plot", height = "650px")
                                 ),
                                 
                                 div(
                                   style = "flex: 1; margin-left: 10px;",
                                   plotlyOutput("movement_trend_plot", height = "650px")
                                 )
                               )
                             )
                           )
                         )
               ),
               
               # ───── STRENGTH ─────
               nav_panel("Strength",
                         div(
                           style = "margin-left: auto; margin-right: auto; max-width: 1400px;",
                           layout_columns(
                             card(
                               full_screen = FALSE,
                               style = "width: 400px; height: 650px;",
                               div(style = "display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 20px; width: 100%;",
                                   dateRangeInput("date_range_strength", "Date Range:",
                                                  start = Sys.Date() %m-% months(3), end = Sys.Date()),
                                   selectInput("selected_athlete_strength", "Athlete:", choices = NULL),
                                   actionButton("get_profile_strength", "Get Profile", class = "btn-primary"),
                                   uiOutput("athlete_profile_strength")
                               )
                             ),
                             card(
                               full_screen = FALSE,
                               style = "width: 400px; height: 650px;",
                               div(style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                   div(style = "height: 60px; display: flex; align-items: center; justify-content: center;",
                                       tags$h4("Percentile Rankings", style = "margin: 0; text-align: center;")
                                   ),
                                   div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; width: 100%;",
                                       plotOutput("strength_percentile_plot", height = "400px")
                                   )
                               )
                             ),
                             card(
                               full_screen = FALSE,
                               style = "width: 400px; height: 650px;",
                               div(style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                   div(style = "height: 60px; display: flex; align-items: center; justify-content: center;",
                                       tags$h4("Futures Score", style = "margin: 0; text-align: center;")
                                   ),
                                   div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; width: 100%;",
                                       uiOutput("futures_score_strength")
                                   )
                               )
                             )
                           ),
                           
                           layout_columns(
                             div(style = "text-align: center;",
                                 actionButton("show_forceplate_table",  "ForcePlate"),
                                 actionButton("show_dynamo_table",      "Dynamo"),
                                 actionButton("show_proteus_table",     "Proteus")
                             )
                           ),
                           
                           uiOutput("strength_summary_tables"),
                           
                           layout_columns(
                             card(
                               full_screen = FALSE,
                               style = "width: 1260px; height: 725px; margin-top: 30px;",
                               div(
                                 style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                 div(style = "margin-top: 20px;",
                                     plotlyOutput("strength_trend_plot", height = "550px")
                                 ),
                                 div(style = "display: flex; flex-direction: row; gap: 20px; justify-content: center; align-items: center; margin-top: 20px;",
                                     pickerInput("strength_trend_metric", "Select a Metric:", choices = NULL,
                                                 options = list(dropupAuto = FALSE, dropdownAlignRight = FALSE, size = 6)),
                                     pickerInput("strength_trend_grouping", "Group By:", choices = c("Date" = "date", "Month" = "month"),
                                                 selected = "date",
                                                 options = list(dropupAuto = FALSE, dropdownAlignRight = FALSE, size = 6))
                                 )
                               )
                             )
                           )
                         )
               ),
               
               # ───── SPEED ─────
               nav_panel("Speed",
                         div(
                           style = "margin-left: auto; margin-right: auto; max-width: 1400px;",
                           layout_columns(
                             card(
                               full_screen = FALSE,
                               style = "width: 400px; height: 650px;",
                               div(style = "display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 20px; width: 100%;",
                                   dateRangeInput("date_range_speed", "Date Range:",
                                                  start = Sys.Date() %m-% months(3), end = Sys.Date()),
                                   selectInput("selected_athlete_speed", "Athlete:", choices = NULL),
                                   actionButton("get_profile_speed", "Get Profile", class = "btn-primary"),
                                   uiOutput("athlete_profile_speed")
                               )
                             ),
                             card(
                               full_screen = FALSE,
                               style = "width: 400px; height: 650px;",
                               div(style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                   div(style = "height: 60px; display: flex; align-items: center; justify-content: center;",
                                       tags$h4("Percentile Rankings", style = "margin: 0; text-align: center;")
                                   ),
                                   div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; width: 100%;",
                                       plotOutput("speed_percentile_plot", height = "400px")
                                   )
                               )
                             ),
                             card(
                               full_screen = FALSE,
                               style = "width: 400px; height: 650px;",
                               div(style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                   div(style = "height: 60px; display: flex; align-items: center; justify-content: center;",
                                       tags$h4("Futures Score", style = "margin: 0; text-align: center;")
                                   ),
                                   div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; width: 100%;",
                                       uiOutput("futures_score_speed")
                                   )
                               )
                             )
                           ),
                           
                           # ── SmartSpeed Summary Table ──
                           layout_columns(
                             div(
                               style = "margin: 0 auto 30px auto;",
                               gt_output("smartspeed_summary_table")
                             )
                           ),
                           
                           layout_columns(
                             card(
                               full_screen = FALSE,
                               style = "width: 1260px; height: 725px; margin-top: 30px;",
                               div(
                                 style = "display: flex; flex-direction: column; height: 100%; width: 100%;",
                                 div(style = "margin-top: 20px;",
                                     plotlyOutput("speed_trend_plot", height = "550px")
                                 ),
                                 div(style = "display: flex; flex-direction: row; gap: 20px; justify-content: center; align-items: center; margin-top: 20px;",
                                     pickerInput("speed_trend_metric", "Select a Metric:", choices = NULL,
                                                 options = list(dropupAuto = FALSE, dropdownAlignRight = FALSE, size = 6)),
                                     pickerInput("speed_trend_grouping", "Group By:", choices = c("Date" = "date", "Month" = "month"),
                                                 selected = "date",
                                                 options = list(dropupAuto = FALSE, dropdownAlignRight = FALSE, size = 6))
                                 )
                               )
                             )
                           )
                         )
               )
               
      ),
      
      nav_panel("Leaderboards",
                div(
                  style = "padding: 20px 10px; max-width: 1400px; margin: 0 auto; text-align: center;",
                  
                  # ── Title + Leaderboard Picker ──
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 30px;",
                    tags$h2("Department Leaderboards", style = "margin: 0;"),
                    div(
                      style = "margin-top: 20px;",
                      pickerInput(
                        inputId = "leaderboard_metric",
                        label = NULL,
                        choices = c("Hitting", "Pitching", "Strength", "Speed"),
                        selected = "Hitting",
                        multiple = FALSE
                      )
                    )
                  ),
                  
                  # Leaderboards Explanation Card
                  div(
                    style = "background-color: #f9f9f9; border: 1px solid #e0e0e0; border-radius: 10px; padding: 20px; margin-bottom: 30px; text-align: left;",
                    tags$p("This panel highlights top-performing athletes across departments."),
                    tags$p("Each leaderboard ranks athletes based on their peak or average values within the selected time frame and filters."),
                    tags$p("Use the filters below to customize the leaderboard:"),
                    tags$ul(
                      tags$li(tags$b("Date Range:"), " View performances recorded within a specific time window."),
                      tags$li(tags$b("Gender:"), " Filter athletes by gender (when applicable)."),
                      tags$li(tags$b("Level:"), " Choose one or more athlete levels.")
                    ),
                    tags$p(tags$b("Tip:"), " All tables are interactive and sortable.")
                  ),
                  
                  # ── Filters: Date Range, Level, Gender (conditional) ──
                  div(
                    style = "display: flex; justify-content: center; gap: 20px; flex-wrap: wrap; margin-bottom: 30px;",
                    uiOutput("leaderboard_date_range_ui"),
                    uiOutput("leaderboard_gender_ui"),
                    uiOutput("leaderboard_level_ui")
                  ),

                  # ── Reactable Output ──
                  div(
                    style = "display: flex; justify-content: center; margin-top: 20px;",
                    reactableOutput("leaderboard_table")
                  )
                )
      ),
      
      nav_panel("Percentiles",
                div(
                  style = "padding: 20px 10px; max-width: 1400px; margin: 0 auto; text-align: center;",
                  
                  # ── Title + Department Picker ──
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 30px;",
                    tags$h2("Percentile Distributions", style = "margin: 0;"),
                    div(
                      style = "margin-top: 20px;",
                      pickerInput(
                        inputId = "percentile_department",
                        label   = NULL,
                        choices = c("Hitting", "Pitching", "Strength", "Speed"),
                        selected= "Hitting",
                        multiple= FALSE
                      )
                    )
                  ),
                  
                  # Percentiles Explanation Card
                  div(
                    style = "background-color: #f9f9f9; border: 1px solid #e0e0e0; border-radius: 10px; padding: 20px; margin-bottom: 30px; text-align: left;",
                    tags$p("This panel visualizes the distribution of athlete performance within and across levels, allowing you to see how athletes stack up in key metrics."),
                    tags$ul(
                      tags$li(tags$b("Metric:"), " Choose the specific skill or test."),
                      tags$li(tags$b("Gender:"), " Filter athletes by gender (when applicable)."),
                      tags$li(tags$b("Level(s):"), " Select up to two levels for comparison.")
                    ),
                    tags$p("Each plot displays a smoothed density curve by level. Hover to see values and estimated percentiles."),
                    tags$ul(
                      tags$li(tags$b("Value:"), " Raw measurement"),
                      tags$li(tags$b("Percentile:"), " Estimated standing within the selected group"),
                      tags$li(tags$b("Time-based metrics:"), " Shorter times equal higher percentiles")
                    ),
                    tags$p(tags$b("Tip:"), " Use this panel to benchmark athletes, compare levels, and identify areas for improvement.")
                  ),
                  
                  # ── Filters: Metric, Level(s), Gender ──
                  div(
                    style = "display: flex; justify-content: center; gap: 20px; flex-wrap: wrap; margin-bottom: 30px;",
                    uiOutput("metric_selector"),            # same pattern as your other panels
                    uiOutput("percentile_gender_ui"),       # wrap your gender picker in a UI output
                    uiOutput("percentile_levels_ui")        # wrap your level picker in a UI output
                  ),
                  
                  # ── Plot Output ──
                  div(
                    style = "display: flex; justify-content: center; margin-top: 20px;",
                    plotlyOutput("percentile_plot", height = "500px", width = "1260px")
                  )
                )
      ),
      
      nav_panel("Reports",
                div(
                  id   = "report_card",
                  class = "mx-auto px-4",
                  style = "display: flex; justify-content: center;",
                  card(
                    class = "p-4 shadow-sm",
                    style = "width: 400px; height: 550px;",
                    card_header(
                      h3("Generate Athlete Report", class = "text-center mb-4")
                    ),
                    div(
                      class = "d-flex flex-column align-items-stretch gap-3",
                      uiOutput("athlete_selector"),
                      
                      layout_columns(
                        col_widths = c(6, 6),
                        
                        selectInput(
                          "report_month", "Month:",
                          choices = month.name,
                          selected = format(Sys.Date(), "%B"),
                          width = "100%"
                        ),
                        
                        selectInput(
                          "report_year", "Year:",
                          choices = 2023:2025,
                          selected = as.numeric(format(Sys.Date(), "%Y")),
                          width = "100%"
                        )
                      ),
                      
                      div(style = "margin-top: 25px;"),
                      
                      uiOutput("report_buttons")
                    )
                  )
                )
      )
    ),
    
    tags$footer(
      style = "
    background-color: #2D89C8;
    color: white;
    padding: 20px 10px;
    text-align: center;
    margin-top: 40px;
  ",
      div(
        style = "display: flex; justify-content: center; align-items: center; gap: 30px; flex-wrap: wrap;",
        
        # Company logo
        tags$a(href = "https://www.futurestrainingcenter.com/", target = "_blank",
               tags$img(src = "full_logo.png", height = "40px", style = "margin-right: 20px;")
        ),
        
        # Socials (FontAwesome icons)
        tags$a(href = "https://www.instagram.com/futurestrainingcenter/", target = "_blank",
               tags$i(class = "fab fa-instagram", style = "font-size: 24px; color: white;")
        ),
        tags$a(href = "https://x.com/futures_tc", target = "_blank",
               tags$i(class = "fab fa-x-twitter", style = "font-size: 24px; color: white;")
        ),
        tags$a(href = "https://www.facebook.com/futurestrainingcentercorona/", target = "_blank",
               tags$i(class = "fab fa-facebook", style = "font-size: 24px; color: white;")
        ),
        tags$a(href = "https://www.linkedin.com/company/futures-training-center/posts/?feedView=all", target = "_blank",
               tags$i(class = "fab fa-linkedin", style = "font-size: 24px; color: white;")
        )
      ),
      
      tags$p("© 2025 Futures Training Center. All rights reserved.", style = "margin-top: 15px;")
    )
  )
)

shinyUI(ui)
