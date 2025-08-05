# server.R

library(shiny)
library(readxl)
library(tidyverse)
library(pdftools)
library(plotly)
library(gt)
library(reactable)

# Source report functions
source("HittingReport.R", local = TRUE)
source("PitchingReport.R", local = TRUE)
source("StrengthReport.R", local = TRUE)
source("SpeedReport.R", local = TRUE)

server <- function(input, output, session) {
  
  # ── Load data once at startup ──────────────────────────────────────
  hitting_data <- readRDS("HittingFacilityData.rds") %>%
    filter(!`Service Name` %in% c(
      "Baseball Cage Rental L1", "Baseball Cage Rental L2", "Baseball Cage Rental L3", 
      "Softball Cage Rental L1", "Softball Cage Rental L2", "Softball Cage Rental L3"))
  
  hittrax_data   <- readRDS("HittraxData.rds")
  pitching_data  <- readRDS("PitchingFacilityData.rds")
  trackman_data  <- readRDS("TrackmanData.rds")
  strength_data  <- readRDS("StrengthFacilityData.rds")
  speed_data     <- readRDS("SpeedFacilityData.rds")
  client_data    <- readRDS("ClientData.rds")
  
  # ── Facility dropdowns ───────────────────────────────────────────
  
  output$facility_date_range_ui <- renderUI({
    data <- switch(input$facility_metric,
                   "HitTrax"   = hitting_data,
                   "Blast"     = hitting_data,
                   "Trackman"  = pitching_data,
                   "Armcare"   = pitching_data,
                   "ForceDecks"= strength_data,
                   "Dynamo"    = strength_data,
                   "Proteus"   = strength_data,
                   "VALD"      = speed_data
    )
    req(data)
    dateRangeInput("facility_date_range", "Select Date Range:",
                   start = Sys.Date() %m-% months(6), end = Sys.Date(), width = '280px')
  })
  
  output$facility_level_ui <- renderUI({
    data <- switch(input$facility_metric,
                   "HitTrax"   = hitting_data,
                   "Blast"     = hitting_data,
                   "Trackman"  = pitching_data,
                   "Armcare"   = pitching_data,
                   "ForceDecks"= strength_data,
                   "Dynamo"    = strength_data,
                   "Proteus"   = strength_data,
                   "VALD"      = speed_data
    )
    req(data)
    pickerInput("facility_level", "Select Level:",
                choices = sort(unique(data$Level)),
                selected = c("L1", "L2", "L3"),
                multiple = TRUE,
                width =  '225px')
  })
  
  output$facility_service_ui <- renderUI({
    data <- switch(input$facility_metric,
                   "HitTrax"    = hitting_data,
                   "Blast"      = hitting_data,
                   "Trackman"   = pitching_data,
                   "Armcare"    = pitching_data,
                   "ForceDecks" = strength_data,
                   "Dynamo"     = strength_data,
                   "Proteus"    = strength_data,
                   "VALD"       = speed_data
    )
    req(data)
    
    default_selected <- switch(input$facility_metric,
                               "HitTrax"  = c("Baseball Hitting L1", "Baseball Hitting L2", "Baseball Hitting L3", "Learning Academy"),
                               "Blast"    = c("Baseball Hitting L1", "Baseball Hitting L2", "Baseball Hitting L3", "Learning Academy"),
                               "Trackman" = c("Baseball Pitching L1", "Baseball Pitching L2", "Baseball Pitching L3", "Learning Academy"),
                               "Armcare"  = c("Baseball Pitching L1", "Baseball Pitching L2", "Baseball Pitching L3", "Learning Academy"),
                               "ForceDecks" = c("Strength L2", "Strength L3", "Power L3", "Learning Academy"),
                               "Dynamo"     = c("Strength L2", "Strength L3", "Power L3", "Learning Academy"),
                               "Proteus"    = c("Strength L2", "Strength L3", "Power L3", "Learning Academy"),
                               "VALD"    = c("Speed L1", "Speed L1/L2", "Speed L2", "Speed L2/L3", "Speed L3", "Learning Academy"),
                               character(0)
    )
    
    pickerInput("facility_service", "Select Service Name:",
                choices = sort(unique(data$`Service Name`)),
                selected = default_selected,
                multiple = TRUE,
                width =  '225px')
  })
  
  output$facility_gender_ui <- renderUI({
    if (input$facility_metric %in% c("Trackman", "Armcare")) {
      return(NULL)
    }
    
    data <- switch(input$facility_metric,
                   "HitTrax"    = hitting_data,
                   "Blast"      = hitting_data,
                   "ForceDecks" = strength_data,
                   "Dynamo"     = strength_data,
                   "Proteus"    = strength_data,
                   "VALD"       = speed_data
    )
    req(data)
    
    pickerInput("facility_gender", "Select Gender:",
                choices = c("Male", "Female"),
                selected = c("Male", "Female"),
                multiple = TRUE,
                width =  '225px')
  })
  
  output$facility_attendance_ui <- renderUI({
    data <- switch(input$facility_metric,
                   "HitTrax"   = hitting_data,
                   "Blast"     = hitting_data,
                   "Trackman"  = pitching_data,
                   "Armcare"   = pitching_data,
                   "ForceDecks"= strength_data,
                   "Dynamo"    = strength_data,
                   "Proteus"   = strength_data,
                   "VALD"      = speed_data
    )
    req(data)
    pickerInput("facility_attendance", "Select Attendance:",
                choices = sort(unique(data$Attendance)),
                selected = unique(data$Attendance),
                multiple = TRUE,
                width =  '225px')
  })
  
  
  generate_hittrax_gt <- function() {
    req(input$facility_date_range, input$facility_level, input$facility_service, input$facility_gender, input$facility_attendance)
    
    # Step 1: Main data with improvements
    df <- hitting_data %>%
      filter(
        Date >= input$facility_date_range[1],
        Date <= input$facility_date_range[2],
        Level %in% input$facility_level,
        `Service Name` %in% input$facility_service,
        Gender %in% input$facility_gender,
        Attendance %in% input$facility_attendance
      ) %>%
      mutate(
        LDP = LDC / HC * 100,
        HHP = HHC / HC * 100
      ) %>%
      pivot_longer(
        cols = c(MaxVel, AvgVel, MaxDist, LDP, HHP),
        names_to  = "metric_col",
        values_to = "Value"
      ) %>%
      filter(!is.na(Value)) %>%
      group_by(`Service Name`, Name, metric_col) %>%
      arrange(Date) %>%
      filter(n() >= 6) %>%
      summarise(
        First      = mean(head(Value, 3), na.rm = TRUE),
        Last       = mean(tail(Value, 3), na.rm = TRUE),
        OverallAvg = mean(Value,          na.rm = TRUE),
        .groups     = "drop"
      ) %>%
      mutate(
        Improvement = Last - First,
        Percent = case_when(
          metric_col %in% c("LDP", "HHP") ~ NA_real_,
          First == 0 ~ NA_real_,
          TRUE ~ Improvement / First * 100
        ),
        Exercise = case_when(
          metric_col == "MaxVel"  ~ "Max Exit Velocity",
          metric_col == "AvgVel"  ~ "Avg Exit Velocity",
          metric_col == "MaxDist" ~ "Max Distance",
          metric_col == "LDP"     ~ "Line Drive %",
          metric_col == "HHP"     ~ "Hard Hit %"
        )
      )
    
    # Step 2: Grouped data for display table
    df_grouped <- df %>%
      group_by(`Service Name`, Exercise) %>%
      summarise(
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 3: True summary row based on all data (not service averages)
    summary_row <- df %>%
      group_by(Exercise) %>%
      summarise(
        `Service Name` = "Summary",
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 4: Combine into final table
    df_final <- bind_rows(summary_row, df_grouped)
    
    # Step 5: Generate gt table
    df_final %>%
      gt(groupname_col = "Exercise", rowname_col = "Service Name") %>%
      text_transform(
        locations = cells_body(columns = c(`Average % Improvement`)),
        fn = function(x) {
          paste0(x, "%")
        }
      ) %>%
      tab_style(
        style = list(cell_text(color = "#008000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` > 0)
        )
      ) %>%
      tab_style(
        style = list(cell_text(color = "#FF0000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` < 0)
        )
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_style(
        style = cell_fill(color = "gray95"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_options(
        table.width = px(1260),
        row_group.as_column = TRUE,
        table.border.top.style = "hidden",
        column_labels.border.top.style = "hidden"
      ) %>%
      tab_stub_indent(
        rows = everything(),
        indent = 2
      ) %>%
      cols_width(
        Exercise ~ px(300),
        `Service Name` ~ px(300),
        `Overall Average` ~ px(220),
        `Average Improvement` ~ px(220),
        `Average % Improvement` ~ px(220)
      ) %>%
      sub_missing()
  }
  
  generate_blast_gt <- function() {
    req(input$facility_date_range, input$facility_level, input$facility_service, input$facility_gender, input$facility_attendance)
    
    # Step 1: Prepare base data
    df <- hitting_data %>%
      filter(
        Date >= input$facility_date_range[1],
        Date <= input$facility_date_range[2],
        Level %in% input$facility_level,
        `Service Name` %in% input$facility_service,
        Gender %in% input$facility_gender,
        Attendance %in% input$facility_attendance
      ) %>%
      pivot_longer(
        cols = c(bat_speed, rotational_acceleration),
        names_to  = "metric_col",
        values_to = "Value"
      ) %>%
      filter(!is.na(Value)) %>%
      group_by(`Service Name`, Name, metric_col) %>%
      arrange(Date) %>%
      filter(n() >= 6) %>%
      summarise(
        First      = mean(head(Value, 3), na.rm = TRUE),
        Last       = mean(tail(Value, 3), na.rm = TRUE),
        OverallAvg = mean(Value,          na.rm = TRUE),
        .groups     = "drop"
      ) %>%
      mutate(
        Improvement = Last - First,
        Percent     = if_else(First == 0, NA_real_, Improvement / First * 100),
        Exercise    = case_when(
          metric_col == "bat_speed" ~ "Bat Speed",
          metric_col == "rotational_acceleration" ~ "Rotational Acceleration"
        )
      )
    
    # Step 2: Group by Service and Exercise
    df_grouped <- df %>%
      group_by(`Service Name`, Exercise) %>%
      summarise(
        `Overall Average`       = round(mean(OverallAvg,  na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement, na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,     na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 3: True summary row based on all underlying data
    summary_row <- df %>%
      group_by(Exercise) %>%
      summarise(
        `Service Name`          = "Summary",
        `Overall Average`       = round(mean(OverallAvg,  na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement, na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,     na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 4: Combine summary row with main data
    df_final <- bind_rows(summary_row, df_grouped)
    
    # Step 5: Generate gt table with styled summary row
    df_final %>%
      gt(groupname_col = "Exercise", rowname_col = "Service Name") %>%
      text_transform(
        locations = cells_body(columns = c(`Average % Improvement`)),
        fn = function(x) {
          paste0(x, "%")
        }
      ) %>%
      tab_style(
        style = list(cell_text(color = "#008000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` > 0)
        )
      ) %>%
      tab_style(
        style = list(cell_text(color = "#FF0000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` < 0)
        )
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_style(
        style = cell_fill(color = "gray95"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_options(
        table.width = px(1260),
        row_group.as_column = TRUE,
        table.border.top.style = "hidden",
        column_labels.border.top.style = "hidden"
      ) %>%
      tab_stub_indent(
        rows = everything(),
        indent = 2
      ) %>%
      cols_width(
        Exercise ~ px(300),
        `Service Name` ~ px(300),
        `Overall Average` ~ px(220),
        `Average Improvement` ~ px(220),
        `Average % Improvement` ~ px(220)
      )
  }
  
  generate_trackman_gt <- function() {
    req(input$facility_date_range, input$facility_level, input$facility_service, input$facility_attendance)
    
    # Step 1: Main data with improvements
    df <- pitching_data %>%
      filter(
        TaggedPitchType == "Fastball",
        Date >= input$facility_date_range[1],
        Date <= input$facility_date_range[2],
        Level %in% input$facility_level,
        `Service Name` %in% input$facility_service,
        Attendance %in% input$facility_attendance
      ) %>%
      mutate(
        Strike_Percentage = ifelse(Pitch_Count > 0, Strike_Count / Pitch_Count * 100, NA_real_)
      ) %>%
      pivot_longer(
        cols = c(Avg_RelSpeed, Max_RelSpeed, Extension, Strike_Percentage),
        names_to  = "metric_col",
        values_to = "Value"
      ) %>%
      filter(!is.na(Value)) %>%
      group_by(`Service Name`, Name, metric_col) %>%
      arrange(Date) %>%
      filter(n() >= 2) %>%
      summarise(
        First      = first(Value, na_rm = TRUE),
        Last       = last(Value,  na_rm = TRUE),
        OverallAvg = mean(Value,  na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      mutate(
        Improvement = Last - First,
        Percent = case_when(
          metric_col == "Strike_Percentage" ~ NA_real_,
          First == 0 ~ NA_real_,
          TRUE ~ Improvement / First * 100
        ),
        Exercise = case_when(
          metric_col == "Avg_RelSpeed"      ~ "Avg Fastball Velocity",
          metric_col == "Max_RelSpeed"      ~ "Max Fastball Velocity",
          metric_col == "Extension"         ~ "Extension (Fastball)",
          metric_col == "Strike_Percentage" ~ "Strike %"
        )
      )
    
    # Step 2: Grouped data for display table
    df_grouped <- df %>%
      group_by(`Service Name`, Exercise) %>%
      summarise(
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 3: True summary row based on all data (not service averages)
    summary_row <- df %>%
      group_by(Exercise) %>%
      summarise(
        `Service Name` = "Summary",
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 4: Combine into final table
    df_final <- bind_rows(summary_row, df_grouped)
    
    # Step 5: Generate gt table
    df_final %>%
      gt(groupname_col = "Exercise", rowname_col = "Service Name") %>%
      text_transform(
        locations = cells_body(columns = c(`Average % Improvement`)),
        fn = function(x) {
          paste0(x, "%")
        }
      ) %>%
      tab_style(
        style = list(cell_text(color = "#008000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` > 0)
        )
      ) %>%
      tab_style(
        style = list(cell_text(color = "#FF0000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` < 0)
        )
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_style(
        style = cell_fill(color = "gray95"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_options(
        table.width = px(1260),
        row_group.as_column = TRUE,
        table.border.top.style = "hidden",
        column_labels.border.top.style = "hidden"
      ) %>%
      tab_stub_indent(
        rows = everything(),
        indent = 2
      ) %>%
      cols_width(
        Exercise ~ px(300),
        `Service Name` ~ px(300),
        `Overall Average` ~ px(220),
        `Average Improvement` ~ px(220),
        `Average % Improvement` ~ px(220)
      ) %>%
      sub_missing()
  }
  
  generate_armcare_gt <- function() {
    req(input$facility_date_range, input$facility_level, input$facility_service, input$facility_gender, input$facility_attendance)
    
    # Step 1: Main data with improvements
    df <- pitching_data %>%
      filter(
        Date >= input$facility_date_range[1],
        Date <= input$facility_date_range[2],
        Level %in% input$facility_level,
        `Service Name` %in% input$facility_service,
        Attendance %in% input$facility_attendance
      ) %>%
      pivot_longer(
        cols = c(total_strength, irtarm_strength, ertarm_strength, starm_strength, gtarm_strength),
        names_to  = "metric_col",
        values_to = "Value"
      ) %>%
      filter(!is.na(Value)) %>%
      group_by(`Service Name`, Name, metric_col) %>%
      arrange(Date) %>%
      filter(n() >= 2) %>%
      summarise(
        First      = first(Value, na_rm = TRUE),
        Last       = last(Value,  na_rm = TRUE),
        OverallAvg = mean(Value,  na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      mutate(
        Improvement = Last - First,
        Percent = case_when(
          First == 0 ~ NA_real_,
          TRUE ~ Improvement / First * 100
        ),
        Exercise = case_when(
          metric_col == "irtarm_strength" ~ "IR",
          metric_col == "ertarm_strength" ~ "ER",
          metric_col == "starm_strength"  ~ "Scaption",
          metric_col == "gtarm_strength"  ~ "Grip",
          metric_col == "total_strength"  ~ "Total Strength",
        )
      )
    
    # Step 2: Grouped data for display table
    df_grouped <- df %>%
      group_by(`Service Name`, Exercise) %>%
      summarise(
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 3: True summary row based on all data
    summary_row <- df %>%
      group_by(Exercise) %>%
      summarise(
        `Service Name` = "Summary",
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 4: Combine into final table
    df_final <- bind_rows(summary_row, df_grouped)
    
    # Step 5: Generate gt table
    df_final %>%
      gt(groupname_col = "Exercise", rowname_col = "Service Name") %>%
      text_transform(
        locations = cells_body(columns = c(`Average % Improvement`)),
        fn = function(x) {
          paste0(x, "%")
        }
      ) %>%
      tab_style(
        style = list(cell_text(color = "#008000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` > 0)
        )
      ) %>%
      tab_style(
        style = list(cell_text(color = "#FF0000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` < 0)
        )
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_style(
        style = cell_fill(color = "gray95"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_options(
        table.width = px(1260),
        row_group.as_column = TRUE,
        table.border.top.style = "hidden",
        column_labels.border.top.style = "hidden"
      ) %>%
      tab_stub_indent(
        rows = everything(),
        indent = 2
      ) %>%
      cols_width(
        Exercise ~ px(300),
        `Service Name` ~ px(300),
        `Overall Average` ~ px(220),
        `Average Improvement` ~ px(220),
        `Average % Improvement` ~ px(220)
      ) %>%
      sub_missing()
  }
  
  generate_forceplate_gt <- function() {
    req(input$facility_date_range, input$facility_level, input$facility_service, input$facility_gender, input$facility_attendance)
    
    # Step 1: Main data with improvements
    df <- strength_data %>%
      filter(
        Date >= input$facility_date_range[1],
        Date <= input$facility_date_range[2],
        Level %in% input$facility_level,
        `Service Name` %in% input$facility_service,
        Gender %in% input$facility_gender,
        Attendance %in% input$facility_attendance
      ) %>%
      pivot_longer(
        cols = c(IBSQT, CMJ, SHLDISOY),
        names_to  = "metric_col",
        values_to = "Value"
      ) %>%
      filter(!is.na(Value)) %>%
      group_by(`Service Name`, Name, metric_col) %>%
      arrange(Date) %>%
      filter(n() >= 2) %>%
      summarise(
        First      = first(Value, na_rm = TRUE),
        Last       = last(Value,  na_rm = TRUE),
        OverallAvg = mean(Value,  na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      mutate(
        Improvement = Last - First,
        Percent = case_when(
          First == 0 ~ NA_real_,
          TRUE ~ Improvement / First * 100
        ),
        Exercise = case_when(
          metric_col == "IBSQT"     ~ "IBSQT",
          metric_col == "CMJ"       ~ "CMJ",
          metric_col == "SHLDISOY"  ~ "SHLDISOY"
        )
      )
    
    # Step 2: Grouped data
    df_grouped <- df %>%
      group_by(`Service Name`, Exercise) %>%
      summarise(
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 3: Summary row based on all data
    summary_row <- df %>%
      group_by(Exercise) %>%
      summarise(
        `Service Name` = "Summary",
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 4: Combine final table
    df_final <- bind_rows(summary_row, df_grouped)
    
    # Step 5: Generate gt table
    df_final %>%
      gt(groupname_col = "Exercise", rowname_col = "Service Name") %>%
      text_transform(
        locations = cells_body(columns = c(`Average % Improvement`)),
        fn = function(x) {
          paste0(x, "%")
        }
      ) %>%
      tab_style(
        style = list(cell_text(color = "#008000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` > 0)
        )
      ) %>%
      tab_style(
        style = list(cell_text(color = "#FF0000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` < 0)
        )
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_style(
        style = cell_fill(color = "gray95"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_options(
        table.width = px(1260),
        row_group.as_column = TRUE,
        table.border.top.style = "hidden",
        column_labels.border.top.style = "hidden"
      ) %>%
      tab_stub_indent(
        rows = everything(),
        indent = 2
      ) %>%
      cols_width(
        Exercise ~ px(300),
        `Service Name` ~ px(300),
        `Overall Average` ~ px(220),
        `Average Improvement` ~ px(220),
        `Average % Improvement` ~ px(220)
      ) %>%
      sub_missing()
  }
  
  generate_dynamo_gt <- function() {
    req(input$facility_date_range, input$facility_level, input$facility_service, input$facility_gender, input$facility_attendance)
    
    # Step 1: Main data with improvements
    df <- strength_data %>%
      filter(
        Date >= input$facility_date_range[1],
        Date <= input$facility_date_range[2],
        Level %in% input$facility_level,
        `Service Name` %in% input$facility_service,
        Gender %in% input$facility_gender,
        Attendance %in% input$facility_attendance
      ) %>%
      pivot_longer(
        cols = c(`External Rotation`, `Internal Rotation`, `Dynamo - Trunk`),
        names_to  = "metric_col",
        values_to = "Value"
      ) %>%
      filter(!is.na(Value)) %>%
      group_by(`Service Name`, Name, metric_col) %>%
      arrange(Date) %>%
      filter(n() >= 2) %>%
      summarise(
        First      = first(Value, na_rm = TRUE),
        Last       = last(Value,  na_rm = TRUE),
        OverallAvg = mean(Value,  na.rm = TRUE),
        .groups    = "drop"
      ) %>% 
      mutate(
        Improvement = Last - First,
        Percent = case_when(
          First == 0 ~ NA_real_,
          TRUE ~ Improvement / First * 100
        ),
        Exercise = case_when(
          metric_col == "External Rotation"  ~ "External Rotation",
          metric_col == "Internal Rotation"  ~ "Internal Rotation",
          metric_col == "Dynamo - Trunk"     ~ "ISO Trunk Rotation"
        )
      )
    
    # Step 2: Grouped data for display table
    df_grouped <- df %>%
      group_by(`Service Name`, Exercise) %>%
      summarise(
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 3: True summary row based on all data
    summary_row <- df %>%
      group_by(Exercise) %>%
      summarise(
        `Service Name` = "Summary",
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 4: Combine into final table
    df_final <- bind_rows(summary_row, df_grouped)
    
    # Step 5: Generate gt table
    df_final %>%
      gt(groupname_col = "Exercise", rowname_col = "Service Name") %>%
      text_transform(
        locations = cells_body(columns = c(`Average % Improvement`)),
        fn = function(x) {
          paste0(x, "%")
        }
      ) %>%
      tab_style(
        style = list(cell_text(color = "#008000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` > 0)
        )
      ) %>%
      tab_style(
        style = list(cell_text(color = "#FF0000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` < 0)
        )
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_style(
        style = cell_fill(color = "gray95"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_options(
        table.width = px(1260),
        row_group.as_column = TRUE,
        table.border.top.style = "hidden",
        column_labels.border.top.style = "hidden"
      ) %>%
      tab_stub_indent(
        rows = everything(),
        indent = 2
      ) %>%
      cols_width(
        Exercise ~ px(300),
        `Service Name` ~ px(300),
        `Overall Average` ~ px(220),
        `Average Improvement` ~ px(220),
        `Average % Improvement` ~ px(220)
      ) %>%
      sub_missing()
  }
  
  generate_proteus_gt <- function() {
    req(input$facility_date_range, input$facility_level, input$facility_service, input$facility_gender, input$facility_attendance)
    
    # Step 1: Main data with improvements
    df <- strength_data %>%
      filter(
        Date >= input$facility_date_range[1],
        Date <= input$facility_date_range[2],
        Level %in% input$facility_level,
        `Service Name` %in% input$facility_service,
        Gender %in% input$facility_gender,
        Attendance %in% input$facility_attendance
      ) %>%
      pivot_longer(
        cols = c(TrunkRotation, ShotPut, D2Ext, D2Flex),
        names_to  = "metric_col",
        values_to = "Value"
      ) %>%
      filter(!is.na(Value)) %>%
      group_by(`Service Name`, Name, metric_col) %>%
      arrange(Date) %>%
      filter(n() >= 2) %>%
      summarise(
        First      = first(Value, na_rm = TRUE),
        Last       = last(Value,  na_rm = TRUE),
        OverallAvg = mean(Value,  na.rm = TRUE),
        .groups    = "drop"
      ) %>% 
      mutate(
        Improvement = Last - First,
        Percent = case_when(
          First == 0 ~ NA_real_,
          TRUE ~ Improvement / First * 100
        ),
        Exercise = case_when(
          metric_col == "TrunkRotation" ~ "Trunk Rotation",
          metric_col == "ShotPut"       ~ "Shot Put",
          metric_col == "D2Flex"        ~ "D2 Flexion",
          metric_col == "D2Ext"         ~ "D2 Extension"
        )
      )
    
    # Step 2: Grouped data for display table
    df_grouped <- df %>%
      group_by(`Service Name`, Exercise) %>%
      summarise(
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 3: True summary row based on all data
    summary_row <- df %>%
      group_by(Exercise) %>%
      summarise(
        `Service Name` = "Summary",
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 1),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 1),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 4: Combine into final table
    df_final <- bind_rows(summary_row, df_grouped)
    
    # Step 5: Generate gt table
    df_final %>%
      gt(groupname_col = "Exercise", rowname_col = "Service Name") %>%
      text_transform(
        locations = cells_body(columns = c(`Average % Improvement`)),
        fn = function(x) {
          paste0(x, "%")
        }
      ) %>%
      tab_style(
        style = list(cell_text(color = "#008000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` > 0)
        )
      ) %>%
      tab_style(
        style = list(cell_text(color = "#FF0000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` < 0)
        )
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_style(
        style = cell_fill(color = "gray95"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_options(
        table.width = px(1260),
        row_group.as_column = TRUE,
        table.border.top.style = "hidden",
        column_labels.border.top.style = "hidden"
      ) %>%
      tab_stub_indent(
        rows = everything(),
        indent = 2
      ) %>%
      cols_width(
        Exercise ~ px(300),
        `Service Name` ~ px(300),
        `Overall Average` ~ px(220),
        `Average Improvement` ~ px(220),
        `Average % Improvement` ~ px(220)
      ) %>%
      sub_missing()
  }
  
  generate_smartspeed_gt <- function() {
    req(input$facility_date_range, input$facility_level, input$facility_service, input$facility_gender, input$facility_attendance)
    
    # Step 1: Main data with improvements
    df <- speed_data %>%
      filter(
        Test == "Futures Sprint 40y: 10y, 20y, 30y, 40y",
        Date >= input$facility_date_range[1],
        Date <= input$facility_date_range[2],
        Level %in% input$facility_level,
        `Service Name` %in% input$facility_service,
        Gender %in% input$facility_gender,
        Attendance %in% input$facility_attendance
      ) %>%
      pivot_longer(
        cols = c(early_acceleration, late_acceleration, thirty_yard, forty_yard, max_velocity),
        names_to  = "metric_col",
        values_to = "Value"
      ) %>%
      filter(!is.na(Value)) %>%
      group_by(`Service Name`, Name, metric_col) %>%
      arrange(Date) %>%
      filter(n() >= 6) %>%
      summarise(
        First      = mean(head(Value, 3), na.rm = TRUE),
        Last       = mean(tail(Value, 3), na.rm = TRUE),
        OverallAvg = mean(Value,          na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      mutate(
        Improvement = case_when(
          metric_col %in% c("early_acceleration", "late_acceleration", "thirty_yard", "forty_yard") ~ First - Last,  # lower is better
          metric_col == "max_velocity" ~ Last - First  # higher is better
        ),
        Percent = case_when(
          First == 0 ~ NA_real_,
          TRUE ~ Improvement / First * 100
        ),
        Exercise = case_when(
          metric_col == "early_acceleration"  ~ "Early Acceleration",
          metric_col == "late_acceleration"   ~ "Late Acceleration",
          metric_col == "thirty_yard"         ~ "30 Yard",
          metric_col == "forty_yard"          ~ "40 Yard",
          metric_col == "max_velocity"        ~ "Top Speed"
        )
      )
    
    # Step 2: Grouped data for display table
    df_grouped <- df %>%
      group_by(`Service Name`, Exercise) %>%
      summarise(
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 2),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 2),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 3: True summary row based on all data
    summary_row <- df %>%
      group_by(Exercise) %>%
      summarise(
        `Service Name` = "Summary",
        `Overall Average`       = round(mean(OverallAvg,       na.rm = TRUE), 2),
        `Average Improvement`   = round(mean(Improvement,      na.rm = TRUE), 2),
        `Average % Improvement` = round(mean(Percent,          na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Step 4: Combine into final table
    df_final <- bind_rows(summary_row, df_grouped)
    
    # Step 5: Generate gt table
    df_final %>%
      gt(groupname_col = "Exercise", rowname_col = "Service Name") %>%
      text_transform(
        locations = cells_body(columns = c(`Average % Improvement`)),
        fn = function(x) {
          paste0(x, "%")
        }
      ) %>%
      tab_style(
        style = list(cell_text(color = "#008000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` > 0)
        )
      ) %>%
      tab_style(
        style = list(cell_text(color = "#FF0000")),
        locations = cells_body(
          columns = c(`Average Improvement`, `Average % Improvement`),
          rows = (`Average Improvement` < 0)
        )
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_style(
        style = cell_fill(color = "gray95"),
        locations = cells_body(rows = `Service Name` == "Summary")
      ) %>%
      tab_options(
        table.width = px(1260),
        row_group.as_column = TRUE,
        table.border.top.style = "hidden",
        column_labels.border.top.style = "hidden"
      ) %>%
      tab_stub_indent(
        rows = everything(),
        indent = 2
      ) %>%
      cols_width(
        Exercise ~ px(300),
        `Service Name` ~ px(300),
        `Overall Average` ~ px(220),
        `Average Improvement` ~ px(220),
        `Average % Improvement` ~ px(220)
      ) %>%
      sub_missing()
  }
  
  output$facility_progression_table <- render_gt({
    req(input$facility_metric)
    
    switch(input$facility_metric,
           "HitTrax"   = generate_hittrax_gt(),
           "Blast"     = generate_blast_gt(),
           "Trackman"  = generate_trackman_gt(),
           "Armcare"   = generate_armcare_gt(),
           "ForceDecks"= generate_forceplate_gt(),
           "Dynamo"    = generate_dynamo_gt(),
           "Proteus"   = generate_proteus_gt(),
           "VALD"      = generate_smartspeed_gt()
    )
  })
  
  
  # ── Dynamic athlete dropdowns ─────────────────────────────────────
  observeEvent(input$date_range_hitting, {
    req(input$date_range_hitting)
    
    choices <- hitting_data %>%
      filter(Date >= input$date_range_hitting[1],
             Date <= input$date_range_hitting[2]) %>%
      distinct(Name) %>%
      arrange(Name) %>%
      pull(Name)
    
    current <- isolate(input$selected_athlete_hitting)
    selected <- if (current %in% choices) current else ""
    
    updateSelectInput(session, "selected_athlete_hitting",
                      choices = c("Select an Athlete" = "", choices),
                      selected = selected)
  })
  
  observeEvent(input$date_range_pitching, {
    req(input$date_range_pitching)
    
    choices <- pitching_data %>%
      filter(Date >= input$date_range_pitching[1],
             Date <= input$date_range_pitching[2]) %>%
      distinct(Name) %>%
      arrange(Name) %>%
      pull(Name)
    
    current <- isolate(input$selected_athlete_pitching)
    selected <- if (current %in% choices) current else ""
    
    updateSelectInput(session, "selected_athlete_pitching",
                      choices = c("Select an Athlete" = "", choices),
                      selected = selected)
  })
  
  observeEvent(input$date_range_strength, {
    req(input$date_range_strength)
    
    choices <- strength_data %>%
      filter(Date >= input$date_range_strength[1],
             Date <= input$date_range_strength[2]) %>%
      distinct(Name) %>%
      arrange(Name) %>%
      pull(Name)
    
    current <- isolate(input$selected_athlete_strength)
    selected <- if (current %in% choices) current else ""
    
    updateSelectInput(session, "selected_athlete_strength",
                      choices = c("Select an Athlete" = "", choices),
                      selected = selected)
  })
  
  observeEvent(input$date_range_speed, {
    req(input$date_range_speed)
    
    choices <- speed_data %>%
      filter(Date >= input$date_range_speed[1],
             Date <= input$date_range_speed[2]) %>%
      distinct(Name) %>%
      arrange(Name) %>%
      pull(Name)
    
    current <- isolate(input$selected_athlete_speed)
    selected <- if (current %in% choices) current else ""
    
    updateSelectInput(session, "selected_athlete_speed",
                      choices = c("Select an Athlete" = "", choices),
                      selected = selected)
  })
  
  # ── function to build profile UI ───────────────────────────
  generate_profile_ui <- function(athlete) {
    req(athlete)
    info <- client_data %>% filter(Name == athlete)
    if (nrow(info) == 0) return(NULL)
    tagList(
      tags$img(
        src   = "Generic-Profile-Image.png",
        style = "width:150px;height:150px;border-radius:50%;object-fit:cover;margin:20px 0;"
      ),
      div(style = "text-align:center;",
          tags$h4(info$Name),
          tags$p(paste0(info$`High School`, " | Class of ", info$`Graduating Class`)),
          tags$p(paste0(
            info$`Position (Baseball/Softball)`, " | ",
            info$Height, " ", info$Weight, "lbs | Age: ", info$Age
          ))
      )
    )
  }
  
  selected_athlete_hitting_event <- eventReactive(input$get_profile_hitting, {
    input$selected_athlete_hitting
  })
  selected_athlete_pitching_event <- eventReactive(input$get_profile_pitching, {
    input$selected_athlete_pitching
  })
  
  selected_athlete_strength_event <- eventReactive(input$get_profile_strength, {
    input$selected_athlete_strength
  })
  
  selected_athlete_speed_event <- eventReactive(input$get_profile_speed, {
    input$selected_athlete_speed
  })
  
  # ── Render profile UIs ────────────────────────────────────────────
  output$athlete_profile_hitting  <- renderUI({ req(input$get_profile_hitting);  isolate(generate_profile_ui(input$selected_athlete_hitting)) })
  output$athlete_profile_pitching <- renderUI({ req(input$get_profile_pitching); isolate(generate_profile_ui(input$selected_athlete_pitching)) })
  output$athlete_profile_strength <- renderUI({ req(input$get_profile_strength); isolate(generate_profile_ui(input$selected_athlete_strength)) })
  output$athlete_profile_speed    <- renderUI({ req(input$get_profile_speed);    isolate(generate_profile_ui(input$selected_athlete_speed)) })
  
  # ── HITTING: data + plots ─────────────────────────────────────────
  
  hitting_plot_data <- eventReactive(input$get_profile_hitting, {
    req(input$selected_athlete_hitting, input$date_range_hitting)
    athlete    <- input$selected_athlete_hitting
    date_range <- input$date_range_hitting
    
    # 1. athlete summary ON the date‐range
    athlete_summary <- hitting_data %>%
      filter(Name == athlete,
             Date >= date_range[1],
             Date <= date_range[2]) %>%
      summarise(
        Name                   = first(Name),
        Level                  = first(Level),
        Gender                 = first(Gender),
        
        MaxVel                 = if(all(is.na(MaxVel))) NA_real_ else max(MaxVel, na.rm = TRUE),
        AvgVel                 = if(all(is.na(AvgVel))) NA_real_ else mean(AvgVel, na.rm = TRUE),
        MaxDist                = if(all(is.na(MaxDist))) NA_real_ else max(MaxDist, na.rm = TRUE),
        bat_speed              = if(all(is.na(bat_speed))) NA_real_ else mean(bat_speed, na.rm = TRUE),
        rotational_acceleration = if(all(is.na(rotational_acceleration))) NA_real_ else mean(rotational_acceleration, na.rm = TRUE),
        .groups = "drop"
      )
    
    # 2. facility summary OVER ALL DATES (or whatever span)
    facility_summary <- hitting_data %>%
      filter(Level  == athlete_summary$Level,
             Gender == athlete_summary$Gender) %>%
      group_by(Name, Level, Gender) %>%
      summarise(
        MaxVel                 = if(all(is.na(MaxVel))) NA_real_ else max(MaxVel, na.rm = TRUE),
        AvgVel                 = if(all(is.na(AvgVel))) NA_real_ else mean(AvgVel, na.rm = TRUE),
        MaxDist                = if(all(is.na(MaxDist))) NA_real_ else max(MaxDist, na.rm = TRUE),
        bat_speed              = if(all(is.na(bat_speed))) NA_real_ else mean(bat_speed, na.rm = TRUE),
        rotational_acceleration = if(all(is.na(rotational_acceleration))) NA_real_ else mean(rotational_acceleration, na.rm = TRUE),
        .groups = "drop"
      )
    
    facility_override <- facility_summary %>%
      filter(Name != athlete) %>%
      bind_rows(athlete_summary)
    
    facility_with_pct <- facility_override %>%
      mutate(
        MaxVel_Percentile = case_when(
          Level == "Collegiate"   ~ round(pnorm((MaxVel - 103.8)   /  7.06)     * 100),
          Level == "Professional" ~ round(pnorm((MaxVel - 108.9622)/  4.008935) * 100),
          TRUE                    ~ round(percent_rank(MaxVel)      * 100)
        ),
        AvgVel_Percentile = case_when(
          Level == "Collegiate"   ~ round(pnorm((AvgVel - 82.2)     /  6.01)     * 100),
          Level == "Professional" ~ round(pnorm((AvgVel - 87.41388)/  3.481693) * 100),
          TRUE                    ~ round(percent_rank(AvgVel)      * 100)
        ),
        MaxDist_Percentile = case_when(
          Level == "Collegiate"   ~ round(pnorm((MaxDist - 367)    / 58.39)     * 100),
          Level == "Professional" ~ round(pnorm((MaxDist - 411.4609)/ 35.09507) * 100),
          TRUE                    ~ round(percent_rank(MaxDist)     * 100)
        ),
        BatSpeed_Percentile = case_when(
          Level == "Collegiate"   ~ round(pnorm((bat_speed - 69.11) /  3.37)     * 100),
          Level == "Professional" ~ round(pnorm((bat_speed - 71.645)/  3.318)    * 100),
          TRUE                    ~ round(percent_rank(bat_speed) * 100)
        ),
        RotationalAcceleration_Percentile = case_when(
          Level == "Collegiate"   ~ round(pnorm((rotational_acceleration - 13.67) /  2.98)   * 100),
          Level == "Professional" ~ round(pnorm((rotational_acceleration - 14.874)/  2.684)  * 100),
          TRUE                    ~ round(percent_rank(rotational_acceleration)       * 100)
        )
      )
    
    athlete_percentiles <- facility_with_pct %>%
      filter(Name == athlete) %>%
      select(ends_with("_Percentile")) %>% 
      pivot_longer(
        cols      = everything(),
        names_to  = "Metric",
        values_to = "Percentile"
      ) %>%
      mutate(
        Metric = case_when(
          Metric == "MaxVel_Percentile"                 ~ "Max EV",
          Metric == "AvgVel_Percentile"                 ~ "Avg EV",
          Metric == "MaxDist_Percentile"                ~ "Max Dist",
          Metric == "BatSpeed_Percentile"               ~ "Bat Speed",
          Metric == "RotationalAcceleration_Percentile" ~ "Rot Accel",
          TRUE                                          ~ Metric
        ),
        Metric = factor(Metric, levels = c("Rot Accel","Bat Speed","Max Dist","Avg EV","Max EV"))
      )
  })
  
  output$hitting_percentile_plot <- renderPlot({
    req(input$get_profile_hitting)
    df <- hitting_plot_data()
    
    ggplot(df, aes(x = Percentile, y = Metric, color = Percentile)) +
      geom_segment(aes(x = 0,    xend = 100, y = Metric, yend = Metric), color = "#B9CED0", linewidth = 2) +
      geom_segment(aes(x = -2.5, xend = Percentile, y = Metric, yend = Metric, color = Percentile), linewidth = 10) +
      geom_segment(aes(x = 4.5,  xend = 5.5, y = Metric, yend = Metric), color = "white", linewidth = 10, alpha = 0.25) +
      geom_segment(aes(x = 49.5, xend = 50.5, y = Metric, yend = Metric), color = "white", linewidth = 10, alpha = 0.25) +
      geom_segment(aes(x = 94.5, xend = 95.5, y = Metric, yend = Metric), color = "white", linewidth = 10, alpha = 0.25) +
      geom_point(aes(fill = Percentile), shape = 21, color = "white", size = 12, stroke = 2) +
      geom_line(color = "#010101", linewidth = 8) +
      geom_text(aes(label = Percentile), color = "white", size = 5, fontface = "bold", hjust = 0.5, vjust = 0.5) +
      scale_x_continuous(limits = c(-2.5, 100)) +
      scale_color_gradientn(colors = c("#2166ac", "#B9CED0", "#b2182b"), limits = c(0, 100), na.value = "#2166ac") +
      scale_fill_gradientn(colors = c("#2166ac", "#B9CED0", "#b2182b"), limits = c(0, 100), na.value = "#2166ac") +
      theme_void() +
      theme(
        legend.position   = "none",
        axis.text.y       = element_text(size = 10, face = "bold", color = "grey20", hjust = 1),
        panel.background  = element_blank()
      )
  })
  
  output$futures_score_hitting <- renderUI({
    req(input$get_profile_hitting)
    df <- hitting_plot_data()
    avg_pct <- mean(df$Percentile, na.rm = TRUE)
    score   <- avg_pct / 10
    if (is.na(score)) return(NULL)
    
    div(
      style = "display: flex; gap: 20px; justify-content: center; align-items: center;",
      tags$img(src = "black_logo.png", style = "height: 50px; width: auto;"),
      div(round(score, 1), style = "font-size: 48px; font-weight: bold;")
    )
  })
  
  # Trend-plot data + UI
  trend_plot_data <- eventReactive(input$get_profile_hitting, {
    req(input$selected_athlete_hitting, input$date_range_hitting)
    hitting_data %>%
      filter(
        Name == input$selected_athlete_hitting,
        Date >= input$date_range_hitting[1],
        Date <= input$date_range_hitting[2]
      ) %>%
      select(Date, Name, MaxVel, AvgVel, MaxDist, AvgDist, bat_speed, rotational_acceleration)
  })
  
  observeEvent(trend_plot_data(), {
    updatePickerInput(session, "trend_metric",
                      choices = list(
                        "HitTrax" = list(
                          "Max EV"   = "MaxVel",
                          "Avg EV"   = "AvgVel",
                          "Max Dist" = "MaxDist",
                          "Avg Dist" = "AvgDist"
                        ),
                        "Blast" = list(
                          "Bat Speed" = "bat_speed",
                          "Rotational Acceleration" = "rotational_acceleration"
                        )
                      ),
                      selected = "MaxVel"
    )
  })
  
  output$hitting_trend_plot <- renderPlotly({
    req(input$get_profile_hitting, input$trend_metric, input$trend_grouping)
    pd <- trend_plot_data() %>%
      filter(!is.na(.data[[input$trend_metric]])) %>%
      mutate(
        GroupDate = if (input$trend_grouping == "month")
          as.Date(format(Date, "%Y-%m-01"))
        else Date
      ) %>%
      group_by(GroupDate) %>%
      summarise(
        Value = if (input$trend_metric %in% c("MaxVel", "MaxDist")) {
          max(.data[[input$trend_metric]], na.rm = TRUE)
        } else {
          mean(.data[[input$trend_metric]], na.rm = TRUE)
        },
        .groups = "drop"
      ) %>%
      arrange(GroupDate)
    
    if (nrow(pd) == 0) {
      return(
        plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(x = 0.5, y = 0.5, text = "No data to display",
                   showarrow = FALSE, font = list(size = 18))
            )
          )
      )
    }
    
    # calculate first & last values
    first_val  <- pd$Value[1]
    last_val   <- pd$Value[nrow(pd)]
    num_change <- round(last_val - first_val, 1)
    pct_change <- round((num_change / first_val) * 100, 1)
    
    # decide on color based on sign
    subtitle_color <- if (num_change >= 0) "#008000" else "#FF0000"
    
    # build the HTML‐styled subtitle
    subtitle_txt <- paste0(
      "<sup style='font-size:14px;color:", subtitle_color, ";'>",
      if (num_change >= 0) "+" else "", num_change,
      " (", if (pct_change >= 0) "+" else "", pct_change, "%)",
      "</sup>"
    )
    
    xaxis_args <- list(
      title      = "",
      tickformat = if (input$trend_grouping == "month") "%b %Y" else "%b %d",
      tickangle  = -45
    )
    if (input$trend_grouping == "month") {
      xaxis_args$tickmode <- "array"
      xaxis_args$tickvals <- unique(pd$GroupDate)
    }
    
    plot_ly(
      data = pd,
      x    = ~GroupDate,
      y    = ~Value,
      type = 'scatter',
      mode = 'lines+markers',
      line   = list(color = '#2D89C8', width = 3),
      marker = list(size = 8, color = '#2D89C8')
    ) %>%
      layout(
        margin     = list(t = 100, b = 50, l = 50, r = 50),
        title      = list(
          text = paste0(
            selected_athlete_hitting_event(), " ",
            input$trend_metric, " by ",
            if (input$trend_grouping == "month") "Month" else "Date",
            "<br>",
            subtitle_txt 
          ),
          x = 0.5
        ),
        font       = list(size = 20),
        xaxis      = xaxis_args,
        yaxis      = list(title = ""),
        hovermode  = 'closest',
        dragmode   = FALSE
      ) %>%
      config(
        displaylogo           = FALSE,
        scrollZoom            = FALSE,
        doubleClick           = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d","pan2d","select2d","lasso2d",
          "zoomIn2d","zoomOut2d","autoScale2d","resetScale2d",
          "hoverClosestCartesian","hoverCompareCartesian"
        ),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0(selected_athlete_hitting_event(), "_TrendPlot"),
          height   = 600,
          width    = 900,
          scale    = 2
        )
      )
  })
  
  # build the summary only once you click “Get Profile”
  hittrax_summary_data <- eventReactive(input$get_profile_hitting, {
    req(input$selected_athlete_hitting, input$date_range_hitting)
    summary_hittrax <- hittrax_data %>%
      filter(
        Name == input$selected_athlete_hitting,
        Date >= input$date_range_hitting[1],
        Date <= input$date_range_hitting[2]
      ) %>%
      summarise(
        `Max EV`   = round(max(ExitVelocity, na.rm = TRUE), 1),
        `Avg EV`   = round(mean(ExitVelocity, na.rm = TRUE), 1),
        `Max Dist` = round(max(Distance, na.rm = TRUE)),
        `Avg Dist` = round(mean(Distance, na.rm = TRUE))
      )
    
    summary_hitting <- hitting_data %>%
      filter(
        Name == input$selected_athlete_hitting,
        Date >= input$date_range_hitting[1],
        Date <= input$date_range_hitting[2]
      ) %>%
      summarise(
        `LD %`       = round(sum(LDC, na.rm = TRUE) / sum(HC, na.rm = TRUE) * 100, 1),
        `FB %`       = round(sum(FBC, na.rm = TRUE) / sum(HC, na.rm = TRUE) * 100, 1),
        `GB %`       = round(sum(GBC, na.rm = TRUE) / sum(HC, na.rm = TRUE) * 100, 1),
        `Hard Hit %` = round(sum(HHC, na.rm = TRUE) / sum(HC, na.rm = TRUE) * 100, 1),
        `Barrel %`   = round(sum(Barrels, na.rm = TRUE) / sum(HC, na.rm = TRUE) * 100, 1),
      )
    
    combined_summary <- bind_cols(summary_hittrax, summary_hitting)
    
  })
  
  output$hittrax_summary_table <- render_gt({
    df <- hittrax_summary_data()
    req(nrow(df) > 0)
    
    df %>%
      gt() %>%
      tab_header(
        title = html(
          '<div style="display: flex; align-items: center; text-align: center; width: 100%;">
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
          <span style="padding: 0 50px; font-weight: 400; font-size: 24px; color: #5a5a5a;">HitTrax Metrics</span>
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
        </div>'
        )
      ) %>%
      tab_options(
        table.width = px(1260),
        heading.align = "center",
        heading.background.color = "white",
        heading.title.font.size = "small",
        table.border.top.style   = "hidden"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#2D89C8"),
          cell_text(color = "white", weight = "bold", align = "center")
        ),
        locations = cells_column_labels(everything())
      ) %>% 
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(everything())
      ) %>%
      cols_width(
        everything() ~ px(140)
      ) %>% 
      sub_missing(
        columns = everything()
      )
  })
  
  blast_summary_data <- eventReactive(input$get_profile_hitting, {
    req(input$selected_athlete_hitting, input$date_range_hitting)
    hitting_data %>%
      filter(
        Name == input$selected_athlete_hitting,
        Date >= input$date_range_hitting[1],
        Date <= input$date_range_hitting[2]
      ) %>%
      summarise(
        `Bat Speed`   = round(mean(bat_speed, na.rm = TRUE), 1),
        `Rot Accel` = round(mean(rotational_acceleration, na.rm = TRUE), 1),
        OPE      = round(mean(on_plane_efficiency, na.rm = TRUE)),
        EC       = round(mean(early_connection, na.rm = TRUE)),
        CxI    = round(mean(connection_at_impact, na.rm = TRUE)),
        VBA   = round(mean(vertical_bat_angle, na.rm = TRUE)),
        Power = round(mean(power,    na.rm = TRUE), 2),
        TTC    = round(mean(time_to_contact, na.rm = TRUE), 2),
      )
  })
  
  output$blast_summary_table <- render_gt({
    df <- blast_summary_data()
    req(nrow(df) > 0)
    
    df %>%
      gt() %>%
      tab_header(
        title = html(
          '<div style="display: flex; align-items: center; text-align: center; width: 100%;">
        <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
        <span style="padding: 0 50px; font-weight: 400; font-size: 24px; color: #5a5a5a;">Blast Metrics</span>
        <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
      </div>'
        )
      ) %>%
      text_transform(
        locations = cells_body(columns = c(`Bat Speed`)),
        fn = function(x) {
          paste0(x, " mph")
        }
      ) %>%
      text_transform(
        locations = cells_body(columns = c(`Rot Accel`)),
        fn = function(x) {
          paste0(x, " g")
        }
      ) %>%
      text_transform(
        locations = cells_body(columns = c(Power)),
        fn = function(x) {
          paste0(x, " kW")
        }
      ) %>%
      text_transform(
        locations = cells_body(columns = c(OPE)),
        fn = function(x) {
          paste0(x, "%")
        }
      ) %>%
      text_transform(
        locations = cells_body(columns = c(EC, CxI, VBA)),
        fn = function(x) {
          paste0(x, "°")
        }
      ) %>%
      text_transform(
        locations = cells_body(columns = c(TTC)),
        fn = function(x) {
          paste0(x, " sec")
        }
      ) %>%
      tab_options(
        table.width = px(1260),
        heading.align = "center",
        heading.background.color = "white",
        heading.title.font.size = "small",
        table.border.top.style   = "hidden"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#2D89C8"),
          cell_text(color = "white", weight = "bold", align = "center")
        ),
        locations = cells_column_labels(everything())
      ) %>% 
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(everything())
      ) %>%
      cols_width(
        everything() ~ px(157.5)
      ) %>% 
      sub_missing(
        columns = everything()
      )
  })
  
  standard_summary_data <- eventReactive(input$get_profile_hitting, {
    req(input$selected_athlete_hitting, input$date_range_hitting)
    hittrax_data %>%
      filter(
        Name == input$selected_athlete_hitting,
        Date >= input$date_range_hitting[1],
        Date <= input$date_range_hitting[2]
      ) %>%
      summarise(
        AB      = sum(Outcome %in% c("Out","Single","Double","Triple","Home Run"), na.rm = TRUE),
        H       = sum(Outcome %in% c("Single","Double","Triple","Home Run"),        na.rm = TRUE),
        `1B`    = sum(Outcome == "Single", na.rm = TRUE),
        `2B`    = sum(Outcome == "Double", na.rm = TRUE),
        `3B`    = sum(Outcome == "Triple", na.rm = TRUE),
        HR      = sum(Outcome == "Home Run", na.rm = TRUE)
      ) %>%
      mutate(
        AVG = round(H / AB, 3),
        SLG = round((`1B` + 2*`2B` + 3*`3B` + 4*HR) / AB, 3)
      )
  })
  
  output$standard_summary_table <- render_gt({
    df <- standard_summary_data()
    req(nrow(df) > 0)
    
    df %>%
      gt() %>%
      tab_header(
        title = html(
          '<div style="display: flex; align-items: center; text-align: center; width: 100%;">
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
          <span style="padding: 0 50px; font-weight: 400; font-size: 24px; color: #5a5a5a;">Standard Stats</span>
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
        </div>'
        )
      ) %>%
      tab_options(
        table.width = px(1260),
        heading.align = "center",
        heading.background.color = "white",
        heading.title.font.size = "small",
        table.border.top.style   = "hidden"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#2D89C8"),
          cell_text(color = "white", weight = "bold", align = "center")
        ),
        locations = cells_column_labels(everything())
      ) %>% 
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(everything())
      ) %>%
      cols_width(
        everything() ~ px(157.5)
      ) %>% 
      sub_missing(
        columns = everything()
      )
  })
  
  # inside server()
  selected_hitting_table <- reactiveVal("hittrax")
  
  observeEvent(input$get_profile_hitting, {
    selected_hitting_table("hittrax")
  })
  
  observeEvent(input$show_hittrax_table, {
    selected_hitting_table("hittrax")
  })
  observeEvent(input$show_blast_table, {
    selected_hitting_table("blast")
  })
  
  output$hitting_summary_tables <- renderUI({
    req(input$get_profile_hitting, selected_hitting_table())
    
    switch(selected_hitting_table(),
           hittrax = layout_columns(
             div(style = "margin: 0 auto 30px auto;",
                 gt_output("hittrax_summary_table")
             )
           ),
           blast   = layout_columns(
             div(style = "margin: 0 auto 30px auto;",
                 gt_output("blast_summary_table")
             )
           )
    )
  })
  
  output$early_connection_value <- renderText({
    req(blast_summary_data())
    paste0(blast_summary_data()$EC, "°")
  })
  
  output$ope_value <- renderText({
    req(blast_summary_data())
    paste0(blast_summary_data()$OPE, "%")
  })
  
  output$connection_impact_value <- renderText({
    req(blast_summary_data())
    paste0(blast_summary_data()$CxI, "°")
  })
  
  output$early_connection_img <- renderUI({
    req(blast_summary_data())
    value <- blast_summary_data()$EC
    
    img_src <- if (is.na(value)) {
      "EC_blank.png"
    } else if (value < 80) {
      "EC_BLW_90.png"
    } else if (value > 105) {
      "EC_ABV_90.png"
    } else {
      "EC_at_90.png"
    }
    
    tags$img(src = img_src, style = "width: 300px; height: auto; margin-top: 30px;")
  })
  
  output$ope_img <- renderUI({
    req(blast_summary_data())
    value <- blast_summary_data()$OPE
    
    img_src <- if (is.na(value)) {
      "OPE_blank.png"
    } else {
      updated_value <- round(value / 5) * 5
      paste0("OPE", updated_value, ".png")
    }
    
    tags$img(src = img_src, style = "width: 300px; height: auto; margin-top: 30px;")
  })
  
  output$connection_impact_img <- renderUI({
    req(blast_summary_data())
    value <- blast_summary_data()$CxI
    
    img_src <- if (is.na(value)) {
      "CI_blank.png"
    } else if (value < 80) {
      "CI_BLW_90.png"
    } else if (value > 95) {
      "CI_ABV_90.png"
    } else {
      "CI_at_90.png"
    }
    
    tags$img(src = img_src, style = "width: 300px; height: auto; margin-top: 30px;")
  })
  
  # ── PITCHING: data + plots ──────────────────────────────────────
  
  pitching_plot_data <- eventReactive(input$get_profile_pitching, {
    req(input$selected_athlete_pitching, input$date_range_pitching)
    athlete    <- input$selected_athlete_pitching
    date_range <- input$date_range_pitching
    
    # 1. athlete summary ON the date‐range
    athlete_summary <- pitching_data %>%
      filter(Name == athlete,
             Date >= date_range[1],
             Date <= date_range[2]) %>%
      arrange(desc(Date)) %>% 
      summarise(
        Name    = first(Name),
        Level   = first(Level),
        
        MaxRelSpeed     = if (all(is.na(Max_RelSpeed)))    NA_real_ else max(Max_RelSpeed,   na.rm = TRUE),
        AvgRelSpeed     = if (all(is.na(Avg_RelSpeed)))    NA_real_ else mean(Avg_RelSpeed,  na.rm = TRUE),
        Extension       = if (all(is.na(Extension)))       NA_real_ else mean(Extension,    na.rm = TRUE),
        irtarm_strength = if (all(is.na(irtarm_strength))) NA_real_ else mean(irtarm_strength, na.rm = TRUE),
        ertarm_strength = if (all(is.na(ertarm_strength))) NA_real_ else mean(ertarm_strength, na.rm = TRUE),
        starm_strength  = if (all(is.na(starm_strength)))  NA_real_ else mean(starm_strength,  na.rm = TRUE),
        gtarm_strength  = if (all(is.na(gtarm_strength)))  NA_real_ else mean(gtarm_strength,  na.rm = TRUE),
        
        .groups = "drop"
      )
    
    # 2. facility summary OVER ALL DATES (same Level & Gender)
    facility_summary <- pitching_data %>%
      filter(Level  == athlete_summary$Level) %>%
      group_by(Name) %>%
      summarise(
        Level            = first(Level),
        
        MaxRelSpeed     = if (all(is.na(Max_RelSpeed)))    NA_real_ else max(Max_RelSpeed,   na.rm = TRUE),
        AvgRelSpeed     = if (all(is.na(Avg_RelSpeed)))    NA_real_ else mean(Avg_RelSpeed,  na.rm = TRUE),
        Extension       = if (all(is.na(Extension)))       NA_real_ else mean(Extension,    na.rm = TRUE),
        irtarm_strength = if (all(is.na(irtarm_strength))) NA_real_ else mean(irtarm_strength, na.rm = TRUE),
        ertarm_strength = if (all(is.na(ertarm_strength))) NA_real_ else mean(ertarm_strength, na.rm = TRUE),
        starm_strength  = if (all(is.na(starm_strength)))  NA_real_ else mean(starm_strength,  na.rm = TRUE),
        gtarm_strength  = if (all(is.na(gtarm_strength)))  NA_real_ else mean(gtarm_strength,  na.rm = TRUE),
        
        .groups = "drop"
      )
    
    # 3. drop the “old” athlete row & 4. bind in your date‐window athlete
    facility_override <- facility_summary %>%
      filter(Name != athlete) %>%
      bind_rows(athlete_summary)
    
    # 5. compute percentiles (pnorm for Coll/Pro, percent_rank otherwise)
    facility_with_pct <- facility_override %>%
      mutate(
        MaxRelSpeed_Percentile = case_when(
          Level == "Collegiate"   ~ round(pnorm((MaxRelSpeed   - 90.0)      /  4.15)     * 100),
          Level == "Professional" ~ round(pnorm((MaxRelSpeed   - 96.35804)  /  2.724521) * 100),
          TRUE                     ~ round(percent_rank(MaxRelSpeed)        * 100)
        ),
        AvgRelSpeed_Percentile = case_when(
          Level == "Collegiate"   ~ round(pnorm((AvgRelSpeed   - 87.4)      /  3.88)     * 100),
          Level == "Professional" ~ round(pnorm((AvgRelSpeed   - 93.95869)  /  2.45731)  * 100),
          TRUE                     ~ round(percent_rank(AvgRelSpeed)        * 100)
        ),
        Extension_Percentile = case_when(
          Level == "Collegiate"   ~ round(pnorm((Extension     - 5.88)      /  0.510)    * 100),
          Level == "Professional" ~ round(pnorm((Extension     - 6.437386)  /  0.4251436)* 100),
          TRUE                     ~ round(percent_rank(Extension)          * 100)
        ),
        irtarm_Percentile  = round(percent_rank(irtarm_strength) * 100),
        ertarm_Percentile  = round(percent_rank(ertarm_strength) * 100),
        starm_Percentile   = round(percent_rank(starm_strength)  * 100),
        gtarm_Percentile   = round(percent_rank(gtarm_strength)  * 100)
      )
    
    # 6. pivot athlete percentiles into two columns
    athlete_percentiles <- facility_with_pct %>%
      filter(Name == athlete) %>%
      select(ends_with("_Percentile")) %>%
      pivot_longer(
        cols      = everything(),
        names_to  = "Metric",
        values_to = "Percentile"
      ) %>%
      mutate(
        Metric = case_when(
          Metric == "MaxRelSpeed_Percentile"       ~ "Max FB",
          Metric == "AvgRelSpeed_Percentile"       ~ "Avg FB",
          Metric == "Extension_Percentile"         ~ "Extension",
          Metric == "irtarm_Percentile"            ~ "IR",
          Metric == "ertarm_Percentile"            ~ "ER",
          Metric == "starm_Percentile"             ~ "Scaption",
          Metric == "gtarm_Percentile"             ~ "Grip",
          TRUE                                     ~ Metric
        ),
        Metric = factor(Metric, levels = c("Grip","Scaption","ER","IR","Extension","Avg FB","Max FB"))
      )
  })
  
  output$pitching_percentile_plot <- renderPlot({
    req(input$get_profile_pitching)
    df <- pitching_plot_data()
    
    ggplot(df, aes(x = Percentile, y = Metric, color = Percentile)) +
      geom_segment(aes(x = 0,    xend = 100, y = Metric, yend = Metric), color = "#B9CED0", linewidth = 2) +
      geom_segment(aes(x = -2.5, xend = Percentile, y = Metric, yend = Metric, color = Percentile), linewidth = 10) +
      geom_segment(aes(x = 4.5,  xend = 5.5, y = Metric, yend = Metric), color = "white", linewidth = 10, alpha = 0.25) +
      geom_segment(aes(x = 49.5, xend = 50.5, y = Metric, yend = Metric), color = "white", linewidth = 10, alpha = 0.25) +
      geom_segment(aes(x = 94.5, xend = 95.5, y = Metric, yend = Metric), color = "white", linewidth = 10, alpha = 0.25) +
      geom_point(aes(fill = Percentile), shape = 21, color = "white", size = 12, stroke = 2) +
      geom_line(color = "#010101", linewidth = 8) +
      geom_text(aes(label = Percentile), color = "white", size = 5, fontface = "bold", hjust = 0.5, vjust = 0.5) +
      scale_x_continuous(limits = c(-2.5, 100)) +
      scale_color_gradientn(colors = c("#2166ac", "#B9CED0", "#b2182b"), limits = c(0, 100), na.value = "#2166ac") +
      scale_fill_gradientn(colors = c("#2166ac", "#B9CED0", "#b2182b"), limits = c(0, 100), na.value = "#2166ac") +
      theme_void() +
      theme(
        legend.position   = "none",
        axis.text.y       = element_text(size = 10, face = "bold", color = "grey20", hjust = 1),
        panel.background  = element_blank()
      )
  })
  
  output$futures_score_pitching <- renderUI({
    req(input$get_profile_pitching)
    df <- pitching_plot_data()
    avg_pct <- mean(df$Percentile, na.rm = TRUE)
    score   <- avg_pct / 10
    if (is.na(score)) return(NULL)
    
    div(
      style = "display: flex; gap: 20px; justify-content: center; align-items: center;",
      tags$img(src = "black_logo.png", style = "height: 50px;"),
      div(round(score, 1), style = "font-size: 48px; font-weight: bold;")
    )
  })
  
  pitching_trend_plot_data <- eventReactive(input$get_profile_pitching, {
    req(input$selected_athlete_pitching, input$date_range_pitching)
    pitching_data %>%
      filter(
        Name == input$selected_athlete_pitching,
        Date >= input$date_range_pitching[1],
        Date <= input$date_range_pitching[2],
        TaggedPitchType == "Fastball"
      ) %>%
      select(Date, Name, Max_RelSpeed, Avg_RelSpeed, Extension, total_strength, irtarm_strength, ertarm_strength, starm_strength, gtarm_strength)
  })
  
  observeEvent(pitching_trend_plot_data(), {
    updatePickerInput(session, "pitching_trend_metric",
                      choices = list(
                        "Trackman" = list(
                          "Max FB"   = "Max_RelSpeed",
                          "Avg FB"   = "Avg_RelSpeed",
                          "Extension" = "Extension"
                        ),
                        "Armcare" = list(
                          "Total Strength" = "total_strength",
                          "IR" = "irtarm_strength",
                          "ER" = "ertarm_strength",
                          "Scaption" = "starm_strength",
                          "Grip" = "gtarm_strength"
                        )
                      ),
                      selected = "Max_RelSpeed"
    )
  })
  
  output$pitching_trend_plot <- renderPlotly({
    req(input$get_profile_pitching, input$pitching_trend_metric, input$pitching_trend_grouping)
    pd <- pitching_trend_plot_data() %>%
      filter(!is.na(.data[[input$pitching_trend_metric]])) %>%
      mutate(
        GroupDate = if (input$pitching_trend_grouping == "month")
          as.Date(format(Date, "%Y-%m-01"))
        else Date
      ) %>%
      group_by(GroupDate) %>%
      summarise(
        Value = if (input$pitching_trend_metric == "Max_RelSpeed") {
          max(.data[[input$pitching_trend_metric]], na.rm = TRUE)
        } else {
          mean(.data[[input$pitching_trend_metric]], na.rm = TRUE)
        },
        .groups = "drop"
      ) %>% 
      arrange(GroupDate)
    
    if (nrow(pd) == 0) {
      return(
        plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(x = 0.5, y = 0.5, text = "No data to display",
                   showarrow = FALSE, font = list(size = 18))
            )
          )
      )
    }
    
    # calculate first & last values
    first_val  <- pd$Value[1]
    last_val   <- pd$Value[nrow(pd)]
    num_change <- round(last_val - first_val, 1)
    pct_change <- round((num_change / first_val) * 100, 1)
    
    # decide on color based on sign
    subtitle_color <- if (num_change >= 0) "#008000" else "#FF0000"
    
    # build the HTML‐styled subtitle
    subtitle_txt <- paste0(
      "<sup style='font-size:14px;color:", subtitle_color, ";'>",
      if (num_change >= 0) "+" else "", num_change,
      " (", if (pct_change >= 0) "+" else "", pct_change, "%)",
      "</sup>"
    )
    
    xaxis_args <- list(
      title      = "",
      tickformat = if (input$pitching_trend_grouping == "month") "%b %Y" else "%b %d",
      tickangle  = -45
    )
    if (input$pitching_trend_grouping == "month") {
      xaxis_args$tickmode <- "array"
      xaxis_args$tickvals <- unique(pd$GroupDate)
    }
    
    plot_ly(
      data = pd,
      x    = ~GroupDate,
      y    = ~Value,
      type = 'scatter',
      mode = 'lines+markers',
      line   = list(color = '#2D89C8', width = 3),
      marker = list(size = 8, color = '#2D89C8')
    ) %>%
      layout(
        margin     = list(t = 100, b = 50, l = 50, r = 50),
        title      = list(
          text = paste0(
            selected_athlete_pitching_event(), " ",
            input$pitching_trend_metric, " by ",
            if (input$pitching_trend_grouping == "month") "Month" else "Date",
            "<br>",
            subtitle_txt 
          ),
          x = 0.5
        ),
        font       = list(size = 20),
        xaxis      = xaxis_args,
        yaxis      = list(title = ""),
        hovermode  = 'closest',
        dragmode   = FALSE
      ) %>%
      config(
        displaylogo           = FALSE,
        scrollZoom            = FALSE,
        doubleClick           = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d","pan2d","select2d","lasso2d",
          "zoomIn2d","zoomOut2d","autoScale2d","resetScale2d",
          "hoverClosestCartesian","hoverCompareCartesian"
        ),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0(selected_athlete_pitching_event(), "_TrendPlot"),
          height   = 600,
          width    = 900,
          scale    = 2
        )
      )
  })
  
  # build the summary only once you click “Get Profile”
  trackman_summary_data <- eventReactive(input$get_profile_pitching, {
    req(input$selected_athlete_pitching, input$date_range_pitching)
    trackman_data %>%
      filter(
        Name == input$selected_athlete_pitching,
        Date >= input$date_range_pitching[1],
        Date <= input$date_range_pitching[2],
        !is.na(RelSpeed)
      ) %>%
      group_by(TaggedPitchType) %>%
      summarise(
        `# Thrown` = n(),  # Count the number of pitches for each TaggedPitchType
        #`K%` = round(mean(ZoneCheck, na.rm = TRUE) * 100, 1),
        `Avg Velo` = round(mean(RelSpeed, na.rm = TRUE), 1),
        `Max Velo` = round(max(RelSpeed, na.rm = TRUE), 1),
        RelHeight = round(mean(RelHeight, na.rm = TRUE), 3),
        RelSide = round(mean(RelSide, na.rm = TRUE), 3),
        Extension = round(mean(Extension, na.rm = TRUE), 1),
        IVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
        HB = round(mean(HorzBreak, na.rm = TRUE), 1),
        `Spin Rate` = round(mean(SpinRate, na.rm = TRUE)),
        PlateLocHeight = round(mean(PlateLocHeight, na.rm = TRUE), 1),
        PlateLocSide = round(mean(PlateLocSide, na.rm = TRUE), 1),
        SpinAxis3dSpinEfficiency = round(mean(SpinAxis3dSpinEfficiency, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>% 
      mutate(
        Total_Pitches = sum(`# Thrown`),  # Total number of pitches thrown by athlete in that month
        `% Thrown` = round((`# Thrown` / Total_Pitches) * 100, 1)  # Percent usage of each TaggedPitchType
      ) %>% 
      select(TaggedPitchType, `# Thrown`, `% Thrown`, `Max Velo`, `Avg Velo`, IVB, HB, `Spin Rate`) %>% 
      rename(Pitch = TaggedPitchType) %>% 
      arrange(desc(`Max Velo`))
    
  })
  
  output$trackman_summary_table <- render_gt({
    df <- trackman_summary_data()
    req(nrow(df) > 0)
    
    df %>%
      gt() %>% 
      tab_header(
        title = html(
          '<div style="display: flex; align-items: center; text-align: center; width: 100%;">
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
          <span style="padding: 0 50px; font-weight: 400; font-size: 24px; color: #5a5a5a;">Trackman Metrics</span>
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
        </div>'
        )
      ) %>%
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_options(
        table.width = px(1260),
        heading.align = "center",
        heading.background.color = "white",
        heading.title.font.size = "small",
        table.border.top.style   = "hidden"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#2D89C8"),
          cell_text(color = "white", weight = "bold", align = "center")
        ),
        locations = cells_column_labels(everything())
      ) %>% 
      cols_width(
        everything() ~ px(157.5)
      ) %>% 
      sub_missing(
        columns = everything()
      )
  })
  
  # build the summary only once you click “Get Profile”
  armcare_summary_data <- eventReactive(input$get_profile_pitching, {
    req(input$selected_athlete_pitching, input$date_range_pitching)
    pitching_data %>%
      filter(
        Name == input$selected_athlete_pitching,
        Date >= input$date_range_pitching[1],
        Date <= input$date_range_pitching[2],
        !is.na(arm_score)
      ) %>%
      select(arm_score, total_strength, shoulder_balance, irtarm_strength, irtarm_rom,
             ertarm_strength, ertarm_rom, starm_strength, gtarm_strength, ftarm_rom) %>% 
      distinct() %>% 
      summarise(
        `Total Strength` = round(mean(total_strength, na.rm = TRUE), 1),
        IR = round(mean(irtarm_strength, na.rm = TRUE), 1),
        `IR ROM` = round(mean(irtarm_rom, na.rm = TRUE), 1),
        ER = round(mean(ertarm_strength, na.rm = TRUE), 1),
        `ER ROM` = round(mean(ertarm_rom, na.rm = TRUE), 1),
        Scaption = round(mean(starm_strength, na.rm = TRUE), 1),
        Grip = round(mean(gtarm_strength, na.rm = TRUE), 1),
        `Flexion ROM` = round(mean(ftarm_rom, na.rm = TRUE), 1)
      )
    
  })
  
  output$armcare_summary_table <- render_gt({
    df <- armcare_summary_data()
    req(nrow(df) > 0)
    
    df %>%
      gt() %>% 
      tab_header(
        title = html(
          '<div style="display: flex; align-items: center; text-align: center; width: 100%;">
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
          <span style="padding: 0 50px; font-weight: 400; font-size: 24px; color: #5a5a5a;">Armcare Metrics</span>
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
        </div>'
        )
      ) %>%
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_options(
        table.width = px(1260),
        heading.align = "center",
        heading.background.color = "white",
        heading.title.font.size = "small",
        table.border.top.style   = "hidden"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#2D89C8"),
          cell_text(color = "white", weight = "bold", align = "center")
        ),
        locations = cells_column_labels(everything())
      ) %>% 
      cols_width(
        everything() ~ px(157.5)
      ) %>% 
      sub_missing(
        columns = everything()
      )
  })
  
  # build the summary only once you click “Get Profile”
  advanced_summary_data <- eventReactive(input$get_profile_pitching, {
    req(input$selected_athlete_pitching, input$date_range_pitching)
    trackman_data %>%
      filter(
        Name == input$selected_athlete_pitching,
        Date >= input$date_range_pitching[1],
        Date <= input$date_range_pitching[2]
      ) %>%
      group_by(TaggedPitchType) %>%
      summarise(
        `Max Velo` = round(max(RelSpeed, na.rm = TRUE), 1),
        RelHeight = round(mean(RelHeight, na.rm = TRUE), 1),
        RelSide = round(mean(RelSide, na.rm = TRUE), 1),
        Extension = round(mean(Extension, na.rm = TRUE), 1),
        VAA = round(mean(VertApprAngle, na.rm = TRUE), 2),
        HAA = round(mean(HorzApprAngle, na.rm = TRUE), 2),
        `Spin Eff` = round(mean((SpinAxis3dSpinEfficiency*100), na.rm = TRUE), 1),
        `Gyro Deg` = round(mean(SpinAxis3dLongitudinalAngle, na.rm = TRUE)),
        .groups = "drop"
      ) %>% 
      arrange(desc(`Max Velo`)) %>% 
      select(TaggedPitchType, RelHeight, RelSide, Extension, VAA, HAA, `Spin Eff`, `Gyro Deg`) %>% 
      rename(Pitch = TaggedPitchType)
    
    
  })
  
  output$advanced_summary_table <- render_gt({
    df <- advanced_summary_data()
    req(nrow(df) > 0)
    
    df %>%
      gt() %>% 
      tab_header(
        title = html(
          '<div style="display: flex; align-items: center; text-align: center; width: 100%;">
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
          <span style="padding: 0 50px; font-weight: 400; font-size: 24px; color: #5a5a5a;">Advanced Metrics</span>
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
        </div>'
        )
      ) %>%
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      tab_options(
        table.width = px(1260),
        heading.align = "center",
        heading.background.color = "white",
        heading.title.font.size = "small",
        table.border.top.style   = "hidden"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#2D89C8"),
          cell_text(color = "white", weight = "bold", align = "center")
        ),
        locations = cells_column_labels(everything())
      ) %>% 
      cols_width(
        everything() ~ px(157.5)
      ) %>% 
      sub_missing(
        columns = everything()
      )
  })
  
  # inside server()
  selected_pitching_table <- reactiveVal("trackman")
  
  observeEvent(input$get_profile_pitching, {
    selected_pitching_table("trackman")
  })
  
  observeEvent(input$show_trackman_table, {
    selected_pitching_table("trackman")
  })
  observeEvent(input$show_armcare_table, {
    selected_pitching_table("armcare")
  })
  
  output$pitching_summary_tables <- renderUI({
    req(input$get_profile_pitching, selected_pitching_table())
    
    switch(selected_pitching_table(),
           trackman = layout_columns(
             div(style = "margin: 0 auto 30px auto;",
                 gt_output("trackman_summary_table")
             )
           ),
           armcare   = layout_columns(
             div(style = "margin: 0 auto 30px auto;",
                 gt_output("armcare_summary_table")
             )
           )
    )
  })
  
  trackman_plot_data <- eventReactive(input$get_profile_pitching, {
    req(input$selected_athlete_pitching, input$date_range_pitching)
    trackman_data %>%
      filter(
        Name == input$selected_athlete_pitching,
        Date >= input$date_range_pitching[1],
        Date <= input$date_range_pitching[2]
      )
  })
  
  release_movement_plot_data <- eventReactive(input$get_profile_pitching, {
    req(input$selected_athlete_pitching, input$date_range_pitching)
    trackman_data %>%
      filter(
        Name == input$selected_athlete_pitching,
        Date >= input$date_range_pitching[1],
        Date <= input$date_range_pitching[2]
      ) %>%
      group_by(TaggedPitchType) %>% 
      summarise(`Avg HorzBreak` = mean(HorzBreak, na.rm = TRUE),
                `Avg VertBreak` = mean(InducedVertBreak, na.rm = TRUE),
                `Avg RelSide` = mean(RelSide, na.rm = TRUE),
                `Avg RelHeight` = mean(RelHeight, na.rm = TRUE))
  })
  
  output$release_trend_plot <- renderPlotly({
    req(input$get_profile_pitching)
    pd <- release_movement_plot_data()
    
    if (nrow(pd) == 0) {
      return(
        plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(x = 0.5, y = 0.5, text = "No data to display",
                   showarrow = FALSE, font = list(size = 18))
            )
          )
      )
    }
    
    plot_ly() %>%
      add_markers(
        data = pd,
        x = ~`Avg RelSide`,
        y = ~`Avg RelHeight`,
        color = ~TaggedPitchType,
        colors = c(
          "ChangeUp"="#3d9be9", "Curveball"="#00FF00", "Cutter"="#FFFF00",
          "Fastball"="#FF0000", "Sinker"="#FFA500", "Slider"="#AD0AFD", "Splitter"="#FF69B4"
        ),
        marker = list(size = 14, line = list(color = "black", width = 2)),
        hoverinfo = "text",
        text = ~paste(
          "Pitch Type: ", TaggedPitchType, "<br>",
          "Avg Release Side: ", round(`Avg RelSide`, 2), "<br>",
          "Avg Release Height: ", round(`Avg RelHeight`, 2)
        )
      ) %>%
      layout(
        margin     = list(t = 100, b = 50, l = 50, r = 50),
        title = list(text = "Release Point", x = 0.5, font = list(size = 20)),
        xaxis = list(title = "", range = c(-4, 4)),
        yaxis = list(title = "", range = c(0, 8)),
        showlegend = FALSE,
        dragmode   = FALSE
      ) %>% 
      config(
        displaylogo           = FALSE,
        scrollZoom            = FALSE,
        doubleClick           = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d","pan2d","select2d","lasso2d",
          "zoomIn2d","zoomOut2d","autoScale2d","resetScale2d",
          "hoverClosestCartesian","hoverCompareCartesian"
        ),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0(selected_athlete_pitching_event(), "_ReleasePlot"),
          height   = 600,
          width    = 900,
          scale    = 2
        )
      )
  })
  
  output$movement_trend_plot <- renderPlotly({
    req(input$get_profile_pitching)
    pd <- release_movement_plot_data()
    trackman <- trackman_plot_data()
    
    if (nrow(pd) == 0) {
      return(
        plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(x = 0.5, y = 0.5, text = "No data to display",
                   showarrow = FALSE, font = list(size = 18))
            )
          )
      )
    }
    
    plot_ly() %>%
      add_markers(
        data = trackman,
        x = ~HorzBreak,
        y = ~InducedVertBreak,
        color = ~TaggedPitchType,
        colors = c(
          "ChangeUp"="#3d9be9", "Curveball"="#00FF00", "Cutter"="#FFFF00",
          "Fastball"="#FF0000", "Sinker"="#FFA500", "Slider"="#AD0AFD", "Splitter"="#FF69B4"
        ),
        marker = list(size = 8, opacity = 0.35, line = list(width = 0)),
        hoverinfo = "text",
        text = ~paste(
          "Pitch Type: ", TaggedPitchType, "<br>",
          "Horz Break: ", round(HorzBreak, 1), "<br>",
          "Vert Break: ", round(InducedVertBreak, 1)
        )
      ) %>%
      add_markers(
        data = pd,
        x = ~`Avg HorzBreak`,
        y = ~`Avg VertBreak`,
        color = ~TaggedPitchType,
        colors = c(
          "ChangeUp"="#3d9be9", "Curveball"="#00FF00", "Cutter"="#FFFF00",
          "Fastball"="#FF0000", "Sinker"="#FFA500", "Slider"="#AD0AFD", "Splitter"="#FF69B4"
        ),
        marker = list(size = 14, line = list(color = "black", width = 2)),
        hoverinfo = "text",
        text = ~paste(
          "Pitch Type: ", TaggedPitchType, "<br>",
          "Avg Horz Break: ", round(`Avg HorzBreak`, 1), "<br>",
          "Avg Vert Break: ", round(`Avg VertBreak`, 1)
        )
      ) %>%
      layout(
        margin     = list(t = 80, b = 50, l = 50, r = 50),
        title = list(text = "Movement Profile", x = 0.5, font = list(size = 20)),
        xaxis = list(title = "", range = c(-30, 30), zeroline = TRUE, zerolinecolor = "lightgray"),
        yaxis = list(title = "", range = c(-30, 30), zeroline = TRUE, zerolinecolor = "lightgray"),
        showlegend = FALSE,
        dragmode   = FALSE
      ) %>% 
      config(
        displaylogo           = FALSE,
        scrollZoom            = FALSE,
        doubleClick           = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d","pan2d","select2d","lasso2d",
          "zoomIn2d","zoomOut2d","autoScale2d","resetScale2d",
          "hoverClosestCartesian","hoverCompareCartesian"
        ),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0(selected_athlete_pitching_event(), "_MovementPlot"),
          height   = 600,
          width    = 900,
          scale    = 2
        )
      )
  })
  
  
  # ── STRENGTH: data + plots ───────────────────────────────────────
  
  strength_plot_data <- eventReactive(input$get_profile_strength, {
    req(input$selected_athlete_strength, input$date_range_strength)
    athlete    <- input$selected_athlete_strength
    date_range <- input$date_range_strength
    
    # 1. Athlete summary within date range
    athlete_summary <- strength_data %>%
      filter(Name == athlete,
             Date >= date_range[1],
             Date <= date_range[2]) %>%
      arrange(desc(Date)) %>%
      summarise(
        Name                   = first(Name),
        Level                  = first(Level),
        Gender                 = first(Gender),
        
        IBSQT    = if (all(is.na(IBSQT)))    NA_real_ else max(IBSQT,    na.rm = TRUE),
        CMJ      = if (all(is.na(CMJ)))      NA_real_ else max(CMJ,       na.rm = TRUE),
        SHLDISOY = if (all(is.na(SHLDISOY))) NA_real_ else max(SHLDISOY,  na.rm = TRUE),
        ShotPut  = if (all(is.na(ShotPut))) NA_real_ else max(ShotPut, na.rm = TRUE),
        ISOTrunk = if (all(is.na(`Dynamo - Trunk`)))    NA_real_ else max(`Dynamo - Trunk`, na.rm = TRUE),
        Trunk    = if (all(is.na(TrunkRotation))) NA_real_ else max(TrunkRotation, na.rm = TRUE),
        .groups  = "drop"
      )
    
    # 2. Facility summary over all dates (same Level)
    facility_summary <- strength_data %>%
      filter(
        Level  == athlete_summary$Level,
        Gender == athlete_summary$Gender
      ) %>%
      group_by(Name) %>%
      summarise(
        Level              = first(Level),
        Gender             = first(Gender),
        IBSQT    = if (all(is.na(IBSQT)))    NA_real_ else max(IBSQT,    na.rm = TRUE),
        CMJ      = if (all(is.na(CMJ)))      NA_real_ else max(CMJ,       na.rm = TRUE),
        SHLDISOY = if (all(is.na(SHLDISOY))) NA_real_ else max(SHLDISOY,  na.rm = TRUE),
        ShotPut  = if (all(is.na(ShotPut))) NA_real_ else max(ShotPut, na.rm = TRUE),
        ISOTrunk = if (all(is.na(`Dynamo - Trunk`)))    NA_real_ else max(`Dynamo - Trunk`,      na.rm = TRUE),
        Trunk    = if (all(is.na(TrunkRotation))) NA_real_ else max(TrunkRotation, na.rm = TRUE),
        .groups  = "drop"
      )
    
    # 3. Replace facility’s old athlete row with date-range summary
    facility_override <- facility_summary %>%
      filter(Name != athlete) %>%
      bind_rows(athlete_summary)
    
    # 4. Compute percentiles for each exercise
    facility_with_pct <- facility_override %>%
      mutate(
        IBSQT_Percentile    = round(percent_rank(IBSQT)    * 100),
        CMJ_Percentile      = round(percent_rank(CMJ)      * 100),
        SHLDISOY_Percentile = round(percent_rank(SHLDISOY) * 100),
        ShotPut_Percentile  = round(percent_rank(ShotPut)  * 100),
        ISOTrunk_Percentile = round(percent_rank(ISOTrunk) * 100),
        Trunk_Percentile    = round(percent_rank(Trunk)    * 100)
      )
    
    # 5. Pivot athlete percentiles into two columns
    athlete_percentiles <- facility_with_pct %>%
      filter(Name == athlete) %>%
      select(ends_with("_Percentile")) %>%
      pivot_longer(
        cols      = everything(),
        names_to  = "Metric",
        values_to = "Percentile"
      ) %>%
      mutate(
        Metric = case_when(
          Metric == "IBSQT_Percentile"    ~ "IBSQT",
          Metric == "CMJ_Percentile"      ~ "CMJ",
          Metric == "SHLDISOY_Percentile" ~ "SHLDISOY",
          Metric == "ShotPut_Percentile"  ~ "Shot Put",
          Metric == "ISOTrunk_Percentile" ~ "ISO Trunk",
          Metric == "Trunk_Percentile"    ~ "Trunk",
          TRUE                             ~ Metric
        ),
        Metric = factor(
          Metric,
          levels = c("Shot Put", "SHLDISOY", "Trunk", "ISO Trunk", "CMJ", "IBSQT")
        )
      )
  })
  
  output$strength_percentile_plot <- renderPlot({
    req(input$get_profile_strength)
    df <- strength_plot_data()
    
    ggplot(df, aes(x = Percentile, y = Metric, color = Percentile)) +
      geom_segment(aes(x = 0,    xend = 100, y = Metric, yend = Metric), color = "#B9CED0", linewidth = 2) +
      geom_segment(aes(x = -2.5, xend = Percentile, y = Metric, yend = Metric, color = Percentile), linewidth = 10) +
      geom_segment(aes(x = 4.5,  xend = 5.5, y = Metric, yend = Metric), color = "white", linewidth = 10, alpha = 0.25) +
      geom_segment(aes(x = 49.5, xend = 50.5, y = Metric, yend = Metric), color = "white", linewidth = 10, alpha = 0.25) +
      geom_segment(aes(x = 94.5, xend = 95.5, y = Metric, yend = Metric), color = "white", linewidth = 10, alpha = 0.25) +
      geom_point(aes(fill = Percentile), shape = 21, color = "white", size = 12, stroke = 2) +
      geom_line(color = "#010101", linewidth = 8) +
      geom_text(aes(label = Percentile), color = "white", size = 5, fontface = "bold", hjust = 0.5, vjust = 0.5) +
      scale_x_continuous(limits = c(-2.5, 100)) +
      scale_color_gradientn(colors = c("#2166ac", "#B9CED0", "#b2182b"), limits = c(0, 100), na.value = "#2166ac") +
      scale_fill_gradientn(colors = c("#2166ac", "#B9CED0", "#b2182b"), limits = c(0, 100), na.value = "#2166ac") +
      theme_void() +
      theme(
        legend.position   = "none",
        axis.text.y       = element_text(size = 10, face = "bold", color = "grey20", hjust = 1),
        panel.background  = element_blank()
      )
  })
  
  output$futures_score_strength <- renderUI({
    req(input$get_profile_strength)
    df <- strength_plot_data()
    avg_pct <- mean(df$Percentile, na.rm = TRUE)
    score   <- avg_pct / 10
    if (is.na(score)) return(NULL)
    
    div(
      style = "display: flex; gap: 20px; justify-content: center; align-items: center;",
      tags$img(src = "black_logo.png", style = "height: 50px;"),
      div(round(score, 1), style = "font-size: 48px; font-weight: bold;")
    )
  })
  
  # Trend-plot data + UI
  strength_trend_plot_data <- eventReactive(input$get_profile_strength, {
    req(input$selected_athlete_strength, input$date_range_strength)
    strength_data %>%
      filter(
        Name == input$selected_athlete_strength,
        Date >= input$date_range_strength[1],
        Date <= input$date_range_strength[2]
      ) %>%
      select(Date, Name, IBSQT, CMJ, SHLDISOY, `Dynamo - Trunk`, `External Rotation`, `Internal Rotation`, 
             ProteusFullTest, TrunkRotation, D2Ext, D2Flex, ShotPut, Weight)
  })
  
  observeEvent(strength_trend_plot_data(), {
    updatePickerInput(session, "strength_trend_metric",
                      choices = list(
                        "ForceDecks" = list(
                          "IBSQT"   = "IBSQT",
                          "CMJ"   = "CMJ",
                          "SHLDISOY" = "SHLDISOY"
                        ),
                        "Dynamo" = list(
                          "ISO Trunk Rotation"   = "Dynamo - Trunk",
                          "Shoudler ER"   = "External Rotation",
                          "Shoulder IR" = "Internal Rotation"
                        ),
                        "Proteus" = list(
                          "Power Score" = "ProteusFullTest",
                          "Trunk Rotation" = "TrunkRotation",
                          "D2 Extension"   = "D2Ext",
                          "D2 Flexion"   = "D2Flex",
                          "Shot Put" = "ShotPut"
                        ),
                        "Other" = list(
                          "Weight" = "Weight"
                        )
                      ),
                      selected = "IBSQT"
    )
  })
  
  output$strength_trend_plot <- renderPlotly({
    req(input$get_profile_strength, input$strength_trend_metric, input$strength_trend_grouping)
    pd <- strength_trend_plot_data() %>%
      filter(!is.na(.data[[input$strength_trend_metric]])) %>%
      mutate(
        GroupDate = if (input$strength_trend_grouping == "month")
          as.Date(format(Date, "%Y-%m-01"))
        else Date
      ) %>%
      group_by(GroupDate) %>%
      summarise(Value = mean(.data[[input$strength_trend_metric]], na.rm = TRUE), .groups = "drop") %>%
      arrange(GroupDate)
    
    if (nrow(pd) == 0) {
      return(
        plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(x = 0.5, y = 0.5, text = "No data to display",
                   showarrow = FALSE, font = list(size = 18))
            )
          )
      )
    }
    
    # calculate first & last values
    first_val  <- pd$Value[1]
    last_val   <- pd$Value[nrow(pd)]
    num_change <- round(last_val - first_val, 1)
    pct_change <- round((num_change / first_val) * 100, 1)
    
    # decide on color based on sign
    subtitle_color <- if (num_change >= 0) "#008000" else "#FF0000"
    
    # build the HTML‐styled subtitle
    subtitle_txt <- paste0(
      "<sup style='font-size:14px;color:", subtitle_color, ";'>",
      if (num_change >= 0) "+" else "", num_change,
      " (", if (pct_change >= 0) "+" else "", pct_change, "%)",
      "</sup>"
    )
    
    xaxis_args <- list(
      title      = "",
      tickformat = if (input$strength_trend_grouping == "month") "%b %Y" else "%b %d",
      tickangle  = -45
    )
    if (input$strength_trend_grouping == "month") {
      xaxis_args$tickmode <- "array"
      xaxis_args$tickvals <- unique(pd$GroupDate)
    }
    
    plot_ly(
      data = pd,
      x    = ~GroupDate,
      y    = ~Value,
      type = 'scatter',
      mode = 'lines+markers',
      line   = list(color = '#2D89C8', width = 3),
      marker = list(size = 8, color = '#2D89C8')
    ) %>%
      layout(
        margin     = list(t = 100, b = 50, l = 50, r = 50),
        title      = list(
          text = paste0(
            selected_athlete_strength_event(), " ",
            input$strength_trend_metric, " by ",
            if (input$strength_trend_grouping == "month") "Month" else "Date",
            "<br>",
            subtitle_txt 
          ),
          x = 0.5
        ),
        font       = list(size = 20),
        xaxis      = xaxis_args,
        yaxis      = list(title = ""),
        hovermode  = 'closest',
        dragmode   = FALSE
      ) %>%
      config(
        displaylogo           = FALSE,
        scrollZoom            = FALSE,
        doubleClick           = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d","pan2d","select2d","lasso2d",
          "zoomIn2d","zoomOut2d","autoScale2d","resetScale2d",
          "hoverClosestCartesian","hoverCompareCartesian"
        ),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0(selected_athlete_strength_event(), "_TrendPlot"),
          height   = 600,
          width    = 900,
          scale    = 2
        )
      )
  })
  
  # build the summary only once you click “Get Profile”
  forceplate_summary_data <- eventReactive(input$get_profile_strength, {
    req(input$selected_athlete_strength, input$date_range_strength)
    strength_data %>%
      filter(
        Name == input$selected_athlete_strength,
        Date >= input$date_range_strength[1],
        Date <= input$date_range_strength[2]
      ) %>%
      summarise(
        CMJ    = if (all(is.na(CMJ)))    NA_real_ else mean(CMJ,    na.rm = TRUE),
        IBSQT    = if (all(is.na(IBSQT)))    NA_real_ else mean(IBSQT,    na.rm = TRUE),
        SHLDISOY    = if (all(is.na(SHLDISOY)))    NA_real_ else mean(SHLDISOY,    na.rm = TRUE)
      )
  })
  
  output$forceplate_summary_table <- render_gt({
    df <- forceplate_summary_data()
    req(nrow(df) > 0)
    
    df %>%
      gt() %>%
      tab_header(
        title = html(
          '<div style="display: flex; align-items: center; text-align: center; width: 100%;">
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
          <span style="padding: 0 50px; font-weight: 400; font-size: 24px; color: #5a5a5a;">Forceplate Metrics</span>
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
        </div>'
        )
      ) %>%
      tab_options(
        table.width = px(1260),
        heading.align = "center",
        heading.background.color = "white",
        heading.title.font.size = "small",
        table.border.top.style   = "hidden"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#2D89C8"),
          cell_text(color = "white", weight = "bold", align = "center")
        ),
        locations = cells_column_labels(everything())
      ) %>% 
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(everything())
      ) %>%
      cols_width(
        everything() ~ px(420)
      ) %>% 
      sub_missing(
        columns = everything()
      )
  })
  
  dynamo_summary_data <- eventReactive(input$get_profile_strength, {
    req(input$selected_athlete_strength, input$date_range_strength)
    strength_data %>%
      filter(
        Name == input$selected_athlete_strength,
        Date >= input$date_range_strength[1],
        Date <= input$date_range_strength[2]
      ) %>%
      summarise(
        `Internal Rotation`  = if (all(is.na(`Internal Rotation`)))    NA_real_ else mean(`Internal Rotation`,    na.rm = TRUE),
        `External Rotation`  = if (all(is.na(`External Rotation`)))    NA_real_ else mean(`External Rotation`,    na.rm = TRUE),
        `ISO Trunk Rotation` = if (all(is.na(`Dynamo - Trunk`)))    NA_real_ else mean(`Dynamo - Trunk`,    na.rm = TRUE)
      )
  })
  
  output$dynamo_summary_table <- render_gt({
    df <- dynamo_summary_data()
    req(nrow(df) > 0)
    
    df %>%
      gt() %>%
      tab_header(
        title = html(
          '<div style="display: flex; align-items: center; text-align: center; width: 100%;">
        <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
        <span style="padding: 0 50px; font-weight: 400; font-size: 24px; color: #5a5a5a;">Dynamo Metrics</span>
        <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
      </div>'
        )
      ) %>%
      tab_options(
        table.width = px(1260),
        heading.align = "center",
        heading.background.color = "white",
        heading.title.font.size = "small",
        table.border.top.style   = "hidden"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#2D89C8"),
          cell_text(color = "white", weight = "bold", align = "center")
        ),
        locations = cells_column_labels(everything())
      ) %>% 
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(everything())
      ) %>%
      cols_width(
        everything() ~ px(420)
      ) %>% 
      sub_missing(
        columns = everything()
      )
  })
  
  proteus_summary_data <- eventReactive(input$get_profile_strength, {
    req(input$selected_athlete_strength, input$date_range_strength)
    strength_data %>%
      filter(
        Name == input$selected_athlete_strength,
        Date >= input$date_range_strength[1],
        Date <= input$date_range_strength[2]
      ) %>%
      summarise(
        `Power Score`    = round(mean(ProteusFullTest, na.rm = TRUE), 1),
        `D2 Extension`   = round(mean(D2Ext, na.rm = TRUE), 1),
        `D2 Flexion`     = round(mean(D2Flex, na.rm = TRUE), 1),
        `Trunk Rotation` = round(mean(TrunkRotation, na.rm = TRUE), 1),
        `Shot Put`       = round(mean(ShotPut, na.rm = TRUE), 1)
      )
  })
  
  output$proteus_summary_table <- render_gt({
    df <- proteus_summary_data()
    req(nrow(df) > 0)
    
    df %>%
      gt() %>%
      tab_header(
        title = html(
          '<div style="display: flex; align-items: center; text-align: center; width: 100%;">
        <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
        <span style="padding: 0 50px; font-weight: 400; font-size: 24px; color: #5a5a5a;">Proteus Metrics</span>
        <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
      </div>'
        )
      ) %>%
      tab_options(
        table.width = px(1260),
        heading.align = "center",
        heading.background.color = "white",
        heading.title.font.size = "small",
        table.border.top.style   = "hidden"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#2D89C8"),
          cell_text(color = "white", weight = "bold", align = "center")
        ),
        locations = cells_column_labels(everything())
      ) %>% 
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(everything())
      ) %>%
      cols_width(
        everything() ~ px(252)
      ) %>% 
      sub_missing(
        columns = everything()
      )
  })
  
  # inside server()
  selected_strength_table <- reactiveVal("forceplate")
  
  observeEvent(input$get_profile_strength, {
    selected_strength_table("forceplate")
  })
  
  observeEvent(input$show_forceplate_table, {
    selected_strength_table("forceplate")
  })
  observeEvent(input$show_dynamo_table, {
    selected_strength_table("dynamo")
  })
  observeEvent(input$show_proteus_table, {
    selected_strength_table("proteus")
  })
  
  output$strength_summary_tables <- renderUI({
    req(input$get_profile_strength, selected_strength_table())
    
    switch(selected_strength_table(),
           forceplate = layout_columns(
             div(style = "margin: 0 auto 30px auto;",
                 gt_output("forceplate_summary_table")
             )
           ),
           dynamo   = layout_columns(
             div(style = "margin: 0 auto 30px auto;",
                 gt_output("dynamo_summary_table")
             )
           ),
           proteus   = layout_columns(
             div(style = "margin: 0 auto 30px auto;",
                 gt_output("proteus_summary_table")
             )
           )
    )
  })
  
  
  # ── SPEED: data + plots ──────────────────────────────────────────
  
  speed_plot_data <- eventReactive(input$get_profile_speed, {
    req(input$selected_athlete_speed, input$date_range_speed)
    athlete    <- input$selected_athlete_speed
    date_range <- input$date_range_speed
    
    # 1. athlete summary within date range
    athlete_summary <- speed_data %>%
      filter(Name == athlete,
             Date >= date_range[1],
             Date <= date_range[2]) %>%
      arrange(desc(Date)) %>% 
      summarise(
        Name                = first(Name),
        Level               = first(Level),
        Gender              = first(Gender),
        early_acceleration  = if (all(is.na(early_acceleration))) NA_real_ else min(early_acceleration, na.rm = TRUE),
        late_acceleration   = if (all(is.na(late_acceleration)))  NA_real_ else min(late_acceleration,  na.rm = TRUE),
        thirty_yard         = if (all(is.na(thirty_yard)))        NA_real_ else min(thirty_yard,        na.rm = TRUE),
        forty_yard          = if (all(is.na(forty_yard)))         NA_real_ else min(forty_yard,         na.rm = TRUE),
        max_velocity        = if (all(is.na(max_velocity)))       NA_real_ else max(max_velocity,       na.rm = TRUE),
        .groups = "drop"
      )
    
    # 2. facility summary over all dates (same Level & Gender)
    facility_summary <- speed_data %>%
      filter(
        Level  == athlete_summary$Level,
        Gender == athlete_summary$Gender
      ) %>%
      group_by(Name) %>%
      summarise(
        Level              = first(Level),
        Gender             = first(Gender),
        early_acceleration = if (all(is.na(early_acceleration))) NA_real_ else min(early_acceleration, na.rm = TRUE),
        late_acceleration  = if (all(is.na(late_acceleration)))  NA_real_ else min(late_acceleration,  na.rm = TRUE),
        thirty_yard        = if (all(is.na(thirty_yard)))        NA_real_ else min(thirty_yard,        na.rm = TRUE),
        forty_yard         = if (all(is.na(forty_yard)))         NA_real_ else min(forty_yard,         na.rm = TRUE),
        max_velocity       = if (all(is.na(max_velocity)))       NA_real_ else max(max_velocity,       na.rm = TRUE),
        .groups = "drop"
      )
    
    # 3. replace facility’s old athlete row with date-range summary
    facility_override <- facility_summary %>%
      filter(Name != athlete) %>%
      bind_rows(athlete_summary)
    
    # 4. compute percentiles (lower times = higher pct)
    facility_with_pct <- facility_override %>%
      mutate(
        early_acceleration_Percentile = round(percent_rank(-early_acceleration) * 100),
        late_acceleration_Percentile  = round(percent_rank(-late_acceleration)  * 100),
        thirty_yard_Percentile        = round(percent_rank(-thirty_yard)        * 100),
        forty_yard_Percentile         = round(percent_rank(-forty_yard)         * 100),
        max_velocity_Percentile       = round(percent_rank(max_velocity)        * 100)
      )
    
    # 5. pivot athlete percentiles into two columns
    athlete_percentiles <- facility_with_pct %>%
      filter(Name == athlete) %>%
      select(ends_with("_Percentile")) %>%
      pivot_longer(
        cols      = everything(),
        names_to  = "Metric",
        values_to = "Percentile"
      ) %>%
      mutate(
        Metric = case_when(
          Metric == "early_acceleration_Percentile" ~ "Early Accel",
          Metric == "late_acceleration_Percentile"  ~ "Late Accel",
          Metric == "thirty_yard_Percentile"        ~ "30 Yard",
          Metric == "forty_yard_Percentile"         ~ "40 Yard",
          Metric == "max_velocity_Percentile"       ~ "Top Speed",
          TRUE                                      ~ Metric
        ),
        Metric = factor(
          Metric,
          levels = c("Top Speed", "40 Yard", "30 Yard", "Late Accel", "Early Accel")
        )
      )
  })
  
  output$speed_percentile_plot <- renderPlot({
    req(input$get_profile_speed)
    df <- speed_plot_data()
    
    ggplot(df, aes(x = Percentile, y = Metric, color = Percentile)) +
      geom_segment(aes(x = 0,    xend = 100, y = Metric, yend = Metric), color = "#B9CED0", linewidth = 2) +
      geom_segment(aes(x = -2.5, xend = Percentile, y = Metric, yend = Metric, color = Percentile), linewidth = 10) +
      geom_segment(aes(x = 4.5,  xend = 5.5, y = Metric, yend = Metric), color = "white", linewidth = 10, alpha = 0.25) +
      geom_segment(aes(x = 49.5, xend = 50.5, y = Metric, yend = Metric), color = "white", linewidth = 10, alpha = 0.25) +
      geom_segment(aes(x = 94.5, xend = 95.5, y = Metric, yend = Metric), color = "white", linewidth = 10, alpha = 0.25) +
      geom_point(aes(fill = Percentile), shape = 21, color = "white", size = 12, stroke = 2) +
      geom_line(color = "#010101", linewidth = 8) +
      geom_text(aes(label = Percentile), color = "white", size = 5, fontface = "bold", hjust = 0.5, vjust = 0.5) +
      scale_x_continuous(limits = c(-2.5, 100)) +
      scale_color_gradientn(colors = c("#2166ac", "#B9CED0", "#b2182b"), limits = c(0, 100), na.value = "#2166ac") +
      scale_fill_gradientn(colors = c("#2166ac", "#B9CED0", "#b2182b"), limits = c(0, 100), na.value = "#2166ac") +
      theme_void() +
      theme(
        legend.position   = "none",
        axis.text.y       = element_text(size = 10, face = "bold", color = "grey20", hjust = 1),
        panel.background  = element_blank()
      )
  })
  
  output$futures_score_speed <- renderUI({
    req(input$get_profile_speed)
    df <- speed_plot_data()
    avg_pct <- mean(df$Percentile, na.rm = TRUE)
    score   <- avg_pct / 10
    if (is.na(score)) return(NULL)
    
    div(
      style = "display: flex; gap: 20px; justify-content: center; align-items: center;",
      tags$img(src = "black_logo.png", style = "height: 50px;"),
      div(round(score, 1), style = "font-size: 48px; font-weight: bold;")
    )
  })
  
  # Trend-plot data + UI
  speed_trend_plot_data <- eventReactive(input$get_profile_speed, {
    req(input$selected_athlete_speed, input$date_range_speed)
    speed_data %>%
      filter(
        Name == input$selected_athlete_speed,
        Date >= input$date_range_speed[1],
        Date <= input$date_range_speed[2],
        Test == "Futures Sprint 40y: 10y, 20y, 30y, 40y"
      ) %>%
      select(Date, Name, early_acceleration, late_acceleration, thirty_yard, forty_yard, max_velocity)
  })
  
  observeEvent(speed_trend_plot_data(), {
    updatePickerInput(session, "speed_trend_metric",
                      choices = list(
                        "VALD" = list(
                          "Early Acceleration"   = "early_acceleration",
                          "Late Acceleration"   = "late_acceleration",
                          "30 Yard" = "thirty_yard",
                          "40 Yard" = "forty_yard",
                          "Max Velocity" = "max_velocity"
                        )
                      ),
                      selected = "early_acceleration"
    )
  })
  
  output$speed_trend_plot <- renderPlotly({
    req(input$get_profile_speed, input$speed_trend_metric, input$speed_trend_grouping)
    pd <- speed_trend_plot_data() %>%
      filter(!is.na(.data[[input$speed_trend_metric]])) %>%
      mutate(
        GroupDate = if (input$speed_trend_grouping == "month")
          as.Date(format(Date, "%Y-%m-01"))
        else Date
      ) %>%
      group_by(GroupDate) %>%
      summarise(Value = mean(.data[[input$speed_trend_metric]], na.rm = TRUE), .groups = "drop") %>%
      arrange(GroupDate)
    
    if (nrow(pd) == 0) {
      return(
        plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(x = 0.5, y = 0.5, text = "No data to display",
                   showarrow = FALSE, font = list(size = 18))
            )
          )
      )
    }
    
    # calculate first & last values
    first_val  <- pd$Value[1]
    last_val   <- pd$Value[nrow(pd)]
    num_change <- round(last_val - first_val, 3)
    pct_change <- round((num_change / first_val) * 100, 1)
    
    # determine if this is a "lower is better" metric
    time_metrics <- c("early_acceleration", "late_acceleration", "thirty_yard", "forty_yard")
    is_time_metric <- input$speed_trend_metric %in% time_metrics
    
    # determine subtitle color
    subtitle_color <- if (is_time_metric) {
      if (num_change <= 0) "#008000" else "#FF0000"  # for time metrics, decrease (negative) is good
    } else {
      if (num_change >= 0) "#008000" else "#FF0000"  # for other metrics, increase (positive) is good
    }
    
    # build the HTML‐styled subtitle
    subtitle_txt <- paste0(
      "<sup style='font-size:14px;color:", subtitle_color, ";'>",
      if (num_change >= 0) "+" else "", num_change,
      " (", if (pct_change >= 0) "+" else "", pct_change, "%)",
      "</sup>"
    )
    
    xaxis_args <- list(
      title      = "",
      tickformat = if (input$speed_trend_grouping == "month") "%b %Y" else "%b %d",
      tickangle  = -45
    )
    if (input$speed_trend_grouping == "month") {
      xaxis_args$tickmode <- "array"
      xaxis_args$tickvals <- unique(pd$GroupDate)
    }
    
    plot_ly(
      data = pd,
      x    = ~GroupDate,
      y    = ~Value,
      type = 'scatter',
      mode = 'lines+markers',
      line   = list(color = '#2D89C8', width = 3),
      marker = list(size = 8, color = '#2D89C8')
    ) %>%
      layout(
        margin     = list(t = 100, b = 50, l = 50, r = 50),
        title      = list(
          text = paste0(
            selected_athlete_speed_event(), " ",
            input$speed_trend_metric, " by ",
            if (input$speed_trend_grouping == "month") "Month" else "Date",
            "<br>",
            subtitle_txt 
          ),
          x = 0.5
        ),
        font       = list(size = 20),
        xaxis      = xaxis_args,
        yaxis      = list(title = ""),
        hovermode  = 'closest',
        dragmode   = FALSE
      ) %>%
      config(
        displaylogo           = FALSE,
        scrollZoom            = FALSE,
        doubleClick           = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d","pan2d","select2d","lasso2d",
          "zoomIn2d","zoomOut2d","autoScale2d","resetScale2d",
          "hoverClosestCartesian","hoverCompareCartesian"
        ),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0(selected_athlete_speed_event(), "_TrendPlot"),
          height   = 600,
          width    = 900,
          scale    = 2
        )
      )
  })
  
  # build the summary only once you click “Get Profile”
  smartspeed_summary_data <- eventReactive(input$get_profile_speed, {
    req(input$selected_athlete_speed, input$date_range_speed)
    speed_data %>%
      filter(
        Name == input$selected_athlete_speed,
        Date >= input$date_range_speed[1],
        Date <= input$date_range_speed[2]
      ) %>%
      summarise(
        `Early Accel`   = round(mean(early_acceleration, na.rm = TRUE), 3),
        `Late Accel`   = round(mean(late_acceleration, na.rm = TRUE), 3),
        `30 Yard` = round(mean(thirty_yard,    na.rm = TRUE), 2),
        `40 Yard` = round(mean(forty_yard,    na.rm = TRUE), 2),
        `Top Speed` = round(mean(max_velocity, na.rm = TRUE), 1)
      )
  })
  
  output$smartspeed_summary_table <- render_gt({
    df <- smartspeed_summary_data()
    req(nrow(df) > 0)
    
    df %>%
      gt() %>%
      tab_header(
        title = html(
          '<div style="display: flex; align-items: center; text-align: center; width: 100%;">
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
          <span style="padding: 0 50px; font-weight: 400; font-size: 24px; color: #5a5a5a;">SmartSpeed Metrics</span>
          <hr style="flex-grow: 1; border: none; height: 1px; background-color: #000000;">
        </div>'
        )
      ) %>%
      tab_options(
        table.width = px(1260),
        heading.align = "center",
        heading.background.color = "white",
        heading.title.font.size = "small",
        table.border.top.style   = "hidden"
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#2D89C8"),
          cell_text(color = "white", weight = "bold", align = "center")
        ),
        locations = cells_column_labels(everything())
      ) %>% 
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(everything())
      ) %>%
      cols_width(
        everything() ~ px(252)
      ) %>% 
      sub_missing(
        columns = everything()
      )
  })
  
  
  output$leaderboard_date_range_ui <- renderUI({
    dateRangeInput("leaderboard_date_range", "Select Date Range:",
                   start = Sys.Date() %m-% months(3), end = Sys.Date())
  })
  
  output$leaderboard_level_ui <- renderUI({
    selectInput("leaderboard_level", "Select Level:",
                choices = c("L1", "L2", "L3", "Collegiate", "Professional", "All"),
                selected = "L3")
  })
  
  output$leaderboard_gender_ui <- renderUI({
    req(input$leaderboard_metric)
    if (input$leaderboard_metric != "Pitching") {
      selectInput("leaderboard_gender", "Select Gender:",
                  choices = c("Male", "Female", "All"),
                  selected = "Male")
    }
  })
  
  
  generate_hitting_leaderboard <- function() {
    req(input$leaderboard_date_range, input$leaderboard_level)
    
    df <- hitting_data %>%
      filter(Date >= input$leaderboard_date_range[1],
             Date <= input$leaderboard_date_range[2],
             Level %in% if (input$leaderboard_level == "All") unique(Level) else input$leaderboard_level,
             Gender %in% if (input$leaderboard_gender == "All" || input$leaderboard_metric == "Pitching") unique(Gender) else input$leaderboard_gender) %>% 
      group_by(Name, Level, Gender) %>%
      summarise(
        MaxVel = if (all(is.na(MaxVel))) NA_real_ else round(max(MaxVel, na.rm = TRUE), 1),
        AvgVel = if (all(is.na(AvgVel))) NA_real_ else round(mean(AvgVel, na.rm = TRUE), 1),
        MaxDist = if (all(is.na(MaxDist))) NA_integer_ else round(max(MaxDist, na.rm = TRUE)),
        bat_speed = if (all(is.na(bat_speed))) NA_real_ else round(mean(bat_speed, na.rm = TRUE), 1),
        rotational_acceleration = if (all(is.na(rotational_acceleration))) NA_real_ else round(mean(rotational_acceleration, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      select(-Level, -Gender)
    
    reactable(
      df,
      defaultSorted = "MaxVel",
      defaultSortOrder = "desc",
      columns = list(
        Name                    = colDef(name = "Athlete",             sortNALast = TRUE, minWidth = 225),
        MaxVel                  = colDef(name = "Max EV (mph)",        sortNALast = TRUE, minWidth = 207),
        AvgVel                  = colDef(name = "Avg EV (mph)",        sortNALast = TRUE, minWidth = 207),
        MaxDist                 = colDef(name = "Max Dist (ft)",       sortNALast = TRUE, minWidth = 207),
        bat_speed               = colDef(name = "Bat Speed (mph)",     sortNALast = TRUE, minWidth = 207),
        rotational_acceleration = colDef(name = "Rot Accel (g)",       sortNALast = TRUE, minWidth = 207)
      ),
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE
    )
  }
  
  generate_pitching_leaderboard <- function() {
    req(input$leaderboard_date_range, input$leaderboard_level)
    
    df <- pitching_data %>%
      filter(Date >= input$leaderboard_date_range[1],
             Date <= input$leaderboard_date_range[2],
             Level %in% if (input$leaderboard_level == "All") unique(Level) else input$leaderboard_level,
             TaggedPitchType == "Fastball") %>% 
      group_by(Name, Level) %>%
      summarise(
        Max_RelSpeed = if (all(is.na(Max_RelSpeed))) NA_real_ else round(max(Max_RelSpeed, na.rm = TRUE), 1),
        Avg_RelSpeed = if (all(is.na(Avg_RelSpeed))) NA_real_ else round(mean(Avg_RelSpeed, na.rm = TRUE), 1),
        total_strength = if (all(is.na(total_strength))) NA_integer_ else round(max(total_strength, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      select(-Level)
    
    reactable(
      df,
      defaultSorted = "Max_RelSpeed",
      defaultSortOrder = "desc",
      columns = list(
        Name           = colDef(name = "Athlete",               sortNALast = TRUE, minWidth = 225),
        Max_RelSpeed   = colDef(name = "Max FB Velo (mph)",     sortNALast = TRUE, minWidth = 345),
        Avg_RelSpeed   = colDef(name = "Avg FB Velo (mph)",     sortNALast = TRUE, minWidth = 345),
        total_strength = colDef(name = "Arm Strength (lbs)",    sortNALast = TRUE, minWidth = 345)
      ),
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE
    )
  }
  
  generate_strength_leaderboard <- function() {
    req(input$leaderboard_date_range, input$leaderboard_level)
    
    df <- strength_data %>%
      filter(Date >= input$leaderboard_date_range[1],
             Date <= input$leaderboard_date_range[2],
             Level %in% if (input$leaderboard_level == "All") unique(Level) else input$leaderboard_level,
             Gender %in% if (input$leaderboard_gender == "All" || input$leaderboard_metric == "Pitching") unique(Gender) else input$leaderboard_gender) %>% 
      group_by(Name, Level, Gender) %>%
      summarise(
        CMJ = if (all(is.na(CMJ))) NA_real_ else round(max(CMJ, na.rm = TRUE)),
        IBSQT = if (all(is.na(IBSQT))) NA_real_ else round(max(IBSQT, na.rm = TRUE)),
        SHLDISOY = if (all(is.na(SHLDISOY))) NA_integer_ else round(max(SHLDISOY, na.rm = TRUE), 1),
        ProteusFullTest = if (all(is.na(ProteusFullTest))) NA_real_ else round(max(ProteusFullTest, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      select(-Level, -Gender)
    
    reactable(
      df,
      defaultSorted = "CMJ",
      defaultSortOrder = "desc",
      columns = list(
        Name                          = colDef(name = "Athlete",             sortNALast = TRUE, minWidth = 225),
        CMJ                           = colDef(name = "CMJ (W)",             sortNALast = TRUE, minWidth = 258.75),
        IBSQT                         = colDef(name = "IBSQT (N)",           sortNALast = TRUE, minWidth = 258.75),
        SHLDISOY                      = colDef(name = "SHLDISOY (N)",        sortNALast = TRUE, minWidth = 258.75),
        ProteusFullTest               = colDef(name = "Power Score (W)",     sortNALast = TRUE, minWidth = 258.75)
      ),
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE
    )
  }
  
  generate_speed_leaderboard <- function() {
    req(input$leaderboard_date_range, input$leaderboard_level)
    
    df <- speed_data %>%
      filter(Date >= input$leaderboard_date_range[1],
             Date <= input$leaderboard_date_range[2],
             Level %in% if (input$leaderboard_level == "All") unique(Level) else input$leaderboard_level,
             Gender %in% if (input$leaderboard_gender == "All" || input$leaderboard_metric == "Pitching") unique(Gender) else input$leaderboard_gender) %>% 
      group_by(Name, Level, Gender) %>%
      summarise(
        early_acceleration = if (all(is.na(early_acceleration))) NA_real_ else round(min(early_acceleration, na.rm = TRUE), 3),
        late_acceleration = if (all(is.na(late_acceleration))) NA_real_ else round(min(late_acceleration, na.rm = TRUE), 3),
        thirty_yard = if (all(is.na(thirty_yard))) NA_integer_ else round(min(thirty_yard, na.rm = TRUE), 2),
        forty_yard = if (all(is.na(forty_yard))) NA_real_ else round(min(forty_yard, na.rm = TRUE), 2),
        max_velocity = if (all(is.na(max_velocity))) NA_real_ else round(max(max_velocity, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      select(-Level, -Gender)
    
    reactable(
      df,
      defaultSorted = "max_velocity",
      defaultSortOrder = "desc",
      columns = list(
        Name               = colDef(name = "Athlete",            sortNALast = TRUE, minWidth = 225),
        early_acceleration = colDef(name = "Early Accel (sec)",  sortNALast = TRUE, minWidth = 207),
        late_acceleration  = colDef(name = "Late Accel (sec)",   sortNALast = TRUE, minWidth = 207),
        thirty_yard        = colDef(name = "30 Yard (sec)",      sortNALast = TRUE, minWidth = 207),
        forty_yard         = colDef(name = "40 Yard (sec)",      sortNALast = TRUE, minWidth = 207),
        max_velocity       = colDef(name = "Max Speed (mph)",    sortNALast = TRUE, minWidth = 207)
      ),
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE
    )
  }
  
  output$leaderboard_table <- renderReactable({
    req(input$leaderboard_metric)
    
    switch(input$leaderboard_metric,
           "Hitting"  = generate_hitting_leaderboard(),
           "Pitching" = generate_pitching_leaderboard(),
           "Strength" = generate_strength_leaderboard(),
           "Speed"    = generate_speed_leaderboard()
    )
  })
  
# Percentile Distributions ------------------------------------------------

  observeEvent(input$percentile_department, {
    metric_choices <- switch(input$percentile_department,
                             "Hitting"   = c("Max EV" = "MaxVel", "Avg EV" = "AvgVel", "Max Dist" = "MaxDist", "Avg Dist" = "AvgDist", "Bat Speed" = "bat_speed", "Rotational Acceleration" = "rotational_acceleration"),
                             "Pitching"  = c("Max FB" = "Max_RelSpeed", "Avg FB" = "Avg_RelSpeed", "FB Extension" = "Extension"),
                             "Strength"  = c("IBSQT", "CMJ", "SHLDISOY", "Trunk Rotation" = "TrunkRotation", "Shot Put" = "ShotPut", "D2 Ext/Flex" = "D2Average"),
                             "Speed"     = c("Early Acceleration" = "early_acceleration", "Late Acceleration" = "late_acceleration", "30 Yard" = "thirty_yard", "40 Yard" = "forty_yard", "Top Speed" = "max_velocity"),
                             character(0)
    )
    
    output$metric_selector <- renderUI({
      selectInput("percentile_metric", "Select Metric:", choices = metric_choices, selected = metric_choices[1])
    })
  })
  
  output$percentile_levels_ui <- renderUI({
    pickerInput("percentile_levels", "Select Level:",
                choices = c("L1", "L2", "L3", "Collegiate", "Professional"),
                selected = c("L3", "Collegiate"),
                multiple = TRUE,
                options = list('max-options' = 2))
  })
  
  output$percentile_gender_ui <- renderUI({
    req(input$percentile_department)
    if (input$percentile_department != "Pitching") {
      selectInput("percentile_gender", "Select Gender:",
                  choices = c("Male", "Female", "All"),
                  selected = "Male")
    }
  })
  
  get_filtered_data <- reactive({
    req(input$percentile_department, input$percentile_metric, input$percentile_gender, input$percentile_levels)
    
    # pick the right data
    df <- switch(input$percentile_department,
                 "Hitting"   = hitting_data,
                 "Pitching"  = pitching_data %>% filter(TaggedPitchType=="Fastball"),
                 "Strength"  = strength_data,
                 "Speed"     = speed_data)
    
    # make sure the metric is valid for this df
    req(input$percentile_metric %in% names(df))
    
    df %>%
      filter(
        Gender %in% input$percentile_gender,
        Level  %in% input$percentile_levels,
        !is.na(.data[[input$percentile_metric]])
      ) %>%
      mutate(Value = .data[[input$percentile_metric]])
  })
  
  output$percentile_plot <- renderPlotly({
    req(get_filtered_data())
    df      <- get_filtered_data()
    levels  <- unique(df$Level)
    
    # which of your metrics are "time" metrics?
    timed_metrics <- c("early_acceleration", "late_acceleration", "thirty_yard", "forty_yard")
    is_timed <- input$percentile_metric %in% timed_metrics
    
    level_colors <- c(
      L1           = "#1f77b4",
      L2           = "#ff7f0e",
      L3           = "#2ca02c",
      Collegiate   = "#d62728",
      Professional = "#9467bd"
    )
    
    plot <- plot_ly()
    for (lvl in levels) {
      level_df <- df %>% filter(Level == lvl)
      if (nrow(level_df) > 1) {
        d       <- density(level_df$Value)
        F_ecdf  <- ecdf(level_df$Value)
        # raw percentile
        pct_raw <- F_ecdf(d$x) * 100
        # if time metric, invert so that smaller values → higher percentile
        pct     <- if (is_timed) 100 - pct_raw else pct_raw
        
        clr <- level_colors[lvl]
        plot <- plot %>%
          add_trace(
            x           = d$x,
            y           = d$y,
            type        = "scatter",
            mode        = "lines",
            name        = lvl,
            fill        = "tozeroy",
            line        = list(width = 2, color = clr),
            fillcolor   = paste0(clr, "33"),
            customdata  = pct,
            hovertemplate = paste0(
              "<b>", lvl, "</b><br>",
              if (is_timed) "Time: %{x:.2f}s<br>" else "Value: %{x:.2f}<br>",
              "Percentile: %{customdata:.1f}%<extra></extra>"
            )
          )
      }
    }
    
    plot %>%
      layout(
        title    = paste("Percentile Distribution of", input$percentile_metric),
        xaxis    = list(title = if (is_timed) "Time (s)" else "Value"),
        yaxis    = list(title = "Density", visible = FALSE),
        legend   = list(x = 0.1, y = 0.9),
        hovermode  = 'closest',
        dragmode   = FALSE
      ) %>%
      config(
        displaylogo           = FALSE,
        scrollZoom            = FALSE,
        doubleClick           = FALSE,
        modeBarButtonsToRemove = c(
          "zoom2d","pan2d","select2d","lasso2d",
          "zoomIn2d","zoomOut2d","autoScale2d","resetScale2d",
          "hoverClosestCartesian","hoverCompareCartesian"
        ),
        toImageButtonOptions = list(
          format   = "png",
          filename = paste0(input$percentile_metric, "_PercentilePlot"),
          height   = 600,
          width    = 900,
          scale    = 2
        )
      )
  })
  
  # Report Downloader -------------------------------------------------------
  
  # UI: Populate athlete selector from client_data
  output$athlete_selector <- renderUI({
    athlete_choices <- client_data %>%
      distinct(Name) %>%
      arrange(Name) %>%
      pull(Name)
    
    pickerInput(
      inputId  = "report_athlete_name",
      label    = "Athlete:",
      choices  = athlete_choices,
      options  = list(
        `live-search`           = TRUE,
        `actions-box`           = FALSE,
        `size`                  = 5,             # show 5 rows then scroll
        `none-selected-text`    = 'Type to search…'
      ),
      width = "100%"
    )
  })
  
  # UI: Conditionally show download buttons based on data presence
  output$report_buttons <- renderUI({
    req(input$report_athlete_name, input$report_month, input$report_year)
    
    filtered_pitching <- pitching_data %>% 
      filter(!is.na(TaggedPitchType))
    
    name <- input$report_athlete_name
    month <- input$report_month
    year <- input$report_year
    
    has_data <- function(df) {
      any(format(df$Date, "%B") == month & format(df$Date, "%Y") == as.character(year) & df$Name == name)
    }
    
    buttons <- tagList()
    
    if (has_data(hitting_data)) {
      buttons <- tagAppendChild(buttons, downloadButton("download_hitting_report", "Hitting"))
    }
    if (has_data(filtered_pitching)) {
      buttons <- tagAppendChild(buttons, downloadButton("download_pitching_report", "Pitching"))
    }
    if (has_data(strength_data)) {
      buttons <- tagAppendChild(buttons, downloadButton("download_strength_report", "Strength"))
    }
    if (has_data(speed_data)) {
      buttons <- tagAppendChild(buttons, downloadButton("download_speed_report", "Speed"))
    }
    
    div(style = "display: flex; gap: 10px; justify-content: center; flex-wrap: wrap;", buttons)
  })
  
  # Predefine a custom waiter for report downloads
  report_waiter <- waiter::Waiter$new(
    html = tagList(
      spin_1(), 
      h3("Generating your report...", style = "margin-top: 20px; color: white;")
    ),
    color = "#000000cc"  # semi-transparent black
  )
  
  # Hitting Report
  output$download_hitting_report <- downloadHandler(
    filename = function() {
      paste0(input$report_athlete_name, " Hitting Report.png")
    },
    content = function(file) {
      report_waiter$show()
      on.exit(report_waiter$hide(), add = TRUE)
      
      report_path <- generate_hitting_report(
        client_data  = client_data,
        hitting_data = hitting_data,
        hittrax_data = hittrax_data,
        input$report_athlete_name,
        input$report_month,
        input$report_year
      )
      file.copy(report_path, file)
    }
  )
  
  # Pitching Report
  output$download_pitching_report <- downloadHandler(
    filename = function() {
      paste0(input$report_athlete_name, " Pitching Report.png")
    },
    content = function(file) {
      report_waiter$show()
      on.exit(report_waiter$hide(), add = TRUE)
      
      report_path <- generate_pitching_report(
        client_data    = client_data,
        pitching_data  = pitching_data,
        trackman_data  = trackman_data,
        input$report_athlete_name,
        input$report_month,
        input$report_year
      )
      file.copy(report_path, file)
    }
  )
  
  # Strength Report
  output$download_strength_report <- downloadHandler(
    filename = function() {
      paste0(input$report_athlete_name, " Strength Report.png")
    },
    content = function(file) {
      report_waiter$show()
      on.exit(report_waiter$hide(), add = TRUE)
      
      report_path <- generate_strength_report(
        client_data   = client_data,
        strength_data = strength_data,
        input$report_athlete_name,
        input$report_month,
        input$report_year
      )
      file.copy(report_path, file)
    }
  )
  
  # Speed Report
  output$download_speed_report <- downloadHandler(
    filename = function() {
      paste0(input$report_athlete_name, " Speed Report.png")
    },
    content = function(file) {
      report_waiter$show()
      on.exit(report_waiter$hide(), add = TRUE)
      
      report_path <- generate_speed_report(
        client_data = client_data,
        speed_data  = speed_data,
        input$report_athlete_name,
        input$report_month,
        input$report_year
      )
      file.copy(report_path, file)
    }
  )
}

shinyServer(server)
