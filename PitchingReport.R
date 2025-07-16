library(tidyverse)
library(ggtext)
library(ggpubr)
library(ggforce)
library(ggimage)
library(readxl)
library(magick)
library(scales)
library(showtext)
library(fmsb)
library(gt)

if (file.exists("www/good times rg.otf")) {
  font_add("Good Times", regular = "www/good times rg.otf")
} else {
  warning("Custom font not found; falling back to default.")
}
showtext_auto()

options(chromote.headless = "new")

generate_pitching_report <- function(client_data, pitching_data, trackman_data, athlete, month, year) { 
  
  report_img <- image_read_pdf("www/Pitching Report Template.pdf")
  
  pitcher_left <- "www/pitcher_left.png"
  pitcher_right <- "www/pitcher_right.png"
  
  pitch_colors <- c("ChangeUp"="#3d9be9", "Curveball"="#00FF00", "Cutter"="#FFFF00", "Fastball"="#FF0000", "Sinker"="#FFA500", "Slider"="#AD0AFD", "Splitter"="#FF69B4")
  
  # Home Plate ----
  x <- c(-8.5, -8, 0, 8, 8.5, -8.5)
  z <- c(0, 2, 4, 2, 0, 0)
  home_plate <- data.frame(x, z)
  
  # Strike Zone 
  x <- c(-10, 10, 10, -10, -10)
  z <- c(18, 18, 42, 42, 18)
  sz <- data.frame(x, z)
  
  # New Strike Zone
  x <- c(-10/3, -10/3, 10/3, 10/3, -10/3)
  z <- c(18, 42, 42, 18, 18)
  sz_2 <- data.frame(x, z)
  
  # New Strike Zone
  x <- c(-10, -10, 10, 10, -10)
  z <- c(26, 34, 34, 26, 26)
  sz_3 <- data.frame(x, z)
  
  ### Outer Zones 
  x <- c(-10, -14, -14, 0, 0, -10, -10)
  z <- c(30, 30, 46, 46, 42, 42, 30)
  kzone_11 <- data.frame(x, z)
  
  x <- c(-10, -14, -14, 0, 0, -10, -10)
  z <- c(30, 30, 14, 14, 18, 18, 30)
  kzone_13 <- data.frame(x, z)
  
  x <- c(10, 10, 14, 14, 0, 0, 10)
  z <- c(42, 30, 30, 46, 46, 42, 42)
  kzone_12 <- data.frame(x, z)
  
  x <- c(10, 10, 14, 14, 0, 0, 10)
  z <- c(18, 30, 30, 14, 14, 18, 18)
  kzone_14 <- data.frame(x, z)
  
  # Arrow icon function with inlined color logic
  arrow_icon <- function(value_string) {
    # Handle NA or blank strings
    if (is.na(value_string) || value_string == "") {
      return(html("<span>&mdash;</span>"))
    }
    
    # Try to split value and percent (e.g., "-2.5 (7)")
    percent_value_split <- strsplit(value_string, " \\(")[[1]]
    
    # If only one value, just print it
    if (length(percent_value_split) < 2) {
      return(html(paste0("<span>", value_string, "</span>")))
    }
    
    percent_change <- as.numeric(percent_value_split[1])
    value_change <- gsub("\\)", "", percent_value_split[2])
    
    # If percent_change is NA, print only value_change
    if (is.na(percent_change)) {
      return(html(paste0("<span>", value_change, "%</span>")))
    }
    
    # Directional arrows and colors
    if (percent_change > 0.01) {
      return(html(paste0("<span style='color:green;'>&#9650; ", percent_change, "% (", value_change, ")</span>")))
    } else if (percent_change < -0.01) {
      return(html(paste0("<span style='color:red;'>&#9660; ", percent_change, "% (", value_change, ")</span>")))
    } else {
      return(html(paste0("<span>&#8212; ", percent_change, "% (", value_change, ")</span>")))
    }
  }
  
  # Helper function to add the ordinal suffix
  ordinal_suffix <- function(n) {
    if (length(n) == 0 || is.na(n)) {
      return(NA)
    }
    if (n %% 100 %in% c(11, 12, 13)) {
      return(paste0(n, "th"))
    }
    last_digit <- n %% 10
    suffix <- switch(as.character(last_digit), 
                     "1" = "st", 
                     "2" = "nd", 
                     "3" = "rd", 
                     "0" = "th",
                     "th")
    return(paste0(n, suffix))
  }
  
  
  month_num <- match(month, month.name)
  cutoff_date <- ceiling_date(ymd(paste(year, month_num, "01", sep = "-")), unit = "month")
  
  pitching_data <- pitching_data %>% 
    filter(Level %in% c("L1", "L2", "L3", "Collegiate", "Professional"), Date < as.Date(cutoff_date))
  
  trackman_data <- trackman_data %>% 
    filter(Date < as.Date(cutoff_date))
  
  # Trackman Data
  summarized_trackman <- trackman_data %>% 
    filter(!is.na(TaggedPitchType), !is.na(Gender), !is.na(Level)) %>% 
    mutate(ZoneCheck = case_when(between(PlateLocHeight, 1.4833, 3.8167) & between(PlateLocSide, -0.9167, 0.9167) ~ TRUE, TRUE ~ FALSE)) %>% 
    group_by(Name, Level, Month, Year, TaggedPitchType) %>% 
    summarise(
      Pitch_Count = n(),  # Count the number of pitches for each TaggedPitchType
      `K%` = round(mean(ZoneCheck, na.rm = TRUE) * 100, 1),
      Avg_RelSpeed = round(mean(RelSpeed, na.rm = TRUE), 1),
      Max_RelSpeed = round(max(RelSpeed, na.rm = TRUE), 1),
      RelHeight = round(mean(RelHeight, na.rm = TRUE), 3),
      RelSide = round(mean(RelSide, na.rm = TRUE), 3),
      Extension = round(mean(Extension, na.rm = TRUE), 1),
      InducedVertBreak = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      HorzBreak = round(mean(HorzBreak, na.rm = TRUE), 1),
      SpinRate = round(mean(SpinRate, na.rm = TRUE)),
      PlateLocHeight = round(mean(PlateLocHeight, na.rm = TRUE), 1),
      PlateLocSide = round(mean(PlateLocSide, na.rm = TRUE), 1),
      SpinAxis3dSpinEfficiency = round(mean(SpinAxis3dSpinEfficiency, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>% 
    group_by(Name, Month, Year) %>% 
    mutate(
      Total_Pitches = sum(Pitch_Count),  # Total number of pitches thrown by athlete in that month
      Pitch_Usage_Pct = round((Pitch_Count / Total_Pitches) * 100, 1)  # Percent usage of each TaggedPitchType
    ) %>% 
    ungroup()
  
  fastball_percentiles <- summarized_trackman %>%
    filter(TaggedPitchType %in% c("Fastball", "Sinker")) %>%
    mutate(PitchPriority = if_else(TaggedPitchType == "Fastball", 1, 2)) %>% 
    group_by(Name, Month, Year) %>%
    slice_min(order_by = PitchPriority, with_ties = FALSE) %>%
    ungroup() %>%
    group_by(Level) %>%
    mutate(
      MaxRelSpeed_Percentile = case_when(
        Level == "Collegiate" ~ round(pnorm((Max_RelSpeed - 90.0) / 4.15) * 100),
        Level == "Professional" ~ round(pnorm((Max_RelSpeed - 96.4)  /  2.72) * 100),
        TRUE ~ round(percent_rank(Max_RelSpeed) * 100)
      ),
      AvgRelSpeed_Percentile = case_when(
        Level == "Collegiate" ~ round(pnorm((Avg_RelSpeed - 87.4) / 3.88) * 100),
        Level == "Professional" ~ round(pnorm((Avg_RelSpeed - 93.96)  /  2.46)  * 100),
        TRUE ~ round(percent_rank(Avg_RelSpeed) * 100)
      ),
      Extension_Percentile = case_when(
        Level == "Collegiate" ~ round(pnorm((Extension - 5.88) / 0.510) * 100),
        Level == "Professional" ~ round(pnorm((Extension - 6.4)  /  0.43)* 100),
        TRUE ~ round(percent_rank(Extension) * 100)
      )
    ) %>%
    ungroup()
  
  summarized_velocity <- fastball_percentiles %>% 
    group_by(Name, Month, Year, Level) %>% 
    summarise(Value = Max_RelSpeed,
              Percentile = MaxRelSpeed_Percentile,
              .groups = "drop") %>%
    mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>%
    arrange(Date) %>%
    group_by(Month, Year, Level) %>% 
    mutate(Rank = rank(-Value, ties.method = "min"),
           Total = n()) %>%
    group_by(Name) %>%
    mutate(
      MonthValueChange = case_when(
        is.na(Value) | is.na(lag(Value)) ~ NA_character_,
        TRUE ~ sprintf("%+.1f", round(Value - lag(Value), 1))
      ),
      MonthPercentChange = if_else(
        is.na(Value) | is.na(lag(Value)), NA_real_,
        round((Value - lag(Value)) / lag(Value) * 100, 1)
      ),
      CareerValueChange = case_when(
        is.na(Value) | is.na(first(Value)) ~ NA_character_,
        TRUE ~ sprintf("%+.1f", round(Value - first(Value), 1))
      ),
      CareerPercentChange = if_else(
        is.na(Value) | is.na(first(Value)), NA_real_,
        round((Value - first(Value)) / first(Value) * 100, 1)
      )
    ) %>%
    ungroup()
  
  # Armcare Data
  
  summarized_armcare <- pitching_data %>% 
    filter(!is.na(arm_score), !is.na(Gender), !is.na(Level)) %>% 
    group_by(Name, Level, Gender, Month, Year) %>% 
    summarise(
      arm_score = round(mean(arm_score, na.rm = TRUE), 1),
      total_strength = round(mean(total_strength, na.rm = TRUE), 1),
      shoulder_balance = round(mean(shoulder_balance, na.rm = TRUE), 1),
      velo = round(mean(velo, na.rm = TRUE), 1),
      svr = round(mean(svr, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>% 
    group_by(Level, Gender) %>% 
    mutate(
      armStrength_Percentile = round(percent_rank(total_strength) * 100)
    ) %>% 
    ungroup()
  
  summarized_strength <- summarized_armcare %>% 
    group_by(Name, Gender, Month, Year, Level) %>% 
    summarise(Value = total_strength,
              Percentile = armStrength_Percentile,
              .groups = "drop") %>%
    mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>%
    arrange(Date) %>%
    group_by(Month, Year, Level) %>% 
    mutate(
      Rank = rank(-Value, ties.method = "min"),
      Total = n()
    ) %>%
    group_by(Name) %>%
    mutate(
      MonthValueChange = case_when(
        is.na(Value) | is.na(lag(Value)) ~ NA_character_,
        TRUE ~ sprintf("%+.1f", round(Value - lag(Value), 1))
      ),
      MonthPercentChange = if_else(
        is.na(Value) | is.na(lag(Value)), NA_real_,
        round((Value - lag(Value)) / lag(Value) * 100, 1)
      ),
      CareerValueChange = case_when(
        is.na(Value) | is.na(first(Value)) ~ NA_character_,
        TRUE ~ sprintf("%+.1f", round(Value - first(Value), 1))
      ),
      CareerPercentChange = if_else(
        is.na(Value) | is.na(first(Value)), NA_real_,
        round((Value - first(Value)) / first(Value) * 100, 1)
      )
    ) %>%
    ungroup()
  
  # Define a temp directory for this athlete
  athlete_dir <- file.path(tempdir(), athlete)
  if (!dir.exists(athlete_dir)) {
    dir.create(athlete_dir, recursive = TRUE)
  }
  
  # Load and process the Attendance data
  attendanceData <- pitching_data %>% 
    filter(!is.na(Attendance)) %>% 
    group_by(Name, Month, Year) %>% 
    summarise(Attendance = mean(Attendance), .groups = 'drop') %>% 
    filter(Month == month, Year == year)
  
  attendance_plot_data <- attendanceData %>%
    filter(Name == athlete) %>% 
    mutate(`Total Weeks` = 4.345, # Adjust this number based on the exact number of weeks in the 1-month period
           `Attendance Score` = round(Attendance / `Total Weeks`, digits = 1))
  
  attendance_score <- max(attendance_plot_data$`Attendance Score`)
  
  # Revised get_color function to handle vector inputs
  get_color <- function(scores) {
    sapply(scores, function(score) {
      if (is.na(score)) {
        return(NA)
      } else if (score < 1) {
        return("#FF0000")
      } else if (score >= 1 & score < 2) {
        return("#FFA500")
      } else {
        return("#008000")
      }
    })
  }
  
  # Plotting
  attendance_plot <- ggplot(attendance_plot_data, aes(x = Name, y = `Attendance Score`)) +
    # Background bars
    geom_col(aes(y = 3), alpha = 0.75, fill = "gray30", color = "black") +
    
    # Foreground colored bars with black borders
    geom_col(aes(fill = get_color(`Attendance Score`)), color = "black") +
    
    # Score text
    geom_text(aes(y = 1.5, label = paste(attendance_score)), size = 12, fontface = "bold", color = "white") +
    
    # Orientation and theme
    coord_flip() +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    ) +
    scale_fill_identity()
  
  attendance_path <- file.path(athlete_dir, paste0(athlete, "_attendancePlot.png"))
  ggsave(attendance_path,
         plot   = attendance_plot,
         width  = 2.15, height = 0.65,
         units  = "in",
         dpi    = 175)
  
  # read & composite back into the same object
  attendancePlot <- image_read(attendance_path)
  report_img     <- image_composite(report_img, attendancePlot, offset = "+1400+300")
  
  # 3. Immediately free the temp image before doing the next step
  rm(attendancePlot)
  gc()
  
  athlete_info <- client_data %>%
    filter(Name == athlete) %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.) | . == "N/A", "--", .)))
  
  # Generate ggplot
  player_profile <- ggplot() +
    xlim(0, 1) +
    ylim(0, 1) +
    # Athlete's Name (Top line)
    annotate("text", x = 0.5, y = 0.9, label = athlete_info$Name, 
             hjust = 0.5, vjust = 1, size = 18, color = "black", family = "Good Times") +
    
    # High School and Graduating Class (Middle line)
    annotate("text", x = 0.5, y = 0.6, 
             label = paste0(athlete_info$`Position (Baseball/Softball)`, " | ", 
                            athlete_info$Height, " ", athlete_info$Weight, "lbs | Age: ", athlete_info$Age), 
             hjust = 0.5, vjust = 1, size = 14, color = "black") +
    
    # Age, Position, and HT/WT (Bottom line)
    annotate("text", x = 0.34, y = 0.225, 
             label = paste0("Avg Weekly Attendance:"), 
             hjust = 0.5, vjust = 1, size = 14, color = "black") +
    
    theme_void()
  
  # 1. Render & save the profile plot
  profile_path <- file.path(athlete_dir, paste0(athlete, "_playerProfile.png"))
  ggsave(profile_path,
         plot   = player_profile,
         width  = 8, height = 2.5,
         units  = "in",
         dpi    = 150)
  
  # 2. Read & composite into report_img (no new HittingReport2 variable)
  playerProfileImg <- image_read(profile_path)
  report_img       <- image_composite(report_img, playerProfileImg, offset = "+700+50")
  
  # 3. Free memory before the next overlay
  rm(playerProfileImg)
  gc()
  
  pitchStats <- summarized_trackman %>%
    filter(Name == athlete, Month == month, Year == year) %>% 
    select(TaggedPitchType, Pitch_Usage_Pct, `K%`, Avg_RelSpeed, Max_RelSpeed, InducedVertBreak, HorzBreak, SpinRate) %>% 
    arrange(desc(Max_RelSpeed))
  
  colnames(pitchStats) <- c('Pitch Type', '%',  'K%', 'Avg Velo (mph)', 'Max Velo (mph)', 'IVB (in)', 'HB (in)', 'Spin (rpm)') 
  
  gt_table <- pitchStats %>% 
    gt(rowname_col = "Pitch Type") %>% 
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    tab_options(
      column_labels.font.weight = "bold",
      table.background.color = "black",
      table.font.color = "white"
    ) %>%
    tab_style(
      style = cell_text(size = px(18), color = "#3d9be9"),
      locations = cells_column_labels()
    ) %>% 
    tab_style(
      style = cell_text(size = px(18), weight = "bold"),
      locations = cells_stub()
    ) %>%
    tab_options(
      table.border.top.style = "hidden",
      table.border.left.style = "solid",
      table.border.right.style = "solid",
      column_labels.border.lr.style = "hidden",
      heading.border.lr.style = "hidden",
      heading.border.bottom.style = "hidden",
      table_body.vlines.style = "solid"
    ) %>% 
    opt_table_font(font = "Helvetica") %>% 
    cols_width(
      `Pitch Type` ~ px(125),
      `%` ~ px(50),
      `K%` ~ px(50),
      everything() ~ px(105)
    ) %>% 
    sub_missing(
      columns = everything()
    )
  
  apply_tab_style_if_exists <- function(gt_table, pitch_type, color) {
    if (pitch_type %in% pitchStats$`Pitch Type`) {
      # Apply background color
      gt_table <- gt_table %>%
        tab_style(
          style = cell_fill(color = color), 
          locations = cells_stub(rows = pitch_type)
        )
      
      # Apply text color for Curveball and Cutter
      if (pitch_type %in% c("Curveball", "Cutter", "Sinker", "Splitter")) {
        gt_table <- gt_table %>%
          tab_style(
            style = cell_text(color = "black"), 
            locations = cells_stub(rows = pitch_type)
          )
      }
    }
    return(gt_table)
  }
  
  pitch_types_colors <- list(
    "ChangeUp" = "#007FFF",
    "Curveball" = "#00FF00",
    "Cutter" = "#FFFF00",
    "Fastball" = "#FF0000",
    "Sinker" = "#FFA500",
    "Slider" = "#AD0AFD",
    "Splitter" = "#FF69B4"
  )
  
  for (pitch_type in names(pitch_types_colors)) {
    gt_table <- apply_tab_style_if_exists(gt_table, pitch_type, pitch_types_colors[[pitch_type]])
  }
  
  # 1. Render & save the pitching summary table
  pitching_summary_path <- file.path(athlete_dir, paste0(athlete, "-pitchingSummary.png"))
  gtsave(gt_table,
         file    = pitching_summary_path,
         vwidth  = 1200,
         expand  = 5)
  
  # 2. Read, clean up, and composite into the existing report image in-place
  pitching_summary_img <- image_read(pitching_summary_path) %>%
    image_transparent(color = "black") %>%
    image_trim()
  report_img           <- image_composite(report_img, pitching_summary_img, offset = "+140+675")
  
  # 3. Free memory immediately
  rm(pitching_summary_img)
  gc()
  
  # Filter data for the athlete and exercise
  filtered_velocity <- summarized_velocity %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  velocity_check <- filtered_velocity %>%
    distinct(Month, Year) %>%
    nrow()
  
  min_velocity <- filtered_velocity %>%
    summarise(minValue = min(Value, na.rm = TRUE)) %>%
    pull(minValue)
  
  max_velocity <- filtered_velocity %>%
    summarise(maxValue = max(Value, na.rm = TRUE)) %>%
    pull(maxValue)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_velocity$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  if (length(unique_months) == 2) {
    breaks_option <- unique_months
  } else {
    breaks_option <- waiver() # Default ggplot behavior
  }
  
  # Filter data for May 2024
  current_velocity <- filtered_velocity %>% 
    filter(Month == month & Year == year)
  
  restructured_velocity <- current_velocity %>% 
    mutate(
      `Month:` = paste0(MonthPercentChange, " (", MonthValueChange, ")"),
      `Career:` = paste0(CareerPercentChange, " (", CareerValueChange, ")")
    ) %>%
    select(`Month:`, `Career:`) %>% 
    pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
  
  if (any(!is.na(current_velocity$MonthValueChange))) {
    
    # Create the gt table with styling and apply arrow icons and value changes
    velocity_gt <- restructured_velocity %>% 
      gt() %>%
      rm_header() %>% 
      cols_align(align = "center", columns = everything()) %>%
      tab_options(
        table.background.color = "black",
        table.font.color = "white",
        column_labels.hidden = TRUE,
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        data_row.padding = px(4)
      ) %>%
      opt_table_font(font = "Helvetica") %>% 
      # Apply arrow icons and value changes
      text_transform(
        locations = cells_body(columns = Value),
        fn = function(x) {
          map_chr(x, ~arrow_icon(.x))
        }
      )
    
    hjust_value <- 0.8
    
  } else {
    
    # Create a simplified gt table without text transformation
    velocity_gt <- tibble(Metric = "", Value = "") %>%
      gt() %>%
      cols_align(align = "center", columns = everything()) %>%
      tab_options(
        table.background.color = "black",
        table.font.color = "white",
        column_labels.hidden = TRUE,
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        data_row.padding = px(4)
      ) %>% 
      opt_table_font(font = "Helvetica")
    
    hjust_value <- -7.50
    
  }
  
  velocity_value <- current_velocity$Value
  velocity_percentile_numeric <- current_velocity$Percentile
  velocity_percentile <- ordinal_suffix(velocity_percentile_numeric)
  velocity_rank <- paste0(current_velocity$Rank, " out of ", current_velocity$Total)
  
  velocity_color <- col_numeric(
    palette = c("#FF0000", "#FFFF00", "#008000"), 
    domain = c(0, 100)  # Scale from 0 to 100 percentiles
  )(velocity_percentile_numeric)
  
  max_point_velo <- filtered_velocity[which.max(filtered_velocity$Value), ]
  
  velocity_plot <- ggplot(filtered_velocity, aes(x = Date, y = Value)) +
    geom_line(color = "#0099f9", linewidth = 2) +
    geom_point(color = "#0099f9", size = 3) +
    geom_point(data = max_point_velo, aes(x = Date, y = Value), color = "#FF0000", size = 5) +
    geom_richtext(
      data = data.frame(x = max_point_velo$Date, y = max_point_velo$Value),
      aes(x = x, y = y, label = paste0("<b style='color:#FF0000;'>", max_point_velo$Value, "</b>")),
      fill = "black", hjust = 0.5, vjust = -0.5, size = 6
    ) +
    labs(
      title = "Max Fastball\n",
      subtitle = paste0(
        "<span style='font-size:50px;'>", velocity_value, "</span> <span style='font-size:25px;'>mph</span><br><br>",
        "<span style='font-size:50px; color:", velocity_color, ";'>", velocity_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
      ),
      tag = paste0("Rank: ", velocity_rank),
      x = NULL,
      y = NULL
    ) +
    scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
    scale_y_continuous(limits = c(min_velocity - 2, max_velocity + 2)) +
    theme_void() +
    theme(
      plot.title = element_text(color = "white", size = 26),
      plot.subtitle = element_markdown(color = "white"), # allows markdown for dynamic color
      plot.tag = element_text(color = "white", size = 22),
      plot.tag.position = c(0.8, 0.71), 
      axis.text = element_text(color = "lightgrey")
    )
  
  # 1. Composite your velocity GT table in-place
  maxEV_path <- file.path(athlete_dir, paste0(athlete, "-maxEV_gt.png"))
  gtsave(velocity_gt, file = maxEV_path, expand = -1)
  
  maxEV_img  <- image_read(maxEV_path) %>%
    image_transparent(color = "black")
  report_img <- image_composite(report_img, maxEV_img, offset = "+540+1480")
  
  rm(maxEV_img)
  gc()
  
  
  # 2. Composite your velocity plot in-place
  velocity_plot_path <- file.path(athlete_dir, paste0(athlete, "-velocityPlot.png"))
  ggsave(velocity_plot_path,
         plot   = velocity_plot,
         width  = 4.9, height = 4.75,
         units  = "in",
         dpi    = 150)
  
  velocity_plot_img <- image_read(velocity_plot_path)
  report_img        <- image_composite(report_img, velocity_plot_img, offset = "+175+1525")
  
  rm(velocity_plot_img)
  gc()
  
  pitch_data <- trackman_data %>% 
    filter(Name == athlete, Month == month, Year == year)
  
  meanLocation <- summarized_trackman %>% 
    filter(Name == athlete, Month == month, Year == year) %>% 
    group_by(TaggedPitchType) %>% 
    summarise(`Avg Location Side` = mean(PlateLocSide, na.rm = TRUE),
              `Avg Location Height` = mean(PlateLocHeight, na.rm = TRUE))
  
  strikezone_plot <- ggplot() +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "lightgrey", color = "grey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "white") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "white") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "white") +
    geom_path(kzone_11, mapping = aes(x, z), lty = 1, color = "white") +
    geom_path(kzone_12, mapping = aes(x, z), lty = 1, color = "white") +
    geom_path(kzone_13, mapping = aes(x, z), lty = 1, color = "white") +
    geom_path(kzone_14, mapping = aes(x, z), lty = 1, color = "white") +
    geom_jitter(pitch_data, mapping = aes(x = PlateLocSide * 12, y = PlateLocHeight * 12, fill = TaggedPitchType), shape = 21, size = 2.5, alpha = 0.35, stroke = 0) +
    geom_point(meanLocation, mapping = aes(x = `Avg Location Side` * 12, y = `Avg Location Height` * 12, fill = TaggedPitchType), shape = 21, size = 5) +
    coord_equal(xlim = c(-24, 24), ylim = c(-6, 60)) +
    scale_fill_manual(values = pitch_colors) +
    theme_void() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  meanMovement <- summarized_trackman %>% 
    filter(Name == athlete, Month == month, Year == year) %>% 
    group_by(TaggedPitchType) %>% 
    summarise(`Avg HorzBreak` = mean(HorzBreak, na.rm = TRUE),
              `Avg VertBreak` = mean(InducedVertBreak, na.rm = TRUE))
  
  movement_plot <- ggplot(pitch_data, aes(x = HorzBreak, y = InducedVertBreak, fill = TaggedPitchType)) +
    geom_jitter(shape = 21, size = 3, alpha = 0.35, stroke = 0) +
    geom_point(data = meanMovement, aes(x = `Avg HorzBreak`, y = `Avg VertBreak`, fill = TaggedPitchType), shape = 21, size = 7, color = "black") +
    coord_cartesian(xlim = c(-30, 30), ylim = c(-30, 30)) +
    geom_vline(mapping = aes(xintercept = 0), linetype = 2, alpha = 0.5, color = "white")+
    geom_hline(mapping = aes(yintercept = 0), linetype = 2, alpha = 0.5, color = "white")+
    scale_fill_manual(values = pitch_colors) +
    labs(caption = "* Pitchers POV") +
    theme_void() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_text(color = "white", size = 15),
          plot.caption = element_text(color = "white", size = 12))
  
  meanRelease <- summarized_trackman %>% 
    filter(Name == athlete, Month == month, Year == year) %>% 
    group_by(TaggedPitchType) %>% 
    summarise(`Avg RelSide` = mean(RelSide, na.rm = TRUE),
              `Avg RelHeight` = mean(RelHeight, na.rm = TRUE))
  
  avg_rel_side <- mean(meanRelease$`Avg RelSide`, na.rm = TRUE)
  
  if (avg_rel_side < 0) {
    arm_x <- -0.1
    arm_y <- 4.4
    pitcher_x <- -2.5
    pitcher_y <- 4.8
    image_path <- pitcher_left
  } else {
    arm_x <- 0.275
    arm_y <- 4.4
    pitcher_x <- -1.4
    pitcher_y <- 4.8
    image_path <- pitcher_right
  }
  
  release_plot <- ggplot(meanRelease, aes(x = `Avg RelSide`, y = `Avg RelHeight`, fill = TaggedPitchType)) +
    geom_ellipse(aes(x0 = 0, y0 = -1.75, a = 4, b = 2.5, angle = 0), fill = "grey", color = "darkgrey") + 
    geom_segment(aes(x = -0.5, y = 0.65, xend = 0.5, yend = 0.65), color = "white", linewidth = 1) +
    geom_image(image = image_path, size = 1.35, x = pitcher_x, y = pitcher_y) +
    geom_segment(x= arm_x, y = arm_y, 
                 xend = meanRelease$`Avg RelSide`, 
                 yend = meanRelease$`Avg RelHeight` , 
                 linewidth = 6, color = "#6892a2", alpha = .5) +
    geom_point(shape = 21, size = 5) +
    coord_cartesian(xlim = c(-4, 4), ylim = c(0, 8), expand = c(0)) +
    scale_fill_manual(values = pitch_colors) +
    labs(caption = "* Pitchers POV") +
    theme_void() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(color = "white", size = 15),
          plot.caption = element_text(color = "white", size = 12))
  
  # 1. Composite your strike zone plot in-place
  strikezone_plot_path <- file.path(athlete_dir, paste0(athlete, " - strikezonePlot.png"))
  ggsave(strikezone_plot_path,
         plot   = strikezone_plot,
         width  = 4, height = 4,
         units  = "in",
         dpi    = 200)
  
  strikezone_img <- image_read(strikezone_plot_path)
  report_img     <- image_composite(report_img, strikezone_img, offset = "+1710+625")
  rm(strikezone_img); gc()
  
  
  # 2. Composite your movement plot in-place
  movement_plot_path <- file.path(athlete_dir, paste0(athlete, " - movementPlot.png"))
  ggsave(movement_plot_path,
         plot   = movement_plot,
         width  = 4, height = 4.15,
         units  = "in",
         dpi    = 150)
  
  movement_img <- image_read(movement_plot_path)
  report_img   <- image_composite(report_img, movement_img, offset = "+1015+1635")
  rm(movement_img); gc()
  
  
  # 3. Composite your release plot in-place
  release_plot_path <- file.path(athlete_dir, paste0(athlete, " - releasePlot.png"))
  ggsave(release_plot_path,
         plot   = release_plot,
         width  = 4, height = 4.15,
         units  = "in",
         dpi    = 150)
  
  release_img <- image_read(release_plot_path)
  report_img  <- image_composite(report_img, release_img, offset = "+1800+1635")
  rm(release_img); gc()
  
  
  # Filter data for the athlete and exercise
  filtered_strength <- summarized_strength %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  strength_check <- filtered_strength %>%
    distinct(Month, Year) %>%
    nrow()
  
  min_strength <- filtered_strength %>%
    summarise(minValue = min(Value, na.rm = TRUE)) %>%
    pull(minValue)
  
  max_strength <- filtered_strength %>%
    summarise(maxValue = max(Value, na.rm = TRUE)) %>%
    pull(maxValue)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_strength$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  if (length(unique_months) == 2) {
    breaks_option <- unique_months
  } else {
    breaks_option <- waiver() # Default ggplot behavior
  }
  
  # Filter data for May 2024
  current_strength <- filtered_strength %>% 
    filter(Month == month & Year == year)
  
  last_strength <- filtered_strength %>% 
    filter(Date == max(Date))
  
  last_strength_value <- last_strength$Value
  
  strength_value <- current_strength$Value
  strength_percentile_numeric <- current_strength$Percentile
  strength_percentile <- ordinal_suffix(strength_percentile_numeric)
  strength_rank <- paste0(current_strength$Rank, " out of ", current_strength$Total)
  
  strength_color <- col_numeric(
    palette = c("#FF0000", "#FFFF00", "#008000"), 
    domain = c(0, 100)  # Scale from 0 to 100 percentiles
  )(strength_percentile_numeric)
  
  max_point_strength <- filtered_strength[which.max(filtered_strength$Value), ]
  
  # Check for more than 1 month of data
  if (strength_check > 1) {
    if (nrow(current_strength) > 0) {
      # Case: More than 1 month and May 2024 data available
      
      restructured_strength <- current_strength %>% 
        mutate(
          `Month:` = paste0(MonthPercentChange, " (", MonthValueChange, ")"),
          `Career:` = paste0(CareerPercentChange, " (", CareerValueChange, ")")
        ) %>%
        select(`Month:`, `Career:`) %>% 
        pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      strength_gt <- restructured_strength %>% 
        gt() %>%
        rm_header() %>% 
        cols_align(align = "center", columns = everything()) %>%
        tab_options(
          table.background.color = "black",
          table.font.color = "white",
          column_labels.hidden = TRUE,
          table.border.top.style = "hidden",
          table.border.bottom.style = "hidden",
          data_row.padding = px(4)
        ) %>%
        opt_table_font(font = "Helvetica") %>% 
        # Apply arrow icons and value changes
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(.x))
          }
        )
      
      # Create plot
      strength_plot <- ggplot(filtered_strength, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        geom_point(data = max_point_strength, aes(x = Date, y = Value), color = "#FF0000", size = 5) +
        geom_richtext(
          data = data.frame(x = max_point_strength$Date, y = max_point_strength$Value),
          aes(x = x, y = y, label = paste0("<b style='color:#FF0000;'>", max_point_strength$Value, "</b>")),
          fill = "black", hjust = 0.5, vjust = -0.5, size = 6
        ) +
        labs(
          title = "Arm Strength\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", strength_value, "</span> <span style='font-size:25px;'>lbs</span><br><br>",
            "<span style='font-size:50px; color:", strength_color, ";'>", strength_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", strength_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_strength - 10, max_strength + 10)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.8, 0.71), 
          axis.text = element_text(color = "lightgrey")
        )
      
    } else {
      # Case: More than 1 month but no May 2024 data
      strength_plot <- ggplot(filtered_strength, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        geom_point(data = max_point_strength, aes(x = Date, y = Value), color = "#FF0000", size = 5) +
        geom_richtext(
          data = data.frame(x = max_point_strength$Date, y = max_point_strength$Value),
          aes(x = x, y = y, label = paste0("<b style='color:#FF0000;'>", max_point_strength$Value, "</b>")),
          fill = "black", hjust = 0.5, vjust = -0.5, size = 6
        ) +
        labs(
          title = "Arm Strength\n",
          subtitle = "No Recent Data\n\n",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_strength - 10, max_strength + 10)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      restructured_strength <- last_strength_value %>% 
        mutate(
          `Career:` = paste0(CareerPercentChange, " (", CareerValueChange, ")")
        ) %>%
        select(`Career:`) %>% 
        pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      strength_gt <- restructured_strength %>% 
        gt() %>%
        rm_header() %>% 
        cols_align(align = "center", columns = everything()) %>%
        tab_options(
          table.background.color = "black",
          table.font.color = "white",
          column_labels.hidden = TRUE,
          table.border.top.style = "hidden",
          table.border.bottom.style = "hidden",
          data_row.padding = px(4)
        ) %>%
        opt_table_font(font = "Helvetica") %>% 
        # Apply arrow icons and value changes
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(.x))
          }
        )
    }
    
  } else if (strength_check == 1) {
    # Case: Only 1 month of data
    if (nrow(current_strength) > 0) {
      # Case: Only 1 month, and it's May 2024
      strength_plot <- ggplot(filtered_strength, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Arm Strength\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", strength_value, "</span> <span style='font-size:25px;'>lbs</span><br><br>",
            "<span style='font-size:50px; color:", strength_color, ";'>", strength_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", strength_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_strength - 10, max_strength + 10)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.8, 0.71), 
          axis.text = element_text(color = "lightgrey")
        )
      
      strength_gt <- tibble(Metric = "", Value = "") %>%
        gt() %>%
        cols_align(align = "center", columns = everything()) %>%
        tab_options(
          table.background.color = "black",
          table.font.color = "white",
          column_labels.hidden = TRUE,
          table.border.top.style = "hidden",
          table.border.bottom.style = "hidden",
          data_row.padding = px(4)
        ) %>% 
        opt_table_font(font = "Helvetica")
      
    } else {
      # Case: Only 1 month but not May 2024
      strength_plot <- ggplot(filtered_strength, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "   Arm Strength\n",
          subtitle = "    No Recent Data",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_strength - 10, max_strength + 10)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      strength_gt <- tibble(Metric = "", Value = "") %>%
        gt() %>%
        cols_align(align = "center", columns = everything()) %>%
        tab_options(
          table.background.color = "black",
          table.font.color = "white",
          column_labels.hidden = TRUE,
          table.border.top.style = "hidden",
          table.border.bottom.style = "hidden",
          data_row.padding = px(4)
        ) %>% 
        opt_table_font(font = "Helvetica")
    }
    
  } else {
    # Case: No data
    strength_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Armcare Data Available", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   Arm Strength\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26),
        plot.subtitle = element_blank()
      )
    
    strength_gt <- tibble(Metric = "", Value = "") %>%
      gt() %>%
      cols_align(align = "center", columns = everything()) %>%
      tab_options(
        table.background.color = "black",
        table.font.color = "white",
        column_labels.hidden = TRUE,
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        data_row.padding = px(4)
      ) %>% 
      opt_table_font(font = "Helvetica")
  }
  
  # 1. Composite your strength GT table in-place
  strength_gt_path <- file.path(athlete_dir, paste0(athlete, "-strength_gt.png"))
  gtsave(strength_gt, file = strength_gt_path, expand = -1)
  
  strength_gt_img <- image_read(strength_gt_path) %>%
    image_transparent(color = "black")
  report_img      <- image_composite(report_img, strength_gt_img, offset = "+540+2400")
  
  rm(strength_gt_img)
  gc()
  
  
  # 2. Composite your strength plot in-place
  strength_plot_path <- file.path(athlete_dir, paste0(athlete, "-strength_plot.png"))
  ggsave(strength_plot_path,
         plot   = strength_plot,
         width  = 4.9, height = 4.75,
         units  = "in",
         dpi    = 150)
  
  strength_plot_img <- image_read(strength_plot_path)
  report_img        <- image_composite(report_img, strength_plot_img, offset = "+175+2440")
  
  rm(strength_plot_img)
  gc()
  
  
  filtered_armcare <- summarized_armcare %>% 
    filter(Name == athlete, Month == month, Year == year)
  
  if (nrow(filtered_armcare) == 0 || is.na(filtered_armcare$svr) || is.na(filtered_armcare$Level)) {
    SVR_plot <- ggplot() +
      geom_text(aes(x = 1.95, y = -0.35, label = "No Current Data"),  # Center text below the plot
                size = 10, color = "white", family = "Good Times") +
      theme_void()
  } else {
    
    SVR_value <- filtered_armcare$svr
    athlete_level <- filtered_armcare$Level
    
    # Set threshold values based on level
    if (athlete_level %in% c("L1", "L2")) {
      red_threshold <- 1.1
      yellow_threshold <- 1.3
    } else {
      red_threshold <- 1.4
      yellow_threshold <- 1.6
    }
    
    # Determine label color and text
    if (SVR_value < red_threshold) {
      SVR_label_color <- "#FF0000"
      SVR_label_text <- "Warning"
    } else if (SVR_value >= red_threshold & SVR_value <= yellow_threshold) {
      SVR_label_color <- "#FFFF00"
      SVR_label_text <- "Watch"
    } else {
      SVR_label_color <- "#008000"
      SVR_label_text <- "Optimal"
    }
    
    # Create a data frame for the color ranges dynamically
    svr_color_ranges <- data.frame(
      xmin = c(0.8, red_threshold, yellow_threshold),
      xmax = c(red_threshold - 0.01, yellow_threshold - 0.01, 3),
      ymin = c(0, 0, 0),
      ymax = c(0.25, 0.25, 0.25),
      fill = c("#FF0000", "#FFFF00", "#008000")  # Red, Yellow, Green
    )
    
    # Create the plot
    SVR_plot <- ggplot() +
      geom_rect(data = svr_color_ranges, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)) +
      geom_text(aes(x = SVR_value, y = 0.6, label = paste(round(SVR_value, digits = 2))),
                size = 12, color = SVR_label_color, family = "Good Times") +
      geom_segment(aes(x = SVR_value, xend = SVR_value, y = -0.10, yend = 0.30), 
                   color = "white", linewidth = 2) +
      scale_fill_identity() +
      theme_void() +
      theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
      coord_cartesian(ylim = c(-1, 1))
  }
  
  
  # Save & composite your SVR plot in-place
  SVR_plot_path <- file.path(athlete_dir, paste0(athlete, " - SVR_average.png"))
  ggsave(SVR_plot_path,
         plot   = SVR_plot,
         width  = 4, height = 3.5,
         units  = "in",
         dpi    = 150)
  
  svr_img    <- image_read(SVR_plot_path)
  report_img <- image_composite(report_img, svr_img, offset = "+1035+2450")
  
  # Free the temporary image from memory
  rm(svr_img)
  gc()
  
  
  if (nrow(filtered_armcare) == 0 || is.na(filtered_armcare$svr) || is.na(filtered_armcare$Level)) {
    SVR_description_plot <- ggplot() +
      theme_void()
  } else {
    # Add dynamic description based on SVR_label_text
    SVR_description <- switch(SVR_label_text,
                              "Optimal" = "Your arm strength effectively supports\nyour velocity. Continue throwing at high\nintensity while maintaining strength.",
                              "Watch" = "Your arm strength is close to optimal,\nbut building more will help reduce injury\nrisk as velocity increases.",
                              "Warning" = "Your arm strength is below the safe range.\nStrength training is essential to support\nvelocity gains and prevent injury."
    )
    
    SVR_description_plot <- ggplot() +
      geom_text(aes(x = 0.5, y = 0.5, label = SVR_description),
                size = 9, color = "white") +
      theme_void()
  }
  
  # Save & composite your SVR description plot in-place
  SVR_description_path <- file.path(athlete_dir, paste0(athlete, " - SVR_description_plot.png"))
  ggsave(SVR_description_path,
         plot   = SVR_description_plot,
         width  = 4, height = 3.5,
         units  = "in",
         dpi    = 150)
  
  svr_desc_img <- image_read(SVR_description_path)
  report_img   <- image_composite(report_img, svr_desc_img, offset = "+1035+2775")
  
  rm(svr_desc_img)
  gc()
  
  
  # Initialize default values
  shoulderBalanceAVG <- NA
  label_color <- "white"
  label_text <- "No Current Data"
  
  # Create an empty color_ranges to avoid errors
  color_ranges <- data.frame(
    xmin = numeric(0),
    xmax = numeric(0),
    ymin = numeric(0),
    ymax = numeric(0),
    fill = character(0)
  )
  
  # Check if data exists and is valid
  if (nrow(filtered_armcare) > 0 && !is.na(filtered_armcare$shoulder_balance)) {
    shoulderBalanceAVG <- filtered_armcare$shoulder_balance
    
    # Determine label color and text based on thresholds
    if (shoulderBalanceAVG >= 0.85 & shoulderBalanceAVG <= 1.05) {
      label_color <- "#008000"
      label_text <- "Balanced"
    } else if ((shoulderBalanceAVG >= 0.70 & shoulderBalanceAVG < 0.85) | 
               (shoulderBalanceAVG > 1.05 & shoulderBalanceAVG <= 1.20)) {
      label_color <- "#FFFF00"
      label_text <- "Imbalanced"
    } else {
      label_color <- "#FF0000"
      label_text <- "Imbalanced"
    }
    
    # Create a data frame for the color ranges dynamically
    color_ranges <- data.frame(
      xmin = c(0.40, 0.70, 0.85, 1.06, 1.21),
      xmax = c(0.69, 0.84, 1.05, 1.20, 1.50),
      ymin = c(0, 0, 0, 0, 0),
      ymax = c(0.25, 0.25, 0.25, 0.25, 0.25),
      fill = c("#FF0000", "#FFFF00", "#008000", "#FFFF00", "#FF0000")  # Red, Yellow, Green
    )
  }
  
  if (is.na(shoulderBalanceAVG)) {
    shoulderBalance_plot <- ggplot() +
      geom_text(aes(x = 1.95, y = -0.35, label = "No Current Data"),  # Center text below the plot
                size = 10, color = "white", family = "Good Times") +
      theme_void()
    
  } else {
    shoulderBalance_plot <- ggplot() +
      geom_rect(data = color_ranges, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)) +
      geom_text(aes(x = 1.00, y = -0.35, label = label_text),  # Center text below the plot
                size = 10, color = label_color, family = "Good Times") +
      geom_text(aes(x = shoulderBalanceAVG, y = 0.6, label = paste(round(shoulderBalanceAVG, digits = 2))),
                size = 12, color = label_color, family = "Good Times") +
      geom_segment(aes(x = shoulderBalanceAVG, xend = shoulderBalanceAVG, y = -0.10, yend = 0.30), 
                   color = "white", linewidth = 2) +
      scale_fill_identity() +
      theme_void() +
      theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
      coord_cartesian(ylim = c(-1, 1))
  }
  
  # Save & composite your shoulder balance plot in-place
  shoulderBalance_path <- file.path(athlete_dir, paste0(athlete, " - shoulderBalance.png"))
  ggsave(shoulderBalance_path,
         plot   = shoulderBalance_plot,
         width  = 4, height = 3.5,
         units  = "in",
         dpi    = 150)
  
  shoulderBalance_img <- image_read(shoulderBalance_path)
  report_img          <- image_composite(report_img, shoulderBalance_img, offset = "+1800+2450")
  
  # Free the temporary image from memory
  rm(shoulderBalance_img)
  gc()
  
  
  if (is.na(shoulderBalanceAVG)) {
    label_description <- NA
  } else if (shoulderBalanceAVG >= 0.85 & shoulderBalanceAVG <= 1.05) {
    label_description <- "You have balanced strength between\nexternal rotators and internal rotators"
  } else if (shoulderBalanceAVG >= 0.70 & shoulderBalanceAVG < 0.85) {
    label_description <- "Your external rotators are weak\ncompared to your internal rotators"
  } else if (shoulderBalanceAVG > 1.05 & shoulderBalanceAVG <= 1.20) {
    label_description <- "Your internal rotators are weak\ncompared to your external rotators"
  } else if (shoulderBalanceAVG < 0.70) {
    label_description <- "Your external rotators are weak\ncompared to your internal rotators"
  } else {
    label_description <- "Your internal rotators are weak\ncompared to your external rotators"
  }
  
  shoulderDescription_plot <- ggplot() +
    geom_text(aes(x = 0.5, y = 0.5, label = label_description),
              size = 9, color = "white") +
    theme_void()
  
  # Save & composite your shoulder description plot in-place
  shoulderDescription_path <- file.path(athlete_dir, paste0(athlete, " - shoulderDescription.png"))
  ggsave(shoulderDescription_path,
         plot   = shoulderDescription_plot,
         width  = 5, height = 2,
         units  = "in",
         dpi    = 150)
  
  shoulderDescription_img <- image_read(shoulderDescription_path)
  report_img              <- image_composite(report_img, shoulderDescription_img, offset = "+1725+2925")
  
  # Free the temporary image from memory
  rm(shoulderDescription_img)
  gc()
  
  
  # Write out the final report
  final_report_path <- file.path(athlete_dir, "FuturesPitchingReport.png")
  image_write(report_img,
              path    = final_report_path,
              format  = "png")
  
  # Return the path for downloadHandler()
  final_report_path
  
}
