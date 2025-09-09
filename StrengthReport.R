library(tidyverse)
library(ggtext)
library(ggpubr)
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

generate_strength_report <- function(client_data, strength_data, athlete, month, year) { 
  
  report_img <- image_read_pdf("www/Strength Report Template.pdf")
  
  # Arrow icon function with inlined color logic
  arrow_icon <- function(value) {
    if (value > 0.01) {
      return(html(paste0("<span style='color:#008000;'>&#9650; ", value, "%</span>")))
    } else if (value < 0) {
      return(html(paste0("<span style='color:#FF0000;'>&#9660; ", value, "%</span>")))
    } else {
      return(html("<span>&#8212;</span>"))
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
  
  strength_data  <- strength_data %>% 
    filter(Level %in% c("L1", "L2", "L3", "Collegiate", "Professional"), Date < as.Date(cutoff_date))
  
  # ForceDecks Data
  
  summarized_IBSQT <- strength_data %>% 
    filter(!is.na(IBSQT)) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(max(IBSQT, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      Percentile = round(percent_rank(Value) * 100),
      Date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(Date) %>%
    group_by(Month, Year, Level, Gender) %>% 
    mutate(
      Rank = rank(-Value, ties.method = "min"),
      Total = n()
    ) %>%
    group_by(Name) %>%
    mutate(`Month:` = round((Value - lag(Value)) / lag(Value) * 100, 1),
           `Career:` = round((Value - first(Value)) / first(Value) * 100, 1)) %>%
    ungroup()
  
  summarized_CMJ <- strength_data %>% 
    filter(!is.na(CMJ)) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(max(CMJ, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      Exercise = "Lower Body\nPower",
      Percentile = round(percent_rank(Value) * 100),
      Date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(Date) %>%
    group_by(Month, Year, Level, Gender) %>% 
    mutate(
      Rank = rank(-Value, ties.method = "min"),
      Total = n()
    ) %>%
    group_by(Name) %>%
    mutate(`Month:` = round((Value - lag(Value)) / lag(Value) * 100, 1),
           `Career:` = round((Value - first(Value)) / first(Value) * 100, 1)) %>%
    ungroup()
  
  summarized_SHLDISOY <- strength_data %>% 
    filter(!is.na(SHLDISOY)) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(max(SHLDISOY, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      Percentile = round(percent_rank(Value) * 100),
      Date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(Date) %>%
    group_by(Month, Year, Level, Gender) %>% 
    mutate(
      Rank = rank(-Value, ties.method = "min"),
      Total = n()
    ) %>%
    group_by(Name) %>%
    mutate(`Month:` = round((Value - lag(Value)) / lag(Value) * 100, 1),
           `Career:` = round((Value - first(Value)) / first(Value) * 100, 1)) %>%
    ungroup()
  
  # Dynamo Data
  
  summarized_ISOtrunkRotation <- strength_data %>% 
    filter(!is.na(`Dynamo - Trunk`)) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(max(`Dynamo - Trunk`, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      Percentile = round(percent_rank(Value) * 100),
      Date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(Date) %>%
    group_by(Month, Year, Level, Gender) %>% 
    mutate(
      Rank = rank(-Value, ties.method = "min"),
      Total = n()
    ) %>%
    group_by(Name) %>%
    mutate(`Month:` = round((Value - lag(Value)) / lag(Value) * 100, 1),
           `Career:` = round((Value - first(Value)) / first(Value) * 100, 1)) %>%
    ungroup()
  
  summarized_rotation <- strength_data %>% 
    filter(!is.na(`External Rotation`) & !is.na(`Internal Rotation`)) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(
      InternalValue = round(max(`Internal Rotation`, na.rm = TRUE)),
      ExternalValue = round(max(`External Rotation`, na.rm = TRUE)),
      .groups = "drop") %>%
    mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>%
    arrange(Date)
  
  # Proteus Data
  
  summarized_trunkRotation <- strength_data %>% 
    filter(!is.na(TrunkRotation)) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(max(TrunkRotation, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      Exercise = "Core Power",
      Percentile = round(percent_rank(Value) * 100),
      Date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(Date) %>%
    group_by(Month, Year, Level, Gender) %>% 
    mutate(
      Rank = rank(-Value, ties.method = "min"),
      Total = n()
    ) %>%
    group_by(Name) %>%
    mutate(`Month:` = round((Value - lag(Value)) / lag(Value) * 100, 1),
           `Career:` = round((Value - first(Value)) / first(Value) * 100, 1)) %>%
    ungroup()
  
  summarized_shotput <- strength_data %>% 
    filter(!is.na(ShotPut), Date > as.Date("2025-02-14")) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(max(ShotPut, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      Exercise = "Upper Body\nPower",
      Percentile = round(percent_rank(Value) * 100),
      Date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(Date) %>%
    group_by(Month, Year, Level, Gender) %>% 
    mutate(
      Rank = rank(-Value, ties.method = "min"),
      Total = n()
    ) %>%
    group_by(Name) %>%
    mutate(`Month:` = round((Value - lag(Value)) / lag(Value) * 100, 1),
           `Career:` = round((Value - first(Value)) / first(Value) * 100, 1)) %>%
    ungroup()
  
  summarized_extensionFlexion <- strength_data %>% 
    filter(!is.na(D2Average), Date > as.Date("2024-04-01")) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(max(D2Average, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      Percentile = round(percent_rank(Value) * 100),
      Date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(Date) %>%
    group_by(Month, Year, Level, Gender) %>% 
    mutate(
      Rank = rank(-Value, ties.method = "min"),
      Total = n()
    ) %>%
    group_by(Name) %>%
    mutate(`Month:` = round((Value - lag(Value)) / lag(Value) * 100, 1),
           `Career:` = round((Value - first(Value)) / first(Value) * 100, 1)) %>%
    ungroup()
  
  summarized_extention <- strength_data %>% 
    filter(!is.na(D2Ext), Date > as.Date("2024-04-01")) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(max(D2Ext, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      Exercise = "Shoulder Decel.\nPower",
      Percentile = round(percent_rank(Value) * 100),
      Date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(Date)
  
  summarized_flexion <- strength_data %>% 
    filter(!is.na(D2Flex), Date > as.Date("2024-04-01")) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(max(D2Flex, na.rm = TRUE)), .groups = "drop") %>%
    mutate(
      Exercise = "Shoulder Accel.\nPower",
      Percentile = round(percent_rank(Value) * 100),
      Date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(Date)
  
  # Define a temp directory for this athlete
  athlete_dir <- file.path(tempdir(), athlete)
  if (!dir.exists(athlete_dir)) {
    dir.create(athlete_dir, recursive = TRUE)
  }
  
  ########################################################################################################
  #############################################  ATTENDANCE  #############################################
  ########################################################################################################
  
  # Attendance Data
  
  attendanceData <- strength_data %>% 
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
  
  # 1. Save the attendance plot
  attendance_path <- file.path(athlete_dir, paste0(athlete, "_attendancePlot.png"))
  ggsave(attendance_path,
         plot   = attendance_plot,
         width  = 2.15, height = 0.65,
         units  = "in",
         dpi    = 175)
  
  # 2. Read & composite into your existing report_img
  attendance_img <- image_read(attendance_path)
  report_img     <- image_composite(report_img, attendance_img, offset = "+1400+300")
  
  # 3. Immediately free the temp image
  rm(attendance_img)
  gc()
  
  
  #######################################################################################################
  ############################################PLAYER PROFILE#############################################
  #######################################################################################################
  
  athlete_info <- client_data %>%
    filter(Name == athlete) %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.) | . == "N/A", "--", .)))
  
  athlete_bio <- strength_data %>% 
    filter(Name == athlete) %>% 
    arrange(desc(Date))  # Ensure most recent data is first
  
  athlete_level <- athlete_bio$Level[1]
  athlete_gender <- athlete_bio$Gender[1]
  
  weight_data <- strength_data %>% 
    filter(Name == athlete, !is.na(Weight)) %>% 
    arrange(Date) %>% 
    group_by(Date, Name) %>% 
    summarise(Weight = mean(Weight, na.rm = TRUE), .groups = "drop") %>% 
    ungroup()  
  
  Q1 <- quantile(weight_data$Weight, 0.25, na.rm = TRUE)
  Q3 <- quantile(weight_data$Weight, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 2.5 * IQR
  upper_bound <- Q3 + 3.5 * IQR
  
  filtered_weight <- weight_data %>% 
    filter(Weight >= lower_bound & Weight <= upper_bound)
  
  # Check if filtered_weight is empty
  if (nrow(filtered_weight) == 0) {
    final_weight <- athlete_info$Weight
  } else {
    # Get first and last weight points
    first_weight <- filtered_weight %>% slice_min(Date, n = 1)
    last_weight <- filtered_weight %>% slice_max(Date, n = 1)
    
    final_weight <- round(last_weight$Weight)
  }
  
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
                            athlete_info$Height, " ", final_weight, "lbs | Age: ", athlete_info$Age), 
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
  
  # 3. Free the temporary image from memory
  rm(profile_img)
  gc()
  
  
  # Filter data for the athlete and exercise
  filtered_IBSQT <- summarized_IBSQT %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  IBSQT_check <- filtered_IBSQT %>%
    distinct(Month, Year) %>%
    nrow()
  
  min_IBSQT <- filtered_IBSQT %>%
    summarise(minValue = min(Value, na.rm = TRUE)) %>%
    pull(minValue)
  
  max_IBSQT <- filtered_IBSQT %>%
    summarise(maxValue = max(Value, na.rm = TRUE)) %>%
    pull(maxValue)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_IBSQT$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  if (length(unique_months) == 2) {
    breaks_option <- unique_months
  } else {
    breaks_option <- waiver() # Default ggplot behavior
  }
  
  # Filter data for May 2024
  current_IBSQT <- filtered_IBSQT %>% 
    filter(Month == month & Year == year)
  
  last_IBSQT <- filtered_IBSQT %>% 
    filter(Date == max(Date))
  
  last_IBSQT_value <- last_IBSQT$Value
  
  IBSQT_value <- current_IBSQT$Value
  IBSQT_percentile_numeric <- current_IBSQT$Percentile
  IBSQT_percentile <- ordinal_suffix(IBSQT_percentile_numeric)
  IBSQT_rank <- paste0(current_IBSQT$Rank, " out of ", current_IBSQT$Total)
  
  IBSQT_color <- col_numeric(
    palette = c("#FF0000", "#FFFF00", "#008000"), 
    domain = c(0, 100)  # Scale from 0 to 100 percentiles
  )(IBSQT_percentile_numeric)
  
  if (athlete_level == "Professional" && athlete_gender == "Female") {
    IBSQT_goal <- 4400
  } else if (athlete_level == "Professional" && athlete_gender == "Male") {
    IBSQT_goal <- 5500
  } else if (athlete_gender == "Female") {
    IBSQT_goal <- 2800
  } else {
    IBSQT_goal <- 3500
  }
  
  # Check for more than 1 month of data
  if (IBSQT_check > 1) {
    if (nrow(current_IBSQT) > 0) {
      # Case: More than 1 month and May 2024 data available
      restructured_IBSQT <- current_IBSQT %>% 
        select(`Month:`, `Career:`) %>%
        pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      IBSQT_gt <- restructured_IBSQT %>% 
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
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
      
      # Create plot
      IBSQT_plot <- ggplot(filtered_IBSQT, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        geom_hline(yintercept = IBSQT_goal, linetype = "dashed", color = "white", linewidth = 1) +
        annotate("text", x = max(filtered_IBSQT$Date), y = IBSQT_goal, label = "Minimum Strength Goal", 
                 color = "white", hjust = 0.825, vjust = -1, size = 5) +
        labs(
          title = "Isometric Belt Squat\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", IBSQT_value, "</span> <span style='font-size:25px;'>N</span><br><br>",
            "<span style='font-size:50px; color:", IBSQT_color, ";'>", IBSQT_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", IBSQT_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(
          limits = c(
            min(min_IBSQT - 1000, IBSQT_goal - 500),
            max(max_IBSQT + 1000, IBSQT_goal + 500) 
          )
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635), 
          axis.text = element_text(color = "lightgrey")
        )
      
    } else {
      # Case: More than 1 month but no May 2024 data
      IBSQT_plot <- ggplot(filtered_IBSQT, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        geom_hline(yintercept = IBSQT_goal, linetype = "dashed", color = "white", linewidth = 1) +
        annotate("text", x = max(filtered_IBSQT$Date), y = IBSQT_goal, label = "Minimum Strength Goal", 
                 color = "white", hjust = 0.825, vjust = -1, size = 5) +
        labs(
          title = "Isometric Belt Squat\n",
          subtitle = "No Recent Data\n\n",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(
          limits = c(
            min(min_IBSQT - 1000, IBSQT_goal - 500),
            max(max_IBSQT + 1000, IBSQT_goal + 500) 
          )
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      restructured_IBSQT <- last_IBSQT %>% 
        select(`Career:`) %>%
        pivot_longer(cols = `Career:`, names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      IBSQT_gt <- restructured_IBSQT %>% 
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
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
    }
    
  } else if (IBSQT_check == 1) {
    # Case: Only 1 month of data
    if (nrow(current_IBSQT) > 0) {
      # Case: Only 1 month, and it's May 2024
      IBSQT_plot <- ggplot(filtered_IBSQT, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        geom_hline(yintercept = IBSQT_goal, linetype = "dashed", color = "white", linewidth = 1) +
        annotate("text", x = max(filtered_IBSQT$Date), y = IBSQT_goal, label = "Minimum Strength Goal", 
                 color = "white", hjust = -0.97, vjust = -1, size = 5) +
        labs(
          title = "Isometric Belt Squat\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", IBSQT_value, "</span> <span style='font-size:25px;'>N</span><br><br>",
            "<span style='font-size:50px; color:", IBSQT_color, ";'>", IBSQT_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", IBSQT_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(
          limits = c(
            min(min_IBSQT - 1000, IBSQT_goal - 500),
            max(max_IBSQT + 1000, IBSQT_goal + 500) 
          )
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635), 
          axis.text = element_text(color = "lightgrey")
        )
      
      IBSQT_gt <- tibble(Metric = "", Value = "") %>%
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
      IBSQT_plot <- ggplot(filtered_IBSQT, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        geom_hline(yintercept = IBSQT_goal, linetype = "dashed", color = "white", linewidth = 1) +
        annotate("text", x = max(filtered_IBSQT$Date), y = IBSQT_goal, label = "Minimum Strength Goal", 
                 color = "white", hjust = -0.97, vjust = -1, size = 5) +
        labs(
          title = "   Isometric Belt Squat\n",
          subtitle = "    No Recent Data",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(
          limits = c(
            min(min_IBSQT - 1000, IBSQT_goal - 500),
            max(max_IBSQT + 1000, IBSQT_goal + 500) 
          )
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      IBSQT_gt <- tibble(Metric = "", Value = "") %>%
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
    IBSQT_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Data Available", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   Isometric Belt Squat\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26),
        plot.subtitle = element_blank()
      )
    
    IBSQT_gt <- tibble(Metric = "", Value = "") %>%
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
  
  # 1. Composite your IBSQT GT table in‑place
  IBSQT_gt_path <- file.path(athlete_dir, paste0(athlete, "-IBSQT_gt.png"))
  gtsave(IBSQT_gt, file = IBSQT_gt_path, expand = -1)
  
  ibsqt_gt_img <- image_read(IBSQT_gt_path) %>%
    image_transparent(color = "black")
  report_img   <- image_composite(report_img, ibsqt_gt_img, offset = "+1175+535")
  
  rm(ibsqt_gt_img)
  gc()
  
  
  # 2. Composite your IBSQT plot in‑place
  IBSQT_plot_path <- file.path(athlete_dir, paste0(athlete, "-IBSQT_plot.png"))
  ggsave(IBSQT_plot_path,
         plot   = IBSQT_plot,
         width  = 5.5, height = 3.5,
         units  = "in",
         dpi    = 150)
  
  ibsqt_plot_img <- image_read(IBSQT_plot_path)
  report_img     <- image_composite(report_img, ibsqt_plot_img, offset = "+600+575")
  
  rm(ibsqt_plot_img)
  gc()
  
  
  # Filter data for the athlete and exercise
  filtered_CMJ <- summarized_CMJ %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  CMJ_check <- filtered_CMJ %>%
    distinct(Month, Year) %>%
    nrow()
  
  min_CMJ <- filtered_CMJ %>%
    summarise(minValue = min(Value, na.rm = TRUE)) %>%
    pull(minValue)
  
  max_CMJ <- filtered_CMJ %>%
    summarise(maxValue = max(Value, na.rm = TRUE)) %>%
    pull(maxValue)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_CMJ$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  if (length(unique_months) == 2) {
    breaks_option <- unique_months
  } else {
    breaks_option <- waiver() # Default ggplot behavior
  }
  
  # Filter data for May 2024
  current_CMJ <- filtered_CMJ %>% 
    filter(Month == month & Year == year)
  
  last_CMJ <- filtered_CMJ %>% 
    filter(Date == max(Date))
  
  CMJ_value <- current_CMJ$Value
  CMJ_percentile_numeric <- current_CMJ$Percentile
  CMJ_percentile <- ordinal_suffix(CMJ_percentile_numeric)
  CMJ_rank <- paste0(current_CMJ$Rank, " out of ", current_CMJ$Total)
  
  CMJ_color <- col_numeric(
    palette = c("#FF0000", "#FFFF00", "#008000"), 
    domain = c(0, 100)  # Scale from 0 to 100 percentiles
  )(CMJ_percentile_numeric)
  
  # Check for more than 1 month of data
  if (CMJ_check > 1) {
    if (nrow(current_CMJ) > 0) {
      # Case: More than 1 month and May 2024 data available
      restructured_CMJ <- current_CMJ %>% 
        select(`Month:`, `Career:`) %>%
        pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      CMJ_gt <- restructured_CMJ %>% 
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
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
      
      # Create plot
      CMJ_plot <- ggplot(filtered_CMJ, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Countermovement Jump\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", CMJ_value, "</span> <span style='font-size:25px;'>W</span><br><br>",
            "<span style='font-size:50px; color:", CMJ_color, ";'>", CMJ_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", CMJ_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_CMJ - 1000, max_CMJ + 1000)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635), 
          axis.text = element_text(color = "lightgrey")
        )
      
    } else {
      # Case: More than 1 month but no May 2024 data
      CMJ_plot <- ggplot(filtered_CMJ, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Countermovement Jump\n",
          subtitle = " No Recent Data\n",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_CMJ - 1000, max_CMJ + 1000)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      restructured_CMJ <- last_CMJ %>% 
        select(`Career:`) %>%
        pivot_longer(cols = `Career:`, names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      CMJ_gt <- restructured_CMJ %>% 
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
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
    }
    
  } else if (CMJ_check == 1) {
    # Case: Only 1 month of data
    if (nrow(current_CMJ) > 0) {
      # Case: Only 1 month, and it's May 2024
      CMJ_plot <- ggplot(filtered_CMJ, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Countermovement Jump\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", CMJ_value, "</span> <span style='font-size:25px;'>W</span><br><br>",
            "<span style='font-size:50px; color:", CMJ_color, ";'>", CMJ_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", CMJ_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_CMJ - 1000, max_CMJ + 1000)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635), 
          axis.text = element_text(color = "lightgrey")
        )
      
      CMJ_gt <- tibble(Metric = "", Value = "") %>%
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
      CMJ_plot <- ggplot(filtered_CMJ, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "   Countermovement Jump\n",
          subtitle = "    No Recent Data",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_CMJ - 1000, max_CMJ + 1000)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      CMJ_gt <- tibble(Metric = "", Value = "") %>%
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
    CMJ_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Data Available", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   Countermovement Jump\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26),
        plot.subtitle = element_blank()
      )
    
    CMJ_gt <- tibble(Metric = "", Value = "") %>%
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
  
  # 1. Composite your CMJ GT table in‑place
  CMJ_gt_path <- file.path(athlete_dir, paste0(athlete, "-CMJ_gt.png"))
  gtsave(CMJ_gt, file = CMJ_gt_path, expand = -1)
  
  cmj_gt_img <- image_read(CMJ_gt_path) %>%
    image_transparent(color = "black")
  report_img <- image_composite(report_img, cmj_gt_img, offset = "+2150+535")
  
  rm(cmj_gt_img)
  gc()
  
  
  # 2. Composite your CMJ plot in‑place
  CMJ_plot_path <- file.path(athlete_dir, paste0(athlete, "-CMJ_plot.png"))
  ggsave(CMJ_plot_path,
         plot   = CMJ_plot,
         width  = 5.5, height = 3.5,
         units  = "in",
         dpi    = 150)
  
  cmj_plot_img <- image_read(CMJ_plot_path)
  report_img    <- image_composite(report_img, cmj_plot_img, offset = "+1570+575")
  
  rm(cmj_plot_img)
  gc()
  
  
  # Filter data for the specific athlete and exercise
  filtered_ISOtrunkRotation <- summarized_ISOtrunkRotation %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  ISOtrunkRotation_check <- filtered_ISOtrunkRotation %>%
    distinct(Month, Year) %>%
    nrow()
  
  min_ISOTR <- filtered_ISOtrunkRotation %>%
    summarise(minValue = min(Value, na.rm = TRUE)) %>%
    pull(minValue)
  
  max_ISOTR <- filtered_ISOtrunkRotation %>%
    summarise(maxValue = max(Value, na.rm = TRUE)) %>%
    pull(maxValue)
  
  athlete_max_ISOTR <- max(filtered_ISOtrunkRotation$Value)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_ISOtrunkRotation$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  if (length(unique_months) == 2) {
    breaks_option <- unique_months
  } else {
    breaks_option <- waiver() # Default ggplot behavior
  }
  
  # Filter data for May 2024
  current_ISOtrunkRotation <- filtered_ISOtrunkRotation %>% 
    filter(Month == month & Year == year)
  
  last_ISOtrunkRotation <- filtered_ISOtrunkRotation %>% 
    filter(Date == max(Date))
  
  last_ISOtrunkRotation_value <- last_ISOtrunkRotation$Value
  
  ISOtrunkRotation_value <- current_ISOtrunkRotation$Value
  ISOtrunkRotation_percentile_numeric <- current_ISOtrunkRotation$Percentile
  ISOtrunkRotation_percentile <- ordinal_suffix(ISOtrunkRotation_percentile_numeric)
  ISOtrunkRotation_rank <- paste0(current_ISOtrunkRotation$Rank, " out of ", current_ISOtrunkRotation$Total)
  
  ISOtrunkRotation_color <- col_numeric(
    palette = c("#FF0000", "#FFFF00", "#008000"), 
    domain = c(0, 100)  # Scale from 0 to 100 percentiles
  )(ISOtrunkRotation_percentile_numeric)
  
  if (athlete_level == "Professional" && athlete_gender == "Female") {
    ISOtrunkRotaion_goal <- 320
  } else if (athlete_level == "Professional" && athlete_gender == "Male") {
    ISOtrunkRotaion_goal <- 400
  } else if (athlete_gender == "Female") {
    ISOtrunkRotaion_goal <- 160
  } else {
    ISOtrunkRotaion_goal <- 200
  }
  
  # Check for more than 1 month of data
  if (ISOtrunkRotation_check > 1) {
    if (nrow(current_ISOtrunkRotation) > 0) {
      # Case: More than 1 month and May 2024 data available
      restructured_ISOtrunkRotation <- current_ISOtrunkRotation %>% 
        select(`Month:`, `Career:`) %>%
        pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      ISOtrunkRotation_gt <- restructured_ISOtrunkRotation %>% 
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
        opt_table_font(font = "Helvetica") %>% 
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
      
      # Create plot
      ISOtrunkRotation_plot <- ggplot(filtered_ISOtrunkRotation, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        geom_hline(yintercept = ISOtrunkRotaion_goal, linetype = "dashed", color = "white", linewidth = 1) +
        annotate("text", x = max(filtered_ISOtrunkRotation$Date), y = ISOtrunkRotaion_goal, label = "Minimum Strength Goal", 
                 color = "white", hjust = 0.825, vjust = -1, size = 5) +
        labs(
          title = "ISO Straight Arm Trunk Rotation\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", ISOtrunkRotation_value, "</span> <span style='font-size:25px;'>N</span><br><br>",
            "<span style='font-size:50px; color:", ISOtrunkRotation_color, ";'>", ISOtrunkRotation_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", ISOtrunkRotation_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(
          limits = c(
            min(min_ISOTR - 50, ISOtrunkRotaion_goal - 20),
            max(max_ISOTR + 50, ISOtrunkRotaion_goal + 20)
          )
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635),
          axis.text = element_text(color = "lightgrey")
        )
      
    } else {
      # Case: More than 1 month but no May 2024 data
      ISOtrunkRotation_plot <- ggplot(filtered_ISOtrunkRotation, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        geom_hline(yintercept = ISOtrunkRotaion_goal, linetype = "dashed", color = "white", linewidth = 1) +
        annotate("text", x = max(filtered_ISOtrunkRotation$Date), y = ISOtrunkRotaion_goal, label = "Minimum Strength Goal", 
                 color = "white", hjust = 0.825, vjust = -1, size = 5) +
        labs(
          title = "ISO Straight Arm Trunk Rotation\n",
          subtitle = " No Recent Data\n",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(
          limits = c(
            min(min_ISOTR - 50, ISOtrunkRotaion_goal - 20),
            max(max_ISOTR + 50, ISOtrunkRotaion_goal + 20)
          )
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      restructured_ISOtrunkRotation <- last_ISOtrunkRotation %>% 
        select(`Career:`) %>%
        pivot_longer(cols = `Career:`, names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      ISOtrunkRotation_gt <- restructured_ISOtrunkRotation %>% 
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
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
    }
    
  } else if (ISOtrunkRotation_check == 1) {
    # Case: Only 1 month of data
    if (nrow(current_ISOtrunkRotation) > 0) {
      # Case: Only 1 month, and it's May 2024
      ISOtrunkRotation_plot <- ggplot(filtered_ISOtrunkRotation, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        geom_hline(yintercept = ISOtrunkRotaion_goal, linetype = "dashed", color = "white", linewidth = 1) +
        annotate("text", x = max(filtered_ISOtrunkRotation$Date), y = ISOtrunkRotaion_goal, label = "Minimum Strength Goal", 
                 color = "white", hjust = -0.99, vjust = -1, size = 5) +
        labs(
          title = "ISO Straight Arm Trunk Rotation\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", ISOtrunkRotation_value, "</span> <span style='font-size:25px;'>N</span><br><br>",
            "<span style='font-size:50px; color:", ISOtrunkRotation_color, ";'>", ISOtrunkRotation_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", ISOtrunkRotation_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(
          limits = c(
            min(min_ISOTR - 50, ISOtrunkRotaion_goal - 20),
            max(max_ISOTR + 50, ISOtrunkRotaion_goal + 20)
          )
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635),
          axis.text = element_text(color = "lightgrey")
        )
      
      ISOtrunkRotation_gt <- tibble(Metric = "", Value = "") %>%
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
      ISOtrunkRotation_plot <- ggplot(filtered_ISOtrunkRotation, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        geom_hline(yintercept = ISOtrunkRotaion_goal, linetype = "dashed", color = "white", linewidth = 1) +
        annotate("text", x = max(filtered_ISOtrunkRotation$Date), y = ISOtrunkRotaion_goal, label = "Minimum Strength Goal", 
                 color = "white", hjust = -0.99, vjust = -1, size = 5) +
        labs(
          title = "   ISO Straight Arm Trunk Rotation\n",
          subtitle = "    No Recent Data",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(
          limits = c(
            min(min_ISOTR - 50, ISOtrunkRotaion_goal - 20),
            max(max_ISOTR + 50, ISOtrunkRotaion_goal + 20)
          )
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      ISOtrunkRotation_gt <- tibble(Metric = "", Value = "") %>%
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
    ISOtrunkRotation_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Data Available", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   ISO Straight Arm Trunk Rotation\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26),
        plot.subtitle = element_blank()
      )
    
    ISOtrunkRotation_gt <- tibble(Metric = "", Value = "") %>%
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
  
  # 1. Composite your ISO Trunk Rotation GT table in-place
  ISOtrunkRotation_gt_path <- file.path(athlete_dir, paste0(athlete, "-ISOtrunkRotation_gt.png"))
  gtsave(ISOtrunkRotation_gt, file = ISOtrunkRotation_gt_path, expand = -1)
  
  iso_gt_img  <- image_read(ISOtrunkRotation_gt_path) %>%
    image_transparent(color = "black")
  report_img  <- image_composite(report_img, iso_gt_img, offset = "+1175+1240")
  rm(iso_gt_img)
  gc()
  
  # 2. Composite your ISO Trunk Rotation plot in-place
  ISOtrunkRotation_plot_path <- file.path(athlete_dir, paste0(athlete, "-ISOtrunkRotation_plot.png"))
  ggsave(ISOtrunkRotation_plot_path,
         plot   = ISOtrunkRotation_plot,
         width  = 5.5, height = 3.5,
         units  = "in",
         dpi    = 150)
  
  iso_plot_img <- image_read(ISOtrunkRotation_plot_path)
  report_img    <- image_composite(report_img, iso_plot_img, offset = "+600+1275")
  rm(iso_plot_img)
  gc()
  
  
  # Filter data for the specific athlete and exercise
  filtered_trunkRotation <- summarized_trunkRotation %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  trunkRotation_check <- filtered_trunkRotation %>%
    distinct(Month, Year) %>%
    nrow()
  
  min_TR <- filtered_trunkRotation %>%
    summarise(minValue = min(Value, na.rm = TRUE)) %>%
    pull(minValue)
  
  max_TR <- filtered_trunkRotation %>%
    summarise(maxValue = max(Value, na.rm = TRUE)) %>%
    pull(maxValue)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_trunkRotation$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  if (length(unique_months) == 2) {
    breaks_option <- unique_months
  } else {
    breaks_option <- waiver() # Default ggplot behavior
  }
  
  # Filter data for May 2024
  current_trunkRotation <- filtered_trunkRotation %>% 
    filter(Month == month & Year == year)
  
  last_trunkRotation <- filtered_trunkRotation %>% 
    filter(Date == max(Date))
  
  trunkRotation_value <- current_trunkRotation$Value
  trunkRotation_percentile_numeric <- current_trunkRotation$Percentile
  trunkRotation_percentile <- ordinal_suffix(trunkRotation_percentile_numeric)
  trunkRotation_rank <- paste0(current_trunkRotation$Rank, " out of ", current_trunkRotation$Total)
  
  trunkRotation_color <- col_numeric(
    palette = c("#FF0000", "#FFFF00", "#008000"), 
    domain = c(0, 100)  # Scale from 0 to 100 percentiles
  )(trunkRotation_percentile_numeric)
  
  # Check for more than 1 month of data
  if (trunkRotation_check > 1) {
    if (nrow(current_trunkRotation) > 0) {
      # Case: More than 1 month and May 2024 data available
      restructured_trunkRotation <- current_trunkRotation %>% 
        select(`Month:`, `Career:`) %>%
        pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      trunkRotation_gt <- restructured_trunkRotation %>% 
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
        opt_table_font(font = "Helvetica") %>% 
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
      
      # Create plot
      trunkRotation_plot <- ggplot(filtered_trunkRotation, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Straight Arm Trunk Rotation\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", trunkRotation_value, "</span> <span style='font-size:25px;'>W</span><br><br>",
            "<span style='font-size:50px; color:", trunkRotation_color, ";'>", trunkRotation_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", trunkRotation_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_TR - 50, max_TR + 50)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635),
          axis.text = element_text(color = "lightgrey")
        )
      
    } else {
      # Case: More than 1 month but no May 2024 data
      trunkRotation_plot <- ggplot(filtered_trunkRotation, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Straight Arm Trunk Rotation\n",
          subtitle = " No Recent Data\n",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_TR - 50, max_TR + 50)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      restructured_trunkRotation <- last_trunkRotation %>% 
        select(`Career:`) %>%
        pivot_longer(cols = `Career:`, names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      trunkRotation_gt <- restructured_trunkRotation %>% 
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
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
    }
    
  } else if (trunkRotation_check == 1) {
    # Case: Only 1 month of data
    if (nrow(current_trunkRotation) > 0) {
      # Case: Only 1 month, and it's May 2024
      trunkRotation_plot <- ggplot(filtered_trunkRotation, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Straight Arm Trunk Rotation\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", trunkRotation_value, "</span> <span style='font-size:25px;'>W</span><br><br>",
            "<span style='font-size:50px; color:", trunkRotation_color, ";'>", trunkRotation_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", trunkRotation_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_TR - 50, max_TR + 50)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635),
          axis.text = element_text(color = "lightgrey")
        )
      
      trunkRotation_gt <- tibble(Metric = "", Value = "") %>%
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
      trunkRotation_plot <- ggplot(filtered_trunkRotation, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "   Straight Arm Trunk Rotation\n",
          subtitle = "    No Recent Data",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_TR - 50, max_TR + 50)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      trunkRotation_gt <- tibble(Metric = "", Value = "") %>%
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
    trunkRotation_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Data Available", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   Straight Arm Trunk Rotation\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26),
        plot.subtitle = element_blank()
      )
    
    trunkRotation_gt <- tibble(Metric = "", Value = "") %>%
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
  
  # 1. Composite your trunkRotation GT table in-place
  trunkRotation_gt_path <- file.path(athlete_dir, paste0(athlete, "-trunkRotation_gt.png"))
  gtsave(trunkRotation_gt, file = trunkRotation_gt_path, expand = -1)
  
  trunk_gt_img <- image_read(trunkRotation_gt_path) %>%
    image_transparent(color = "black")
  report_img   <- image_composite(report_img, trunk_gt_img, offset = "+2150+1240")
  
  rm(trunk_gt_img)
  gc()
  
  
  # 2. Composite your trunkRotation plot in-place
  trunkRotation_plot_path <- file.path(athlete_dir, paste0(athlete, "-trunkRotation_plot.png"))
  ggsave(trunkRotation_plot_path,
         plot   = trunkRotation_plot,
         width  = 5.5, height = 3.5,
         units  = "in",
         dpi    = 150)
  
  trunk_plot_img <- image_read(trunkRotation_plot_path)
  report_img     <- image_composite(report_img, trunk_plot_img, offset = "+1570+1275")
  
  rm(trunk_plot_img)
  gc()
  
  
  # Filter data for the specific athlete and exercise
  filtered_SHLDISOY <- summarized_SHLDISOY %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  SHLDISOY_check <- filtered_SHLDISOY %>%
    distinct(Month, Year) %>%
    nrow()
  
  min_SHLDISOY <- filtered_SHLDISOY %>%
    summarise(minValue = min(Value, na.rm = TRUE)) %>%
    pull(minValue)
  
  max_SHLDISOY <- filtered_SHLDISOY %>%
    summarise(maxValue = max(Value, na.rm = TRUE)) %>%
    pull(maxValue)
  
  athlete_max_SHLDISOY <- max(filtered_SHLDISOY$Value)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_SHLDISOY$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  if (length(unique_months) == 2) {
    breaks_option <- unique_months
  } else {
    breaks_option <- waiver() # Default ggplot behavior
  }
  
  # Filter data for May 2024
  current_SHLDISOY <- filtered_SHLDISOY %>% 
    filter(Month == month & Year == year)
  
  last_SHLDISOY <- filtered_SHLDISOY %>% 
    filter(Date == max(Date))
  
  last_SHLDISOY_value <- last_SHLDISOY$Value
  
  SHLDISOY_value <- current_SHLDISOY$Value
  SHLDISOY_percentile_numeric <- current_SHLDISOY$Percentile
  SHLDISOY_percentile <- ordinal_suffix(SHLDISOY_percentile_numeric)
  SHLDISOY_rank <- paste0(current_SHLDISOY$Rank, " out of ", current_SHLDISOY$Total)
  
  SHLDISOY_color <- col_numeric(
    palette = c("#FF0000", "#FFFF00", "#008000"), 
    domain = c(0, 100)  # Scale from 0 to 100 percentiles
  )(SHLDISOY_percentile_numeric)
  
  if (athlete_level == "Professional" && athlete_gender == "Female") {
    SHLDISOY_goal <- 160
  } else if (athlete_level == "Professional" && athlete_gender == "Male") {
    SHLDISOY_goal <- 200
  } else if (athlete_gender == "Female") {
    SHLDISOY_goal <- 120
  } else {
    SHLDISOY_goal <- 150
  }
  
  # Check for more than 1 month of data
  if (SHLDISOY_check > 1) {
    if (nrow(current_SHLDISOY) > 0) {
      # Case: More than 1 month and May 2024 data available
      restructured_SHLDISOY <- current_SHLDISOY %>% 
        select(`Month:`, `Career:`) %>%
        pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      SHLDISOY_gt <- restructured_SHLDISOY %>% 
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
        opt_table_font(font = "Helvetica") %>% 
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
      
      # Create plot
      SHLDISOY_plot <- ggplot(filtered_SHLDISOY, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        geom_hline(yintercept = SHLDISOY_goal, linetype = "dashed", color = "white", linewidth = 1) +
        annotate("text", x = max(filtered_SHLDISOY$Date), y = SHLDISOY_goal, label = "Minimum Strength Goal", 
                 color = "white", hjust = 0.825, vjust = -1, size = 5) +
        labs(
          title = "Isometric Y\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", SHLDISOY_value, "</span> <span style='font-size:25px;'>N</span><br><br>",
            "<span style='font-size:50px; color:", SHLDISOY_color, ";'>", SHLDISOY_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", SHLDISOY_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(
          limits = c(
            min(min_SHLDISOY - 50, SHLDISOY_goal - 20), # Ensure the lower limit includes space for maxEV_goal
            max(max_SHLDISOY + 50, SHLDISOY_goal + 20)  # Ensure the upper limit includes space for maxEV_goal
          )
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635),
          axis.text = element_text(color = "lightgrey")
        )
      
    } else {
      # Case: More than 1 month but no May 2024 data
      SHLDISOY_plot <- ggplot(filtered_SHLDISOY, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        geom_hline(yintercept = ifelse(athlete_level == "Professional", 200, 130), linetype = "dashed", color = "white", linewidth = 1) +
        annotate("text", x = max(filtered_SHLDISOY$Date), y = ifelse(athlete_level == "Professional", 200, 130), label = "Minimum Strength Goal", 
                 color = "white", hjust = 0.825, vjust = -1, size = 5) +
        labs(
          title = "Isometric Y\n",
          subtitle = " No Recent Data\n",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(
          limits = c(
            min(min_SHLDISOY - 50, SHLDISOY_goal - 20), # Ensure the lower limit includes space for maxEV_goal
            max(max_SHLDISOY + 50, SHLDISOY_goal + 20)  # Ensure the upper limit includes space for maxEV_goal
          )
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      restructured_SHLDISOY <- last_SHLDISOY %>% 
        select(`Career:`) %>%
        pivot_longer(cols = `Career:`, names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      SHLDISOY_gt <- restructured_SHLDISOY %>% 
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
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
    }
    
  } else if (SHLDISOY_check == 1) {
    # Case: Only 1 month of data
    if (nrow(current_SHLDISOY) > 0) {
      # Case: Only 1 month, and it's May 2024
      SHLDISOY_plot <- ggplot(filtered_SHLDISOY, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        geom_hline(yintercept = ifelse(athlete_level == "Professional", 200, 130), linetype = "dashed", color = "white", linewidth = 1) +
        annotate("text", x = max(filtered_SHLDISOY$Date), y = ifelse(athlete_level == "Professional", 200, 130), label = "Minimum Strength Goal", 
                 color = "white", hjust = -0.99, vjust = -1, size = 5) +
        labs(
          title = "Isometric Y\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", SHLDISOY_value, "</span> <span style='font-size:25px;'>N</span><br><br>",
            "<span style='font-size:50px; color:", SHLDISOY_color, ";'>", SHLDISOY_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", SHLDISOY_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(
          limits = c(
            min(min_SHLDISOY - 50, SHLDISOY_goal - 20), # Ensure the lower limit includes space for maxEV_goal
            max(max_SHLDISOY + 50, SHLDISOY_goal + 20)  # Ensure the upper limit includes space for maxEV_goal
          )
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635),
          axis.text = element_text(color = "lightgrey")
        )
      
      SHLDISOY_gt <- tibble(Metric = "", Value = "") %>%
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
      SHLDISOY_plot <- ggplot(filtered_SHLDISOY, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        geom_hline(yintercept = SHLDISOY_goal, linetype = "dashed", color = "white", linewidth = 1) +
        annotate("text", x = max(filtered_SHLDISOY$Date), y = SHLDISOY_goal, label = "Minimum Strength Goal", 
                 color = "white", hjust = -0.99, vjust = -1, size = 5) +
        labs(
          title = "   Isometric Y\n",
          subtitle = "    No Recent Data",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(
          limits = c(
            min(min_SHLDISOY - 50, SHLDISOY_goal - 20), # Ensure the lower limit includes space for maxEV_goal
            max(max_SHLDISOY + 50, SHLDISOY_goal + 20)  # Ensure the upper limit includes space for maxEV_goal
          )
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      SHLDISOY_gt <- tibble(Metric = "", Value = "") %>%
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
    SHLDISOY_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Data Available", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   Isometric Y\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26),
        plot.subtitle = element_blank()
      )
    
    SHLDISOY_gt <- tibble(Metric = "", Value = "") %>%
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
  
  # 1. Composite your SHLD ISO Y GT table in-place
  SHLDISOY_gt_path <- file.path(athlete_dir, paste0(athlete, "-SHLDISOY_gt.png"))
  gtsave(SHLDISOY_gt, file = SHLDISOY_gt_path, expand = -1)
  
  shld_isoy_gt_img <- image_read(SHLDISOY_gt_path) %>%
    image_transparent(color = "black")
  report_img        <- image_composite(report_img, shld_isoy_gt_img, offset = "+1175+1935")
  
  rm(shld_isoy_gt_img)
  gc()
  
  
  # 2. Composite your SHLD ISO Y plot in-place
  SHLDISOY_plot_path <- file.path(athlete_dir, paste0(athlete, "-SHLDISOY_plot.png"))
  ggsave(SHLDISOY_plot_path,
         plot   = SHLDISOY_plot,
         width  = 5.5, height = 3.5,
         units  = "in",
         dpi    = 150)
  
  shld_isoy_plot_img <- image_read(SHLDISOY_plot_path)
  report_img         <- image_composite(report_img, shld_isoy_plot_img, offset = "+600+1975")
  
  rm(shld_isoy_plot_img)
  gc()
  
  
  # Filter data for the specific athlete and exercise
  filtered_shotput <- summarized_shotput %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  shotput_check <- filtered_shotput %>%
    distinct(Month, Year) %>%
    nrow()
  
  min_PP <- filtered_shotput %>%
    summarise(minValue = min(Value, na.rm = TRUE)) %>%
    pull(minValue)
  
  max_PP <- filtered_shotput %>%
    summarise(maxValue = max(Value, na.rm = TRUE)) %>%
    pull(maxValue)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_shotput$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  if (length(unique_months) == 2) {
    breaks_option <- unique_months
  } else {
    breaks_option <- waiver() # Default ggplot behavior
  }
  
  # Filter data for May 2024
  current_shotput <- filtered_shotput %>% 
    filter(Month == month & Year == year)
  
  last_shotput <- filtered_shotput %>% 
    filter(Date == max(Date))
  
  shotput_value <- current_shotput$Value
  shotput_percentile_numeric <- current_shotput$Percentile
  shotput_percentile <- ordinal_suffix(shotput_percentile_numeric)
  shotput_rank <- paste0(current_shotput$Rank, " out of ", current_shotput$Total)
  
  shotput_color <- col_numeric(
    palette = c("#FF0000", "#FFFF00", "#008000"), 
    domain = c(0, 100)  # Scale from 0 to 100 percentiles
  )(shotput_percentile_numeric)
  
  # Check for more than 1 month of data
  if (shotput_check > 1) {
    if (nrow(current_shotput) > 0) {
      # Case: More than 1 month and May 2024 data available
      restructured_shotput <- current_shotput %>% 
        select(`Month:`, `Career:`) %>%
        pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      shotput_gt <- restructured_shotput %>% 
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
        opt_table_font(font = "Helvetica") %>% 
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
      
      # Create plot
      shotput_plot <- ggplot(filtered_shotput, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Proteus Shot Put\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", shotput_value, "</span> <span style='font-size:25px;'>W</span><br><br>",
            "<span style='font-size:50px; color:", shotput_color, ";'>", shotput_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", shotput_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_PP - 50, max_PP + 50)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635),
          axis.text = element_text(color = "lightgrey")
        )
      
    } else {
      # Case: More than 1 month but no May 2024 data
      shotput_plot <- ggplot(filtered_shotput, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Proteus Shot Put\n",
          subtitle = " No Recent Data\n",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_PP - 50, max_PP + 50)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      restructured_shotput <- last_shotput %>% 
        select(`Career:`) %>%
        pivot_longer(cols = `Career:`, names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      shotput_gt <- restructured_shotput %>% 
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
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
    }
    
  } else if (shotput_check == 1) {
    # Case: Only 1 month of data
    if (nrow(current_shotput) > 0) {
      # Case: Only 1 month, and it's May 2024
      shotput_plot <- ggplot(filtered_shotput, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Proteus Shot Put\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", shotput_value, "</span> <span style='font-size:25px;'>W</span><br><br>",
            "<span style='font-size:50px; color:", shotput_color, ";'>", shotput_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", shotput_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_PP - 50, max_PP + 50)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635),
          axis.text = element_text(color = "lightgrey")
        )
      
      shotput_gt <- tibble(Metric = "", Value = "") %>%
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
      shotput_plot <- ggplot(filtered_shotput, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "   Proteus Shot Put\n",
          subtitle = "    No Recent Data",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_PP - 50, max_PP + 50)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      shotput_gt <- tibble(Metric = "", Value = "") %>%
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
    shotput_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Data Available", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   Proteus Shot Put\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26),
        plot.subtitle = element_blank()
      )
    
    shotput_gt <- tibble(Metric = "", Value = "") %>%
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
  
  # 1. Composite your Shot Put GT table in-place
  shotput_gt_path <- file.path(athlete_dir, paste0(athlete, "-shotput_gt.png"))
  gtsave(shotput_gt, file = shotput_gt_path, expand = -1)
  
  shotput_gt_img <- image_read(shotput_gt_path) %>%
    image_transparent(color = "black")
  report_img     <- image_composite(report_img, shotput_gt_img, offset = "+2150+1935")
  
  rm(shotput_gt_img)
  gc()
  
  
  # 2. Composite your Shot Put plot in-place
  shotput_plot_path <- file.path(athlete_dir, paste0(athlete, "-shotput_plot.png"))
  ggsave(shotput_plot_path,
         plot   = shotput_plot,
         width  = 5.5, height = 3.5,
         units  = "in",
         dpi    = 150)
  
  shotput_plot_img <- image_read(shotput_plot_path)
  report_img       <- image_composite(report_img, shotput_plot_img, offset = "+1570+1975")
  
  rm(shotput_plot_img)
  gc()
  
  
  # Filter data for the specific athlete
  filtered_rotation <- summarized_rotation %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  rotation_check <- filtered_rotation %>%
    distinct(Month, Year) %>%
    nrow()
  
  # Get min/max values across both rotation types
  min_rotation <- filtered_rotation %>%
    summarise(minValue = min(c(InternalValue, ExternalValue), na.rm = TRUE)) %>%
    pull(minValue)
  
  max_rotation <- filtered_rotation %>%
    summarise(maxValue = max(c(InternalValue, ExternalValue), na.rm = TRUE)) %>%
    pull(maxValue)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_rotation$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  breaks_option <- if (length(unique_months) == 2) unique_months else waiver()
  
  # Filter for current month
  current_rotation <- filtered_rotation %>%
    filter(Month == month, Year == 2025)
  
  # Check if either rotation type is missing or has only NA values
  if (nrow(current_rotation) > 0 &&
      (all(is.na(current_rotation$ExternalValue)) || all(is.na(current_rotation$InternalValue)))) {
    
    missing_types <- c()
    if (all(is.na(current_rotation$ExternalValue))) missing_types <- c(missing_types, "External Rotation")
    if (all(is.na(current_rotation$InternalValue))) missing_types <- c(missing_types, "Internal Rotation")
    
    shoulderBalance_color <- "#FF0000"
    shoulderBalance <- paste("Insufficient Data: missing", paste(missing_types, collapse = " and "))
    
    plot_data <- filtered_rotation %>%
      select(Date, ExternalValue, InternalValue) %>%
      pivot_longer(cols = c(ExternalValue, InternalValue), names_to = "Rotation", values_to = "Value") %>%
      mutate(Rotation = recode(Rotation, ExternalValue = "External Rotation", InternalValue = "Internal Rotation"))
    
    rotation_plot <- ggplot(plot_data, aes(x = Date, y = Value, group = Rotation, color = Rotation)) +
      geom_line(linewidth = 2) +
      geom_point(size = 3) +
      labs(
        title = "Shoulder Balance\n",
        subtitle = paste0(
          "<span style='font-size:30px; color:", shoulderBalance_color, ";'>", shoulderBalance, "</span><br><br><br>"
        ),
        x = NULL,
        y = NULL
      ) +
      scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
      scale_y_continuous(limits = c(min_rotation - 25, max_rotation + 25)) +
      scale_color_manual(
        values = c("External Rotation" = "#0099f9", "Internal Rotation" = "#f90099"),
        labels = c("External Rotation" = "ER", "Internal Rotation" = "IR")
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26),
        plot.subtitle = element_markdown(color = "white"),
        axis.text = element_text(color = "lightgrey"),
        legend.title = element_blank(),
        legend.text = element_text(color = "white", size = 13)
      )
    
  } else if (rotation_check > 1) {
    if (nrow(current_rotation) > 0) {
      shoulderBalance <- round(
        mean(current_rotation$ExternalValue, na.rm = TRUE) / 
          mean(current_rotation$InternalValue, na.rm = TRUE), 2
      )
      
      if (is.na(shoulderBalance)) {
        label_description <- NA
      } else if (shoulderBalance >= 0.85 & shoulderBalance <= 1.05) {
        label_description <- "Balanced: You have balanced strength between external rotators and internal rotators"
        shoulderBalance_color <- "#008000"
      } else if (shoulderBalance >= 0.70 & shoulderBalance < 0.85) {
        label_description <- "Imbalanced: Your external rotators are weak compared to your internal rotators"
        shoulderBalance_color <- "#FFFF00"
      } else if (shoulderBalance >= 1.05 & shoulderBalance <= 1.20) {
        label_description <- "Imbalanced: Your internal rotators are weak compared to your external rotators"
        shoulderBalance_color <- "#FFFF00"
      } else if (shoulderBalance < 0.70) {
        label_description <- "Warning: Your external rotators are weak compared to your internal rotators"
        shoulderBalance_color <- "#FF0000"
      } else {
        label_description <- "Warning: Your internal rotators are weak compared to your external rotators"
        shoulderBalance_color <- "#FF0000"
      }
      
      plot_data <- filtered_rotation %>%
        select(Date, ExternalValue, InternalValue) %>%
        pivot_longer(cols = c(ExternalValue, InternalValue), names_to = "Rotation", values_to = "Value") %>%
        mutate(Rotation = recode(Rotation, ExternalValue = "External Rotation", InternalValue = "Internal Rotation"))
      
      rotation_plot <- ggplot(plot_data, aes(x = Date, y = Value, group = Rotation, color = Rotation)) +
        geom_line(linewidth = 2) +
        geom_point(size = 3) +
        labs(
          title = "Shoulder Balance\n",
          subtitle = paste0(
            "<span style='font-size:50px; color:", shoulderBalance_color, ";'>", shoulderBalance, "</span> <span style='font-size:25px;'>Ratio</span><br><br><br>",
            "<span style='font-size:22px;'>", label_description, "</span><br>"
          ),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_rotation - 25, max_rotation + 25)) +
        scale_color_manual(
          values = c("External Rotation" = "#0099f9", "Internal Rotation" = "#f90099"),
          labels = c("External Rotation" = "ER", "Internal Rotation" = "IR")
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          axis.text = element_text(color = "lightgrey"),
          legend.title = element_blank(),
          legend.text = element_text(color = "white", size = 13)
        )
    } else {
      # More than 1 month of data but no May data available
      rotation_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No Recent Data", 
                 size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
        labs(
          title = "Shoulder Balance\n",
          x = NULL,
          y = NULL
        ) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22)
        )
    }
    
  } else if (rotation_check == 1) {
    rotation_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "Not Enough Data", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   Shoulder Balance\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26)
      )
    
  } else {
    rotation_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Data Available", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   Shoulder Balance\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26)
      )
  }
  
  # Save & composite your rotation plot in-place
  rotation_plot_path <- file.path(athlete_dir, paste0(athlete, "-rotation_plot.png"))
  ggsave(rotation_plot_path,
         plot   = rotation_plot,
         width  = 5.75, height = 3.5,
         units  = "in",
         dpi    = 150)
  
  rotation_img <- image_read(rotation_plot_path)
  report_img   <- image_composite(report_img, rotation_img, offset = "+600+2675")
  
  # Free the temporary image from memory
  rm(rotation_img)
  gc()
  
  
  # Filter data for the specific athlete and exercise
  filtered_extensionFlexion <- summarized_extensionFlexion %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  extensionFlexion_check <- filtered_extensionFlexion %>%
    distinct(Month, Year) %>%
    nrow()
  
  min_EF <- filtered_extensionFlexion %>%
    summarise(minValue = min(Value, na.rm = TRUE)) %>%
    pull(minValue)
  
  max_EF <- filtered_extensionFlexion %>%
    summarise(maxValue = max(Value, na.rm = TRUE)) %>%
    pull(maxValue)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_extensionFlexion$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  if (length(unique_months) == 2) {
    breaks_option <- unique_months
  } else {
    breaks_option <- waiver() # Default ggplot behavior
  }
  
  # Filter data for May 2024
  current_extensionFlexion <- filtered_extensionFlexion %>% 
    filter(Month == month & Year == year)
  
  last_extensionFlexion <- filtered_extensionFlexion %>% 
    filter(Date == max(Date))
  
  extensionFlexion_value <- current_extensionFlexion$Value
  extensionFlexion_percentile_numeric <- current_extensionFlexion$Percentile
  extensionFlexion_percentile <- ordinal_suffix(extensionFlexion_percentile_numeric)
  extensionFlexion_rank <- paste0(current_extensionFlexion$Rank, " out of ", current_extensionFlexion$Total)
  
  extensionFlexion_color <- col_numeric(
    palette = c("#FF0000", "#FFFF00", "#008000"), 
    domain = c(0, 100)  # Scale from 0 to 100 percentiles
  )(extensionFlexion_percentile_numeric)
  
  # Check for more than 1 month of data
  if (extensionFlexion_check > 1) {
    if (nrow(current_extensionFlexion) > 0) {
      # Case: More than 1 month and May 2024 data available
      restructured_extensionFlexion <- current_extensionFlexion %>% 
        select(`Month:`, `Career:`) %>%
        pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      extensionFlexion_gt <- restructured_extensionFlexion %>% 
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
        opt_table_font(font = "Helvetica") %>% 
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
      
      # Create plot
      extensionFlexion_plot <- ggplot(filtered_extensionFlexion, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Proteus D2 Extension & Flexion\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", extensionFlexion_value, "</span> <span style='font-size:25px;'>W</span><br><br>",
            "<span style='font-size:50px; color:", extensionFlexion_color, ";'>", extensionFlexion_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", extensionFlexion_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_EF - 50, max_EF + 50)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635),
          axis.text = element_text(color = "lightgrey")
        )
      
    } else {
      # Case: More than 1 month but no May 2024 data
      extensionFlexion_plot <- ggplot(filtered_extensionFlexion, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Proteus D2 Extension & Flexion\n",
          subtitle = " No Recent Data\n",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_EF - 50, max_EF + 50)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      restructured_extensionFlexion <- last_extensionFlexion %>% 
        select(`Career:`) %>%
        pivot_longer(cols = `Career:`, names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      extensionFlexion_gt <- restructured_extensionFlexion %>% 
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
        text_transform(
          locations = cells_body(columns = Value),
          fn = function(x) {
            map_chr(x, ~arrow_icon(as.numeric(.x)))
          }
        )
    }
    
  } else if (extensionFlexion_check == 1) {
    # Case: Only 1 month of data
    if (nrow(current_extensionFlexion) > 0) {
      # Case: Only 1 month, and it's May 2024
      extensionFlexion_plot <- ggplot(filtered_extensionFlexion, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Proteus D2 Extension & Flexion\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", extensionFlexion_value, "</span> <span style='font-size:25px;'>W</span><br><br>",
            "<span style='font-size:50px; color:", extensionFlexion_color, ";'>", extensionFlexion_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", extensionFlexion_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_EF - 50, max_EF + 50)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.855, 0.635),
          axis.text = element_text(color = "lightgrey")
        )
      
      extensionFlexion_gt <- tibble(Metric = "", Value = "") %>%
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
      extensionFlexion_plot <- ggplot(filtered_extensionFlexion, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "   Proteus D2 Extension & Flexion\n",
          subtitle = "    No Recent Data",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_EF - 50, max_EF + 50)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      extensionFlexion_gt <- tibble(Metric = "", Value = "") %>%
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
    extensionFlexion_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Data Available", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   Proteus D2 Extension & Flexion\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26),
        plot.subtitle = element_blank()
      )
    
    extensionFlexion_gt <- tibble(Metric = "", Value = "") %>%
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
  
  # 1. Composite your Extension/Flexion GT table in-place
  extensionFlexion_gt_path <- file.path(athlete_dir, paste0(athlete, "-extensionFlexion_gt.png"))
  gtsave(extensionFlexion_gt, file = extensionFlexion_gt_path, expand = -1)
  
  extf_gt_img <- image_read(extensionFlexion_gt_path) %>%
    image_transparent(color = "black")
  report_img   <- image_composite(report_img, extf_gt_img, offset = "+2150+2640")
  
  rm(extf_gt_img)
  gc()
  
  
  # 2. Composite your Extension/Flexion plot in-place
  extensionFlexion_plot_path <- file.path(athlete_dir, paste0(athlete, "-extensionFlexion_plot.png"))
  ggsave(extensionFlexion_plot_path,
         plot   = extensionFlexion_plot,
         width  = 5.5, height = 3.5,
         units  = "in",
         dpi    = 150)
  
  extf_plot_img <- image_read(extensionFlexion_plot_path)
  report_img     <- image_composite(report_img, extf_plot_img, offset = "+1570+2675")
  
  rm(extf_plot_img)
  gc()
  
  
  if (!is.null(IBSQT_value) && length(IBSQT_value) > 0) {
    if (athlete_level == "Professional") {
      outputOne <- if (IBSQT_value >= 5500) "Focus: Power" else "Focus: Strength"
    } else {
      outputOne <- if (IBSQT_value >= 3500) "Focus: Power" else "Focus: Strength"
    }
  } else if (!is.null(last_IBSQT_value) && length(last_IBSQT_value) > 0) {
    if (athlete_level == "Professional") {
      outputOne <- if (last_IBSQT_value >= 5500) "Focus: Power" else "Focus: Strength"
    } else {
      outputOne <- if (last_IBSQT_value >= 3500) "Focus: Power" else "Focus: Strength"
    }
  } else {
    outputOne <- "Focus: Strength"
  }
  
  if (!is.null(ISOtrunkRotation_value) && length(ISOtrunkRotation_value) > 0) {
    if (athlete_level == "Professional") {
      outputTwo <- if (ISOtrunkRotation_value >= 400) "Focus: Power" else "Focus: Strength"
    } else {
      outputTwo <- if (ISOtrunkRotation_value >= 200) "Focus: Power" else "Focus: Strength"
    }
  } else if (!is.null(last_ISOtrunkRotation_value) && length(last_ISOtrunkRotation_value) > 0) {
    if (athlete_level == "Professional") {
      outputTwo <- if (last_ISOtrunkRotation_value >= 400) "Focus: Power" else "Focus: Strength"
    } else {
      outputTwo <- if (last_ISOtrunkRotation_value >= 200) "Focus: Power" else "Focus: Strength"
    }
  } else {
    outputTwo <- "Focus: Strength"
  }
  
  if (!is.null(SHLDISOY_value) && length(SHLDISOY_value) > 0) {
    if (athlete_level == "Professional") {
      outputThree <- if (SHLDISOY_value >= 200) "Focus: Power" else "Focus: Strength"
    } else {
      outputThree <- if (SHLDISOY_value >= 130) "Focus: Power" else "Focus: Strength"
    }
  } else if (!is.null(last_SHLDISOY_value) && length(last_SHLDISOY_value) > 0) {
    if (athlete_level == "Professional") {
      outputThree <- if (last_SHLDISOY_value >= 200) "Focus: Power" else "Focus: Strength"
    } else {
      outputThree <- if (last_SHLDISOY_value >= 130) "Focus: Power" else "Focus: Strength"
    }
  } else {
    outputThree <- "Focus: Strength"
  }
  
  focus_labels <- ggplot() +
    annotate("text", x = 1, y = 3, label = outputOne, size = 10, color = "white") +
    annotate("text", x = 1, y = 2, label = outputTwo, size = 10, color = "white") +
    annotate("text", x = 1, y = 1, label = outputThree, size = 10, color = "white") +
    xlim(0, 3) + ylim(1, 3) +
    theme_void()
  
  # 1. Save & composite your Focus Labels image in-place
  focus_labels_path <- file.path(athlete_dir, paste0(athlete, " - focus_labels.png"))
  ggsave(focus_labels_path,
         plot   = focus_labels,
         width  = 3, height = 10.25,
         units  = "in",
         dpi    = 150)
  
  focus_labels_img <- image_read(focus_labels_path)
  report_img       <- image_composite(report_img, focus_labels_img, offset = "+185+950")
  
  rm(focus_labels_img)
  gc()
  
  
  # 2. Write out the final Strength Report
  final_report_path <- file.path(athlete_dir, "FuturesStrengthReport.png")
  image_write(report_img,
              path    = final_report_path,
              format  = "png")
  
  # Return the full path for downloadHandler()
  final_report_path
  
}