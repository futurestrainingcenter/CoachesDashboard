library(tidyverse)
library(ggtext)
library(ggpubr)
library(readxl)
library(magick)
library(scales)
library(showtext)
library(fmsb)
library(gt)
library(RPostgres)
library(DBI)

if (file.exists("www/good times rg.otf")) {
  font_add("Good Times", regular = "www/good times rg.otf")
} else {
  warning("Custom font not found; falling back to default.")
}
showtext_auto()

options(chromote.headless = "new")

generate_speed_report <- function(client_data, speed_data, athlete, month, year) { 
  
  report_img <- image_read_pdf("www/Speed Report Template.pdf")
  
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
  
  speed_data <- speed_data %>% 
    filter(Level %in% c("L1", "L2", "L3", "Collegiate", "Professional"), Date < as.Date(cutoff_date))
  
  summarized_thirtyYard <- speed_data %>% 
    filter(!is.na(thirty_yard)) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(min(thirty_yard, na.rm = TRUE), 3), .groups = "drop") %>%
    mutate(
      Percentile = round(percent_rank(-Value) * 100),
      Date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(Date) %>%
    group_by(Month, Year, Level, Gender) %>% 
    mutate(
      Rank = rank(Value, ties.method = "min"),
      Total = n()
    ) %>%
    group_by(Name) %>%
    mutate(`Month:` = round((lag(Value) - Value) / lag(Value) * 100, 1),
           `Career:` = round((first(Value) - Value) / first(Value) * 100, 1)) %>%
    ungroup()
  
  summarized_early_acceleration <- speed_data %>% 
    filter(!is.na(early_acceleration)) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(min(early_acceleration, na.rm = TRUE), 3), .groups = "drop") %>%
    mutate(
      Percentile = round(percent_rank(-Value) * 100),
      Date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(Date) %>%
    group_by(Month, Year, Level, Gender) %>% 
    mutate(
      Rank = rank(Value, ties.method = "min"),
      Total = n()
    ) %>%
    group_by(Name) %>%
    mutate(`Month:` = round((lag(Value) - Value) / lag(Value) * 100, 1),
           `Career:` = round((first(Value) - Value) / first(Value) * 100, 1)) %>%
    ungroup()
  
  summarized_late_acceleration <- speed_data %>% 
    filter(!is.na(late_acceleration)) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(min(late_acceleration, na.rm = TRUE), 3), .groups = "drop") %>%
    mutate(
      Percentile = round(percent_rank(-Value) * 100),
      Date = ymd(paste(Year, Month, "01", sep = "-"))
    ) %>%
    arrange(Date) %>%
    group_by(Month, Year, Level, Gender) %>% 
    mutate(
      Rank = rank(Value, ties.method = "min"),
      Total = n()
    ) %>%
    group_by(Name) %>%
    mutate(`Month:` = round((lag(Value) - Value) / lag(Value) * 100, 1),
           `Career:` = round((first(Value) - Value) / first(Value) * 100, 1)) %>%
    ungroup()
  
  summarized_maxVelo <- speed_data %>% 
    filter(!is.na(max_velocity)) %>% 
    group_by(Name, Month, Year, Level, Gender) %>% 
    summarise(Value = round(max(max_velocity, na.rm = TRUE), 1), .groups = "drop") %>%
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
  
  # con <- dbConnect(RPostgres::Postgres(),
  #                  host = Sys.getenv("DB_HOST"),
  #                  port = Sys.getenv("DB_PORT"),
  #                  dbname = Sys.getenv("DB_NAME"),
  #                  user = Sys.getenv("DB_USER"),
  #                  password = Sys.getenv("DB_PASSWORD"))
  # 
  # vuemotion_data <- dbGetQuery(con, "SELECT * FROM tbl_vuemotion_flyanalysis_10yd") %>% 
  #   mutate(athlete_name = str_squish(athlete_name) %>% str_to_title())
  # 
  # # AWS Credentials and S3 Configuration
  # access_key <- Sys.getenv("AWS_ACCESS_KEY_ID")
  # secret_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
  # region <- Sys.getenv("AWS_REGION")
  # bucket <- Sys.getenv("AWS_BUCKET")
  # folder_prefix <- Sys.getenv("AWS_FOLDER_PREFIX")
  # 
  # # Configure AWS S3 Client
  # s3 <- paws::s3(
  #   config = list(
  #     credentials = list(
  #       creds = list(
  #         access_key_id = access_key,
  #         secret_access_key = secret_key
  #       )
  #     )
  #   )
  # )
  # 
  # # List objects in the S3 bucket
  # s3_objects <- s3$list_objects_v2(Bucket = bucket, Prefix = folder_prefix)
  # 
  # # Extract the image file names
  # image_files <- sapply(s3_objects$Contents, function(x) x$Key)
  # image_files <- image_files[grepl("\\.jpg$", image_files)]  # Filter only .jpg files
  # 
  # # Create a data frame with extracted details
  # image_data <- data.frame(
  #   image_path = image_files,
  #   file_name = basename(image_files)
  # ) %>%
  #   mutate(
  #     prefix = case_when(
  #       str_starts(file_name, "Natural_SF_") ~ "Natural_SF",  # More specific first!
  #       str_starts(file_name, "Natural_") ~ "Natural",
  #       str_starts(file_name, "SF_BBGD_") ~ "SF_BBGD",
  #       TRUE ~ NA_character_
  #     ),
  #     video_name = file_name %>%
  #       str_remove("^(Natural_SF_|Natural_|SF_BBGD_)") %>%  # Remove prefixes in correct order
  #       str_remove("\\.jpg$"),  # Remove .jpg extension
  #     step_count = str_extract(video_name, "_[567]Steps$") %>%  # Extract the suffix (_5Steps, _6Steps, _7Steps)
  #       str_remove("^_"),  # Remove leading underscore
  #     video_name = str_remove(video_name, "_[567]Steps$")  # Remove suffix from video_name
  #   ) %>%
  #   drop_na(prefix) %>%
  #   select(video_name, prefix, step_count, image_path)  # Include step_count in final output
  # 
  # # Reshape to wide format so each `unique_id` has separate columns for each image type
  # image_wide <- image_data %>%
  #   pivot_wider(names_from = prefix, values_from = image_path)
  # 
  # # Merge with vuemotion_data
  # merged_vuemotion <- vuemotion_data %>%
  #   left_join(image_wide, by = "video_name") %>%
  #   filter(!is.na(step_count)) %>% 
  #   distinct()
  
  # Define a temp directory for this athlete
  athlete_dir <- file.path(tempdir(), athlete)
  if (!dir.exists(athlete_dir)) {
    dir.create(athlete_dir, recursive = TRUE)
  }
  
  # Load and process the check-ins data
  attendanceData <- speed_data %>% 
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
      } else if (score < 0.75) {
        return("#FF0000")
      } else if (score >= 0.75 & score < 1.5) {
        return("#FFA500")
      } else {
        return("#008000")
      }
    })
  }
  
  # Plotting
  attendance_plot <- ggplot(attendance_plot_data, aes(x = Name, y = `Attendance Score`)) +
    # Background bars
    geom_col(aes(y = 2), alpha = 0.75, fill = "gray30", color = "black") +
    
    # Foreground colored bars with black borders
    geom_col(aes(fill = get_color(`Attendance Score`)), color = "black") +
    
    # Score text
    geom_text(aes(y = 1, label = paste(attendance_score)), size = 12, fontface = "bold", color = "white") +
    
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
  
  
  # Filter data for the athlete and exercise
  filtered_thirtyYard <- summarized_thirtyYard %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  thirtyYard_check <- filtered_thirtyYard %>%
    distinct(Month, Year) %>%
    nrow()
  
  min_thirtyYard <- filtered_thirtyYard %>%
    summarise(minValue = min(Value, na.rm = TRUE)) %>%
    pull(minValue)
  
  max_thirtyYard <- filtered_thirtyYard %>%
    summarise(maxValue = max(Value, na.rm = TRUE)) %>%
    pull(maxValue)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_thirtyYard$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  if (length(unique_months) == 2) {
    breaks_option <- unique_months
  } else {
    breaks_option <- waiver() # Default ggplot behavior
  }
  
  # Filter data for November 2024
  current_thirtyYard <- filtered_thirtyYard %>% 
    filter(Date == max(Date))
  
  thirtyYard_value <- current_thirtyYard$Value
  thirtyYard_percentile_numeric <- current_thirtyYard$Percentile
  thirtyYard_percentile <- ordinal_suffix(thirtyYard_percentile_numeric)
  thirtyYard_rank <- paste0(current_thirtyYard$Rank, " out of ", current_thirtyYard$Total)
  
  thirtyYard_color <- col_numeric(
    palette = c("#FF0000", "#FFFF00", "#008000"), 
    domain = c(0, 100)  # Scale from 0 to 100 percentiles
  )(thirtyYard_percentile_numeric)
  
  # Check for more than 1 month of data
  if (thirtyYard_check > 1) {
    if (nrow(current_thirtyYard) > 0) {
      # Case: More than 1 month and November 2024 data available
      restructured_thirtyYard <- current_thirtyYard %>% 
        select(`Month:`, `Career:`) %>%
        pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      thirtyYard_gt <- restructured_thirtyYard %>% 
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
      thirtyYard_plot <- ggplot(filtered_thirtyYard, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "30 Yard Sprint\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", thirtyYard_value, "</span> <span style='font-size:25px;'>sec</span><br><br>",
            "<span style='font-size:50px; color:", thirtyYard_color, ";'>", thirtyYard_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", thirtyYard_rank),
          x = NULL,
          y = NULL,
          caption = "*Y-axis inverted for visual purposes"
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_reverse(limits = c(max_thirtyYard + 0.1, min_thirtyYard - 0.1)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.caption = element_text(color = "white", size = 15, margin = margin(t = 15)),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.8, 0.705), 
          axis.text = element_text(color = "lightgrey")
        )
      
    } else {
      # Case: More than 1 month but no November 2024 data
      thirtyYard_plot <- ggplot(filtered_thirtyYard, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "30 Yard Sprint\n",
          subtitle = "No Recent Data\n\n",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      restructured_thirtyYard <- current_thirtyYard %>% 
        select(`Career:`) %>%
        pivot_longer(cols = `Career:`, names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      thirtyYard_gt <- restructured_thirtyYard %>% 
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
    
  } else if (thirtyYard_check == 1) {
    # Case: Only 1 month of data
    if (nrow(current_thirtyYard) > 0) {
      # Case: Only 1 month, and it's November 2024
      thirtyYard_plot <- ggplot(filtered_thirtyYard, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "30 Yard Sprint\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", thirtyYard_value, "</span> <span style='font-size:25px;'>sec</span><br><br>",
            "<span style='font-size:50px; color:", thirtyYard_color, ";'>", thirtyYard_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", thirtyYard_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.8, 0.705), 
          axis.text = element_text(color = "lightgrey")
        )
      
      thirtyYard_gt <- tibble(Metric = "", Value = "") %>%
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
      # Case: Only 1 month but not November 2024
      thirtyYard_plot <- ggplot(filtered_thirtyYard, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "   30 Yard Sprint\n",
          subtitle = "    No Recent Data",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      thirtyYard_gt <- tibble(Metric = "", Value = "") %>%
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
    thirtyYard_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Data Available", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   30 Yard Sprint\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26),
        plot.subtitle = element_blank()
      )
    
    thirtyYard_gt <- tibble(Metric = "", Value = "") %>%
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
  
  # 1. Composite your Thirty Yard GT table in-place
  thirtyYard_gt_path <- file.path(athlete_dir, paste0(athlete, "-thirtyYard_gt.png"))
  gtsave(thirtyYard_gt, file = thirtyYard_gt_path, expand = -1)
  
  thirtyYard_gt_img <- image_read(thirtyYard_gt_path) %>%
    image_transparent(color = "black")
  report_img        <- image_composite(report_img, thirtyYard_gt_img, offset = "+600+550")
  
  rm(thirtyYard_gt_img)
  gc()
  
  
  # 2. Composite your Thirty Yard plot in-place
  thirtyYard_plot_path <- file.path(athlete_dir, paste0(athlete, "-thirtyYard_plot.png"))
  ggsave(thirtyYard_plot_path,
         plot   = thirtyYard_plot,
         width  = 5.25, height = 4.75,
         units  = "in",
         dpi    = 150)
  
  thirtyYard_plot_img <- image_read(thirtyYard_plot_path)
  report_img          <- image_composite(report_img, thirtyYard_plot_img, offset = "+100+600")
  
  rm(thirtyYard_plot_img)
  gc()
  
  
  # Filter data for the athlete and exercise
  filtered_acceleration <- summarized_early_acceleration %>% 
    filter(Name == athlete)
  
  filtered_late_acceleration <- summarized_late_acceleration %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  acceleration_check <- filtered_acceleration %>%
    distinct(Month, Year) %>%
    nrow()
  
  min_acceleration <- filtered_acceleration %>%
    summarise(minValue = min(Value, na.rm = TRUE)) %>%
    pull(minValue)
  
  max_acceleration <- filtered_late_acceleration %>%
    summarise(maxValue = max(Value, na.rm = TRUE)) %>%
    pull(maxValue)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_acceleration$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  if (length(unique_months) == 2) {
    breaks_option <- unique_months
  } else {
    breaks_option <- waiver() # Default ggplot behavior
  }
  
  # Filter data for November 2024
  current_acceleration <- filtered_acceleration %>% 
    filter(Date == max(Date))
  
  acceleration_value <- current_acceleration$Value
  acceleration_percentile_numeric <- current_acceleration$Percentile
  acceleration_percentile <- ordinal_suffix(acceleration_percentile_numeric)
  acceleration_rank <- paste0(current_acceleration$Rank, " out of ", current_acceleration$Total)
  
  acceleration_color <- col_numeric(
    palette = c("#FF0000", "#FFFF00", "#008000"), 
    domain = c(0, 100)  # Scale from 0 to 100 percentiles
  )(acceleration_percentile_numeric)
  
  # Check for more than 1 month of data
  if (acceleration_check > 1) {
    if (nrow(current_acceleration) > 0) {
      # Case: More than 1 month and November 2024 data available
      restructured_acceleration <- current_acceleration %>% 
        select(`Month:`, `Career:`) %>%
        pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      acceleration_gt <- restructured_acceleration %>% 
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
      acceleration_plot <- ggplot() +
        # Early acceleration
        geom_line(data = filtered_acceleration, aes(x = Date, y = Value, color = "Early"), linewidth = 2) +
        geom_point(data = filtered_acceleration, aes(x = Date, y = Value, color = "Early"), size = 3) +
        # Late acceleration
        geom_line(data = filtered_late_acceleration, aes(x = Date, y = Value, color = "Late"), linewidth = 2) +
        geom_point(data = filtered_late_acceleration, aes(x = Date, y = Value, color = "Late"), size = 3) +
        labs(
          title = "Acceleration\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", acceleration_value, "</span> <span style='font-size:25px;'>sec</span><br><br>",
            "<span style='font-size:50px; color:", acceleration_color, ";'>", acceleration_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", acceleration_rank),
          x = NULL,
          y = NULL,
          caption = "*Y-axis inverted for visual purposes",
          color = "Metric"  # Legend title
        ) +
        scale_color_manual(values = c("Early" = "#0099f9", "Late" = "#f90099")) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_reverse(limits = c(max_acceleration + 0.1, min_acceleration - 0.1)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.caption = element_text(color = "white", size = 15, margin = margin(t = 15)),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.8, 0.705), 
          axis.text = element_text(color = "lightgrey"),
          legend.title = element_blank(),
          legend.text = element_text(color = "white")
        )
      
    } else {
      # Case: More than 1 month but no November 2024 data
      acceleration_plot <- ggplot(filtered_acceleration, aes(x = Date, y = Value)) +
        # Early acceleration
        geom_line(data = filtered_acceleration, aes(x = Date, y = Value, color = "Early"), linewidth = 2) +
        geom_point(data = filtered_acceleration, aes(x = Date, y = Value, color = "Early"), size = 3) +
        # Late acceleration
        geom_line(data = filtered_late_acceleration, aes(x = Date, y = Value, color = "Late"), linewidth = 2) +
        geom_point(data = filtered_late_acceleration, aes(x = Date, y = Value, color = "Late"), size = 3) +
        labs(
          title = "Acceleration\n",
          subtitle = "No Recent Data\n\n",
          x = NULL,
          y = NULL
        ) +
        scale_color_manual(values = c("Early" = "#0099f9", "Late" = "#f90099")) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey"),
          legend.title = element_blank(),
          legend.text = element_text(color = "white")
        )
      
      restructured_acceleration <- current_acceleration %>% 
        select(`Career:`) %>%
        pivot_longer(cols = `Career:`, names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      acceleration_gt <- restructured_acceleration %>% 
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
    
  } else if (acceleration_check == 1) {
    # Case: Only 1 month of data
    if (nrow(current_acceleration) > 0) {
      # Case: Only 1 month, and it's November 2024
      acceleration_plot <- ggplot(filtered_acceleration, aes(x = Date, y = Value)) +
        # Early acceleration
        geom_point(data = filtered_acceleration, aes(x = Date, y = Value, color = "Early"), size = 3) +
        # Late acceleration
        geom_point(data = filtered_late_acceleration, aes(x = Date, y = Value, color = "Late"), size = 3) +
        labs(
          title = "Acceleration\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", acceleration_value, "</span> <span style='font-size:25px;'>sec</span><br><br>",
            "<span style='font-size:50px; color:", acceleration_color, ";'>", acceleration_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", acceleration_rank),
          x = NULL,
          y = NULL
        ) +
        scale_color_manual(values = c("Early" = "#0099f9", "Late" = "#f90099")) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.8, 0.705), 
          axis.text = element_text(color = "lightgrey"),
          legend.title = element_blank(),
          legend.text = element_text(color = "white")
        )
      
      acceleration_gt <- tibble(Metric = "", Value = "") %>%
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
      # Case: Only 1 month but not November 2024
      acceleration_plot <- ggplot(filtered_acceleration, aes(x = Date, y = Value)) +
        # Early acceleration
        geom_point(data = filtered_acceleration, aes(x = Date, y = Value, color = "Early"), size = 3) +
        # Late acceleration
        geom_point(data = filtered_late_acceleration, aes(x = Date, y = Value, color = "Late"), size = 3) +
        labs(
          title = "   Acceleration\n",
          subtitle = "    No Recent Data",
          x = NULL,
          y = NULL
        ) +
        scale_color_manual(values = c("Early" = "#0099f9", "Late" = "#f90099")) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey"),
          legend.title = element_blank(),
          legend.text = element_text(color = "white")
        )
      
      acceleration_gt <- tibble(Metric = "", Value = "") %>%
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
    acceleration_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Data Available", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   Acceleration\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26),
        plot.subtitle = element_blank()
      )
    
    acceleration_gt <- tibble(Metric = "", Value = "") %>%
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
  
  # 1. Composite your Acceleration GT table in-place
  acceleration_gt_path <- file.path(athlete_dir, paste0(athlete, "-acceleration_gt.png"))
  gtsave(acceleration_gt, file = acceleration_gt_path, expand = -1)
  
  acc_gt_img  <- image_read(acceleration_gt_path) %>%
    image_transparent(color = "black")
  report_img  <- image_composite(report_img, acc_gt_img, offset = "+600+1480")
  
  rm(acc_gt_img)
  gc()
  
  
  # 2. Composite your Acceleration plot in-place
  acceleration_plot_path <- file.path(athlete_dir, paste0(athlete, "-acceleration_plot.png"))
  ggsave(acceleration_plot_path,
         plot   = acceleration_plot,
         width  = 5.25, height = 4.75,
         units  = "in",
         dpi    = 150)
  
  acc_plot_img <- image_read(acceleration_plot_path)
  report_img   <- image_composite(report_img, acc_plot_img, offset = "+100+1525")
  
  rm(acc_plot_img)
  gc()
  
  
  # Filter data for the athlete and exercise
  filtered_maxVelo <- summarized_maxVelo %>% 
    filter(Name == athlete)
  
  # Calculate the number of unique combinations of month and year
  maxVelo_check <- filtered_maxVelo %>%
    distinct(Month, Year) %>%
    nrow()
  
  min_maxVelo <- filtered_maxVelo %>%
    summarise(minValue = min(Value, na.rm = TRUE)) %>%
    pull(minValue)
  
  max_maxVelo <- filtered_maxVelo %>%
    summarise(maxValue = max(Value, na.rm = TRUE)) %>%
    pull(maxValue)
  
  # Calculate the unique months in the data
  unique_months <- unique(floor_date(filtered_maxVelo$Date, "month"))
  
  # Determine if there are exactly 2 unique months
  if (length(unique_months) == 2) {
    breaks_option <- unique_months
  } else {
    breaks_option <- waiver() # Default ggplot behavior
  }
  
  # Filter data for November 2024
  current_maxVelo <- filtered_maxVelo %>% 
    filter(Date == max(Date))
  
  maxVelo_value <- current_maxVelo$Value
  maxVelo_percentile_numeric <- current_maxVelo$Percentile
  maxVelo_percentile <- ordinal_suffix(maxVelo_percentile_numeric)
  maxVelo_rank <- paste0(current_maxVelo$Rank, " out of ", current_maxVelo$Total)
  
  maxVelo_color <- col_numeric(
    palette = c("#FF0000", "#FFFF00", "#008000"), 
    domain = c(0, 100)  # Scale from 0 to 100 percentiles
  )(maxVelo_percentile_numeric)
  
  # Check for more than 1 month of data
  if (maxVelo_check > 1) {
    if (nrow(current_maxVelo) > 0) {
      # Case: More than 1 month and November 2024 data available
      restructured_maxVelo <- current_maxVelo %>% 
        select(`Month:`, `Career:`) %>%
        pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      maxVelo_gt <- restructured_maxVelo %>% 
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
      maxVelo_plot <- ggplot(filtered_maxVelo, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Max Velocity\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", maxVelo_value, "</span> <span style='font-size:25px;'>MPH</span><br><br>",
            "<span style='font-size:50px; color:", maxVelo_color, ";'>", maxVelo_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", maxVelo_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        scale_y_continuous(limits = c(min_maxVelo - 0.5, max_maxVelo + 0.5)) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.8, 0.705), 
          axis.text = element_text(color = "lightgrey")
        )
      
    } else {
      # Case: More than 1 month but no November 2024 data
      maxVelo_plot <- ggplot(filtered_maxVelo, aes(x = Date, y = Value)) +
        geom_line(color = "#0099f9", linewidth = 2) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Max Velocity\n",
          subtitle = "No Recent Data\n\n",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      restructured_maxVelo <- current_maxVelo %>% 
        select(`Career:`) %>%
        pivot_longer(cols = `Career:`, names_to = "Metric", values_to = "Value")
      
      # Create the gt table with styling
      maxVelo_gt <- restructured_maxVelo %>% 
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
    
  } else if (maxVelo_check == 1) {
    # Case: Only 1 month of data
    if (nrow(current_maxVelo) > 0) {
      # Case: Only 1 month, and it's November 2024
      maxVelo_plot <- ggplot(filtered_maxVelo, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "Max Velocity\n",
          subtitle = paste0(
            "<span style='font-size:50px;'>", maxVelo_value, "</span> <span style='font-size:25px;'>mph</span><br><br>",
            "<span style='font-size:50px; color:", maxVelo_color, ";'>", maxVelo_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
          ),
          tag = paste0("Rank: ", maxVelo_rank),
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_markdown(color = "white"),
          plot.tag = element_text(color = "white", size = 22),
          plot.tag.position = c(0.8, 0.705), 
          axis.text = element_text(color = "lightgrey")
        )
      
      maxVelo_gt <- tibble(Metric = "", Value = "") %>%
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
      # Case: Only 1 month but not November 2024
      maxVelo_plot <- ggplot(filtered_maxVelo, aes(x = Date, y = Value)) +
        geom_point(color = "#0099f9", size = 3) +
        labs(
          title = "   Max Velocity\n",
          subtitle = "    No Recent Data",
          x = NULL,
          y = NULL
        ) +
        scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
        theme_void() +
        theme(
          plot.title = element_text(color = "white", size = 26),
          plot.subtitle = element_text(color = "#FF0000", size = 22),
          axis.text = element_text(color = "lightgrey")
        )
      
      maxVelo_gt <- tibble(Metric = "", Value = "") %>%
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
    maxVelo_plot <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No Data Available", 
               size = 8, color = "white", hjust = 0.5, vjust = 0.5) +
      labs(
        title = "   Max Velocity\n",
        x = NULL,
        y = NULL
      ) +
      theme_void() +
      theme(
        plot.title = element_text(color = "white", size = 26),
        plot.subtitle = element_blank()
      )
    
    maxVelo_gt <- tibble(Metric = "", Value = "") %>%
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
  
  # 1. Composite your Max Velo GT table in‐place
  maxVelo_gt_path <- file.path(athlete_dir, paste0(athlete, "- maxVelo_gt.png"))
  gtsave(maxVelo_gt, file = maxVelo_gt_path, expand = -1)
  
  maxVelo_gt_img <- image_read(maxVelo_gt_path) %>%
    image_transparent(color = "black")
  report_img     <- image_composite(report_img, maxVelo_gt_img, offset = "+600+2405")
  
  rm(maxVelo_gt_img)
  gc()
  
  
  # 2. Composite your Max Velo plot in‐place
  maxVelo_plot_path <- file.path(athlete_dir, paste0(athlete, " - maxVelo_plot.png"))
  ggsave(maxVelo_plot_path,
         plot   = maxVelo_plot,
         width  = 5.25, height = 4.5,
         units  = "in",
         dpi    = 150)
  
  maxVelo_plot_img <- image_read(maxVelo_plot_path)
  report_img       <- image_composite(report_img, maxVelo_plot_img, offset = "+100+2450")
  
  rm(maxVelo_plot_img)
  gc()
  
  
  # # --- Acceleration Kinogram Processing ---
  # vuemotion_Acc_data <- merged_vuemotion %>%
  #   filter(
  #     run_type     == "Acc",
  #     athlete_name == athlete,
  #     !is.na(Natural),
  #     !is.na(Natural_SF),
  #     !is.na(SF_BBGD)
  #   )
  # 
  # if (nrow(vuemotion_Acc_data) > 0) {
  #   filtered_acc_data <- vuemotion_Acc_data %>%
  #     filter(`Video ID` == max(`Video ID`, na.rm = TRUE))
  #   
  #   # Download & composite Natural image
  #   local_file <- tempfile(fileext = ".jpg")
  #   s3$download_file(Bucket = bucket, Key = filtered_acc_data$Natural, Filename = local_file)
  #   raw_acc_img <- image_read(local_file)
  #   
  #   acc_crop <- raw_acc_img %>%
  #     image_crop(
  #       geometry = if (filtered_acc_data$step_count == "5Steps")
  #         "4100x475+250+75" else "4200x475+200+75"
  #     ) %>%
  #     image_resize(if (filtered_acc_data$step_count == "5Steps") "42%" else "35%")
  #   
  #   report_img <- image_composite(report_img, acc_crop, offset = "+965+650")
  #   rm(raw_acc_img, acc_crop); gc()
  #   
  #   # Download & composite SF_BBGD image (two crops)
  #   local_file <- tempfile(fileext = ".jpg")
  #   s3$download_file(Bucket = bucket, Key = filtered_acc_data$SF_BBGD, Filename = local_file)
  #   raw_sfbgd <- image_read(local_file)
  #   
  #   if (filtered_acc_data$step_count == "5Steps") {
  #     crop1 <- image_crop(raw_sfbgd, "650x475+2800+75")
  #     report_img <- image_composite(report_img, crop1, offset = "+1050+1700")
  #     rm(crop1); gc()
  #     
  #     crop2 <- image_crop(raw_sfbgd, "525x525+2875+525")
  #     report_img <- image_composite(report_img, crop2, offset = "+1800+1675")
  #     rm(crop2); gc()
  #     
  #   } else if (filtered_acc_data$step_count == "6Steps") {
  #     crop1 <- image_crop(raw_sfbgd, "525x475+1725+75")
  #     report_img <- image_composite(report_img, crop1, offset = "+1050+1700")
  #     rm(crop1); gc()
  #     
  #     crop2 <- image_crop(raw_sfbgd, "525x500+3050+550")
  #     report_img <- image_composite(report_img, crop2, offset = "+1800+1675")
  #     rm(crop2); gc()
  #     
  #   } else {
  #     crop1 <- image_crop(raw_sfbgd, "525x475+2050+75")
  #     report_img <- image_composite(report_img, crop1, offset = "+1050+1700")
  #     rm(crop1); gc()
  #     
  #     crop2 <- image_crop(raw_sfbgd, "525x500+2000+550")
  #     report_img <- image_composite(report_img, crop2, offset = "+1800+1675")
  #     rm(crop2); gc()
  #   }
  #   rm(raw_sfbgd); gc()
  #   
  #   # Add upload date label
  #   acc_date <- ggplot() +
  #     annotate("text", x = 0.5, y = 0.5,
  #              label = paste("Processed on:", filtered_acc_data$upload_date),
  #              size = 4, hjust = 0.5, vjust = 0.5, color = "white") +
  #     theme_void()
  #   
  #   acc_date_path <- file.path(athlete_dir, paste0(athlete, " - acc_date.png"))
  #   ggsave(acc_date_path, plot = acc_date, width = 3, height = 1, units = "in", dpi = 150)
  #   
  #   date_img   <- image_read(acc_date_path)
  #   report_img <- image_composite(report_img, date_img, offset = "+2135+845")
  #   rm(date_img); gc()
  #   
  # } else {
  #   vuemotion_acc_plot <- ggplot() +
  #     annotate("text", x = 0.5, y = 0.5,
  #              label = "No Vuemotion Kinograms Available for This Run Type",
  #              size = 10, hjust = 0.5, vjust = 0.5, color = "white") +
  #     theme_void()
  #   
  #   acc_fallback_path <- file.path(athlete_dir, paste0(athlete, " - vuemotion_acc_plot.png"))
  #   ggsave(acc_fallback_path, plot = vuemotion_acc_plot, width = 5, height = 1, units = "in", dpi = 200)
  #   
  #   fallback_img <- image_read(acc_fallback_path)
  #   report_img   <- image_composite(report_img, fallback_img, offset = "+1200+625")
  #   rm(fallback_img); gc()
  # }
  # 
  # # --- Fly Kinogram Processing ---
  # vuemotion_fly_data <- merged_vuemotion %>%
  #   filter(
  #     run_type     == "Fly",
  #     athlete_name == athlete,
  #     !is.na(Natural),
  #     !is.na(Natural_SF),
  #     !is.na(SF_BBGD)
  #   )
  # 
  # if (nrow(vuemotion_fly_data) > 0) {
  #   latest_fly <- vuemotion_fly_data %>%
  #     filter(`Video ID` == max(`Video ID`, na.rm = TRUE))
  #   
  #   # Natural Fly image
  #   local_file <- tempfile(fileext = ".jpg")
  #   s3$download_file(Bucket = bucket, Key = latest_fly$Natural, Filename = local_file)
  #   raw_fly <- image_read(local_file)
  #   
  #   fly_crop  <- raw_fly %>%
  #     image_crop("4150x475+150+75") %>%
  #     image_resize("35%")
  #   report_img <- image_composite(report_img, fly_crop, offset = "+970+1100")
  #   rm(raw_fly, fly_crop); gc()
  #   
  #   # SF_BBGD Fly image (three crops)
  #   local_file <- tempfile(fileext = ".jpg")
  #   s3$download_file(Bucket = bucket, Key = latest_fly$SF_BBGD, Filename = local_file)
  #   raw_fly_sfbgd <- image_read(local_file)
  #   
  #   # posture
  #   posture <- raw_fly_sfbgd %>%
  #     image_crop("525x475+200+575") %>%
  #     image_trim()
  #   report_img <- image_composite(report_img, posture, offset = "+1025+2600")
  #   rm(posture); gc()
  #   
  #   # leg recovery
  #   recovery <- raw_fly_sfbgd %>%
  #     image_crop("525x475+2325+575") %>%
  #     image_trim()
  #   report_img <- image_composite(report_img, recovery, offset = "+1600+2600")
  #   rm(recovery); gc()
  #   
  #   # foot placement
  #   placement <- raw_fly_sfbgd %>%
  #     image_crop("525x475+1590+575") %>%
  #     image_trim()
  #   report_img <- image_composite(report_img, placement, offset = "+2125+2600")
  #   rm(placement, raw_fly_sfbgd); gc()
  #   
  #   # Add upload date label
  #   fly_date <- ggplot() +
  #     annotate("text", x = 0.5, y = 0.5,
  #              label = paste("Processed on:", latest_fly$upload_date),
  #              size = 4, hjust = 0.5, vjust = 0.5, color = "white") +
  #     theme_void()
  #   
  #   fly_date_path <- file.path(athlete_dir, paste0(athlete, " - fly_date.png"))
  #   ggsave(fly_date_path, plot = fly_date, width = 3, height = 1, units = "in", dpi = 150)
  #   
  #   fly_date_img <- image_read(fly_date_path)
  #   report_img    <- image_composite(report_img, fly_date_img, offset = "+2135+1285")
  #   rm(fly_date_img); gc()
  #   
  # } else {
  #   vuemotion_fly_plot <- ggplot() +
  #     annotate("text", x = 0.5, y = 0.5,
  #              label = "No Vuemotion Kinograms Available for This Run Type",
  #              size = 10, hjust = 0.5, vjust = 0.5, color = "white") +
  #     theme_void()
  #   
  #   fly_fallback_path <- file.path(athlete_dir, paste0(athlete, " - rotation_plot.png"))
  #   ggsave(fly_fallback_path, plot = vuemotion_fly_plot, width = 5, height = 1, units = "in", dpi = 200)
  #   
  #   fallback_img <- image_read(fly_fallback_path)
  #   report_img   <- image_composite(report_img, fallback_img, offset = "+1200+1085")
  #   rm(fallback_img); gc()
  # }
  
  # Final write-out
  final_report_path <- file.path(athlete_dir, "FuturesSpeedReport.png")
  image_write(report_img,
              path    = final_report_path,
              format  = "png")
  
  # Return for downloadHandler
  final_report_path
  
}