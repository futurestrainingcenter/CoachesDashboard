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

generate_hitting_report <- function(client_data, hitting_data, hittrax_data, athlete, month, year) {  
  
  report_img <- image_read_pdf("www/Hitting Report Template.pdf")
  
  # Spray chart dimensions
  A <- sqrt(8)/2
  B <- A/2
  C <- 0.3 * sqrt(8)
  D <- C * sin(pi/6)
  f1 <- function(a, b) c(0, seq(a, b, length.out = 100), 0)
  f2 <- function(a, b) c(0, sqrt(2.0001 - seq(a, b, len = 100)^2) + A, 0)
  f3 <- function(a, b) c(0, sqrt(C^2 - seq(a, b, length.out = 100)^2) + C, 0)
  
  # Arrow icon function with inlined color logic for both percentage and value
  arrow_icon <- function(value_string) {
    if (grepl("\\(", value_string)) {
      # Has both percent and value
      percent_value_split <- strsplit(value_string, " \\(")[[1]]
      percent_change <- as.numeric(percent_value_split[1])
      value_change <- gsub("\\)", "", percent_value_split[2])
      
      if (is.na(percent_change)) {
        return(html(paste0("<span>", value_change, "%</span>")))
      }
      
      if (percent_change > 0.01) {
        return(html(paste0("<span style='color:green;'>&#9650; ", percent_change, "% (", value_change, "%)</span>")))
      } else if (percent_change < -0.01) {
        return(html(paste0("<span style='color:red;'>&#9660; ", percent_change, "% (", value_change, "%)</span>")))
      } else {
        return(html(paste0("<span>&#8212; ", percent_change, "% (", value_change, "%)</span>")))
      }
      
    } else {
      # Only value present (e.g., for Barrel %)
      value_num <- suppressWarnings(as.numeric(value_string))
      
      if (is.na(value_num)) {
        return(html(paste0("<span>", value_string, "</span>")))
      } else if (value_num > 0.01) {
        return(html(paste0("<span style='color:green;'>", value_string, "%</span>")))
      } else if (value_num < -0.01) {
        return(html(paste0("<span style='color:red;'>", value_string, "%</span>")))
      } else {
        return(html("<span>&#8212;</span>"))
      }
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
  
  hitting_data <- hitting_data %>% 
    filter(Level %in% c("L1", "L2", "L3", "Collegiate", "Professional"), Date < as.Date(cutoff_date))
  
  hittrax_data <- hittrax_data %>% 
    filter(Date < as.Date(cutoff_date))
  
  normalize_ope <- function(ope_values) {
    ideal_min <- 65
    ideal_max <- 85
    perfect   <- 72.5
    min_bound <- 25
    max_bound <- 125
    
    sapply(ope_values, function(x) {
      if (is.na(x)) {
        return(NA_real_)
      } else if (x <= min_bound || x >= max_bound) {
        return(0)
      } else if (x >= perfect) {
        if (x <= ideal_max) {
          return(round(100 - ((x - perfect) / (ideal_max - perfect))^2 * 25))
        } else {
          return(round(max(0, 75 - ((x - ideal_max) / (max_bound - ideal_max))^2 * 75)))
        }
      } else {
        if (x >= ideal_min) {
          return(round(100 - ((perfect - x) / (perfect - ideal_min))^2 * 25))
        } else {
          return(round(max(0, 75 - ((ideal_min - x) / (ideal_min - min_bound))^2 * 75)))
        }
      }
    })
  }
  
  normalize_vba <- function(vba_values) {
    best_min <- -30
    best_max <- -20
    min_bound <- -50 
    max_bound <- 0
    
    sapply(vba_values, function(x) {
      if (is.na(x)) {
        return(NA_real_)
      } else if (x >= best_min && x <= best_max) {
        return(100)
      } else if (x < best_min && x >= min_bound) {
        return(round(100 * (x - min_bound) / (best_min - min_bound)))
      } else if (x > best_max && x <= max_bound) {
        return(round(100 * (max_bound - x) / (max_bound - best_max)))
      } else {
        return(0)
      }
    })
  }
  
  normalize_attack_angle <- function(attack_angle_values) {
    best_min <- 6      # Optimal lower bound
    best_max <- 12     # Optimal upper bound
    min_bound <- -10   # Absolute lower bound
    max_bound <- 25    # Absolute upper bound
    
    sapply(attack_angle_values, function(x) {
      if (is.na(x)) {
        return(NA_real_)
      } else if (x >= best_min && x <= best_max) {
        return(100)
      } else if (x < best_min && x >= min_bound) {
        return(round(100 * (x - min_bound) / (best_min - min_bound)))
      } else if (x > best_max && x <= max_bound) {
        return(round(100 * (max_bound - x) / (max_bound - best_max)))
      } else {
        return(0)
      }
    })
  }
  
  hitting_summary <- hitting_data %>% 
    group_by(Name, Level, Gender, Month, Year) %>% 
    summarise(
      MaxVel = if(all(is.na(MaxVel))) NA_real_ else max(MaxVel, na.rm = TRUE),
      AvgVel = if(all(is.na(AvgVel))) NA_real_ else round(mean(AvgVel, na.rm = TRUE), 1),
      MaxDist  = if(all(is.na(MaxDist))) NA_real_ else max(MaxDist, na.rm = TRUE),
      AvgDist  = if(all(is.na(AvgDist))) NA_real_ else round(mean(AvgDist, na.rm = TRUE)),
      BatSpeed = if(all(is.na(bat_speed))) NA_real_ else mean(bat_speed, na.rm = TRUE),
      RotAcc = if(all(is.na(rotational_acceleration))) NA_real_ else mean(rotational_acceleration, na.rm = TRUE),
      TTC = if(all(is.na(time_to_contact))) NA_real_ else mean(time_to_contact, na.rm = TRUE),
      OnPlaneEff = if(all(is.na(on_plane_efficiency))) NA_real_ else mean(on_plane_efficiency, na.rm = TRUE),
      EarlyConn = if(all(is.na(early_connection))) NA_real_ else mean(early_connection, na.rm = TRUE),
      ConnImpact = if(all(is.na(connection_at_impact))) NA_real_ else mean(connection_at_impact, na.rm = TRUE),
      VertBatAngle = if(all(is.na(vertical_bat_angle))) NA_real_ else mean(vertical_bat_angle, na.rm = TRUE),
      AttackAngle = if(all(is.na(attack_angle))) NA_real_ else mean(attack_angle, na.rm = TRUE),
      RightOF = sum(RightOF, na.rm = TRUE),
      CenterOF = sum(CenterOF, na.rm = TRUE),
      LeftOF = sum(LeftOF, na.rm = TRUE),
      RightIF = sum(RightIF, na.rm = TRUE),
      CenterIF = sum(CenterIF, na.rm = TRUE),
      LeftIF = sum(LeftIF, na.rm = TRUE),
      Barrels = sum(Barrels, na.rm = TRUE),
      LDC = sum(LDC, na.rm = TRUE),
      FBC = sum(FBC, na.rm = TRUE),
      GBC = sum(GBC, na.rm = TRUE),
      HC = sum(HC, na.rm = TRUE),
      Barrel_Pct = ifelse(HC == 0, NA_real_, round(Barrels / HC * 100, 1)),
      .groups = "drop"
    ) %>% 
    group_by(Level, Gender) %>%  # Re-group for percentile calculation
    mutate(
      MaxVel_Percentile = case_when(
        Level == "Collegiate"   ~ round(pnorm((MaxVel - 103.8)   /  7.06)     * 100),
        Level == "Professional" ~ round(pnorm((MaxVel - 108.9622)/  4.008935) * 100),
        TRUE                    ~ round(percent_rank(MaxVel) * 100)
      ),
      AvgVel_Percentile = case_when(
        Level == "Collegiate"   ~ round(pnorm((AvgVel - 82.2)     /  6.01)     * 100),
        Level == "Professional" ~ round(pnorm((AvgVel - 87.41388)/  3.481693) * 100),
        TRUE                    ~ round(percent_rank(AvgVel) * 100)
      ),
      MaxDist_Percentile = case_when(
        Level == "Collegiate"   ~ round(pnorm((MaxDist - 367)    / 58.39)     * 100),
        Level == "Professional" ~ round(pnorm((MaxDist - 411.4609)/ 35.09507) * 100),
        TRUE                    ~ round(percent_rank(MaxDist) * 100)
      ),
      AvgDist_Percentile = case_when(
        Level == "Collegiate"   ~ round(pnorm((AvgDist - 153.4)    / 34.43)   * 100),
        Level == "Professional" ~ round(pnorm((AvgDist - 161.0702)/ 25.67749) * 100),
        TRUE                    ~ round(percent_rank(AvgDist) * 100)
      ),
      BatSpeed_Percentile = case_when(
        Level == "Collegiate"   ~ round(pnorm((BatSpeed - 69.11) /  3.37)     * 100),
        Level == "Professional" ~ round(pnorm((BatSpeed - 71.645)/  3.318)    * 100),
        TRUE                    ~ round(percent_rank(BatSpeed) * 100)
      ),
      RotationalAcceleration_Percentile = case_when(
        Level == "Collegiate"   ~ round(pnorm((RotAcc - 13.67) /  2.98)   * 100),
        Level == "Professional" ~ round(pnorm((RotAcc - 14.874)/  2.684)  * 100),
        TRUE                    ~ round(percent_rank(RotAcc) * 100)
      ),
      TTC_Percentile = case_when(
        Level == "Collegiate"   ~ round((1 - pnorm((TTC - 0.152) / 0.01))   * 100),
        Level == "Professional" ~ round((1 - pnorm((TTC - 0.149) / 0.008))  * 100),
        TRUE                    ~ round(percent_rank(-TTC) * 100)  # invert rank
      ),
      Barrel_Percentile = round(percent_rank(Barrel_Pct) * 100),
      OnPlaneEfficiency_Percentile = normalize_ope(OnPlaneEff),
      VerticalBatAngle_Percentile = normalize_vba(VertBatAngle),
      AttackAngle_Percentile = normalize_attack_angle(AttackAngle)
    ) %>% 
    ungroup() %>% 
    mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>% 
    arrange(Name, Date) %>%
    group_by(Level, Gender, Date) %>%
    mutate(
      MaxVel_Rank = rank(-MaxVel, ties.method = "min"),
      AvgVel_Rank = rank(-AvgVel, ties.method = "min"),
      MaxDist_Rank = rank(-MaxDist, ties.method = "min"),
      #AvgDist_Rank = rank(-AvgDist, ties.method = "min"),
      Barrel_Rank = rank(-Barrel_Pct, ties.method = "min"),
      Total_Players = n()
    ) %>% 
    ungroup()
  
  percentile_data <- hitting_summary %>%
    group_by(Level, Gender) %>%
    summarise(
      MaxVel_90th = quantile(MaxVel, 0.9, na.rm = TRUE),
      AvgVel_90th = quantile(AvgVel, 0.9, na.rm = TRUE),
      MaxDist_90th = quantile(MaxDist, 0.9, na.rm = TRUE),
      # AvgDist_90th = quantile(AvgDist, 0.9, na.rm = TRUE),
      Barrel_90th = quantile(Barrel_Pct, 0.9, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    ungroup()
  
  # Define a temp directory for this athlete
  athlete_dir <- file.path(tempdir(), athlete)
  if (!dir.exists(athlete_dir)) {
    dir.create(athlete_dir, recursive = TRUE)
  }
  
  # Load and process the check-ins data
  attendanceData <- hitting_data %>% 
    filter(!is.na(Attendance), !is.na(MaxVel)) %>% 
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
    } else if (score < 1.5) {
      return("#FF0000")
    } else if (score >= 1.5 & score < 2.5) {
      return("#FFA500")
    } else {
      return("#008000")
    }
  })
}

# Plotting
attendance_plot <- ggplot(attendance_plot_data, aes(x = Name, y = `Attendance Score`)) +
  # Background bars
  geom_col(aes(y = 4), alpha = 0.75, fill = "gray30", color = "black") +
  
  # Foreground colored bars with black borders
  geom_col(aes(fill = get_color(`Attendance Score`)), color = "black") +
  
  # Score text
  geom_text(aes(y = 2, label = paste(attendance_score)), size = 12, fontface = "bold", color = "white") +
  
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


# 1. Render your attendance plot, write it out, then composite it directly back into report_img
attendance_path <- file.path(athlete_dir, paste0(athlete, "_attendancePlot.png"))
ggsave(attendance_path,
       plot   = attendance_plot,
       width  = 2.15, height = 0.65,
       units  = "in",
       dpi    = 175)

# read & composite back into the same object
attendancePlot <- image_read(attendance_path)
report_img     <- image_composite(report_img, attendancePlot, offset = "+1400+300")

# 2. Immediately free the temp image before doing the next step
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

##################################################################################################################
##################################################################################################################
##################################################################################################################

athlete_summary <- hitting_summary %>% 
  filter(Name == athlete, !is.na(MaxVel), !is.na(AvgVel), !is.na(MaxDist), !is.na(Barrel_Pct)) %>% 
  arrange(Date) %>%
  group_by(Name) %>%
  mutate(
    `MaxEV Value Change (Month)` = case_when(
      is.na(MaxVel) | is.na(lag(MaxVel)) ~ NA_character_,
      TRUE ~ sprintf("%+.1f", round(MaxVel - lag(MaxVel), 1))
    ),
    `MaxEV Percent Change (Month)` = round((MaxVel - lag(MaxVel)) / lag(MaxVel) * 100, 1),
    
    `MaxEV Value Change (Career)` = case_when(
      is.na(MaxVel) | is.na(first(MaxVel)) ~ NA_character_,
      TRUE ~ sprintf("%+.1f", round(MaxVel - first(MaxVel), 1))
    ),
    `MaxEV Percent Change (Career)` = round((MaxVel - first(MaxVel)) / first(MaxVel) * 100, 1),
    
    `AvgEV Value Change (Month)` = case_when(
      is.na(AvgVel) | is.na(lag(AvgVel)) ~ NA_character_,
      TRUE ~ sprintf("%+.1f", round(AvgVel - lag(AvgVel), 1))
    ),
    `AvgEV Percent Change (Month)` = round((AvgVel - lag(AvgVel)) / lag(AvgVel) * 100, 1),
    
    `AvgEV Value Change (Career)` = case_when(
      is.na(AvgVel) | is.na(first(AvgVel)) ~ NA_character_,
      TRUE ~ sprintf("%+.1f", round(AvgVel - first(AvgVel), 1))
    ),
    `AvgEV Percent Change (Career)` = round((AvgVel - first(AvgVel)) / first(AvgVel) * 100, 1),
    
    `MaxDist Value Change (Month)` = case_when(
      is.na(MaxDist) | is.na(lag(MaxDist)) ~ NA_character_,
      TRUE ~ sprintf("%+.1f", round(MaxDist - lag(MaxDist), 1))
    ),
    `MaxDist Percent Change (Month)` = round((MaxDist - lag(MaxDist)) / lag(MaxDist) * 100, 1),
    
    `MaxDist Value Change (Career)` = case_when(
      is.na(MaxDist) | is.na(first(MaxDist)) ~ NA_character_,
      TRUE ~ sprintf("%+.1f", round(MaxDist - first(MaxDist), 1))
    ),
    `MaxDist Percent Change (Career)` = round((MaxDist - first(MaxDist)) / first(MaxDist) * 100, 1),
    
    `Barrel Value Change (Month)` = case_when(
      is.na(Barrel_Pct) | is.na(lag(Barrel_Pct)) ~ NA_character_,
      TRUE ~ sprintf("%+.1f", round(Barrel_Pct - lag(Barrel_Pct), 1))
    ),
    
    `Barrel Value Change (Career)` = case_when(
      is.na(Barrel_Pct) | is.na(first(Barrel_Pct)) ~ NA_character_,
      TRUE ~ sprintf("%+.1f", round(Barrel_Pct - first(Barrel_Pct), 1))
    )
  ) %>%
  ungroup()

# Get the athlete's level and gender from athlete_summary
athlete_level <- athlete_summary$Level[nrow(athlete_summary)]
athlete_gender <- athlete_summary$Gender[nrow(athlete_summary)]

filtered_percentile <- percentile_data %>% 
  filter(Level == athlete_level, Gender == athlete_gender)

current_athlete_summary <- athlete_summary %>% 
  filter(Month == month & Year == year)

restructured_maxEV <- current_athlete_summary %>% 
  mutate(
    `Month:` = paste0(`MaxEV Percent Change (Month)`, " (", `MaxEV Value Change (Month)`, ")"),
    `Career:` = paste0(`MaxEV Percent Change (Career)`, " (", `MaxEV Value Change (Career)`, ")")
  ) %>%
  select(`Month:`, `Career:`) %>% 
  pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")

if (any(!is.na(current_athlete_summary$`MaxEV Percent Change (Month)`))) {
  
  # Create the gt table with styling and apply arrow icons and value changes
  maxEV_gt <- restructured_maxEV %>% 
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
  maxEV_gt <- tibble(Metric = "", Value = "") %>%
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

min_maxVel <- athlete_summary %>%
  summarise(minValue = min(MaxVel, na.rm = TRUE)) %>%
  pull(minValue)

max_maxVel <- athlete_summary %>%
  summarise(maxValue = max(MaxVel, na.rm = TRUE)) %>%
  pull(maxValue)

# Calculate the unique months in the data
unique_months <- unique(floor_date(athlete_summary$Date, "month"))

# Determine if there are exactly 2 unique months
if (length(unique_months) == 2) {
  breaks_option <- unique_months
} else {
  breaks_option <- waiver() # Default ggplot behavior
}

maxEV_value <- current_athlete_summary$MaxVel
maxEV_percentile_numeric <- current_athlete_summary$MaxVel_Percentile
maxEV_percentile <- ordinal_suffix(round(maxEV_percentile_numeric))
maxEV_rank <- paste0(current_athlete_summary$MaxVel_Rank, " out of ", current_athlete_summary$Total_Players)
maxEV_goal <- filtered_percentile$MaxVel_90th

# Create a color gradient from red to yellow to green
maxEV_color <- col_numeric(
  palette = c("#FF0000", "#FFFF00", "#008000"), 
  domain = c(0, 100)  # Scale from 0 to 100 percentiles
)(maxEV_percentile_numeric)

max_point <- athlete_summary[which.max(athlete_summary$MaxVel), ]

maxEV_plot <- ggplot(athlete_summary, aes(x = Date, y = MaxVel)) +
  geom_line(color = "#0099f9", linewidth = 2) +
  geom_point(color = "#0099f9", size = 3) +
  geom_hline(yintercept = maxEV_goal, linetype = "dashed", color = "white", linewidth = 1) +
  annotate("text", x = max(athlete_summary$Date), y = maxEV_goal, label = "Goal", 
           color = "white", hjust = hjust_value, vjust = -1, size = 5) +
  geom_point(data = max_point, aes(x = Date, y = MaxVel), color = "#FF0000", size = 5) +
  geom_richtext(
    data = data.frame(x = max_point$Date, y = max_point$MaxVel),
    aes(x = x, y = y, label = paste0("<b style='color:#FF0000;'>", max_point$MaxVel, "</b>")),
    fill = "black", hjust = 0.5, vjust = -0.5, size = 6
  ) +
  labs(
    title = "Max EV\n",
    subtitle = paste0(
      "<span style='font-size:50px;'>", maxEV_value, "</span> <span style='font-size:25px;'>mph</span><br><br>",
      "<span style='font-size:50px; color:", maxEV_color, ";'>", maxEV_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
    ),
    tag = paste0("Rank: ", maxEV_rank),
    x = NULL,
    y = NULL
  ) +
  scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
  scale_y_continuous(
    limits = c(
      min(min_maxVel - 5, maxEV_goal - 5),
      max(max_maxVel + 5, maxEV_goal + 5) 
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "white", size = 26),
    plot.subtitle = element_markdown(color = "white"), # allows markdown for dynamic color
    plot.tag = element_text(color = "white", size = 22),
    plot.tag.position = c(0.8, 0.71), 
    axis.text = element_text(color = "lightgrey")
  )

# 1. Composite your gt table image in-place
maxEV_gt_path <- file.path(athlete_dir, paste0(athlete, "_maxEV_gt.png"))
gtsave(maxEV_gt, file = maxEV_gt_path, expand = -1)

gt_img       <- image_read(maxEV_gt_path) %>%
  image_transparent(color = "black")
report_img   <- image_composite(report_img, gt_img, offset = "+1225+2400")

rm(gt_img)  
gc()


# 2. Composite your maxEV plot in-place
maxEV_plot_path <- file.path(athlete_dir, paste0(athlete, "_maxEV_plot.png"))
ggsave(maxEV_plot_path,
       plot   = maxEV_plot,
       width  = 5, height = 4.75,
       units  = "in",
       dpi    = 150)

maxEV_img     <- image_read(maxEV_plot_path)
report_img    <- image_composite(report_img, maxEV_img, offset = "+830+2425")

rm(maxEV_img)
gc()

restructured_maxDist <- current_athlete_summary %>% 
  mutate(
    `Month:` = paste0(`MaxDist Percent Change (Month)`, " (", `MaxDist Value Change (Month)`, ")"),
    `Career:` = paste0(`MaxDist Percent Change (Career)`, " (", `MaxDist Value Change (Career)`, ")")
  ) %>%
  select(`Month:`, `Career:`) %>% 
  pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")

if (any(!is.na(current_athlete_summary$`MaxDist Percent Change (Month)`))) {
  
  # Create the gt table with styling and apply arrow icons and value changes
  maxDist_gt <- restructured_maxDist %>% 
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
} else {
  
  # Create a simplified gt table without text transformation
  maxDist_gt <- tibble(Metric = "", Value = "") %>%
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

min_maxDist <- athlete_summary %>%
  summarise(minValue = min(MaxDist, na.rm = TRUE)) %>%
  pull(minValue)

max_maxDist <- athlete_summary %>%
  summarise(maxValue = max(MaxDist, na.rm = TRUE)) %>%
  pull(maxValue)

maxDist_value <- current_athlete_summary$MaxDist
maxDist_percentile_numeric <- current_athlete_summary$MaxDist_Percentile
maxDist_percentile <- ordinal_suffix(round(maxDist_percentile_numeric))
maxDist_rank <- paste0(current_athlete_summary$MaxDist_Rank, " out of ", current_athlete_summary$Total_Players)
maxDist_goal <- filtered_percentile$MaxDist_90th

maxDist_color <- col_numeric(
  palette = c("#FF0000", "#FFFF00", "#008000"), 
  domain = c(0, 100)
)(maxDist_percentile_numeric)

maxDist_point <- athlete_summary[which.max(athlete_summary$MaxDist), ]

maxDist_plot <- ggplot(athlete_summary, aes(x = Date, y = MaxDist)) +
  geom_line(color = "#0099f9", linewidth = 2) +
  geom_point(color = "#0099f9", size = 3) +
  geom_hline(yintercept = maxDist_goal, linetype = "dashed", color = "white", linewidth = 1) +
  annotate("text", x = max(athlete_summary$Date), y = maxDist_goal, label = "Goal", 
           color = "white", hjust = hjust_value, vjust = -1, size = 5) +
  geom_point(data = maxDist_point, aes(x = Date, y = MaxDist), color = "#FF0000", size = 5) +
  geom_richtext(
    data = data.frame(x = maxDist_point$Date, y = maxDist_point$MaxDist),
    aes(x = x, y = y, label = paste0("<b style='color:#FF0000;'>", maxDist_point$MaxDist, "</b>")),
    fill = "black", hjust = 0.5, vjust = -0.5, size = 6
  ) +
  labs(
    title = "Max Distance\n",
    subtitle = paste0(
      "<span style='font-size:50px;'>", maxDist_value, "</span> <span style='font-size:25px;'>ft</span><br><br>",
      "<span style='font-size:50px; color:", maxDist_color, ";'>", maxDist_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
    ),
    tag = paste0("Rank: ", maxDist_rank),
    x = NULL,
    y = NULL
  ) +
  scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
  scale_y_continuous(
    limits = c(
      min(min_maxDist - 15, maxDist_goal - 15),
      max(max_maxDist + 15, maxDist_goal + 15) 
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "white", size = 26),
    plot.subtitle = element_markdown(color = "white"),
    plot.tag = element_text(color = "white", size = 22),
    plot.tag.position = c(0.8, 0.71), 
    axis.text = element_text(color = "lightgrey")
  )

# 1. Composite your maxDist gt table in-place
maxDist_gt_path <- file.path(athlete_dir, paste0(athlete, "_maxDist_gt.png"))
gtsave(maxDist_gt, file = maxDist_gt_path, expand = -1)

maxDist_gt_img <- image_read(maxDist_gt_path) %>%
  image_transparent(color = "black")
report_img      <- image_composite(report_img, maxDist_gt_img, offset = "+2050+2400")

rm(maxDist_gt_img)
gc()


# 2. Composite your maxDist plot in-place
maxDist_plot_path <- file.path(athlete_dir, paste0(athlete, "_maxDist_plot.png"))
ggsave(maxDist_plot_path,
       plot   = maxDist_plot,
       width  = 5, height = 4.75,
       units  = "in",
       dpi    = 150)

maxDist_plot_img <- image_read(maxDist_plot_path)
report_img       <- image_composite(report_img, maxDist_plot_img, offset = "+1675+2425")

rm(maxDist_plot_img)
gc()

restructured_avgEV <- current_athlete_summary %>% 
  mutate(
    `Month:` = paste0(`AvgEV Percent Change (Month)`, " (", `AvgEV Value Change (Month)`, ")"),
    `Career:` = paste0(`AvgEV Percent Change (Career)`, " (", `AvgEV Value Change (Career)`, ")")
  ) %>%
  select(`Month:`, `Career:`) %>% 
  pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")

if (any(!is.na(current_athlete_summary$`AvgEV Percent Change (Month)`))) {
  
  # Create the gt table with styling and apply arrow icons and value changes
  avgEV_gt <- restructured_avgEV %>% 
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
} else {
  
  # Create a simplified gt table without text transformation
  avgEV_gt <- tibble(Metric = "", Value = "") %>%
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

min_avgVel <- athlete_summary %>%
  summarise(minValue = min(AvgVel, na.rm = TRUE)) %>%
  pull(minValue)

max_avgVel <- athlete_summary %>%
  summarise(maxValue = max(AvgVel, na.rm = TRUE)) %>%
  pull(maxValue)

avgEV_value <- current_athlete_summary$AvgVel
avgEV_percentile_numeric <- current_athlete_summary$AvgVel_Percentile
avgEV_percentile <- ordinal_suffix(round(avgEV_percentile_numeric))
avgEV_rank <- paste0(current_athlete_summary$AvgVel_Rank, " out of ", current_athlete_summary$Total_Players)
avgEV_goal <- filtered_percentile$AvgVel_90th

avgEV_color <- col_numeric(
  palette = c("#FF0000", "#FFFF00", "#008000"), 
  domain = c(0, 100)  # Scale from 0 to 100 percentiles
)(avgEV_percentile_numeric)

avgEV_point <- athlete_summary[which.max(athlete_summary$AvgVel), ]

avgEV_plot <- ggplot(athlete_summary, aes(x = Date, y = AvgVel)) +
  geom_line(color = "#0099f9", linewidth = 2) +
  geom_point(color = "#0099f9", size = 3) +
  geom_point(data = avgEV_point, aes(x = Date, y = AvgVel), color = "red", size = 5) +
  geom_text(data = avgEV_point, aes(x = Date, y = AvgVel, label = AvgVel), 
            color = "red", vjust = -1.5, fontface = "bold", size = 6) +
  geom_hline(yintercept = avgEV_goal, linetype = "dashed", color = "white", linewidth = 1) +
  annotate("text", x = max(athlete_summary$Date), y = avgEV_goal, label = "Goal", 
           color = "white", hjust = hjust_value, vjust = -1, size = 5) +
  geom_point(data = avgEV_point, aes(x = Date, y = AvgVel), color = "#FF0000", size = 5) +
  geom_richtext(
    data = data.frame(x = avgEV_point$Date, y = avgEV_point$AvgVel),
    aes(x = x, y = y, label = paste0("<b style='color:#FF0000;'>", avgEV_point$AvgVel, "</b>")),
    fill = "black", hjust = 0.5, vjust = -0.5, size = 6
  ) +
  labs(
    title = "Average EV\n",
    subtitle = paste0(
      "<span style='font-size:50px;'>", avgEV_value, "</span> <span style='font-size:25px;'>mph</span><br><br>",
      "<span style='font-size:50px; color:", avgEV_color, ";'>", avgEV_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
    ),
    tag = paste0("Rank: ", avgEV_rank),
    x = NULL,
    y = NULL
  ) +
  scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
  scale_y_continuous(
    limits = c(
      min(min_avgVel - 5, avgEV_goal - 5),
      max(max_avgVel + 5, avgEV_goal + 5) 
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "white", size = 26),
    plot.subtitle = element_markdown(color = "white"),
    plot.tag = element_text(color = "white", size = 22),
    plot.tag.position = c(0.8, 0.71), 
    axis.text = element_text(color = "lightgrey")
  )

# 1. Composite your avgEV gt table in-place
avgEV_gt_path <- file.path(athlete_dir, paste0(athlete, "_avgEV_gt.png"))
gtsave(avgEV_gt, file = avgEV_gt_path, expand = -1)

avgEV_gt_img <- image_read(avgEV_gt_path) %>%
  image_transparent(color = "black")
report_img    <- image_composite(report_img, avgEV_gt_img, offset = "+1225+1525")

rm(avgEV_gt_img)
gc()


# 2. Composite your avgEV plot in-place
avgEV_plot_path <- file.path(athlete_dir, paste0(athlete, "_avgEV_plot.png"))
ggsave(avgEV_plot_path,
       plot   = avgEV_plot,
       width  = 5, height = 4.75,
       units  = "in",
       dpi    = 150)

avgEV_plot_img <- image_read(avgEV_plot_path)
report_img      <- image_composite(report_img, avgEV_plot_img, offset = "+830+1550")

rm(avgEV_plot_img)
gc()

restructured_barrel <- current_athlete_summary %>% 
  mutate(
    `Month:` = `Barrel Value Change (Month)`,
    `Career:` = `Barrel Value Change (Career)`
  ) %>%
  select(`Month:`, `Career:`) %>% 
  pivot_longer(cols = c(`Month:`, `Career:`), names_to = "Metric", values_to = "Value")

if (any(!is.na(current_athlete_summary$`Barrel Value Change (Month)`))) {
  
  # Create the gt table with styling and apply arrow icons and value changes
  barrel_gt <- restructured_barrel %>% 
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
} else {
  
  # Create a simplified gt table without text transformation
  barrel_gt <- tibble(Metric = "", Value = "") %>%
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

min_barrel <- athlete_summary %>%
  summarise(minValue = min(Barrel_Pct, na.rm = TRUE)) %>%
  pull(minValue)

max_barrel <- athlete_summary %>%
  summarise(maxValue = max(Barrel_Pct, na.rm = TRUE)) %>%
  pull(maxValue)

barrel_value <- current_athlete_summary$Barrel_Pct
barrel_percentile_numeric <- current_athlete_summary$Barrel_Percentile
barrel_percentile <- ordinal_suffix(round(barrel_percentile_numeric))
barrel_rank <- paste0(current_athlete_summary$Barrel_Rank, " out of ", current_athlete_summary$Total_Players)
barrel_goal <- filtered_percentile$Barrel_90th

barrel_color <- col_numeric(
  palette = c("#FF0000", "#FFFF00", "#008000"), 
  domain = c(0, 100)  # Scale from 0 to 100 percentiles
)(barrel_percentile_numeric)

barrel_point <- athlete_summary[which.max(athlete_summary$Barrel_Pct), ]

barrel_plot <- ggplot(athlete_summary, aes(x = Date, y = Barrel_Pct)) +
  geom_line(color = "#0099f9", linewidth = 2) +
  geom_point(color = "#0099f9", size = 3) +
  geom_point(data = barrel_point, aes(x = Date, y = Barrel_Pct), color = "red", size = 5) +
  geom_text(data = barrel_point, aes(x = Date, y = Barrel_Pct, label = Barrel_Pct), 
            color = "red", vjust = -1.5, fontface = "bold", size = 6) +
  geom_hline(yintercept = barrel_goal, linetype = "dashed", color = "white", linewidth = 1) +
  annotate("text", x = max(athlete_summary$Date), y = barrel_goal, label = "Goal", 
           color = "white", hjust = hjust_value, vjust = -1, size = 5) +
  geom_point(data = barrel_point, aes(x = Date, y = Barrel_Pct), color = "#FF0000", size = 5) +
  geom_richtext(
    data = data.frame(x = barrel_point$Date, y = barrel_point$Barrel_Pct),
    aes(x = x, y = y, label = paste0("<b style='color:#FF0000;'>", barrel_point$Barrel_Pct, "</b>")),
    fill = "black", hjust = 0.5, vjust = -0.5, size = 6
  ) +
  labs(
    title = "Barrel %\n",
    subtitle = paste0(
      "<span style='font-size:50px;'>", barrel_value, "</span> <span style='font-size:25px;'>%</span><br><br>",
      "<span style='font-size:50px; color:", barrel_color, ";'>", barrel_percentile, "</span> <span style='font-size:25px;'>Percentile</span><br><br>"
    ),
    tag = paste0("Rank: ", barrel_rank),
    x = NULL,
    y = NULL
  ) +
  scale_x_date(date_labels = "%b %Y", breaks = breaks_option) +
  scale_y_continuous(
    limits = c(
      min(min_barrel - 15, barrel_goal - 15),
      max(max_barrel + 15, barrel_goal + 15) 
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "white", size = 26),
    plot.subtitle = element_markdown(color = "white"),
    plot.tag = element_text(color = "white", size = 22),
    plot.tag.position = c(0.8, 0.71), 
    axis.text = element_text(color = "lightgrey")
  )

# 1. Composite your barrel gt table in-place
barrel_gt_path <- file.path(athlete_dir, paste0(athlete, "_barrel_gt.png"))
gtsave(barrel_gt, file = barrel_gt_path, expand = -1)

barrel_gt_img <- image_read(barrel_gt_path) %>%
  image_transparent(color = "black")
report_img    <- image_composite(report_img, barrel_gt_img, offset = "+2185+1525")

rm(barrel_gt_img)
gc()


# 2. Composite your barrel plot in-place
barrel_plot_path <- file.path(athlete_dir, paste0(athlete, "_barrel_plot.png"))
ggsave(barrel_plot_path,
       plot   = barrel_plot,
       width  = 5, height = 4.75,
       units  = "in",
       dpi    = 150)

barrel_plot_img <- image_read(barrel_plot_path)
report_img      <- image_composite(report_img, barrel_plot_img, offset = "+1675+1550")

rm(barrel_plot_img)
gc()

hit_locations <- athlete_summary %>%
  filter(Month == month, Year == year) %>% 
  mutate(
    LeftIF_Pct = round(LeftIF / HC * 100, 1),
    RightIF_Pct = round(RightIF / HC * 100, 1),
    CenterIF_Pct = round(CenterIF / HC * 100, 1),
    LeftOF_Pct = round(LeftOF / HC * 100, 1),
    RightOF_Pct = round(RightOF / HC * 100, 1),
    CenterOF_Pct = round(CenterOF / HC * 100, 1)
  ) %>% 
  select(RightOF_Pct, CenterOF_Pct, LeftOF_Pct, RightIF_Pct, CenterIF_Pct, LeftIF_Pct)

summarized_values <- unlist(hit_locations)

dimensions_calculator <- data.frame(value = rep(summarized_values, each = 102), 
                                    group = rep(letters[1:6], each = 102),
                                    x     = c(f1(A, B), f1(B, -B), f1(-B, -A), 
                                              f1(C, D), f1(D, -D), f1(-D, -C)),
                                    y     = c(f2(A, B), f2(B, -B), f2(-B, -A),
                                              f3(C, D), f3(D, -D), f3(-D, -C)))

spray_plot <- ggplot(dimensions_calculator, aes(x, y, group = group, fill = value)) +
  geom_polygon(color = 'lightgrey', linewidth = 1.3) +
  geom_label(data = . %>% group_by(group) %>%
               summarize(x = ifelse(n() > 20, mean(x), mean(range(x))), 
                         y = ifelse(n() > 20, mean(y), sqrt(8) / 2),
                         value = mean(value)), fill = 'white',
             aes(label = paste0(value, "%")), fontface = 4, size = 8) +
  scale_fill_gradientn(colours = c("#FFFFFF", "#90EE90", "#023020"),
                       values = c(0, 0.1, 0.55, 0.95, 1), guide = 'none') +
  coord_equal() +
  theme_void()

# Save & composite your spray chart plot in-place
spray_plot_path <- file.path(athlete_dir, paste0(athlete, "_spraychart.png"))

ggsave(spray_plot_path,
       plot   = spray_plot,
       width  = 3.5, height = 3.5,
       units  = "in",
       dpi    = 150)

spray_img   <- image_read(spray_plot_path)
report_img  <- image_composite(report_img, spray_img, offset = "+1710+600")

rm(spray_img)
gc()

# 2) set three thresholds based on that level
if (athlete_level == "L1") {
  thr <- c(55, 60, 65)
} else if (athlete_level == "L2") {
  thr <- c(70, 75, 80)
} else {
  # covers L3, Collegiate, Professional
  thr <- c(85, 90, 95)
}

# 3) rebuild batted_ball_metrics using thr[1], thr[2], thr[3]
batted_ball_metrics <- hittrax_data %>%
  filter(Name == athlete, Month == month, Year == year) %>%
  filter(Outcome %in% c("Out", "Single", "Double", "Triple", "Home Run")) %>%
  group_by(HitType) %>%
  summarise(
    Count   = n(),
    `Avg EV` = round(mean(ExitVelocity, na.rm = TRUE), 1),
    Over1   = sum(ExitVelocity > thr[1], na.rm = TRUE),
    Over2   = sum(ExitVelocity > thr[2], na.rm = TRUE),
    Over3   = sum(ExitVelocity > thr[3], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Total    = sum(Count),
    Pct      = round(Count / Total * 100, 1),
    # create three new columns named e.g. "55+", "60+", "65+" (or "70+", etc.)
    !!paste0(thr[1], "+") := round(Over1 / Count * 100, 1),
    !!paste0(thr[2], "+") := round(Over2 / Count * 100, 1),
    !!paste0(thr[3], "+") := round(Over3 / Count * 100, 1)
  ) %>%
  select(HitType, Pct, `Avg EV`, paste0(thr, "+"))

# 4) build the gt table exactly as before (no other changes needed)
batted_ball_table <- batted_ball_metrics %>%
  gt() %>% 
  fmt(
    columns = `Avg EV`,
    fns = function(x) paste0(x, " mph")
  ) %>%
  fmt(
    columns = c(Pct, paste0(thr, "+")),
    fns = function(x) paste0(x, "%")
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left",   columns = HitType) %>%
  tab_options(
    column_labels.font.weight = "bold",
    table.background.color   = "black",
    table.font.color         = "white",
    table.border.top.style   = "hidden",
    table.border.bottom.style= "hidden",
    data_row.padding         = px(7),
    table.font.size          = px(13)
  ) %>% 
  opt_table_font(font = "Helvetica") %>% 
  tab_style(
    style     = cell_text(color = "#3d9be9"),
    locations = cells_column_labels()
  ) %>% 
  cols_width(
    HitType ~ px(90),
    `Avg EV` ~ px(90),
    everything() ~ px(65)
  )

# 1. Composite your batted ball table in-place
batted_ball_table_path <- file.path(athlete_dir, paste0(athlete, "_batted_ball_table.png"))
gtsave(batted_ball_table, file = batted_ball_table_path, vwidth = 1200, expand = 0)

batted_ball_img <- image_read(batted_ball_table_path) %>%
  image_transparent(color = "black")
report_img      <- image_composite(report_img, batted_ball_img, offset = "+1540+1175")

rm(batted_ball_img)
gc()


##################################################################################################################
##################################################################################################################
##################################################################################################################

if (is.na(current_athlete_summary$OnPlaneEff)) {
  # Athlete has no Blast data
  no_data_plot <- ggplot() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.5, label = "No Blast Data", size = 13, color = "grey")
  
  # 1. Composite your connection overview blank plot in-place
  connection_blank_path <- "www/connection_overview_blank.png"
  ggsave(connection_blank_path,
         plot   = no_data_plot,
         width  = 5, height = 5,
         dpi    = 150)
  
  conn_img   <- image_read(connection_blank_path)
  report_img <- image_composite(report_img, conn_img, offset = "+390+600")
  rm(conn_img); gc()
  
  
  # 2. Composite your contact profile blank plot in-place
  contact_blank_path <- "www/contact_profile_blank.png"
  ggsave(contact_blank_path,
         plot   = no_data_plot,
         width  = 5, height = 5,
         dpi    = 150)
  
  contact_img <- image_read(contact_blank_path)
  report_img  <- image_composite(report_img, contact_img, offset = "+90+1550")
  rm(contact_img); gc()
  
  
  # 3. Composite your power profile blank plot in-place
  power_blank_path <- "www/power_profile_blank.png"
  ggsave(power_blank_path,
         plot   = no_data_plot,
         width  = 5, height = 5,
         dpi    = 150)
  
  power_img  <- image_read(power_blank_path)
  report_img <- image_composite(report_img, power_img, offset = "+90+2425")
  rm(power_img); gc()
  
  
  # 4. Write out the final report
  final_report_path <- file.path(athlete_dir, "FuturesHittingReport.png")
  image_write(report_img,
              path    = final_report_path,
              format  = "png")
  
  # Return the path for downloadHandler()
  final_report_path
  
} else {
  # Athlete has Blast data
  on_plane_efficiency <- round(current_athlete_summary$OnPlaneEff)
  
  # Determine which OPE image to use
  OPE_image_value <- round(on_plane_efficiency / 5) * 5
  OPE_image_path  <- paste0("www/OPE", OPE_image_value, ".png")
  
  # Read & composite into report_img in-place
  ope_img    <- image_read(OPE_image_path)
  report_img <- image_composite(report_img, ope_img, offset = "+565+850")
  
  # Free memory before the next step
  rm(ope_img)
  gc()
  
  if (OPE_image_value <= 35) {
    OPE_color <- "#FF0000" # Red
  } else if (OPE_image_value >= 40 & OPE_image_value <= 60) {
    OPE_color <- "#FFFF00" # Yellow
  } else if (OPE_image_value >= 65 & OPE_image_value <= 85) {
    OPE_color <- "#008000" # Green
  } else if (OPE_image_value >= 90 & OPE_image_value <= 100) {
    OPE_color <- "#FFFF00" # Yellow
  } else {
    OPE_color <- NA
  }
  
  OPE_plot <- ggplot(data.frame(x = 0, y = 0), aes(x, y)) +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
    annotate("text", x = 0, y = 0, label = paste0(OPE_image_value, "%"), size = 13, color = OPE_color, family = "Good Times")
  
  # Save & composite your OPE text plot in-place
  ope_text_path <- file.path(athlete_dir, paste0(athlete, "_OPE_text.png"))
  ggsave(ope_text_path,
         plot   = OPE_plot,
         width  = 3, height = 3,
         dpi    = 150)
  
  ope_text_img <- image_read(ope_text_path)
  report_img   <- image_composite(report_img, ope_text_img, offset = "+560+500")
  
  rm(ope_text_img)
  gc()
  
  # Process EC image and plot
  early_connection <- round(current_athlete_summary$EarlyConn)
  
  if (early_connection < 80) {
    EC_color <- "#FF0000"
    EC_image <- "www/EC_BLW_90.png"
  } else if (early_connection > 105) {
    EC_color <- "#FF0000"
    EC_image <- "www/EC_ABV_90.png"
  } else {
    EC_color <- "#008000"
    EC_image <- "www/EC_at_90.png"
  }
  
  # Read & composite into report_img in-place
  ec_img     <- image_read(EC_image)
  report_img <- image_composite(report_img, ec_img, offset = "+110+850")
  
  # Free memory immediately
  rm(ec_img)
  gc()
  
  EC_plot <- ggplot(data.frame(x = 0, y = 0), aes(x, y)) +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
    annotate("text", x = 0, y = 0, label = paste0(early_connection, "°"), size = 13, color = EC_color, family = "Good Times")
  
  # Save & composite your EC text plot in-place
  ec_text_path <- file.path(athlete_dir, paste0(athlete, "_EC_text.png"))
  ggsave(ec_text_path,
         plot   = EC_plot,
         width  = 3, height = 3,
         dpi    = 150)
  
  ec_text_img <- image_read(ec_text_path)
  report_img  <- image_composite(report_img, ec_text_img, offset = "+75+500")
  
  # Free the temporary image from memory
  rm(ec_text_img)
  gc()
  
  
  # Process CI image and plot
  connection_at_impact <- round(current_athlete_summary$ConnImpact)
  
  if (connection_at_impact < 80) {
    CI_color <- "#FF0000"
    CI_file <- "www/CI_BLW_90.png"
  } else if (connection_at_impact > 95) {
    CI_color <- "#FF0000"
    CI_file <- "www/CI_ABV_90.png"
  } else {
    CI_color <- "#008000"
    CI_file <- "www/CI_at_90.png"
  }
  
  # Read & composite your CI image in-place
  ci_img     <- image_read(CI_file)
  report_img <- image_composite(report_img, ci_img, offset = "+1035+850")
  
  # Free the temporary image from memory
  rm(ci_img)
  gc()
  
  CI_plot <- ggplot(data.frame(x = 0, y = 0), aes(x, y)) +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
    annotate("text", x = 0, y = 0, label = paste0(connection_at_impact, "°"), size = 13, color = CI_color, family = "Good Times")
  
  # Save and read CI text plot in the athlete's temp directory
  ci_text_path <- file.path(athlete_dir, paste0(athlete, "_CI_text.png"))
  
  # Save & composite your CI text plot in-place
  ci_text_path <- file.path(athlete_dir, paste0(athlete, "_CI_text.png"))
  ggsave(ci_text_path,
         plot   = CI_plot,
         width  = 3, height = 3,
         dpi    = 150)
  
  ci_text_img <- image_read(ci_text_path)
  report_img  <- image_composite(report_img, ci_text_img, offset = "+1025+500")
  
  # Free the temporary image from memory
  rm(ci_text_img)
  gc()
  
  
  # Contact Table
  contact_data <- tibble(
    `Blast Metrics` = c("Rotational Acceleration", "On Plane Efficiency", "Attack Angle"),
    Value = c(
      paste0(round(current_athlete_summary$RotAcc, 1), " g"),
      paste0(OPE_image_value, " %"),
      paste0(round(current_athlete_summary$AttackAngle, 1), " °")
    ),
    Percentile = c(
      current_athlete_summary$RotationalAcceleration_Percentile, 
      current_athlete_summary$OnPlaneEfficiency_Percentile, 
      current_athlete_summary$AttackAngle_Percentile
    ),
    RawValue = c(
      NA,  # RA
      OPE_image_value,  # OPE actual metric
      NA   # AA
    )
  )
  
  contact_table <- gt(contact_data) %>%
    cols_align(align = "right", columns = Value) %>%
    cols_width(
      `Blast Metrics` ~ px(150),
      Value ~ px(100)
    ) %>%
    cols_hide(columns = c(Percentile, RawValue)) %>%
    tab_options(
      column_labels.font.weight = "bold",
      table.background.color = "black",
      table.font.color = "white",
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden",
      data_row.padding = px(5),
      table.font.size = px(13)
    ) %>%
    opt_table_font(font = "Helvetica") %>% 
    # Standard percentile coloring for non-OPE rows
    tab_style(
      style = cell_text(color = "#FF0000"),
      locations = cells_body(
        columns = Value,
        rows = Percentile < 33 & `Blast Metrics` != "On Plane Efficiency"
      )
    ) %>%
    tab_style(
      style = cell_text(color = "#FFFF00"),
      locations = cells_body(
        columns = Value,
        rows = Percentile >= 33 & Percentile < 66 & `Blast Metrics` != "On Plane Efficiency"
      )
    ) %>%
    tab_style(
      style = cell_text(color = "#008000"),
      locations = cells_body(
        columns = Value,
        rows = Percentile >= 66 & `Blast Metrics` != "On Plane Efficiency"
      )
    ) %>%
    # Custom rule for OPE actual value
    tab_style(
      style = cell_text(color = "#008000"),  # Green
      locations = cells_body(
        columns = Value,
        rows = `Blast Metrics` == "On Plane Efficiency" & RawValue >= 65 & RawValue <= 85
      )
    ) %>%
    tab_style(
      style = cell_text(color = "#FF0000"),  # Red
      locations = cells_body(
        columns = Value,
        rows = `Blast Metrics` == "On Plane Efficiency" & RawValue <= 35
      )
    ) %>%
    tab_style(
      style = cell_text(color = "#FFFF00"),  # Yellow
      locations = cells_body(
        columns = Value,
        rows = `Blast Metrics` == "On Plane Efficiency" & (RawValue > 35 & RawValue < 65 | RawValue > 85)
      )
    ) %>%
    tab_style(
      style = cell_text(color = "#3d9be9"),
      locations = cells_column_labels()
    )
  
  # Composite your contact table in-place
  contact_table_path <- file.path(athlete_dir, paste0(athlete, "_contact_table.png"))
  gtsave(contact_table, file = contact_table_path, expand = -1)
  
  contact_img <- image_read(contact_table_path) %>%
    image_transparent(color = "black")
  report_img  <- image_composite(report_img, contact_img, offset = "+217+1545")
  
  rm(contact_img)
  gc()
  
  contact_percentiles <- current_athlete_summary %>%
    select(RotationalAcceleration_Percentile, AttackAngle_Percentile, OnPlaneEfficiency_Percentile)
  
  # Rename columns for plot labels
  colnames(contact_percentiles) <- c("RA", "AA", "OPE")
  
  # Prepare data for radar chart
  contact_radar_data <- bind_rows(
    tibble(RA = 100, AA = 100, OPE = 100),
    tibble(RA = 0,   AA = 0,   OPE = 0),
    contact_percentiles
  ) %>%
    select(RA, AA, OPE)
  
  # Define file path for radar plot
  contact_radar_path <- file.path(athlete_dir, "contact_radar_plot.png")
  
  # Save the radar plot to the athlete's temp directory
  png(contact_radar_path, width = 750, height = 750, bg = "transparent")
  par(col = "white")
  
  # Contact Radar Plot
  radarchart(
    contact_radar_data,
    axistype = 0,
    pcol = "#C0C0C0",
    pfcol = scales::alpha("white", 0.3),
    plwd = 7,
    cglcol = "#C0C0C0",
    cglty = 1,
    axislabcol = "white",
    vlcex = 2.5
  )
  
  dev.off()
  
  # # Add text for the percentile
  # text(x = -0.2, y = 1.1,    # Adjust x, y coordinates as needed
  #      labels = rotAcc_percentile,
  #      col = rotAcc_color,  # Use dynamic color
  #      cex = 2.5,              # Adjust text size
  #      font = 2)
  # 
  # text(x = 0.2, y = 1.09,    # Adjust x, y coordinates as needed
  #      labels = "Percentile",
  #      col = "white",  # Use dynamic color
  #      cex = 1.85,              # Adjust text size
  #      font = 1)    
  
  # Read & composite the radar plot image in-place
  radar_img  <- image_read(contact_radar_path)
  report_img <- image_composite(report_img, radar_img, offset = "+75+1750")
  
  # Free the temporary image from memory
  rm(radar_img)
  gc()
  
  
  
  # # Add text for the percentile
  # text(x = -0.2, y = 1.1,    # Adjust x, y coordinates as needed
  #      labels = batSpeed_percentile,
  #      col = batSpeed_color,  # Use dynamic color
  #      cex = 2.5,              # Adjust text size
  #      font = 2)
  # 
  # text(x = 0.2, y = 1.09,    # Adjust x, y coordinates as needed
  #      labels = "Percentile",
  #      col = "white",  # Use dynamic color
  #      cex = 1.85,              # Adjust text size
  #      font = 1)  
  
  # Power Table
  power_data <- tibble(
    `Blast Metrics` = c("Bat Speed", "Time to Contact", "Vertical Bat Angle"),
    Value = c(
      paste0(round(current_athlete_summary$BatSpeed, 1), " mph"),
      paste0(round(current_athlete_summary$TTC, 3), " sec"),
      paste0(round(current_athlete_summary$VertBatAngle, 1), " °")
    ),
    Percentile = c(
      current_athlete_summary$BatSpeed_Percentile, 
      current_athlete_summary$TTC_Percentile, 
      current_athlete_summary$VerticalBatAngle_Percentile
    )
  )
  
  power_table <- gt(power_data) %>% 
    cols_align(align = "right", columns = Value) %>%
    cols_width(
      `Blast Metrics` ~ px(150),
      Value ~ px(100)
    ) %>%
    cols_hide(columns = Percentile) %>%
    tab_options(
      column_labels.font.weight = "bold",
      table.background.color = "black",
      table.font.color = "white",
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden",
      data_row.padding = px(5),
      table.font.size = px(13)
    ) %>%
    opt_table_font(font = "Helvetica") %>% 
    tab_style(
      style = cell_text(color = "#FF0000"),  # Red
      locations = cells_body(
        columns = Value,
        rows = Percentile < 33
      )
    ) %>%
    tab_style(
      style = cell_text(color = "#FFFF00"),  # Yellow
      locations = cells_body(
        columns = Value,
        rows = Percentile >= 33 & Percentile < 66
      )
    ) %>%
    tab_style(
      style = cell_text(color = "#008000"),  # Green
      locations = cells_body(
        columns = Value,
        rows = Percentile >= 66
      )
    ) %>%
    tab_style(
      style = cell_text(color = "#3d9be9"),
      locations = cells_column_labels()
    )
  
  # Composite your power table in-place
  power_table_path <- file.path(athlete_dir, paste0(athlete, "_power_table.png"))
  gtsave(power_table, file = power_table_path, expand = -1)
  
  power_img  <- image_read(power_table_path) %>%
    image_transparent(color = "black")
  report_img <- image_composite(report_img, power_img, offset = "+217+2415")
  
  rm(power_img)
  gc()
  
  
  damage_percentiles <- current_athlete_summary %>%
    select(BatSpeed_Percentile, TTC_Percentile, VerticalBatAngle_Percentile)
  
  # Rename columns for plot labels
  colnames(damage_percentiles) <- c("BS", "TTC", "VBA")
  
  # Prepare data for radar chart
  damage_radar_data <- bind_rows(
    tibble(BS = 100, TTC = 100, VBA = 100),
    tibble(BS = 0,   TTC = 0,   VBA = 0),
    damage_percentiles
  ) %>%
    select(BS, VBA, TTC)
  
  # Define file path for damage radar plot
  damage_radar_path <- file.path(athlete_dir, "damage_radar_plot.png")
  
  # Save the radar plot to the athlete's temp directory
  png(damage_radar_path, width = 750, height = 750, bg = "transparent")
  par(col = "white")
  
  # Damage Plot
  radarchart(
    damage_radar_data,
    axistype = 0,
    pcol = "#C0C0C0",
    pfcol = scales::alpha("white", 0.3),
    plwd = 7,
    cglcol = "#C0C0C0",
    cglty = 1,
    axislabcol = "white",
    vlcex = 2.5
  )
  
  dev.off()
  
  # Read & composite the damage radar plot in-place
  damage_radar_img <- image_read(damage_radar_path)
  report_img <- image_composite(report_img, damage_radar_img, offset = "+75+2625")
  rm(damage_radar_img)
  gc()
  
  # Write out the final report
  final_report_path <- file.path(athlete_dir, "FuturesHittingReport.png")
  image_write(report_img,
              path    = final_report_path,
              format  = "png")
  
  # Return the path for downloadHandler()
  final_report_path
  
  }
}