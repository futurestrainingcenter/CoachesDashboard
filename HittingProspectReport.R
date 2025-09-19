library(tidyverse)
library(readxl)
library(fmsb)
library(magick)
library(showtext)

if (file.exists("www/good times rg.otf")) {
  font_add("Good Times", regular = "www/good times rg.otf")
} else {
  warning("Custom font not found; falling back to default.")
}
showtext_auto()

generate_hitting_report <- function(client_data, hitting_data, pitching_data, strength_data, speed_data, athlete) {  

report_img <- image_read_pdf("www/Hitting Cover Template.pdf")

filtered_pitching <- pitching_data %>% filter(TaggedPitchType == "Fastball")
performance_data <- full_join(hitting_data, filtered_pitching, by = c("Date", "Month", "Year", "Name", "Level", "Skill", "Gender"))
physicality_data <- full_join(strength_data, speed_data, by = c("Date", "Month", "Year", "Name", "Level", "Skill", "Gender"))


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

normalize_ec <- function(ec_values) {
  ideal_min <- 88
  ideal_max <- 105
  perfect   <- 95
  min_bound <- 75
  max_bound <- 120
  
  sapply(ec_values, function(x) {
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

normalize_conn <- function(conn_values) {
  ideal_min <- 80
  ideal_max <- 100
  perfect   <- 90 # (ideal_min + ideal_max) / 2
  min_bound <- 60
  max_bound <- 110
  
  sapply(conn_values, function(x) {
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

# --- Helper Function to Define Level Progression ---
# This function determines the next competition level based on the current one.
get_next_skill <- function(current_skill) {
  # Define the order of progression
  skill_progression <- c(
    "Youth"        = "Intermediate", 
    "Intermediate" = "Varsity", 
    "JV"           = "Collegiate",
    "Varsity"      = "Collegiate",
    "Collegiate"   = "Professional",
    "Professional" = "Professional" # Stays at the top level
  )
  # Look up the next level; returns NA if current_level is not found
  return(skill_progression[current_skill])
}

  
  
# Define a temp directory for this athlete
athlete_dir <- file.path(tempdir(), athlete)
if (!dir.exists(athlete_dir)) {
  dir.create(athlete_dir, recursive = TRUE)
}



# Athlete Bio -------------------------------------------------------------



athlete_info <- client_data %>% 
  filter(Name == athlete) %>% 
  mutate(
    across(where(is.character), ~ ifelse(is.na(.) | . == "N/A", "--", .)),
    across(where(is.numeric), ~ ifelse(is.na(.) | . == "N/A", "--", .))
  )

# Generate ggplot
player_profile <- ggplot() +
  xlim(0, 1) +
  ylim(0, 1) +
  # Athlete's Name (Top line) - Use the "Black" weight
  annotate("text", x = 0.1, y = 0.9, label = athlete_info$Name,
           hjust = 0, size = 26, color = "black", family = "Good Times") +
  
  # High School and Graduating Class (Middle line) - Use the "SemiBold" weight
  annotate("text", x = 0.1, y = 0.5, label = paste0(athlete_info$`High School`, "  |  Class of ", athlete_info$`Graduating Class`),
           hjust = 0, size = 13, fontface = "bold", color = "black") +
  
  # Age, Position, and HT/WT (Bottom line) - Use the "Medium" weight
  annotate("text", x = 0.1, y = 0.2,
           label = paste0(athlete_info$`Position (Baseball/Softball)`, "  |  ",
                          athlete_info$Height, " ", athlete_info$Weight, "lbs  |  Age: ", athlete_info$Age),
           hjust = 0, size = 13, fontface = "bold", color = "black") +
  
  theme_void()


# 1. Render your attendance plot, write it out, then composite it directly back into report_img
profile_path <- file.path(athlete_dir, paste0(athlete, "_playerProfile.png"))
ggsave(profile_path, plot = player_profile, width = 15, height = 2.5, units = "in", dpi = 150)

# 2. Read & composite into report_img (no new HittingReport2 variable)
playerProfileImg <- image_read(profile_path)
report_img       <- image_composite(report_img, playerProfileImg, offset = "+425+60")

# 3. Free memory before the next overlay
rm(playerProfileImg)
gc()



# Strength Profile --------------------------------------------------------




# 1. Get the latest record for the selected athlete
# This part remains the same: it finds the most recent non-NA value for each metric for the specified athlete.
athlete_strength_summary <- physicality_data %>%
  filter(Name == athlete) %>%
  arrange(desc(as.Date(Date))) %>%
  summarise(
    Name   = first(Name),
    Level  = first(Level),
    Skill  = first(Skill),
    Gender = first(Gender),
    across(
      .cols = c(CMJ, ShotPut, TrunkRotation, D2Average, thirty_yard),
      .fns = ~ first(na.omit(.x), default = NA_real_)
    ),
    .groups = "drop"
  ) %>% 
  mutate(Skill = if_else(Skill == "JV", "Varsity", Skill))


# 2. Determine the athlete's current and next competition levels
current_gender <- athlete_strength_summary$Gender
current_skill  <- athlete_strength_summary$Skill
next_skill     <- get_next_skill(current_skill)

# --- CURRENT LEVEL PERCENTILE CALCULATION ---

# 3. Summarize facility data for the athlete's CURRENT skill
facility_strength_current <- physicality_data %>%
  filter(
    Skill  == current_skill,
    Gender == current_gender
  ) %>%
  group_by(Name, Gender) %>%
  summarise(
    CMJ          = if (all(is.na(CMJ))) NA_real_ else max(CMJ, na.rm = TRUE),
    ShotPut      = if (all(is.na(ShotPut))) NA_real_ else max(ShotPut, na.rm = TRUE),
    TrunkRotation= if (all(is.na(TrunkRotation))) NA_real_ else max(TrunkRotation, na.rm = TRUE),
    D2Average    = if (all(is.na(D2Average))) NA_real_ else max(D2Average, na.rm = TRUE),
    thirty_yard  = if (all(is.na(thirty_yard))) NA_real_ else min(thirty_yard, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  # Combine with the athlete's specific data
  filter(Name != athlete) %>%
  bind_rows(athlete_strength_summary)

# 4. Compute percentiles against the CURRENT skill group
strength_percentiles_current <- facility_strength_current %>%
  mutate(
    CMJ_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((CMJ - 6300)   /  1116)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((CMJ - 6750) /  868.1) * 100),
        TRUE                                       ~ round(percent_rank(CMJ) * 100)
      ),
    Trunk_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((TrunkRotation - 300)   /  61.5)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((TrunkRotation - 315) /  60) * 100),
        TRUE                                       ~ round(percent_rank(TrunkRotation) * 100)
      ),
    D2_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((D2Average - 65)   /  11.5)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((D2Average - 70) /  11) * 100),
        TRUE                                       ~ round(percent_rank(D2Average) * 100)
      ),
    Thirty_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((3.80 - thirty_yard) / 0.20) * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((3.75 - thirty_yard) / 0.18) * 100),
        TRUE                                       ~ round(percent_rank(-thirty_yard) * 100)
      )
  ) %>%
  filter(Name == athlete) # Isolate the athlete's row


# --- NEXT LEVEL PERCENTILE CALCULATION ---

# 5. Summarize facility data for the athlete's NEXT level
facility_strength_next <- physicality_data %>%
  filter(
    Skill  == next_skill, # The only change is filtering for the next level
    Gender == current_gender
  ) %>%
  group_by(Name) %>%
  summarise(
    Level        = first(Level),
    Skill        = first(Skill),
    Gender       = first(Gender),
    CMJ          = if (all(is.na(CMJ))) NA_real_ else max(CMJ, na.rm = TRUE),
    ShotPut      = if (all(is.na(ShotPut))) NA_real_ else max(ShotPut, na.rm = TRUE),
    TrunkRotation= if (all(is.na(TrunkRotation))) NA_real_ else max(TrunkRotation, na.rm = TRUE),
    D2Average    = if (all(is.na(D2Average))) NA_real_ else max(D2Average, na.rm = TRUE),
    thirty_yard  = if (all(is.na(thirty_yard))) NA_real_ else min(thirty_yard, na.rm = TRUE),
    .groups  = "drop"
  )


athlete_summary_for_next_skill <- athlete_strength_summary %>%
  mutate(Skill = next_skill)


# 6. Compute percentiles against the NEXT level group
strength_percentiles_next <- facility_strength_next %>%
  bind_rows(athlete_summary_for_next_skill) %>%
  mutate(
    CMJ_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((CMJ - 6300)   /  1116)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((CMJ - 6750) /  868.1) * 100),
        TRUE                                       ~ round(percent_rank(CMJ) * 100)
      ),
    Trunk_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((TrunkRotation - 300)   /  61.5)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((TrunkRotation - 315) /  60) * 100),
        TRUE                                       ~ round(percent_rank(TrunkRotation) * 100)
      ),
    D2_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((D2Average - 65)   /  11.5)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((D2Average - 70) /  11) * 100),
        TRUE                                       ~ round(percent_rank(D2Average) * 100)
      ),
    Thirty_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((3.80 - thirty_yard) / 0.20) * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((3.75 - thirty_yard) / 0.18) * 100),
        TRUE                                       ~ round(percent_rank(-thirty_yard) * 100)
      )
  ) %>%
  filter(Name == athlete) # Isolate the athlete's row


# 7. Build the final data frame for plotting with Current and Next percentiles
athlete_strength_percentiles <- tibble(
  Metric = factor(
    c("Speed", "Shoulder", "Core Rotation", "Lower Body"),
    levels = c("Speed", "Shoulder", "Core Rotation", "Lower Body")
  ),
  Current = c(
    strength_percentiles_current$Thirty_Percentile,
    strength_percentiles_current$D2_Percentile,
    strength_percentiles_current$Trunk_Percentile,
    strength_percentiles_current$CMJ_Percentile
  ),
  Next = c(
    strength_percentiles_next$Thirty_Percentile,
    strength_percentiles_next$D2_Percentile,
    strength_percentiles_next$Trunk_Percentile,
    strength_percentiles_next$CMJ_Percentile
  )
) %>%
  mutate(
    # Score category is based ONLY on the current percentile
    score_category = case_when(
      Current >= 80  ~ "Good",
      Current >= 60  ~ "Average",
      TRUE           ~ "Needs Improvement"
    ),
    score_category = factor(score_category, levels = c("Good", "Average", "Needs Improvement"))
  )


# --- CURRENT LEVEL PLOT CODE ---
strength_plot <- ggplot(athlete_strength_percentiles, aes(x = Metric)) +
  # The grey background bar, drawn from y=0 to y=100
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = 100), color = "#3a3d46", linewidth = 8, lineend = "round", show.legend = FALSE) +
  
  # The main score bar, drawn from y=0 to y=Current
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = pmax(Current, 1), color = score_category), linewidth = 8, lineend = "round", show.legend = FALSE) +
  
  # Text label for the Metric name
  geom_text(
    aes(label = Metric, y = 0),
    hjust = 0,
    vjust = -2.5,
    color = "white",
    fontface = "bold",
    size = 8.5
  ) +
  
  # Text label for the 'Current' percentile value
  geom_text(
    data = . %>% filter(!is.na(Current)),
    aes(label = Current, y = 100),
    hjust = 1,
    vjust = -2,
    color = "white",
    fontface = "bold",
    size = 10
  ) +
  
  # Manual color scale for the score categories (using scale_color_manual)
  scale_color_manual(
    name = "Performance",
    values = c(
      "Good" = "#00bf63",
      "Average" = "orange",
      "Needs Improvement" = "red"
    ),
    guide = "none" # Hide the legend
  ) +
  
  coord_flip() +
  theme_void()


# --- NEXT LEVEL PLOT CODE ---
next_strength_plot <- ggplot(athlete_strength_percentiles, aes(x = Metric)) +
  # The grey background bar, drawn from y=0 to y=100
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = 100), color = "#3a3d46", linewidth = 4, lineend = "round", show.legend = FALSE) +
  
  # The main score bar, drawn from y=0 to y=Current
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = pmax(Next, 1)), color = "#3d9be9", linewidth = 4, lineend = "round", show.legend = FALSE) +

  coord_flip() +
  theme_void()

# --- Process and composite the 'strength_plot' ---
# 1. Create a temporary file path
strength_plot_path <- tempfile(fileext = ".png")

# 2. Save the plot to the temporary path
ggsave(strength_plot_path, plot = strength_plot, units = "in", width = 5, height = 5, dpi = 175)

# 3. Read the image back and composite it onto the main report
strength_img <- image_read(strength_plot_path)
report_img   <- image_composite(report_img, strength_img, offset = "+172+890")

# 4. Clean up the image object to free memory
rm(strength_img)
gc()


# --- Process and composite the 'next_strength_plot' ---
# 1. Create a second temporary file path
next_strength_plot_path <- tempfile(fileext = ".png")

# 2. Save the plot to the temporary path
ggsave(next_strength_plot_path, plot = next_strength_plot, units = "in", width = 5.1, height = 5, dpi = 175)

# 3. Read the image back and composite it
next_strength_img <- image_read(next_strength_plot_path)
report_img        <- image_composite(report_img, next_strength_img, offset = "+164+935")

# 4. Clean up the image object
rm(next_strength_img)
gc()

  


# Hitting Profile ---------------------------------------------------------




# 1. Get the latest record for the selected athlete
athlete_performance_summary <- performance_data %>%
  filter(Name == athlete) %>%
  arrange(desc(as.Date(Date))) %>%
  summarise(
    Name   = first(Name),
    Level  = first(Level),
    Skill  = first(Skill),
    Gender = first(Gender),
    across(
      .cols = c(MaxVel, AvgVel, MaxDist, AvgDist, bat_speed,
                rotational_acceleration, on_plane_efficiency,
                attack_angle, early_connection, connection_at_impact,
                vertical_bat_angle, time_to_contact, Max_RelSpeed),
      .fns = ~ first(na.omit(.x), default = NA_real_)
    ),
    CompetitiveSwings     = first(na.omit(CompetitiveSwings), default = NA_real_),
    SwingCount            = first(na.omit(SwingCount), default = NA_real_),
    CompetitiveSwings_Pct = ifelse(SwingCount == 0, NA_real_, round(CompetitiveSwings / SwingCount * 100, 1)),
    .groups = "drop"
  ) %>% 
  mutate(Skill = if_else(Skill == "JV", "Varsity", Skill))

# 2. Determine the athlete's current and next competition levels
current_gender <- athlete_performance_summary$Gender
current_skill  <- athlete_performance_summary$Skill
next_skill     <- get_next_skill(current_skill)
 
# --- CURRENT LEVEL PERCENTILE CALCULATION ---

# 3. Summarize facility data for the athlete's CURRENT level
facility_performance_current <- performance_data %>%
  filter(
    Skill  == current_skill,
    Gender == current_gender
  ) %>%
  group_by(Name) %>%
  summarise(
    Level        = first(Level),
    Skill        = first(Skill),
    Gender       = first(Gender),
    MaxVel                 = if(all(is.na(MaxVel))) NA_real_ else max(MaxVel, na.rm = TRUE),
    AvgVel                 = if(all(is.na(AvgVel))) NA_real_ else mean(AvgVel, na.rm = TRUE),
    MaxDist                = if(all(is.na(MaxDist))) NA_real_ else max(MaxDist, na.rm = TRUE),
    AvgDist                = if(all(is.na(AvgDist))) NA_real_ else mean(AvgDist, na.rm = TRUE),
    bat_speed              = if(all(is.na(bat_speed))) NA_real_ else mean(bat_speed[bat_speed >= quantile(bat_speed, 0.25, na.rm = TRUE)], na.rm = TRUE),
    rotational_acceleration = if(all(is.na(rotational_acceleration))) NA_real_ else mean(rotational_acceleration, na.rm = TRUE),
    on_plane_efficiency     = if(all(is.na(on_plane_efficiency))) NA_real_ else mean(on_plane_efficiency, na.rm = TRUE),
    attack_angle            = if(all(is.na(attack_angle))) NA_real_ else mean(attack_angle, na.rm = TRUE),
    early_connection        = if(all(is.na(early_connection))) NA_real_ else mean(early_connection, na.rm = TRUE),
    connection_at_impact    = if(all(is.na(connection_at_impact))) NA_real_ else mean(connection_at_impact, na.rm = TRUE),
    vertical_bat_angle      = if(all(is.na(vertical_bat_angle))) NA_real_ else mean(vertical_bat_angle, na.rm = TRUE),
    time_to_contact         = if(all(is.na(time_to_contact))) NA_real_ else mean(time_to_contact, na.rm = TRUE),
    CompetitiveSwings       = if(all(is.na(CompetitiveSwings))) NA_real_ else sum(CompetitiveSwings, na.rm = TRUE),
    SwingCount              = if(all(is.na(SwingCount))) NA_real_ else sum(SwingCount, na.rm = TRUE),
    CompetitiveSwings_Pct   = ifelse(SwingCount == 0, NA_real_, round(CompetitiveSwings / SwingCount * 100, 1)),
    Max_RelSpeed            = if(all(is.na(Max_RelSpeed))) NA_real_ else max(Max_RelSpeed, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Compute percentiles against the CURRENT level group
performance_percentiles_current <- facility_performance_current %>%
  filter(Name != athlete) %>%
  bind_rows(athlete_performance_summary) %>%
  mutate(
    MaxVel_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((MaxVel - 103.8)   /  7.06)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((MaxVel - 108.9622)/  4.008935) * 100),
        TRUE                                       ~ round(percent_rank(MaxVel) * 100)
      ),
    AvgVel_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((AvgVel - 82.2)     /  6.01)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((AvgVel - 87.41388)/  3.481693) * 100),
        TRUE                                       ~ round(percent_rank(AvgVel) * 100)
      ),
    MaxDist_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((MaxDist - 367)    / 58.39)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((MaxDist - 411.4609)/ 35.09507) * 100),
        TRUE                                       ~ round(percent_rank(MaxDist) * 100)
      ),
    BatSpeed_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((bat_speed - 69.11) /  3.37)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((bat_speed - 71.645)/  3.318)    * 100),
        TRUE                                       ~ round(percent_rank(bat_speed) * 100)
      ),
    RotationalAcceleration_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((rotational_acceleration - 13.67) /  2.98)   * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((rotational_acceleration - 14.874)/  2.684)  * 100),
        TRUE                                       ~ round(percent_rank(rotational_acceleration) * 100)
      ),
    TimeToContact_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round((1 - pnorm((time_to_contact - 0.152) / 0.01))   * 100),
        Skill == "Professional" & Gender == "Male" ~ round((1 - pnorm((time_to_contact - 0.149) / 0.008))  * 100),
        TRUE                                       ~ round(percent_rank(-time_to_contact) * 100)  # invert rank
      ),
    CompetitiveSwings_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((CompetitiveSwings_Pct - 70) /  20.5)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((CompetitiveSwings_Pct - 75)/  18.5)    * 100),
        TRUE                                       ~ round(percent_rank(CompetitiveSwings_Pct) * 100)
      ),
    MaxRelSpeed_Percentile = case_when(
        Skill == "Collegiate"   ~ round(pnorm((Max_RelSpeed   - 90.0)      /  4.15)     * 100),
        Skill == "Professional" ~ round(pnorm((Max_RelSpeed   - 96.35804)  /  2.724521) * 100),
        TRUE                    ~ round(percent_rank(Max_RelSpeed)        * 100)
      ),
    OnPlaneEfficiency_Percentile  = normalize_ope(on_plane_efficiency),
    AttackAngle_Percentile        = normalize_attack_angle(attack_angle),
    EarlyConnection_Percentile    = normalize_ec(early_connection),
    ConnectionAtImpact_Percentile = normalize_conn(connection_at_impact),
    VerticalBatAngle_Percentile   = normalize_vba(vertical_bat_angle)
  ) %>% 
  ungroup() %>% 
  mutate(
    Load_Rank = (EarlyConnection_Percentile + BatSpeed_Percentile) / 2,
    SwingPlane_Rank = (OnPlaneEfficiency_Percentile + AttackAngle_Percentile) / 2,
    Sequencing_Rank = (RotationalAcceleration_Percentile + TimeToContact_Percentile) / 2,
    ImpactPosition_Rank = (VerticalBatAngle_Percentile + ConnectionAtImpact_Percentile) / 2
  ) %>%
  filter(Name == athlete)


# --- NEXT LEVEL PERCENTILE CALCULATION ---

# 5. Summarize facility data for the athlete's NEXT level
facility_performance_next <- performance_data %>%
  filter(
    Skill  == next_skill,
    Gender == current_gender
  ) %>%
  group_by(Name) %>%
  summarise(
    Level        = first(Level),
    Skill        = first(Skill),
    Gender       = first(Gender),
    MaxVel                 = if(all(is.na(MaxVel))) NA_real_ else max(MaxVel, na.rm = TRUE),
    AvgVel                 = if(all(is.na(AvgVel))) NA_real_ else mean(AvgVel, na.rm = TRUE),
    MaxDist                = if(all(is.na(MaxDist))) NA_real_ else max(MaxDist, na.rm = TRUE),
    AvgDist                = if(all(is.na(AvgDist))) NA_real_ else mean(AvgDist, na.rm = TRUE),
    bat_speed              = if(all(is.na(bat_speed))) NA_real_ else mean(bat_speed[bat_speed >= quantile(bat_speed, 0.25, na.rm = TRUE)], na.rm = TRUE),
    rotational_acceleration = if(all(is.na(rotational_acceleration))) NA_real_ else mean(rotational_acceleration, na.rm = TRUE),
    on_plane_efficiency     = if(all(is.na(on_plane_efficiency))) NA_real_ else mean(on_plane_efficiency, na.rm = TRUE),
    attack_angle            = if(all(is.na(attack_angle))) NA_real_ else mean(attack_angle, na.rm = TRUE),
    early_connection        = if(all(is.na(early_connection))) NA_real_ else mean(early_connection, na.rm = TRUE),
    connection_at_impact    = if(all(is.na(connection_at_impact))) NA_real_ else mean(connection_at_impact, na.rm = TRUE),
    vertical_bat_angle      = if(all(is.na(vertical_bat_angle))) NA_real_ else mean(vertical_bat_angle, na.rm = TRUE),
    time_to_contact         = if(all(is.na(time_to_contact))) NA_real_ else mean(time_to_contact, na.rm = TRUE),
    CompetitiveSwings       = if(all(is.na(CompetitiveSwings))) NA_real_ else sum(CompetitiveSwings, na.rm = TRUE),
    SwingCount              = if(all(is.na(SwingCount))) NA_real_ else sum(SwingCount, na.rm = TRUE),
    CompetitiveSwings_Pct   = ifelse(SwingCount == 0, NA_real_, round(CompetitiveSwings / SwingCount * 100, 1)),
    Max_RelSpeed            = if(all(is.na(Max_RelSpeed))) NA_real_ else max(Max_RelSpeed, na.rm = TRUE),
    .groups = "drop"
  )

athlete_summary_for_next_skill <- athlete_performance_summary %>%
  mutate(Skill = next_skill)

# 6. Compute percentiles against the NEXT level group
performance_percentiles_next <- facility_performance_next %>%
  bind_rows(athlete_summary_for_next_skill) %>%
  mutate(
    MaxVel_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((MaxVel - 103.8)   /  7.06)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((MaxVel - 108.9622)/  4.008935) * 100),
        TRUE                                       ~ round(percent_rank(MaxVel) * 100)
      ),
    AvgVel_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((AvgVel - 82.2)     /  6.01)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((AvgVel - 87.41388)/  3.481693) * 100),
        TRUE                                       ~ round(percent_rank(AvgVel) * 100)
      ),
    MaxDist_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((MaxDist - 367)    / 58.39)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((MaxDist - 411.4609)/ 35.09507) * 100),
        TRUE                                       ~ round(percent_rank(MaxDist) * 100)
      ),
    BatSpeed_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((bat_speed - 69.11) /  3.37)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((bat_speed - 71.645)/  3.318)    * 100),
        TRUE                                       ~ round(percent_rank(bat_speed) * 100)
      ),
    RotationalAcceleration_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((rotational_acceleration - 13.67) /  2.98)   * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((rotational_acceleration - 14.874)/  2.684)  * 100),
        TRUE                                       ~ round(percent_rank(rotational_acceleration) * 100)
      ),
    TimeToContact_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round((1 - pnorm((time_to_contact - 0.152) / 0.01))   * 100),
        Skill == "Professional" & Gender == "Male" ~ round((1 - pnorm((time_to_contact - 0.149) / 0.008))  * 100),
        TRUE                                       ~ round(percent_rank(-time_to_contact) * 100)  # invert rank
      ),
    CompetitiveSwings_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((CompetitiveSwings_Pct - 70) /  20.5)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((CompetitiveSwings_Pct - 75)/  18.5)    * 100),
        TRUE                                       ~ round(percent_rank(CompetitiveSwings_Pct) * 100)
      ),
    MaxRelSpeed_Percentile = case_when(
        Skill == "Collegiate"   ~ round(pnorm((Max_RelSpeed   - 90.0)      /  4.15)     * 100),
        Skill == "Professional" ~ round(pnorm((Max_RelSpeed   - 96.35804)  /  2.724521) * 100),
        TRUE                    ~ round(percent_rank(Max_RelSpeed)        * 100)
      ),
    OnPlaneEfficiency_Percentile  = normalize_ope(on_plane_efficiency),
    AttackAngle_Percentile        = normalize_attack_angle(attack_angle),
    EarlyConnection_Percentile    = normalize_ec(early_connection),
    ConnectionAtImpact_Percentile = normalize_conn(connection_at_impact),
    VerticalBatAngle_Percentile   = normalize_vba(vertical_bat_angle)
  ) %>% 
  ungroup() %>% 
  mutate(
    Load_Rank = (EarlyConnection_Percentile + BatSpeed_Percentile) / 2,
    SwingPlane_Rank = (OnPlaneEfficiency_Percentile + AttackAngle_Percentile) / 2,
    Sequencing_Rank = (RotationalAcceleration_Percentile + TimeToContact_Percentile) / 2,
    ImpactPosition_Rank = (VerticalBatAngle_Percentile + ConnectionAtImpact_Percentile) / 2
  ) %>%
  filter(Name == athlete)


# 7. Build the final data frame for plotting with Current and Next percentiles
athlete_hitting_percentiles <- tibble(
  Metric     = factor(
    c("Impact Position", "Sequencing", "Swing Plane", "Load"),
    levels = c("Impact Position", "Sequencing", "Swing Plane", "Load")
  ),
  Current = c(
    round(performance_percentiles_current$ImpactPosition_Rank),
    round(performance_percentiles_current$Sequencing_Rank),
    round(performance_percentiles_current$SwingPlane_Rank),
    round(performance_percentiles_current$Load_Rank)
  ),
  Next = c(
    round(performance_percentiles_next$ImpactPosition_Rank),
    round(performance_percentiles_next$Sequencing_Rank),
    round(performance_percentiles_next$SwingPlane_Rank),
    round(performance_percentiles_next$Load_Rank)
  )
) %>%
  mutate(
    # Score category is based ONLY on the current percentile
    score_category = case_when(
      Current >= 80  ~ "Good",
      Current >= 60  ~ "Average",
      TRUE           ~ "Needs Improvement"
    ),
    score_category = factor(score_category, levels = c("Good", "Average", "Needs Improvement"))
  )

# --- Plot Code using the new category ---
hitting_plot <- ggplot(athlete_hitting_percentiles, aes(x = Metric)) +
  # The grey background bar, drawn from y=0 to y=100
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = 100), color = "#3a3d46", linewidth = 8, lineend = "round", show.legend = FALSE) +
  
  # The main score bar, drawn from y=0 to y=Current
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = pmax(Current, 1), color = score_category), linewidth = 8, lineend = "round", show.legend = FALSE) +
  
  # Text label for the Metric name
  geom_text(
    aes(label = Metric, y = 0),
    hjust = 0,
    vjust = -2.5,
    color = "white", 
    fontface = "bold", 
    size = 8.5
  ) +
  
  # Text label for the 'Current' percentile value
  geom_text(
    data = . %>% filter(!is.na(Current)),
    aes(label = Current, y = 100),
    hjust = 1,
    vjust = -2,
    color = "white", 
    fontface = "bold", 
    size = 10
  ) +
  
  # Manual color scale for the score categories (using scale_color_manual)
  scale_color_manual(
    name = "Performance",
    values = c(
      "Good" = "#00bf63",
      "Average" = "orange",
      "Needs Improvement" = "red"
    ),
    guide = "none" # Hide the legend
  ) +
  
  coord_flip() +
  theme_void()


# --- NEXT LEVEL PLOT CODE ---
next_hitting_plot <- ggplot(athlete_hitting_percentiles, aes(x = Metric)) +
  # The grey background bar, drawn from y=0 to y=100
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = 100), color = "#3a3d46", linewidth = 4, lineend = "round", show.legend = FALSE) +
  
  # The main score bar, drawn from y=0 to y=Current
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = pmax(Next, 1)), color = "#3d9be9", linewidth = 4, lineend = "round", show.legend = FALSE) +

  coord_flip() +
  theme_void()


# --- Process and composite the 'hitting_plot' ---
# 1. Create a temporary file path
hitting_plot_path <- tempfile(fileext = ".png")

# 2. Save the plot to the temporary path
ggsave(hitting_plot_path, plot = hitting_plot, units = "in", width = 5, height = 5, dpi = 175)

# 3. Read the image back and composite it onto the main report
hitting_img <- image_read(hitting_plot_path)
report_img  <- image_composite(report_img, hitting_img, offset = "+1212+890")

# 4. Clean up the image object to free memory
rm(hitting_img)
gc()


# --- Process and composite the 'next_hitting_plot' ---
# 1. Create another temporary file path
next_hitting_plot_path <- tempfile(fileext = ".png")

# 2. Save the plot to the temporary path
ggsave(next_hitting_plot_path, plot = next_hitting_plot, units = "in", width = 5.1, height = 5, dpi = 175)

# 3. Read the image back and composite it
next_hitting_img <- image_read(next_hitting_plot_path)
report_img       <- image_composite(report_img, next_hitting_img, offset = "+1204+935")

# 4. Clean up the image object
rm(next_hitting_img)
gc()




# Performance Profile -----------------------------------------------------




# 1) build a new 3‐row tibble
athlete_performance_percentiles <- tibble(
  Metric     = factor(
    c("Throwing Velocity", "Bat Speed", "Max Distance", "Max Exit Velocity"),
    levels = c("Throwing Velocity", "Bat Speed", "Max Distance", "Max Exit Velocity")
  ),
  Current = c(
    round(performance_percentiles_current$MaxRelSpeed_Percentile),
    round(performance_percentiles_current$BatSpeed_Percentile),
    round(performance_percentiles_current$MaxDist_Percentile),
    round(performance_percentiles_current$MaxVel_Percentile)
  ),
  Next = c(
    round(performance_percentiles_next$MaxRelSpeed_Percentile),
    round(performance_percentiles_next$BatSpeed_Percentile),
    round(performance_percentiles_next$MaxDist_Percentile),
    round(performance_percentiles_next$MaxVel_Percentile)
  ),
  Value = c(
    paste0(round(performance_percentiles_current$Max_RelSpeed), " mph"),
    paste0(round(performance_percentiles_current$bat_speed), " mph"),
    paste0(round(performance_percentiles_current$MaxDist), " ft"),
    paste0(round(performance_percentiles_current$MaxVel), " mph")
  )
) %>%
  mutate(
    # Score category is based ONLY on the current percentile
    score_category = case_when(
      Current >= 80  ~ "Good",
      Current >= 60  ~ "Average",
      TRUE           ~ "Needs Improvement"
    ),
    score_category = factor(score_category, levels = c("Good", "Average", "Needs Improvement"))
  )


# --- Plot Code using the new category ---
performance_plot <- ggplot(athlete_performance_percentiles, aes(x = Metric)) +
  # The grey background bar, drawn from y=0 to y=100
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = 100), color = "#3a3d46", linewidth = 8, lineend = "round", show.legend = FALSE) +
  
  # The main score bar, drawn from y=0 to y=Current
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = pmax(Current, 1), color = score_category), linewidth = 8, lineend = "round", show.legend = FALSE) +
  
  # --- ADD THIS LAYER FOR THE LABELS ---
  geom_text(
    aes(label = Metric), 
    y = 0,                 # Position text near the far left (y=0)
    hjust = 0,             # Left-justify the text
    vjust = -2.5,
    color = "white", 
    fontface = "bold", 
    size = 8.5
  ) +
  
  # --- ADD THIS LAYER FOR THE LABELS ---
  geom_text(
    data = . %>% filter(!is.na(Current)),
    aes(label = Current), 
    y = 100,                 # Position text near the far left (y=0)
    hjust = 1,             # Left-justify the text
    vjust = -2,
    color = "white", 
    fontface = "bold", 
    size = 10
  ) +
  
  # --- ADD THIS LAYER FOR THE LABELS ---
  geom_label(
    data = . %>% filter(!is.na(Current)),
    aes(label = Value), 
    y = 75,
    hjust = 0.5,
    vjust = -0.680,
    color = "white", 
    fill = NA,
    fontface = "bold",
    size = 8
  ) +
  
  # Manual color scale for the score categories (using scale_color_manual)
  scale_color_manual(
    name = "Performance",
    values = c(
      "Good" = "#00bf63",
      "Average" = "orange",
      "Needs Improvement" = "red"
    ),
    guide = "none" # Hide the legend
  ) +
  
  coord_flip() +
  theme_void()


# --- NEXT LEVEL PLOT CODE ---
next_performance_plot <- ggplot(athlete_performance_percentiles, aes(x = Metric)) +
  # The grey background bar, drawn from y=0 to y=100
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = 100), color = "#3a3d46", linewidth = 4, lineend = "round", show.legend = FALSE) +
  
  # The main score bar, drawn from y=0 to y=Current
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = pmax(Next, 1)), color = "#3d9be9", linewidth = 4, lineend = "round", show.legend = FALSE) +

  coord_flip() +
  theme_void()


# --- Process and composite the 'performance_plot' ---
# 1. Create a temporary file path
performance_plot_path <- tempfile(fileext = ".png")

# 2. Save the plot to the temporary path
ggsave(performance_plot_path, plot = performance_plot, units = "in", width = 5, height = 5, dpi = 175)

# 3. Read the image back and composite it onto the main report
performance_img <- image_read(performance_plot_path)
report_img      <- image_composite(report_img, performance_img, offset = "+2252+890")

# 4. Clean up the image object to free memory
rm(performance_img)
gc()


# --- Process and composite the 'next_performance_plot' ---
# 1. Create another temporary file path
next_performance_plot_path <- tempfile(fileext = ".png")

# 2. Save the plot to the temporary path
ggsave(next_performance_plot_path, plot = next_performance_plot, units = "in", width = 5.1, height = 5, dpi = 175)

# 3. Read the image back and composite it
next_performance_img <- image_read(next_performance_plot_path)
report_img           <- image_composite(report_img, next_performance_img, offset = "+2244+935")

# 4. Clean up the image object
rm(next_performance_img)
gc()



# Hitter Score ------------------------------------------------------------




avg_hitting_score <- round(mean(athlete_hitting_percentiles$Current, na.rm = TRUE))

hitting_gauge_data <- tibble(
  category = c("Score", "Remaining"),
  value = c(avg_hitting_score, 100 - avg_hitting_score)
)

# Determine the correct color based on the score's value
if (avg_hitting_score >= 80) {
  score_color <- "#00bf63"
} else if (avg_hitting_score >= 60) {
  score_color <- "orange"
} else {
  score_color <- "red"
}


hitter_score_plot <- ggplot(hitting_gauge_data, aes(x = 1, y = value, fill = category)) +
  geom_col(width = 0.4) +
  # Convert to polar coordinates to make it a circle
  coord_polar(theta = "y", start = pi, direction = 1) +
  # Set the fill colors manually
  scale_fill_manual(values = c("Score" = score_color, "Remaining" = "#3a3d46")) +
  # Add the score text in the middle
  annotate("text", x = 0, y = 0, label = avg_hitting_score, size = 24, color = "white",  family = "Good Times") +
  # Add the title text below the score
  # Set limits to ensure a full circle
  ylim(0, 100) +
  # Use a minimal theme
  theme_void() +
  theme(
    legend.position = "none"
  )

# --- Process and composite the 'hitter_score_plot' ---
hitter_score_path <- tempfile(fileext = ".png")
ggsave(hitter_score_path, plot = hitter_score_plot, width = 3, height = 3, units = "in", dpi = 150)

hitter_score_img <- image_read(hitter_score_path)
report_img       <- image_composite(report_img, hitter_score_img, offset = "+1425+1915")

rm(hitter_score_img)
gc()



# Strength Score ------------------------------------------------------------


avg_strength_score <- round(mean(athlete_strength_percentiles$Current, na.rm = TRUE))

strength_gauge_data <- tibble(
  category = c("Score", "Remaining"),
  value = c(avg_strength_score, 100 - avg_strength_score)
)

# Determine the correct color based on the score's value
if (avg_strength_score >= 80) {
  score_color <- "#00bf63"
} else if (avg_strength_score >= 60) {
  score_color <- "orange"
} else {
  score_color <- "red"
}


strength_score_plot <- ggplot(strength_gauge_data, aes(x = 1, y = value, fill = category)) +
  geom_col(width = 0.4) +
  # Convert to polar coordinates to make it a circle
  coord_polar(theta = "y", start = pi, direction = 1) +
  # Set the fill colors manually
  scale_fill_manual(values = c("Score" = score_color, "Remaining" = "#3a3d46")) +
  # Add the score text in the middle
  annotate("text", x = 0, y = 0, label = avg_strength_score, size = 24, color = "white",  family = "Good Times") +
  # Add the title text below the score
  # Set limits to ensure a full circle
  ylim(0, 100) +
  # Use a minimal theme
  theme_void() +
  theme(
    legend.position = "none"
  )


# --- Process and composite the 'strength_score_plot' ---
strength_score_path <- tempfile(fileext = ".png")
ggsave(strength_score_path, plot = strength_score_plot, width = 3, height = 3, units = "in", dpi = 150)

strength_score_img <- image_read(strength_score_path)
report_img         <- image_composite(report_img, strength_score_img, offset = "+380+1915")

rm(strength_score_img)
gc()



# Performance Score -------------------------------------------------------



avg_performance_score <- round(mean(athlete_performance_percentiles$Current, na.rm = TRUE))

performance_gauge_data <- tibble(
  category = c("Score", "Remaining"),
  value = c(avg_performance_score, 100 - avg_performance_score)
)

# Determine the correct color based on the score's value
if (avg_performance_score > 80) {
  score_color <- "#00bf63"
} else if (avg_performance_score >= 60) {
  score_color <- "orange"
} else {
  score_color <- "red"
}

performance_score_plot <- ggplot(performance_gauge_data, aes(x = 1, y = value, fill = category)) +
  geom_col(width = 0.4) +
  # Convert to polar coordinates to make it a circle
  coord_polar(theta = "y", start = pi, direction = 1) +
  # Set the fill colors manually
  scale_fill_manual(values = c("Score" = score_color, "Remaining" = "#3a3d46")) +
  # Add the score text in the middle
  annotate("text", x = 0, y = 0, label = avg_performance_score, size = 24, color = "white",  family = "Good Times") +
  # Add the title text below the score
  # Set limits to ensure a full circle
  ylim(0, 100) +
  # Use a minimal theme
  theme_void() +
  theme(
    legend.position = "none"
  )

# --- Process and composite the 'performance_score_plot' ---
performance_score_path <- tempfile(fileext = ".png")
ggsave(performance_score_path, plot = performance_score_plot, width = 3, height = 3, units = "in", dpi = 150)

performance_score_img <- image_read(performance_score_path)
report_img            <- image_composite(report_img, performance_score_img, offset = "+2460+1915")

rm(performance_score_img)
gc()
  
  
# Write out the final report
final_report_path <- file.path(athlete_dir, "HittingProspectProfile.png")
image_write(report_img,
            path    = final_report_path,
            format  = "png")

# Return the path for downloadHandler()
final_report_path
  
}