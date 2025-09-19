library(tidyverse)
library(tidymodels)
library(extrasteps)
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

generate_pitching_report <- function(client_data, trackman_data, strength_data, speed_data, fastball_model, fastball_params, 
                                    breaking_model, breaking_params, offspeed_model, offspeed_params, athlete) {  

report_img <- image_read_pdf("www/Pitching Cover Template.pdf")

physicality_data <- full_join(strength_data, speed_data, by = c("Date", "Month", "Year", "Name", "Level", "Skill", "Gender"))


prepare_facility_data <- function(raw_facility_df) {
  
  # --- A. Define Pitch Categories for Facility Data ---
  # NOTE: These names ("Fastball", "Slider") must match your facility data's `TaggedPitchType` column.
  is_fastball <- function(pt) pt %in% c("Fastball", "Sinker", "Cutter")
  is_breaking <- function(pt) pt %in% c("Slider", "Curveball")
  is_offspeed <- function(pt) pt %in% c("ChangeUp", "Splitter")
  
  # --- B. Column Name Mapping & Unit Conversion ---
  processed_df <- raw_facility_df %>%
    rename(
      release_speed = RelSpeed,
      spin_rate = SpinRate,
      extension = Extension,
      ax = ax0,
      az = az0,
      x0 = x0,
      z0 = z0,
      spin_axis = SpinAxis
    ) %>%
    # *** NEW: Add the pitch_category column ***
    mutate(
      pitch_category = case_when(
        is_fastball(TaggedPitchType) ~ "fastball",
        is_breaking(TaggedPitchType) ~ "breaking",
        is_offspeed(TaggedPitchType) ~ "offspeed",
        .default = NA_character_
      )
    ) %>%
    # We only want to predict on pitches we have a model for
    filter(!is.na(pitch_category)) 
  
  # --- C. Recreate ALL Feature Engineering Steps ---
  # 1. Mirror features for pitcher handedness
  processed_df <- processed_df %>%
    mutate(
      hand_sign = if_else(PitcherThrows == "Left", -1, 1),
      ax_mir = ax * hand_sign,
      x0_mir = x0 * hand_sign
    )
  
  # 2. Calculate pitcher-specific fastball baselines
  fb_baselines <- processed_df %>%
    filter(pitch_category == "fastball") %>%
    group_by(Name) %>%
    summarise(
      fb_velo   = median(release_speed, na.rm = TRUE),
      fb_ax_mir = median(ax_mir, na.rm = TRUE),
      fb_az     = median(az, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 3. Join baselines and create delta features with a fallback for pitchers who threw no fastballs
  processed_df <- processed_df %>%
    left_join(fb_baselines, by = "Name") %>%
    group_by(Name) %>%
    mutate(
      fastest_pitch_velo = max(release_speed, na.rm = TRUE),
      fastest_pitch_az   = az[which.max(release_speed)],
      fastest_pitch_ax   = ax_mir[which.max(release_speed)],
      fb_velo   = coalesce(fb_velo, fastest_pitch_velo),
      fb_az     = coalesce(fb_az, fastest_pitch_az),
      fb_ax_mir = coalesce(fb_ax_mir, fastest_pitch_ax)
    ) %>%
    ungroup() %>%
    mutate(
      speed_diff = release_speed - fb_velo,
      ax_diff    = ax_mir - fb_ax_mir,
      az_diff    = az - fb_az
    )
  
  # --- D. Final Selection ---
  # Ensure the final data has the exact columns the models expect
  final_df <- processed_df %>%
    transmute(
      Name, Date, Month, Year, Level, Skill, TaggedPitchType,
      pitch_category, # Keep the new category column!
      release_speed, spin_rate, extension,
      ax = ax_mir,
      az,
      x0 = x0_mir,
      z0,
      spin_axis,
      speed_diff,
      az_diff,
      ax_diff
    ) %>%
    drop_na()
  
  return(final_df)
}

models <- list(
  fastball = fastball_model,
  breaking = breaking_model,
  offspeed = offspeed_model
)

scaling_params <- bind_rows(
  fastball = fastball_params,
  breaking = breaking_params,
  offspeed = offspeed_params,
  .id = "pitch_category" # Adds a column to identify which params are which
)

# Define the Stuff+ scaling function
scale_to_plus <- function(x, mean_ref, sd_ref, sd_points = 10) {
  100 + sd_points * (x - mean_ref) / sd_ref
}

# --- B. Load and Process New Facility Data ---
trackman_data <- trackman_data %>%
  filter(!is.na(TaggedPitchType), !Skill %in% c("Youth", "Intermediate"))
  
cleaned_trackman <- prepare_facility_data(trackman_data)

# --- C. Generate Predictions for Each Pitch Category ---
# *** NEW: Split data, predict with the correct model, and recombine ***
predictions <- cleaned_trackman %>%
  group_by(pitch_category) %>%
  nest() %>%
  mutate(
    model = map(pitch_category, ~models[[.x]]),
    preds = map2(model, data, ~predict(.x, .y))
  ) %>%
  select(data, preds) %>%
  unnest(cols = c(data, preds))


# --- D. Calculate Stuff+ Using Category-Specific Parameters ---
final_results <- predictions %>%
  # Join the correct mean/sd for each pitch
  left_join(scaling_params, by = "pitch_category") %>%
  mutate(
    # Use the matched mean and sd to scale the prediction
    Stuff_Plus = scale_to_plus(x = -.pred, mean_ref = mean, sd_ref = sd)
  )

stuff_pitcher <- final_results %>%
  group_by(Name, Date, Level, Skill, pitch_category) %>%
  summarise(
    StuffPlus = mean(Stuff_Plus, na.rm = TRUE),
    NumPitches = n(),
    .groups = "drop"
  )


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
  # Look up the next level; returns NA if current_skill is not found
  return(skill_progression[current_skill])
}
  
  

# Define a temp directory for this athlete
athlete_dir <- file.path(tempdir(), athlete)
if (!dir.exists(athlete_dir)) {
  dir.create(athlete_dir, recursive = TRUE)
}
  
  

# Athlete Bio -------------------------------------------------------------


athlete_info <- client_data %>% 
  filter(Name == athlete)

# Generate ggplot
player_profile <- ggplot() +
  xlim(0, 1) +
  ylim(0, 1) +
  # Athlete's Name (Top line) - Use the "Black" weight
  annotate("text", x = 0.1, y = 0.9, label = athlete_info$Name,
           hjust = 0, size = 28, color = "black", family = "Good Times") +
  
  # High School and Graduating Class (Middle line) - Use the "SemiBold" weight
  annotate("text", x = 0.1, y = 0.5, label = paste0(athlete_info$`High School`, "  |  Class of ", athlete_info$`Graduating Class`),
           hjust = 0, size = 14, fontface = "bold", color = "black") +
  
  # Age, Position, and HT/WT (Bottom line) - Use the "Medium" weight
  annotate("text", x = 0.1, y = 0.2,
           label = paste0(athlete_info$`Position (Baseball/Softball)`, "  |  ",
                          athlete_info$Height, " ", athlete_info$Weight, "lbs  |  Age: ", athlete_info$Age),
           hjust = 0, size = 14, fontface = "bold", color = "black") +
  
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
      .cols = c(CMJ, ShotPut, TrunkRotation, D2Ext, D2Flex, thirty_yard),
      .fns = ~ first(na.omit(.x), default = NA_real_)
    ),
    .groups = "drop"
  ) %>% 
  mutate(Skill = if_else(Skill == "JV", "Varsity", Skill))

# 2. Determine the athlete's current and next competition levels
current_skill <- athlete_strength_summary$Skill
next_skill    <- get_next_skill(current_skill)

# --- CURRENT LEVEL PERCENTILE CALCULATION ---

# 3. Summarize facility data for the athlete's CURRENT level
facility_strength_current <- physicality_data %>%
  filter(
    Skill  == current_skill,
    Gender == athlete_strength_summary$Gender
  ) %>%
  group_by(Name, Gender) %>%
  summarise(
    CMJ          = if (all(is.na(CMJ))) NA_real_ else max(CMJ, na.rm = TRUE),
    ShotPut      = if (all(is.na(ShotPut))) NA_real_ else max(ShotPut, na.rm = TRUE),
    TrunkRotation= if (all(is.na(TrunkRotation))) NA_real_ else max(TrunkRotation, na.rm = TRUE),
    D2Ext         = if (all(is.na(D2Ext))) NA_real_ else max(D2Ext, na.rm = TRUE),
    D2Flex        = if (all(is.na(D2Flex))) NA_real_ else max(D2Flex, na.rm = TRUE),
    thirty_yard  = if (all(is.na(thirty_yard))) NA_real_ else min(thirty_yard, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  # Combine with the athlete's specific data
  filter(Name != athlete) %>%
  bind_rows(athlete_strength_summary)

# 4. Compute percentiles against the CURRENT level group
strength_percentiles_current <- facility_strength_current %>%
  mutate(
    CMJ_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((CMJ - 6298)   /  1116) * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((CMJ - 6742) /  868.1) * 100),
        TRUE                                       ~ round(percent_rank(CMJ) * 100)
      ),
    Trunk_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((TrunkRotation - 300)   /  61.5)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((TrunkRotation - 315) /  60) * 100),
        TRUE                                       ~ round(percent_rank(TrunkRotation) * 100)
      ),
    D2Ext_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((D2Ext - 60)   /  11.5)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((D2Ext - 65.5) /  11) * 100),
        TRUE                                       ~ round(percent_rank(D2Ext) * 100)
      ),
    D2Flex_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((D2Flex - 71)   /  11.75)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((D2Flex - 76.5) /  11.2) * 100),
        TRUE                                       ~ round(percent_rank(D2Flex) * 100)
      ),
    Thirty_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((3.90 - thirty_yard) / 0.20) * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((3.80 - thirty_yard) / 0.18) * 100),
        TRUE                                       ~ round(percent_rank(-thirty_yard) * 100)
      )
  ) %>%
  filter(Name == athlete) # Isolate the athlete's row


# --- NEXT LEVEL PERCENTILE CALCULATION ---

# 5. Summarize facility data for the athlete's NEXT level
facility_strength_next <- physicality_data %>%
  filter(
    Skill  == next_skill, # The only change is filtering for the next level
    Gender == athlete_strength_summary$Gender
  ) %>%
  group_by(Name) %>%
  summarise(
    Level        = first(Level),
    Skill        = first(Skill),
    Gender       = first(Gender),
    CMJ          = if (all(is.na(CMJ))) NA_real_ else max(CMJ, na.rm = TRUE),
    ShotPut      = if (all(is.na(ShotPut))) NA_real_ else max(ShotPut, na.rm = TRUE),
    TrunkRotation= if (all(is.na(TrunkRotation))) NA_real_ else max(TrunkRotation, na.rm = TRUE),
    D2Ext         = if (all(is.na(D2Ext))) NA_real_ else max(D2Ext, na.rm = TRUE),
    D2Flex        = if (all(is.na(D2Flex))) NA_real_ else max(D2Flex, na.rm = TRUE),
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
    D2Ext_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((D2Ext - 60)   /  11.5)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((D2Ext - 65.5) /  11) * 100),
        TRUE                                       ~ round(percent_rank(D2Ext) * 100)
      ),
    D2Flex_Percentile = case_when(
        Skill == "Collegiate" & Gender == "Male"   ~ round(pnorm((D2Flex - 71)   /  11.75)     * 100),
        Skill == "Professional" & Gender == "Male" ~ round(pnorm((D2Flex - 76.5) /  11.2) * 100),
        TRUE                                       ~ round(percent_rank(D2Flex) * 100)
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
    c("Speed", "Shoulder Accelerators", "Shoulder Decelerators", "Core Rotation", "Lower Body"),
    levels = c("Speed", "Shoulder Accelerators", "Shoulder Decelerators", "Core Rotation", "Lower Body")
  ),
  Current = c(
    strength_percentiles_current$Thirty_Percentile,
    strength_percentiles_current$D2Ext_Percentile,
    strength_percentiles_current$D2Flex_Percentile,
    strength_percentiles_current$Trunk_Percentile,
    strength_percentiles_current$CMJ_Percentile
  ),
  Next = c(
    strength_percentiles_next$Thirty_Percentile,
    strength_percentiles_next$D2Ext_Percentile,
    strength_percentiles_next$D2Flex_Percentile,
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
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = 100), color = "#3a3d46", linewidth = 7, lineend = "round", show.legend = FALSE) +
  
  # The main score bar, drawn from y=0 to y=Current
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = pmax(Current, 1), color = score_category), linewidth = 7, lineend = "round", show.legend = FALSE) +
  
  # Text label for the Metric name
  geom_text(
    aes(label = Metric, y = 0),
    hjust = 0,
    vjust = -2,
    color = "white",
    fontface = "bold",
    size = 8.5
  ) +
  
  # Text label for the 'Current' percentile value
  geom_text(
    data = . %>% filter(!is.na(Current)),
    aes(label = Current, y = 99),
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
report_img   <- image_composite(report_img, strength_img, offset = "+535+890")

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
report_img        <- image_composite(report_img, next_strength_img, offset = "+527+930")

# 4. Clean up the image object
rm(next_strength_img)
gc()



# Pitching Profile --------------------------------------------------------



# --- Stuff+ Percentile Calculation (Current & Next Level) ---

# 1. DATA PREP: Get athlete's data and facility data for both levels

# Get the athlete's most recent Stuff+ for each pitch category
athlete_stuff_summary <- stuff_pitcher %>%
  filter(Name == athlete) %>%
  arrange(desc(as.Date(Date))) %>%
  group_by(pitch_category) %>%
  summarise(
    Name = first(Name),
    Level = first(Level),
    Skill = first(Skill),
    StuffPlus = first(na.omit(StuffPlus), default = NA_real_),
    .groups = "drop"
  ) %>% 
  mutate(Skill = if_else(Skill == "JV", "Varsity", Skill))

# 2. Determine the athlete's current and next competition levels
current_skill <- athlete_stuff_summary$Skill[1]
next_skill    <- get_next_skill(current_skill)

# Get the average Stuff+ for all other athletes at the CURRENT level
facility_stuff_current <- stuff_pitcher %>%
  filter(Skill == current_skill, Name != athlete) %>%
  group_by(Name, pitch_category) %>%
  summarise(StuffPlus = mean(StuffPlus, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(StuffPlus))

# Get the average Stuff+ for all other athletes at the NEXT level
facility_stuff_next <- stuff_pitcher %>%
  filter(Skill == next_skill, Name != athlete) %>%
  group_by(Name, pitch_category) %>%
  summarise(StuffPlus = mean(StuffPlus, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(StuffPlus))

# 3. CALCULATE PERCENTILES

# Calculate current level percentiles
if (current_skill == "Professional") {
  
  # --- Professional Level Calculation ---
  stuff_percentiles_current <- athlete_stuff_summary %>%
    mutate(
      Current = round(pnorm((StuffPlus - 100) / 3.5) * 100)
    ) %>%
    select(pitch_category, Current)
  
} else if (current_skill == "Collegiate") {
  
  # --- Collegiate Level Calculation ---
  stuff_percentiles_current <- athlete_stuff_summary %>%
    mutate(
      Current = round(pnorm((StuffPlus - 95) / 5) * 100)
    ) %>%
    select(pitch_category, Current)
  
} else {
  
  # --- All Other Levels Calculation ---
  stuff_percentiles_current <- athlete_stuff_summary %>%
    mutate(
      Current = round(pnorm((StuffPlus - 85) / 7) * 100)
    ) %>%
    select(pitch_category, Current)
}

# Calculate next level percentiles
# Check if the next level is 'Professional'
if (next_skill == "Professional") {
  
  # --- Professional Level Calculation ---
  stuff_percentiles_next <- athlete_stuff_summary %>%
    mutate(
      Next = round(pnorm((StuffPlus - 100) / 3.5) * 100)
    ) %>%
    select(pitch_category, Next)
  
} else if (next_skill == "Collegiate") {
  
  # --- Collegiate Level Calculation ---
  stuff_percentiles_next <- athlete_stuff_summary %>%
    mutate(
      Next = round(pnorm((StuffPlus - 95) / 5) * 100)
    ) %>%
    select(pitch_category, Next)
  
} else {
  
  # --- All Other Levels Calculation ---
  stuff_percentiles_next <- athlete_stuff_summary %>%
    mutate(
      Next = round(pnorm((StuffPlus - 85) / 7) * 100)
    ) %>%
    select(pitch_category, Next)
}

# 4. COMBINE & FORMAT STUFF+ RESULTS

# First, combine the calculated percentiles for the available pitches
calculated_stuff_percentiles <- stuff_percentiles_current %>%
  full_join(stuff_percentiles_next, by = "pitch_category") %>%
  transmute(
    Metric = paste0(str_to_title(pitch_category), "+"),
    Current,
    Next
  )

# Define a template with all required metrics
required_metrics <- tibble(
  Metric = c("Fastball+", "Breaking+", "Offspeed+")
)

# Join the calculated results to the template to ensure all metrics are present
athlete_stuff_percentiles <- required_metrics %>%
  left_join(calculated_stuff_percentiles, by = "Metric")


# --- Command Percentile Calculation (Current & Next Level) ---

# 5. DATA PREP: Get athlete and facility command data

# Get the athlete's overall strike percentage
athlete_trackman_summary <- trackman_data %>%
  filter(Name == athlete) %>%
  mutate(ZoneCheck = case_when(between(PlateLocHeight, 1.4833, 3.8167) & between(PlateLocSide, -0.9167, 0.9167) ~ TRUE, TRUE ~ FALSE)) %>%
  group_by(Name, Date, Level, Skill) %>%
  summarise(
    Max_RelSpeed = max(RelSpeed, na.rm = TRUE),
    StrikePct   = mean(ZoneCheck, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  arrange(desc(as.Date(Date))) %>%
  summarise(
    Name   = first(Name),
    Level  = first(Level),
    Skill  = first(Skill),
    Max_RelSpeed = first(na.omit(Max_RelSpeed), default = NA_real_),
    StrikePct   = first(na.omit(StrikePct), default = NA_real_),
    .groups = "drop"
  ) %>% 
  mutate(Skill = if_else(Skill == "JV", "Varsity", Skill))


# Get average strike percentage for all other athletes at the CURRENT level
facility_trackman_current <- trackman_data %>%
  filter(Skill == current_skill, Name != athlete) %>%
  mutate(ZoneCheck = case_when(between(PlateLocHeight, 1.4833, 3.8167) & between(PlateLocSide, -0.9167, 0.9167) ~ TRUE, TRUE ~ FALSE)) %>%
  group_by(Name) %>%
  summarise(
    Max_RelSpeed = max(RelSpeed, na.rm = TRUE),
    StrikePct   = mean(ZoneCheck, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  filter(!is.na(Max_RelSpeed), !is.na(StrikePct))


# Get average strike percentage for all other athletes at the NEXT level
facility_trackman_next <- trackman_data %>%
  filter(Skill == next_skill, Name != athlete) %>%
  mutate(ZoneCheck = case_when(between(PlateLocHeight, 1.4833, 3.8167) & between(PlateLocSide, -0.9167, 0.9167) ~ TRUE, TRUE ~ FALSE)) %>%
  group_by(Name) %>%
  summarise(
    Max_RelSpeed = max(RelSpeed, na.rm = TRUE),
    StrikePct   = mean(ZoneCheck, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  filter(!is.na(Max_RelSpeed), !is.na(StrikePct))


# 6. CALCULATE & FORMAT TRACKMAN PERCENTILES

# Calculate current level percentiles for both metrics
athlete_percentiles_current <- facility_trackman_current %>%
  bind_rows(athlete_trackman_summary) %>%
  mutate(
    Velo_Current = case_when(
        Level == "Collegiate"   ~ round(pnorm((Max_RelSpeed   - 90.0)      /  4.15)     * 100),
        Level == "Professional" ~ round(pnorm((Max_RelSpeed   - 96.4)  /  2.72) * 100),
        TRUE                    ~ round(percent_rank(Max_RelSpeed)        * 100)
      ),
    Command_Current = case_when(
        Level == "Collegiate"   ~ round(pnorm((StrikePct   - 0.5)      /  0.054)     * 100),
        Level == "Professional" ~ round(pnorm((StrikePct   - 0.55)     /  0.05)      * 100),
        TRUE                    ~ round(percent_rank(StrikePct)        * 100)
      )
  ) %>%
  filter(Name == athlete) %>%
  select(Velo_Current, Command_Current)

# Calculate next level percentiles for both metrics
athlete_percentiles_next <- facility_trackman_next %>%
  bind_rows(athlete_trackman_summary) %>%
  mutate(
    Velo_Next = case_when(
        Level == "Collegiate"   ~ round(pnorm((Max_RelSpeed   - 90.0)      /  4.15)     * 100),
        Level == "Professional" ~ round(pnorm((Max_RelSpeed   - 96.4)  /  2.72) * 100),
        TRUE                    ~ round(percent_rank(Max_RelSpeed)        * 100)
      ),
    Command_Next = case_when(
        Level == "Collegiate"   ~ round(pnorm((StrikePct   - 0.5)      /  0.054)     * 100),
        Level == "Professional" ~ round(pnorm((StrikePct   - 0.55)     /  0.05)      * 100),
        TRUE                    ~ round(percent_rank(StrikePct)        * 100)
      )
  ) %>%
  filter(Name == athlete) %>%
  select(Velo_Next, Command_Next)

# Create the final tibble for both Velo and Command, including raw values
athlete_trackman_percentiles <- tibble(
  Metric = c("Fastball Velo", "Command"),
  Value = c(
    round(athlete_trackman_summary$Max_RelSpeed, 1),
    round(athlete_trackman_summary$StrikePct * 100, 0)
  ),
  Current = c(athlete_percentiles_current$Velo_Current, athlete_percentiles_current$Command_Current),
  Next = c(athlete_percentiles_next$Velo_Next, athlete_percentiles_next$Command_Next)
)

# --- FINAL: Combine, Add Categories, and Arrange ---

athlete_pitching_percentiles <- athlete_stuff_percentiles %>%
  bind_rows(athlete_trackman_percentiles) %>%
  mutate(
    # MODIFICATION: Update factor levels to use "Fastball Velo"
    Metric = factor(Metric, levels = c("Command", "Offspeed+", "Breaking+", "Fastball+", "Fastball Velo")),
    # Add score categories based on the current level's percentile
    score_category = case_when(
      Current >= 80  ~ "Good",
      Current >= 60  ~ "Average",
      TRUE           ~ "Needs Improvement"
    ),
    score_category = factor(score_category, levels = c("Good", "Average", "Needs Improvement"))
  ) %>%
  arrange(Metric)


# --- CURRENT LEVEL PLOT CODE ---
performance_plot <- ggplot(athlete_pitching_percentiles, aes(x = Metric)) +
  # The grey background bar, drawn from y=0 to y=100
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = 100), color = "#3a3d46", linewidth = 7, lineend = "round", show.legend = FALSE) +
  
  # The main score bar, drawn from y=0 to y=Current
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = pmax(Current, 1), color = score_category), linewidth = 7, lineend = "round", show.legend = FALSE) +
  
  # Text label for the Metric name
  geom_text(
    aes(label = Metric, y = 0),
    hjust = 0,
    vjust = -2,
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
  
  # MODIFICATION: Add a label for the raw Fastball Velo value
  geom_label(
    data = . %>% filter(Metric == "Fastball Velo" & !is.na(Value)),
    aes(y = Current, label = paste0(Value, " mph")),
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
next_performance_plot <- ggplot(athlete_pitching_percentiles, aes(x = Metric)) +
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
report_img      <- image_composite(report_img, performance_img, offset = "+1875+890")

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
report_img           <- image_composite(report_img, next_performance_img, offset = "+1867+930")

# 4. Clean up the image object
rm(next_performance_img)
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
report_img         <- image_composite(report_img, strength_score_img, offset = "+765+1915")

rm(strength_score_img)
gc()



# Performance Score -------------------------------------------------------



avg_performance_score <- round(mean(athlete_pitching_percentiles$Current, na.rm = TRUE))

performance_gauge_data <- tibble(
  category = c("Score", "Remaining"),
  value = c(avg_performance_score, 100 - avg_performance_score)
)

# Determine the correct color based on the score's value
if (avg_performance_score >= 80) {
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
report_img            <- image_composite(report_img, performance_score_img, offset = "+2110+1915")

rm(performance_score_img)
gc()
  
  
# Write out the final report
final_report_path <- file.path(athlete_dir, "PitchingProspectProfile.png")
image_write(report_img,
            path    = final_report_path,
            format  = "png")

# Return the path for downloadHandler()
final_report_path
}