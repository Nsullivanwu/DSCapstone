library(tidyverse)
library(openxlsx)

# Open full CSV
file_csv <- "data/raw/GM1 - PPvsUCSD copy.csv"

# Read CSV and create standardized event table
tagged_actions <- read_csv(file_csv) |>
  transmute(
    action_id = row_number(),
    start_time = `Start time`,
    end_time = `Start time` + Duration,
    code = Row,
    duration_time_sec = Duration
  )
View(tagged_actions)

# Keep PP Shots
pp_shots <- tagged_actions |>
  filter(code == "PP Shots") |>
  arrange(start_time) |>
  mutate(
    shot_id = row_number(),
    start_time_min = floor(start_time / 60),
    start_time_sec = start_time %% 60
  )
# sort by time
View(pp_shots)
nrow(pp_shots)
# 27 shots

# mark GOALS from pp shots 
pp_goals <- tagged_actions |>
  filter(code == "PP Goals")
View(pp_goals)
nrow(pp_goals)

#check if goals overlap with shots
pp_shots <- pp_shots |>
  rowwise() |>
  mutate(
    goal = as.integer(
      any(
        pp_goals$start_time <= end_time &
        pp_goals$end_time >= start_time
      )
    )
  ) |>
  ungroup()
View(pp_shots)
count(pp_shots, goal)
# 13 goals, 14 non-goals

# mark 6ON5 from shots/ identify advantages
pp_advantage <- tagged_actions |>
  filter(code %in% c("PP 6/5", "PP 5/6", "PP 7/6"))

# identify penalty situations
pp_penalties <- tagged_actions |>
  filter(code == "PP Penalty")

# add to the table
pp_shots <- pp_shots |>
  rowwise() |>
  mutate(
    advantage = case_when(
      any(
        start_time >= pp_penalties$end_time &
          start_time - pp_penalties$end_time <= 15
      ) ~ "penalty",
      any(
        pp_advantage$code == "PP 7/6" &
        pp_advantage$start_time <= end_time &
        pp_advantage$end_time >= start_time
      ) ~ "7v6",
      any(
        pp_advantage$code == "PP 6/5" &
        pp_advantage$start_time <= end_time &
        pp_advantage$end_time >= start_time
      ) ~ "6v5",
      any(
        pp_advantage$code == "PP 5/6" &
        pp_advantage$start_time <= end_time &
        pp_advantage$end_time >= start_time
      ) ~ "5v6",
      TRUE ~ "even"
    )
  ) |>
  ungroup()
View(pp_shots)
count(pp_shots, advantage)
# 12 shots on 6on5, 1 shot on 7on6, 12 shots even, 2 Penalties 


# MANUAL SHOT CODING TABLE
coding_sheet <- pp_shots |>
  mutate(
    # Game Info
    game_id = "GM1",
    opponent = "UCSD",
    
    # Manual variables
    shooter = NA_character_,
    shot_type = NA_character_,
    target_loc = NA_character_,
    shot_clock = NA_real_, # hard to see 
    
    # use goals from Sportscode
    result = if_else(
      goal == 1,
      "Goal",
      NA_character_
    ),
     # Rules to help manual process easier 
    defense = if_else(
      advantage == "penalty",
      "N/A",
      NA_character_
    ),
    post_shot = if_else(
      advantage %in% c("6v5", "7v6"),
      NA_character_,   # manually code Yes / No / Unsure
      "N/A"
    ),
    center_shot = if_else(
      advantage %in% c("6v5", "penalty"),
      "N/A",
      NA_character_
    ),
    shot_zone = if_else(
      advantage == "penalty",
      "N/A",
      NA_character_
    ),
    center_move = if_else(
      advantage %in% c("6v5", "penalty"),
      "N/A",
      NA_character_
    ),
    notes = NA_character_
  ) |>
  
  select(
    game_id,
    opponent,
    shot_id,
    start_time,
    start_time_min,
    start_time_sec,
    advantage,
    goal,
    
    # Manual
    shooter,
    center_shot,
    shot_zone,
    shot_type,
    center_move,
    post_shot,
    target_loc,
    defense,
    shot_clock,
    result,
    notes
  )
View(coding_sheet)
nrow(coding_sheet)

# EXPORT SHEET TO EXCEL (with drop downs)
# Create workbook
wb <- createWorkbook()

# Add main coding sheet
addWorksheet(wb, "Shot Coding")

# Add hidden sheet that stores drop down options
addWorksheet(wb, "Lists")

# Shooter numbers found in Sportscode
roster_numbers <- tagged_actions |>
  filter(str_detect(code, "^#\\d+$")) |>
  distinct(code) |>
  mutate(
    number = str_remove(code, "#"),
    number_numeric = as.numeric(number)
  ) |>
  arrange(number_numeric) |>
  pull(number)
roster_numbers <- c(roster_numbers, "Unknown")

# manual options for drop down
center_options <- c(
  "Yes",
  "No",
  "Unsure",
  "N/A"
)
zone_options <- c(
  "1", "2", "3", "4",
  "5", "6", "7", "8",
  "Unsure", "N/A"
  #should I do 1-6 COME BACK
)
shot_type_options <- c(
  "Direct",
  "Skip",
  "Lob",
  "Other",
  "Unsure"
)
center_move_options <- c(
  "Sweep",
  "Backhand",
  "Layout",
  "Step-Out",
  "Other",
  "Unsure",
  "N/A"
)
post_shot_options <- c(
  "Yes",
  "No",
  "Unsure",
  "N/A"
)
target_loc_options <- c(
  "GHL",
  "GCH",
  "GHR",
  "GLL",
  "GCL",
  "GLR",
  "Unsure"
)
defense_options <- c(
  "Open", # no meaningful defensive pressure
  "Pressured", # defense that affects the shot but doesn't touch the ball
  "Deflected", # defender touches or deflects the ball
  "Unsure", # Can't tell
  "N/A" # doesn't apply 
)
result_options <- c(
  "Goal",
  "Goalie Save",
  "Field Block",
  "Miss",
  "Unsure"
)
# put coding into workbook
writeData(
  wb,
  sheet = "Shot Coding",
  x = coding_sheet
)

# put allowed values on the 2nd sheet
writeData(wb, "Lists", roster_numbers, startCol = 1, colNames = FALSE)
writeData(wb, "Lists", center_options, startCol = 2, colNames = FALSE)
writeData(wb, "Lists", zone_options, startCol = 3, colNames = FALSE)
writeData(wb, "Lists", shot_type_options, startCol = 4, colNames = FALSE)
writeData(wb, "Lists", post_shot_options, startCol = 5, colNames = FALSE)
writeData(wb, "Lists", center_move_options, startCol = 6, colNames = FALSE)
writeData(wb, "Lists", target_loc_options, startCol = 7, colNames = FALSE)
writeData(wb, "Lists", defense_options, startCol = 8, colNames = FALSE)
writeData(wb, "Lists", result_options, startCol = 9, colNames = FALSE)

# Drop downs for some of the manual variables 
last_row <- nrow(coding_sheet) + 1

# shooter drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 9,
  rows = 2:last_row,
  type = "list",
  value = paste0("'Lists'!$A$1:$A$", length(roster_numbers))
)

# center shot drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 10,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$B$1:$B$4"
)

# shot zone drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 11,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$C$1:$C$10"
)

# shot type drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 12,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$D$1:$D$5"
)

# center move drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 13,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$F$1:$F$7"
)

# post shot drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 14,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$E$1:$E$4"
)

# target location drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 15,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$G$1:$G$7"
)

# defense drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 16,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$H$1:$H$5"
)

# result drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 18,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$I$1:$I$5"
)

# freeze column titles  
freezePane(
  wb,
  "Shot Coding",
  firstRow = TRUE
)

#make col. readable 
setColWidths(
  wb,
  "Shot Coding",
  cols = 1:ncol(coding_sheet),
  widths = "auto"
)

#hide the list sheet 
sheetVisibility(wb) <- c("visible", "hidden")

#SAVE 
saveWorkbook(
  wb,
  "data/processed/GM1_PPvsUCSD_template.xlsx",
  overwrite = TRUE
)




# READ OLD MANUAL CODING
manual_v1 <- read.xlsx(
  "data/processed/GM1_PPvsUCSD_manual_V1.xlsx",
  sheet = "Shot Coding"
)

# make fields consistent types
manual_v1 <- manual_v1 |>
  mutate(
    shooter = as.character(shooter),
    shot_zone = as.character(shot_zone)
  )

View(manual_v1)
nrow(manual_v1)

