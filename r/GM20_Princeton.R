library(tidyverse)
library(openxlsx)

# GAME INFO
file_csv <- "data/raw/GM20 - PP vsPrinceton.csv"
game_id <- "GM20"
opponent <- "Princeton"
output_file <- "data/processed/GM20_PPvsPrinceton_template.xlsx"

#-----------
# READ file
#-----------
# Create standardized event table
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
  filter(code == "D Shots") |>
  arrange(start_time) |>
  mutate(
    shot_id = row_number(),
    start_time_min = floor(start_time / 60),
    start_time_sec = start_time %% 60
  )
# sort by time
View(pp_shots)
nrow(pp_shots)
# 29 PP shots 

# mark GOALS from pp shots 
pp_goals <- tagged_actions |>
  filter(code == "D Goals")
View(pp_goals)
nrow(pp_goals)
# 1 PP goal 


#check if goals overlap with shots
pp_shots <- pp_shots |>
  rowwise() |>
  mutate(
    goal = as.integer(
      any(
        abs(pp_goals$start_time - start_time) <= 0.05
      )
    )
  ) |>
  ungroup()

View(pp_shots)
count(pp_shots, goal)
# 1 goal, 28 non-goals 


#-------------------
# POSESSION ADVANTAGE
#--------------------
pp_advantage <- tagged_actions |>
  filter(code %in% c("D 6/5", "D 5/6", "D 7/6"))
pp_penalties <- tagged_actions |>
  filter(code == "D PENALTY")

# add to the table
pp_shots <- pp_shots |>
  rowwise() |>
  mutate(
    advantage = case_when(
      any( # Penalty shot 
        start_time >= pp_penalties$end_time &
          start_time - pp_penalties$end_time <= 15
      ) ~ "penalty",
      any( # 7 ON 6
        pp_advantage$code == "D 7/6" &
          pp_advantage$start_time <= end_time &
          pp_advantage$end_time >= start_time
      ) ~ "7v6",
      any( # 6 ON 5
        pp_advantage$code == "D 6/5" &
          pp_advantage$start_time <= end_time &
          pp_advantage$end_time >= start_time
      ) ~ "6v5",
      any( # 5 ON 6
        pp_advantage$code == "D 5/6" &
          pp_advantage$start_time <= end_time &
          pp_advantage$end_time >= start_time
      ) ~ "5v6",
      TRUE ~ "even"
    )
  ) |>
  ungroup()
count(pp_shots, advantage)
# 11 shots on 6on5, 1 shots 7on6, 15 shots even, 2 penalties

#---------------------
# MANUAL CODING TABLE
#---------------------
coding_sheet <- pp_shots |>
  mutate(
    game_id = game_id,
    opponent = opponent,
    
    shooter = NA_character_,
    
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
    
    shot_type = NA_character_,
    
    center_move = if_else(
      advantage %in% c("6v5", "penalty"),
      "N/A",
      NA_character_
    ),
    
    post_shot = if_else(
      advantage %in% c("6v5", "7v6"),
      NA_character_,   # manually code Yes / No / Unsure
      "N/A"
    ),
    
    target_loc = NA_character_,
    
    defense = if_else(
      advantage == "penalty",
      "N/A",
      NA_character_
    ),
    
    shot_clock = NA_real_, # hard to see 
    
    result = if_else(
      goal == 1,
      "Goal",
      NA_character_
    ),
    notes = NA_character_
  ) |>
  
  select(
    # Automatic from Sportscode
    game_id,
    opponent,
    shot_id,
    start_time,
    start_time_min,
    start_time_sec,
    advantage,
    goal,
    # Manually added
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

#-----------------------
# EXPORT SHEET TO EXCEL
#-----------------------
wb <- createWorkbook()

# Add main coding sheet
addWorksheet(wb, "Shot Coding")

# Add sheet that stores drop down options
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

#-------------------
# DROP DOWN OPTIONS
#-------------------
center_options <- c(
  "Yes", "No", "Unsure", "N/A"
)
zone_options <- c(
  "1", "2", "3", "4", "5", "6", "7", "8", "Unsure", "N/A"
)
shot_type_options <- c(
  "Direct", "Skip", "Lob", "Other", "Unsure"
)
center_move_options <- c(
  "Sweep", "Backhand", "Layout", "Step-Out", "Other", "Unsure", "N/A"
)
post_shot_options <- c(
  "Yes", "No", "Unsure", "N/A"
)
target_loc_options <- c(
  "GHL", "GCH", "GHR","GLL","GCL", "GLR", "Unsure"
)
defense_options <- c(
  "Open", "Pressured", "Unsure", "N/A"
)
result_options <- c(
  "Goal", "Goalie Save", "Field Block", "Miss", "Unsure"
)
# put into workbook
writeData(
  wb,
  sheet = "Shot Coding",
  x = coding_sheet
)

# 2nd sheet - value dictionary with titles and allowed values
writeData(wb, "Lists", "Shooter", startCol = 1, startRow = 1, colNames = FALSE)
writeData(wb, "Lists", "Center Shot", startCol = 2, startRow = 1, colNames = FALSE)
writeData(wb, "Lists", "Shot Zone", startCol = 3, startRow = 1, colNames = FALSE)
writeData(wb, "Lists", "Shot Type", startCol = 4, startRow = 1, colNames = FALSE)
writeData(wb, "Lists", "Post Shot", startCol = 5, startRow = 1, colNames = FALSE)
writeData(wb, "Lists", "Center Move", startCol = 6, startRow = 1, colNames = FALSE)
writeData(wb, "Lists", "Target Location", startCol = 7, startRow = 1, colNames = FALSE)
writeData(wb, "Lists", "Defense", startCol = 8, startRow = 1, colNames = FALSE)
writeData(wb, "Lists", "Result", startCol = 9, startRow = 1, colNames = FALSE)

# define allowed values
writeData(wb, "Lists", roster_numbers, startCol = 1, startRow = 2, colNames = FALSE)
writeData(wb, "Lists", center_options, startCol = 2, startRow = 2, colNames = FALSE)
writeData(wb, "Lists", zone_options, startCol = 3, startRow = 2, colNames = FALSE)
writeData(wb, "Lists", shot_type_options, startCol = 4, startRow = 2, colNames = FALSE)
writeData(wb, "Lists", post_shot_options, startCol = 5, startRow = 2, colNames = FALSE)
writeData(wb, "Lists", center_move_options, startCol = 6, startRow = 2, colNames = FALSE)
writeData(wb, "Lists", target_loc_options, startCol = 7, startRow = 2, colNames = FALSE)
writeData(wb, "Lists", defense_options, startCol = 8, startRow = 2, colNames = FALSE)
writeData(wb, "Lists", result_options, startCol = 9, startRow = 2, colNames = FALSE)
# Drop downs for some of the manual variables 
last_row <- nrow(coding_sheet) + 1

# shooter drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 9,
  rows = 2:last_row,
  type = "list",
  value = paste0("'Lists'!$A$2:$A$", length(roster_numbers) +1)
)

# center shot drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 10,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$B$2:$B$5"
)

# shot zone drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 11,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$C$2:$C$11"
)

# shot type drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 12,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$D$2:$D$6"
)

# center move drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 13,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$F$2:$F$8"
)

# post shot drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 14,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$E$2:$E$5"
)

# target location drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 15,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$G$2:$G$8"
)

# defense drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 16,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$H$2:$H$5"
)

# result drop down
dataValidation(
  wb,
  "Shot Coding",
  cols = 18,
  rows = 2:last_row,
  type = "list",
  value = "'Lists'!$I$2:$I$6"
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

sheetVisibility(wb) <- c("visible", "visible")

#SAVE 
saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)