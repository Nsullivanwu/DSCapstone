library(tidyverse)
library(openxlsx)

process_game <- function(file_csv, game_id, opponent) {
  
  tagged_actions <- read_csv(
    file_csv,
    show_col_types = FALSE
  ) |>
    transmute(
      action_id = row_number(),
      start_time = `Start time`,
      end_time = `Start time` + Duration,
      code = Row,
      duration_time_sec = Duration
    )
  
  pp_shots <- tagged_actions |>
    filter(code == "PP Shots") |>
    arrange(start_time) |>
    mutate(
      shot_id = row_number(),
      start_time_min = floor(start_time / 60),
      start_time_sec = start_time %% 60
    )
  
  pp_goals <- tagged_actions |>
    filter(code == "PP Goals")
  
  pp_advantage <- tagged_actions |>
    filter(code %in% c(
      "PP 6/5",
      "PP 5/6",
      "PP 7/6"
    ))
  
  pp_penalties <- tagged_actions |>
    filter(code == "PP Penalty")
  
  pp_shots <- pp_shots |>
    rowwise() |>
    mutate(
      goal = as.integer(
        any(
          pp_goals$start_time <= end_time &
            pp_goals$end_time >= start_time
        )
      ),
      
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
    ungroup() |>
    mutate(
      game_id = game_id,
      opponent = opponent
    )
  
  list(
    shots = pp_shots,
    tagged_actions = tagged_actions
  )
}