library(tidyverse) # obviously
library(gtools) # combinations
library(gt) # tables
library(nbaplotR) # logos in tables
library(shiny) # shiny application
library(rsconnect) # deploy to shinyappsio
library(rvest) # scraping


# pull odds table from tankathon
#url <- "https://www.tankathon.com/pick_odds"
#page_html <- url %>%
#  read_html()

# clean table, get it into long format
#lottery_odds <- page_html %>%
#  html_node("table") %>%
#  html_table(fill = TRUE) %>%
#  mutate(across(c(`11`:`14`), ~na_if(.,"")),
#         across(c(`11`:`14`), ~na_if(.,">0.0")),
#         across(c(`11`:`14`), ~as.double(.))) %>%
#  select(-Avg) %>%
#  mutate(across(c(`1`: `14`), ~replace_na(., 0.0))) %>%
#  pivot_longer(`1`:`14`, names_to = "pick", values_to = "odds") %>%
#  mutate(pick = as.double(pick)) %>%
#  mutate(Team = str_replace(Team, "^\\d+\\s*", ""))

#write.csv(lottery_odds, "lottery_odds.csv")
#lottery_odds <- read_csv("/Users/shanefaberman/lottery_odds_2026/lottery_odds.csv")
lottery_odds <- read_csv("lottery_odds.csv")

# vector of team names for standings
standings <- lottery_odds %>% 
  filter(pick == 1) %>% 
  pull(Team)

# get pick combo numbers 
nums <- lottery_odds %>% 
  filter(pick == 1) %>% 
  pull(odds)
nums <- nums*10

# 14 C 4 (how the lottery actually works)
all_combinations = combinations(14,4)

df <- as.data.frame(all_combinations)

# assign combinations to teams
teams = rep(standings[1], nums[1])
teams = c(teams, rep(standings[2], nums[2]), rep(standings[3], nums[3]), rep(standings[4], nums[4]), rep(standings[5], nums[5]),
          rep(standings[6], nums[6]), rep(standings[7], nums[7]), rep(standings[8], nums[8]), rep(standings[9], nums[9]), 
          rep(standings[10], nums[10]), rep(standings[11], nums[11]), rep(standings[12], nums[12]), 
          rep(standings[13], nums[13]), rep(standings[14], nums[14]), NA) # 1001 combinations so we need one 1 NA
id <- c(1:1000)
df <- df %>% 
  mutate(team = sample(teams, size = n(), replace = FALSE)) %>% # need to randomly sample these
  relocate(team) %>% 
  filter(!is.na(team)) %>% 
  cbind(id)



simulate_lottery <- function(df, standings,
                             force_team1 = "No Selection", force_team2 = "No Selection",
                             force_team3 = "No Selection", force_team4 = "No Selection",
                             force_team5 = "No Selection", force_team6 = "No Selection",
                             force_team7 = "No Selection", force_team8 = "No Selection",
                             force_team9 = "No Selection", force_team10 = "No Selection",
                             force_team11 = "No Selection", force_team12= "No Selection",
                             force_team13 = "No Selection", force_team14 = "No Selection") {
  VALID <- FALSE
  while(!VALID) {
    lottery_results <- rep(NA, 14)
    forced_teams <- c(force_team1, force_team2, force_team3, force_team4,
                      force_team5, force_team6, force_team7, force_team8,
                      force_team9, force_team10, force_team11, force_team12,
                      force_team13, force_team14)
    
    forced_teams_noselection <- forced_teams[forced_teams != "No Selection"]
    if (any(duplicated(forced_teams_noselection))) {
      stop("You selected the same team more than once.")
    }
    
    # put all forced teams 1-4 into correct positions 
    for (pick_pos in 1:4) {
      forced_team <- forced_teams[pick_pos]
      if (forced_team != "No Selection") {
        lottery_results[pick_pos] <- forced_team
      }
    }
    
    # figure out pool of teams who the remaining lottery balls are drawn from
    worst_cases <- standings[1:10] # best odds can drop to 5th, second best odds can drop to 6th, etc.
    outside_top_four <- forced_teams[5:14]
    for(i in 10:1) { # count backwards because if a team at the end of the list has its worst case, all teams in front do too
      if(worst_cases[i] == outside_top_four[i]) {
        outside_top_four[1:i] = worst_cases[1:i]
        break # if true for i, it is true for i-1
      }
    }
    already_picked <- lottery_results[!is.na(lottery_results)]
    forced_outside_top_four <- outside_top_four[outside_top_four != "No Selection"]
    
    # simulate lottery drawing
    for(p in 1:4) {
      if(is.na(lottery_results[p])) {
        # Filter combinations: team not already picked AND not forced into 5-14
        eligible_combos <- df %>% 
          filter(!team %in% already_picked, !team %in% forced_outside_top_four)
        
        if(nrow(eligible_combos) == 0) stop("Impossible constraints!")
        
        selected_team <- sample(eligible_combos$team, 1)
        lottery_results[p] <- selected_team
        already_picked <- c(already_picked, selected_team)
      }
    }
    
    # fill in 5-14 by reverse standings order
    remaining_teams <- standings[!(standings %in% lottery_results[1:4])]
    lottery_results[5:14] <- remaining_teams
    
    Matches <- TRUE
    for(p in 5:14) {
      if(forced_teams[p] != "No Selection") {
        if(lottery_results[p] != forced_teams[p]) {
          # This means the user forced a scenario that is mathematically impossible
          # try again
          Matches <- FALSE
          
        }
      }
    }
    if (Matches) {
      VALID <- TRUE
    }
    
    
  }
  
  
  return(lottery_results)
}



ui <- fluidPage(
  
  
  titlePanel("2026 NBA Draft Lottery Simulator"),
  
  sidebarLayout(
    sidebarPanel(width = 2,
                 actionButton("simulate_btn", "Simulate Lottery", style = "color: white; background-color: #0072B2; border-color: #005b96;"),
                 h4("Optionally Set Forced Picks:"),
                 selectInput("1", 
                             "1: ", 
                             choices = c("No Selection", standings),
                             selected = "No Selection"),
                 selectInput("2", 
                             "2: ", 
                             choices = c("No Selection", standings),
                             selected = "No Selection"),
                 selectInput("3", 
                             "3: ", 
                             choices = c("No Selection", standings),
                             selected = "No Selection"),
                 selectInput("4", 
                             "4: ", 
                             choices = c("No Selection", standings),
                             selected = "No Selection"),
                 selectInput("5", 
                             "5: ", 
                             choices = c("No Selection", lottery_odds %>% filter(pick == 5, odds > 0) %>% arrange(-odds) %>% pull(Team)),
                             selected = "No Selection"),
                 selectInput("6", 
                             "6: ", 
                             choices = c("No Selection", lottery_odds %>% filter(pick == 6, odds > 0) %>% arrange(-odds) %>% pull(Team)),
                             selected = "No Selection"),
                 selectInput("7", 
                             "7: ", 
                             choices = c("No Selection", lottery_odds %>% filter(pick == 7, odds > 0) %>% arrange(-odds) %>% pull(Team)),
                             selected = "No Selection"),
                 selectInput("8", 
                             "8: ", 
                             choices = c("No Selection", lottery_odds %>% filter(pick == 8, odds > 0) %>% arrange(-odds) %>% pull(Team)),
                             selected = "No Selection"),
                 selectInput("9", 
                             "9: ", 
                             choices = c("No Selection", lottery_odds %>% filter(pick == 9, odds > 0) %>% arrange(-odds) %>% pull(Team)),
                             selected = "No Selection"),
                 selectInput("10", 
                             "10: ", 
                             choices = c("No Selection", lottery_odds %>% filter(pick == 10, odds > 0) %>% arrange(-odds) %>% pull(Team)),
                             selected = "No Selection"),
                 selectInput("11", 
                             "11: ", 
                             choices = c("No Selection", lottery_odds %>% filter(pick == 11, odds > 0) %>% arrange(-odds) %>% pull(Team)),
                             selected = "No Selection"),
                 selectInput("12", 
                             "12: ", 
                             choices = c("No Selection", lottery_odds %>% filter(pick == 12, odds > 0) %>% arrange(-odds) %>% pull(Team)),
                             selected = "No Selection"),
                 selectInput("13", 
                             "13: ", 
                             choices = c("No Selection", lottery_odds %>% filter(pick == 13, odds > 0) %>% arrange(-odds) %>% pull(Team)),
                             selected = "No Selection"),
                 selectInput("14", 
                             "14: ", 
                             choices = c("No Selection", lottery_odds %>% filter(pick == 14, odds > 0) %>% arrange(-odds) %>% pull(Team)),
                             selected = "No Selection")
    ),
    
    
    mainPanel(width = 10,
      h3("Simulated Odds", style = "text-align: center;"),
      tableOutput("tbl"),
      h6("Note: The lottery is simulated 10,000 times. Since these are simulated odds, they aren't the exact probabilities. The more unlikely the scenario, the longer the simulation will take.", style = "text-align: center;"),
      h3("Original Odds", style = "text-align: center;"),
      tableOutput("tbl2"),
      h4("Relevant picks owed to other teams"),
      HTML("<p style='color: #FDB927; font-weight: bold;'>- ATL owns more favorable of NO's pick and MIL's pick</p>"),
      HTML("<p style='color: #006bb6; font-weight: bold;'>- NYK owns WAS's pick, top-8 protected</p>"),
      HTML("<p style='color: #1d428a; font-weight: bold;'>- LAC owns IND's pick if it falls between 5 and 9</p>"),
      HTML("<p style='color: #002D62; font-weight: bold;'>- OKC owns the most favorable of its pick, HOU's pick (protected 1-4), and LAC's pick</p>"),
      HTML("<p style='color: #006bb6; font-weight: bold;'>- PHI owns the second most favorable of OKC's pick, HOU's pick (protected 1-4), and LAC's pick</p>"),
      HTML("<p style='color: #002D62; font-weight: bold;'>- OKC owns UTA's pick, top-8 protected</p>"),
      HTML("<p style='color: #002D62; font-weight: bold;'>- OKC owns PHI's pick, top-4 protected</p>"),
      HTML("<p style='color: #000000; font-weight: bold;'>- SA owns the more favorable of their and ATL's pick</p>"),
      HTML("<p style='color: #FDB927; font-weight: bold;'>- ATL owns the less favorable of their and SA's pick</p>"),
      HTML("<p style='color: #5d76a9; font-weight: bold;'>- MEM owns the two most favorable of its pick, the least favorable of PHX's and WAS's pick, and ORL's pick</p>"),
      HTML("<p style='color: #1d1160; font-weight: bold;'>- CHA owns the least favorable of MEM's pick, the least favorable of PHX's and WAS's pick, and ORL's pick</p>"),
      a("For more details on these protections and other pick trade details, visit realgm.com", 
        href = "https://basketball.realgm.com/nba/draft/future_drafts/detailed", 
        target = "_blank"
      )
    )
  ),
  absolutePanel(
    top = 10, right = 10,
    width = "auto", height = "auto",
    style = "background-color: transparent; font-size: 12px; color: gray;",
    "Created by Shane Faberman"
  )
)


server <- function(input, output) {
  
  
  output$lottery_table <- renderTable({
    data.frame(Pick = 1:14, Team = results())
  })
  
  reactive_sim <- eventReactive(input$simulate_btn, {
    withProgress(message = "Simulating... (Depending on your selections, this could take a few minutes.)", value = 0.2, {
      results_list <- replicate(
        n = 10000,
        expr = simulate_lottery(df, standings,
                                force_team1 = input$`1`, force_team2 = input$`2`,
                                force_team3 = input$`3`, force_team4 = input$`4`, force_team5 = input$`5`,
                                force_team6 = input$`6`, force_team7 = input$`7`, force_team8 = input$`8`,
                                force_team9 = input$`9`, force_team10 = input$`10`, force_team11 = input$`11`,
                                force_team12 = input$`12`, force_team13 = input$`13`, force_team14 = input$`14`
        ),
        simplify = FALSE
      )
      
      results_df <- results_list %>%
        map_dfr(~ set_names(as.list(.x), 1:14))
      
      results_long <- results_df %>%
        pivot_longer(cols = everything(), names_to = "Pick", values_to = "Team") %>%
        group_by(Pick, Team) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(freq = count / 10000,
               Pick = as.integer(Pick)) %>%
        select(-count) %>%
        arrange(Pick, Team) %>%
        pivot_wider(id_cols = Team, names_from = Pick, values_from = freq)
      
      results_long <- results_long %>%
        mutate(Team = fct_relevel(Team, standings)) %>%
        arrange(Team)
      
      results_long
    })
  })
  
  
  output$tbl <- render_gt({
    
    # simulation table
    
    reactive_sim() %>% 
      gt() %>% 
      fmt_percent(  
        columns = -Team,
        decimals = 2
      ) %>% 
      gt_nba_logos(columns = c("Team")) %>% 
      opt_stylize(style = 5, color = "blue") %>% 
      sub_missing(
        columns = everything(),
        rows = everything(),
        missing_text = ""
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#006bb6"),
          cell_text(color = "#f58426", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`9`:`14`),
          rows = Team == "WAS"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#FDB927"),
          cell_text(color = "#C8102E", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`1`:`14`),
          rows = Team == "NO"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#002D62"),
          cell_text(color = "#EF3B24", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`5`:`14`),
          rows = Team == "PHI"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#002D62"),
          cell_text(color = "#EF3B24", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`1`:`14`),
          rows = Team == "LAC"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#002D62"),
          cell_text(color = "#EF3B24", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`9`:`14`),
          rows = Team == "UTA"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#5d76a9"),
          cell_text(color = "#12173f", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`1`:`14`),
          rows = Team == "PHX"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#000000"),
          cell_text(color = "#c4ced4", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`1`:`14`),
          rows = Team == "ATL"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#1d428a"),
          cell_text(color = "#c8102e", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`5`:`9`),
          rows = Team == "IND"
        )
      )
  })
  
  output$tbl2 <- render_gt({
    
    # true odds table
    
    lottery_odds %>% 
      select(Team, pick, odds) %>% 
      mutate(odds = round(odds,2),
             odds = ifelse(odds == "0.00%", "-", odds)) %>% 
      mutate(odds = sprintf("%.2f%%", odds),
             odds = ifelse(odds == "0.00%", "", odds)) %>% 
      pivot_wider(names_from = pick, values_from = odds) %>%
      gt() %>% 
      gt_nba_logos(columns = c("Team")) %>% 
      opt_stylize(style = 5, color = "blue") %>% 
      tab_style(
        style = list(
          cell_fill(color = "#006bb6"),
          cell_text(color = "#f58426", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`9`:`14`),
          rows = Team == "WAS"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#FDB927"),
          cell_text(color = "#C8102E", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`1`:`14`),
          rows = Team == "NO"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#002D62"),
          cell_text(color = "#EF3B24", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`5`:`14`),
          rows = Team == "PHI"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#002D62"),
          cell_text(color = "#EF3B24", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`1`:`14`),
          rows = Team == "LAC"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#002D62"),
          cell_text(color = "#EF3B24", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`9`:`14`),
          rows = Team == "UTA"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#5d76a9"),
          cell_text(color = "#12173f", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`1`:`14`),
          rows = Team == "PHX"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#000000"),
          cell_text(color = "#c4ced4", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`1`:`14`),
          rows = Team == "ATL"
        )
      ) %>% 
      tab_style(
        style = list(
          cell_fill(color = "#1d428a"),
          cell_text(color = "#c8102e", weight = "bold")
        ),
        locations = cells_body(
          columns = c(`5`:`9`),
          rows = Team == "IND"
        )
      )
  })
}


shinyApp(ui = ui, server = server)
