library(shiny)
library(ggplot2)
library(shinythemes)
source("Bookworm_Brigade.R")


fluidPage(
  
  titlePanel("Bookworm Brigade 2024 Reading Challenge"),
  theme = shinytheme("paper"),
  sidebarPanel(
  #could change these drop downs to select the person's books or other info
   #  selectInput('bailey', 'Bailey', range(1:10)),
   #  selectInput('cal', 'Cal', range(1:10)),
   # selectInput('dustin', 'Dustin', range(1:10)),
   # selectInput('emy', 'Emy', range(1:10)),
   # selectInput('katherine', 'Katherine', range(1:10)),
   # 

  checkboxGroupInput(
    "checkGroup",
    "Select readers:",
    choices = standings_df$name,
    selected = c("Emy", "Bookworms")
  )
  
  ),
  
  mainPanel(
    
    fluidRow(
      verticalLayout(
        splitLayout(cellWidths = c("50%", "50%"), plotOutput('team_standing_totals'),tableOutput('individuals_table'))
        ,
        plotOutput('individual_piechart')
      )
    )
  )
)