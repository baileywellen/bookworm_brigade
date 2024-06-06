library(shiny)
library(ggplot2)
source("Bookworm_Brigade.R")

function(input, output) {
  
  standings_df <-create_standings_df()
  
  filtered_df <- reactive({
    req(input$checkGroup)
    standings_df %>% filter(name %in% input$checkGroup)
  })

  people_colors <- c("Bailey" = 'turquoise4', "Cal" = "orange", "Emy" = "purple", "Dustin" = "green", "Katherine" = "pink", "Bookworms" = "black")
  #bar chart of totals
  output$team_standing_totals <- renderPlot({
      team_standings_plot <- filtered_df() %>% 
        ggplot(aes(x=reorder(name, -books_read), y = books_read, fill = name)) +
      geom_bar(stat = "identity") + 
        xlab("Reader") + 
        ylab("Number of Books Completed (#)") + 
        ggtitle("Standings as Totals") + 
        scale_fill_manual(values = people_colors)
      
      print(team_standings_plot)
  })
  
  #bar chart of percent of goal
  output$team_standing_perc <- renderPlot({
    team_standings_perc <- filtered_df() %>% 
      ggplot(aes(x=reorder(name, -percent_complete), y = percent_complete, fill = name)) +
      geom_bar(stat = "identity") + 
      xlab("Reader") + 
      ylab("Percent of Goal Complete (%)") + 
      ggtitle("Standings as Percentage of Goal") + 
      scale_fill_manual(values = people_colors)
    
    print(team_standings_perc)
  })
  
  #pie chart of individuals' progress
  output$individual_progress <- renderPlot({
    individual_standings_plot <- filtered_df() %>%
      ggplot(aes(x="", y=books_read, fill=name)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      ggtitle("Book Totals") + 
      scale_fill_manual(values = people_colors)
    
    print(individual_standings_plot)
  })
  
  output$individual_piechart <- renderPlot({
    individual_piechart <- filtered_df() %>%
      pivot_longer(cols = c(percent_complete, percent_remaining), names_to = "category") %>%
      ggplot(aes(x = "", y = value, fill = category)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      facet_wrap(~ name, ncol = 3) +
      ggtitle("Progress Towards Goal") +
      theme_void() +
      scale_fill_manual(values = c("percent_complete" = "darkgreen", "percent_remaining" = "darkred"))
    
    print(individual_piechart)
  })
  
  output$individuals_table <- renderTable({
    output_table <- filtered_df() %>%
      arrange(desc(books_read))
  })
  
}