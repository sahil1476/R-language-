library(shiny)
library(tidyverse)

# Load dataset
baseball <- read.csv(choose.files())

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML('
                    body{background-color: #87CEEB;
                     background-image: url("https://t3.ftcdn.net/jpg/03/90/01/34/360_F_390013401_QPpHNmNtoV5AQvjzELo1zP1rDvHVe78v.jpg");
                    background-size: cover;
                    background-position: center;
                    }'))),
  titlePanel("Baseball Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("position", "Select positions:",
                     choices = c("All", unique(baseball$position)),
                     multiple = TRUE),
      helpText("There are nine positions in a standard baseball game:-

                 1]   Pitcher: The player who throws the ball to the batter to start each play.
                  
                 2]   Catcher: The player who crouches behind home plate and receives the pitcher's throw.
                    
                 3]   First baseman: The player who stands at first base and tries to catch the ball thrown by infielders and tag the runner out.
                    
                 4]  Second baseman: The player who stands at second base and tries to catch the ball thrown by infielders and tag the runner out.
                    
                 5]  Third baseman: The player who stands at third base and tries to catch the ball thrown by infielders and tag the runner out.
                    
                 6]   Shortstop: The player who stands between second and third base and tries to catch the ball thrown by infielders and tag the runner out.
                    
                 7]   Left fielder: The player who stands in left field and tries to catch the ball hit by the batter.
                    
                 8]  Center fielder: The player who stands in center field and tries to catch the ball hit by the batter.
                    
                 9]  Right fielder: The player who stands in right field and tries to catch the ball hit by the batter.")
                ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphs",
                 plotOutput("positions_plot"),
                 plotOutput("game_run_plot"),
                 plotOutput("win_position_plot"),
                 plotOutput("line_chart"),
                 plotOutput("area_chart"),
                 plotOutput("pie_chart")
                 
        ),
        tabPanel("Descriptive Statistics",
                 dataTableOutput("stats_table")
        )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  filtered_data <- reactive({
    if ("All" %in% input$position) {
      baseball
    } else {
      baseball %>% filter(position %in% input$position)
    } 
  })
  
  # Calculate descriptive statistics
  output$stats_table <- renderDataTable({
    stats <- filtered_data() %>%
      summarise(
        count = n(),
        avg_games = mean(Games),
        avg_runs = mean(Runs),
        avg_win = mean(Win),
        avg_loss = mean(Loss),
        avg_strikeouts = mean(Strikeouts)
      )
    stats <- cbind(statistic = rownames(stats), stats)
    rownames(stats) <- NULL
    stats
  })
  
  output$positions_plot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = position, fill = position)) +
      geom_bar() +
      labs(title = "Player Positions", x = "Position", y = "Count") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$game_run_plot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = Games , y = Runs, color = position)) +
      geom_point() +
      labs(title = "Player Games and Runs", x = "Games (total)", y = "Runs (totals)") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$win_position_plot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = position, y = Win, fill = position)) +
      geom_boxplot() +
      labs(title = "Player Win match by Position", x = "Position", y = "Win") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$line_chart <- renderPlot({
    chart_data <- filtered_data()
    if (length(input$position) == 1 || length(input$position) == 2 || length(input$position) == 3 || length(input$position) == 4 || length(input$position) == 5 || length(input$position) == 6 || length(input$position) == 7 || length(input$position) == 8 || length(input$position) == 9  ) {
      chart_data <- chart_data %>% mutate(label = Complete_Game)
    }
    ggplot(data = chart_data, aes(x = Games_played, y = Complete_Game, color = position, label = label)) +
      geom_line() +
      labs(title = "Player Runs over Time", x = "Games played", y = "Complete Game") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(label = ifelse(!is.na(label), label, "")), hjust = 0.5, vjust = 0.5)
  })
  
  output$area_chart <- renderPlot({
    ggplot(data = filtered_data(), aes(x = Strikeouts, y = Win, fill = position)) +
      geom_area() +
      labs(title = "Player Strikeouts", x = "Year", y = "Win numbers") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  output$pie_chart <- renderPlot({
    ggplot(data = filtered_data(), aes(x = "", fill = Loss)) +
      geom_bar(width = 2) +
      coord_polar("y", start = 0) +
      labs(title = "Distribution of match lossed", fill = "Loss matches Range") +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
