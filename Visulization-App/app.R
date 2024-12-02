library(shiny)
library(ggplot2)

song_data <- read.csv("Spotify-2000.csv")
numeric_cols <- c("Index", 
                  "Year", 
                  "Beats.Per.Minute..BPM.", 
                  "Energy", 
                  "Danceability", 
                  "Loudness..dB.",
                  "Liveness",
                  "Valence",
                  "Length..Duration.",
                  "Acousticness",
                  "Speechiness",
                  "Popularity"
                  )

ui <- fluidPage(
  titlePanel("Dynamic Column Plotter"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Choose Independent Variable (X-axis):",
                  choices = numeric_cols,
                  selected = "Year"),
      selectInput("y_var", "Choose Dependent Variable (Y-axis):",
                  choices = numeric_cols,
                  selected = "Danceability")
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    ggplot(data = song_data, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(color = "blue", size = 3) +
      labs(title = paste("Scatter Plot of", input$y_var, "vs", input$x_var),
           x = input$x_var,
           y = input$y_var) +
      theme_grey()
  })
}

shinyApp(ui = ui, server = server)
