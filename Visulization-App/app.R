library(shiny)
library(ggplot2)
library(plotly)

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
  titlePanel("Spotify Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dim", "Dimension:", 
                  choices = c("2D", "3D"),
                  selected = "2D"),
      selectInput("x_var", "X-axis:",
                  choices = numeric_cols,
                  selected = "Year"),
      selectInput("y_var", "Y-axis",
                  choices = numeric_cols,
                  selected = "Danceability"),
      selectInput("z_var", "Z-axis",
                  choices = numeric_cols,
                  selected = "Energy")
    ),
    mainPanel(
      plotlyOutput("scatterPlot"),
    )
  )
)

server <- function(input, output) {
  dimension <- reactive({
    dimension <- input$dim
  })
  observe({
    if (dimension() == "3D") {
      output$scatterPlot <- renderPlotly({
        plot_ly(data=song_data, 
                x = ~get(input$x_var), 
                y = ~get(input$y_var), 
                z = ~get(input$z_var), 
                type="scatter3d", 
                mode="markers") %>%
          layout(
            title = paste(input$z_var, "as a function of", input$x_var, "and", input$y_var),
            scene = list(
              xaxis = list(title = paste("", input$x_var)),
              yaxis = list(title = paste("", input$y_var)),
              zaxis = list(title = paste("", input$z_var))
            )
          )
      })
    } else {
      output$scatterPlot <- renderPlotly({
        ggplot(data = song_data, aes_string(x = input$x_var, y = input$y_var)) +
          geom_point(color = "blue", size = 3) +
          labs(title = paste("Scatter Plot of", input$y_var, "vs", input$x_var),
               x = input$x_var,
               y = input$y_var) +
          theme_grey()
      })
    }
  })
}

shinyApp(ui = ui, server = server)
