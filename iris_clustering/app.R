library(shiny)
library(shinyWidgets)
library(bslib)
library(stats)
library(ggplot2)

# Load data
data("iris")

# Define UI for app
ui <- page_sidebar(
  sidebar = sidebar(
    selectInput("x", "X Value", choices = c("Petal Length", "Petal Width", "Sepal Length", "Sepal Width"), selected = "Petal Length"),
    selectInput("y", "Y Value", choices = c("Petal Length", "Petal Width", "Sepal Length", "Sepal Width"), selected = "Petal Width"),
    sliderTextInput("n_clusters",
                    label = "Number of Clusters",
                    grid = FALSE,
                    force_edges = TRUE,
                    choices = seq(2, 12),
                    selected = 3
    )
  ),
  title = "K-means Clustering of Iris Dataset",
  plotOutput(outputId = "scatterPlot")
)

# Define server logic
server <- function(input, output) {
  
  # Render a scatter plot
  output$scatterPlot <- renderPlot({
    
    # select x and y
    x = gsub(" ", ".", input$x)
    y = gsub(" ", ".", input$y)
    
    # Create new data frame with selected columns
    iris_data <- iris[, c(x, y)]
    #iris_data
    
    # Perform k-means clustering
    set.seed(42)  # Set seed for reproducibility
    kmeans_result <- kmeans(iris_data, centers = input$n_clusters)  # Assuming 3 clusters (since there are 3 species of iris)
    
    # Clusters
    iris_data$cluster <- kmeans_result$cluster
    #iris_data
    
    # scatterplot
    ggplot(iris_data, aes_string(x=x, y=y, color = "factor(cluster)")) + 
      geom_point(size = 3) +
      labs(x = input$x, y = input$y, color = "cluster") +
      ggtitle("K-means Clustering of Iris Dataset") +
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            axis.title = element_text(size = 16, face = "bold"),
            legend.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 14))
  })
}

# run shiny app
shinyApp(ui = ui, server = server)
