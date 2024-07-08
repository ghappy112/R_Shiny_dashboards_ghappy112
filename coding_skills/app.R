library(shiny)
library(bslib)
library(ggplot2)

# Define UI for app
ui <- page_sidebar(
  # App title
  title = "Coding Languages Skill Levels",
  # Output: Bar Chart
  plotOutput(outputId = "barPlot")
)

# Define server logic
server <- function(input, output) {
  
  # Define dataframe with categories and frequencies
  df = data.frame(
    Language = c("Python", "R", "Stata", "SQL", "HTML", "CSS", "JavaScript", "DAX"),
    Skill = c(2.5, 2, 1.5, 3, 1, 0.5, 1, 2)
  )
  # engineer features for dataframe for bar chart
  df = df[order(df$Language, decreasing=FALSE), ]
  df = df[order(df$Skill, decreasing=TRUE), ]
  row.names(df) <- NULL
  df$Order = seq(1, nrow(df))
  df$Skill = df$Skill * 2
  
  # Render a bar chart based on dataframe
  output$barPlot <- renderPlot({
    
    # Create bar chart using ggplot
    ggplot(df, aes(x = Skill, y = reorder(Language, -Order))) +
      geom_bar(stat = "identity", fill = "#00AFBB", color = "white") +
      labs(title = "Coding Languages Skill Levels",
           x = "Skill Level",
           y = "") +
      theme_minimal() + theme(plot.background = element_blank(),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              plot.title = element_text(hjust = 0.5, size = rel(1.8)),
                              axis.title = element_text(size = rel(1.2)),
                              axis.text = element_text(size = rel(1.2))
                              ) +
      scale_x_continuous(labels = function(x) {
        ifelse(x == 0, "Novice",
               ifelse(x == 1, "",
                 ifelse(x == 2, "Basic",
                        ifelse(x == 3, "",
                               ifelse(x == 4, "Intermediate",
                                      ifelse(x == 5, "", "Advanced"))))))
        })
  })
}

# run shiny app
shinyApp(ui = ui, server = server)
