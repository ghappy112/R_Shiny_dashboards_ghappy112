library(shiny)
library(bslib)
library(ggplot2)

# Define UI for app
ui <- page_sidebar(
  sidebar = sidebar(
  sidebarPanel(
    checkboxGroupInput("filter", "Filter:",
                       choices = c("Coding Languages", "Python Packages", "R Packages"),
                       selected = c("Coding Languages")),
    width = 100
  )),
  # App title
  title = "Coding Languages Proficiencies",
  # Output: Bar Chart
  plotOutput(outputId = "barPlot")
)

# Define server logic
server <- function(input, output) {
  
  # Define dataframe with categories and frequencies
  df = data.frame(
    Language = c("Python", "R", "Stata", "SQL", "HTML", "CSS", "JavaScript", "DAX", "Pandas", "NumPy", "SciPy", "scikit-learn", "Keras", "TensorFlow", "Matplotlib", "Seaborn", "Regex", "requests", "Beautiful Soup", "Selenium", "json", "XGBoost", "CATBoost", "pickle", "joblib", "statsmodels", "NLTK", "Flask", "Shiny", "Stargazer", "ggplot2", "plotly", "tidy2", "dplyr"),
    Skill = c(2.5, 2, 1.5, 3, 1, 0.5, 1, 2, 3, 2, 1.5, 3, 2, 2, 2, 2, 3, 2, 2.5, 1, 1.5, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1),
    filter = c("Coding Languages", "Coding Languages", "Coding Languages", "Coding Languages", "Coding Languages", "Coding Languages", "Coding Languages", "Coding Languages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "R Packages", "R Packages", "R Packages", "R Packages", "R Packages", "R Packages")
  )
  # engineer sorting order feature for dataframe for sorting bar chart
  df = df[order(df$Language), ]
  df = df[order(df$Skill, decreasing=TRUE), ]
  df$Order = seq(1, nrow(df))
  
  # Render a bar chart based on dataframe
  output$barPlot <- renderPlot({
    
    # Filter dataframe based on selected filters
    filtered_df <- df[df$filter %in% input$filter, ]
    
    # Create bar chart using ggplot
    ggplot(filtered_df, aes(x = Skill, y = reorder(Language, -Order))) +
      geom_bar(stat = "identity", fill = "#00AFBB", color = "white") +
      labs(title = "Coding Languages Proficiencies",
           x = "Proficiency",
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
               ifelse(x == 0.5, "",
                      ifelse(x == 1, "Basic",
                             ifelse(x == 1.5, "",
                                    ifelse(x == 2, "Intermediate",
                                           ifelse(x == 2.5, "", "Advanced"))))))
        })
  })
}

# run shiny app
shinyApp(ui = ui, server = server)
