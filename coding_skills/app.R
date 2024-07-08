library(shiny)
library(bslib)
library(ggplot2)

# Define UI for app
ui <- page_sidebar(
  sidebar = sidebar(
    sidebarPanel(
      checkboxGroupInput("filter", "Technology Filter",
                         choices = c("Cloud", "Coding Languages", "Python Packages", "R Packages", "Software"),
                         selected = c("Coding Languages")),
      width = 100
    ),
    sidebarPanel(
      checkboxGroupInput("skill_filter", "Proficiency Filter",
                         choices = c("Novice", "Basic", "Intermediate", "Advanced"),
                         selected = c("Novice", "Basic", "Intermediate", "Advanced")),
      width = 100
    )),
  # App title
  title = "Coding Languages & Technical Tools Proficiencies",
  # Output: Bar Chart
  plotOutput(outputId = "barPlot")
)

# Define server logic
server <- function(input, output) {
  
  # Define dataframe with categories and frequencies
  df = data.frame(
    Language = c("Python", "R", "Stata", "SQL", "HTML", "CSS", "JavaScript", "DAX", "Pandas", "NumPy", "SciPy", "scikit-learn", "Keras", "TensorFlow", "Matplotlib", "Seaborn", "Regex", "requests", "Beautiful Soup", "Selenium", "json", "XGBoost", "CATBoost", "pickle", "joblib", "statsmodels", "NLTK", "Flask", "Shiny", "Stargazer", "ggplot2", "plotly", "tidy2", "dplyr", "PySpark", "Transformers", "Power BI", "Tableau", "Excel", "Power Automate", "Power Apps", "AWS", "Azure"),
    Skill = c(2.5, 2, 1.5, 3, 1, 0.5, 1, 2, 3, 2, 1.5, 3, 2, 2, 2, 2, 3, 2, 2.5, 1, 1.5, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 1, 1.5, 2, 1, 2, 1, 1, 2, 0.5),
    filter = c("Coding Languages", "Coding Languages", "Coding Languages", "Coding Languages", "Coding Languages", "Coding Languages", "Coding Languages", "Coding Languages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "Python Packages", "R Packages", "R Packages", "R Packages", "R Packages", "R Packages", "R Packages", "Python Packages", "Python Packages", "Software", "Software", "Software", "Software", "Software", "Cloud", "Cloud")
  )
  
  # engineer sorting order feature for dataframe for sorting bar chart
  df = df[order(df$Language), ]
  df = df[order(df$Skill, decreasing=TRUE), ]
  df$Order = seq(1, nrow(df))
  
  # engineer labeled skill levels for filtering by skill level
  skill_labels = c()
  for (x in df$Skill) {
    if (x < 1) {
      skill_labels = append(skill_labels, "Novice")
    } else if (x < 2) {
      skill_labels = append(skill_labels, "Basic")
    } else if (x < 3) {
      skill_labels = append(skill_labels, "Intermediate")
    } else {
      skill_labels = append(skill_labels, "Advanced")
    }
  }
  df$skill_filter = skill_labels
  
  # Render a bar chart based on dataframe
  output$barPlot <- renderPlot({
    
    # Filter dataframe based on selected filters
    filtered_df <- df[df$filter %in% input$filter, ]
    filtered_df <- filtered_df[filtered_df$skill_filter %in% input$skill_filter, ]
    
    # get list of selected tool categories
    inputs <- c(input$filter)
    
    # set title based on user inputs
    if (length(inputs) == 1) {
      my_title <- inputs[1]
    } else if (length(inputs) == 0) {
      my_title <- ""
    } else if (length(inputs) == 2) {
      my_title <- paste(inputs[1], inputs[2], sep = " & ")
      if (my_title == "Python Packages & R Packages") {
        my_title <- "Python & R Packages"
      }
    } else if (length(inputs) > 2) {
      
      coding_languages <- "Coding Languages" %in% inputs
      coding_packages <- "Python Packages" %in% inputs | "R Packages" %in% inputs
      tools <- "Cloud" %in% inputs | "Software" %in% inputs
      
      if (coding_languages & tools & !coding_packages) {
        my_title <- "Coding Languages & Technical Tools"
      } else if ((coding_languages | coding_packages) & tools) {
        my_title <- "Coding & Technical Tools"
      } else {
        my_title <- "Coding Languages and Packages"
      }
    }
    
    # Create bar chart using ggplot
    ggplot(filtered_df, aes(x = Skill, y = reorder(Language, -Order))) +
      geom_bar(stat = "identity", fill = "#00AFBB", color = "white") +
      labs(title = my_title,
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
               ifelse(x == 1, "Basic",
                      ifelse(x == 2, "Intermediate",
                             ifelse(x == 3, "Advanced", ""))))
      })
  })
}

# run shiny app
shinyApp(ui = ui, server = server)
