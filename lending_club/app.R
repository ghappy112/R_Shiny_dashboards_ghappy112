library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(ggsci)
library(dplyr)

# For reproducibility for scatter plots
set.seed(42)

# load data
df <- read.csv("loan_data.csv")

# filter out negative debt to income
df <- df[df$debt_to_income >= 0, ]

# dates list for time series filter
dates = sort(unique(df$date))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Lending Club Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Interest Rates Dashboard", tabName = "interest_rate_dashboard", icon = icon("dashboard")),
      menuItem("Defaults Dashboard", tabName = "default_dashboard", icon = icon("dashboard"))
    ),
    sidebarPanel(
      style = "background-color: transparent; border: 3px solid",  # Make the sidebar transparent
      sliderTextInput("filter0",
                      label = "Year Issued",
                      grid = FALSE,
                      force_edges = TRUE,
                      choices = dates,
                      selected = c(dates[1], tail(dates, n=1))
      ),
      width=0
    ),
    sidebarPanel(
      style = "background-color: transparent; border: 3px solid",  # Make the sidebar transparent
      checkboxGroupInput("filter1",
                         "Default",
                         choices = sort(unique(df$default_label)),
                         selected = unique(df$default_label)
      ),
      width=0
    ),
    sidebarPanel(
      style = "background-color: transparent; border: 3px solid",  # Make the sidebar transparent
      sliderInput("filter3",
                  label = "Interest Rate",
                  min = min(df$int_rate),
                  max = max(df$int_rate),
                  value = c(min(df$int_rate), max(df$int_rate)),
                  step = 0.01  # Adjust the step size if needed
      ),
      width=0
    ),
    sidebarPanel(
      style = "background-color: transparent; border: 3px solid",  # Make the sidebar transparent
      sliderInput("filter4",
                  label = "FICO Score",
                  min = min(df$fico),
                  max = max(df$fico),
                  value = c(min(df$fico), max(df$fico)),
                  step = 1  # Adjust the step size if needed
      ),
      width=0
    ),
    sidebarPanel(
      style = "background-color: transparent; border: 3px solid",  # Make the sidebar transparent
      sliderInput("filter5",
                  label = "Debt to Income Ratio",
                  min = min(df$debt_to_income),
                  max = max(df$debt_to_income),
                  value = c(min(df$debt_to_income), max(df$debt_to_income)),
                  step = 1  # Adjust the step size if needed
      ),
      width=0
    ),
    sidebarPanel(
      style = "background-color: transparent; border: 3px solid",  # Make the sidebar transparent
      checkboxGroupInput("filter2",
                         "Loan Grade",
                         choices = c("A", "B", "C", "D", "E", "F", "G"),
                         selected = c("A", "B", "C", "D", "E", "F", "G")
      ),
      width=0
    )
  ),
  dashboardBody(
    #use_theme(mytheme),
    tabItems(
      tabItem(tabName = "interest_rate_dashboard",
              fluidRow(
                box(plotOutput("interest_rate_plot2", width = "100%", height = 400)),
                box(plotOutput("ts_plot2", width = "100%", height = 400))
              ),
              fluidRow(
                box(plotOutput("grade_plot2", width = "100%", height = 400)),
                box(plotOutput("fico_plot2", width = "100%", height = 400))
              ),
              fluidRow(
                box(plotOutput("dti_plot2", width = "100%", height = 400)),
                box(plotOutput("interest_rate_plot3", width = "100%", height = 400))
              )
      ),
      tabItem(tabName = "default_dashboard",
              fluidRow(
                box(plotOutput("loan_status_plot", width = "100%", height = 400)),
                box(plotOutput("ts_plot", width = "100%", height = 400))
              ),
                fluidRow(
                box(plotOutput("grade_plot", width = "100%", height = 400)),
                box(plotOutput("fico_plot", width = "100%", height = 400))
              ),
              fluidRow(
                box(plotOutput("dti_plot", width = "100%", height = 400)),
                box(plotOutput("interest_rate_plot", width = "100%", height = 400))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {

  # build reactive functionality
  
  # Create a reactive value to store the filtered dataframe
  filtered_df <- reactiveVal(df)
  
  # Observe changes in user input
  observe({
    filtered_df(df[
      df$default_label %in% input$filter1 & 
      df$grade %in% input$filter2 & 
      df$date %in% seq(as.integer(input$filter0[1]), as.integer(input$filter0[2])) &
      df$int_rate >= input$filter3[1] &
      df$int_rate <= input$filter3[2] &
      df$fico >= input$filter4[1] &
      df$fico <= input$filter4[2] &
      df$debt_to_income >= input$filter5[1] &
      df$debt_to_income <= input$filter5[2], ])
  })
  
  # reactive sample dataframe for scatterplots
  sample_df <- reactive({
    filtered_df()[sample(nrow(filtered_df()), 1000), ]
  })
  
  
  # data viz
  
  # default rate by grade data viz
  output$grade_plot <- renderPlot({
    
    grade_df <- filtered_df() %>% group_by(grade) %>% summarise(default_rate = mean(default), .groups = 'drop')
    grade_df <- data.frame(grade_df)
    grade_df$default_rate <- grade_df$default_rate * 100
    
    ggplot(grade_df, aes(x = grade, y = default_rate)) +#, fill = grade)) +
      geom_bar(stat = "identity", color = "white", fill="#4DBBD5FF") +
      labs(title = "Default Rate by Grade",
           x = "Loan Grade",
           y = "Default Rate (%)") +
      #scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
      scale_fill_npg() +
      theme_minimal() + 
      theme(plot.background = element_rect(fill = "white", color = "white"),
            panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            axis.title = element_text(size = 16, face = "bold"),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.position = "none"
      )# +
    #scale_y_continuous(labels = label_percent(scale = 1))
  })
  
  # dti by loan status data viz
  output$dti_plot <- renderPlot({
    ggplot(filtered_df(), aes(default_label, debt_to_income)) + 
      geom_boxplot(aes(fill = default_label)) +
      #scale_fill_viridis(discrete = TRUE, option = "D", direction = 1) +
      scale_fill_npg() +
      labs(title = "Debt to Income by Loan Status",
           x = "Loan Status",
           y = "Debt to Income Ratio") +
      theme_minimal() +
      theme(legend.position = "None",
            plot.background = element_rect(fill = "white", color = "white"),
            panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            axis.title = element_text(size = 16, face = "bold"),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))
  })
  
  # fico by loan status data viz
  output$fico_plot <- renderPlot({
    ggplot(filtered_df(), aes(default_label, fico)) + 
      geom_boxplot(aes(fill = default_label)) +
      #scale_fill_viridis(discrete = TRUE, option = "D", direction = 1) +
      scale_fill_npg() +
      labs(title = "FICO by Loan Status",
           x = "Loan Status",
           y = "FICO Score") +
      theme_minimal() +
      theme(legend.position = "None",
            plot.background = element_rect(fill = "white", color = "white"),
            panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            axis.title = element_text(size = 16, face = "bold"),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))
  })
  
  # interest rate by loan status data viz
  output$interest_rate_plot <- renderPlot({
    ggplot(filtered_df(), aes(default_label, int_rate)) + 
      geom_boxplot(aes(fill = default_label)) +
      #scale_fill_viridis(discrete = TRUE, option = "D", direction = 1) +
      scale_fill_npg() +
      labs(title = "Interest Rate by Loan Status",
           x = "Loan Status",
           y = "Interest Rate (%)") +
      theme_minimal() +
      theme(legend.position = "None",
            plot.background = element_rect(fill = "white", color = "white"),
            panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            axis.title = element_text(size = 16, face = "bold"),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))
  })
  
  # interest rate by loan status data viz
  output$interest_rate_plot3 <- renderPlot({
    ggplot(filtered_df(), aes(default_label, int_rate)) + 
      geom_boxplot(aes(fill = default_label)) +
      #scale_fill_viridis(discrete = TRUE, option = "D", direction = 1) +
      scale_fill_npg() +
      labs(title = "Interest Rate by Loan Status",
           x = "Loan Status",
           y = "Interest Rate (%)") +
      theme_minimal() +
      theme(legend.position = "None",
            plot.background = element_rect(fill = "white", color = "white"),
            panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            axis.title = element_text(size = 16, face = "bold"),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))
  })
  
  # loan status data viz
  output$loan_status_plot <- renderPlot({
    
    loan_status_df <- filtered_df() %>% group_by(default_label) %>% summarise(freq = n(), .groups = 'drop')
    loan_status_df <- data.frame(loan_status_df)
    loan_status_df$pct_freq <- loan_status_df$freq / sum(loan_status_df$freq)
    loan_status_df$ymax <- cumsum(loan_status_df$pct_freq) # Compute the cumulative percentages (top of each rectangle)
    loan_status_df$ymin <- c(0, head(loan_status_df$ymax, n=-1)) # Compute the bottom of each rectangle
    loan_status_df$labelPosition <- (loan_status_df$ymax + loan_status_df$ymin) / 2 # Compute label position
    loan_status_df$label <- paste0(round(loan_status_df$pct_freq * 100, 2), "%") # Compute a good label
    
    ggplot(loan_status_df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = default_label)) +
      geom_rect() +
      labs(title = "Loan Status", fill = NULL) +
      geom_text(aes(x = 2, y = labelPosition, label = label), size = 6) +
      #scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
      scale_fill_npg() +
      scale_color_manual(values = c("black", "black")) +
      coord_polar(theta = "y") +
      xlim(c(-1, 4)) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.key = element_rect(fill = NA, color = NA), # Make legend key background transparent
            legend.key.size = unit(1, "cm"), # Adjust the size of the legend key
            legend.text = element_text(size = 14), # Adjust the text size in the legend
            plot.title = element_text(hjust = 0.5, size = 24, face = "bold"))
  })
  
  # default rate time series
  output$ts_plot <- renderPlot({
    
    ts_df <- filtered_df() %>% group_by(date) %>% summarise(default_rate = mean(default) * 100, .groups = 'drop')
    ts_df <- data.frame(ts_df)
    ts_df <- ts_df[order(ts_df$date), ]
    
    ggplot(data = ts_df, aes(x = date, y = default_rate, group = 1)) +
      geom_line(color = "#4DBBD5FF", size = 1.2) +
        geom_point(color = "#4DBBD5FF", size = 3, shape = 21, fill = "white") +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#4DBBD5FF", linetype = "dashed") +
        labs(title = "Default Rate over Time",
             x = "Year Issued",
             y = "Default Rate (%)") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels vertically
          axis.text.y = element_text(size = 14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = "none"
        ) +
        theme(plot.background = element_rect(fill = "white", color = NA)) +
        theme(panel.grid.major = element_line(color = "#d9d9d9"),##d9d9d9
              panel.grid.minor = element_line(color = "#f5f5f5", linetype = "dashed")) +
        theme(plot.margin = margin(10, 10, 30, 10, "pt")) +
        theme(panel.background = element_rect(fill = "#d9d9d9", color = NA))
  })
  
  # interest rate boxplot
  output$interest_rate_plot2 <- renderPlot({
    ggplot(filtered_df(), aes(y = int_rate)) + 
      geom_boxplot(fill = "#4DBBD5FF", width = 0.5) +  # Set box plot width
      labs(title = "Interest Rates",
           x = NULL,  # No x-axis label for single-variable box plot
           y = "Interest Rate (%)") +
      theme_minimal() +
      theme(legend.position = "None",
            plot.background = element_rect(fill = "white", color = "white"),
            panel.grid.major = element_blank(),  # Remove major grid lines
            panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
            panel.grid.minor.y = element_line(color = "grey80"),
            #panel.grid.minor = element_blank(),  # Remove minor grid lines
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            axis.title = element_text(size = 16, face = "bold"),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 14)) + 
      coord_cartesian(xlim = c(-0.5, 0.5))  # Adjust xlim to control width
  })
  
  # interest rate time series
  output$ts_plot2 <- renderPlot({
    
    ts_df2 <- filtered_df() %>% group_by(date) %>% summarise(interest_rate = mean(int_rate), .groups = 'drop')
    ts_df2 <- data.frame(ts_df2)
    ts_df2 <- ts_df2[order(ts_df2$date), ]
    
    ggplot(data = ts_df2, aes(x = date, y = interest_rate, group = 1)) +
      geom_line(color = "#4DBBD5FF", size = 1.2) +
      geom_point(color = "#4DBBD5FF", size = 3, shape = 21, fill = "white") +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#4DBBD5FF", linetype = "dashed") +
      labs(title = "Interest Rate over Time",
           x = "Year Issued",
           y = "Avg. Interest Rate (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels vertically
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none"
      ) +
      theme(plot.background = element_rect(fill = "white", color = NA)) +
      theme(panel.grid.major = element_line(color = "#d9d9d9"),##d9d9d9
            panel.grid.minor = element_line(color = "#f5f5f5", linetype = "dashed")) +
      theme(plot.margin = margin(10, 10, 30, 10, "pt")) +
      theme(panel.background = element_rect(fill = "#d9d9d9", color = NA))
  })
  
  # interest rate fico
  output$fico_plot2 <- renderPlot({
    ggplot(sample_df(), aes(x = fico, y = int_rate, color = default_label)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
      scale_color_npg() +  # Use the Academic color palette for color
      labs(x = "FICO Score", 
           y = "Interest Rate (%)", 
           color = NULL,
           title = "FICO vs Interest Rate") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            axis.title = element_text(size = 16, face = "bold"),
            #legend.title = NULL,  # Uncommented legend.title for better visibility
            axis.text = element_text(size = 14),
            legend.position = "bottom",
            legend.text = element_text(size = 14))
  })
  
  # interest rate dti
  output$dti_plot2 <- renderPlot({
    ggplot(sample_df(), aes(x = debt_to_income, y = int_rate, color = default_label)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
      scale_color_npg() +  # Use the Academic color palette for color
      labs(x = "Debt to Income Ratio", 
           y = "Interest Rate (%)", 
           color = NULL,
           title = "Debt to Income vs Interest Rate") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            axis.title = element_text(size = 16, face = "bold"),
            #legend.title = NULL,  # Uncommented legend.title for better visibility
            axis.text = element_text(size = 14),
            legend.position = "bottom",
            legend.text = element_text(size = 14))
  })
  
  # interest rate grade
  output$grade_plot2 <- renderPlot({
    
    grade_df2 <- filtered_df() %>% group_by(grade) %>% summarise(interest_rate = mean(int_rate), .groups = 'drop')
    grade_df2 <- data.frame(grade_df2)
    
    ggplot(grade_df2, aes(x = grade, y = interest_rate)) +#, fill = grade)) +
      geom_bar(stat = "identity", color = "white", fill="#4DBBD5FF") +
      labs(title = "Interest Rate by Grade",
           x = "Loan Grade",
           y = "Avg. Interest Rate (%)") +
      #scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
      scale_fill_npg() +
      theme_minimal() + 
      theme(plot.background = element_rect(fill = "white", color = "white"),
            panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            axis.title = element_text(size = 16, face = "bold"),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.position = "none")
  })
}

# run shiny app
shinyApp(ui = ui, server = server)
