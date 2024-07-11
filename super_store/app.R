library(shinydashboard)
library(fresh)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(rio)


# color palette for stacked bar chart
custom_palette <- c("#0072B2", "#009E73", "#D55E00", "#CC79A7")

# load excel file
data <- import_list("sample_-_superstore.xls") 
#data

# function for cleaning a df's column names
clean_colnames <- function(df, df_name) {
  for (colname in colnames(df)) {
    names(df)[names(df) == colname] <- gsub(paste(df_name, ".", sep = ""), "", colname)
  }
  return(df)
}

# get orders sheet data and convert to dataframe
orders = data.frame(data[1])
orders = clean_colnames(orders, "Orders")
#orders

# get returns sheet data and convert to dataframe
returns = data.frame(data[2])
returns = clean_colnames(returns, "Returns")
#returns

# get salesperson sheet data and convert to dataframe
people = data.frame(data[3])
people = clean_colnames(people, "People")
#people

# join dataframes
df = merge(orders, people, by = "Region")
#df = merge(df, returns, by = "Order.ID")
#df

# delete data that is no longer needed
rm(data, orders, returns, people)

# engineer Quarter Data
dates = c()
for (x in as.character(df$Order.Date)) {
  month = as.numeric(substr(x, 6, 7))
  year = substr(x, 1, 4)
  if (month <= 3) {
    quarter = 1
  } else if (month <= 6) {
    quarter = 2
  } else if (month <= 9) {
    quarter = 3
  } else {
    quarter = 4
  }
  date = paste(year, paste("Q", quarter, sep=""), sep = " ")
  dates = append(dates, date)
}
df$Year_Quarter = dates
#df

# delete data that is no longer needed
rm(date, dates, month, year, quarter, x)


# custom theme
mytheme = create_theme(
  adminlte_color(
    green = "#009E73"
  )
)


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Superstore Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sales Dashboard", tabName = "sales_dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    use_theme(mytheme),
    tabItems(
      tabItem(tabName = "sales_dashboard",
              fluidRow(
                box(plotOutput("time_series_plot", width = "100%", height = 400)),
                box(plotOutput("bar_plot", width = "100%", height = 400))
              ),
              fluidRow(
                box(plotOutput("stacked_bar_plot", width = "100%", height = 400)),
                box(infoBoxOutput("value2", width = NULL), width = 4),
                box(infoBoxOutput("value1", width = NULL), width = 4),
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # engineer sales by salesperson
  df_grp_person = df %>% group_by(Person)  %>%
    summarise(total_sales = sum(Sales), 
              .groups = 'drop')
  df_grp_person = data.frame(df_grp_person)
  #df_grp_person
  
  # engineer sales by year and quarter
  df_grp_time = df %>% group_by(Year_Quarter)  %>%
    summarise(total_sales = sum(Sales), 
              .groups = 'drop')
  df_grp_time = data.frame(df_grp_time)
  #df_grp_time
  
  # engineer sales by product category and salesperson
  df_grp_cat_person = df %>% group_by(Category, Person)  %>%
    summarise(total_sales = sum(Sales), 
              .groups = 'drop')
  df_grp_cat_person = data.frame(df_grp_cat_person)
  #df_grp_cat_person
  
  # engineer average quarterly growth metric
  df_grp_time2 = df_grp_time %>% mutate(total_sales_lag = lag(total_sales))
  df_grp_time2$Growth = (df_grp_time2$total_sales - df_grp_time2$total_sales_lag) / df_grp_time2$total_sales_lag
  df_grp_time2 = df_grp_time2 %>% drop_na()
  #df_grp_time
  avg_growth = paste(round(mean(df_grp_time2$Growth) * 100, 2), "%", sep = "")
  #avg_growth
  
  # engineer total sales
  total_sales = round(sum(df$Sales), 0)
  nchars = nchar(as.character(total_sales))
  if (nchars > 6) {
    total_sales = paste("$", round(total_sales / 1000000, 1), "M", sep = "")
  } else if (nchars > 3) {
    total_sales = paste("$", round(total_sales / 1000, 1), "K", sep = "")
  } else {
    total_sales = paste("$", round(total_sales, 1), sep = "")
  }
  total_sales
  
  # remove nchars
  rm(nchars)
  
  
  # render time series chart
  output$time_series_plot <- renderPlot({
    
    ggplot(data = df_grp_time, aes(x = Year_Quarter, y = total_sales, group = 1)) +
      
      # Line and point aesthetics
      geom_line(color = custom_palette[1], size = 1.2) +
      geom_point(color = custom_palette[1], size = 3, shape = 21, fill = "white") +
      
      # Add trend line (dashed) with the same color as the main line
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = custom_palette[1], linetype = "dashed") +
      
      # Labels and titles
      labs(title = "Quarterly Sales",
           x = "Quarter",
           y = "Sales") +
      
      # Theme adjustments for a clean look
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
      
      # Apply a subtle gray background to the plot area
      theme(plot.background = element_rect(fill = "white", color = NA)) +
      
      # Customizing the plot grid lines
      theme(panel.grid.major = element_line(color = "#d9d9d9"),##d9d9d9
            panel.grid.minor = element_line(color = "#f5f5f5", linetype = "dashed")) +
      
      # Adding a gradient shadow effect to the plot area
      theme(plot.margin = margin(10, 10, 30, 10, "pt")) +
      
      # Modifying the color of the facets
      theme(panel.background = element_rect(fill = "#d9d9d9", color = NA)) +
      
      # Format y-axis labels with $ sign and K/M suffix
      scale_y_continuous(labels = function(x) paste0("$", label_number_si()(x)))
  })
  
  # render bar chart
  output$bar_plot <- renderPlot({
    
    # sales by salesperson bar chart
    ggplot(df_grp_person, aes(x = total_sales, y = reorder(Person, total_sales))) +
      geom_bar(stat = "identity", fill = "#0072B2", color = "white") +
      labs(title = "Sales by Salesperson",
           x = "Sales",
           y = "Salesperson") +
      theme_minimal() + theme(plot.background = element_rect(fill = "white", color = "white"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
                              axis.title = element_text(size = 16, face = "bold"),
                              axis.text.x = element_text(size = 14),
                              axis.text.y = element_text(size = 14),
                              legend.position = "none"
      ) +
      scale_x_continuous(labels = function(x) paste0("$", scales::label_number_si()(x))) 
  })
  
  # render stacked bar chart
  output$stacked_bar_plot <- renderPlot({
    
    # sales by product category and salesperson bar chart
    ggplot(df_grp_cat_person, aes(fill = reorder(Person, total_sales), y = reorder(Category, total_sales), x=total_sales)) + 
      geom_bar(position="stack", stat="identity", color = NA) +
      labs(title = "Sales by Product Category",
           x = "Sales",
           y = "Product Category",
           fill = "") +
      scale_fill_manual(values = rev(custom_palette)) +
      theme_minimal() + theme(plot.background = element_rect(fill = "white", color = "white"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
                              axis.title = element_text(size = 16, face = "bold"),
                              axis.text.x = element_text(size = 14),
                              axis.text.y = element_text(size = 14)
      ) +
      theme(legend.position = "bottom") +
      scale_x_continuous(labels = function(x) paste("$", scales::label_number_si()(x))) 
  })
  
  # info box 1
  output$value1 <- renderInfoBox({
    infoBox(
      "Average Quarterly Growth",
      avg_growth,
      icon = icon("arrow-up"),
      color = "green"
    )
  })
  
  # value box 2
  output$value2 <- renderValueBox({
    infoBox(
      "Total Sales",
      total_sales,
      icon = icon("check"),
      color = "green"
    )
  })
}

# run app
shinyApp(ui, server)
