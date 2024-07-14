library(shinydashboard)
library(shinyWidgets)
library(fresh)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(rio)


# custom color palette
#custom_palette <- c("#0072B2", "#D55E00", "#b20019", "#009E73")
#custom_palette <- c("#009E73", "#0072B2", "#D55E00", "#b20019")
#custom_palette <- c("#0072B2", "#D55E00", "#b20019", "#00AAAA")
custom_palette <- c("#0072B2", "#D55E00", "#b20019", "#008080")

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
df = merge(x = df, y = returns, by = "Order.ID", all.x = TRUE)
#df

# delete data that is no longer needed
rm(data, orders, returns, people)

# engineer returns feature
returns = c()
returns_cat = c()
for (x in df$Returned) {
  if (is.na(x)) {
    returns = append(returns, 0)
    returns_cat = append(returns_cat, "Kept")
  } else {
    returns = append(returns, 1)
    returns_cat = append(returns_cat, "Returned")
  }
}
df$Returned <- returns
df$Returned_Label <- returns_cat

# engineer quarter feature
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
rm(date, month, year, quarter, x)

# engineer list of dates for sidebar filters
dates = sort(unique(df$Year_Quarter))
dates = sub("(\\d{4})\\s(Q\\d)", "\\2 \\1", dates)


# function for generating list of years and quarters given a max and min quarter and year
get_dates_vec <- function(start_date, end_date) {
  
  min_date <- strsplit(start_date, " ")[[1]]
  min_quarter = as.integer(gsub("Q", "", min_date[1]))
  min_year = as.integer(min_date[2])
  
  max_date <- strsplit(end_date, " ")[[1]]
  max_quarter = as.integer(gsub("Q", "", max_date[1]))
  max_year = as.integer(max_date[2])
  
  dates_vec = c()
  
  for (year in seq(from=min_year, to=max_year)) {
    
    from = 1
    to = 4
    
    if (year == min_year) {
      from = min_quarter
    }
    
    if (year == max_year) {
      to = max_quarter
    }
    
    for (quarter in seq(from=from, to=to)) {
      dates_vec = append(dates_vec, paste(year, " ", "Q", quarter, sep=""))
    }
  }
  
  return(dates_vec)
  
}


# custom theme
mytheme = create_theme(
  adminlte_color(
    #green = "#009E73",
    green = "#228B22", # forest green
    #green = "#2E8B57", # sea green
    #green = "#3CB371", # medium sea green
    red = "#b20019",
    blue = "#0072B2"
  )
)


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Superstore Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sales Dashboard", tabName = "sales_dashboard", icon = icon("dashboard")),
      menuItem("Returns Dashboard", tabName = "returns_dashboard", icon = icon("dashboard")),
      menuItem("Profitability Dashboard", tabName = "profit_dashboard", icon = icon("dashboard"))
    ),
    sidebarPanel(
      style = "background-color: transparent; border: 3px solid",  # Make the sidebar transparent
      sliderTextInput("filter0",
                      label = "Date Range Filter",
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
                         "Salesperson Filter",
                         choices = sort(unique(df$Person)),
                         selected = unique(df$Person),
                         #inline = TRUE,
                         #width = 0,
                         #choiceNames = NULL,
                         #choiceValues = NULL
                         ),
      width=0
  ),
  sidebarPanel(
    style = "background-color: transparent; border: 3px solid",  # Make the sidebar transparent
    checkboxGroupInput("filter2",
                       "Product Category Filter",
                       choices = sort(unique(df$Category)),
                       selected = unique(df$Category),
                       #inline = FALSE,
                       #width = 0,
                       #choiceNames = NULL,
                       #choiceValues = NULL
    ),
    width=0
  )
  ),
  dashboardBody(
    use_theme(mytheme),
    tabItems(
      tabItem(tabName = "sales_dashboard",
              fluidRow(
                box(infoBoxOutput("value2", width = NULL), width = 4),
                box(infoBoxOutput("value1", width = NULL), width = 4),
                box(infoBoxOutput("value0", width = NULL), width = 4)
              ),
              fluidRow(
                box(plotOutput("time_series_plot", width = "100%", height = 400)),
                box(plotOutput("bar_plot", width = "100%", height = 400))
              ),
              fluidRow(
                box(plotOutput("stacked_bar_plot", width = "100%", height = 400)),
                box(plotOutput("discount_rate_plot", width = "100%", height = 400))
              )
      ),
      tabItem(tabName = "returns_dashboard",
              fluidRow(
                box(infoBoxOutput("value3", width = NULL), width = 4),
                box(infoBoxOutput("value4", width = NULL), width = 4),
                box(infoBoxOutput("value5", width = NULL), width = 4)
              ),
              fluidRow(
                box(plotOutput("returns_bar_plot", width = "100%", height = 400)),
                box(plotOutput("returns_rate_bar_plot", width = "100%", height = 400))
              ),
              fluidRow(
                box(plotOutput("returns_ts", width = "100%", height = 400)),
                box(plotOutput("returns_donut", width = "100%", height = 400))
              )
      ),
      tabItem(tabName = "profit_dashboard",
              fluidRow(
                box(infoBoxOutput("value6", width = NULL), width = 4),
                box(infoBoxOutput("value7", width = NULL), width = 4)
              ),
              fluidRow(
                box(plotOutput("pm_plot", width = "100%", height = 400)),
                box(plotOutput("pm_ts_plot", width = "100%", height = 400))
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
  
  # Observe changes in user input (e.g., checkboxGroupInput)
  observe({
    filtered_df(df[df$Person %in% input$filter1 & df$Category %in% input$filter2 & df$Year_Quarter %in% get_dates_vec(input$filter0[1], input$filter0[2]), ])
  })
  
  # Render the summary of the filtered dataframe
  output$filtered_summary <- renderPrint({
    summary(filtered_df())
  })
  
  
  # reactive feature engineering
  
  # Engineer sales by salesperson
  df_grp_person <- reactive({
    filtered_df() %>%
      #filter(Person %in% input$filter1 & df$Category %in% input$filter2) %>%
      group_by(Person) %>%
      summarise(total_sales = sum(Sales), .groups = 'drop')
  })
  
  # Engineer sales by year and quarter
  df_grp_time <- reactive({
    filtered_df() %>%
      #filter(Person %in% input$filter1 & df$Category %in% input$filter2) %>%
      group_by(Year_Quarter) %>%
      summarise(total_sales = sum(Sales), .groups = 'drop')
  })
  
  # Engineer sales by product category and salesperson
  df_grp_cat_person <- reactive({
    filtered_df() %>%
      #filter(Person %in% input$filter1 & df$Category %in% input$filter2) %>%
      group_by(Category, Person) %>%
      summarise(total_sales = sum(Sales), .groups = 'drop')
  })
  
  # Engineer average quarterly growth metric
  df_grp_time2 <- reactive({
    df_grp_time() %>%
      mutate(total_sales_lag = lag(total_sales)) %>%
      drop_na() %>%
      mutate(Growth = (total_sales - total_sales_lag) / total_sales_lag)
  })
  
  # Calculate average growth
  avg_growth <- reactive({
    avg <- mean(df_grp_time2()$Growth) * 100
    if (as.character(avg) == "NaN") {
      ""
    } else {
      paste0(round(avg, 2), "%")
    }
  })
  
  # Calculate total sales
  total_sales <- reactive({
    total <- sum(filtered_df()$Sales)
    if (total >= 1000000) {
      paste("$", round(total / 1000000, 1), "M", sep = "")
    } else if (total >= 1000) {
      paste("$", round(total / 1000, 1), "K", sep = "")
    } else {
      paste("$", round(total, 1), sep = "")
    }
  })
  
  # calculate average discount rate
  discount_rate <- reactive({
    dr = paste0(round((sum(filtered_df()$Discount * filtered_df()$Sales) / sum(filtered_df()$Sales)) * 100, 2), "%")
    if (dr == "NaN%") {
      dr = ""
    }
    dr
  })
  
  # average discount rate by category
  dr_df <- reactive({
    discount_rate_df = filtered_df() %>% group_by(Category)  %>%
      summarise(discount_rate = (sum(Discount * Sales) / sum(Sales)) * 100,
                .groups = 'drop')
    discount_rate_df = data.frame(discount_rate_df)
    discount_rate_df
  })
  
  # set color for average growth
  value1_color <- reactive({
    if(avg_growth() == "") {
      "red"
    } else if (as.numeric(gsub("%", "", avg_growth())) <= 0) {
      "red"
    } else {
      "green"
    }
  })
  
  # set icon for average growth
  value1_icon <- reactive({
    if (avg_growth() == ""){
      ""
    } else if(value1_color() == "red") {
      "arrow-down"
    } else {
      "arrow-up"
    }
  })
  
  # set color for total sales
  value2_color <- reactive({
    if(total_sales() == "$0") {
      "red"
    } else {
      "green"
    }
  })
  
  # set icon for total sales
  value2_icon <- reactive({
    if(value2_color() == "red") {
      ""
    } else {
      "plus"
    }
  })
  
  # engineer total returns ($) and return rate (%) by category
  return_df <- reactive({
    return_df_grp_cat = filtered_df() %>% group_by(Category)  %>%
      summarise(total_returns = sum(Returned * Quantity), total_returns_dollars = sum(Sales * Returned),
                .groups = 'drop')
    return_df_grp_cat = data.frame(return_df_grp_cat)
    return_df_grp_cat = merge(return_df_grp_cat, data.frame(filtered_df() %>% group_by(Category) %>% summarise(n = sum(Quantity), .groups = 'drop')), by = "Category")
    return_df_grp_cat$return_rate = (return_df_grp_cat$total_returns / return_df_grp_cat$n) * 100
    return_df_grp_cat
  })
  
  # engineer return rate by quarter
  return_ts_df <- reactive({return_q = filtered_df() %>% group_by(Year_Quarter)  %>%
    summarise(total_returns = sum(Returned * Quantity), n = sum(Quantity),
                .groups = 'drop')
    return_q = data.frame(return_q)
    return_q$return_rate = (return_q$total_returns / return_q$n) * 100
    return_q
  })
  
  # engineer number of items by returned or not
  return_df2 <- reactive({
    # aggregations
    return_label_df <- filtered_df() %>% group_by(Returned_Label)  %>%
      summarise(n = sum(Quantity),
                .groups = 'drop')
    return_label_df = data.frame(return_label_df)
    return_label_df$n = return_label_df$n / sum(return_label_df$n)
    
    # Compute the cumulative percentages (top of each rectangle)
    return_label_df$ymax <- cumsum(return_label_df$n)
    
    # Compute the bottom of each rectangle
    return_label_df$ymin <- c(0, head(return_label_df$ymax, n=-1))
    
    # Compute label position
    return_label_df$labelPosition <- (return_label_df$ymax + return_label_df$ymin) / 2
    
    # Compute a good label
    return_label_df$label <- paste0(return_label_df$Returned_Label, "\n", round(return_label_df$n * 100, 2), "%")
    
    return_label_df
  })
  
  # calculate total returns
  total_returns <- reactive({
    x = round(sum((filtered_df() %>% filter(Returned == 1))$Sales), 0)
    if (x >= 1000) {
      x = round(x / 1000, 1)
      x = paste0(x, "K")
    }
    x = paste0("$", x)
    x
  })
  
  # calculate average return rate
  return_rate <- reactive({
    x = paste0(round((sum(filtered_df()$Returned * filtered_df()$Quantity) / sum(filtered_df()$Quantity)) * 100, 2), "%")
    if (x == "NaN%") {
      x = ""
    }
    x
  })
  
  # calculate average change in return rate
  return_rate_delta <- reactive({
    return_q = return_ts_df() %>% mutate(return_rate_lag = lag(return_rate))
    return_q$return_rate_change = return_q$return_rate - return_q$return_rate_lag
    return_q = return_q %>% drop_na()
    return_q = paste0(round(mean(return_q$return_rate_change), 2), "%")
    if (return_q == "NaN%") {
      return_q = ""
    }
    return_q
  })
  
  # set icon for total returns
  value3_icon <- reactive({
    if (total_returns() == "$0"){
      ""
    } else {
      "minus"
    }
  })
  
  # set color for total returns
  value3_color <- reactive({
    if(value3_icon() == "minus") {
      "red"
    } else {
      "green"
    }
  })
  
  # set icon for return rate
  value4_icon <- reactive({
    if (return_rate() == ""){
      ""
    } else {
      "bug"
    }
  })
  
  # set color for return rate
  value4_color <- reactive({
    if(value4_icon() == "bug") {
      "red"
    } else {
      "green"
    }
  })
  
  # set icon for return rate change
  value5_icon <- reactive({
    if (return_rate_delta() == ""){
      ""
    } else if(as.numeric(gsub("%", "", return_rate_delta())) >= 0) {
      "arrow-up"
    } else {
      "arrow-down"
    }
  })
  
  # set color for return rate change
  value5_color <- reactive({
    if(value5_icon() == "arrow-up") {
      "red"
    } else {
      "green"
    }
  })
  
  # profit margin
  profit_margin <- reactive({
    pm = paste0(round((sum(filtered_df()$Profit) / sum(filtered_df()$Sales)) * 100, 2), "%")
    if (pm == "NaN%") {
      pm = ""
    }
    pm
  })
  
  # profit margin quarterly change
  profit_margin_change <- reactive({
    pm_chg <- pm_ts_df() %>% mutate(profit_margin_lag = lag(profit_margin))
    pm_chg <- pm_chg %>% drop_na()
    pm_chg$profit_margin_change <- pm_chg$profit_margin - pm_chg$profit_margin_lag
    pm_chg <- paste0(round(mean(pm_chg$profit_margin_change), 2), "%")
    if (pm_chg == "NaN%") {
      pm_chg <- ""
    }
    pm_chg
  })
  
  # set profit margin quarterly change icon
  profit_margin_change_icon <- reactive({
    if (profit_margin_change() == "") {
      profit_margin_icon <- ""
    } else if (as.numeric(gsub("%", "", profit_margin_change())) < 0) {
      profit_margin_icon <- "arrow-down"
    } else if (as.numeric(gsub("%", "", profit_margin_change())) == 0) {
      profit_margin_icon <- ""
    } else {
      profit_margin_icon <- "arrow-up"
    }
  })
  
  # set profit margin quarterly change color
  profit_margin_change_color <- reactive({
    if (profit_margin_change_icon() == "arrow-up") {
      profit_margin_change_color <- "green"
    } else if (profit_margin_change_icon() == "arrow-down" | profit_margin_change_icon() == "") {
      profit_margin_change_color <- "red"
    }
  })
  
  # profit margin by category
  pm_df <- reactive({
    data.frame(filtered_df() %>% group_by(Category)  %>% summarise(profit_margin = (sum(Profit) / sum(Sales)) * 100,.groups = 'drop'))
  })
  
  # profit margin time series df
  pm_ts_df <- reactive({
    data.frame(filtered_df() %>% group_by(Year_Quarter)  %>% summarise(profit_margin = (sum(Profit) / sum(Sales)) * 100,.groups = 'drop'))
  })
  
  # render plots
  
  # sales dashboard plots
  
  # render time series chart
  output$time_series_plot <- renderPlot({
    
    ggplot(data = df_grp_time(), aes(x = Year_Quarter, y = total_sales, group = 1)) +
      
      # Line and point aesthetics
      geom_line(color = "#0072B2", size = 1.2) +
      geom_point(color = "#0072B2", size = 3, shape = 21, fill = "white") +
      
      # Add trend line (dashed) with the same color as the main line
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#0072B2", linetype = "dashed") +
      
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
    ggplot(df_grp_person(), aes(x = total_sales, y = reorder(Person, total_sales))) +
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
    ggplot(df_grp_cat_person(), aes(fill = reorder(Person, total_sales), y = reorder(Category, total_sales), x=total_sales)) + 
      geom_bar(position="stack", stat="identity", color = NA) +
      labs(title = "Sales by Products",
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
      scale_x_continuous(labels = function(x) paste0("$", scales::label_number_si()(x))) 
  })
  
  # render discount rate bar chart
  output$discount_rate_plot <- renderPlot({
  
    ggplot(dr_df(), aes(x = discount_rate, y = reorder(Category, discount_rate))) +
      geom_bar(stat = "identity", fill = "#0072B2", color = "white") +
      labs(title = "Discount Rate by Products",
           x = "Avg. Discount Rate",
           y = "Product Category") +
      theme_minimal() + theme(plot.background = element_rect(fill = "white", color = "white"),  # Dark grey background box,#f0f0f0
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
                              axis.title = element_text(size = 16, face = "bold"),
                              axis.text.x = element_text(size = 14),
                              axis.text.y = element_text(size = 14),
                              legend.position = "none"
      ) +
      scale_x_continuous(labels = function(x) paste0("%", scales::label_number_si()(x))) 
  })
  
  # info box 1
  output$value1 <- renderInfoBox({
    infoBox(
      "Quarterly Growth (Avg.)",
      avg_growth(),
      icon = icon(value1_icon()),
      color = value1_color()
    )
  })
  
  # info box 2
  output$value2 <- renderInfoBox({
    infoBox(
      "Total Sales",
      total_sales(),
      icon = icon(value2_icon()),
      color = value2_color()
    )
  })
  
  # info box 0
  output$value0 <- renderInfoBox({
    infoBox(
      "Discount Rate (Avg.)",
      discount_rate(),
      icon = icon("tags"),
      color = "blue"
    )
  })
  
  # returns dashboard plots
  
  # render returned rate by category bar chart
  output$returns_rate_bar_plot <- renderPlot({
    
    ggplot(return_df(), aes(x = return_rate, y = reorder(Category, return_rate))) +
    geom_bar(stat = "identity", fill = "#b20019", color = "white") +
    labs(title = "Return Rate by Products",
         x = "Return Rate",
         y = "Product Category") +
    theme_minimal() + theme(plot.background = element_rect(fill = "white", color = "white"),  # Dark grey background box,#f0f0f0
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.border = element_blank(),
                            plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
                            axis.title = element_text(size = 16, face = "bold"),
                            axis.text.x = element_text(size = 14),
                            axis.text.y = element_text(size = 14),
                            legend.position = "none"
    ) +
    scale_x_continuous(labels = function(x) paste0("%", scales::label_number_si()(x)))
  })
  
  # render returned dollars by category bar chart
  output$returns_bar_plot <- renderPlot({
    ggplot(return_df(), aes(x = total_returns_dollars, y = reorder(Category, total_returns_dollars))) +
      geom_bar(stat = "identity", fill = "#b20019", color = "white") +
      labs(title = "Total Returns by Products",
           x = "Returns",
           y = "Product Category") +
      theme_minimal() + theme(plot.background = element_rect(fill = "white", color = "white"),  # Dark grey background box,#f0f0f0
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
  
  # render return rate time series chat
  
  output$returns_ts <- renderPlot({
    
    ggplot(data = return_ts_df(), aes(x = Year_Quarter, y = return_rate, group = 1)) +
      
      # Line and point aesthetics
      geom_line(color = "#b20019", size = 1.2) +
      geom_point(color = "#b20019", size = 3, shape = 21, fill = "white") +
      
      # Add trend line (dashed) with the same color as the main line
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#b20019", linetype = "dashed") +
      
      # Labels and titles
      labs(title = "Quarterly Return Rate",
           x = "Quarter",
           y = "Return Rate") +
      
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
      scale_y_continuous(labels = function(x) paste0("%", label_number_si()(x)))
  })
  
  # render donut chart
  output$returns_donut <- renderPlot({
  
    ggplot(return_df2(), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Returned_Label)) +
      geom_rect() +
      labs(title = "Return Rate") +
      geom_text( x=2, aes(y=labelPosition, label=label, color=Returned_Label), size=6) + # x here controls label position (inner / outer)
      scale_fill_manual(values = c("#008080", "#B20019")) +
      #scale_fill_brewer(palette=3) +
      scale_color_manual(values = c("black", "black")) +
      #scale_color_brewer(palette=3) +
      coord_polar(theta="y") +
      xlim(c(-1, 4)) +
      theme_void() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 24, face = "bold"))
  })
  
  # info box 3
  output$value3 <- renderInfoBox({
    infoBox(
      "Total Returns",
      total_returns(),
      icon = icon(value3_icon()),
      color = value3_color()
    )
  })
  
  # info box 4
  output$value4 <- renderInfoBox({
    infoBox(
      "Return Rate",
      return_rate(),
      icon = icon(value4_icon()),
      color = value4_color()
    )
  })
  
  # info box 5
  output$value5 <- renderInfoBox({
    infoBox(
      "Return Rate Change (Avg.)",
      return_rate_delta(),
      icon = icon(value5_icon()),
      color = value5_color()
    )
  })
  
  # profit dashboard plots
  
  # render profit_margin by category bar chart
  output$pm_plot <- renderPlot({
    
    ggplot(pm_df(), aes(x = profit_margin, y = reorder(Category, profit_margin))) +
      geom_bar(stat = "identity", fill = "#228B22", color = "white") +
      labs(title = "Profit Margin by Products",
           x = "Profit Margin",
           y = "Product Category") +
      theme_minimal() + theme(plot.background = element_rect(fill = "white", color = "white"),  # Dark grey background box,#f0f0f0
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.border = element_blank(),
                              plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
                              axis.title = element_text(size = 16, face = "bold"),
                              axis.text.x = element_text(size = 14),
                              axis.text.y = element_text(size = 14),
                              legend.position = "none"
      ) +
      scale_x_continuous(labels = function(x) paste0("%", scales::label_number_si()(x)))
  })
  
  # render profit margin time series
  output$pm_ts_plot <- renderPlot({
    
    ggplot(data = pm_ts_df(), aes(x = Year_Quarter, y = profit_margin, group = 1)) +
      
      # Line and point aesthetics
      geom_line(color = "#228B22", size = 1.2) +
      geom_point(color = "#228B22", size = 3, shape = 21, fill = "white") +
      
      # Add trend line (dashed) with the same color as the main line
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#228B22", linetype = "dashed") +
      
      # Labels and titles
      labs(title = "Quarterly Profit Margin",
           x = "Quarter",
           y = "Profit Margin") +
      
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
      scale_y_continuous(labels = function(x) paste0("%", label_number_si()(x)))
  })
  
  # info box 6
  output$value6 <- renderInfoBox({
    infoBox(
      "Profit Margin",
      profit_margin(),
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  # info box 7
  output$value7 <- renderInfoBox({
    infoBox(
      "Profit Margin Change",
      profit_margin_change(),
      icon = icon(profit_margin_change_icon()),
      color = profit_margin_change_color()
    )
  })
}

# run app
shinyApp(ui, server)
