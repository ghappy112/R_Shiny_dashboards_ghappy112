library(data.table)
library(tidyr)
library(dplyr)

# load data
df <- fread(
  "accepted_2007_to_2018Q4.csv", 
  select = c("loan_status", "issue_d", "int_rate", "grade", "dti", "dti_joint", "last_fico_range_low", "last_fico_range_high")
)


# filtering and feature engineering

# filter out current loans
df <- df[df$loan_status != "Current"]

# filter out fico low ranges of 0
df <- df[df$last_fico_range_low != 0]

# engineer fico score feature
df$fico <- (df$last_fico_range_low + df$last_fico_range_high) / 2

# fn for engineering dti feature
get_dti <- function(dti, dti_joint) {
  if (is.null(dti_joint) || is.na(dti_joint)){
    return(dti)
  } else {
    return(dti_joint)
  }
}

# engineer dti feature
df$debt_to_income <- Map(get_dti, df$dti, df$dti_joint)

# fn for engineering date feature
get_date <- function(date) {
  return(substr(date, 5, 8))
}

# engineer date feature
df$date <- Map(get_date, df$issue_d)

# fn for engineering default feature
get_default <- function(loan_status) {
  if (loan_status == "Default" || loan_status == "Charged Off" || loan_status == "Does not meet the credit policy. Status:Charged Off") {
    return(1)
  } else {
    return(0)
  }
}

# engineer default feature
df$default <- Map(get_default, df$loan_status)

# fn for engineering default label
get_label <- function(default) {
  if (default == 1) {
    return("Default")
  } else {
    return("Fully Paid")
  }
}

# engineer default label feature
df$default_label <- Map(get_label, df$default)

# select relevant features
df <- subset(df, select = c(date, grade, debt_to_income, fico, int_rate, default, default_label))

# convert list of lists columns to vectors
df$default <- sapply(df$default, function(x) x[[1]])
df$debt_to_income <- sapply(df$debt_to_income, function(x) x[[1]])
df$default_label <- sapply(df$default_label, function(x) x[[1]])
df$date <- sapply(df$date, function(x) x[[1]])

# drop missing values
df <- df %>% drop_na()
#df

# save as csv for shiny app
write.csv(df, file = "loan_data.csv")
