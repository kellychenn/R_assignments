library(dplyr)
library(lubridate)

# Load the data
data <- file.choose()
data <- readRDS("/Users/kellychen/Downloads/opioids_toy.rds")
View(data)
# Convert date columns to Date class
data$washout_start_dt <- as.Date(data$washout_start_dt)
data$RX_FILL_DT <- as.Date(data$RX_FILL_DT)

# Function to calculate opioid misuse score for a given window
calculate_misuse_score <- function(data, start_date, end_date) {
  window_data <- data %>%
    filter(RX_FILL_DT >= start_date, RX_FILL_DT < end_date)
  
  distinct_prescribers <- n_distinct(window_data$PRSCRBNG_PRVDR_NPI)
  distinct_dispensers <- n_distinct(window_data$DSPNSNG_PRVDR_NPI)
  days_supply <- sum(window_data$DAYS_SUPPLY)
  
  # Calculate misuse scores based on the provided rules
  prescriber_score <- cut(distinct_prescribers, breaks = c(-Inf, 2, 4, Inf), labels = c(0, 1, 2), include.lowest = TRUE)
  dispenser_score <- cut(distinct_dispensers, breaks = c(-Inf, 2, 4, Inf), labels = c(0, 1, 2), include.lowest = TRUE)
  days_supply_score <- cut(days_supply, breaks = c(-Inf, 185, 240, Inf), labels = c(0, 1, 2), include.lowest = TRUE)
  
  # Sum the individual scores to get the overall misuse score
  total_score <- as.integer(as.character(prescriber_score)) + as.integer(as.character(dispenser_score)) + as.integer(as.character(days_supply_score))
  return(total_score)
}

# Create a data frame to store the results
misuse_scores <- data.frame(id = character(), start_date = lubridate::ymd(character()), end_date = lubridate::ymd(character()), score = numeric())
# Loop through each beneficiary
#unique(data$id)
for (i in unique(data$id)) {
  data_loop <- data[data$id == i,]
  washout_start_date <- data_loop$washout_start_dt[1]
  # Generate three 6-month rolling windows
  for (j in 0:2) {
    start_date <- washout_start_date %m+% months(j)
    end_date <- start_date %m+% months(6)
    
    # Calculate misuse score for the current window
    score <- calculate_misuse_score(data_loop, start_date, end_date)
    
    # Append the results to the misuse_scores data frame
    misuse_scores <- rbind(misuse_scores, data.frame(id = i, start_date = start_date, end_date = end_date, score = score))
  }
}

print(misuse_scores)
