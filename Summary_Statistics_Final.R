# Assuming DRG807 and DRG788 are your dataframes and finalcost is the column of interest

# Function to calculate the summary statistics
# Assuming DRG807 and DRG788 are your dataframes and finalcost is the column of interest

# Function to calculate the summary statistics and format the range as a string
calculate_stats <- function(costs) {
  mean_cost <- mean(costs, na.rm = TRUE)
  median_cost <- median(costs, na.rm = TRUE)
  iqr_cost <- IQR(costs, na.rm = TRUE)
  range_cost <- range(costs, na.rm = TRUE)
  percentile_10 <- quantile(costs, probs = 0.1, na.rm = TRUE)
  percentile_90 <- quantile(costs, probs = 0.9, na.rm = TRUE)
  percentile_range <- paste(formatC(percentile_10, format = "f", digits = 2),
                            "-", 
                            formatC(percentile_90, format = "f", digits = 2))
  
  data.frame(
    Mean = mean_cost,
    Median = median_cost,
    IQR = iqr_cost,
    Range = diff(range_cost),
    `10_90_Percentile_Range` = percentile_range
  )
}

# Calculate stats for Low-Risk Vaginal Delivery (DRG807)
low_risk_vaginal_stats <- calculate_stats(DRG807$finalcost)

# Calculate stats for Low-Risk Cesarean Section Delivery (DRG788)
low_risk_cesarean_stats <- calculate_stats(DRG788$finalcost)

# Combine the stats into one table
summary_stats_table <- rbind(
  `Low-Risk Vaginal Delivery` = low_risk_vaginal_stats,
  `Low-Risk Cesarean Section Delivery` = low_risk_cesarean_stats
)

# Print the table
print(summary_stats_table)


hospital_cesarean_stats <-calculate_stats(average_finalcost_by_dshospid_DRG788$AverageFinalCost)
hospital_vaginal_stats <-calculate_stats(average_finalcost_by_dshospid_DRG807$AverageFinalCost)

hospital_summary_stats_table <- rbind(
  `Low-Risk Vaginal Delivery by Hospital` = hospital_vaginal_stats,
  `Low-Risk Cesarean Section Delivery by Hospital` = hospital_cesarean_stats
)

library(dplyr)

# Function to categorize HHI_value
categorize_hhi <- function(value) {
  case_when(
    value >= 0 & value < 2500    ~ "0-2500",
    value >= 2500 & value < 5000 ~ "2500-5000",
    value >= 5000 & value < 7500 ~ "5000-7500",
    value >= 7500 & value <= 10000 ~ "7500-10000",
    TRUE ~ "Other"
  )
}

# Function to process the data
process_data <- function(dataframe) {
  dataframe %>%
    mutate(HHI_category = categorize_hhi(HHI_value)) %>%
    select(HOSPID, CNTRL, CBSATYPE, MAPP8, HHI_category) %>%
    gather(key = "Variable", value = "Category", -HOSPID) %>%
    group_by(Variable, Category) %>%
    summarise(Count = n_distinct(HOSPID), .groups = "drop")
}

# Apply the function to both dataframes
hospital_sample_788 <- process_data(DRG788)
hospital_sample_807 <- process_data(DRG807)

write.csv(hospital_sample_788, "hospital_sample_788.csv", row.names = FALSE)
write.csv(hospital_sample_807, "hospital_sample_807.csv", row.names = FALSE)
