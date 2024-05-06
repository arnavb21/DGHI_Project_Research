library(dplyr)

#import NC 2019 SID 
library(haven)
NC_SID_2019_CORE <- read_dta("~/Desktop/DGHI Research Spring 2024/NC_SID_2019_CORE.dta")
View(NC_SID_2019_CORE)

#import charlson co-morbidity index (created by Ashwini)
library(haven)
SID_Core_Charlson_Index_1_ <- read_dta("~/Desktop/DGHI Research Spring 2024/SID Core Charlson Index (1).dta")
View(SID_Core_Charlson_Index_1_)

#import Cost-to-Charge ratio data 
library(readr)
cc2019CDSID <- read_csv("~/Desktop/DGHI Research Spring 2024/cc2019CDSID.csv")
View(cc2019CDSID)
NCchargecodes <- cc2019CDSID[cc2019CDSID$`'Z013'` == "'NC'", ]

#import AHA linkage files
library(haven)
NC_2019_American_Hospital_Association_Linkage_Files <- read_dta("~/Desktop/DGHI Research Spring 2024/NC_2019_American Hospital Association Linkage Files.dta")
View(NC_2019_American_Hospital_Association_Linkage_Files)


#import AHA data 
library(readxl)
AS2020FULLFILE <- read_excel("~/Desktop/DGHI Research Spring 2024/AS2020FULLFILE.xlsx")
View(AS2020FULLFILE)



#merge SID and charlson co-morbidity index data together 
SID_Core_Charlson_Index_1_ <- select(SID_Core_Charlson_Index_1_, KEY, charlindex)

final_dataframe <- left_join(NC_SID_2019_CORE, SID_Core_Charlson_Index_1_, by= "KEY")

#Merge SID and AHA Linkage files

final_dataframe <- left_join(final_dataframe, NC_2019_American_Hospital_Association_Linkage_Files, by= 'DSHOSPID')

#merge cost-to-charge data and SID 

NCchargecodes$`'HOSPID'` <- gsub("'", "", NCchargecodes$`'HOSPID'`)

NCchargecodes <- NCchargecodes %>%
       mutate(`'HOSPID'` = as.numeric((`'HOSPID'`)))

final_dataframe <- left_join(final_dataframe, NCchargecodes, by = c("HOSPID" = "'HOSPID'"))

#create variable for final hospital cost using APICC and GAPICC cost-to-charge data 

final_dataframe <- final_dataframe %>%
  mutate(
    finalcost = if_else(
      !is.na(`'APICC'`) & `'APICC'` != ".", 
      as.numeric(`TOTCHG`) * as.numeric(`'APICC'`), 
      as.numeric(`TOTCHG`) * as.numeric(`'GAPICC'`)
    )
  )


# calculate HHI by FIPS County Codes and merge with final_dataframe 

HHI <- final_dataframe %>%
  group_by(HFIPSSTCO, HOSPID) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

HHI$market_proportion_squared = (100 * HHI$freq)^2


HHI_Index_Table <- HHI %>%
  group_by(HFIPSSTCO) %>%
  summarise(HHI_value = sum(market_proportion_squared))

final_dataframe <- left_join(final_dataframe, HHI_Index_Table, by = "HFIPSSTCO" )


# merge dataset with AHA annual survey data

AS2020FULLFILE <- select(AS2020FULLFILE, ID, TRAUML90, SERV, CNTRL, MAPP8, CBSATYPE)

final_dataframe <- left_join(final_dataframe, AS2020FULLFILE, by = c("AHAID" = "ID"))

# turn variables into factor variables 


final_dataframe$CNTRL <- as.factor(final_dataframe$CNTRL)
final_dataframe$TRAUML90 <- as.factor(final_dataframe$TRAUML90)
final_dataframe$CBSATYPE <- as.factor(final_dataframe$CBSATYPE)
final_dataframe$MAPP8 <- as.factor(final_dataframe$MAPP8)



final_dataframe$pay1.f <- as.factor(final_dataframe$PAY1)
final_dataframe$race.f <- as.factor(final_dataframe$RACE)
final_dataframe$MEDINCSTQ.f <- as.factor(final_dataframe$MEDINCSTQ)

# re-level factor variables 

final_dataframe$pay1.f <- relevel(final_dataframe$pay1.f, ref = "3")

final_dataframe$CBSATYPE <- relevel(final_dataframe$CBSATYPE, ref = "Metro")



# remove missing data for any patient level variables 
library(dplyr)
library(tidyr)

# Function to calculate and record the removal of observations with NA values
calculate_removals <- function(data, column) {
  initial_rows <- nrow(data)
  filtered_data <- drop_na(data, {{column}})
  removed_count <- initial_rows - nrow(filtered_data)
  data.frame(Column = deparse(substitute(column)), Observations_Removed = removed_count)
}

# Apply the function to each column and combine the results
removal_summary <- bind_rows(
  calculate_removals(final_dataframe, AGE),
  calculate_removals(final_dataframe, LOS),
  calculate_removals(final_dataframe, pay1.f),
  calculate_removals(final_dataframe, race.f),
  calculate_removals(final_dataframe, MEDINCSTQ.f)
)

#clean  up data by removing NA and those under 18

cleaned_dataframe <- drop_na(final_dataframe, AGE, LOS, pay1.f, race.f, MEDINCSTQ.f)

under_18_count <- cleaned_dataframe %>%
  filter(AGE < 18) %>%
  nrow()


# Remove observations with AGE under 18
cleaned_dataframe <- cleaned_dataframe %>%
  filter(AGE >= 18)



# separate into data-frame with vaginal delivery (DRG807) and c-section (DRG788)

DRG788 <- cleaned_dataframe  %>%
  filter(DRG == 788)

DRG807 <- cleaned_dataframe  %>%
  filter(DRG == 807)

#remove missing HOSPID observations 
# Filter out rows where HOSPID is NA or -9999 from DRG788
DRG788 <- DRG788 %>% 
  filter(!is.na(HOSPID) & HOSPID != -9999)

# Filter out rows where HOSPID is NA or -9999 from DRG807
DRG807 <- DRG807 %>% 
  filter(!is.na(HOSPID) & HOSPID != -9999)


#fit models
library(lme4)

fit788 <- lmer(finalcost ~ LOS + race.f + AGE + MEDINCSTQ.f + pay1.f + charlindex + HHI_value + CNTRL  + CBSATYPE + MAPP8 + (1 | HOSPID), data = DRG788)
fit807 <- lmer(finalcost ~ LOS + race.f + AGE + MEDINCSTQ.f + pay1.f + charlindex + HHI_value + CNTRL  + CBSATYPE + MAPP8 + (1 | HOSPID), data = DRG807)


library(texreg)
Fit788 <- screenreg(fit788)
Fit807 <- screenreg(fit807)


capture.output(Fit788, file = "Fit788.txt")
capture.output(Fit807, file = "Fit807.txt")


#calculate # of low-risk c-section + vaginal deliveries in North Carolina and # of hospitals 
all_low_risk_childbirth <- nrow(NC_SID_2019_CORE[NC_SID_2019_CORE$DRG %in% c(788, 807), ])

unique_hospitals_count <- NC_SID_2019_CORE[NC_SID_2019_CORE$DRG %in% c(788, 807), ] %>%
  .$DSHOSPID %>%
  unique() %>%
  length()


# get regression equation for multi-level model 
# Get fixed effect coefficients
# Summarize the model
summary(Fit807)
coefficients807 <- fixef(Fit807)
print(coefficients807)



# get counts for age group summary stats 
count_age_groups <- function(df) {
  age_counts <- df %>%
    mutate(AgeGroup = case_when(
      AGE >= 18 & AGE <= 34 ~ "18-34",
      AGE > 34 ~ "35+",
      TRUE ~ "Other/Unknown"  # To catch any data that doesn't fit the above categories
    )) %>%
    group_by(AgeGroup) %>%
    summarise(Count = n()) %>%
    ungroup()  # Remove the grouping for further use
  
  return(age_counts)
}


# Apply the function to DRG788
age_counts_DRG788 <- count_age_groups(DRG788)

# Apply the function to DRG807
age_counts_DRG807 <- count_age_groups(DRG807)




# do simliar analysis for LOS 

# Define the function to count LOS groups
count_los_groups <- function(df) {
  los_counts <- df %>%
    mutate(LOSGroup = case_when(
      LOS >= 0 & LOS <= 3 ~ "LOS = 0-3",
      LOS >= 4 & LOS <= 6 ~ "LOS = 4-6",
      LOS >= 7 ~ "LOS = 7 and up",
      TRUE ~ "Other/Unknown"  # To catch any data that doesn't fit the above categories
    )) %>%
    group_by(LOSGroup) %>%
    summarise(Count = n()) %>%
    ungroup()  # Remove the grouping for further use
  
  return(los_counts)
}

# Assuming DRG788 and DRG807 are your dataframes with a LOS column

# Apply the function to DRG788
los_counts_DRG788 <- count_los_groups(DRG788)

# Apply the function to DRG807
los_counts_DRG807 <- count_los_groups(DRG807)


# Define a function to count observations for a given variable
count_variable_observations <- function(df, var_name) {
  counts <- df %>%
    group_by(!!sym(var_name)) %>%
    summarise(Count = n()) %>%
    ungroup()  # Remove the grouping for further use
  
  return(counts)
}

# A wrapper function to apply the counting function to multiple variables
count_all_variables <- function(df) {
  vars_to_count <- c("race.f", "MEDINCSTQ.f", "charlindex", "pay1.f")
  all_counts <- lapply(vars_to_count, function(var) {
    count_variable_observations(df, var)
  })
  
  # Name the list elements for clarity
  names(all_counts) <- vars_to_count
  
  return(all_counts)
}

# Assuming DRG788 and DRG807 are your dataframes

# Apply the function to DRG788
counts_DRG788 <- count_all_variables(DRG788)

# Apply the function to DRG807
counts_DRG807 <- count_all_variables(DRG807)

# Print the results
print("Counts for DRG788:")
print(counts_DRG788)
print("Counts for DRG807:")
print(counts_DRG807)












# Calculate the average finalcost for each DSHOSPID in DRG807
average_finalcost_by_dshospid_DRG807 <- DRG807 %>%
  group_by(HOSPID) %>%
  summarise(AverageFinalCost = mean(finalcost, na.rm = TRUE)) %>%
  ungroup()  # Remove the grouping for further use

# Calculate the average finalcost for each DSHOSPID in DRG788
average_finalcost_by_dshospid_DRG788 <- DRG788 %>%
  group_by(HOSPID) %>%
  summarise(AverageFinalCost = mean(finalcost, na.rm = TRUE)) %>%
  ungroup()  # Remove the grouping for further use

# Print the results
print("Average finalcost by DSHOSPID for DRG807:")
print(average_finalcost_by_dshospid_DRG807)
print("Average finalcost by DSHOSPID for DRG788:")
print(average_finalcost_by_dshospid_DRG788)




find_missing_values <- function(df) {
  missing_dshospid <- df %>%
    filter(is.na(CNTRL) | is.na(CBSATYPE) | is.na(MAPP8)) %>%
    select(HOSPID) %>%
    distinct()
  
  return(missing_dshospid)
}

# Find missing values in DRG807
missing_values_DRG807 <- find_missing_values(DRG807)
# Find missing values in DRG788
missing_values_DRG788 <- find_missing_values(DRG788)

print(missing_values_DRG807)
print(missing_values_DRG788)


hospi_levels <- levels(fit807@flist$HOSPID)
print(hospi_levels)


