# Lim Wei Lun - TP069058
# Goh Xin Tong - TP069712
# Chang Qi Eue - TP065956
# Lee Jun Zhe - TP065879

#####################################################################################################
### Import Necessary Packages ###
#####################################################################################################

if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")
if (!requireNamespace("mice", quietly = TRUE)) install.packages("mice")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("countrycode", quietly = TRUE)) install.packages("countrycode")
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")

library(readxl)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)
library(lubridate)
library(reshape2)
library(mice)
library(scales)
library(tidyr)
library(stringr)
library(countrycode)
library(purrr)
library(randomForest)
library(RColorBrewer)


################################################################################
### DATA IMPORT ###
################################################################################

FILE_PATH <- "C:\\Users\\user\\New folder (2)\\OneDrive - Asia Pacific University\\APU Degree\\YEAR2 SEM1\\PFDA\\PFDA Assigment\\4.hackingdata.csv"

# data populated with csv data
data <- read.csv(FILE_PATH, stringsAsFactors = FALSE)

################################################################################
### DATA CLEANING ###
################################################################################


#' Clean Invalid Values in Dataset
#'
#' @param data DataFrame: The dataset to clean.
#' @return DataFrame: The cleaned dataset where 'unknown', 'empty' and 'null' values are replaced with NA.
#'
#' @details
#' This function replaces 'unknown', 'Unkno', 'null', and empty string ("") values with NA across all columns.
#' 
cleanInvalidValues <- function(data) {
  data <- data %>%
    # Convert column names to lowercase
    rename_with(tolower) %>%

    # Replace invalid values ("Unknown", "null", "Unkno", "") with NA
    mutate(across(everything(), ~ case_when(
      str_detect(as.character(.), "(?i)^Unkno|^null|^unknown$") | as.character(.) == "" ~ NA,
      TRUE ~ .
    )))
  
  return(data)
}
data <- cleanInvalidValues(data)


#' remove duplicated row
data <- data[!duplicated(data), ]

############################
### DATA CLEANING - DATE ###
############################

# check how many column of date is empty
# No column is empty
print("Missing Data - Date")
sum(is.na(data$date)) / nrow(data)

#' Detect and identify distinct date formats in a given vector
#' 
#' @param date_vector A vector of character strings containing potential date entries in various formats.
#' 
#' @return A vector of distinct date formats found within the input date_vector.
#'         The formats are identified using regular expressions and may include:
#'         - "yyyy-mm-dd": Dates in the format of year-month-day (e.g., "2025-02-06").
#'         - "yyyy dd mm": Dates in the format of year day month (e.g., "2025 06 02").
#'         - "yyyy": Dates containing only the year (e.g., "2025").
#'         - "dd/mm/yyyy": Dates in the format of day/month/year (e.g., "06/02/2025").
#'         - "dd-mm-yyyy": Dates in the format of day-month-year (e.g., "06-02-2025").
#'         - "yyyy-mm": Dates in the format of year-month (e.g., "2025-02").
#'         - "Invalid format": For any entries that don't match any of the specified formats.
#'
#' @description
#' This function checks each element in the input vector against a set of predefined regular
#' expressions that match common date formats. It returns the unique formats found in the vector.
#' If a value does not match any recognized date format, it is flagged as "Invalid format".
#' 
detect_date_formats <- function(date_vector) {
  formats <- sapply(date_vector, function(x) {
    # Check different formats using regular expressions
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
      return("yyyy-mm-dd")
    } else if (grepl("^\\d{4} \\d{2} \\d{2}$", x)) {
      return("yyyy dd mm")
    } else if (grepl("^\\d{4}$", x)) {
      return("yyyy")
    } else if (grepl("^\\d{2}/\\d{2}/\\d{4}$", x)) {
      return("dd/mm/yyyy")
    } else if (grepl("^\\d{2}-\\d{2}-\\d{4}$", x)) {
      return("dd-mm-yyyy")
    } else if (grepl("^\\d{4}-\\d{2}$", x)) {
      return("yyyy-mm")
    } else {
      return("Invalid format")
    }
  })
  
  # Return distinct formats
  return(unique(formats))
}

# print all the available date format in date column 
# print results: yyyy-mm-dd
print("Original Date Format")
print(detect_date_formats(data$date))


#' Parse date column and extract the year
#' 
#' @param data DataFrame: The dataset containing a 'date' column with varying date formats.
#' 
#' @description
#' The code first uses the `parse_date_time` function from the `lubridate` package to
#' parse the 'date' column in the dataset into a consistent Date format. It supports
#' multiple date formats, including day-month-year (dmy), month-day-year (mdy),
#' year-month-day (ymd), and a format that includes day, month as a word, and year (b d Y).
#' After parsing the dates, it then extracts the year from each date and stores it in a new
#' column called 'year', which is converted to a numeric type for further analysis.
data$date <- parse_date_time(data$date, orders = c("dmy", "mdy", "ymd", "b d Y"))
data$year <- as.numeric(format(as.Date(data$date), "%Y"))

# check the cleaned data
table(data$year)

##########################
### Data CLeaning - OS ###
##########################

# Chech how many OS is NA
# Since only 0.046 of data is missing, therefore, it is safe to just disable it
print("Missing data - OS")
sum(is.na(data$os)) / nrow(data)

#' Group OS into Category 
#' 
#' @param data DataFrame: The dataset to be categorized
#' @return DataFrame: The categorized dataset.
#' 
#' @details
#' The "OS" field contains entries for different versions of the same family of 
#' operating systems (e.g., various versions of Windows, Linux distributions, 
#' etc.). It is more efficient and meaningful to group these versions together 
#' under broader categories (e.g., "Windows", "Linux", "MacOS") for better 
#' analysis, reporting, and visualization. This will help in simplifying the 
#' dataset by reducing redundancy, enhancing interpretability, and ensuring 
#' consistency when analyzing trends or patterns across similar OS families.
#' 
categorize_os <- function(data) {
  data$os_category <- case_when(
    grepl("Windows|Win|Microsoft", data$os, ignore.case = TRUE) & !grepl("Phone|Mobile", data$os, ignore.case = TRUE) ~ "Windows",
    grepl("Linux|Ubuntu|Debian|Red Hat|CentOS|Fedora|Mint", data$os, ignore.case = TRUE) ~ "Linux",
    grepl("BSD|FreeBSD|OpenBSD|NetBSD|Tru64|AIX|Solaris|HP-UX|IRIX|Unix", data$os, ignore.case = TRUE) ~ "Unix",
    grepl("MacOS|OS X", data$os, ignore.case = TRUE) ~ "MacOS",
    grepl("Android|iOS|Windows Phone|iPXE|F5|Cisco|Juniper|Netgear|HP", data$os, ignore.case = TRUE) ~ "Embedded",
    TRUE ~ "Others"
  )
  return(data)
}

data <- categorize_os(data)

# Check the cleaned data
table(data$os_category)

##################################
### DATA CLEANING - Web Server ###
##################################

#' Clean and Standardize Web Server Names
#'
#' This function takes in a vector of web server names (including versions or additional information) 
#' and standardizes them by removing extra details and grouping similar servers under a common name.
#'
#' @param webserver Character vector: A vector of web server names, which may include versions and other extra information.
#' 
#' @return Character vector: A vector of standardized web server names, where each entry is grouped under a common category 
#' like "IIS", "nginx", "Apache", etc. If no match is found, the function returns `NA` for that entry.
#'
clean_webserver <- function(webserver) {
  # Remove versions and extra information after '/'
  cleaned <- str_replace_all(webserver, "\\/.*", "")  # Remove everything after '/'
  
  # Standardize common web servers by grouping them into broader categories
  cleaned <- case_when(
    str_detect(cleaned, "IIS") ~ "IIS",
    str_detect(cleaned, "nginx") ~ "nginx",
    str_detect(cleaned, "Apache") ~ "Apache",
    str_detect(cleaned, "lighttpd") ~ "lighttpd",
    str_detect(cleaned, "Zeus") ~ "Zeus",
    str_detect(cleaned, "Tomahawk") ~ "Tomahawk",
    str_detect(cleaned, "Oracle") ~ "Oracle",
    str_detect(cleaned, "Sun") ~ "Sun",
    str_detect(cleaned, "Microsoft") ~ "Microsoft",
    str_detect(cleaned, "Squid") ~ "Squid",
    str_detect(cleaned, "Varnish") ~ "Varnish",
    TRUE ~ NA
  )
  
  return(cleaned)  # Return the cleaned and standardized web server names
}

# Apply cleaning function to the 'webserver' column
data <- data %>%
  mutate(webserver_cleaned = clean_webserver(webserver))

# Check the distribution of the cleaned webserver data
table(data$webserver_cleaned)


###############################
### DATA CLEANING - Country ###
###############################

# check how many row of contry is NA 
# 0.3175 data is missing 
print("Missing Data - Country")
sum(is.na(data$country)) / nrow(data)

#' Standardize country names
#'
#' @param country Character: The country name to be standardized.
#' @param standard_list DataFrame: A DataFrame containing the standard country names.
#' @return Character: The standardized country name.
#' 
standardize_country <- function(country, standard_list) {
  if (is.na(country)) return(NA_character_)  # Return NA if country is missing
  
  # Use Jaro-Winkler distance to find the best match from standard list
  distances <- stringdist::stringdist(tolower(country), tolower(standard_list$country.name.en), method = "jw")
  
  # Find the best match
  best_match <- standard_list$country.name.en[which.min(distances)]
  
  return(best_match) # Return the standardized country name
}

# Prepare standardized country list from countrycode package
standard_countries <- countrycode::codelist %>% 
  select(country.name.en) %>% 
  distinct() %>% 
  filter(!is.na(country.name.en))

# Correct misspelled/abbreviated/differently cased country names in 'data' and convert to lowercase
data <- data %>%
  mutate(
    country_cleaned = map_chr(country, ~ standardize_country(.x, standard_countries)),  # Apply function
  )

table(data$country_cleaned)


#' Since 31.7% of the data is missing, removing these rows and conduct analysis
#' directly might cause inaccurate the data, therefore, we choose to use 
#' machine learning to learn the pattern and fill in the data 
sum(is.na(data$country_cleaned)) / count(data)

# --------------------------------------------------------------------------------
# Step 1: Convert categorical variables to factors
# --------------------------------------------------------------------------------

data$os_category <- as.factor(data$os_category)
data$country_cleaned <- as.factor(data$country_cleaned)

# --------------------------------------------------------------------------------
# Step 2: Create a continent column from country_cleaned using countrycode
#         For rows with a known country, countrycode returns the corresponding continent.
# --------------------------------------------------------------------------------

data$continent <- countrycode(data$country_cleaned,
                              origin = "country.name",
                              destination = "continent")
table(data$continent)

data$continent <- as.factor(data$continent)

# --------------------------------------------------------------------------------
# Step 3: Split the data into training (rows with known continent) and 
#         testing (rows with missing continent) datasets.
# --------------------------------------------------------------------------------

train_data <- data %>% filter(!is.na(continent))
test_data <- data %>% filter(is.na(continent))

# --------------------------------------------------------------------------------
# Step 4: Train a Random Forest model to predict the missing continent values.
#         Since continent is now a factor, Random Forest will perform classification.
# --------------------------------------------------------------------------------

if (nrow(test_data) > 0) {
  model <- randomForest(continent ~ downtime + year + os_category,
                        data = train_data,
                        ntree = 100,
                        importance = TRUE)
  
  # Predict continent for rows with missing continent
  test_data$continent <- predict(model, test_data)
  
  # Replace the missing continent values in the original dataset with the predictions
  data$continent[is.na(data$continent)] <- test_data$continent
}

table(data$continent)

#####################################################################################################
### Data Validation ###
#####################################################################################################

################
### Downtime ###
################

# ensure downtime is numeric 
data$downtime <- as.numeric(data$downtime)

# remove all the outlier in downtime
# Calculate the IQR for downtime
Q1 <- quantile(data$downtime, 0.25, na.rm = TRUE)  # First quartile
Q3 <- quantile(data$downtime, 0.75, na.rm = TRUE)  # Third quartile
IQR_value <- Q3 - Q1  # IQR

# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter data to remove outliers in the downtime column
data_no_outliers <- data %>%
  filter(downtime >= lower_bound & downtime <= upper_bound)

# View the cleaned data without outliers
print(data_no_outliers)

cat("Number of rows removed: ", nrow(data) - nrow(data_no_outliers))
# No outlier in downtime, so nothing need to be removed. 











#####################################################################################################
### Analysis ###
#####################################################################################################

##############################
### Lim Wei Lun - TP069058 ###
##############################

############
### RQ 1 ###
############
#' LIM WEI LUN - TP069058
#' RQ 1
#' Which OS category contributes the most to the frequency of downtime?

# Step 1: Count the number of incidents per year for each OS category
incident_frequency_per_year_os <- data %>%
  group_by(os_category, year) %>%
  summarize(incident_count = n(), .groups = "drop")

# Step 2: Calculate the average incident count across all years for each OS category
avg_incidents_os <- incident_frequency_per_year_os %>%
  group_by(os_category) %>%
  summarize(avg_incident_count = mean(incident_count, na.rm = TRUE), .groups = "drop")

# Step 3: Create the bar plot with the average incident count for each OS category
ggplot(avg_incidents_os, aes(x = reorder(os_category, -avg_incident_count), 
                             y = avg_incident_count, 
                             fill = os_category)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Fixed syntax error in width parameter
  labs(
    title = "Average Incident Count Across Years by OS Category",
    x = "Operating System Category",
    y = "Average Incident Count"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "darkblue"),
    axis.text.y = element_text(face = "bold", color = "darkred"),
    plot.title = element_text(face = "bold", hjust = 0.5, color = "purple"),
    legend.position = "none"
  ) +
  geom_text(aes(label = round(avg_incident_count, 1)), 
            vjust = -0.5, 
            size = 5, 
            fontface = "bold", 
            color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # Add space for top labels

############
### RQ 2 ###
############
#' LIM WEI LUN - TP069058
#' RQ 2
#' What is the trend in average system downtime by year for each operating system category?

# Step 1: Filter out "Unknown" OS category and calculate average downtime per year per OS
avg_data <- data %>%
  filter(os_category != "Unknown") %>%
  group_by(year, os_category) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = 'drop')

# Step 2: Calculate the correlation between year and average downtime for each OS category
correlation_by_os <- avg_data %>%
  group_by(os_category) %>%
  summarise(correlation = cor(year, avg_downtime, use = "complete.obs"))

# Step 3: Create scatter plot with linear regression line & connected dots
ggplot(avg_data, aes(x = year, y = avg_downtime, color = os_category, group = os_category)) +
  geom_point(alpha = 0.7, size = 3) +  # Scatter plot points
  geom_line(alpha = 0.6, size = 1) +  # Connect the dots with a line
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +  # Add linear regression line
  facet_wrap(~ os_category, scales = "free") +  # Facet by OS category
  labs(title = "Visualization of Average Downtime Trends per OS Category",
       x = "Year",
       y = "Average Downtime (hours)",
       color = "Operating System Category") +
  theme_minimal() +
  theme(legend.position = "bottom")

############
### RQ 3 ###
############
#' LIM WEI LUN - TP069058
#' RQ 3
#' Which operating system is recommended based on the highest score, where the 
#' score is calculated by having the lowest average downtime 
#' and the least occurrences of the highest downtime?

# Step 1 & 2: Group data by year & os_category, calculate avg downtime, 
# and identify highest and lowest downtime for each year
avg_downtime_per_os <- data %>%
  filter(!is.na(os_category)) %>%
  group_by(year, os_category) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = 'drop')

# Identify highest and lowest downtime by year in one step
highest_downtime_per_year <- avg_downtime_per_os %>%
  group_by(year) %>%
  filter(avg_downtime == max(avg_downtime)) %>%
  ungroup()

lowest_downtime_per_year <- avg_downtime_per_os %>%
  group_by(year) %>%
  filter(avg_downtime == min(avg_downtime)) %>%
  ungroup()

# Step 3: Count occurrences for highest and lowest downtime and calculate score
combined_os_count <- full_join(
  highest_downtime_per_year %>% group_by(os_category) %>% summarise(highest_count = n(), .groups = 'drop'),
  lowest_downtime_per_year %>% group_by(os_category) %>% summarise(lowest_count = n(), .groups = 'drop'),
  by = "os_category"
) %>%
  replace_na(list(highest_count = 0, lowest_count = 0)) %>%
  mutate(score = lowest_count - highest_count)

# Step 4: Reshape data for stacked bar chart
combined_os_count_long <- combined_os_count %>%
  gather(key = "downtime_type", value = "count", highest_count, lowest_count)

# Step 5: Create the plot with horizontal stacked bars, and only display the score once
ggplot(combined_os_count_long, aes(x = os_category, y = count, fill = downtime_type)) +
  geom_bar(stat = "identity") +  # Stacked bars
  coord_flip() +  # Horizontal bars
  scale_fill_manual(values = c("highest_count" = "#E74C3C", "lowest_count" = "#27AE60")) +  # Colors
  geom_text(aes(label = ifelse(downtime_type == "lowest_count", paste("Score:", score), "")), 
            position = position_stack(vjust = 0.5), color = "black", size = 5, hjust = -0.1) +  # Only Score label for lowest downtime
  labs(
    title = "Highest and Lowest Downtime for OS Categories with Scores", 
    x = "OS Category", 
    y = "Count of Downtime", 
    fill = "Downtime Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12, color = "darkblue"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "darkred"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(size = 12, color = "darkgreen")
  )

############
### RQ 4 ###
############
#' LIM WEI LUN - TP069058
#' RQ 4
#' Which operating system category showed the most improvement in downtime frequency over the years?

# Step 1: Remove rows with missing or "Unknown" values
clean_data <- data %>%
  filter(!is.na(year), !is.na(os_category), !is.na(downtime), year != "Unknown", os_category != "Unknown", downtime != "Unknown")

# Step 2: Calculate the average downtime for each operating system by year
avg_downtime_by_os <- clean_data %>%
  filter(os_category != "Unknown") %>%
  group_by(year, os_category) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = 'drop')

# Step 3: Calculate the change in downtime from the first to the last year
downtime_diff <- avg_downtime_by_os %>%
  group_by(os_category) %>%
  summarise(
    first_year_downtime = avg_downtime[which.min(year)],  # Downtime in the earliest year
    last_year_downtime = avg_downtime[which.max(year)],   # Downtime in the latest year
    downtime_change = last_year_downtime - first_year_downtime
  )

# Step 4: Sort the data by the magnitude of the downtime change, focusing on the most significant improvement
downtime_diff_sorted <- downtime_diff %>%
  arrange(desc(downtime_change))

# Step 5: Create a bar chart of downtime changes for each operating system
ggplot(downtime_diff_sorted, aes(x = reorder(os_category, downtime_change), y = downtime_change, fill = downtime_change > 0)) +
  geom_bar(stat = "identity", width = 0.7) +  # Bar chart
  labs(title = "Change in Average System Downtime Across Years by OS Category",
       x = "Operating System Category",
       y = "Change in Average Downtime (Days)",
       fill = "Legend") +  # Adjust legend title 
  scale_fill_manual(values = c("FALSE" = "#27AE60", "TRUE" = "#E74C3C"), labels = c("TRUE" = "Setback", "FALSE" = "Improve")) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

############
### RQ 5 ###
############
#' LIM WEI LUN - TP069058
#' RQ 5
#' Which OS category had the shortest recovery time?

# Step 1: Count occurrences per OS category
os_downtime_summary <- data %>%
  group_by(os_category) %>%
  summarise(
    total_downtime = sum(downtime, na.rm = TRUE), # Sum total downtime
    occurrences = n(),  # Count occurrences
    avg_downtime_per_occurrence = total_downtime / occurrences  # Calculate average downtime per incident
  ) %>%
  arrange(avg_downtime_per_occurrence)  # Sort by lowest downtime

# Step 2: Visualize with a bar chart including value labels
ggplot(os_downtime_summary, aes(x = reorder(os_category, avg_downtime_per_occurrence), 
                                y = avg_downtime_per_occurrence, fill = os_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avg_downtime_per_occurrence, 2)),  # Add labels with 2 decimal places
            hjust = -0.2, size = 4) +  # Adjust text position and size
  coord_flip() +  # Flip for readability
  labs(title = "Average Downtime per Occurrence by OS Category",
       x = "OS Category",
       y = "Average Downtime (Days)") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend since colors are categorical



###############################################################################
###############################################################################

###############################
### Goh Xin Tong - TP069712 ###
###############################

############
### RQ 1 ###
############
#' Goh Xin Tong - TP069712
#' RQ 1
#' Which continents show the greatest reduction in downtime between the first and last years?

# Step 1: Calculate the first and last years, and average downtime for each continent
result <- data %>%
  group_by(continent) %>%
  summarize(
    first_year = min(year),
    last_year = max(year),
    avg_downtime_first = mean(downtime[year == min(year)], na.rm = TRUE),
    avg_downtime_last = mean(downtime[year == max(year)], na.rm = TRUE)
  ) %>%
  mutate(difference = avg_downtime_last - avg_downtime_first)

# Step 2: Create a bar plot to visualize the difference in average downtime
ggplot(result, aes(x = continent, y = difference, fill = continent)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Difference in Average Downtime Between First and Last Years",
    x = "Continent",
    y = "Downtime Difference (Last Year - First Year)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############
### RQ 2 ###
############
#' Goh Xin Tong - TP069712
#' RQ 2
#' Do certain years show a more significant increase in downtime incidents across continents?

# Step 1: Create a summary table that counts downtime incidents for each continent and year
downtime_trends <- data %>%
  count(continent, year)  # Counting incidents per continent-year combination

# Step 2: Create a line plot to show trends in downtime incidents over the years for each continent
ggplot(downtime_trends, aes(x = year, y = n, color = continent, group = continent)) +
  geom_line(size = 1) +  # Line plot to show the trend
  geom_point(size = 3) +  # Add points for each year to make it clearer
  labs(
    title = "Trends in Downtime Incidents Over the Years by Continent",
    x = "Year",
    y = "Number of Downtime Incidents",
    color = "Continent"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

###########################
# Step 1: Create a summary table that counts downtime incidents for each continent and year
downtime_summary <- data %>%
  count(continent, year)  # Counting incidents per continent-year combination

# Step 2: Create a stacked bar chart to show the distribution of downtime incidents across continents and years
ggplot(downtime_summary, aes(x = year, y = n, fill = continent)) +
  geom_bar(stat = "identity") +  # Stack bars for each year
  labs(
    title = "Distribution of Downtime Incidents Across Years by Continent",
    x = "Year",
    y = "Number of Downtime Incidents",
    fill = "Continent"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
############################


############
### RQ 3 ###
############
#' Goh Xin Tong - TP069712
#' RQ 3
#' Which continent experienced the highest number of incidents in any specific year?

# Step 1: Calculate the average downtime for each year and continent
avg_downtime_per_year <- data %>%
  group_by(continent, year) %>%
  summarize(avg_downtime = mean(downtime, na.rm = TRUE))

# Step 2: Create the facet line plot with correlation line (regression line)
ggplot(avg_downtime_per_year, aes(x = year, y = avg_downtime, color = continent)) +
  geom_line(size = 1.2) +  # Thicker line for better visibility
  geom_point(size = 3, shape = 21, fill = "white", color = "black") +  # Add points for visibility
  geom_smooth(method = "lm", aes(group = continent), se = FALSE, linetype = "dashed", size = 1) +  # Linear regression line
  facet_wrap(~ continent, scales = "free_y") +  # Facet by continent, free y-axis for each continent
  scale_color_brewer(palette = "Set1") +  # Custom color palette from RColorBrewer
  labs(
    title = "Average Downtime Trends with Correlation Line Across Continents and Years",
    subtitle = "A visualization of the trend in downtime incidents over the years for each continent",
    x = "Year",
    y = "Average Downtime",
    caption = "Source: Your Dataset"
  ) +
  theme_minimal(base_size = 15) +  # Minimal theme with larger base font size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.text = element_text(size = 12),  # Increase font size for axis text
    axis.title = element_text(size = 14),  # Increase font size for axis titles
    plot.title = element_text(size = 18, face = "bold"),  # Larger title with bold
    plot.subtitle = element_text(size = 14, face = "italic"),  # Subtitle in italic
    plot.caption = element_text(size = 12, color = "gray")  # Smaller caption
  )

############
### RQ 4 ###
############
#' Goh Xin Tong - TP069712
#' RQ 4
#' Which continent experienced the highest number of incidents in any specific year?

# Step 1: Count the number of incidents per year for each continent
incident_frequency_per_year_continent <- data %>%
  group_by(continent, year) %>%
  summarize(incident_count = n(), .groups = "drop")

# Step 2: Calculate the average incident count across all years for each continent
avg_incidents_continent <- incident_frequency_per_year_continent %>%
  group_by(continent) %>%
  summarize(avg_incident_count = mean(incident_count, na.rm = TRUE), .groups = "drop")

# Step 3: Create the bar plot with the average incident count for each continent
ggplot(avg_incidents_continent, aes(x = continent, y = avg_incident_count, fill = continent)) +
  geom_bar(stat = "identity", width = 0.7) +  # Bar plot for average incident count
  labs(
    title = "Average Incident Count Across Years by Continent",
    x = "Continent",
    y = "Average Incident Count"
  ) +
  theme_minimal(base_size = 15) +  # Minimal theme with larger font size
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


###############################################################################
###############################################################################


write.csv(data, "output.csv", row.names = FALSE)


#######################################