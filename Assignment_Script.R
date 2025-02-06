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


#' Since 30% of the data is missing, removing these rows and conduct analysis
#' directly might cause inaccurate the data, therefore, we choose to use 
#' machine learning to learn the pattern and fill in the data 

# Convert categorical variables to factors
data$os_category <- as.factor(data$os_category)
data$country_cleaned <- as.factor(data$country_cleaned)

# Split dataset into known and unknown country_cleaned
train_data <- data %>% filter(!is.na(country_cleaned) & !is.na(os_category))  # Filter out rows with NA os_category
test_data <- data %>% filter(is.na(country_cleaned))

# Train a random forest model
model <- randomForest(country_cleaned ~ downtime + year + os_category, data = train_data, ntree = 100, importance = TRUE)

# Predict missing country_cleaned values
test_data$country_cleaned <- predict(model, test_data)

# Combine back the dataset
data$country_cleaned[is.na(data$country_cleaned)] <- test_data$country_cleaned

# View the updated dataset
print(data)

# Group the data into continent
data$continent <- countrycode(data$country_cleaned, origin = "country.name", destination = "continent")

# View the updated dataset with the continent column
print(data)

# Optionally, group by continent and summarize if needed
continent_summary <- data %>%
  group_by(continent) %>%
  summarize(count = n())

# View continent summary
print(continent_summary)

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
#' RQ 2
#' Which OS category contributes the most to the frequency of downtime?

# Step 1: Count the frequency of each OS category
os_frequency <- data %>%
  group_by(os_category) %>%
  summarise(frequency = n())

# Step 2: Create a pie chart for OS frequency
ggplot(os_frequency, aes(x = "", y = frequency, fill = os_category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  
  labs(title = "Pie Chart of OS Categories Contributing to Downtime Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_blank())  # Hide axis labels


############
### RQ 2 ###
############
#' LIM WEI LUN - TP069058
#' RQ 2
#' What is the trend in average system downtime by year for each operating system category?

# Calculate average downtime by year and OS category
avg_data <- data %>%
  filter(os_category != "Unknown") %>%
  group_by(year, os_category) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = 'drop')
# Calculate the correlation between year and average downtime for each OS category
correlation_by_os <- avg_data %>%
  group_by(os_category) %>%
  summarise(correlation = cor(year, avg_downtime, use = "complete.obs"))

# Create scatter plot with linear regression line
ggplot(avg_data, aes(x = year, y = avg_downtime, color = os_category)) +
  geom_point(alpha = 0.7, size = 3) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ os_category, scales = "free") +  # Facet by OS category
  labs(title = "Average System Downtime by Year and OS Category",
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
#' Which operating system has the most years where it experienced the lowest average downtime?

# Step 1: Group the data by year and os_category to calculate average downtime
avg_downtime_per_os <- data %>%
  filter(!is.na(data$os_category)) %>%
  group_by(year, os_category) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = 'drop')

# Step 2: Identify the OS with the highest downtime for each year
highest_downtime_per_year <- avg_downtime_per_os %>%
  group_by(year) %>%
  filter(avg_downtime == max(avg_downtime)) %>%
  ungroup()

# Step 3: Count how many times each OS category had the highest downtime
os_count <- highest_downtime_per_year %>%
  group_by(os_category) %>%
  summarise(count = n(), .groups = 'drop')

# Step 4: Generate the pie chart with count labels
ggplot(os_count, aes(x = "", y = count, fill = os_category)) +
  geom_bar(stat = "identity", width = 1) +  # Bar chart to mimic pie
  coord_polar(theta = "y") +  # Convert to polar coordinates to make it a pie chart
  labs(title = "Count of Highest Downtime OS Categories Across Years", fill = "OS Category") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.title.x = element_blank(),  # Remove x-axis title
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  geom_text(aes(label = paste(count)), position = position_stack(vjust = 0.5), color = "white")  # Display counts


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
       y = "Change in Average Downtime (hours)",
       fill = "Legend") +  # Adjust legend title 
  scale_fill_manual(values = c("FALSE" = "#27AE60", "TRUE" = "#E74C3C"), labels = c("TRUE" = "Setback", "FALSE" = "Improvement")) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

############
### RQ 5 ###
############
#' LIM WEI LUN - TP069058
#' RQ 5
#' Which OS category had the shortest recovery time

# Step 1: Count occurrences per OS category
os_downtime_summary <- data %>%
  group_by(os_category) %>%
  summarise(
    total_downtime = sum(downtime, na.rm = TRUE), # Sum total downtime
    occurrences = n(),  # Count occurrences
    avg_downtime_per_occurrence = total_downtime / occurrences  # Calculate average downtime per incident
  ) %>%
  arrange(avg_downtime_per_occurrence)  # Sort by lowest downtime

# Step 2: Visualize with a bar chart
ggplot(os_downtime_summary, aes(x = reorder(os_category, avg_downtime_per_occurrence), 
                                y = avg_downtime_per_occurrence, fill = os_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip for readability
  labs(title = "Average Downtime per Occurrence by OS Category",
       x = "OS Category",
       y = "Average Downtime (Days)") +
  theme_minimal()



###############################################################################
###############################################################################

###############################
### Goh Xin Tong - TP069712 ###
###############################

# Goh Xin Tong 
# RQ 1
# Calculate variance of downtime for each continent 
state_variance <- data %>%
  filter(!is.na(continent )) %>%
  group_by(continent ) %>%
  summarise(downtime_variance = var(downtime, na.rm = TRUE), .groups = 'drop')

# Sort by variance (lowest first, most consistent continent  first)
state_variance <- state_variance %>%
  arrange(downtime_variance)

# Visualize which continent  has the most consistent downtime using variance
ggplot(state_variance, aes(x = reorder(continent , downtime_variance), y = downtime_variance, fill = continent )) +
  geom_bar(stat = "identity") +
  labs(title = "Downtime Consistency by Region (Variance)", x = "continent ", y = "Variance of Downtime") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# RQ 2
# Group data by 'year' and 'continent ' and calculate the total downtime for each year and continent 
total_downtime_per_year_state <- data %>%
  filter(!is.na(continent )) %>%
  group_by(year, continent ) %>%
  summarise(total_downtime = sum(downtime, na.rm = TRUE)) %>%
  arrange(year, continent )


# Create a heatmap to visualize the total downtime per state over the years
ggplot(total_downtime_per_year_state, aes(x = year, y = continent , fill = total_downtime)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +  # Set color gradient for heatmap
  labs(
    title = "Total Downtime per State Over the Years",
    x = "Year",
    y = "State",
    fill = "Total Downtime"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Aggregate total downtime by state
state_downtime <- data %>%
  filter(!is.na(continent) & !is.na(downtime)) %>%
  group_by(continent) %>%
  summarise(total_downtime = sum(downtime, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total_downtime))

# Plot the top 10 states with highest downtime
ggplot(state_downtime, aes(x = reorder(continent, total_downtime), y = total_downtime, fill = total_downtime)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +  # Flip for better readability
  scale_fill_gradient(low = "blue", high = "red") +  # Color gradient
  labs(title = "Total System Downtime by State",
       x = "State",
       y = "Total Downtime (hours)",
       fill = "Total Downtime") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# RQ 4
# Summarize the data: calculate average downtime by continent and year
avg_downtime_trends <- data %>%
  filter(!is.na(continent )) %>%
  group_by(continent, year) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE)) %>%
  arrange(continent, year)

# Visualization of the trends using faceting
ggplot(avg_downtime_trends, aes(x = year, y = avg_downtime)) +
  geom_line(aes(color = continent)) +
  geom_point(aes(color = continent)) +
  facet_wrap(~ continent, scales = "free_y") +  # Facet by continent
  labs(title = "Trends in Average Downtime by Continent and Year",
       x = "Year",
       y = "Average Downtime (hours)") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend since it's already facet-based

downtime_by_continent_year <- data %>%
  filter(!is.na(continent )) %>%
  group_by(continent, year) %>%
  summarise(average_downtime = mean(downtime, na.rm = TRUE))

# Calculate the change in downtime from the first to the last year for each continent
downtime_change <- downtime_by_continent_year %>%
  group_by(continent) %>%
  summarise(first_year_downtime = first(average_downtime),
            last_year_downtime = last(average_downtime),
            change_in_downtime = last_year_downtime - first_year_downtime)

# Bar Chart for Change in Downtime (First to Last Year)
ggplot(downtime_change, aes(x = reorder(continent, change_in_downtime), y = change_in_downtime, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Change in Downtime by Continent (First to Last Year)",
       x = "Continent", y = "Change in Downtime") +
  coord_flip() +  # Flip to make it horizontal
  theme_minimal() +
  theme(legend.position = "none")

##############################################################################

write.csv(data, "output.csv", row.names = FALSE)


#######################################