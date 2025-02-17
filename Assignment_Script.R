# Lim Wei Lun - TP069058
# Goh Xin Tong - TP069712
# Chang Qi Eue - TP065956
# Lee Jun Zhe - TP065879

#####################################################################################################
### Import Necessary Packages ###
#####################################################################################################

if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")
if (!requireNamespace("mice", quietly = TRUE)) install.packages("mice")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("countrycode", quietly = TRUE)) install.packages("countrycode")
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("stringdist", quietly = TRUE)) install.packages("stringdist")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")

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
library(stringdist)
library(caret)

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

################################################################################
### DATA PREPROSESSING + DATA EXPLORATION + DATA VALIDATION ###
################################################################################

##########################################################
### DATE ###
##########################################################

########################
### Data Exploration ###
########################

# check how many column of date is empty
# No column is empty
print("Missing Data - Date")
sum(is.na(data$date)) / nrow(data)
summary(data$date)

##########################
### Data Preprocessing ###
##########################

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

#######################
### Data Validation ###
#######################

data$year <- as.integer(data$year)


# check the cleaned data
table(data$year)



##########################################################
### Data Attribute - IP ###
##########################################################

########################
### Data Exploration ###
########################

# Calculate the percentage of missing values in the IP column
missing_percentage <- sum(is.na(data$ip)) / nrow(data) * 100
cat("Percentage of missing data in IP column:", missing_percentage, "%\n") # 13.9 %

# Filter out rows where 'ip' is NA and view the first few rows
filtered_data <- data %>% filter(!is.na(ip))

# View the first few IPs after filtering
head(filtered_data$ip)

# By viewing the first few rows of the ip, we know that 
# the ip is in format eg 192.168.1.1 


##########################
### Data Preprocessing ###
##########################

#' Classify IP Address into Network Class
#'
#' This function takes an IPv4 address and classifies it into one of the following 
#' network classes: Class A, Class B, Class C, Class D (Multicast), or Class E (Reserved).
#'
#' @param ip A character string representing an IPv4 address (e.g., "192.168.1.1").
#' 
#' @return A character string indicating the class of the provided IP address. 
#' Possible return values are:
#' \itemize{
#'   \item Class A
#'   \item Class B
#'   \item Class C
#'   \item Class D (Multicast)
#'   \item Class E (Reserved)
#'   \item Invalid IP (if the input is not a valid IPv4 address).
#' }
#' 
#' @examples
#' classify_ip("192.168.1.1")  # Returns "Class C"
#' classify_ip("10.0.0.1")     # Returns "Class A"
#' classify_ip("256.256.256.256")  # Returns "Invalid IP"
#' 
#' @export
classify_ip <- function(ip) {
  if (is.na(ip)) {
    return(NA)  # Return NA if the IP is missing
  }
  # Split the IP into its octets
  octets <- as.numeric(strsplit(ip, "\\.")[[1]])
  
  # Classify based on the first octet
  if (octets[1] >= 1 & octets[1] <= 127) {
    return("Class A")
  } else if (octets[1] >= 128 & octets[1] <= 191) {
    return("Class B")
  } else if (octets[1] >= 192 & octets[1] <= 223) {
    return("Class C")
  } else if (octets[1] >= 224 & octets[1] <= 239) {
    return("Class D (Multicast)")
  } else if (octets[1] >= 240 & octets[1] <= 255) {
    return("Class E (Reserved)")
  } else {
    return("Invalid IP")
  }
}

# Apply the classify_ip function to the 'ip' column and create 'ip_class' column
data$ip_class <- sapply(data$ip, classify_ip)

table(data$ip_class)

#######################
### Data Validation ###
#######################

# Count the number of valid vs invalid IP addresses
valid_ips <- sum(data$ip_class != "Invalid IP" & !is.na(data$ip_class))
invalid_ips <- sum(data$ip_class == "Invalid IP" | is.na(data$ip_class))

# Visualize the validation results (valid vs invalid IPs)
validation_data <- data.frame(
  IP_Validation = c("Valid IP", "Invalid IP"),
  Count = c(valid_ips, invalid_ips)
)

ggplot(validation_data, aes(x = IP_Validation, y = Count, fill = IP_Validation)) +
  geom_bar(stat = "identity", color = "black") +
  ggtitle("Valid vs Invalid IP Addresses") +
  xlab("IP Validation Status") +
  ylab("Count") +
  theme_minimal()



##########################################################
### Data Attribute - OS ###
##########################################################

########################
### Data Exploration ###
########################

# Chech how many OS is NA
# Since only 0.046 of data is missing, therefore, it is safe to just disable it
print("Missing data - OS")
sum(is.na(data$os)) / nrow(data)

##########################
### Data Preprocessing ###
##########################

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

#######################
### Data Validation ###
#######################

# Ensure that the 'os_category' column is a factor
data$os_category <- as.factor(data$os_category)

# Count the number of different OS categories and inspect
os_category_count <- table(data$os_category)
cat("OS Category Validation:\n")
print(os_category_count)

# Visualize the OS categories in a bar chart
ggplot(data, aes(x = os_category)) +
  geom_bar(fill = "skyblue", color = "black") +
  ggtitle("Distribution of OS Categories") +
  xlab("OS Category") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##########################################################
### Data Attribute - Web Server ###
##########################################################

########################
### Data Exploration ###
########################

# Chech how many data is NA
print("Missing data - Web Server")
sum(is.na(data$webserver)) / nrow(data) # 27.76%

##########################
### Data Preprocessing ###
##########################

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

#######################
### Predict Missing WebServer ###
#######################

# Convert categorical variables to factors
data$webserver_cleaned <- as.factor(data$webserver_cleaned)
data$os_category <- as.factor(data$os_category)
data$encoding_group <- as.factor(data$encoding_group)

# Separate Data into Training & Testing Sets
train_data <- data %>% filter(!is.na(webserver_cleaned))
test_data <- data %>% filter(is.na(webserver_cleaned))

# Train a Random Forest model to predict WebServer category
if (nrow(test_data) > 0) {
  model_webserver <- randomForest(webserver_cleaned ~ os_category + year + encoding_group + downtime,
                                  data = train_data, ntree = 100, importance = TRUE)
  
  # Predict missing WebServer values
  test_data$webserver_cleaned <- predict(model_webserver, test_data)
  
  # Update the dataset with predicted values
  data$webserver_cleaned[is.na(data$webserver_cleaned)] <- test_data$webserver_cleaned
}

#######################
### Data Validation ###
#######################

data$webserver_cleaned <- as.factor(data$webserver_cleaned)

# Count the number of different web server categories and inspect
webserver_category_count <- table(data$webserver_cleaned)
cat("Web Server Category Validation:\n")
print(webserver_category_count)

# Visualize the cleaned web server data
ggplot(data, aes(x = webserver_cleaned)) +
  geom_bar(fill = "lightgreen", color = "black") +
  ggtitle("Final Distribution of Web Server Categories After Prediction") +
  xlab("Web Server Category") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




##########################################################
### Data Attribute - Country ###
##########################################################

########################
### Data Exploration ###
########################
# check how many row of contry is NA 
# 0.3175 data is missing 
print("Missing Data - Country")
sum(is.na(data$country)) / nrow(data)

##########################
### Data Preprocessing ###
##########################

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

# Step 1: Convert categorical variables to factors
data$os_category <- as.factor(data$os_category)
data$country_cleaned <- as.factor(data$country_cleaned)

# Step 2: Create a continent column from country_cleaned using countrycode
#         For rows with a known country, countrycode returns the corresponding continent.
data$continent <- countrycode(data$country_cleaned,
                              origin = "country.name",
                              destination = "continent")
table(data$continent)
data$continent <- as.factor(data$continent)


# Step 3: Split the data into training (rows with known continent) and 
#         testing (rows with missing continent) datasets.
train_data <- data %>% filter(!is.na(continent))
test_data <- data %>% filter(is.na(continent))


# Step 4: Train a Random Forest model to predict the missing continent values.
#         Since continent is now a factor, Random Forest will perform classification.
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

# Step 5: Validate the model performance
if (nrow(test_data) > 0 && !all(is.na(test_data$continent))) {
  predicted_continents <- predict(model, test_data)
  
  # Confusion Matrix for validation (Only if true continent values exist for test_data)
  confusion_result <- confusionMatrix(predicted_continents, test_data$continent)
  print(confusion_result)
}

#######################
### Data Validation ###
#######################

data$continent <- as.factor(data$continent)

# Visualize the distribution of countries
ggplot(data, aes(x = country_cleaned)) +
  geom_bar(fill = "lightblue", color = "black") +
  ggtitle("Distribution of Countries") +
  xlab("Country") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualize the distribution of continents
ggplot(data, aes(x = continent)) +
  geom_bar(fill = "lightcoral", color = "black") +
  ggtitle("Distribution of Continents") +
  xlab("Continent") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




##########################################################
### Data Attribute - Encoding ###
##########################################################

########################
### Data Exploration ###
########################

# 58.97% data is missing
sum(is.na(data$encoding)) / nrow(data)

##########################
### Data Preprocessing ###
##########################

data <- data %>%
  mutate(encoding_group = case_when(
    grepl("utf", encoding, ignore.case = TRUE) ~ "UTF",
    grepl("windows", encoding, ignore.case = TRUE) ~ "Windows",
    grepl("iso", encoding, ignore.case = TRUE) ~ "ISO",
    grepl("big5", encoding, ignore.case = TRUE) ~ "Big5",
    grepl("euc", encoding, ignore.case = TRUE) ~ "EUC",
    grepl("ascii", encoding, ignore.case = TRUE) ~ "ASCII",
  ))

sum(is.na(data$encoding_group)) / count(data)

table(data$encoding_group)

# Step 1: Convert encoding_group to factor
data$encoding_group <- as.factor(data$encoding_group)

# Step 2: Split the data into training (with known encoding_group) and testing (with missing encoding_group)
train_data_encoding <- data %>% filter(!is.na(encoding_group))
test_data_encoding <- data %>% filter(is.na(encoding_group))

# Step 3: Train a Random Forest model to predict the missing encoding_group values
if (nrow(test_data_encoding) > 0) {
  model_encoding <- randomForest(encoding_group ~ downtime + year + os_category + continent, 
                                 data = train_data_encoding,
                                 ntree = 100,
                                 importance = TRUE)
  
  # Predict encoding_group for rows with missing encoding_group
  test_data_encoding$encoding_group <- predict(model_encoding, test_data_encoding)
  
  # Replace the missing encoding_group values in the original dataset with the predictions
  data$encoding_group[is.na(data$encoding_group)] <- test_data_encoding$encoding_group
}

# Check the result
table(data$encoding_group)

#######################
### Data Validation ###
#######################

# ensure the encoding is factor 
data$encoding_group <- as.factor(data$encoding_group)


##########################################################
### Data Attribute - Downtime ###
##########################################################

########################
### Data Exploration ###
########################

# check how many row is empty 
sum(is.na(data$downtime)) / nrow(data)
# no row is empty 

boxplot(data$downtime, 
        main = "Box Plot of Downtime",
        ylab = "Downtime",
        col = "lightblue",
        border = "blue")

##########################
### Data Preprocessing ###
##########################

# remove if downtime is less than 0
invalid_downtime <- data %>% filter(downtime < 0)
print(paste("Number of negative downtime values:", nrow(invalid_downtime)))

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

#######################
### Data Validation ###
#######################

data$downtime <- as.numeric(data$downtime)







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
#' How has the proportion of downtime attributed to different OS categories changed over the years?

loss_proportion <- data %>%
  group_by(year, os_category) %>%
  summarise(
    Total_Loss = sum(downtime)
  ) %>%
  mutate(
    Proportion = Total_Loss / sum(Total_Loss)
  )  # Calculate proportion for each OS category

#Stacked Bar to check the proportion for each OS category
ggplot(loss_proportion, aes(x = year, y = Proportion, fill = os_category)) +
  geom_bar(stat = "identity", position = "fill") +  
  scale_y_continuous(labels = scales::percent) +                #Convert the scale into proportion for frequency
  labs(
    title = "Proportion of Downtime by OS Category Over Years",
    x = "Year", 
    y = "Total Loss"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

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

############
### RQ 5 ###
############
#' Goh Xin Tong - TP069712
#' RQ 5
#' Which continent consistently experiences the lowest system downtime across multiple years?

# Step 1: Identify which continent has the lowest average downtime each year
min_downtime_per_year <- avg_data %>%
  group_by(year) %>%
  slice(which.min(avg_downtime)) %>%
  ungroup()  # Select the continent with the lowest downtime for each year

# Step 2: Count how many times each continent had the lowest downtime
continent_min_count <- min_downtime_per_year %>%
  count(continent) %>%
  rename(min_count = n)

# Step 3: Create the pie chart with labels
ggplot(continent_min_count, aes(x = "", y = min_count, fill = continent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Convert to pie chart
  labs(title = "Distribution of Years with Lowest Downtime by Continent") +
  theme_void() +  # Removes axis labels and ticks
  theme(legend.title = element_blank()) +  # Removes legend title
  geom_text(aes(label = paste(continent, "(", min_count, ")", sep = "")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 5)  # Add labels in white color



# View the result
print(continent_max_count)


###############################################################################
###############################################################################

##############################
### CHANG QI EUE - TP065956 ###
##############################

############
### RQ 1 ###
############
webserver_counts <- data.frame(table(data$webserver_cleaned))

# Generate the bar chart with exact count labels
ggplot(webserver_counts, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  ggtitle("Distribution of Web Server Categories") +
  xlab("Web Server Category") +
  ylab("Count") +
  scale_y_continuous(labels = comma) +
  geom_text(aes(label = comma(Freq)), vjust = -0.3, size = 5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#########TOP 3################

# Step 1: Calculate the Average Downtime for Each Web Server Type
avg_downtime_webserver <- data %>%
  group_by(webserver_cleaned) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_downtime)) %>%
  head(3)  

# Step 2: Filter Data for Only the Top 3 Web Servers
top3_data <- data %>% 
  filter(webserver_cleaned %in% avg_downtime_webserver$webserver_cleaned)

# Step 3: Create a Box Plot to Compare Downtime for the Top 3 Web Servers
ggplot(top3_data, aes(x = webserver_cleaned, y = downtime, fill = webserver_cleaned)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16) +  
  labs(
    title = "Top 3 Web Servers with the Highest Downtime",
    x = "Web Server Type",
    y = "Downtime Duration (Days)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "darkblue"),
    axis.text.y = element_text(face = "bold", color = "darkred"),
    plot.title = element_text(face = "bold", hjust = 0.5, color = "purple"),
    legend.position = "none"
  ) +
  geom_text(data = avg_downtime_webserver, 
            aes(x = webserver_cleaned, y = avg_downtime, label = round(avg_downtime, 1)), 
            vjust = -0.5, size = 5, fontface = "bold", color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

############
### RQ 2 ###
############
# Step 1: Calculate Average Downtime by Year & Web Server
avg_downtime_per_server <- data %>%
  group_by(webserver_cleaned, year) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = "drop")

# Step 2: Plot Heatmap
ggplot(avg_downtime_per_server, aes(x = year, y = reorder(webserver_cleaned, avg_downtime), fill = avg_downtime)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c("purple4", "blue", "cyan4", "yellowgreen", "yellow"), name = "Avg Downtime") +
  labs(
    title = "Heatmap of Downtime by Web Server Age",
    x = "Year of Web Server",
    y = "Web Server Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

############
### RQ 3 ###
############
# Step 1: Calculate the average downtime for each web server category
avg_downtime_webserver <- data %>%
  group_by(webserver_cleaned) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_downtime)) 

# Step 2: Create a Bar Plot
ggplot(avg_downtime_webserver, aes(x = reorder(webserver_cleaned, avg_downtime), 
                                   y = avg_downtime, fill = webserver_cleaned)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  labs(
    title = "Average Downtime of Each Web Server",
    x = "Web Server",
    y = "Average Downtime (Days)"
  ) +
  theme_minimal(base_size = 15) +
  scale_fill_viridis_d() +
  geom_text(aes(label = round(avg_downtime, 1)), vjust = -0.5, size = 5, 
            fontface = "bold", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############
### RQ 4 ###
############
# Step 1: Calculate Average Downtime for Each Continent and Web Server Type
avg_downtime_continent <- data %>%
  group_by(continent, webserver_cleaned) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_downtime))

# Step 2: Plot Average Downtime for Each Continent and Web Server
ggplot(avg_downtime_continent, aes(x = continent, y = avg_downtime, fill = continent)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~ webserver_cleaned, scales = "free_y") +
  labs(title = "Downtime by Continent for Each Web Server",
       x = "Continent",
       y = "Average Downtime (Days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############
### RQ 5 ###
############
# Step 1: Calculate the average downtime for each OS-Web Server combination
avg_downtime_os_webserver <- data %>%
  group_by(os_category, webserver_cleaned) %>%
  summarise(avg_downtime = mean(downtime, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_downtime))  

# Step 2: Create a Heatmap Visualization
ggplot(avg_downtime_os_webserver, aes(x = webserver_cleaned, y = os_category, fill = avg_downtime)) +
  geom_tile(color = "white") +
  labs(
    title = "Impact of OS and Web Server Type on Downtime",
    x = "Web Server",
    y = "Operating System",
    fill = "Avg Downtime (Days)"
  ) +
  scale_fill_viridis_c(option = "plasma") + 
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################################################
###############################################################################

################################
### LEE JUN ZHE ### TP065879 ###
################################
###########
### RQ1 ###
###########

### Do different types of encoding formats affect downtime? ###
### step 1 : check the unique value of encoding to ensure that the data format is correct ###
unique(data$encoding)

### step 2 : Calculate the average downtime of different encodings ###
encoding_downtime <- data %>%
  group_by(encoding) %>%
  summarise(mean_downtime = mean(downtime, na.rm = TRUE),
            median_downtime = median(downtime, na.rm = TRUE),
            sd_downtime = sd(downtime, na.rm = TRUE),
            count = n()) %>%
  arrange(desc(mean_downtime))

print(encoding_downtime)

### step 3 : Obsolete/unsupported encoding format ###
legacy_encodings <- c("ISO-8859-15", "Big5", "windows-874", "GB2312", 
                      "windows-1253", "TIS-620", "windows-1255", "Big5-HKSCS")
data <- data %>%
  mutate(encoding_category = ifelse(encoding %in% legacy_encodings, "Legacy", "Modern"))

### step 4 : Calculate the average downtime of two types of encoding ###
legacy_analysis <- data %>%
  group_by(encoding_category) %>%
  summarise(mean_downtime = mean(downtime, na.rm = TRUE),
            median_downtime = median(downtime, na.rm = TRUE),
            sd_downtime = sd(downtime, na.rm = TRUE),
            count = n())

print(legacy_analysis)

### step 5: Statistical test to determine whether there is a significant difference ### 
wilcox_test <- wilcox.test(downtime ~ encoding_category, data = data)
print(wilcox_test)

### step 6 : Visual comparison of downtime between Legacy and Modern encodings ###

ggplot(data, aes(x = encoding_category, y = downtime, fill = encoding_category)) +
  geom_boxplot() +
  labs(title = "Obsolete Code vs Modern Coding Downtime comparison",
       x = "Coding category",
       y = "downtime") +
  theme_minimal()

#############
### RQ 2 ####
#############

### What is the average downtime for each encoding format? ###
### step 1 : Take the top 10 codes with the highest downtime ###
top_encodings <- data %>%
  group_by(encoding) %>%
  summarise(mean_downtime = mean(downtime, na.rm = TRUE)) %>%
  arrange(desc(mean_downtime)) %>%
  top_n(10, mean_downtime)  

### step 2 : Bar Chart Visualization ###
ggplot(top_encodings, aes(x = reorder(encoding, -mean_downtime), y = mean_downtime, fill = encoding)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 codes with the most downtime",
       x = "Encoding",
       y = "Average downtime (downtime)")

#############
### RQ 3 ####
#############

### Is there a significant relationship between encoding types and average downtime for different web servers?  ###
### step 1 : Filter out N and NA ###
data_filtered <- data %>%
  filter(!is.na(webserver) & webserver != "N",
         !is.na(encoding) & encoding != "N")

### step 2 : Calculate the frequency of each encoding and keep the top 10  ###
############ Calculate the frequency of each web server and keep the top 4 ###
top_encoding <- data_filtered %>%
  count(encoding) %>%
  top_n(10, n) %>%
  pull(encoding)

top_webservers <- data_filtered %>%
  count(webserver) %>%
  top_n(4, n) %>%
  pull(webserver)

### step 3 :  Filtering Data ###
data_filtered <- data_filtered %>%
  filter(encoding %in% top_encoding)
data_filtered <- data_filtered %>%
  filter(webserver %in% top_webservers)

### step 4 : Drawing heatmap ###
ggplot(data_filtered, aes(x = webserver, y = encoding, fill = downtime)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Average Downtime for Top 4 Web Servers and Encoding Types",
       x = "Web Server",
       y = "Encoding",
       fill = "Avg Downtime") +
  theme_minimal()

#############
### RQ 4 ###
############

### Is there a difference in downtime between different encoding formats over time? ###
### step 1 : Calculate downtime for different years ###
data_summary <- data %>%
  group_by(year, encoding_category) %>%
  summarise(mean_downtime = mean(downtime, na.rm = TRUE),
            count = n())

### step 2 : Downtime of different encoding formats over time (line graph) ###
ggplot(data_summary, aes(x = year, y = mean_downtime, color = encoding_category, group = encoding_category)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal() +
  labs(title = "Downtime changes over time for different encoding formats",
       x = "Years",
       y = "Average downtime",
       color = "Encoding Type")

############
### RQ 5 ###
############

### Determine the difference in downtime between legacy and modern code on an OS? ###
### step 1 : Calculate the average downtime for different OS & Encoding categories ###
data_summary <- data %>%
  group_by(os_category, encoding_category) %>%
  summarise(mean_downtime = mean(downtime, na.rm = TRUE))

### step 2 : Plotting a heat map ###
ggplot(data_summary, aes(x = encoding_category, y = os_category, fill = mean_downtime)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Average downtime by OS & encoding",
       x = "Encoding format",
       y = "Operating system",
       fill = "Average downtime")

###############################################################################
###############################################################################


write.csv(data, "output.csv", row.names = FALSE)


#######################################
