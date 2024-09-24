# Load necessary libraries
library(readr)
library(ggplot2)
library(reshape2)
library(dplyr)



# Define the file path
file_path <- "D:/Crime data/Crime data/all_regions.csv"

# Read the CSV file
crime_data <- read_csv(file_path)

# Convert the first column to row names
crime_data <- as.data.frame(crime_data)
rownames(crime_data) <- crime_data[[1]]
crime_data <- crime_data[,-1] # Remove the first column after setting row names

# Create a temporary data frame for calculation (to avoid changing the original crime_data)
temp_data <- crime_data[1:17, ] # Only take rows 1 to 17 for calculation

# Convert Population NZ row to numeric
population_row <- as.numeric(crime_data["Population NZ", ])

# Perform the calculation on the temporary data (divide by population and multiply by 100,000)
temp_data <- sweep(as.matrix(temp_data), 2, population_row, FUN = "/") * 100000

##################################


# Find the column (year) where each crime type peaks without altering the original crime_data
peak_years <- apply(temp_data, 1, function(x) colnames(crime_data)[which.max(x)])

# Create a data frame showing crime types and their peak year
peak_data <- data.frame(
  Crime_Type = rownames(crime_data)[1:17],
  Peak_Year = peak_years
)

# Create a data frame for the year-party mapping
party_data <- data.frame(
  Year = c("1980/1981", "1981/1982", "1982/1983", "1983/1984", "1984/1985", "1985/1986", "1986/1987", 
           "1987/1988", "1988/1989", "1989/1990", "1990/1991", "1991/1992", "1992/1993", "1993/1994", 
           "1994/1995", "1995/1996", "1996/1997", "1997/1998", "1998/1999", "1999/2000", "2000/2001", 
           "2001/2002", "2002/2003", "2003/2004", "2004/2005", "2005/2006", "2006/2007", "2007/2008", 
           "2008/2009", "2009/2010", "2010/2011", "2011/2012", "2012/2013", "2013/2014", "2014/2015", 
           "2015/2016", "2016/2017", "2017/2018", "2018/2019", "2019/2020"),
  Parties = c("National Second term", "National Third term", "National Third term", "National Third term", 
              "Labour First term", "Labour First term", "Labour First term", "Labour Second term", 
              "Labour Second term", "Labour Second term", "National First term", "National First term", 
              "National First term", "National Second term", "National Second term", "National Second term", 
              "National Third term", "National Third term", "National Third term", "Labour First term", 
              "Labour First term", "Labour First term", "Labour Second term", "Labour Second term", 
              "Labour Second term", "Labour Third term", "Labour Third term", "Labour Third term", 
              "National First term", "National First term", "National First term", "National Second term", 
              "National Second term", "National Second term", "National Third term", "National Third term", 
              "National Third term", "Labour First term", "Labour First term", "Labour First term")
)

# Join the peak_data with the party_data on the "Peak_Year" and "Year" columns
peak_data_with_parties <- peak_data %>%
  left_join(party_data, by = c("Peak_Year" = "Year"))


##################################
# Find the percentage change between the peak year and 2019/2020 for each crime type
percentage_change <- sapply(1:nrow(temp_data), function(i) {
  peak_year_value <- temp_data[i, peak_years[i]] # Value for peak year
  recent_value <- temp_data[i, "2019/2020"] # Value for 2019/2020
  
  # Calculate percentage change
  ((peak_year_value - recent_value) / peak_year_value) * 100
})

# Create a data frame showing crime types, peak years, and percentage change
change_data <- data.frame(
  Crime_Type = rownames(temp_data),
  Peak_Year = peak_years,
  Percentage_Change = percentage_change
)

# Sort by percentage change from highest to lowest
sorted_change_data <- change_data[order(-change_data$Percentage_Change), ]

##################################

# Function to calculate year-over-year change for a single row
year_over_year_change <- function(row_data) {
  # Calculate the percentage change for each year-over-year pair
  diff <- ((row_data[2:length(row_data)] - row_data[1:(length(row_data) - 1)]) / row_data[1:(length(row_data) - 1)]) * 100
  return(diff)
}

# Apply the function to each row of temp_data
yearly_percentage_changes <- t(apply(temp_data, 1, year_over_year_change))

# Create column names for the year-over-year changes
change_years <- paste0(colnames(temp_data)[2:ncol(temp_data)], " vs ", colnames(temp_data)[1:(ncol(temp_data) - 1)])

# Create a new data frame to store the year-over-year changes
temp_data_yoy_change <- as.data.frame(yearly_percentage_changes)
colnames(temp_data_yoy_change) <- change_years
rownames(temp_data_yoy_change) <- rownames(temp_data)


##################################
# Find the highest increase and decrease for each crime type
highest_increase <- apply(temp_data_yoy_change, 1, max, na.rm = TRUE)  # Maximum value per row
highest_decrease <- apply(temp_data_yoy_change, 1, min, na.rm = TRUE)  # Minimum value per row

# Find the corresponding year for each crime type's highest increase and decrease
year_of_increase <- apply(temp_data_yoy_change, 1, function(x) colnames(temp_data_yoy_change)[which.max(x)])
year_of_decrease <- apply(temp_data_yoy_change, 1, function(x) colnames(temp_data_yoy_change)[which.min(x)])

# Extract the year part from "year vs year" format
extract_year <- function(year_vs_year) {
  sub(" vs .*", "", year_vs_year)  # Extract the first part of the "year vs year"
}

# Apply the extract function
year_of_increase <- sapply(year_of_increase, extract_year)
year_of_decrease <- sapply(year_of_decrease, extract_year)

##################################
# Create a data frame with crime types, highest increase/decrease, and the corresponding years
increase_decrease_data <- data.frame(
  Crime_Type = rownames(temp_data_yoy_change),
  Highest_Increase = highest_increase,
  Year_of_Increase = year_of_increase,
  Highest_Decrease = highest_decrease,
  Year_of_Decrease = year_of_decrease
)


# Join the increase/decrease data with the party data for both increase and decrease years
increase_decrease_with_parties <- increase_decrease_data %>%
  left_join(party_data, by = c("Year_of_Increase" = "Year")) %>%
  rename(Party_of_Increase = Parties) %>%
  left_join(party_data, by = c("Year_of_Decrease" = "Year")) %>%
  rename(Party_of_Decrease = Parties)

