# Clear the environment
rm(list = ls())

# Load necessary libraries
install.packages("zoo")
library(zoo)
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



# Extract the row corresponding to "Homicide And Related Offences"
homicide_data <- crime_data %>%
  filter(rownames(crime_data) == "Homicide And Related Offences")

# Extract the row corresponding to "Population NZ"
population_row <- as.numeric(crime_data["Population NZ", ])

# Transpose the homicide data
homicide_data_t <- t(homicide_data)  # Transpose the data
homicide_df <- as.data.frame(homicide_data_t, stringsAsFactors = FALSE)  # Convert to data frame

# Rename the columns for clarity
colnames(homicide_df) <- c("Offences")
homicide_df$Year <- rownames(homicide_data_t)

# Convert Offences to numeric
homicide_df$Offences <- as.numeric(homicide_df$Offences)

# Standardize the data by dividing by the population and multiplying by 100,000
homicide_df$Standardized_Offences <- (homicide_df$Offences / population_row) * 100000

# Calculate the 3-year moving average for the standardized offences
homicide_df$Moving_Avg <- rollmean(homicide_df$Standardized_Offences, k = 3, fill = NA, align = "right")

# Ensure Year is treated as a factor for proper plotting
homicide_df$Year <- factor(homicide_df$Year, levels = homicide_df$Year)  # Keep Year in original order

# Assuming you already have your party_data defined, here it is for reference:
party_data <- data.frame(
  Year = c("1980/1981", "1981/1982", "1982/1983", "1983/1984", "1984/1985", "1985/1986", "1986/1987", 
           "1987/1988", "1988/1989", "1989/1990", "1990/1991", "1991/1992", "1992/1993", "1993/1994", 
           "1994/1995", "1995/1996", "1996/1997", "1997/1998", "1998/1999", "1999/2000", "2000/2001", 
           "2001/2002", "2002/2003", "2003/2004", "2004/2005", "2005/2006", "2006/2007", "2007/2008", 
           "2008/2009", "2009/2010", "2010/2011", "2011/2012", "2012/2013", "2013/2014", "2014/2015", 
           "2015/2016", "2016/2017", "2017/2018", "2018/2019", "2019/2020"),
  Parties = c("National", "National", "National", "National", 
              "Labour", "Labour", "Labour", "Labour", 
              "Labour", "Labour", "National", "National", 
              "National", "National", "National", "National", 
              "National", "National", "National", "Labour", 
              "Labour", "Labour", "Labour", "Labour", 
              "Labour", "Labour", "Labour", "Labour", 
              "National", "National", "National", "National", 
              "National", "National", "National", "National", 
              "National", "Labour", "Labour", "Labour")
)

# Join homicide_df with party_data by the Year
homicide_df <- homicide_df %>%
  left_join(party_data, by = "Year")

# Ensure Year is treated as a factor for proper plotting
homicide_df$Year <- factor(homicide_df$Year, levels = homicide_df$Year)

# Create the plot with shading for different parties
ggplot(homicide_df, aes(x = Year, group = 1)) +
  # Shading the background by party
  geom_rect(aes(xmin = as.numeric(Year) - 0.5, xmax = as.numeric(Year) + 0.5, 
                ymin = -Inf, ymax = Inf, fill = Parties), alpha = 0.1) +
  # Plot the original standardized data
  geom_line(aes(y = Standardized_Offences, color = "Standardized Offences"), size = 1) +  # Original line
  geom_line(aes(y = Moving_Avg, color = "3-Year Moving Average"), size = 1, linetype = "dashed") +  # Moving average line
  geom_point(aes(y = Standardized_Offences), color = "red", size = 2) +  # Add points for clarity
  labs(title = "Standardized Homicide And Related Offences Over Time",
       x = "Year",
       y = "Standardized Offences (per 100,000 people)") +
  scale_fill_manual(values = c("National" = "blue", "Labour" = "red")) +  # Background colors
  scale_color_manual(values = c("Standardized Offences" = "blue", "3-Year Moving Average" = "red")) +  # Line colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.major.x = element_blank())  # Rotate the x-axis labels for readability and remove grid lines

#######################################


# Extract the rows corresponding to the violent crimes
violent_crimes <- crime_data %>%
  filter(rownames(crime_data) %in% c("Homicide And Related Offences", 
                                     "Acts Intended To Cause Injury", 
                                     "Sexual Assault And Related Offences"))

# Extract the population row
population_row <- as.numeric(crime_data["Population NZ", ])

# Transpose the violent crimes data
violent_crimes_t <- t(violent_crimes)  # Transpose the data
violent_crimes_df <- as.data.frame(violent_crimes_t, stringsAsFactors = FALSE)  # Convert to data frame

# Sum the violent crimes to get total violent crimes per year
violent_crimes_df$Total_Violent_Crimes <- rowSums(violent_crimes_df)

# Add the Year column
violent_crimes_df$Year <- rownames(violent_crimes_t)

# Convert Total_Violent_Crimes to numeric
violent_crimes_df$Total_Violent_Crimes <- as.numeric(violent_crimes_df$Total_Violent_Crimes)

# Standardize the data by dividing by the population and multiplying by 100,000
violent_crimes_df$Standardized_Violent_Crimes <- (violent_crimes_df$Total_Violent_Crimes / population_row) * 100000

# Calculate the 3-year moving average for the standardized violent crimes
violent_crimes_df$Moving_Avg <- rollmean(violent_crimes_df$Standardized_Violent_Crimes, k = 3, fill = NA, align = "right")

# Join violent_crimes_df with party_data by the Year
violent_crimes_df <- violent_crimes_df %>%
  left_join(party_data, by = "Year")

# Ensure Year is treated as a factor for proper plotting
violent_crimes_df$Year <- factor(violent_crimes_df$Year, levels = violent_crimes_df$Year)

# Create the plot with shading for different parties
ggplot(violent_crimes_df, aes(x = Year, group = 1)) +
  # Shading the background by party
  geom_rect(aes(xmin = as.numeric(Year) - 0.5, xmax = as.numeric(Year) + 0.5, 
                ymin = -Inf, ymax = Inf, fill = Parties), alpha = 0.1) +
  # Plot the original standardized data
  geom_line(aes(y = Standardized_Violent_Crimes, color = "Standardized Violent Crimes"), size = 1) +  # Original line
  geom_line(aes(y = Moving_Avg, color = "3-Year Moving Average"), size = 1, linetype = "dashed") +  # Moving average line
  geom_point(aes(y = Standardized_Violent_Crimes), color = "red", size = 2) +  # Add points for clarity
  labs(title = "Standardized Violent Crimes (Homicide, Injury, Sexual Assault) Over Time",
       x = "Year",
       y = "Standardized Violent Crimes (per 100,000 people)") +
  scale_fill_manual(values = c("National" = "blue", "Labour" = "red")) +  # Background colors
  scale_color_manual(values = c("Standardized Violent Crimes" = "blue", "3-Year Moving Average" = "red")) +  # Line colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.major.x = element_blank())  # Rotate the x-axis labels for readability and remove grid lines

##############################

# Extract the rows corresponding to the property-related crimes
property_crimes <- crime_data %>%
  filter(rownames(crime_data) %in% c("Robbery, Extortion And Related Offences", 
                                     "Unlawful Entry With Intent/Burglary, Break And Enter", 
                                     "Theft And Related Offences"))

# Extract the population row
population_row <- as.numeric(crime_data["Population NZ", ])

# Transpose the property crimes data
property_crimes_t <- t(property_crimes)  # Transpose the data
property_crimes_df <- as.data.frame(property_crimes_t, stringsAsFactors = FALSE)  # Convert to data frame

# Sum the property crimes to get total property-related crimes per year
property_crimes_df$Total_Property_Crimes <- rowSums(property_crimes_df)

# Add the Year column
property_crimes_df$Year <- rownames(property_crimes_t)

# Convert Total_Property_Crimes to numeric
property_crimes_df$Total_Property_Crimes <- as.numeric(property_crimes_df$Total_Property_Crimes)

# Standardize the data by dividing by the population and multiplying by 100,000
property_crimes_df$Standardized_Property_Crimes <- (property_crimes_df$Total_Property_Crimes / population_row) * 100000

# Calculate the 3-year moving average for the standardized property crimes
property_crimes_df$Moving_Avg <- rollmean(property_crimes_df$Standardized_Property_Crimes, k = 3, fill = NA, align = "right")

# Join property_crimes_df with party_data by the Year
property_crimes_df <- property_crimes_df %>%
  left_join(party_data, by = "Year")

# Ensure Year is treated as a factor for proper plotting
property_crimes_df$Year <- factor(property_crimes_df$Year, levels = property_crimes_df$Year)

# Create the plot with shading for different parties
ggplot(property_crimes_df, aes(x = Year, group = 1)) +
  # Shading the background by party
  geom_rect(aes(xmin = as.numeric(Year) - 0.5, xmax = as.numeric(Year) + 0.5, 
                ymin = -Inf, ymax = Inf, fill = Parties), alpha = 0.1) +
  # Plot the original standardized data
  geom_line(aes(y = Standardized_Property_Crimes, color = "Standardized Property Crimes"), size = 1) +  # Original line
  geom_line(aes(y = Moving_Avg, color = "3-Year Moving Average"), size = 1, linetype = "dashed") +  # Moving average line
  geom_point(aes(y = Standardized_Property_Crimes), color = "red", size = 2) +  # Add points for clarity
  labs(title = "Standardized Property Crimes (Robbery, Burglary, Theft) Over Time",
       x = "Year",
       y = "Standardized Property Crimes (per 100,000 people)") +
  scale_fill_manual(values = c("National" = "blue", "Labour" = "red")) +  # Background colors
  scale_color_manual(values = c("Standardized Property Crimes" = "blue", "3-Year Moving Average" = "red")) +  # Line colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.major.x = element_blank())  # Rotate the x-axis labels for readability and remove grid lines