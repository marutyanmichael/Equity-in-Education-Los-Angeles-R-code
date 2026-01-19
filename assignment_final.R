rm(list=ls()) # clear the environment
#-------Import necessary packages here-------------------#
# This is the only package you need for the coding assignment
# Including other packages for the autograder may cause some issues
library(tidyverse)
library(stringr)
library(janitor)
#------ Uploading PERMID --------------------------------#
PERMID <- "8844052" #Type your PERMID with the quotation marks
PERMID <- as.numeric(gsub("\\D", "", PERMID)) #Don't touch
set.seed(PERMID) #Don't touch
#------- Answers ----------------------------------------#

#--------PART 1---------#
#Question 1

#part a
# Import the schoolData.csv dataset
school_data <- read_csv("schoolData.csv")

# Clean column names to snake_case
school_data <- school_data %>%
  clean_names()

# Rename specific columns
school_data <- school_data %>%
  rename(
    total_students = total_students_all_grades_excludes_ae,
    reduced_lunch_eligible = reduced_price_lunch_eligible_students,
    free_reduced = free_and_reduced_lunch_students,
    hispanic = hispanic_students,
    black = black_or_african_american_students,
    white = white_students,
    asian = asian_or_asian_pacific_islander_students
  )

# Filter rows where school_level contains specified values and school_type is "1-Regular school"
schoolData1 <- school_data %>%
  filter(
    grepl("Primary|Middle|High|Elementary|Secondary", school_level, ignore.case = TRUE),
    grepl("1-Regular", school_type, ignore.case = TRUE)
  ) %>%
  select(-school_type) # Remove the school_type column

# View the final tibble
print(schoolData1)
#partb#
# Define the cleanValues function
cleanValues <- function(data) {
  # Remove values that are just punctuation
  data <- data %>%
    mutate(across(everything(), ~ ifelse(grepl("^[[:punct:]]*$", .), NA, .)))
  
  # Convert columns with only numbers to numeric
  data <- type.convert(data, as.is = TRUE)  # Corrected the as.is parameter
  
  return(data)
}

# Assuming cleanValues() is a function that cleans your dataset
schoolData <- cleanValues(schoolData1)

# Filter out rows where agency_name is missing
schoolData <- schoolData %>%
  filter(!is.na(agency_name))

# View the resulting tibble
print(schoolData)
#q2#
#Question 2
# Read the districtData.csv file
districtData <- read_csv("districtData.csv")

# Clean column names to snake_case
districtData <- districtData %>%
  clean_names()

# Rename specific columns
districtData <- districtData %>%
  rename(
    iep_students = individualized_education_program_students,
    total_teachers = full_time_equivalent_fte_teachers
  )

# Apply the cleanValues() function to clean the data
districtData1 <- cleanValues(districtData)

# View the cleaned tibble
print(districtData1)
#q3#
# Read the teacherSalaryData.csv file
teacherSalaryData <- read_csv("teacherSalaryData.csv")

# Convert column names to snake_case
teacherSalaryData1 <- teacherSalaryData %>%
  clean_names()

# View the cleaned tibble
print(teacherSalaryData1)
#q4#
# Read the incomeData.csv file
incomeData <- read_csv("incomeData.csv")

# Convert column names to snake_case
incomeData <- incomeData %>%
  clean_names()

# Filter rows for counties in California
countyIncomeData <- incomeData %>%
  filter(str_detect(geo_name, ", CA$")) %>% # Keep rows where geo_name ends with ", CA"
  select(geo_fips, year, income, population) %>% # Keep only specified columns
  mutate(income = as.numeric(income)) # Convert income column to numeric

# View the resulting tibble
print(countyIncomeData)
#q5#
cpiData <- read_csv("CPIAUCSL.csv") %>% 
  clean_names()
print(colnames(cpiData))
# Load necessary libraries

# Read the CPIAUCSL.csv file
cpiData <- read_csv("CPIAUCSL.csv") %>%
  clean_names() # Convert column names to snake_case

# Process the data
cpiData <- cpiData %>%
  filter(month(ymd(date)) == 8) %>% # Filter for rows where the month is August
  mutate(
    year = year(ymd(date)), # Extract year from the date column
    cpi = cpiaucsl / cpiaucsl[year == 2023] * 100 # Scale CPI to 2023 = 100
  ) %>%
  select(year, cpi) # Keep only year and cpi columns

# View the resulting tibble

print(cpiData)
#-----Part2-----------#
#part 1a#
schoolData2 <- schoolData %>%
  mutate(
    asian_perc = (asian / total_students),
    black_perc = (black / total_students),
    hispanic_perc = (hispanic / total_students),
    white_perc = (white / total_students),
    free_reduced_perc = (free_reduced / total_students),
    migrant_students_perc = (migrant_students / total_students)
  )

# Print the new tibble
print(schoolData2)
#part 1b#
schoolData_summary1 <- schoolData2 %>%
  group_by(agency_id_nces_assigned, year) %>%
  summarize(
    num_schools = n(),
    num_students = sum(total_students, na.rm = TRUE),
    charter_school_perc = mean(charter_school == "1-Yes", na.rm = TRUE),
    title_i_perc = mean(school_wide_title_i == "1-Yes", na.rm = TRUE),
    asian_perc = weighted.mean(asian_perc, w = total_students, na.rm = TRUE),
    black_perc = weighted.mean (black_perc, w = total_students, na.rm = TRUE),
    hispanic_perc = weighted.mean(hispanic_perc, w = total_students, na.rm = TRUE),
    white_perc = weighted.mean(white_perc, w = total_students, na.rm = TRUE),
    free_reduced_perc = weighted.mean(free_reduced_perc, w = total_students, na.rm = TRUE),
    migrant_students_perc = weighted.mean(migrant_students_perc, w = total_students, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()
print(schoolData_summary1)
#part 1c#
# Join schoolData_summary1 and districtData1
districtData2 <- schoolData_summary1 %>%
  left_join(
    districtData1, 
    by = c("year", "agency_id_nces_assigned")
  ) %>%
  filter(!is.na(agency_id_nces_assigned)) # Remove rows where agency_id_nces_assigned is NA

# View the resulting tibble
print(districtData2)
#part 2a#
name_cleaner <- function(string, list_of_words) {
  string <- str_replace_all(string, "[[:punct:]]", " ") # Replace all punctuation with a space
  string <- str_squish(string) # Remove excess whitespace
  
  # Create a regex pattern for the list of words
  pattern <- paste0("\\b(", paste(tolower(list_of_words), collapse = "|"), ")\\b")
  
  # Remove unwanted words
  string <- str_remove_all(tolower(string), pattern)
  
  string <- str_squish(string) # Remove any new excess whitespace
  string <- str_to_title(string) # Convert to title case
  
  return(string)
}

# part 2b#
districtData3 <- districtData2 %>%
  mutate(agency_name = name_cleaner(agency_name, list_of_words = c("district")))

teacherSalaryData2 <- teacherSalaryData1 %>%
  mutate(employer_name = name_cleaner(employer_name, list_of_words = c("district")))

# Test name_cleaner on a specific string
result <- name_cleaner("ALVIEW-DAIRYLAND UNION ELEMENTARY", list_of_words = "district")
print(result)

# View the cleaned data
head(districtData3)
head(teacherSalaryData2)
#part 2b#
districtData4 <- districtData3 %>%
  left_join(
    teacherSalaryData2 %>% filter(!is.na(employer_name)), # Remove NAs from employer_name only
    by = c("agency_name" = "employer_name", "year"),
    na_matches = "never" # Prevent NA-to-NA matches
  )
print(districtData4)
#part 2c
# Join districtData4 with cpiData by year, keeping all rows from districtData4
districtData <- districtData4 %>%
  left_join(cpiData, by = "year")

# View the resulting tibble
print(districtData)
#print3a#
countyData1 <- districtData %>%
  group_by(county_number, year) %>%
  summarize(
    county_name = first(county_name), # Keep the first county name for each group
    
    # (i) Percent of schools that are charter or school-wide Title I
    charter_school_perc = weighted.mean(charter_school_perc, w = num_schools, na.rm = TRUE),
    title_i_perc = weighted.mean(title_i_perc, w = num_schools, na.rm = TRUE),
    
    # (ii) Average teacher salary weighted by the number of teachers
    teacher_salary = weighted.mean(salary, w = total_teachers, na.rm = TRUE),
    
    # (iii) Weighted percentages of student demographics
    asian_perc = weighted.mean(asian_perc, w = num_students, na.rm = TRUE),
    white_perc = weighted.mean(white_perc, w = num_students, na.rm = TRUE),
    black_perc = weighted.mean(black_perc, w = num_students, na.rm = TRUE),
    hispanic_perc = weighted.mean(hispanic_perc, w = num_students, na.rm = TRUE),
    free_reduced_perc = weighted.mean(free_reduced_perc, w = num_students, na.rm = TRUE),
    migrant_students_perc = weighted.mean(migrant_students_perc, w = num_students, na.rm = TRUE),
    
    # (iv) Counts for districts, schools, staff, teachers, and students
    num_districts = n(), # Number of districts in the group
    num_schools = sum(num_schools, na.rm = TRUE),
    num_staff = sum(total_staff, na.rm = TRUE),
    num_teachers = sum(total_teachers, na.rm = TRUE),
    num_students = sum(num_students, na.rm = TRUE),
    
    .groups = "drop" # Ungroup after summarizing
  )

# View the summarized county-level data
print(countyData1)
#part 3b#
countyData1_temp <- countyData1 %>%
  mutate(county_number = as.numeric(county_number))

countyIncomeData_temp <- countyIncomeData %>%
  mutate(geo_fips = as.numeric(geo_fips))

# Perform the join
countyData2 <- countyData1_temp %>%
  left_join(
    countyIncomeData_temp,
    by = c("county_number" = "geo_fips", "year" = "year")
  )

# View the resulting tibble
print(countyData2)
#part 3c#
# Join countyData2 with cpiData by year
countyData <- countyData2 %>%
  left_join(
    cpiData,
    by = "year"
  ) %>%
  mutate(
    # Calculate real_income and real_teacher_salary
    real_income = income / (cpi / 100),  # Adjust income to 2023 dollars
    real_teacher_salary = teacher_salary / (cpi / 100)  # Adjust teacher_salary to 2023 dollars
  )

# View the resulting tibble
print(countyData)
#---------Part 3-------------#
#part 1#
schoolDataClean <- schoolData %>%
  filter(!is.na(national_school_lunch_program), !is.na(locale)) %>%
  mutate(
    national_school_lunch_program = case_when(
      national_school_lunch_program == "Yes under Community Eligibility Option (CEO)" ~ "Yes with CEO",
      national_school_lunch_program == "No" ~ "No",
      TRUE ~ "Yes without CEO"
    ),
    locale = case_when(
      locale %in% c("11-City: Large", "12-City: Mid-size", "13-City: Small") ~ "City",
      locale %in% c("21-Suburb: Large", "22-Suburb: Mid-size", "23-Suburb: Small") ~ "Suburb",
      locale %in% c("31-Town: Large", "32-Town: Small", "31-Town: Fringe", "32-Town: Distant", "33-Town: Remote") ~ "Town",
      locale %in% c("41-Rural: Large", "42-Rural: Small", "41-Rural: Fringe", "42-Rural: Distant", "43-Rural: Remote") ~ "Rural",
      TRUE ~ locale
    )
  )

nslp_locale_table <- schoolDataClean %>%
  tabyl(year, national_school_lunch_program, locale) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting()

# View the resulting table
print(nslp_locale_table)
#part 2#
# Create salaryQuartilesData
salaryQuartilesData <- countyData %>%
  filter(!is.na(real_income)) %>%  # Exclude rows with NA values in real_income
  group_by(year) %>%  # Group by year
  summarize(
    income_min = min(real_income, na.rm = TRUE),  # Minimum real income
    income_25 = quantile(real_income, 0.25, na.rm = TRUE),  # 25th percentile
    income_50 = median(real_income, na.rm = TRUE),  # Median (50th percentile)
    income_75 = quantile(real_income, 0.75, na.rm = TRUE),  # 75th percentile
    income_max = max(real_income, na.rm = TRUE),  # Maximum real income
    .groups = "drop"  # Ungroup after summarization
  )

# View the resulting tibble
print(salaryQuartilesData)
#part 3#
# Filter the data for required years and ensure `free_reduced_perc` is not NA
histogram_data <- schoolData2 %>%
  filter(year %in% c(1999, 2009, 2019), !is.na(free_reduced_perc))

# Create the histograms using ggplot
free_reduced_histogram <- ggplot(histogram_data, aes(x = free_reduced_perc)) +
  geom_histogram(binwidth = 0.05, color = "black", fill = "darkgray") +  # Histogram with bin width
  facet_wrap(~ year, scales = "free_y") +  # Facet by year with independent y-axes
  labs(
    x = "Percentage of Students Eligible for Free/Reduced Lunch",
    y = "Count",
    title = "Distribution of Free/Reduced Lunch Eligibility in 1999, 2009, and 2019"
  ) +
  theme_minimal()

# Save the histogram as a PNG file
ggsave("free_reduced_histogram.png", free_reduced_histogram, width = 12, height = 6)

# Display the histogram
print(free_reduced_histogram)
#part 4#
county_income_quartiles_data <- countyData %>%
  filter(year == 2019) %>%
  left_join(salaryQuartilesData, by = "year") %>%  # Join to get quartile cutoffs
mutate(
  quartile = case_when(
    !is.na(real_income) & real_income <= income_25 ~ 1,  # First quartile
    !is.na(real_income) & real_income <= income_50 ~ 2,  # Second quartile
    !is.na(real_income) & real_income <= income_75 ~ 3,  # Third quartile
    !is.na(real_income) & real_income > income_75 ~ 4,   # Fourth quartile
    TRUE ~ NA_integer_  # NA for missing values
  )
) %>%
  select(
    year, county_number, county_name, title_i_perc,
    charter_school_perc, teacher_salary,
    asian_perc, white_perc, black_perc, hispanic_perc,
    free_reduced_perc, migrant_students_perc,
    num_districts, num_schools, num_staff, num_teachers, num_students,
    income, population,
    cpi, real_teacher_salary, real_income, quartile
  ) %>%  # Remove quartile cutoff columns
  arrange(real_income)

# Validate `county_income_quartiles_data`
print(county_income_quartiles_data)
# Create the bar plot
county_income_plot <- county_income_quartiles_data %>%
  ggplot(aes(
    x = reorder(county_name, real_income),  # Arrange counties by income
    y = real_income,
    fill = factor(quartile)  # Color by quartile
  )) +
  geom_bar(stat = "identity", show.legend = TRUE) +  # Use bar plot
  scale_fill_manual(
    values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),  # Assign colors to quartiles
    name = "Income Quartile",
    labels = c("First Quartile", "Second Quartile", "Third Quartile", "Fourth Quartile")
  ) +
  labs(
    title = "Average Real Income by County in 2019",
    x = "County",
    y = "Average Real Income"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5)
  )

# Display the plot
print(county_income_plot)
#part5#
library(ggplot2)
library(dplyr)

# Create the tibble for free or reduced-price lunch data
free_reduced_county_data <- countyData %>%
  filter(year == 2019) %>%
  left_join(salaryQuartilesData, by = "year") %>%
  mutate(
    quartile = case_when(
      !is.na(real_income) & real_income <= income_25 ~ 1,  # First quartile
      !is.na(real_income) & real_income <= income_50 ~ 2,  # Second quartile
      !is.na(real_income) & real_income <= income_75 ~ 3,  # Third quartile
      !is.na(real_income) & real_income > income_75 ~ 4,   # Fourth quartile
      TRUE ~ NA_integer_  # NA for missing values
    )
  ) %>%
  select(
    county_number, county_name, free_reduced_perc, real_income, quartile,
    title_i_perc, charter_school_perc, teacher_salary, asian_perc, white_perc,
    black_perc, hispanic_perc, migrant_students_perc, num_districts, num_schools,
    num_staff, num_teachers, num_students, income, population, cpi, real_teacher_salary, year
  ) %>%
  arrange(free_reduced_perc)

# Validate `free_reduced_county_data`
print(free_reduced_county_data)

# Create the bar plot
# Save the plot object to the variable
free_reduced_county_plot <- ggplot(free_reduced_county_data, aes(x = reorder(county_name, free_reduced_perc), y = free_reduced_perc, fill = factor(quartile))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("1" = "#1f77b4", "2" = "#ff7f0e", "3" = "#2ca02c", "4" = "#d62728"),
    name = "Income Quartile",
    labels = c("Bottom", "Second", "Third", "Top")
  ) +
  labs(
    title = "Percent of Students Eligible for Free/Reduced-Price Lunch by County (2019)",
    x = "County",
    y = "Percent Eligible for Free/Reduced-Price Lunch"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "top"
  )

# Display the plot
print(free_reduced_county_plot)
