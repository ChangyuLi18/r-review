# Data Prep
library(dplyr)

# LOAD DATA: read in the `wa-county-data.csv` and `wa-voter-turnout.csv` files
# Make sure to set your working directory
raw_county_data <- read.csv('./data/wa-county-data.csv', stringsAsFactors = F)
raw_voter_data <- read.csv('./data/wa-voter-turnout.csv', stringsAsFactors = F)

# Fix encoding error
colnames(raw_county_data)[1] <- "County.name"

# DATA WRANGLING: clean and join the data frames

# County data: remove commas and percent signs from numeric columns
format_numbers <- function(col, data) {
  new.col <- gsub(",", "", data[,col])
  new.col <- gsub("%", "", new.col)
  return(new.col)
}

# Format the numeric columns
county_data <- data.frame(
  lapply(colnames(raw_county_data), format_numbers, data=raw_county_data), 
  stringsAsFactors = F
)

colnames(county_data) <- tolower(colnames(raw_county_data))
county_data <- county_data %>% 
  mutate(county = gsub(" County", "", county.name)) %>% 
  select(-county.name) %>% 
  select(county, everything()) # for ordering the data frame

# Voter turnout data: remove blank rows and "total" rows
voter_data <- raw_voter_data %>% 
  filter(county != "", county != "Total")

voter_data <- data.frame(
  lapply(colnames(voter_data), format_numbers, data=voter_data), 
  stringsAsFactors = F
)

colnames(voter_data) <- tolower(colnames(raw_voter_data))

# Join the data frames together, set as numeric
all_data <- left_join(county_data, voter_data, by='county') 
all_data[,2:ncol(all_data)] <- sapply(all_data[,2:ncol(all_data)], as.numeric)
