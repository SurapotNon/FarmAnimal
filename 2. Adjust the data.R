# 2. Adjust data

install.packages('rsample')
library(readr)
library(rsample)
library(lubridate)
library(tidyr)
library()
#----------------------------------------------------------------------------------------------------#
# Ensure necessary columns are factors and set levels across the entire dataset
combined_lamb <- read_csv("combined_lamb.csv")
#----------------------------------------------------------------------------------------------------#
# Adjust the dataset
# Create a new column 'DamYear' by extracting the year from 'Dam'
combined_lamb <- combined_lamb %>%
  mutate(DamYear = as.numeric(sub(".*/", "", Dam)) + 2000)

# Remove the /09 part from Dam
combined_lamb <- combined_lamb %>%
  mutate(Dam = sub("/.*", "", Dam))

# Extract unique No. and D.O.B pairs
mother_dob <- combined_lamb %>%
  select(No., D.O.B) %>%
  distinct() %>%
  rename(Dam = No., MotherDOB = D.O.B)

# Extract unique No. and D.O.B pairs to create a lookup table
lookup <- combined_lamb %>%
  select(No., D.O.B) %>%
  rename(Dam = No., MotherDOB = D.O.B) %>%
  mutate(Dam = as.character(Dam))

# Convert Dam in the original dataset to character
combined_lamb <- combined_lamb %>%
  mutate(Dam = as.character(Dam))

# Join the mother's D.O.B to the original dataset
combined_lamb <- combined_lamb %>%
  left_join(lookup, by = "Dam")

# Extract the year from MotherDOB and create a new column MotherDOBYear
combined_lamb <- combined_lamb %>%
  mutate(MotherDOBYear = year(MotherDOB))

# Update MotherDOB to NA where DamYear does not match MotherDOBYear
combined_lamb <- combined_lamb %>%
  mutate(MotherDOB = if_else(DamYear == MotherDOBYear, MotherDOB, as.Date(NA)))

# Ensure MotherDOB is in Date format
combined_lamb <- combined_lamb %>%
  mutate(MotherDOB = as.Date(MotherDOB))

# Create a new column for the age of the mother on the lamb's birth date
combined_lamb <- combined_lamb %>%
  mutate(MotherAgeOnBornDate = as.numeric(difftime(D.O.B, MotherDOB, units = "days")) / 365.25)

# Drop duplicate rows
combined_lamb<- combined_lamb %>%
  distinct()

# Update MotherDOBYear to NA if MotherDOB is NA
combined_lamb <- combined_lamb %>%
  mutate(MotherDOBYear = if_else(is.na(MotherDOB), NA_real_, MotherDOBYear))

# Update the Sire column to correct the name
combined_lamb <- combined_lamb %>%
  mutate(Sire = ifelse(Sire == "hurcules", "Hurcules", Sire))

combined_lamb <- combined_lamb %>%
  mutate(
    Sex = as.factor(Sex),
    type = as.factor(type),
    BirthType = as.factor(BirthType),
    Sire = as.factor(Sire),
    Dam = as.factor(Dam)
  )

# Save the combined dataset
write_csv(combined_lamb, "combined_lamb_gpt.csv")
#----------------------------------------------------------------------------------------------------#
