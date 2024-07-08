library(readr)
library(dplyr)
#----------------------------------------------------------------------------------------------------#
# Function to read, convert factors, add Year
process_and_add_year <- function(file_path, year) {
  data <- read_csv(file_path) %>%
    mutate(
      Year = year,
      Sex = as.factor(Sex),
      type = as.factor(type),
      BirthType = as.factor(BirthType),
      Sire = as.character(Sire)  # Ensure Sire is character
    )
  return(data)
}

# Process each dataset and add the Year column
lamb2016 <- process_and_add_year("lamb2016r.csv", 2016)
lamb2017 <- process_and_add_year("lamb2017r.csv", 2017)
lamb2018 <- process_and_add_year("lamb2018r.csv", 2018)
lamb2019 <- process_and_add_year("lamb2019r.csv", 2019)
lamb2020 <- process_and_add_year("lamb2020r.csv", 2020)
lamb2021 <- process_and_add_year("lamb2021r.csv", 2021)
lamb2022 <- process_and_add_year("lamb2022r.csv", 2022)

# Combine all datasets into one
combined_lamb <- bind_rows(lamb2016, lamb2017, lamb2018, lamb2019, lamb2020, lamb2021, lamb2022)

# Create a unique LambID for each unique No.
combined_lamb <- combined_lamb %>%
  group_by(No.) %>%
  mutate(LambID = paste0("Lamb", cur_group_id())) %>%
  ungroup()

# Save the combined dataset
write_csv(combined_lamb, "combined_lamb.csv")

# Check the structure of the combined data
str(combined_lamb)
#----------------------------------------------------------------------------------------------------#