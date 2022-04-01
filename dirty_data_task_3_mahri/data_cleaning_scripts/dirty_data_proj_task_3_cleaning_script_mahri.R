# loading relevant libraries 
library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(stringr)

#data read from excel files (only need the 3rd and 4th for reference)
ship_data_by_record <- read_excel("raw_data/seabirds.xls")
bird_data_by_record <- read_excel("raw_data/seabirds.xls", sheet = 2)

ship_data_codes <- read_excel("raw_data/seabirds.xls", sheet = 3)
bird_data_codes <- read_excel("raw_data/seabirds.xls", sheet = 4)


# using janitor to clean column names (lower case, separated by _ etc.)
ship_data_records <- janitor::clean_names(ship_data_by_record)
bird_data_records <- janitor::clean_names(bird_data_by_record)


# renaming columns to make it easier to read and selecting those that are relevant

bird_data_renamed <- bird_data_records %>%
  rename("species_common_name" = "species_common_name_taxon_age_sex_plumage_phase",
         "species_scientific_name" = "species_scientific_name_taxon_age_sex_plumage_phase",
         "birds_counted" = "count") %>% 
  select(record, 
         record_id, 
         species_common_name, 
         species_scientific_name, 
         species_abbreviation, 
         birds_counted)

ship_data_renamed <- ship_data_records %>% 
  rename("latitude" = "lat", 
         "longitude" = "long") %>% 
  select(record, 
         record_id, 
         date,
         latitude, 
         longitude)

# Using Left Join to join the sheets by their record IDs. 
# Rearranging column order.
# And keeping record numbers for both the ship and the bird data but naming both 
# more appropriately for my better understanding.


birds_and_ships_joined <- left_join(bird_data_renamed, 
                                    ship_data_renamed, by = "record_id") %>% 
  rename("record_num_birds"= record.x, "record_num_ship" = record.y)  %>% 
  relocate(birds_counted, .before = 1) %>% 
  relocate(record_num_birds, .before = 7)


# write tidy data to a csv vile:
write_csv(birds_and_ships_joined, "clean_data/birds_and_ships_clean.csv")