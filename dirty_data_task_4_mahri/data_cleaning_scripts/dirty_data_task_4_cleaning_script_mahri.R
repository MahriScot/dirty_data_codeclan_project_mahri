#loading libraries
library(tidyverse)
library(readxl)

#loading in data from excel sheets
candy_2015 <- read_excel("../dirty_data_task_4_mahri/raw_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_excel("../dirty_data_task_4_mahri/raw_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_excel("../dirty_data_task_4_mahri/raw_data/boing-boing-candy-2017.xlsx")

#library for cleaning
library(janitor)

# janitor used for cleaning column names
janitor_candy_2015 <- janitor::clean_names(candy_2015)
janitor_candy_2015

janitor_candy_2016 <- janitor::clean_names(candy_2016)
janitor_candy_2016

janitor_candy_2017 <- janitor::clean_names(candy_2017)
janitor_candy_2017



#REMOVING AND RENAMEING for 2015, 2016, then 2017 
# Removing unnecessary columns and adding a year column (for reference once rows 
# are bound) and an id column (for practice).
# Renaming so they match when binding rows/ make more sense to the reader

#2015 remove
col_removed_candy_2015 <- janitor_candy_2015 %>% 
  select(-c(116:124), -c(97:113), -c(93:95), -c(90, 91), 
         -c(peterson_brand_sidewalk_chalk, spotted_dick, mint_leaves, 
            joy_joy_mit_iodine, minibags_of_chips, lapel_pins, kale_smoothie, 
            hugs_actual_physical_hugs, heath_bar, healthy_fruit, 
            creepy_religious_comics_chick_tracts, broken_glow_stick, 
            glow_sticks, generic_brand_acetaminophen, dental_paraphenalia, 
            cash_or_other_forms_of_legal_tender,
            vials_of_pure_high_fructose_corn_syrup_for_main_lining_into_your_vein, 
            box_o_raisins, timestamp)) %>% 
  add_column(year = "2015", .before = 1) %>% 
  mutate(id_number = row_number(), .before = 2)

#2015 rename
candy_2015_renamed <- col_removed_candy_2015 %>% 
  rename(age = how_old_are_you, 
         trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself,
         anonymous_black_and_orange_wrapper = 
           anonymous_brown_globs_that_come_in_black_and_orange_wrappers, 
         brach_not_including_candy_corn = brach_products_not_including_candy_corn, 
         restaurant_candy = 
           candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants, 
         hersheys_dark_chocolate = dark_chocolate_hershey, 
         gummy_bears = gummy_bears_straight_up, 
         hersheys_kissables = hershey_s_kissables, 
         hersheys_milk_chocolate = hershey_s_milk_chocolate, 
         licorice_black = licorice, 
         reeses_peanut_butter_cups = reese_s_peanut_butter_cups, 
         toblerone = tolberone_something_or_other, 
         peanut_m_ms = peanut_m_m_s, 
         chick_o_stick = chick_o_sticks_we_don_t_know_what_that_is, 
         circus_peanuts = those_odd_marshmallow_circus_peanut_things, 
         sea_salt_chocolate = 
           sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year)

# 2016 remove
col_removed_candy_2016 <- janitor_candy_2016 %>% 
  select(-c(104, 105, 107:123), 
         -c(vicodin, vials_of_pure_high_fructose_corn_syrup_for_main_lining_into_your_vein, 
            trail_mix, spotted_dick,
            person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes,
            minibags_of_chips, kale_smoothie, joy_joy_mit_iodine, hugs_actual_physical_hugs, 
            heath_bar, healthy_fruit, glow_sticks, generic_brand_acetaminophen, 
            dental_paraphenalia, creepy_religious_comics_chick_tracts, chardonnay,
            cash_or_other_forms_of_legal_tender, broken_glow_stick, boxo_raisins, 
            bonkers_the_board_game, timestamp)) %>% 
  add_column(year = "2016", .before = 1) %>% 
  mutate(id_number = max(candy_2015_renamed$id_number) + row_number(), .before = 2)

# 2016 rename 
candy_2016_renamed <- col_removed_candy_2016 %>% 
  rename(trick_or_treating = 
           are_you_going_actually_going_trick_or_treating_yourself, 
         gender = your_gender, 
         age = how_old_are_you, 
         country = which_country_do_you_live_in, 
         state_or_prov = which_state_province_county_do_you_live_in, 
         anonymous_black_and_orange_wrapper = 
           anonymous_brown_globs_that_come_in_black_and_orange_wrappers, 
         bonkers = bonkers_the_candy, 
         restaurant_candy = 
           candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants, 
         chick_o_stick = chick_o_sticks_we_don_t_know_what_that_is, 
         gummy_bears = gummy_bears_straight_up, 
         hersheys_milk_chocolate = hershey_s_milk_chocolate, 
         licorice_black = licorice_yes_black, 
         peanut_m_ms = peanut_m_m_s, 
         party_bag_m_ms = third_party_m_ms, 
         reeses_peanut_butter_cups = reese_s_peanut_butter_cups, 
         sourpatch_kids = sourpatch_kids_i_e_abominations_of_nature, 
         sweetarts = sweet_tarts, 
         sweetums = sweetums_a_friend_to_diabetes, 
         circus_peanuts = those_odd_marshmallow_circus_peanut_things, 
         toblerone = tolberone_something_or_other)

# 2017 Remove
col_removed_candy_2017 <- janitor_candy_2017 %>%  
  select(-c(102, 104, 105, 107, 108, 110:120), 
         -c(q6_spotted_dick, 
            q6_sandwich_sized_bags_filled_with_boo_berry_crunch,
            q6_real_housewives_of_orange_county_season_9_blue_ray, 
            q6_minibags_of_chips, 
            q6_abstained_from_m_ming, 
            q6_kale_smoothie, q6_joy_joy_mit_iodine, 
            q6_hugs_actual_physical_hugs, 
            q6_heath_bar, 
            q6_healthy_fruit, 
            q6_glow_sticks, 
            q6_generic_brand_acetaminophen, 
            q6_dental_paraphenalia, 
            q6_creepy_religious_comics_chick_tracts, 
            q6_chardonnay, 
            q6_cash_or_other_forms_of_legal_tender, 
            q6_broken_glow_stick, 
            q6_boxo_raisins, 
            q6_bonkers_the_board_game, 
            internal_id)) %>% 
  add_column(year = "2017", .before = 1) %>% 
  mutate(id_number = max(candy_2016_renamed$id_number) + row_number(), .before = 2)


# 2017 rename
# taking the "q1-6_" out of column names first
candy_2017_q_removed <- col_removed_candy_2017 %>% 
  rename_all(~ sub("^[q0-9]{2}_", "", 
                   make.names(names(col_removed_candy_2017))))

candy_2017_renamed <- candy_2017_q_removed %>% 
  rename(trick_or_treating = going_out, 
         state_or_prov = state_province_county_etc, 
         x100_grand_bar = `100_grand_bar`, 
         mary_janes = 
           anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes, 
         bonkers = bonkers_the_candy, 
         restaurant_candy = 
           candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants, 
         chick_o_stick = chick_o_sticks_we_don_t_know_what_that_is, 
         gummy_bears = gummy_bears_straight_up, 
         hersheys_milk_chocolate = hershey_s_milk_chocolate, 
         licorice_black = licorice_yes_black, 
         peanut_m_ms = peanut_m_m_s, 
         green_m_ms = green_party_m_ms, 
         lone_m_ms = independent_m_ms, 
         reeses_peanut_butter_cups = reese_s_peanut_butter_cups, 
         sourpatch_kids = sourpatch_kids_i_e_abominations_of_nature, 
         sweetarts = sweet_tarts, 
         sweetums = sweetums_a_friend_to_diabetes, 
         circus_peanuts = those_odd_marshmallow_circus_peanut_things, 
         toblerone = tolberone_something_or_other)



# Joining all three years by binding rows so as to keep everything
bound_candy <- bind_rows(candy_2015_renamed, 
                         candy_2016_renamed, 
                         candy_2017_renamed)

bound_candy <- bound_candy %>% 
  relocate(country, .before = 5) %>% 
  relocate(state_or_prov, .before = 6) %>% 
  relocate(gender, .before = 7)



# pivoting longer to see candy against it's individual rating
total_ratings <- bound_candy %>% 
  select(-c(year, id_number, age, 
            trick_or_treating, country, 
            state_or_prov, gender)) %>%
  pivot_longer(butterfinger:take_5, 
               names_to = "candy", 
               values_to = "rating")



# cleaning the age column (previously a character variable)
bound_age_to_numeric <- bound_candy %>% 
  mutate(age = as.integer(age)) %>% 
  arrange(age)

# unlikely anyone under 100 years old is going out trick or treating so 
# removing them here
bound_age_cleaning <- bound_age_to_numeric %>%
  mutate(age = if_else(age > 99, NA_integer_, age))


# load library for stringr, regex
library(stringr)

# regex etc for cleaning the country column - there are lots of notes on this in 
# the RMD analysis file

bound_country_clean <- bound_age_cleaning %>% 
  mutate(country = str_replace_all(country, pattern = 
                                     "[0-9][0-9][.][0-9]", "States"),
         country = str_replace_all(country, pattern = 
                                     "[3|4|5][0-9]", "States"),
         country = str_replace_all(country, pattern = "[uU][a-zA-Z.]* [sS][a-zA-Z]* [oO][fF] [Aa]*[a-zA-Z]*", "States"),
         country = str_replace_all(country, pattern = "[ .]*[uU]+[a-zA-Z.]* [sS]+[a-zA-Z]*[ .]*", "States"),
         country = str_replace_all(country, pattern = "[ ]*[uU]+[ .!]*[sS]+[ .!]*[aA]*[ .!]*", "States"),
         # the above changes austria and australia - watch out!
         country = str_replace_all(country, pattern = "[uU]+[nN]+[a-z]* [sS]+[tT][aA][tT][a-z]* [oO]+[fF] [aA][mM][a-z]*\t*", "States"),
         # there's still on "United States of America" which won't go - spaces?
         country = str_replace_all(country, pattern = "^[mM][uU|eE][rR]+[iI][cC|kK][aA]", "States"),
         country = str_replace_all(country, pattern = "^[aA][mM][eE][rR][iI][cC][aA]", "States"),
         country = str_replace_all(country, pattern = "^\\'[mM][uU|eE][rR][iI][cC][aA]", "States"),
         country = str_replace_all(country, pattern = "[Tthe]*[ ]*[sS]+[tT][aA][tT][a-z]*[Ss][tT][a-z]*[Ss]*[tT]*[a-z]*[Ss]*[tT]*[a-z]*", "States"),
         # removing States repeated
         country = str_replace_all(country, pattern = "[Tt][a-z]* [bB][a-z]* [a-zA-Z]+ [-]+ [uU][sS][aA]", "States"),
         # this was an attempt at "the best one - USA"
         country = str_replace_all(country, pattern = "[nN][a-z]+ [cC|yY|jJ]+[a-z]*|[pP|tT]+[iI|rR]+[tT|uU]+[a-z]+|[cC][aA][lL][iI][a-z]*|[aA][lL][aA][a-z]*", "States"),
         # The above is for "New York, North Carolina, New Jersey, Pittsburgh, 
         # Trumpistan, California, and Alaska (it also changed the end of New Zealand to New Zestates...)
         country = str_replace_all(country, pattern = "Ahem....Amerca", "States"),
         country = str_replace_all(country, pattern = "^[cC]+[aA]+[nN]+[aA]+[dD|eE]+[aA]*[aA|iI|rR]*[aA|nN]*[iI]*[aA]*[`]*|[s][o][v][a-z]* [cC][a-z]*", "Canada"),
         country = str_replace_all(country, pattern = "^[eE]+[nN]+[a-zA-Z]*|^[sS]+[cC]+[a-zA-Z]*|[uU]+[nN]+[a-zA-Z]* [kK]+[a-zA-Z]*", "United Kingdom"),
         country = str_replace_all(country, pattern = "^[eE]+[nN]+[a-zA-Z]*|^[sS]+[cC]+[a-zA-Z]*|[uU]+[nN]+[a-zA-Z]* [kK]+[.]*[a-zA-Z]*|[uU]+[.]*[kK]+[.]*", "United Kingdom")
  )

bound_country_clean %>% 
  distinct(country)


# Writing clean data to a csv file
# write_csv(candy_combined_clean , "clean_data/candy_clean.csv")