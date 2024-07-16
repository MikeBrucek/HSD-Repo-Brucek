
# Setup Environment ----
library(tidyverse)

setwd("C:\\Users\\MichaelBrucek\\Desktop\\HSD Task")

# Read Data ----
data = read.csv("boost_df.csv")


# Create Features and Clean Data
claimed_ride_feature_set = data %>%
  mutate(metro_area = as.numeric(as.factor(metro_area)),  # using xgboost so any number representation will do (no need for one hot)
         trip_start_year = year(trip_start_date),
         trip_start_month = month(trip_start_date),
         trip_start_day = day(trip_start_date),
         trip_start_hour = hour(trip_start_date),
         trip_start_minute = minute(trip_start_date),
         total_price_change = total_price - base_price,
         total_price_pct_increase = ifelse(boost_number > 0, (total_price - lag(total_price)) / lag(total_price) * 100, 0),
         hidden_charges = total_price - base_plus_boost,  # Reveal hidden charges (base + boost does not always == total_price)
         boost_amount_pct_of_total = (boost_amount / total_price) * 100,
         boost_pct_increase = ifelse(boost_number < 2, 0, (boost_amount - lag(boost_amount)) / lag(boost_amount) * 100),
         boost_pct_increase = ifelse(is.na(boost_pct_increase), 0, boost_pct_increase),
         # Hidden charge doesn't kick in until after the 2nd boost                            
         hidden_charge_pct_of_total = (hidden_charges / total_price) * 100,
         hidden_charge_pct_increase = ifelse(boost_number < 3, 0, (hidden_charges - lag(hidden_charges)) / lag(hidden_charges) * 100),
         hidden_charge_pct_increase = ifelse(is.na(hidden_charge_pct_increase), 0, hidden_charge_pct_increase),
         driver_supply_pct_increase = ifelse(boost_number > 0, (total_driver_supply - lag(total_driver_supply)) / lag(total_driver_supply) * 100, 0)
    
  ) %>%
  # reorder for easier interpretation
  select(# Trip metadata
         trip_id, claimed, metro_area, 
         trip_start_year, trip_start_month, trip_start_day, trip_start_hour, trip_start_minute,
         # High level price info
         base_price, total_price, total_price_change, total_price_pct_increase,
         # Boost info
         boost_number, boost_amount, boost_pct_increase, boost_amount_pct_of_total, 
         # Hidden charge info
         hidden_charges, hidden_charge_pct_increase, hidden_charge_pct_of_total,
         # Driver supply info
         total_driver_supply, driver_supply_pct_increase)


 
