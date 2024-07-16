
# Setup Environment ----
library(tidyverse)
setwd("C:\\Users\\MichaelBrucek\\Desktop\\HSD Task")

# Read Data ----
data = read.csv("boost_df.csv")



# Create Features and Clean Data
df_2 = data %>%
  mutate(timestamp = lubridate::as_datetime(timestamp),
         trip_start_date = lubridate::as_datetime(trip_start_date),
         hidden_charges = total_price - base_plus_boost,  # Reveal hidden charges (base + boost does not always == total_price)
         hours_from_boost_to_ride = as.numeric(trip_start_date - timestamp) * 24,
         boost_amount_pct_of_total = (boost_amount / total_price) * 100,
         boost_pct_increase = ifelse(boost_number < 2, 0, (boost_amount - lag(boost_amount)) / lag(boost_amount) * 100),
         boost_pct_increase = ifelse(is.na(boost_pct_increase), 0, boost_pct_increase),
         # Hidden charge doesn't kick in until after the 2nd boost                            
         hidden_charge_pct_of_total = (hidden_charges / total_price) * 100,
         hidden_charge_pct_increase = ifelse(boost_number < 3, 0, (hidden_charges - lag(hidden_charges)) / lag(hidden_charges) * 100),
         hidden_charge_pct_increase = ifelse(is.na(hidden_charge_pct_increase), 0, hidden_charge_pct_increase)
    
  ) %>%
  # reorder for easier interpretation
  select(c(trip_id, metro_area, trip_start_date, timestamp, hours_from_boost_to_ride,
           boost_number, base_price, boost_amount, boost_pct_increase, boost_amount_pct_of_total, 
           base_plus_boost, base_plus_boost, hidden_charges, hidden_charge_pct_increase, hidden_charge_pct_of_total, 
           total_price, claimed, total_driver_supply))


 
# Aggregate the data at the trip level to match the level of prediction ----

# Add Trip-Level Details
trip_summary_stats = df_2 %>%
  group_by(trip_id) %>% 
  mutate( post_lead_time = max(hours_from_boost_to_ride),
          total_price = max(total_price),
          price_change = max(total_price) - min(base_price),
          
          total_boosts = max(boost_number),  # acts as proxy for time lapse since all boosts are 5 mins
          largest_boost = max(boost_amount),
          largest_boost_pct_increase = max(boost_pct_increase),
          step_of_largest_boost_pct_increase = max(boost_number[boost_pct_increase==largest_boost_pct_increase]),
          largest_boost_pct_of_total = max(boost_amount_pct_of_total),
          step_of_largest_boost_pct_of_total = max(boost_number[boost_amount_pct_of_total==largest_boost_pct_of_total]),
            
          count_negative_boosts = sum(boost_pct_increase < 0),
          largest_negative_boost = min(boost_pct_increase),
          step_of_largest_negative_boost = max(boost_number[boost_pct_increase==largest_negative_boost]),
 
          largest_hidden_charge = max(hidden_charges),
          largest_hidden_charge_increase = max(hidden_charge_pct_increase),
          largest_hidden_charge_pct_of_total = max(hidden_charge_pct_of_total),
          step_of_largest_hidden_charge_pct_of_total = max(boost_number[hidden_charge_pct_of_total==largest_hidden_charge_pct_of_total]),
          
          min_driver_supply = min(total_driver_supply),
          step_of_min_driver_supply = max(boost_number[total_driver_supply==min_driver_supply]),
          max_driver_supply = max(total_driver_supply),
          step_of_max_driver_supply = max(boost_number[total_driver_supply==max_driver_supply])
        ) %>%
  ungroup() 


# Roll up trips to the accepted rides
claimed_trip_df = trip_summary_stats %>%
  filter(claimed == 1) %>%
  # Covert Date to numeric,
  mutate(trip_start_year = year(trip_start_date),
         trip_start_month = month(trip_start_date),
         trip_start_day = day(trip_start_date),
         trip_start_hour = hour(trip_start_date),
         trip_start_minute = minute(trip_start_date)) %>%
  # Drop unused features and reorder for easier interpretation
  select(trip_id, metro_area, 
         trip_start_year, trip_start_month, trip_start_day, trip_start_hour, trip_start_minute,
         hours_from_boost_to_ride,
         base_price, total_price, price_change,

         boost_number, boost_amount, boost_pct_increase, boost_amount_pct_of_total, 
         total_boosts, largest_boost, largest_boost_pct_increase, step_of_largest_boost_pct_increase, 
         largest_boost_pct_of_total, step_of_largest_boost_pct_of_total, count_negative_boosts, 
         largest_negative_boost, step_of_largest_negative_boost, 
         
         hidden_charges, hidden_charge_pct_increase, hidden_charge_pct_of_total, largest_hidden_charge,
         largest_hidden_charge_increase, largest_hidden_charge_pct_of_total, step_of_largest_hidden_charge_pct_of_total,
         
         total_driver_supply, min_driver_supply, step_of_min_driver_supply, max_driver_supply, step_of_max_driver_supply)












