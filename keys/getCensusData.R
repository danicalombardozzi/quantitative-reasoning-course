# collect census data for data vis lesson

library(tidyverse)
library(tidycensus)


census_api_key("32c2220d9742d12204a87074b475e571a00fc0df", install = TRUE)

# search for variables
v20 <- load_variables(2020, "acs5", cache = TRUE)

View(v20)


# get income and age
larimer_income <- get_acs(
  geography = "tract",
  variables = c(median_age = "B01002_001",
                median_income = "B19013_001"),
  year = 2021,
  survey = "acs5",
  state = "CO",
  county = "Larimer",
  output = "wide"
) %>% 
  select(GEOID, NAME, median_income = median_incomeE, median_age = median_ageE)




# get race and ethnicity data
race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012"
)

larimer_race <- get_acs(
  geography = "tract",
  state = "CO",
  county = "Larimer",
  variables = race_vars,
  summary_var = "B03002_001"
  #year = 2020
) 


# calculate percent
larimer_race_percent <- larimer_race %>%
  mutate(race_percent = 100 * (estimate / summary_est)) %>%
  select(GEOID, NAME, variable, race_percent)

# calculate % non-white
larimer_race <- larimer_race_percent %>% 
  group_by(GEOID) %>% 
  summarise(non_white_percent = sum(race_percent[variable != "White"])) %>% 
  left_join(larimer_race_percent) %>% 
  select(GEOID, NAME, everything()) %>% 
  pivot_wider(names_from = variable, values_from = race_percent)


#join all together to get census data for Larimer and save for data viz exercise
larimer_census <- full_join(larimer_income, larimer_race, by = c("GEOID", "NAME"))

