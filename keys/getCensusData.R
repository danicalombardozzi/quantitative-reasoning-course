# collect census data for data vis lesson

library(tidyverse)
library(tidycensus)


census_api_key("32c2220d9742d12204a87074b475e571a00fc0df", install = TRUE)


larimer_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  #year = NULL,
  survey = "acs5",
  state = "CO",
  county = "Larimer",
  geometry = TRUE
)
