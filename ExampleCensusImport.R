require(tidyverse)
require(tidycensus)

#### Use (install =TRUE) option to save the api key to R environment
census_api_key("YOUR API KEY HERE", install = T)

#### Loads a table of variables for the American Community Survey of a certain year
vacs17<-load_variables(2017, "acs5/profile")

#######   Example call for data profiles  #########
ma_state_bg<-get_acs(
  geography = "tract", # size of municipalities to zoom in on
  variables = "DP02_0112PE",
  state = '25', # optional: State 25 is MA
  county = '25',
  year=2018,
  survey = "acs5" # survey you are choosing from (also supports 'acs5')
)
