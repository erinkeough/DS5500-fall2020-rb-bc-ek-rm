require(tidyverse)
require(tidycensus)

#### Use (install =TRUE) option to save the api key to R environment
census_api_key("YOUR API KEY HERE", install = T)

#### Loads a table of variables for the American Community Survey of a certain year
vacs19<-load_variables(2019, "acs1/profile")
vacs11<-load_variables(2011, "acs1/profile")

bos_long<--71.057083
bos_lat<-42.361145

### '11 variables in order ( total pop, 
######                       median income, Ed. Attainment: <9th, 9th-12th, HS Dip., some col, assoc deg, bach or higher, 
######                       white, black, Nat. Am., asian, HI or PAI,
######                       lang at home eng only, lang at home other, lang other than eng and eng not spoken 'well')
acs_vars<-c(#"B01001_001E",
            "DP03_0062E", "DP02_0059E","DP02_0060E","DP02_0061E","DP02_0062E", "DP02_0063E", "DP02_0067E",
            "DP05_0032E", "DP05_0033E","DP05_0034E", "DP05_0039E", "DP05_0047E", 
            "DP02_0112E", "DP02_0113E", "DP02_0114E")
#######   Example call for data profiles  #########
#### cannot make a call to different API's at same time (ie different prefixes of variables)
bos_acs12<-get_acs(
  geography = "tract", # size of municipalities to zoom in on
  variables = acs_vars,
  state = 'MA', # optional: State 25 is MA
  county = 'Suffolk', # Suffolk county is '025'
  year=2012,
  geometry = T, # for map visualizations
  survey = "acs5", # survey you are choosing from (also supports 'acs5')
  output = "wide"
)
bos_acs12_pop<-get_acs(
  geography = "tract", 
  variables = "B01001_001E",
  state = 'MA',
  county = 'Suffolk', 
  year=2012,
  survey = "acs5", 
  output = "wide"
)

bos_acs12<-bos_acs12_pop%>%
  select(-NAME)%>%
  left_join(bos_acs12, by = "GEOID")
##### getting 2019 data
bos_acs19<-get_acs(
  geography = "tract", 
  variables = acs_vars,
  state = 'MA',
  county = 'Suffolk', 
  year=2012,
  geometry = T, 
  survey = "acs5",
  output = "wide"
)

bos_acs19_pop<-get_acs(
  geography = "tract",
  variables = "B01001_001E",
  state = 'MA',
  county = 'Suffolk',
  year=2019,
  survey = "acs5",
  output = "wide"
)

bos_acs19<-bos_acs19_pop%>%
  select(-NAME)%>%
  left_join(bos_acs19, by = "GEOID")








