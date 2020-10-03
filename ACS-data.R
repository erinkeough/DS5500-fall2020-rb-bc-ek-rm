require(tidyverse)
require(tidycensus)
require(tmap)

#### Use (install =TRUE) option to save the api key to R environment
census_api_key("YOUR API KEY HERE", install = T)

#### Loads a table of variables for the American Community Survey of a certain year
vacs19<-load_variables(2019, "acs1/profile")
vacs13<-load_variables(2013, "acs1/profile")

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


#######  call for data profiles  #########
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
  geometry = T,
  survey = "acs5", 
  output = "wide"
)

bos_acs12<-bos_acs12_pop%>%
  select(-NAME)%>%
  left_join(bos_acs12, by = "GEOID")

##### getting 2019 data
bos_acs18<-get_acs(
  geography = "tract", 
  variables = acs_vars,
  state = 'MA',
  county = 'Suffolk', 
  year=2018,
  geometry = T, 
  survey = "acs5",
  output = "wide"
)

bos_acs18_pop<-get_acs(
  geography = "tract",
  variables = "B01001_001E",
  state = 'MA',
  county = 'Suffolk',
  year=2018,
  survey = "acs5",
  output = "wide"
)

bos_acs18<-bos_acs18_pop%>%
  select(-NAME)%>%
  left_join(bos_acs18, by = "GEOID")

################################
## TODO: convert 2012 $ into 2018 $ (inflation)
##       meaningful names for variable
##       merge tables with variable for year
##       convert columns to relevent percentages



##########################################################################
#######   General EDA Visualization of census tracts
##########################################################################

### Smaller tables for visualization
# Med HH income '18
mhi18<-get_acs(
  geography = "tract",
  variables = "DP03_0062E",
  state = 'MA',
  county = 'Suffolk',
  year=2018,
  geometry = T,
  survey = "acs5",
  output = "wide"
)

tm_shape(mhi18)+
  tm_fill(col = "DP03_0062E",
          palette = "Greens",
          style = 'jenks',
          title = "Median HH\nIncome '18")+
  tm_borders(col = "darkgray")


# Med HH income '12
mhi12<-get_acs(
  geography = "tract",
  variables = "DP03_0062E",
  state = 'MA',
  county = 'Suffolk',
  year=2012,
  geometry = T,
  survey = "acs5",
  output = "wide"
)

tm_shape(mhi12)+
  tm_fill(col = "DP03_0062E",
          palette = "Blues",
          style = 'jenks',
          title = "Median HH\nIncome '12")+
  tm_borders(col = "darkgray")

# Bach or higher Ed '18
bach18<-get_acs(
  geography = "tract",
  variables = "DP02_0067E",
  state = 'MA',
  county = 'Suffolk',
  year=2018,
  geometry = T,
  survey = "acs5",
  output = "wide"
)

tm_shape(bach18)+
  tm_fill(col = "DP02_0067E",
          palette = "Greens",
          style = 'jenks',
          title = "Bach. or Higher\nEduc '18")+
  tm_borders(col = "darkgray")

# Bach or higher Ed '12
bach12<-get_acs(
  geography = "tract",
  variables = c("DP02_0065E","DP02_0064E"),
  state = 'MA',
  county = 'Suffolk',
  year=2012,
  geometry = T,
  survey = "acs5",
  output = "wide"
)
bach12$bachUp<-bach12$DP02_0065E+bach12$DP02_0064E
tm_shape(bach12)+
  tm_fill(col = "bachUp",
          palette = "Blues",
          style = 'jenks',
          title = "Bach. or Higher\nEduc '12")+
  tm_borders(col = "darkgray")



