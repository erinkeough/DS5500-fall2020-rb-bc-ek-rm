require(tidyverse)
require(tidycensus)
require(tmap)

#### Use (install =TRUE) option to save the api key to R environment
census_api_key("YOUR API KEY HERE", install = T)

#### Loads a table of variables for the American Community Survey of a certain year
vacs19<-load_variables(2019, "acs1/profile")
vacs13<-load_variables(2010, "acs1/profile")

bos_long<--71.057083
bos_lat<-42.361145

### '11 variables in order ( total pop, 
######                       median income, Ed. Attainment: <9th, 9th-12th, HS Dip., some col, assoc deg, bach or higher, 
######                       white, black, Nat. Am., asian, HI or PAI,
######                       lang at home eng only, lang at home other, lang other than eng and eng not spoken 'well')
acs_vars<-c(#"B01001_001E",
  "DP03_0062E", "DP02_0059E","DP02_0060E","DP02_0061E","DP02_0062E", "DP02_0063E",
  "DP05_0032E", "DP05_0033E","DP05_0034E", "DP05_0039E", "DP05_0047E", 
  "DP02_0111PE", "DP02_0113E", "DP02_0114E")


#######  call for data profiles  #########
#### cannot make a call to different API's at same time (ie different prefixes of variables)
bos_acs10<-get_acs(
  geography = "tract", # size of municipalities to zoom in on
  variables = acs_vars,
  state = 'MA', # optional: State 25 is MA
  county = 'Suffolk', # Suffolk county is '025'
  year=2010,
  geometry = T, # for map visualizations
  survey = "acs5", # survey you are choosing from (also supports 'acs5')
  output = "wide"
)
bos_acs10_pop<-get_acs(
  geography = "tract", 
  variables = "B01001_001E",
  state = 'MA',
  county = 'Suffolk', 
  year=2010,
  #geometry = T,
  survey = "acs5", 
  output = "wide"
)

bos10<-bos_acs10%>%
  select(-NAME)%>%
  left_join(bos_acs10_pop, by = "GEOID")%>%
  mutate(adjMHI = DP03_0062E*1.0935,
         percEng = DP02_0111PE, 
         percWhite = 100*DP05_0032E/B01001_001E)%>%
  mutate(GEOID = str_remove(bos_acs10$GEOID, "25025"))%>%
  filter(!str_detect(GEOID, "^9"))

### ( total pop, 
####  median income, Ed. Attainment: <9th, 9th-12th, HS Dip., some col, assoc deg, bach or higher, 
####  white, black, Nat. Am., asian, HI or PAI,
##### lang at home eng only, lang at home other, lang other than eng and eng not spoken 'well')
# acs_vars<-c(#"B01001_001E",
#   "DP03_0062E", "DP02_0059E","DP02_0060E","DP02_0061E","DP02_0062E", "DP02_0063E",
#   "DP05_0032E", "DP05_0033E","DP05_0034E", "DP05_0039E", "DP05_0047E", 
#   "DP02_0111PE", "DP02_0113E", "DP02_0114E")
bos10_trim<-bos10%>%
  mutate(tot_pop = B01001_001E, 
         med_income = DP03_0062E, ed_hs = DP02_0059E+DP02_0060E+DP02_0061E,
         rc_white =DP05_0032E, lang_eng = DP02_0111PE)%>%
  select(tot_pop, med_income, ed_hs, rc_white, lang_eng, percEng, percWhite)
  

write_csv(bos10_trim, "census-csv/bos_2010_trim.csv")

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

bos_acs18<-bos_acs18%>%
  select(-NAME)%>%
  left_join(bos_acs18_pop, by = "GEOID")%>%
  mutate(percEng = DP02_0111PE, percWhite = 100*DP05_0032E/B01001_001E)

################################
## TODO: convert 2012 $ into 2018 $ (inflation)
##       meaningful names for variable
##       merge tables with variable for year



##########################################################################
#######   General EDA Visualization of census tracts
##########################################################################

##### Med HH Income (2018 dollars)
tm_shape(bos_acs18)+
  tm_fill(col = "DP03_0062E",
          palette = "Greens",
          style = 'jenks',
          title = "Median HH\nIncome '18")+
  tm_borders(col = "darkgray")


tm_shape(bos_acs12)+
  tm_fill(col = "adjMHI",
          palette = "Blues",
          style = 'jenks',
          title = "Median HH\nIncome '12")+
  tm_borders(col = "darkgray")

############### Perc Eng at Home 

tm_shape(bos_acs18)+
  tm_fill(col = "percEng",
          palette = "Greens",
          style = 'jenks',
          title = "% Eng. Only HH '18")+
  tm_borders(col = "darkgray")

tm_shape(bos_acs12)+
  tm_fill(col = "percEng",
          palette = "Blues",
          style = 'jenks',
          title = "% Eng. Only HH '12")+
  tm_borders(col = "darkgray")
