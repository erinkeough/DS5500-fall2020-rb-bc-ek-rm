require(tidyverse)

## Suffolk county Nominal data from NHGIS 
suff_nom<-read_csv("census-csv/census_suffolk_nominal.csv")%>%select(-X1)

rel00_10<-read_csv("census-csv/marelationship0010.csv", col_types = "ccccddcddccccddcdddddddddddddd")%>%
  filter(COUNTY00 == "025", COUNTY10 == "025")%>%
  select(-contains("STATE"),
         -contains("COUNTY"),
         -contains("GEOID"))%>%
  rename(TRACT12=TRACT10)


rel90_00 <-
  read_csv("census-csv/suffrelationship9000.txt", col_names = F) %>%
  mutate(
    TRACT90 = str_sub(X1, 6, 11),
    PART90 = ifelse(str_sub(X1, 12, 12) == "P", "P", "W"),
    POP90 = as.numeric(str_sub(X1, 13, 21)),
    POPPCT90 = as.numeric(str_sub(X1, 22, 25)) / 1000,
    TRACT00 = str_sub(X1, 31, 36),
    PART00 = ifelse(str_sub(X1, 37, 37) == "P", "P", "W"),
    POP00 = as.numeric(str_sub(X1, 38, 46)),
    POPPCT00 = as.numeric(str_sub(X1, 47, 50)) / 1000
  ) %>%
  select(-X1) %>% filter(TRACT00 != "000000", TRACT90 != "000000")

####
## For 90 tracts - Translate from 90-00, Then from 00-10
## For 00 tracts - Translate from 00-10
## For 10 tracts - subset original df
##
## Merge three dfs on tract id for final df


nom90_as_00<-tract_translate(suff_nom, rel90_00,"1990", "2000")
nom90_as_10<-tract_translate(suff_nom, rel00_10, "2000","2012", subset_data = nom90_as_00)
nom00_as_10<-tract_translate(suff_nom, rel00_10, "2000", "2012")
nom10_as_10<-suff_nom%>%filter(!is.na(GIS.Join.Match.Code..2012))%>%
  select(contains("2012"))%>%
  mutate(TRACT12 = str_sub(GIS.Join.Match.Code..2012,9))
  
translated_nominal<-nom90_as_10%>%
  full_join(nom00_as_10)%>%full_join(nom10_as_10)

write_csv(translated_nominal, "census-csv/census_nominal_translated2010.csv")
