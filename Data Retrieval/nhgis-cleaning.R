require(tidyverse)

## Suffolk county Standard data from NHGIS (not the Nominal Data)
suff_std <-read_csv("census-csv/census_suffolk_standardized.csv")

## Suffolk county Nominal data from NHGIS 
suff_nom<-read_csv("~/Desktop/census_suffolk_nominal.csv")%>%select(-X1)

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


nom_translate_9000<-tract_translate(suff_nom, rel90_00,"1990", "2000")
nom_translate_0010<-tract_translate(suff_nom, rel00_10, "2000", "2012")


#####
# Convert nominal 90 - 00
#  1) filter table with just 90 data \/
#  2) create 90A (table with '00 tract codes and 90 data columns)
#  3) Fill 90A according to rel90_00

# 1)
nom90<-suff_nom%>%
  filter(!is.na(GIS.Join.Match.Code..1990))%>%
  select(contains("1990"))%>%
  select(-Area.Name..1990)%>%
  mutate(TRACT90 = str_sub(GIS.Join.Match.Code..1990,9),
         TRACT90 = str_pad(TRACT90, 6, side = "right",pad="0"))

############
# 2)
nom90B<-suff_nom%>%
  select(GIS.Join.Match.Code..2000)%>%
  filter(!is.na(GIS.Join.Match.Code..2000))

for(newcol in colnames(nom90)){
  nom90B[[newcol]]<-0
}
nom90B<-nom90B%>%
  select(-GIS.Join.Match.Code..1990,-TRACT90)%>%
  mutate(TRACT00 = str_sub(GIS.Join.Match.Code..2000,9),
         TRACT00 = str_pad(TRACT00, 6, side = "right",pad="0"))%>%
  filter(TRACT00 != "000000")

############
# 3)
for(row in 1:nrow(rel90_00)){
  t90<-rel90_00$TRACT90[row]
  t00<-rel90_00$TRACT00[row]
  pct<-rel90_00$POPPCT90[row]
  
  ## Skip first and last column 
  ## First col is tract id, last column is fixed tract id
  for(column in 2:ncol(nom90)){
    if(column == ncol(nom90)){next()}
    nom90B[nom90B$TRACT00==t00, column]<-nom90B[nom90B$TRACT00==t00, column]+
                                            nom90[nom90$TRACT90==t90, column]*pct
  }
  
}