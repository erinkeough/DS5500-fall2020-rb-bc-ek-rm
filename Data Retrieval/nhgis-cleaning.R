require(tidyverse)

## Suffolk county Standard data from NHGIS (not the Nominal Data)
suff_std <-read_csv("census-csv/census_suffolk_standardized.csv")

## Suffolk county Nominal data from NHGIS 
suff_nom<-read_csv("census-csv/census_suffolk_nominal.csv")

rel00_10<-read_csv("census-csv/marelationship0010.csv", col_types = "ccccddcddccccddcdddddddddddddd")%>%
  filter(COUNTY00 == "025")%>%
  select(-contains("STATE"),
         -contains("COUNTY"),
         -contains("GEOID"))


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

# #Any tracts not included in these lists were ineligible to gentrify for both time periods
# gentrified_90_00 <- c(304, 608, 605.01, 604, 1205)
# gentrified_00_10 <- c(103, 102.03, 104.08, 1207, 818, 612, 907, 804.01, 1401.07, 502, 503, 512)
# 
# eligible_90_00 <- c(612, 907, 804.01, 1401.07, 502, 503, 512, 1402.01, 1402.02, 9807, 1401.05, 1105.02, 9811, 1011.02, 1101.03, 1202.01, 924, 1005, 1006.01, 1007, 1006.03, 920, 917, 916, 915, 902, 815, 812, 814, 611.01, 704.02, 701.01, 8.03, 507, 1401.02, 1403, 1401.06, 1104.01, 1102.01, 1010.01, 1010.02, 921.01, 1011.01, 1002, 1003, 1001, 923, 919, 918, 911, 903, 914, 904, 820, 811, 806.01, 805, 817, 803, 801, 906, 913, 504, 506, 505, 501.01, 509.01, 510)
# eligible_00_10 <- c(5.02, 5.03, 5.04, 7.01, 7.04, 104.05, 1009, 1404, 1304.06, 1304.04, 1009, 1401.02, 1403, 1401.06, 1104.01, 1102.01, 1010.01, 1010.02, 921.01, 1011.01, 1002, 1003, 1001, 923, 919, 918, 911, 903, 914, 904, 820, 811, 806.01, 805, 817, 803, 801, 906, 913, 504, 506, 505, 501.01, 509.01, 510)
# 
# ineligible_90_00 <- c(103, 102.03, 104.08, 1207, 818, 5.02, 5.03, 5.04, 7.01, 7.04, 104.05, 1009, 1404, 1304.06, 1304.04, 1009)
# ineligible_00_10 <- c(304, 608, 605.01, 604, 1205, 1402.01, 1402.02, 9807, 1401.05, 1105.02, 9811, 1011.02, 1101.03, 1202.01, 924, 1005, 1006.01, 1007, 1006.03, 920, 917, 916, 915, 902, 815, 812, 814, 611.01, 704.02, 701.01, 8.03, 507)