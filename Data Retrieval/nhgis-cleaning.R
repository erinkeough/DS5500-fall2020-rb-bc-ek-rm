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


rel90_00<-read_csv("census-csv/suffrelationship9000.txt", col_names = F)%>%
  mutate(
    TRACT90 = str_sub(X1, 6,11),
    PART90 = ifelse(str_sub(X1, 12,12)=="P", "P", "W"),
    POP90 = as.numeric(str_sub(X1, 13, 21)),
    POPPCT90 = as.numeric(str_sub(X1, 22,25))/10,
    TRACT00 = str_sub(X1, 31,36),
    PART00 = ifelse(str_sub(X1, 37,37)=="P", "P", "W"),
    POP00 = as.numeric(str_sub(X1, 38,46)),
    POPPCT00 = as.numeric(str_sub(X1, 47,50))/10
  )%>%
  select(-X1)%>%filter(TRACT00!="000000")




# #Any tracts not included in these lists were ineligible to gentrify for both time periods
# gentrified_90_00 <- c(304, 608, 605.01, 604, 1205)
# gentrified_00_10 <- c(103, 102.03, 104.08, 1207, 818, 612, 907, 804.01, 1401.07, 502, 503, 512)
# 
# eligible_90_00 <- c(612, 907, 804.01, 1401.07, 502, 503, 512, 1402.01, 1402.02, 9807, 1401.05, 1105.02, 9811, 1011.02, 1101.03, 1202.01, 924, 1005, 1006.01, 1007, 1006.03, 920, 917, 916, 915, 902, 815, 812, 814, 611.01, 704.02, 701.01, 8.03, 507, 1401.02, 1403, 1401.06, 1104.01, 1102.01, 1010.01, 1010.02, 921.01, 1011.01, 1002, 1003, 1001, 923, 919, 918, 911, 903, 914, 904, 820, 811, 806.01, 805, 817, 803, 801, 906, 913, 504, 506, 505, 501.01, 509.01, 510)
# eligible_00_10 <- c(5.02, 5.03, 5.04, 7.01, 7.04, 104.05, 1009, 1404, 1304.06, 1304.04, 1009, 1401.02, 1403, 1401.06, 1104.01, 1102.01, 1010.01, 1010.02, 921.01, 1011.01, 1002, 1003, 1001, 923, 919, 918, 911, 903, 914, 904, 820, 811, 806.01, 805, 817, 803, 801, 906, 913, 504, 506, 505, 501.01, 509.01, 510)
# 
# ineligible_90_00 <- c(103, 102.03, 104.08, 1207, 818, 5.02, 5.03, 5.04, 7.01, 7.04, 104.05, 1009, 1404, 1304.06, 1304.04, 1009)
# ineligible_00_10 <- c(304, 608, 605.01, 604, 1205, 1402.01, 1402.02, 9807, 1401.05, 1105.02, 9811, 1011.02, 1101.03, 1202.01, 924, 1005, 1006.01, 1007, 1006.03, 920, 917, 916, 915, 902, 815, 812, 814, 611.01, 704.02, 701.01, 8.03, 507)