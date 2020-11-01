#####    Creating labels Based on "governing.com"
######     https://www.governing.com/gov-data/boston-gentrification-maps-demographic-data.html#status

tract_year<-read_csv("census-csv/prop_change_suffolk.csv")%>%
  select(Data.Measurement.Year, Census.Tract.Code)%>%
  mutate(Census.Tract.Code = str_pad(as.character(Census.Tract.Code),width = 6, side = "left", pad = "0"))
  

# #Any tracts not included in these lists were ineligible to gentrify for both time periods
gentrified_90_00 <- c(304, 608, 605.01, 604, 1205)
gentrified_00_10 <- c(103, 102.03, 104.08, 1207, 818, 612, 907, 804.01, 1401.07, 502, 503, 512)

eligible_90_00 <- c(612, 907, 804.01, 1401.07, 502, 503, 512, 1402.01, 1402.02, 9807, 1401.05, 1105.02, 9811, 1011.02, 1101.03, 1202.01, 924, 1005, 1006.01, 1007, 1006.03, 920, 917, 916, 915, 902, 815, 812, 814, 611.01, 704.02, 701.01, 8.03, 507, 1401.02, 1403, 1401.06, 1104.01, 1102.01, 1010.01, 1010.02, 921.01, 1011.01, 1002, 1003, 1001, 923, 919, 918, 911, 903, 914, 904, 820, 811, 806.01, 805, 817, 803, 801, 906, 913, 504, 506, 505, 501.01, 509.01, 510)
eligible_00_10 <- c(5.02, 5.03, 5.04, 7.01, 7.04, 104.05, 1009, 1404, 1304.06, 1304.04, 1009, 1401.02, 1403, 1401.06, 1104.01, 1102.01, 1010.01, 1010.02, 921.01, 1011.01, 1002, 1003, 1001, 923, 919, 918, 911, 903, 914, 904, 820, 811, 806.01, 805, 817, 803, 801, 906, 913, 504, 506, 505, 501.01, 509.01, 510)

ineligible_90_00 <- c(103, 102.03, 104.08, 1207, 818, 5.02, 5.03, 5.04, 7.01, 7.04, 104.05, 1009, 1404, 1304.06, 1304.04, 1009)
ineligible_00_10 <- c(304, 608, 605.01, 604, 1205, 1402.01, 1402.02, 9807, 1401.05, 1105.02, 9811, 1011.02, 1101.03, 1202.01, 924, 1005, 1006.01, 1007, 1006.03, 920, 917, 916, 915, 902, 815, 812, 814, 611.01, 704.02, 701.01, 8.03, 507)

### Converting array of numeric tracts into character tracts.
convert_tract<-function(tracts){
  new_tracts<-c()
  i<-1
  for(tract in tracts){
    if(tract %% 1 == 0){
      new_tract<-paste0(as.character(tract),".00")
    }else{
      new_tract<-as.character(tract)
    }
    new_tract<-str_remove(new_tract, "\\.")
    new_tract<-str_pad(new_tract, 6, side = "left", pad="0")
    new_tracts[i]<-new_tract
    i<-i+1
  }
  return(new_tracts)
}

gent_90_00<-convert_tract(gentrified_90_00)
gent_00_10<-convert_tract(gentrified_00_10)
el_90_00<-convert_tract(eligible_90_00)
el_00_10<-convert_tract(eligible_00_10)
inel_90_00<-convert_tract(ineligible_90_00)
inel_00_10<-convert_tract(ineligible_00_10)

tract_label<-tract_year%>%mutate(Gentrified = ifelse(Data.Measurement.Year == "1990-2000" & Census.Tract.Code%in%gent_90_00,
                                        1, Data.Measurement.Year == "2000-2010" & Census.Tract.Code%in%gent_00_10),
                    Eligible = ifelse(Data.Measurement.Year =="1990-2000" & Census.Tract.Code%in%el_90_00,
                                      1, Data.Measurement.Year=="2000-2010"&Census.Tract.Code%in%el_00_10),
                    Ineligible = ifelse(Data.Measurement.Year=="1990-2000" & Census.Tract.Code%in%inel_90_00,
                                        1, Data.Measurement.Year=="2000-2010"&Census.Tract.Code%in%inel_00_10),
                    Gent_Label = ifelse(Gentrified, "Gentrified",
                                        ifelse(Eligible, "Eligible", "Ineligible")))
write_csv(tract_label, "census-csv/tracts_label.csv")

