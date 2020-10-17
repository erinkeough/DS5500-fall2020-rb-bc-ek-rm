bos00<-read_csv("census-csv/bos_2000.csv")
bos10<-read_csv("census-csv/bos_2010_trim.csv")
bos90<-read_csv("census-csv/bos_90.csv")

######################
## Create tracts 301, 302 by duplicating 300 for '90 and '00
## This retains the geometry to map out the tracts from '10 ACS data
tr_301<-filter(bos90, GEOID == "000300")%>%mutate(GEOID = "000301")
bos90<-bos90%>%filter(GEOID != "000000")%>%
  mutate(GEOID = ifelse(GEOID == "000300", "000302", GEOID))%>%
  bind_rows(tr_301)

tr_301<-filter(bos00, GEOID == "000300")%>%mutate(GEOID = "000301")%>%select(-med_income_00)
bos00<-bos00%>%select(-med_income_00)%>%
  mutate(GEOID = ifelse(GEOID == "000300", "000302", GEOID))%>%
  bind_rows(tr_301)

bos_all<-left_join(bos00, bos10, by = "GEOID")%>%left_join(bos90, by = "GEOID")

write_csv(bos_all, "census-csv/bos_all.csv")
####################################################################################################
####################################################################################################
