### New NHGIS file
## Filter out counties
### filter out years
## Criteria for eligible/ineligible 1990
## Criteria for Gentrified/eligible/ineligible 2000


###############
pop_tot<-read_csv("census-csv/nhgis_labels/Population.csv")%>%
  select(GISJOIN, DATAYEAR, STATE, COUNTY,TRACTA, CL8AA, CL9AA)%>%
  rename(tot_pop = CL8AA, urb_pop=CL9AA)

c_types<-"ccnccccccccdddddddd"
hhi_ed<-read_csv("census-csv/nhgis_labels/Income_Ed.csv", col_types = c_types)%>%
  rename(ed_att = B69AC, med_inc = B79AA)%>%
  select(NHGISCODE:NAME, ed_att, med_inc)
rm(c_types)

######################################################
## Albuquerque NM
alb_pop<-pop_tot%>%
  filter(str_detect(COUNTY, "^Bernalillo"),
         tot_pop > 499)%>%
  mutate(in_city = (urb_pop/tot_pop) > 0.24)

alb_hhed<-hhi_ed%>%
  filter(str_detect(COUNTY, "^Bernalillo"),
         YEAR>=1990)



  
  
  