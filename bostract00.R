##     2000 Census data by Tract of Suffolk county MA (Boston, ma)
##
## Scraping from FFIEC website (both pages)
bos001<-read_html("https://www.ffiec.gov/census/report.aspx?year=2000&county=025&tract=ALL&state=25&report=demographic")%>%
  html_node("form table")%>%
  html_table()
bos002<-read_html("https://www.ffiec.gov/census/report.aspx?year=2000&county=025&tract=ALL&state=25&report=demographic&page=2")%>%
  html_node("form table")%>%
  html_table()
bostract00<-rbind(bos001, bos002)
rm(bos001, bos002)

### Clean up names and 
bostract00<-bostract00%>%
  rename(tract = `Tract Code`, med_income00 = `2000 Est. Tract/ BNA Median Family Income`,
         med_income90 = `1990 Tract/ BNA Median Family Income`, tot_pop = `Tract/ BNA Population`,
         nonwhite_pop = `Minority Population`, owner_occ = `Owner Occupied Units`)%>%
  select(tract, med_income00, med_income90, tot_pop, nonwhite_pop, owner_occ)

bos00<-bostract00%>%
  mutate(GEOID = ifelse(str_detect(as.character(tract), "\\."), as.character(tract), paste0(as.character(tract),".00")), 
         med_income00 = str_remove_all(med_income00, "[\\$\\,]"),
         med_income90 = str_remove_all(med_income90, "[\\$\\,]"))%>%
  mutate(GEOID = str_pad(GEOID, width = 7, pad = "0"))%>%
  mutate(GEOID = str_remove(GEOID, "\\."))

bos00<-bos00%>%
  mutate(GEOID = str_pad(as.character(tract),width = 6, side = "right", pad ="0"))%>%
  select(-tract)

#write_csv(bos00, "bos_2000.csv")

### tract 000301, 000302 is combined into 000300

