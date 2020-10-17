##   1990 Census data Suffolk County MA (Boston MA)
##
##   Data downloaded from https://www2.cdc.gov/nceh/lead/census90/house11/files/stf3a_25.zip
##



##  Rename to understandable col names FROM original ".dbf" file
bos_90<-tract90%>%
  filter(CNTY=="025") %>% mutate(TRACT = as.factor(TRACT))%>%
  select(TRACT, P0010001, P0050001, P0290001,P080A001,
         P0570001, P0570002, P0570003, P0570004, P0570005, P0570006, P0570007,
         P0120001, P0120002, P0120003, P0120004, P0120005)%>%
  group_by(TRACT)%>%summarise(tot_pop_90 = sum(P0010001),
                              #num_hh = sum(P0050001),
                              rc_white_90 = sum(P0120001),
                              rc_black = sum(P0120002),
                              rc_ai = sum(P0120003),
                              rc_asian = sum(P0120004),
                              rc_other = sum(P0120005),
                              hh_english_90 = sum(P0290001),
                              ed_9 = sum(P0570001),
                              ed_912 = sum(P0570002),
                              ed_hs_90 = sum(P0570003)+ed_9+ed_912,
                              ed_some_col = sum(P0570004),
                              ed_assoc = sum(P0570005),
                              ed_ba = sum(P0570006)+sum(P0570007),
                              ed_grad = sum(P0570007),
                              med_income_90 = mean(P080A001))



##  Convert TRACT to 6-digit tract GEOID excluding State and county prefix
bos_90<-bos_90%>%mutate(GEOID = str_remove_all(TRACT, "^25025|\\."))%>%
  mutate(GEOID = str_pad(GEOID, width = 6, side = "right", pad="0"))





#write_csv(bos_90, "bos_90.csv")
