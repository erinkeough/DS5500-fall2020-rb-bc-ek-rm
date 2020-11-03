census_standardized <- read.csv("https://raw.githubusercontent.com/erinkeough/DS5500-fall2020-rb-bc-ek-rm/master/census-csv/census_suffolk_standardized.csv")


#Create new dataframe to insert the transformed variables into 
cen_data <- census_standardized[,1:3]



#Changing vars in dataset to proportions or interesting things
#Proportion of the population for each census tract by centennial that is under 18
cen_data$prop.population.under.18 <-
  (census_standardized$Persons..Under.5.years +
     census_standardized$Persons..5.to.9.years +
     census_standardized$Persons..10.to.14.years +
     census_standardized$Persons..15.to.17.years)/
  census_standardized$Persons..Total

#Proportion of population for each census tract by centennial that is a young adult which we are defining as anyone 18 or older and 34 or younger
cen_data$prop.population.young.adult <- (census_standardized$Persons..18.and.19.years +
                                           census_standardized$Persons..20.years +
                                           census_standardized$Persons..21.years +
                                           census_standardized$Persons..22.to.24.years +
                                           census_standardized$Persons..25.to.29.years +
                                           census_standardized$Persons..30.to.34.years) / census_standardized$Persons..Total

#Proportion of population for each census tract by centennial that is middle aged which we are defining as anyone that is at least 35 years old but less than 55 years old
cen_data$prop.population.middle.aged <- ( census_standardized$Persons..35.to.39.years + 
                                            census_standardized$Persons..40.to.44.years + 
                                            census_standardized$Persons..45.to.49.years + 
                                            census_standardized$Persons..50.to.54.years) / 
  census_standardized$Persons..Total

#Proportion of population for each census tract by centennial that is in their older adulthood which is anyone aged 55 years or older
cen_data$prop.population.older.adulthood <- ( census_standardized$Persons..55.to.59.years + 
                                                census_standardized$Persons..60.and.61.years + 
                                                census_standardized$Persons..62.to.64.years + 
                                                census_standardized$Persons..65.to.69.years + 
                                                census_standardized$Persons..70.to.74.years + 
                                                census_standardized$Persons..75.to.79.years + 
                                                census_standardized$Persons..80.to.84.years + 
                                                census_standardized$Persons..85.years.and.over) / 
  census_standardized$Persons..Total

#These proportions should add up to 1, should I check? Probably but I was focusing on making the columns so I can come back to that...

#Proportion of population that is not hispanic or latino who identifies as White 
#Note: We are only using single race data for all years as there was no option for multiple race selection until the 2000 census
cen_data$prop.population.race.White <- census_standardized$Persons..Not.Hispanic.or.Latino...White..single.race.reported.or..since.2000..race.combinations.likely.to.report.this.single.race. / 
  census_standardized$Persons..Total

#Proportion of population that is not hispanic or latino who identifies as Black or African American. Will use the term "black" to capture this in the variable name for brevity's sake
cen_data$prop.population.race.Black <- census_standardized$Persons..Not.Hispanic.or.Latino...Black.or.African.American..single.race.reported.or..since.2000..race.combinations.likely.to.report.this.single.race./ 
  census_standardized$Persons..Total

#Proportion of population that is not hispanic or latino who identifies as american indian or alaska native. Will use the term "native" to capture this in the variable name for brevity
cen_data$prop.population.race.Native <- census_standardized$Persons..Not.Hispanic.or.Latino...American.Indian.and.Alaska.Native..single.race.reported.or..since.2000..race.combinations.likely.to.report.this.single.race. / 
  census_standardized$Persons..Total

#Proportion of population that is not hispanic or latino who identifies as asian or pacific islander. Will use the term "islander" to capture this in variable
cen_data$prop.population.race.Islander <- census_standardized$Persons..Not.Hispanic.or.Latino...Asian.and.Pacific.Islander..single.race.reported.or..since.2000..race.combinations.likely.to.report.this.single.race. / 
  census_standardized$Persons..Total

#Proportion of population that is not hispanic or latino who identifies as Other
cen_data$prop.population.race.Other <- census_standardized$Persons..Not.Hispanic.or.Latino...Some.Other.Race..single.race.reported.or..since.2000..race.combinations.likely.to.report.this.single.race. / 
  census_standardized$Persons..Total 

#Proportion of population that identified as hispanic or latino. Will use the term "latino" to capture this in variable name for brevity
cen_data$prop.population.race.Latino <- census_standardized$Persons..Hispanic.or.Latino / census_standardized$Persons..Total

#Proportion of total households that are Nonfamily households. We do not also need the proportion of households that are family since family + nonfamily households = total households
#CHECK that assumption to make sure it is true for all 
cen_data$prop.households.Nonfamily <- census_standardized$Households..Nonfamily/ 
  census_standardized$Households..Total

#Proportion of households that are owner occupied (ie. the person that owns the house/condo/etc. is also the person that lives in it). We do not also track proportion renter occupied as the owner + renter occupied proportions should add to 1 thus if one increases the other decreases by the same amount
#Hypothesis: Gentrification has a negative relationship with the proportion of owner occupied housing (if gentrified -> lower prop of owner occupied bc more expensive to buy property)
#Check assumption for all rows that the number of owner occupied housing units + number of renter occupied housing units = households total
cen_data$prop.housing.Owner.Occupied <- census_standardized$Housing.units..Owner.occupied / 
  census_standardized$Households..Total


#Proportion of total population that lives in owner occupied housing units. Will also have to dothis for rental as I know that owner occupied + renter occupied != total population in all rows so it is possible that as one increases the other stays the same
cen_data$prop.population.Owner.Occupied <- census_standardized$Persons..In.owner.occupied.housing.units / 
  census_standardized$Persons..Total

#Proportion of population that lives in renter occupied housing units
cen_data$prop.population.Renter.Occupied <- census_standardized$Persons..In.renter.occupied.housing.units / 
  census_standardized$Persons..Total

#Proportion of households where the householder is White
cen_data$prop.housing.householder.race.White <- census_standardized$Housing.units..Occupied...Householder.is.White..single.race. / 
  census_standardized$Households..Total

#Proportion of households where the householder is Black or African American
cen_data$prop.housing.householder.race.Black <- census_standardized$Housing.units..Occupied...Householder.is.Black.or.African.American..single.race. / 
  census_standardized$Households..Total

#Proportion of households where the householder is American Indian or Alaskan Native
cen_data$prop.housing.householder.race.Native <- census_standardized$Housing.units..Occupied...Householder.is.American.Indian.and.Alaska.Native..single.race. / 
  census_standardized$Households..Total

#Proportion of households where the householder is asian or pacific islander
cen_data$prop.housing.householder.race.Islander <- census_standardized$Housing.units..Occupied...Householder.is.Asian.and.Pacific.Islander..single.race. / 
  census_standardized$Households..Total

#Proportion of households where the householders is Other race
cen_data$prop.housing.householder.race.Other <- census_standardized$Housing.units..Occupied...Householder.is.Some.Other.Race..single.race. / 
  census_standardized$Households..Total

#Proportion of owner occupied housing units where the householder is White
cen_data$prop.housing.owner.occupied.householder.race.White <- census_standardized$Housing.units..Owner.occupied...Householder.is.White..single.race. / 
  census_standardized$Housing.units..Owner.occupied

#proportion of owner occupied housing units where the householder is black/african american
cen_data$prop.housing.owner.occupied.householder.race.Black <- census_standardized$Housing.units..Owner.occupied...Householder.is.Black.or.African.American..single.race. / 
  census_standardized$Housing.units..Owner.occupied

#proportion of owner occupied housing units whereh the householder is american indian or alaskan native
cen_data$prop.housing.owner.occupied.householder.race.Native <- census_standardized$Housing.units..Owner.occupied...Householder.is.American.Indian.and.Alaska.Native..single.race. / 
  census_standardized$Housing.units..Owner.occupied

#proportion of owner occupied housing units where the householder is asian or pacific islander
cen_data$prop.housing.owner.occupied.householder.race.Islander <- census_standardized$Housing.units..Owner.occupied...Householder.is.Asian.and.Pacific.Islander..single.race. / 
  census_standardized$Housing.units..Owner.occupied

#proportion of owner occupied housing units where the householder is Other race
cen_data$prop.housing.owner.occupied.householder.race.Other <- census_standardized$Housing.units..Owner.occupied...Householder.is.Some.Other.Race..single.race. / census_standardized$Housing.units..Owner.occupied

#proportion of renter occupied housing units where the householder is White
cen_data$prop.housing.renter.occupied.householder.race.White <- census_standardized$Housing.units..Renter.occupied...Householder.is.White..single.race. / census_standardized$Housing.units..Renter.occupied

#propotion of renter occupied housing units where the householder is Black/African American
cen_data$prop.housing.renter.occupied.householder.race.Black <- census_standardized$Housing.units..Renter.occupied...Householder.is.Black.or.African.American..single.race. / 
  census_standardized$Housing.units..Renter.occupied

#proportion of renter occupied housing units where the householder is american indian or alaskan native
cen_data$prop.housing.renter.occupied.householder.race.Native <- census_standardized$Housing.units..Renter.occupied...Householder.is.American.Indian.and.Alaska.Native..single.race. /
  census_standardized$Housing.units..Renter.occupied

#proportion of renter occupied housing units where the houseolder is asian or pacific islander
cen_data$prop.housing.renter.occupied.householder.race.Islander <- census_standardized$Housing.units..Renter.occupied...Householder.is.Asian.and.Pacific.Islander..single.race. / 
  census_standardized$Housing.units..Renter.occupied

#proportion of renter occupied housing units where the householder is other race
cen_data$prop.housing.renter.occupied.householder.race.Other <- census_standardized$Housing.units..Renter.occupied...Householder.is.Some.Other.Race..single.race. / 
  census_standardized$Housing.units..Renter.occupied

#Average household size for renter occupied housing units. For the households with 7 or more people we just used 7 for calculating the average as there was no upper bound given for number of people nor any sort of median/mean
cen_data$avg.household.size.renter.occupied <- (census_standardized$Housing.units..Renter.occupied...1.person.household +
                                                  2 * 
                                                  census_standardized$Housing.units..Renter.occupied...2.person.household + 3 *
                                                  census_standardized$Housing.units..Renter.occupied...3.person.household + 4 * 
                                                  census_standardized$Housing.units..Renter.occupied...4.person.household + 5 * 
                                                  census_standardized$Housing.units..Renter.occupied...5.person.household + 6 * 
                                                  census_standardized$Housing.units..Renter.occupied...6.person.household + 7 * 
                                                  census_standardized$Housing.units..Renter.occupied...7.or.more.person.household) / 
  
  (census_standardized$Housing.units..Renter.occupied...1.person.household +
     census_standardized$Housing.units..Renter.occupied...2.person.household +
     census_standardized$Housing.units..Renter.occupied...3.person.household + 
     census_standardized$Housing.units..Renter.occupied...4.person.household + 
     census_standardized$Housing.units..Renter.occupied...5.person.household +
     census_standardized$Housing.units..Renter.occupied...6.person.household + 
     
     census_standardized$Housing.units..Renter.occupied...7.or.more.person.household)

#Average household size for owner occupied housing units. For the households with 7 or more people we just used 7 for calculating the average as there was no upper bound given for number of people nor any sort of median/mean
cen_data$avg.household.size.owner.occupied <- (census_standardized$Housing.units..Owner.occupied...1.person.household + 2 * 
                                                 census_standardized$Housing.units..Owner.occupied...2.person.household + 3 * 
                                                 census_standardized$Housing.units..Owner.occupied...3.person.household + 4 * 
                                                 census_standardized$Housing.units..Owner.occupied...4.person.household + 5 * 
                                                 census_standardized$Housing.units..Owner.occupied...5.person.household + 6 * 
                                                 census_standardized$Housing.units..Owner.occupied...6.person.household + 7 * 
                                                 census_standardized$Housing.units..Owner.occupied...7.or.more.person.household) / 
  (census_standardized$Housing.units..Owner.occupied...1.person.household + 
     census_standardized$Housing.units..Owner.occupied...2.person.household + 
     census_standardized$Housing.units..Owner.occupied...3.person.household + 
     census_standardized$Housing.units..Owner.occupied...4.person.household + 
     census_standardized$Housing.units..Owner.occupied...5.person.household + 
     census_standardized$Housing.units..Owner.occupied...6.person.household +
     census_standardized$Housing.units..Owner.occupied...7.or.more.person.household)

#Average household size overall (includes owner occupied and renter occupied). For households with 7 or more people just used 7 for simplicity
#CHECK that housing units owner occupied = sum(housing units owner occupied X person household) bc then can simplify equation (check this also for renter occupied and check that renter + owner occupied = total household)
cen_data$avg.household.size <- ((census_standardized$Housing.units..Owner.occupied...1.person.household + 2 * 
                                   census_standardized$Housing.units..Owner.occupied...2.person.household + 3 * 
                                   census_standardized$Housing.units..Owner.occupied...3.person.household + 4 * 
                                   census_standardized$Housing.units..Owner.occupied...4.person.household + 5 * 
                                   census_standardized$Housing.units..Owner.occupied...5.person.household + 6 * 
                                   census_standardized$Housing.units..Owner.occupied...6.person.household + 7 * 
                                   census_standardized$Housing.units..Owner.occupied...7.or.more.person.household) + (census_standardized$Housing.units..Renter.occupied...1.person.household +
                                                                                                                        2 * 
                                                                                                                        census_standardized$Housing.units..Renter.occupied...2.person.household + 3 *
                                                                                                                        census_standardized$Housing.units..Renter.occupied...3.person.household + 4 * 
                                                                                                                        census_standardized$Housing.units..Renter.occupied...4.person.household + 5 * 
                                                                                                                        census_standardized$Housing.units..Renter.occupied...5.person.household + 6 * 
                                                                                                                        census_standardized$Housing.units..Renter.occupied...6.person.household + 7 * 
                                                                                                                        census_standardized$Housing.units..Renter.occupied...7.or.more.person.household)) / ((census_standardized$Housing.units..Renter.occupied...1.person.household +
                                                                                                                                                                                                                census_standardized$Housing.units..Renter.occupied...2.person.household +
                                                                                                                                                                                                                census_standardized$Housing.units..Renter.occupied...3.person.household + 
                                                                                                                                                                                                                census_standardized$Housing.units..Renter.occupied...4.person.household + 
                                                                                                                                                                                                                census_standardized$Housing.units..Renter.occupied...5.person.household +
                                                                                                                                                                                                                census_standardized$Housing.units..Renter.occupied...6.person.household + 
                                                                                                                                                                                                                
                                                                                                                                                                                                                census_standardized$Housing.units..Renter.occupied...7.or.more.person.household) + (census_standardized$Housing.units..Owner.occupied...1.person.household + 
                                                                                                                                                                                                                                                                                                      census_standardized$Housing.units..Owner.occupied...2.person.household + 
                                                                                                                                                                                                                                                                                                      census_standardized$Housing.units..Owner.occupied...3.person.household + 
                                                                                                                                                                                                                                                                                                      census_standardized$Housing.units..Owner.occupied...4.person.household + 
                                                                                                                                                                                                                                                                                                      census_standardized$Housing.units..Owner.occupied...5.person.household + 
                                                                                                                                                                                                                                                                                                      census_standardized$Housing.units..Owner.occupied...6.person.household +
                                                                                                                                                                                                                                                                                                      census_standardized$Housing.units..Owner.occupied...7.or.more.person.household))

#Convert the data from centennial to time phase 
#new dataframe to put the percent change information into
pc_data <- cen_data[FALSE,]
#Keep track of idx as this is the "unique identifier" for each row
x_idx <- 1
#iterate through dataframe by census tract code
for( census_tract in unique(cen_data$Census.Tract.Code) )
{
  r_1990 <- cen_data[which(cen_data$Data.Measurement.Year == 1990 & cen_data$Census.Tract.Code == census_tract),]
  r_2000 <- cen_data[which(cen_data$Data.Measurement.Year == 2000 & cen_data$Census.Tract.Code == census_tract),]
  r_2010 <- cen_data[which(cen_data$Data.Measurement.Year == 2010 & cen_data$Census.Tract.Code == census_tract),]
  
  #Take percent change between the columns to create new rows for the finalized dataset (the first 3 columns of each var will need to be changed)
  pc_90_00 <- (r_2000 - r_1990) / r_1990
  pc_00_10 <- (r_2010 - r_2000) / r_2000
  
  # Change the first 3 rows of each new row
  pc_90_00$X <- x_idx
  pc_00_10$X <- x_idx + 1
  x_idx <- x_idx + 2
  
  pc_90_00$Data.Measurement.Year <- "1990-2000"
  pc_00_10$Data.Measurement.Year <- "2000-2010"
  
  pc_90_00$Census.Tract.Code <- census_tract
  pc_00_10$Census.Tract.Code <- census_tract
  
  #Add these new rows to the new percent change data frame
  pc_data <- rbind(pc_data, pc_90_00, pc_00_10)
}

#rename the rows from prop to prop change
#This function is from https://stackoverflow.com/questions/39670918/replace-characters-in-column-names-gsub
colClean <- function(x){ colnames(x) <- gsub(pattern="prop", replacement="prop.change", colnames(x)); x }
pc_data<-colClean(pc_data)
colClean <- function(x){ colnames(x) <- gsub(pattern="avg", replacement="prop.change", colnames(x)); x }
pc_data<-colClean(pc_data)

#Get tract labels from the github
tract_labels <- read.csv("https://raw.githubusercontent.com/erinkeough/DS5500-fall2020-rb-bc-ek-rm/master/census-csv/tracts_label.csv")

#merge labels with the proportion change df
label_pc <- merge(pc_data, tract_labels, by=c("Data.Measurement.Year", "Census.Tract.Code"))
label_pc <- subset(label_pc, select = -c(Gentrified, Eligible, Ineligible, X))

#This is what was uploaded to the github
write.csv(label_pc, "prop_change_suffolk_labeled.csv")

