### Subset nominal data to Before year
### arg(nom_data, before_year)
### return(subset nominal data of before year)
subset_nom<-function(nom_data, before_year){
  GIS_code<-paste0("GIS.Join.Match.Code..", before_year)
  subset_data<-nom_data[!is.na(nom_data[GIS_code]),]%>%
    select(contains(before_year),
           -contains("Area.Name"))
  subset_data[paste0("TRACT",str_sub(before_year,3))]<-
    str_pad(str_sub(subset_data[[GIS_code]],9), 6, side = "right", pad = "0")
  
  return(subset_data)
}


### Create Destination df for converted values
### arg(nom_data, subset_data, after_year, before_year)
### return(df with column for each tract in after_year, 0 for column values)
create_after_df<-function(nom_data, subset_data, before_year, after_year){
  GIS_after<-paste0("GIS.Join.Match.Code..",after_year)
  GIS_before<-paste0("GIS.Join.Match.Code..", before_year)
  
  dest_df<-nom_data[!is.na(nom_data[GIS_after]),]
  dest_df<-as.tibble(dest_df[GIS_after])
  
  for(newcol in colnames(subset_data)){
    dest_df[[newcol]]<-0
  }
  drop_col<-c(GIS_before,paste0("TRACT",str_sub(before_year,3)))
  dest_df<-dest_df[,!(names(dest_df) %in% drop_col)]
    
  dest_df[paste0("TRACT",str_sub(after_year,3))]<-
    str_pad(str_sub(dest_df[[GIS_after]],9), 6, side = "right", pad = "0")
  
  return(dest_df)
}


