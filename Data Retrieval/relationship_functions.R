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


### Use Relationship File to translate Before tract pop to After tract pop
### arg(nom_data, rel_df, before_year, after_year)
### return(dest_df) \filled in with pops
tract_translate<-function(nom_data, rel_df, before_year, after_year){
  subset_data<-subset_nom(nom_data, before_year)
  dest_df<-create_after_df(nom_data, subset_data, before_year,after_year)
  
  col_before<-paste0("TRACT",str_sub(before_year,3))
  col_after<-paste0("TRACT", str_sub(after_year,3))
  col_pct<-paste0("POPPCT", str_sub(before_year,3))
  
  for(row in 1:nrow(rel_df)){
    t_before<-rel_df[[col_before]][row]
    t_after<-rel_df[[col_after]][row]
    pct<-rel_df[[col_pct]][row]
    
    ## loop through columns of specific row
    for(c_index in 1:ncol(subset_data)){
      c_name = colnames(subset_data[,c_index])
      if(str_detect(c_name, "GIS.Join")|str_detect(c_name, "TRACT")){
        next()
      }
      
      dest_df[dest_df[[col_after]]==t_after,c_index]<-
        dest_df[dest_df[[col_after]]==t_after, c_index]+
        subset_data[subset_data[[col_before]]==t_before, c_index]*pct
    }
    
  }
  
  return(dest_df)
}








