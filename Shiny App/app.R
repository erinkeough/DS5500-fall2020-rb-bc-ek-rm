
# Load packages ----------------------------------------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(readr)
library(ggplot2)
library(grid)
library(gridExtra)
library(cluster)
library(factoextra)
library(ggmap)
library(reshape2)
library(purrr)
library(maps)

rm(list = ls())

# Load county data -------------------------------------------------------------
# If running in RStudio Cloud:
#setwd("/cloud/project/gentrification")
# If running locally:
#setwd("~/GitHub/DS5500-fall2020-rb-bc-ek-rm/Shiny App")

d.suffolk <- read_csv("data/prop_change_suffolk_labeled.csv")
d.bernalillo <- read_csv("data/prop_change_bernalillo_labeled.csv")
d.davidson <- read_csv("data/prop_change_davidson_labeled.csv")
d.douglas <- read_csv("data/prop_change_douglas_labeled.csv")
d.elpaso <- read_csv("data/prop_change_elpaso_labeled.csv")
d.fresno <- read_csv("data/prop_change_fresno_labeled.csv")
d.hennepin <- read_csv("data/prop_change_hennepin_labeled.csv")
d.sanfran <- read_csv("data/prop_change_sanfrancisco_labeled.csv")

tracts <- read_csv("data/tracts_geography.csv")


# Define UI for application that plots trends in city data (Boston, City1, ... ,
#  Cityx) for comparison among features and between different cities -----------
ui <- fluidPage(theme = shinytheme("lumen"),
  
  # Application title ----------------------------------------------------------
  titlePanel("Gentrification in the Boston Area and Beyond"),
  
  # Sidebar layout with a input and output definitions -------------------------
  sidebarLayout(
    
    # Inputs: Select variables to plot -----------------------------------------
    sidebarPanel(
      
      # Select analysis type ---------------------------------------------------
      selectInput(inputId = "selection",
                  label = "Select the type of analysis to complete:",
                  choices = c("Select analysis type" = "null",
                              "Exploratory Data Analysis" = "eda",
                              "Clustering" = "clustering"),
                  selected = "null"
      ),
      
      br(),
      
      # Select cities to analyze -----------------------------------------------
      checkboxGroupInput(inputId = "city", 
                         label = "Select the cities you wish to observe:",
                         choices = c("Boston, MA" = "boston", # Suffolk
                                     "El Paso, TX" = "elpaso", # El Paso
                                     "Omaha, NE" = "omaha", # Douglas
                                     "Nashville, TN" = "nashville", # Davidson
                                     "Fresno, CA" = "fresno", # Fresno
                                     "Minneapolis, MN" = "minneapolis", # Hennepin
                                     "San Fransisco, CA" = "sanfran", # Sanfran
                                     "Albuquerque, NM" = "abq"), # Bernalillo
                         selected = "boston"), # Default choice
      
      hr(),
      
      # Conditional panel - Select # of clusters -------------------------------
      conditionalPanel(condition = "input.selection == 'clustering'",
                       radioButtons(inputId = "k", 
                                    label = "Select the number of clusters:",
                                    choices = list("2" = 2, 
                                                   "3" = 3, 
                                                   "4" = 4),
                                    selected = 2)),
      
      # Conditional panel - Select type of EDA ---------------------------------
      conditionalPanel(condition = "input.selection == 'eda'",
                       selectInput(inputId = "type", 
                                    label = "Select the type of EDA you would like to complete:",
                                    choices = list("Distribution of Tract Labels by City" = "tract", 
                                                   "Household Ownsership over Time" = "race",
                                                   "Change in Tract Age Composition" = "age"),
                                    selected = "tract")),
      
      br(),
      
      # Conditional panel - Choose to show data? -------------------------------
      conditionalPanel(condition = "input.selection == 'eda' & input.type == 'tract'",
                       checkboxInput(inputId = "showdata",
                                     label = "Show Data",
                                     value = FALSE))
      
    ),
    
    mainPanel(
      
      br(),
      
      plotOutput(outputId = "mainplot"),
      
      br(),
      
      DT::dataTableOutput(outputId = "labeltable")

    )
    
  )
  
)




server <- function(input, output) {
  
  # REACTIVE CITY SELECTOR:
  datalist <- reactive({
    
    req(input$city)
    l <- list()

    # Iterate through the number of chosen cities ( = length of the list)
    for (i in 1:length(input$city)) {
      
      # Add each city's dataset to the list
      if (input$city[i] == "boston") {
        l[[i]] <- d.suffolk
      } else if (input$city[i] == "abq") {
        l[[i]] <- d.bernalillo
      } else if (input$city[i] == "elpaso") {
        l[[i]] <- d.elpaso
      } else if (input$city[i] == "fresno") {
        l[[i]] <- d.fresno
      } else if (input$city[i] == "sanfran") {
        l[[i]] <- d.sanfran
      } else if (input$city[i] == "omaha") {
        l[[i]] <- d.douglas
      } else if (input$city[i] == "nashville") {
        l[[i]] <- d.davidson
      } else if (input$city[i] == "minneapolis") {
        l[[i]] <- d.hennepin
      }
      
    }

    l
    
  })
  # REACTIVE CITY SELECTOR -- raw data:
  rawdata <- reactive({
    
    req(input$city)
    l <- list()
    
    # Iterate through the number of chosen cities ( = length of the list)
    for (i in 1:length(input$city)) {
      
      # Add each city's dataset to the list
      if (input$city[i] == "boston") {
        l[[i]] <- read_csv("data/census_suffolk_standardized.csv")
      } else if (input$city[i] == "abq") {
        l[[i]] <- read_csv("data/census_bernalillo_standardized.csv")
      } else if (input$city[i] == "elpaso") {
        l[[i]] <- read_csv("data/census_elpaso_standardized.csv")
      } else if (input$city[i] == "fresno") {
        l[[i]] <- read_csv("data/census_fresno_standardized.csv")
      } else if (input$city[i] == "sanfran") {
        l[[i]] <- read_csv("data/census_sanfrancisco_standardized.csv")
      } else if (input$city[i] == "omaha") {
        l[[i]] <- read_csv("data/census_douglas_standardized.csv")
      } else if (input$city[i] == "nashville") {
        l[[i]] <- read_csv("data/census_davidson_standardized.csv")
      } else if (input$city[i] == "minneapolis") {
        l[[i]] <- read_csv("data/census_hennepin_standardized.csv")
      }
      
    }
    
    l
    
  })
  
  
  numclusters <- reactive({
    
    req(input$k)
    input$k
    
  })
  
  
  
  # EDA OUTPUT PLOT:
  output$mainplot <- renderPlot({ 
    
    if (input$selection == "null") {
      
      # Connect to Google APIs with your key:
      ggmap::register_google(key = "YOUR KEY HERE")
      
      locations <- data.frame(nrow = 8, ncol = 2)
      locations[1,] <- geocode("Boston, Massachusetts", source = "google")
      locations[2,] <- geocode("Albuquerque, New Mexico", source = "google")
      locations[3,] <- geocode("El Paso, Texas", source = "google")
      locations[4,] <- geocode("Omaha, Nebraska", source = "google")
      locations[5,] <- geocode("Minneapolis, Minnesota", source = "google")
      locations[6,] <- geocode("Fresno, California", source = "google")
      locations[7,] <- geocode("San Francisco, California", source = "google")
      locations[8,] <- geocode("Nashville, Tennessee", source = "google")
      colnames(locations) <- c("Longitude", "Latitude")
      
      us_states <- map_data("state")
      
      ggplot() +
        geom_polygon(data = us_states, aes(x = long, y = lat, group = group), 
                     color = "gray30", fill = "bisque", size = 0.2) + guides(fill = FALSE) +
        geom_point(data = locations, aes(x = Longitude, y = Latitude), 
                   size = 4, color = "brown3") +
        theme(axis.title = element_blank(), axis.line = element_blank(), axis.ticks = element_blank(),
              axis.text = element_blank(), plot.background = element_blank(),
              panel.grid = element_blank(), panel.background = element_blank()) +
        coord_fixed(ratio = 1.5)
      
      
      
      
    } else if (input$selection == "eda") {
      
      plots <- list()
      
      # Iterate through the datasets
      for (i in 1:length(datalist())) {
  
        # Grab the city name for the plot title
        if (input$city[i] == "boston") {
          name <- "Boston, MA"
        } else if (input$city[i] == "abq") {
          name <- "Albuquerque, NM"
        } else if (input$city[i] == "elpaso") {
          name <- "El Paso, TX"
        } else if (input$city[i] == "fresno") {
          name <- "Fresno, CA"
        } else if (input$city[i] == "sanfran") {
          name <- "San Francisco, CA"
        } else if (input$city[i] == "omaha") {
          name <- "Omaha, NE"
        } else if (input$city[i] == "nashville") {
          name <- "Nashville, TN"
        } else if (input$city[i] == "minneapolis") {
          name <- "Minneapolis, MN"
        }
        
        if (input$type == "tract") {
        
          df <- datalist()[[i]] %>% na.omit()

          p <- ggplot(df) +
            geom_bar(aes(x = Gent_Label, fill = Gent_Label)) +  
            scale_fill_manual(values = c("darkslategray3", "goldenrod2", "darkseagreen4")) +
            labs(title = name, x = "Gentrification Label", y = "Count") +
            theme(plot.title = element_text(hjust = 0.5, size = 12),
                  legend.position = "none")
          
        } else if (input$type == "race") {
          
          df <- rawdata()[[i]]
          df_new <- df %>%
            transmute(
              `Data Measurement Year` = Data.Measurement.Year,
              `White Owner` = Housing.units..Owner.occupied...Householder.is.White..single.race.,
              `Non-White Owner` = 
              Housing.units..Owner.occupied...Householder.is.Black.or.African.American..single.race. +
              Housing.units..Owner.occupied...Householder.is.American.Indian.and.Alaska.Native..single.race. +
              Housing.units..Owner.occupied...Householder.is.Asian.and.Pacific.Islander..single.race. +
              Housing.units..Owner.occupied...Householder.is.Some.Other.Race..single.race.
              ) %>%
            melt(id.vars = "Data Measurement Year", measure.vars = c("White Owner", "Non-White Owner"))
          
          p <- ggplot(df_new, aes(x = `Data Measurement Year`, y = value, fill = as.factor(variable))) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_manual("Race of Owner", values = c("darkslategray3", "darkseagreen4")) +
            labs(title = name, y = "Count") +
            theme(plot.title = element_text(hjust = 0.5, size = 12))

        } else if (input$type == "age") {
          
          df <- datalist()[[i]] %>% na.omit()
          
          df_new <- data.frame(`Data Measurement Year` = 
                                 c("1990-2000","1990-2000","1990-2000","2000-2010","2000-2010","2000-2010"),
                               `Gentrification Status` = 
                                 c("Ineligible","Eligible","Gentrified","Ineligible","Eligible","Gentrified")
                               )
          df_new$Youth[1] <- mean(df$prop.change.population.under.18[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Ineligible"])
          df_new$Youth[2] <- mean(df$prop.change.population.under.18[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Eligible"])
          df_new$Youth[3] <- mean(df$prop.change.population.under.18[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Gentrified"])
          df_new$Youth[4] <- mean(df$prop.change.population.under.18[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Ineligible"])
          df_new$Youth[5] <- mean(df$prop.change.population.under.18[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Eligible"])
          df_new$Youth[6] <- mean(df$prop.change.population.under.18[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Gentrified"])
          
          df_new$`Young Adult`[1] <- mean(df$prop.change.population.young.adult[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Ineligible"])
          df_new$`Young Adult`[2] <- mean(df$prop.change.population.young.adult[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Eligible"])
          df_new$`Young Adult`[3] <- mean(df$prop.change.population.young.adult[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Gentrified"])
          df_new$`Young Adult`[4] <- mean(df$prop.change.population.young.adult[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Ineligible"])
          df_new$`Young Adult`[5] <- mean(df$prop.change.population.young.adult[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Eligible"])
          df_new$`Young Adult`[6] <- mean(df$prop.change.population.young.adult[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Gentrified"])
          
          df_new$`Middle Aged`[1] <- mean(df$prop.change.population.middle.aged[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Ineligible"])
          df_new$`Middle Aged`[2] <- mean(df$prop.change.population.middle.aged[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Eligible"])
          df_new$`Middle Aged`[3] <- mean(df$prop.change.population.middle.aged[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Gentrified"])
          df_new$`Middle Aged`[4] <- mean(df$prop.change.population.middle.aged[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Ineligible"])
          df_new$`Middle Aged`[5] <- mean(df$prop.change.population.middle.aged[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Eligible"])
          df_new$`Middle Aged`[6] <- mean(df$prop.change.population.middle.aged[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Gentrified"])
          
          df_new$`Older Adult`[1] <- mean(df$prop.change.population.older.adulthood[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Ineligible"])
          df_new$`Older Adult`[2] <- mean(df$prop.change.population.older.adulthood[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Eligible"])
          df_new$`Older Adult`[3] <- mean(df$prop.change.population.older.adulthood[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Gentrified"])
          df_new$`Older Adult`[4] <- mean(df$prop.change.population.older.adulthood[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Ineligible"])
          df_new$`Older Adult`[5] <- mean(df$prop.change.population.older.adulthood[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Eligible"])
          df_new$`Older Adult`[6] <- mean(df$prop.change.population.older.adulthood[df$Data.Measurement.Year=="1990-2000" & df$Gent_Label=="Gentrified"])

          df_new <- df_new %>%
            melt(id.vars = c("Data.Measurement.Year", "Gentrification.Status"),
                 measure.vars = c("Youth", "Young Adult", "Middle Aged", "Older Adult"))

          p <- ggplot(df_new, aes(x = variable, y = value, 
                                  fill = as.factor(Gentrification.Status))) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_manual("Gentrification Status",
                              values = c("darkslategray3", "goldenrod2", "darkseagreen4")) +
            labs(title = name, y = "Proportion of Change", x = "Age Range", 
                 fill = "Gentrification Status") +
            theme(plot.title = element_text(hjust = 0.5, size = 12)) +
            facet_wrap(~Data.Measurement.Year)
          
        }
  
        plots[[i]] <- p
        plot(plots[[i]])
        
      }
      
      
      # ARRANGING THE GRIDS:
      if (input$type == "tract") {
        do.call("grid.arrange", c(plots, nrow = ceiling(length(input$city) / 2),
                                  top = "Gentrification Labels for City Census Tracts"))
      } else if (input$type == "race") {
        do.call("grid.arrange", c(plots, nrow = ceiling(length(input$city) / 2),
                                  top = "White versus Non-White Household Ownership"))
      } else if (input$type == "age") {
        do.call("grid.arrange", c(plots, ncol = 1,
                                  top = "Proportion of Change in Tract Age Composition by Year"))
      }
  
      
      
      
    } else if (input$selection == "clustering"){
        
        plots <- list()

        k = numclusters()

        # Iterate through the cluster datasets for each chosen city:
        for (i in 1:length(datalist())) {
          
          new <- datalist()[[i]] 

          # for (i in nrow(new)) {
          #   for (j in ncol(new)) {
          #     
          #     ifelse(is.na(new[i, j]), 0, new[i, j])
          #     ifelse(!is.finite(new[i, j]), 0, new[i, j])
          #     
          #   }
          # }
          new <- do.call(data.frame, lapply(new, function(x) replace(x, is.infinite(x), 0)))
          #new[,4:35] <- sapply(new[,4:35], as.numeric)

          df <- new[,4:35] %>% scale()
          df[is.na(df)] <- 0
          df[!is.finite(df)] <- 0
          #df <- ifelse(is.na(df), df[is.na(df)], 0, df)
          
          # Get city-specific data
          if (input$city[i] == "boston") {
            t <- tracts[grepl("^25025", tracts$GEOID), ] # Keep tracts that begin w/ FIPS code
            fips <- 25025
            code <- "Boston, Massachusetts"
          } else if (input$city[i] == "abq") {
            t <- tracts[grepl("^35001", tracts$GEOID), ]
            fips <- 35001
            code <- "Albuquerque, New Mexico"
          } else if (input$city[i] == "elpaso") {
            t <- tracts[grepl("^48141", tracts$GEOID), ]
            fips <- 48141
            code <- "El Paso, Texas"
          } else if (input$city[i] == "fresno") {
            t <- tracts[grepl("^06019", tracts$GEOID), ]
            fips <- 06019
            code <- "Fresno, California"
          } else if (input$city[i] == "sanfran") {
            t <- tracts[grepl("^06075", tracts$GEOID), ]
            fips <- 06075
            code <- "San Francisco, California"
          } else if (input$city[i] == "omaha") {
            t <- tracts[grepl("^08035", tracts$GEOID), ]
            fips <- 08035
            code <- "Omaha, Nebraska"
          } else if (input$city[i] == "nashville") {
            t <- tracts[grepl("^47037", tracts$GEOID), ]
            fips <- 47037
            code <- "Nashville, Tennessee"
          } else if (input$city[i] == "minneapolis") {
            t <- tracts[grepl("^27053", tracts$GEOID), ]
            fips <- 27053
            code <- "Minneapolis, Minnesota"
          } 
          
          km <- kmeans(df, centers = k, nstart = 25)
          
          # Get full FIPS code
          new$GEOID <- as.numeric(paste(fips, 
                                       sprintf("%06d", new$Census.Tract.Code), sep = ""))
          
          # Join datasets on GEOID
          geo <- new %>% left_join(t) %>% as.data.frame() 
          
          # Add cluster assignments from k-means
          geo$Cluster <- km$cluster
          #geo$Cluster00.10 <- kmeans2$cluster

          # Determine the coordinates of the city:
          center <- geocode(code, source = "google")
          
          p <- qmap(c(lon = center$lon, lat = center$lat)) + 
            geom_point(data = geo[!is.na(geo$Data.Measurement.Year),], 
                       aes(x = INTPTLONG, y = INTPTLAT, 
                                       color = as.factor(Cluster), size = 2)) +
            scale_size(guide = 'none') +
            scale_x_continuous(limits = c(min(geo$INTPTLONG), max(geo$INTPTLONG))) +
            scale_y_continuous(limits = c(min(geo$INTPTLAT), max(geo$INTPTLAT))) +
            labs(title = code, color = "Cluster") + 
            facet_wrap(~ Data.Measurement.Year)

          plots[[i]] <- p
          plot(plots[[i]])
          
        }
        
        # The only thing I need to fix here is making the top label larger than the plot title:
        # do.call("grid.arrange", c(plots, ncol = 1,
        #                           top = "Cluster Assignments from 1990-2000 versus 2000-2010"))
        # 
        plots
    }
      
  })
  
  output$labeltable <- DT::renderDataTable({
    if(input$showdata){
      
      City <- c("Boston, MA", "El Paso, TX", "Omaha, NE", "Albuquerque, NM", "Fresno, CA", 
                "San Francisco, CA", "Nashville, TN", "Minneapolis, MN")
      Ineligible <- c(sum(d.suffolk$Gent_Label == "Ineligible"), sum(d.elpaso$Gent_Label == "Ineligible"),
                     sum(d.douglas$Gent_Label == "Ineligible"),sum(d.bernalillo$Gent_Label == "Ineligible"),
                     sum(d.fresno$Gent_Label == "Ineligible"), sum(d.sanfran$Gent_Label == "Ineligible"), 
                     sum(d.davidson$Gent_Label == "Ineligible"), sum(d.hennepin$Gent_Label == "Ineligible"))
      Eligible <- c(sum(d.suffolk$Gent_Label == "Eligible"), sum(d.elpaso$Gent_Label == "Eligible"),
                    sum(d.douglas$Gent_Label == "Eligible"),sum(d.bernalillo$Gent_Label == "Eligible"),
                    sum(d.fresno$Gent_Label == "Eligible"), sum(d.sanfran$Gent_Label == "Eligible"), 
                    sum(d.davidson$Gent_Label == "Eligible"), sum(d.hennepin$Gent_Label == "Eligible"))
      Gentrified <- c(sum(d.suffolk$Gent_Label == "Gentrified"), sum(d.elpaso$Gent_Label == "Gentrified"),
                      sum(d.douglas$Gent_Label == "Gentrified"),sum(d.bernalillo$Gent_Label == "Gentrified"),
                      sum(d.fresno$Gent_Label == "Gentrified"), sum(d.sanfran$Gent_Label == "Gentrified"), 
                      sum(d.davidson$Gent_Label == "Gentrified"), sum(d.hennepin$Gent_Label == "Gentrified"))
      
      DT::datatable(data = data.frame(City, Ineligible, Eligible, Gentrified), 
                    options = list(pageLength = 5), 
                    rownames = FALSE)
    }
    
  })
  
}
  

  



# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)



