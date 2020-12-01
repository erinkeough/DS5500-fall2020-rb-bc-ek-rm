
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

rm(list = ls())

# Load county data -------------------------------------------------------------
# If running in RStudio Cloud:
#setwd("/cloud/project/gentrification")
# If running locally:
setwd("~/GitHub/DS5500-fall2020-rb-bc-ek-rm/Shiny App")

d.suffolk <- read_csv("data/prop_change_suffolk_labeled.csv")
d.bernalillo <- read_csv("data/prop_change_bernalillo_unlabeled.csv")
d.davidson <- read_csv("data/prop_change_davidson_unlabeled.csv")
d.douglas <- read_csv("data/prop_change_douglas_unlabeled.csv")
d.elpaso <- read_csv("data/prop_change_elpaso_unlabeled.csv")
d.fresno <- read_csv("data/prop_change_fresno_unlabeled.csv")
d.hennepin <- read_csv("data/prop_change_hennepin_unlabeled.csv")
d.sanfran <- read_csv("data/prop_change_sanfrancisco_unlabeled.csv")

tracts <- read_csv("data/tracts_geography.csv")

# Transform data that will be used for clustering ------------------------------
# SUFFOLK COUNTY
c.suffolk.1 <- scale(d.suffolk %>%
                       filter(Data.Measurement.Year == "1990-2000") %>%
                       select(3:34)) # First time frame & keep numeric data cols
c.suffolk.2 <- scale(d.suffolk %>%
                       filter(Data.Measurement.Year == "2000-2010") %>%
                       select(3:34)) # Second time frame & keep numeric data cols

# BERNALILLO COUNTY
c.bernalillo.1 <- scale(d.bernalillo %>%
                       filter(Data.Measurement.Year == "1990-2000") %>%
                       select(5:36)) # First time frame & keep numeric data cols
c.bernalillo.2 <- scale(d.bernalillo %>%
                       filter(Data.Measurement.Year == "2000-2010") %>%
                       select(5:36)) # Second time frame & keep numeric data cols

# DAVIDSON COUNTY
c.davidson.1 <- scale(d.davidson %>%
                       filter(Data.Measurement.Year == "1990-2000") %>%
                       select(5:36)) # First time frame & keep numeric data cols
c.davidson.2 <- scale(d.davidson %>%
                       filter(Data.Measurement.Year == "2000-2010") %>%
                       select(5:36)) # Second time frame & keep numeric data cols

# DOUGLAS COUNTY
c.douglas.1 <- scale(d.douglas %>%
                       filter(Data.Measurement.Year == "1990-2000") %>%
                       select(5:36)) # First time frame & keep numeric data cols
c.douglas.2 <- scale(d.douglas %>%
                       filter(Data.Measurement.Year == "2000-2010") %>%
                       select(5:36)) # Second time frame & keep numeric data cols

# EL PASO COUNTY
c.elpaso.1 <- scale(d.elpaso %>%
                       filter(Data.Measurement.Year == "1990-2000") %>%
                       select(5:36) %>% # First time frame & keep numeric data cols
                       complete.cases())
  
c.elpaso.2 <- scale(d.elpaso %>%
                       filter(Data.Measurement.Year == "2000-2010") %>%
                       select(5:36) %>% # Second time frame & keep numeric data cols
                       complete.cases())
                      
# FRESNO COUNTY
c.fresno.1 <- scale(d.fresno %>%
                       filter(Data.Measurement.Year == "1990-2000") %>%
                       select(5:36)) # First time frame & keep numeric data cols
c.fresno.2 <- scale(d.fresno %>%
                       filter(Data.Measurement.Year == "2000-2010") %>%
                       select(5:36)) # Second time frame & keep numeric data cols

# HENNEPIN COUNTY
c.hennepin.1 <- scale(d.hennepin %>%
                       filter(Data.Measurement.Year == "1990-2000") %>%
                       select(5:36)) # First time frame & keep numeric data cols
c.hennepin.2 <- scale(d.hennepin %>%
                       filter(Data.Measurement.Year == "2000-2010") %>%
                       select(5:36)) # Second time frame & keep numeric data cols

# SAN FRANCISCO COUNTY
c.sanfran.1 <- scale(d.sanfran %>%
                       filter(Data.Measurement.Year == "1990-2000") %>%
                       select(5:36)) # First time frame & keep numeric data cols
c.sanfran.2 <- scale(d.sanfran %>%
                       filter(Data.Measurement.Year == "2000-2010") %>%
                       select(5:36)) # Second time frame & keep numeric data cols

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
                  choices = c("Exploratory Data Analysis" = "eda",
                              "Clustering" = "clustering"),
                  selected = "eda"
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
      
      br(),
      
      # Conditional panel - Select # of clusters -------------------------------
      conditionalPanel(condition = "input.selection == clustering",
                       radioButtons(inputId = "k", label = "Select the number of clusters:",
                                    choices = list("2" = 2, "3" = 3, "4" = 4, "5" = 5),
                                    selected = 2))
      
      
    ),
    
    mainPanel(
      
      br(),
      
      plotOutput(outputId = "mainplot")

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
  
  
  
  # REACTIVE CITY SELECTOR (clustering -- there will be TWO reactive variables for each timeframe):
  clusdata1 <- reactive({
    
    req(input$city)
    l <- list()
    
    # Iterate through the number of chosen cities ( = length of the list)
    for (i in 1:length(input$city)) {
      
      # Add each city's dataset to the list
      if (input$city[i] == "boston") {
        l[[i]] <- c.suffolk.1
      } else if (input$city[i] == "abq") {
        l[[i]] <- c.bernalillo.1
      } else if (input$city[i] == "elpaso") {
        l[[i]] <- c.elpaso.1
      } else if (input$city[i] == "fresno") {
        l[[i]] <- c.fresno.1
      } else if (input$city[i] == "sanfran") {
        l[[i]] <- c.sanfran.1
      } else if (input$city[i] == "omaha") {
        l[[i]] <- c.douglas.1
      } else if (input$city[i] == "nashville") {
        l[[i]] <- c.davidson.1
      } else if (input$city[i] == "minneapolis") {
        l[[i]] <- c.hennepin.1
      }
      
    }
    
    l
    
  })
  clusdata2 <- reactive({
    
    req(input$city)
    l <- list()
    
    # Iterate through the number of chosen cities ( = length of the list)
    for (i in 1:length(input$city)) {
      
      # Add each city's dataset to the list
      if (input$city[i] == "boston") {
        l[[i]] <- c.suffolk.2
      } else if (input$city[i] == "abq") {
        l[[i]] <- c.bernalillo.2
      } else if (input$city[i] == "elpaso") {
        l[[i]] <- c.elpaso.2
      } else if (input$city[i] == "fresno") {
        l[[i]] <- c.fresno.2
      } else if (input$city[i] == "sanfran") {
        l[[i]] <- c.sanfran.2
      } else if (input$city[i] == "omaha") {
        l[[i]] <- c.douglas.2
      } else if (input$city[i] == "nashville") {
        l[[i]] <- c.davidson.2
      } else if (input$city[i] == "minneapolis") {
        l[[i]] <- c.hennepin.2
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
    
    if (input$selection == "eda") {
      
      plots <- list()
      
      # Iterate through the datasets
      for (i in 1:length(datalist())) {
  
        df <- datalist()[[i]]
  
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
        
        p <- ggplot(df) +
          # THE PLOTTED VARIABLE WILL BE REPLACED BY GENTRIFICATION LABELS ONCE DONE:
          geom_bar(aes(x = Data.Measurement.Year)) + #, fill = Gent_Label)) +  
          labs(title = name, x = "Gentrification Label", y = "Count") +
          theme(plot.title = element_text(hjust = 0.5, size = 12),
                legend.position = "none")
  
        plots[[i]] <- p
        plot(plots[[i]])
        
      }
      
      # The only thing I need to fix here is making the top label larger than the plot title:
      do.call("grid.arrange", c(plots, nrow = ceiling(length(input$city) / 2),
                                top = "Gentrification Labels for City Census Tracts"))
  
      
      
      
    } else if (input$selection == "clustering"){
        
        # Grab the lat/long for each census tract in the US:
        tracts <- read_csv("data/tracts_geography.csv")
        
        plots <- list()
        set.seed(1)
        
        # Iterate through the cluster datasets for each chosen city:
        for (i in 1:length(clusdata1())) {
          
          df1 <- as.data.frame(clusdata1()[[i]])
          df2 <- as.data.frame(clusdata2()[[i]])
          
          # Update 01 DECEMBER: the code has no problems with more than 2 clusters,
          # the issue seems to be with getting k from numclusters():
          kmeans1 <- kmeans(df1, centers = 3, nstart = 25)
          kmeans2 <- kmeans(df2, centers = 3, nstart = 25)
          
          # Get city-specific data
          if (input$city[i] == "boston") {
            t <- tracts[grepl("^25025", tracts$GEOID), ] # Keep tracts that begin w/ FIPS code
            fips <- 25025
            fulldata <- d.suffolk
            code <- "Boston, Massachusetts"
          } else if (input$city[i] == "abq") {
            t <- tracts[grepl("^35001", tracts$GEOID), ]
            fips <- 35001
            fulldata <- d.bernalillo
            code <- "Albuquerque, New Mexico"
          } else if (input$city[i] == "elpaso") {
            t <- tracts[grepl("^48141", tracts$GEOID), ]
            fips <- 48141
            fulldata <- d.elpaso
            code <- "El Paso, Texas"
          } else if (input$city[i] == "fresno") {
            t <- tracts[grepl("^06019", tracts$GEOID), ]
            fips <- 06019
            fulldata <- d.fresno
            code <- "Fresno, California"
          } else if (input$city[i] == "sanfran") {
            t <- tracts[grepl("^06075", tracts$GEOID), ]
            fips <- 06075
            fulldata <- d.sanfran
            code <- "San Francisco, California"
          } else if (input$city[i] == "omaha") {
            t <- tracts[grepl("^08035", tracts$GEOID), ]
            fips <- 08035
            fulldata <- d.douglas
            code <- "Omaha, Nebraska"
          } else if (input$city[i] == "nashville") {
            t <- tracts[grepl("^47037", tracts$GEOID), ]
            fips <- 47037
            fulldata <- d.davidson
            code <- "Nashville, Tennessee"
          } else if (input$city[i] == "minneapolis") {
            t <- tracts[grepl("^27053", tracts$GEOID), ]
            fips <- 27053
            fulldata <- d.hennepin
            code <- "Minneapolis, Minnesota"
          }
          
          # Get full FIPS code
          fulldata$GEOID <- as.numeric(paste(fips, 
                                             sprintf("%06d", fulldata$Census.Tract.Code),
                                             sep = ""))
          
          # Join datasets on GEOID
          geo <- fulldata %>% left_join(t) %>% as.data.frame() 
          # Add cluster assignments from k-means
          geo$Cluster90.00 <- kmeans1$cluster
          geo$Cluster00.10 <- kmeans2$cluster
          
          # Connect to Google APIs with your key:
          ggmap::register_google(key = "YOUR KEY HERE")
          
          # Determine the coordinates of the city:
          center <- geocode(code, source = "google")
          
          p1 <- qmap(c(lon = center$lon, lat = center$lat), zoom = 12) +
            geom_point(data = geo, aes(x = INTPTLONG, y = INTPTLAT, 
                                       color = as.factor(geo$Cluster90.00), size = 2)) +
            #scale_color_manual(breaks = c("1", "2"), values = c("darkgoldenrod1", "darkorchid4")) +
            scale_size(guide = 'none') +
            labs(title = code, color = "Cluster") 
          p2 <- qmap(c(lon = center$lon, lat = center$lat), zoom = 12) +
            geom_point(data = geo, aes(x = INTPTLONG, y = INTPTLAT, 
                                       color = as.factor(geo$Cluster00.10), size = 2)) +
            #scale_color_manual(breaks = c("1", "2"), values = c("darkgoldenrod1", "darkorchid4")) +
            scale_size(guide = 'none') +
            labs(color = "Cluster")
          
          p <- grid.arrange(p1, p2, ncol = 2)
          
          plots[[i]] <- p
          plot(plots[[i]])
          
        }
        
        # The only thing I need to fix here is making the top label larger than the plot title:
        do.call("grid.arrange", c(plots, ncol = 1,
                                  top = "Cluster Assignments from 1990-2000 versus 2000-2010"))
        
    }
      
  })
  
}
  
  
  



# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)



