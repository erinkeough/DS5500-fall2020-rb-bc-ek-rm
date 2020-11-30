
# Load packages ----------------------------------------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(readr)
library(ggplot2)
library(grid)
library(gridExtra)

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
                       select(5:36)) # First time frame & keep numeric data cols
c.elpaso.2 <- scale(d.elpaso %>%
                       filter(Data.Measurement.Year == "2000-2010") %>%
                       select(5:36)) # Second time frame & keep numeric data cols

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
      
      # 
      
    ),
    
    mainPanel(
      
      br(),
      
      plotOutput(outputId = "labelplot")

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
  
  
  # EDA OUTPUT PLOT:
  output$labelplot <- renderPlot({ 
    
    plots <- list()
    
    # Iterate through the datasets
    for (i in 1:length(datalist())) {

      df <- datalist()[[i]]

      # Add each city's dataset to the list
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
        geom_bar(aes(x = Data.Measurement.Year)) + #, fill = Gent_Label)) +   <- REPLACE VARIABLE
        labs(title = name, x = "Gentrification Label", y = "Count") +
        theme(plot.title = element_text(hjust = 0.5, size = 12),
              legend.position = "none")

      plots[[i]] <- p
      plot(plots[[i]])
      
    }
    
    # The only thing I need to fix here is making the top label larger than the plot title:
    do.call("grid.arrange", c(plots, nrow = ceiling(length(input$city) / 2),
                              top = "Gentrification Labels for City Census Tracts"))
  
  })
  
  # CLUSTERING OUTPUT PLOT:
  
  
  
}
  
  
  



# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)



