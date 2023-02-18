library(shinydashboard)
library(shiny)
library(DT)
# Import libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(glue)
library(plotly)
library(lubridate)
library(readxl)
library(rgdal)
library(leaflet)
library(geojsonio)

# Settingan Agar tidak muncul numeric value
options(scipen = 9999)

# read data
data <- read_excel('assets/Adidas US Sales Datasets.xlsx', skip = 2)
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
data_map <- merge(x = data, y= states, by.x = 'State', by.y = 'name')

# cleansing data
data$Retailer <- factor(data$Retailer)
data$`Sales Method` <- factor(data$`Sales Method`)
data$Region <- factor(data$Region)
data$State <- factor(data$State)
data$City <- factor(data$City)
data$Product <- factor(data$Product)
data$year_invoice <- year(data$`Invoice Date`)
data$month_invoice <- month(data$`Invoice Date`)
data$day_invoice <- day(data$`Invoice Date`)

data_filter <- data %>% select(Region, State, City)

total_sales <- sum(data$`Total Sales`)

# Please type your code here
# Preparation data vids_count
