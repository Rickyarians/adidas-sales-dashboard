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

# Settingan Agar tidak muncul numeric value
options(scipen = 9999)

# read data
data <- read_excel('assets/Adidas US Sales Datasets.xlsx', skip = 2)



# cleansing data
data$Retailer <- factor(data$Retailer)
data$`Sales Method` <- factor(data$`Sales Method`)
data$Region <- factor(data$Region)
data$State <- factor(data$State)
data$City <- factor(data$City)
data$Product <- factor(data$Product)

data_filter <- data %>% select(Region, State, City)

total_sales <- sum(data$`Total Sales`)

# Please type your code here
# Preparation data vids_count
