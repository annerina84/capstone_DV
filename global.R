#import library
library(dplyr)
library(ggplot2)
library(scales)
library(glue)
library(plotly)
library(leaflet)
library(lubridate)
library(DT)
#library(tidyverse)
#library(hrbrthemes)
options(scipen = 100)

library(shiny)
library(shinydashboard)

# read data
data <- read.csv("data_input/katalog_gempa.csv")


# cleansing data
data$tgl <- ymd(data$tgl)
data$ot <- hms(data$ot)
data_cleans <- data %>%
  select(-strike1, -dip1, -rake1, -dip2, -strike2, -rake2) %>%
  mutate(hari= wday(tgl, label = TRUE), bulan = month(tgl, label= TRUE), tahun = year(tgl)) %>%
  filter(tahun!=2008 &tahun != 2023 & mag <= 8 & mag >= 4) %>%
  mutate(tahun=as.factor(tahun), bulan=as.factor(bulan), hari=as.factor(hari), remark = as.factor(remark))

match.row <- match(unique(data_cleans$remark), data_cleans$remark)
remark_pos <- data_cleans[match.row, ]
remark_pos <-remark_pos %>% 
  select(-c(tgl, ot, depth, mag, bulan, hari, tahun)) %>% 
  mutate(frek = c(1851, 1875, 3993, 127, 1102, 305, 1527, 119, 543, 475, 1731, 401,  1532, 1293, 391,125, 
                  537, 1403, 582, 300, 291, 78, 23, 1879 ,184, 599, 751, 678, 30, 1041, 1087, 660, 804,  80, 
                  218, 10, 151, 6, 470,  41, 164, 15, 11, 23, 92, 82, 7, 2, 96, 164, 3))
remark_pos10 <- remark_pos %>% arrange(-frek) %>% head(10)
remark_pos10$frek <- as.character(remark_pos10$frek)

gempa_banda_hari <- data_cleans %>% 
  select(tgl, lat, lon, depth, remark, mag) 
