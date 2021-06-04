library(RSelenium)
library(magrittr)
library(rvest)
library(tidyverse)
library(xlsx)

rD <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer, browser = "firefox")

remDr <- rD[["client"]]
remDr$navigate("https://www.realbiathlon.com/races.html?raceId=BT2021SWRLCP01SMSP&localtime=false&level=1")

source1<-remDr$getPageSource()[[1]] %>% 
  read_html() %>%
  html_table()
res_sprintmen10km<-source1[[1]][-1,]

remDr$navigate("https://www.realbiathlon.com/races.html?raceId=BT2021SWRLCP01SMSP&year=2021&level=1&discipline=Total&category=shooting&relative=total&overview=false&min=120&chart=false&stat=&rank=false&movingavg=none&localtime=false&compare=total")
source2<-remDr$getPageSource()[[1]] %>% 
  read_html() %>%
  html_table()
shooting_sprintmen10km<-source2[[1]][-1,]

remDr$navigate("https://www.realbiathlon.com/races.html?raceId=BT2021SWRLCP01SWSP&localtime=false&level=1")

source3<-remDr$getPageSource()[[1]] %>% 
  read_html() %>%
  html_table()
res_sprintwomen10km<-source3[[1]][-1,]

remDr$navigate("https://www.realbiathlon.com/races.html?raceId=BT2021SWRLCP01SWSP&year=2021&level=1&discipline=Total&category=shooting&relative=total&overview=false&min=120&chart=false&stat=&rank=false&movingavg=none&localtime=false&compare=total")

source4<-remDr$getPageSource()[[1]] %>% 
  read_html() %>%
  html_table()
shooting_sprintwomen10km<-source4[[1]][-1,]

remDr$close()
rm(list = ls())
  # %>%
  # slice(-1)
