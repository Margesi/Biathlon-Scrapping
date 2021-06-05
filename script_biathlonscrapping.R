library(RSelenium)
library(magrittr)
library(rvest)
library(tidyverse)

rD <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer, browser = "firefox")

remDr <- rD[["client"]]

results<- function(year,gender,wcnr) {
  
remDr$navigate(paste0("https://www.realbiathlon.com/races.html?raceId=BT",year,"SWRLCP",wcnr,"S",gender,"SP&localtime=false&level=1"))
Sys.sleep(8)

source1<-remDr$getPageSource()[[1]] %>% 
  read_html() %>%
  html_table()

res <-source1[[1]][-1,] 
}



shooting<- function(year,gender,wcnr) {
remDr$navigate(paste0("https://www.realbiathlon.com/races.html?raceId=BT",year,"SWRLCP",wcnr,"S",gender,"SP&year=",year,"&level=1&discipline=Total&category=shooting&relative=total&overview=false&min=120&chart=false&stat=&rank=false&movingavg=none&localtime=false&compare=total"))
  Sys.sleep(8)
  source2<-remDr$getPageSource()[[1]] %>% 
  read_html() %>%
  html_table()
res1<-source2[[1]][-1,] 

}


resM202102<-results("2021","M","02")
shoot202102<-shooting("2021","M","02")
resW201904<-results("2021","W","01")


remDr$close()
rm(list = ls())
  # %>%
  # slice(-1)
