library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(data.table)

setwd(readClipboard()); getwd()
df_2018 <- read_excel('2018.xlsx') %>% data.frame
df_2019 <- read_excel('2019.xlsx') %>% data.frame
df_2020 <- read_excel('2020.xlsx') %>% data.frame
df_2021 <- read_excel('2021.xlsx') %>% data.frame

maecom <- rbind(df_2018, df_2019, df_2020, df_2021)
maecom <- maecom %>% select(판매일자, 판매시간, 매출금액) %>% na.omit()

maecom <- maecom %>% mutate(Time = substr(판매시간, 1,5))
str(maecom)

df_weather <- fread('weather.csv')
head(df_weather)
str(df_weather)
df_weather <- df_weather %>% dplyr::rename(Temporature = '기온(°C)', Rain = '누적강수량(mm)') %>% select(일시, Temporature, Rain) %>% mutate(Year = substr(일시, 1, 10), Time = substr(일시, 12, 16), Rain_TF = ifelse(Rain == 0, 0, 1))
str(df_weather)

maecom %>% left_join(df_weather, by = c('판매일자' = 'Year', 'Time')) %>% na.omit %>% select(판매일자, 판매시간, 매출금액, Temporature, Rain, Rain_TF) %>% group_by(Rain_TF) %>% summarise()



# 음식 메뉴 가져오기기

