# 시간대 별 손님 수 그래프 그리기

library(ggplot2)
library(dplyr)
library(readxl)
library(scales)
library(lubridate)

setwd(readClipboard()); getwd()
df_2018 <- read_excel('2018.xlsx') %>% data.frame
df_2019 <- read_excel('2019.xlsx') %>% data.frame
df_2020 <- read_excel('2020.xlsx') %>% data.frame
df_2021 <- read_excel('2021.xlsx') %>% data.frame

maecom <- rbind(df_2018, df_2019, df_2020, df_2021)
maecom <- maecom %>% select(판매일자, 판매시간, 매출금액) %>% na.omit()
str(maecom)

maecom %>% filter(year(판매일자) == '2021') %>% 
  mutate(Hour = hour(hms(판매시간)), Month = month(판매일자), Month = case_when(
    Month %in% c(12,1,2) ~ 'Winter',
    Month %in% c(3,4,5) ~ 'Spring',
    Month %in% c(6,7,8) ~ 'Summer',
    Month %in% c(9,10,11) ~ 'Autumn',
  )) %>%
  filter(Month == 'Winter') %>%
  group_by(Month, Hour) %>%
  summarise(NofCustomer = n()) %>%
  ggplot(aes(x=Hour, y=NofCustomer, colours = 'blue')) + 
  geom_line() +
  scale_x_continuous(breaks=seq(10, 22, 2)) +
  geom_point(size=3, shape=19, colour="blue") +
  ylab('Number of Customer') +
  ggtitle('Winter') +
  theme_bw() +
  theme(axis.text.y=element_blank())





