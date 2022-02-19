library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(data.table)

setwd(readClipboard()); getwd()
df_2021 <- read_excel('2021.xlsx') %>% data.frame
df_2021 <- df_2021 %>% select(판매일자, 판매시간, 매출금액) %>% na.omit() # %>% mutate(Time = substr(판매시간, 1,5))



df_weather <- fread('weather.csv')
df_weather <- df_weather %>% dplyr::rename(Temporature = '기온(°C)', Rain = '누적강수량(mm)') %>% select(일시, Temporature, Rain) %>% mutate(Year = substr(일시, 1, 10), Time = substr(일시, 12, 16), Rain_TF = ifelse(Rain == 0, 0, 1)) %>% select(Year, Time, Temporature, Rain, Rain_TF)
head(df_weather)
#          Year  Time Temporature Rain Rain_TF
# 1: 2021-01-01 00:01        -4.1    0       0
# 2: 2021-01-01 00:02        -4.1    0       0
# 3: 2021-01-01 00:03        -4.1    0       0
# 4: 2021-01-01 00:04        -4.1    0       0
# 5: 2021-01-01 00:05        -4.1    0       0
# 6: 2021-01-01 00:06        -4.0    0       0



# 음식 메뉴 가져오기기
df_menu <- read_excel('2021_menu.xlsx', skip = 6) 
df_menu <- df_menu %>% filter(!is.na(구분)) %>% select(구분, 메뉴명) %>% rowwise() %>% mutate(menu = case_when(
  구분 == '판매' ~ 메뉴명
)) %>% data.frame
head(df_menu)
#         구분       메뉴명         menu
# 1 2021-01-01     11:02:45         <NA>
# 2       판매   매콤돈가스   매콤돈가스
# 3 2021-01-01     11:28:57         <NA>
# 4       판매         배달         배달
# 5       판매   매콤돈가스   매콤돈가스
# 6       판매 치즈롤돈가스 치즈롤돈가스


for(i in 1:nrow(df_menu)){
  if(df_menu[i, '구분'] == '판매'){
    df_menu[i, '구분'] <- df_menu[i-1, '구분']
    df_menu[i, '메뉴명'] <- df_menu[i-1, '메뉴명']
  }
}
df_menu <- df_menu %>% na.omit()
head(df_menu)
#         구분   메뉴명         menu
# 2 2021-01-01 11:02:45   매콤돈가스
# 4 2021-01-01 11:28:57         배달
# 5 2021-01-01 11:28:57   매콤돈가스
# 6 2021-01-01 11:28:57 치즈롤돈가스
# 7 2021-01-01 11:28:57         우동
# 9 2021-01-01 11:52:02       물냉면



maecom <- df_2021 %>% left_join(df_menu, by = c('판매일자' = '구분', '판매시간' = '메뉴명')) %>% dplyr::rename(Year = '판매일자') %>% mutate(Time = substr(판매시간, 1, 5)) %>% select(Year, Time, 매출금액, menu) %>% left_join(df_weather, by = c('Year', 'Time')) %>% na.omit()



# install.packages("arules")
library(arules)

# 비오는날 인기 음식은?
# 비오는 시간 추출
maecom_rain <- maecom %>% 
  filter(Rain_TF == 1, !menu %in% c('곱배기', '배달', '공기밥', '배달료', '소스', '음료', '사리추가')) %>% 
  mutate(ID = paste0(Year, ' ', Time)) %>% 
  mutate(menu_1 = case_when(
    menu == '돈냉면 사리추가' ~ '돈냉면',
    menu == '돈냉면곱배기' ~ '돈냉면',
    menu == '돈냉면곱배기 사리추가' ~ '돈냉면',
    menu == '매운돈가스곱배기' ~ '매운돈가스',
    menu == '매콤돈가스곱배기' ~ '매콤돈가스',
    menu == '무진장돈가스곱배기' ~ '무진장돈가스',
    menu == '순한돈가스곱배기' ~ '순한돈가스',
    menu == '치즈돈까스곱배기' ~ '치즈롤돈가스',
    TRUE ~ menu
  ))

# table(maecom_rain$menu_1)

maecom_rain.list <- split(maecom_rain$menu_1, maecom_rain$ID)
# head(maecom_rain.list)

maecom_rain.trans <- as(maecom_rain.list, "transactions")
# inspect(head(maecom_rain.trans))
# summary(maecom_rain.trans)

itemFrequencyPlot(maecom_rain.trans, topN=10, type="absolute", xlab="Item Name", ylab="Frequency (absolute)", main="Asolute Item Frequency Plot", col="pink")



# 겨울철 인기 메뉴는?
maecom_winter <- maecom %>% 
  mutate(Month = month(Year)) %>%
  filter(Month %in% c(12,1,2), !menu %in% c('곱배기', '배달', '공기밥', '배달료', '소스', '음료', '사리추가')) %>% 
  mutate(ID = paste0(Year, ' ', Time)) %>% 
  mutate(menu_1 = case_when(
    menu == '돈냉면 사리추가' ~ '돈냉면',
    menu == '돈냉면곱배기' ~ '돈냉면',
    menu == '돈냉면곱배기 사리추가' ~ '돈냉면',
    menu == '매운돈가스곱배기' ~ '매운돈가스',
    menu == '매콤돈가스곱배기' ~ '매콤돈가스',
    menu == '무진장돈가스곱배기' ~ '무진장돈가스',
    menu == '순한돈가스곱배기' ~ '순한돈가스',
    menu == '치즈돈까스곱배기' ~ '치즈롤돈가스',
    TRUE ~ menu
  ))

head(maecom_winter)
table(maecom_winter$menu_1)

maecom_winter.list <- split(maecom_winter$menu_1, maecom_winter$ID)
# head(maecom_winter.list)

maecom_winter.trans <- as(maecom_winter.list, "transactions")
# inspect(head(maecom_winter.trans))
# summary(maecom_winter.trans)

itemFrequencyPlot(maecom_winter.trans, topN=10, type="relative", xlab="메뉴", ylab="Frequency (relative)", col="pink", main = 'Winter')

# 여름철 인기 메뉴는?
maecom_summer <- maecom %>% 
  mutate(Month = month(Year)) %>%
  filter(Month %in% c(6,7,8), !menu %in% c('곱배기', '배달', '공기밥', '배달료', '소스', '음료', '사리추가')) %>% 
  mutate(ID = paste0(Year, ' ', Time)) %>% 
  mutate(menu_1 = case_when(
    menu == '돈냉면 사리추가' ~ '돈냉면',
    menu == '돈냉면곱배기' ~ '돈냉면',
    menu == '돈냉면곱배기 사리추가' ~ '돈냉면',
    menu == '매운돈가스곱배기' ~ '매운돈가스',
    menu == '매콤돈가스곱배기' ~ '매콤돈가스',
    menu == '무진장돈가스곱배기' ~ '무진장돈가스',
    menu == '순한돈가스곱배기' ~ '순한돈가스',
    menu == '치즈돈까스곱배기' ~ '치즈롤돈가스',
    TRUE ~ menu
  ))

head(maecom_summer)
table(maecom_summer$menu_1)

maecom_summer.list <- split(maecom_summer$menu_1, maecom_summer$ID)
# head(maecom_summer.list)

maecom_summer.trans <- as(maecom_summer.list, "transactions")
# inspect(head(maecom_summer.trans))
# summary(maecom_summer.trans)

itemFrequencyPlot(maecom_summer.trans, topN=10, type="relative", xlab="메뉴", ylab="Frequency (relative)", col="pink", main = 'Summer')


# 연관분석
maecom_rules <- maecom %>% 
  filter(!menu %in% c('곱배기', '배달', '공기밥', '배달료', '소스', '음료', '사리추가')) %>% 
  mutate(ID = paste0(Year, ' ', Time)) %>% 
  mutate(menu_1 = case_when(
    menu == '돈냉면 사리추가' ~ '돈냉면',
    menu == '돈냉면곱배기' ~ '돈냉면',
    menu == '돈냉면곱배기 사리추가' ~ '돈냉면',
    menu == '매운돈가스곱배기' ~ '매운돈가스',
    menu == '매콤돈가스곱배기' ~ '매콤돈가스',
    menu == '무진장돈가스곱배기' ~ '무진장돈가스',
    menu == '순한돈가스곱배기' ~ '순한돈가스',
    menu == '치즈돈까스곱배기' ~ '치즈롤돈가스',
    TRUE ~ menu
  ))
maecom_rules.list <- split(maecom_rules$menu_1, maecom_rules$ID)
# head(maecom_rules.list)

maecom_rules.trans <- as(maecom_rules.list, "transactions")
maecom_rules <- apriori(maecom_rules.trans)
inspect(maecom_rules)
