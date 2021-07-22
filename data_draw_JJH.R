library(readr)
# 
# complete2019 <- read_csv("C:/Users/joshu/Desktop/move/complete2019.csv", 
#                          skip = 2)
# complete2020 <- read_csv("C:/Users/joshu/Desktop/move/complete2020.csv", 
#                          skip = 2)
# complete2021 <- read_csv("C:/Users/joshu/Desktop/move/complete2021.csv", 
#                          skip = 2)
# 
# colnames(complete2019)[10] <- "Move"
# complete2019[11] <- log(complete2019[10])
# colnames(complete2019)[11] <- "Ln(Move)"
# 
# colnames(complete2020)[10] <- "Move"
# complete2020[11] <- log(complete2020[10])
# colnames(complete2020)[11] <- "Ln(Move)"
# 
# colnames(complete2021)[10] <- "Move"
# complete2021[11] <- log(complete2021[10])
# colnames(complete2021)[11] <- "Ln(Move)"
# 
# final <- rbind(complete2019,complete2020,complete2021)

final <- read_csv("C:/Users/joshu/Desktop/move/final.csv")
final <- final %>% as.data.frame()

temp <- final[,c("year", "Week_Number")] %>% unique()

library(tidyverse)

final2019 <- final %>% filter(year == 2019)
final2020 <- final %>% filter(year == 2020)
final2021 <- final %>% filter(year == 2021)

final2019.merge <- final2019 %>% filter(type != "합계" & type != "~") %>%
  group_by(city2, Week_Number) %>%
  summarize(value2019 = sum(value))

final2020.merge <- final2020 %>% filter(type != "합계" & type != "~") %>%
  group_by(city2, Week_Number) %>%
  summarize(value2020 = sum(value))

final2021.merge <- final2021 %>% filter(type != "합계" & type != "~") %>%
  group_by(city2, Week_Number) %>%
  summarize(value2021 = sum(value))

yearbind <- full_join(final2019.merge, final2020.merge)
yearbind <- full_join(yearbind, final2021.merge)

yearbind$com20.19 <- (yearbind$value2020 - yearbind$value2019)/yearbind$value2019
yearbind$com21.19 <- (yearbind$value2021 - yearbind$value2019)/yearbind$value2019
yearbind$com21.20 <- (yearbind$value2021 - yearbind$value2020)/yearbind$value2020

yearbind <- yearbind %>% as.matrix()  %>% as.data.frame()

for ( i in 2:8){
  yearbind[,i] <- yearbind[,i] %>% as.numeric()
}

temp1 <- yearbind[,c(1,2,6)]
temp1[,4] <- 2020
colnames(temp1)[3] <- "change"
temp2 <- yearbind[,c(1,2,7)]
temp2[,4] <- 2021
temp2[,2] <- temp2[,2] + 53
colnames(temp2)[3] <- "change"


yearbind.merge <- rbind(temp1, temp2)
colnames(yearbind.merge)[4]<-"Year"
yearbind.merge[,4]<- yearbind.merge[,4] %>% as.character()

yearbind.merge <- drop_na(yearbind.merge)

yearbind.merge <- left_join(yearbind.merge, world)
yearbind.merge <- left_join(yearbind.merge, asia)
yearbind.merge <- left_join(yearbind.merge, usa)

yearbind.merge[yearbind.merge$city2 == "강원도",1] <- "강원"
yearbind.merge[yearbind.merge$city2 == "경기도",1] <- "경기"
yearbind.merge[yearbind.merge$city2 == "경상남도",1] <- "경남"
yearbind.merge[yearbind.merge$city2 == "경상북도",1] <- "경북"
yearbind.merge[yearbind.merge$city2 == "광주광역시",1] <- "광주"
yearbind.merge[yearbind.merge$city2 == "대구광역시",1] <- "대구"
yearbind.merge[yearbind.merge$city2 == "대전광역시",1] <- "대전"
yearbind.merge[yearbind.merge$city2 == "부산광역시",1] <- "부산"
yearbind.merge[yearbind.merge$city2 == "서울특별시",1] <- "서울"
yearbind.merge[yearbind.merge$city2 == "세종특별자치시",1] <- "세종"
yearbind.merge[yearbind.merge$city2 == "울산광역시",1] <- "울산"
yearbind.merge[yearbind.merge$city2 == "인천광역시",1] <- "인천"
yearbind.merge[yearbind.merge$city2 == "전라남도",1] <- "전남"
yearbind.merge[yearbind.merge$city2 == "전라북도",1] <- "전북"
yearbind.merge[yearbind.merge$city2 == "제주특별자치도",1] <- "제주"
yearbind.merge[yearbind.merge$city2 == "충청남도",1] <- "충남"
yearbind.merge[yearbind.merge$city2 == "충청북도",1] <- "충북"

yearbind.merge <- left_join(yearbind.merge, covid.korea.merge.death)

yearbind.merge <- left_join(yearbind.merge, covid.korea.merge.cases)

yearbind.merge[is.na(yearbind.merge)] <- 0 

policychange <- read_excel("C:/Users/joshu/Desktop/policychange.xlsx", sheet = "Sheet3")

policychange <- pivot_longer(policychange, !Week_Number, names_to = "city2", values_to = "policy")

yearbind.merge <- left_join(yearbind.merge, policychange)

yearbind.merge <- left_join(yearbind.merge, covid.korea.merge.cases.total)

yearbind.merge <- left_join(yearbind.merge, covid.korea.merge.death.total)

yearbind.merge_2020 <- yearbind.merge %>% filter(Year == "2020")
yearbind.merge_2021 <- yearbind.merge %>% filter(Year == "2021")

yearbind.merge_2020$month <- as.Date(paste(2020, yearbind.merge_2020$Week_Number, 1, sep="-"), "%Y-%U-%u") %>%
  lubridate::month()
yearbind.merge_2021$month <- as.Date(paste(2021, yearbind.merge_2021$Week_Number, 1, sep="-"), "%Y-%U-%u") %>%
  lubridate::month()

yearbind.merge<-rbind(yearbind.merge_2020,yearbind.merge_2021)

data <- read_excel("C:/Users/joshu/Desktop/dataset_pop_political_rev.xlsx")
data <- data %>% as.data.frame()
data[,3] <- data[,3] %>% as.Date(format = "YYYY-mm-dd")
data$month <- data[,3] %>% lubridate::month()
colnames(data)[2] <- "city2"
data$Year <- data[,3] %>% lubridate::year()

# data_2020 <- data[data$Year == "2020",]
# 
# data_2021 <- data[data$Year == "2021",]
# 
# data_2020$Week_Number <- data_2020$Week_Number %>% as.numeric()
# data_2021$Week_Number <- data_2021$Week_Number %>% as.numeric()
# 
# data_2021$Week_Number <- data_2021$Week_Number + 53

# data <- rbind(data_2020, data_2021)
data$Year <- data$Year %>% as.character()

yearbind.merge <- left_join(yearbind.merge, data)

write.csv(yearbind.merge, "withpolicy.csv")

yearbind %>% 
  filter(city2 == "경기도" | city2 == "부산광역시" | city2 == "서울특별시") %>%
  ggplot(aes(x = `Week_Number`, y = `com20.19`)) +
  geom_line()+
  facet_wrap(~ city2) +
  labs(title = "(2020 - 2019)/2019", x = "Week Number", y = "Decrease by percent of 2019") +
  geom_hline(yintercept = 0, , color="red") +
  theme_jhp()

yearbind %>% 
  filter(city2 != "경기도" & city2!="부산광역시" & city2!="서울특별시") %>%
  ggplot(aes(x = `Week_Number`, y = `com20.19`)) +
  geom_line()+
  facet_wrap(~ city2) +
  labs(title = "(2020 - 2019)/2019", x = "Week Number", y = "Decrease by percent of 2019") +
  geom_hline(yintercept = 0, , color="red") +
  theme_jhp()

yearbind %>% 
  filter(city2 == "경기도" | city2 == "부산광역시" | city2 == "서울특별시") %>%
  ggplot(aes(x = `Week_Number`, y = `com21.19`)) +
  geom_line()+
  facet_wrap(~ city2) +
  labs(title = "(2021 - 2019)/2019", x = "Week Number", y = "Decrease by percent of 2019") +
  geom_hline(yintercept = 0, , color="red") +
  theme_jhp()

yearbind %>% 
  filter(city2 != "경기도" & city2!="부산광역시" & city2!="서울특별시") %>%
  ggplot(aes(x = `Week_Number`, y = `com21.19`)) +
  geom_line()+
  facet_wrap(~ city2) +
  labs(title = "(2021 - 2019)/2019", x = "Week Number", y = "Decrease by percent of 2019") +
  geom_hline(yintercept = 0, , color="red") +
  theme_jhp()

yearbind %>% 
  filter(city2 == "경기도" | city2 == "부산광역시" | city2 == "서울특별시") %>%
  ggplot(aes(x = `Week_Number`, y = `com21.20`)) +
  geom_line()+
  facet_wrap(~ city2) +
  labs(title = "(2021 - 2020)/2020", x = "Week Number", y = "Decrease by percent of 2020") +
  geom_hline(yintercept = 0, , color="red") +
  theme_jhp()

yearbind %>% 
  filter(city2 != "경기도" & city2!="부산광역시" & city2!="서울특별시") %>%
  ggplot(aes(x = `Week_Number`, y = `com21.20`)) +
  geom_line()+
  facet_wrap(~ city2) +
  labs(title = "(2021 - 2020)/2020", x = "Week Number", y = "Decrease by percent of 2020") +
  geom_hline(yintercept = 0, , color="red") +
  theme_jhp()
# 
# complete2019 %>% 
#   filter(city2 == "????Ư????" & time == "?հ?" & vector == "?߻???" & type != "?Ϲ???") %>%
#   ggplot(aes(x = `Week_Number`, y = `Move`, group = type, colour = type)) +
#   geom_line()+
#   facet_wrap(~ city3) +
#   labs(title = "2019") +
#   theme_jhp()
# 
# complete2020 %>% 
#   filter(city2 == "????Ư????" & time == "?հ?" & vector == "?߻???" & type == "?Ϲ???") %>%
#   ggplot(aes(x = Week_Number, y = `Move`, group = type, colour = type)) +
#   geom_line()+
#   facet_wrap(~ city3) +
#   labs(title = "2020") +
#   theme_jhp()
# 
# complete2020 %>% 
#   filter(city2 == "????Ư????" & time == "?հ?" & vector == "?߻???" & type != "?Ϲ???") %>%
#   ggplot(aes(x = Week_Number, y = `Move`, group = type, colour = type)) +
#   geom_line()+
#   facet_wrap(~ city3) +
#   labs(title = "2020") +
#   theme_jhp()
# 
# complete2021 %>% 
#   filter(city2 == "????Ư????" & time == "?հ?" & vector == "?߻???" & Week_Number != 53 & type == "?Ϲ???") %>%
#   ggplot(aes(x = Week_Number, y = `Move`, group = type, colour = type)) +
#   geom_line()+
#   facet_wrap(~ city3) +
#   labs(title = "2021") +
#   theme_jhp()
# 
# complete2021 %>% 
#   filter(city2 == "????Ư????" & time == "?հ?" & vector == "?߻???" & Week_Number != 53 & type != "?Ϲ???") %>%
#   ggplot(aes(x = Week_Number, y = `Move`, group = type, colour = type)) +
#   geom_line()+
#   facet_wrap(~ city3) +
#   labs(title = "2021") +
#   theme_jhp()
# 
# temp1 <- complete2019 %>% filter(type == "일반인" & city2 == "서울특별시")
# temp2 <- complete2020 %>% filter(type == "일반인" & city2 == "서울특별시")
# 


