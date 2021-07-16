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

yearbind <- left_join(final2019.merge, final2020.merge)
yearbind <- left_join(yearbind, final2021.merge)

yearbind$com20.19 <- (yearbind$value2020 - yearbind$value2019)/yearbind$value2019
yearbind$com21.19 <- (yearbind$value2021 - yearbind$value2019)/yearbind$value2019
yearbind$com21.20 <- (yearbind$value2021 - yearbind$value2020)/yearbind$value2020

yearbind <- yearbind %>% as.matrix()  %>% as.data.frame()

for ( i in 2:8){
  yearbind[,i] <- yearbind[,i] %>% as.numeric()
  
}

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


