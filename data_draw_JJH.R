library(readr)

complete2019 <- read_csv("C:/Users/joshu/Desktop/move/complete2019.csv", 
                         skip = 2)
complete2020 <- read_csv("C:/Users/joshu/Desktop/move/complete2020.csv", 
                         skip = 2)
complete2021 <- read_csv("C:/Users/joshu/Desktop/move/complete2021.csv", 
                         skip = 2)

colnames(complete2019)[10] <- "Move"
complete2019[11] <- log(complete2019[10])
colnames(complete2019)[11] <- "Ln(Move)"

colnames(complete2020)[10] <- "Move"
complete2020[11] <- log(complete2020[10])
colnames(complete2020)[11] <- "Ln(Move)"

colnames(complete2021)[10] <- "Move"
complete2021[11] <- log(complete2021[10])
colnames(complete2021)[11] <- "Ln(Move)"

final <- rbind(complete2019,complete2020,complete2021)

library(tidyverse)

complete2019 %>% 
  filter(city2 == "????Ư????" & time == "?հ?" & vector == "?߻???" & type == "?Ϲ???") %>%
  ggplot(aes(x = `Week_Number`, y = `Move`, group = type, colour = type)) +
  geom_line()+
  facet_wrap(~ city3) +
  labs(title = "2019") +
  theme_jhp()

complete2019 %>% 
  filter(city2 == "????Ư????" & time == "?հ?" & vector == "?߻???" & type != "?Ϲ???") %>%
  ggplot(aes(x = `Week_Number`, y = `Move`, group = type, colour = type)) +
  geom_line()+
  facet_wrap(~ city3) +
  labs(title = "2019") +
  theme_jhp()

complete2020 %>% 
  filter(city2 == "????Ư????" & time == "?հ?" & vector == "?߻???" & type == "?Ϲ???") %>%
  ggplot(aes(x = Week_Number, y = `Move`, group = type, colour = type)) +
  geom_line()+
  facet_wrap(~ city3) +
  labs(title = "2020") +
  theme_jhp()

complete2020 %>% 
  filter(city2 == "????Ư????" & time == "?հ?" & vector == "?߻???" & type != "?Ϲ???") %>%
  ggplot(aes(x = Week_Number, y = `Move`, group = type, colour = type)) +
  geom_line()+
  facet_wrap(~ city3) +
  labs(title = "2020") +
  theme_jhp()

complete2021 %>% 
  filter(city2 == "????Ư????" & time == "?հ?" & vector == "?߻???" & Week_Number != 53 & type == "?Ϲ???") %>%
  ggplot(aes(x = Week_Number, y = `Move`, group = type, colour = type)) +
  geom_line()+
  facet_wrap(~ city3) +
  labs(title = "2021") +
  theme_jhp()

complete2021 %>% 
  filter(city2 == "????Ư????" & time == "?հ?" & vector == "?߻???" & Week_Number != 53 & type != "?Ϲ???") %>%
  ggplot(aes(x = Week_Number, y = `Move`, group = type, colour = type)) +
  geom_line()+
  facet_wrap(~ city3) +
  labs(title = "2021") +
  theme_jhp()

temp1 <- complete2019 %>% filter(type == "일반인" & city2 == "서울특별시")
temp2 <- complete2020 %>% filter(type == "일반인" & city2 == "서울특별시")



