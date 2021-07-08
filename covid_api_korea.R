install.packages("httr") # HTTP 불러오기

library(dplyr)
library(httr)
library(jsonlite)

# give directory
dir <- "C:\\Users\\joshu\\Desktop\\move"

#give date
today <- 20210704

url <-   paste0("http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19SidoInfStateJson?",
                "ServiceKey=","31W2Fodij75E8QVYmLZp9QDERO1%2FqJIvrIvdRTl495GjN%2BdLYbl9ELUTM1xGXkoPyAX21BjQ0%2BqccGi%2BKuSaAg%3D%3D",
                "&pageNo=1&numOfRows=10&startCreateDt=20190101&endCreateDt=",
                today)
data <- GET(url)

data.html <- content(data, as = "text")
str(data.html)

data.df.lite <- jsonlite::fromJSON(data.html)
dplyr::glimpse(data.df.lite)

covid.korea <- data.df.lite$response$body %>% as.data.frame()

write.csv(covid.korea, paste0(dir, "\\", today, "covidkorea",".csv"))

date <- covid.korea[,1]

day <- substr(date, start = 1, stop = 10)

day <- format(as.Date(day, '%Y-%m-%d'), "%m/%d/%Y") %>% as.Date(., format="%m/%d/%Y")

covid.korea <- cbind(day,covid.korea)

covid.korea$year <- format(as.Date(covid.korea$day, '%Y-%m-%d'), "%Y")

covid.korea$weeknum <-  format(covid.korea$day, format = "%V")

covid.korea$dayofweek <- weekdays(as.Date(covid.korea$day))

covid.korea.sundays <- covid.korea %>% filter(dayofweek == "일요일")

# covid.korea[covid.korea$items.item.qurRate == "-","items.item.qurRate"] <- "."
# 
# temp <- covid.korea$items.item.qurRate 
# 
# covid.korea$items.item.qurRate <- covid.korea$items.item.qurRate %>% as.numeric()
# 
# covid.korea.temp <- covid.korea %>% filter(items.item.gubun == "서울")

# covid.korea.sum <- covid.korea %>% 
#   dplyr::select(., -c(items.item.createDt, items.item.gubunCn, items.item.gubunEn, items.item.stdDay, items.item.updateDt,day, items.item.qurRate)) %>%
#   group_by(items.item.gubun, weeknum, year) %>% summarise_all(list(sum), na.rm = TRUE)

covid19_2020 <- covid.korea.sundays[covid.korea.sundays$year == "2020",]

covid19_2021 <- covid.korea.sundays[covid.korea.sundays$year == "2021",]

covid19_2021 <- covid19_2021 %>% filter(weeknum != 53)

covid19_2020$weeknum <- covid19_2020$weeknum %>% as.numeric()
covid19_2021$weeknum <- covid19_2021$weeknum %>% as.numeric()

covid19_2021$weeknum <- covid19_2021$weeknum + 53

covid.korea.final <- rbind(covid19_2020, covid19_2021)

covid.korea.final %>% 
  filter(items.item.gubun != "합계") %>%
  ggplot(aes(x = `weeknum`, y = `items.item.deathCnt`)) +
  geom_point()+
  facet_wrap(~ items.item.gubun) +
  # labs(title = "2019") +
  theme_jhp()


covid.korea.final <- covid.korea.final %>% arrange(items.item.gubun, day)

covid.korea.final<- covid.korea.final %>% 
  group_by(items.item.gubun) %>%
  mutate(l.items.item.deathCnt = lag(items.item.deathCnt))

covid.korea.final$increase <- covid.korea.final$items.item.deathCnt - covid.korea.final$l.items.item.deathCnt

covid.korea.final %>% 
  filter(items.item.gubun != "합계") %>%
  ggplot(aes(x = `weeknum`, y = `increase`)) +
  geom_line()+
  facet_wrap(~ items.item.gubun) +
  # labs(title = "2019") +
  theme_jhp()
