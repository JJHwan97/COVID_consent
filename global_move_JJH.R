https://covid.ourworldindata.org/data/owid-covid-data.csv

covid19.foriegn <- read.csv(url("https://covid.ourworldindata.org/data/owid-covid-data.csv"))

covid19.foriegn$date <- format(as.Date(covid19.foriegn$date, '%Y-%m-%d'), "%m/%d/%Y") %>% as.Date(., format="%m/%d/%Y")

covid19.foriegn$year <- format(as.Date(covid19.foriegn$date, '%Y-%m-%d'), "%Y")

covid19.foriegn$weeknum <-  format(covid19.foriegn$date, format = "%V")

covid19.foriegn.summarize <- dplyr::select(covid19.foriegn, -c(iso_code, date, continent,tests_units))

covid19.foriegn.summarize.sum <- covid19.foriegn.summarize %>% group_by(location, weeknum, year) %>% summarise_all(list(sum), na.rm = TRUE)

temp.name <- covid19.foriegn[,c("iso_code", "continent","location")] %>% unique()

covid19 <- left_join(covid19.foriegn.summarize.sum, temp.name)

covid19_2020 <- covid19[covid19$year == "2020",]

covid19_2021 <- covid19[covid19$year == "2021",]

covid19_2021 <- covid19_2021 %>% filter(weeknum != 53)


covid19_2020$week_past <- covid19_2020$weeknum %>% as.numeric()
covid19_2021$week_past <- covid19_2021$weeknum %>% as.numeric()

covid19_2021$week_past <- covid19_2021$week_past + 53

covid19.final <- rbind(covid19_2020, covid19_2021)

covid19.final$week_past <- covid19.final$week_past %>% as.integer()
covid19.final$year <- covid19.final$year %>% as.character()


covid19.final %>% 
  filter(location == "Argentina") %>%
  ggplot(aes(x = `week_past`, y = `new_deaths`)) +
  geom_line()+
  labs(x = "Week Past", y = "New Death", title = "Argentina") +
  theme_jhp()

covid19.final.world <- covid19.final %>% as.data.frame() %>% dplyr::select(., -c(location))

covid19.final.world %>%
  dplyr::select(., -c(iso_code, continent,year,weeknum)) %>%
  group_by(week_past) %>% summarise_all(list(sum)) %>%
  ggplot(aes(x = `week_past`, y = `new_deaths`)) +
  geom_line()+
  labs(x = "Week Past", y = "New Death", title = "World") +
  theme_jhp()
