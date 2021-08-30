#https://covid.ourworldindata.org/data/owid-covid-data.csv

##Get Packages
library(tidyverse)
library(dplyr)
library(countrycode)
theme_jhp <- function (base_size = 10, base_family = "sans") 
{
  colors <- tibble::deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])
  (theme_foundation(base_size = base_size, base_family = base_family) + 
      theme(line = element_line(colour = "black"),
            rect = element_rect(fill = colors["Light Gray"], 
                                linetype = 0, colour = NA),
            text = element_text(colour = colors["Dark Gray"]), 
            ## axis.title = element_blank(), axis.text = element_text(), 
            axis.ticks = element_blank(), axis.line = element_blank(), 
            legend.background = element_rect(), legend.position = "bottom", 
            legend.direction = "horizontal", legend.box = "vertical", 
            panel.grid = element_line(colour = NULL), 
            panel.grid.major = element_line(colour = colors["Medium Gray"]), 
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
            plot.margin = unit(c(1, 1, 1, 1), "lines"),
            strip.background = element_rect()))
}

covid19.foriegn <- read.csv(url("https://covid.ourworldindata.org/data/owid-covid-data.csv"))

covid19.foriegn$date <- format(as.Date(covid19.foriegn$date, '%Y-%m-%d'), "%m/%d/%Y") %>% as.Date(., format="%m/%d/%Y")

covid19.foriegn$year <- format(as.Date(covid19.foriegn$date, '%Y-%m-%d'), "%Y")

covid19.foriegn$weeknum <-  format(covid19.foriegn$date, format = "%V")

covid19.foriegn.summarize <- dplyr::select(covid19.foriegn, -c(iso_code, date, continent,tests_units))

covid19.foriegn.summarize[covid19.foriegn.summarize$weeknum == 53 & covid19.foriegn.summarize$year == 2021, which(colnames(covid19.foriegn.summarize)=="year")] <- "2020"

covid19.foriegn.summarize.sum <- covid19.foriegn.summarize %>% 
  dplyr::select(location, weeknum, year, new_deaths_per_million) %>%
  group_by(location, weeknum, year) %>% 
  summarise(new_death = sum(new_deaths_per_million, na.rm = TRUE))

temp.name <- covid19.foriegn[,c("iso_code", "continent","location")] %>% unique()

covid19 <- left_join(covid19.foriegn.summarize.sum, temp.name)

covid19_2020 <- covid19[covid19$year == "2020",] 

covid19_2021 <- covid19[covid19$year == "2021",]

covid19_2020$Week_Number <- covid19_2020$weeknum %>% as.numeric()
covid19_2021$Week_Number <- covid19_2021$weeknum %>% as.numeric()

temp <- rbind(covid19_2020, covid19_2021)

covid19_2020 <- temp[temp$year == "2020",]

covid19_2021 <- temp[temp$year == "2021",]

covid19_2021$Week_Number <- covid19_2021$Week_Number + 53

covid19.final <- rbind(covid19_2020, covid19_2021)

covid19.final$Week_Number <- covid19.final$Week_Number %>% as.integer()
covid19.final$Year <- covid19.final$year %>% as.character()
covid19.final <- covid19.final %>% as.data.frame()

world <- covid19.final %>% filter(location == "World") %>% dplyr::select(Week_Number, Year, new_death)
asia <- covid19.final %>% filter(location == "Asia") %>% dplyr::select(Week_Number, Year, new_death)
usa <-  covid19.final %>% filter(location == "United States") %>% dplyr::select(Week_Number, Year, new_death)

colnames(world)[3] <- "world_cases"
colnames(asia)[3] <- "asia_cases"
colnames(usa)[3] <- "usa_cases"

covid19.final %>% 
  filter(location == "United States") %>%
  ggplot(aes(x = `Week_Number`, y = `new_death`)) +
  geom_line()+
  labs(x = "Week Past", y = "New Death", title = "United States") +
  theme_jhp()

covid19.final.world <- covid19.final %>% as.data.frame() %>% dplyr::select(., -c(location))

covid19.final %>% 
  filter(location == "World") %>%
  ggplot(aes(x = `Week_Number`, y = `new_death`)) +
  geom_line()+
  labs(x = "Week Past", y = "New Death", title = "United States") +
  theme_jhp()

