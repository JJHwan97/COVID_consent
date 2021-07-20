library(openxlsx)
library(dplyr)
library(tidyr)

setwd("C:\\Users\\joshu\\Desktop\\move\\read\\temp2")

file_list <- list.files(path="C:\\Users\\joshu\\Desktop\\move\\read\\temp2")

file_list <- list.files(path="C:\\Users\\joshu\\Desktop\\move\\read\\temp")

for (i in 1:length(file_list)){
  data <- read.xlsx(xlsxFile = file_list[i], fillMergedCells = TRUE, colNames = FALSE)
#data <- read.xlsx(xlsxFile = "C:\\Users\\joshu\\Desktop\\move\\?̿뷮 ??ǥ(???????෮)_20210622 - 2021-06-22T192125.768.xlsx", fillMergedCells = TRUE, colNames = FALSE)
  data <- data %>% as.matrix()
  
  name <- c()
  
  for(j in 1:55){
    a <- data[1,j]
    b <- data[2,j]
    temp <- paste(a,".",b)
    if (length(name) ==0) {
      name <- c(temp)
    }else{
      name <- c(name, temp)
    }
  }
  
  name <- c("?õ??ڵ?","?õ?","?ñ???","??","?̿???��??",name[6:55])
  
  data <- data %>% as.data.frame()
  
  colnames(data) <- name
  
  data <- data[-(1:2),]
  
  new <- pivot_longer(data=data,
               cols=`?հ? . ?߻???`:`03 . ??????`,
               names_to=c("name","name1"),
               names_sep ='\\.')
  
  date <- new[nrow(new),4]
  
  day <- substr(date,1,nchar(date)-3)
  day <- format(as.Date(day, '%Y-%m-%d'), "%m/%d/%Y") %>% as.Date(., format="%m/%d/%Y")
  
  new[,4] <- day
  
  assign(paste0("clean",".",day),new)
  
  write.csv(get(paste0("clean",".",day)),paste0("C:\\Users\\joshu\\Desktop\\move\\forpython\\","clean",".",day,".csv"))
f_list <- mget(ls(pattern = "^clean.2019*"))

final.2019 <- dplyr::bind_rows(df_list)

df_list <- mget(ls(pattern = "^clean.2020*"))

final.2020 <- dplyr::bind_rows(df_list)

df_list <- mget(ls(pattern = "^clean.2021*"))

final.2021 <- dplyr::bind_rows(df_list)

complete %>% 
  group_by(year = year(date), week = week(date)) %>% 
  summarise_if(is.numeric, sum)
