library(dplyr)
library(ggplot2)

dat1 <- read.csv("./data/digital_PCR_result.csv", skip = 1, header = FALSE) 

year1 <- as.numeric(sapply(strsplit(as.character(dat1[[5]][dat1[[1]] != "Title"]), " "), last))
title1 <- as.character(dat1[[1]][dat1[[1]] != "Title"])

dat2 <- read.csv("./data/dpcR_result.csv", skip = 1, header = FALSE) 

year2 <- as.numeric(sapply(strsplit(as.character(dat2[[5]][dat2[[1]] != "Title"]), " "), last))
title2 <- as.character(dat2[[1]][dat2[[1]] != "Title"])

data.frame(title = c(title1, title2), year = c(year1, year2)) %>% 
  filter(!duplicated(.)) %>% 
  ggplot(aes(x = year)) +
  geom_bar()


year_plot <- data.frame(year = c(year1, year2))

ggplot(year_plot, aes(x = year)) + geom_bar()
