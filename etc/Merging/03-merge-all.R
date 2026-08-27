#load libraries----
library(groundhog)
#install.packages("systemfonts", type = "binary")
library(systemfonts)
my.date <- "2025-10-01"
pkgs <- c("tidyverse",
          "ggplot2",
          "cowplot",
          "RColorBrewer",
          "randomcoloR",
          "forcats",
          "readr",
          "maps",
          "mapproj",
          "viridis",
          "reshape2",
          "english",
          "purrr")
groundhog.library(pkgs,my.date, ignore.deps = "systemfonts")

options(tidyverse.quiet = TRUE)

#set working directory and load data----
#setwd(getSrcDirectory()[1]) # run this line if using plain R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #run this line if using RStudio

#Visualize NumValue as non-scientific notation
options(scipen = 1000000000)

#load and merge data

all.data <- NULL





data.glossed <- read.csv("../glossed/all.data.checked.csv")
data.reglossed <- read.csv("../re-glossed/all.data.checked.csv",row.names = 1)
doculects.good <- read.csv("../doculects.to.use.in.analyses_20260816.csv")

data.glossed.unfiltered <- read.csv("../glossed/all.data.glossed.csv")
data.reglossed.unfiltered <- read.csv("../re-glossed/all.data.glossed.csv",row.names = 1)

data.merged <- rbind(data.glossed,
                     data.reglossed)

length(unique(data.merged$FileName))
# 5978 doculects
# 6190 if all.data.glossed.csv
write.csv(data.merged, "all_data.csv")



data.merged %>%
  left_join(doculects.good, join_by(FileName == File)) %>% 
  filter(Use_in_analyses == "YES") -> data.final
  
table(doculects.good$Use_in_analyses)

length(unique(data.final$FileName))
# 4856 doculects
# 5048 if all.data.glossed.csv
write.csv(data.merged, "all_data_filtered.csv")

#check mismatches between glossed and checked
data.glossed.unfiltered %>%
  filter(!(FileName %in% data.glossed$FileName)) -> problematic.glossed


  
data.reglossed.unfiltered %>%
  filter(!(FileName %in% data.reglossed$FileName)) -> problematic.reglossed
