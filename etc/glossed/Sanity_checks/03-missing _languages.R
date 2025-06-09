#load libraries----
library(groundhog)
my.date <- "2025-01-01"
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
          "stringr")
groundhog.library(pkgs,my.date)

options(tidyverse.quiet = TRUE)

#set working directory and load data----
#setwd(getSrcDirectory()[1]) # run this line if using plain R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #run this line if using RStudio
setwd('..')

#Visualize NumValue as non-scientific notation
options(scipen = 1000000000)


#-------
# languages  still to code
setwd("..")


# ############

# ############

#some languages have been coded from different doculects than in langs.to.code.

#Let's match our coded data with their corresponding language-level glottocodes (according to Glottolog 5.0)
read.csv("for_glossing/languages_ac84f3f2c4e79b2225f31db1426dcdd3de3edf4a.csv") -> langs.to.code
read.csv("../cldf/languages.csv") -> langs.full

#remove bookkeeping languages

langs.full %>%
  filter(Family != "Bookkeeping") %>%
  dplyr::select(c(ID,Glottocode,Macroarea,Family)) -> correspondences_nb

#add new data by Zariquiey
all.data %>%
  filter(grepl("zariquiey-", Language_ID) ) %>%
  distinct(Language_ID) %>%
  mutate(Glottocode =  str_match(Language_ID, "zariquiey-\\s*(.*?)\\s*-1")[,2]) %>%
  rename(ID = Language_ID) %>%
  mutate(Macroarea = "South America",
         Family = "Pano")-> tmp

correspondences_nb <- rbind(correspondences_nb,tmp)

all.data %>%
  mutate(Language_ID_mod = paste0("numerals-",Language_ID)) -> all.data_mod

#extract the glottocode from the all.data file:

all.data_mod %>% 
  mutate(is.mod = ifelse(Language_ID %in% correspondences_nb$ID, 1, ifelse(Language_ID_mod %in% correspondences_nb$ID,2,0 ) )) %>%
  mutate(Language_ID_final = ifelse(is.mod == 1, Language_ID, ifelse(is.mod == 2, Language_ID_mod, Language_ID))) %>%
  left_join(correspondences_nb, join_by(Language_ID_final == ID)) ->
  all.data_matched

#pick unique languages
all.data_matched %>%
  distinct(Language_ID_final, .keep_all = T) -> unique_languages_matched

nrow(unique_languages_matched)
#4843 languages have been glossed

table(unique_languages_matched$Glosser, useNA = "always")
# EAT   EK   KM   NK   RB 
# 821   52  737  956 2277 

#of these, how many are not linked:
unique_languages_matched %>%
  filter(is.na(Glottocode)) -> unique_languages_glossed_no_match

table(unique_languages_glossed_no_match$Glosser)
# EAT   RB 
# 7  127 




#which glottocodes are missing:



correspondences_nb %>%
  distinct(Glottocode) -> glottocodes.to.gloss

langs.to.code

glottocodes.to.gloss %>%
  filter(!(Glottocode %in% unique_languages_matched$Glottocode)) -> glottocodes.yet.to.gloss


langs.to.code %>% 
      filter(Glottocode %in% glottocodes.yet.to.gloss$Glottocode) -> languages.not.glossed




      table(languages.not.glossed$Macroarea)
      -sort(-table(languages.not.glossed$Family))

      
langs.full %>%
  filter(Family == "Afro-Asiatic") -> langs.aa


sort(table(langs.aa$Glottocode))

langs.aa %>%
  group_by(Glottocode) %>%
  filter(n()>1) -> aa.duplicates

# 
# ########################
# 
# read.csv("../cldf/languages.csv") -> langs.full
# 
# langs.full %>% 
#   dplyr::select(c(Glottocode,ID)) -> unique_languages
# 
# all.data %>%
#   mutate(Language_ID_mod = paste0("numerals-",Language_ID)) -> all.data_mod
#   
# unique_languages %>%
#   #  filter(Macroarea == "Africa") %>%
#   mutate(is.glossed =( (ID %in% all.data_mod$Language_ID) | (ID %in% all.data_mod$Language_ID_mod) ) ) %>%
#   arrange(desc(is.glossed))-> unique_languages.is.glossed
# 
# #pick the glossed one for each glottocode:
# unique_languages.is.glossed %>%
#   group_by(Glottocode) %>%
#   top_n(1) %>%
#   ungroup() -> this
# 
# -sort(-table(this$Glottocode))
# 
# 
# table(this$is.glossed) #there are repetitions
# # FALSE  TRUE 
# # 672  4609 
# #while the total number of glossed doculects is:
# all.data %>%
#   distinct(Language_ID) %>%
#   nrow()
# #4788
# this %>%
#   filter(is.glossed == F) %>%
#   distinct(Glottocode) -> missing.glottocodes
# 
# 
# langs.full %>%
#   filter(Glottocode %in% missing.glottocodes$Glottocode) %>%
#   distinct(Glottocode,.keep_all = T) -> unique_languages.not.glossed
# write.csv(unique_languages.is.glossed,"glossed/languages.glossed.and.not.glossed.csv")
# write.csv(unique_languages.not.glossed,"glossed/languages.not.glossed.csv")
# 
# table(unique_languages.not.glossed$Macroarea) %>% View()
# -sort(-table(unique_languages.not.glossed$Family)) %>% View()
# 
# 
# unique_languages.is.glossed %>%
#   left_join(all.data, join_by(ID==Language_ID)) %>%
#   distinct(ID, .keep_all = T) -> that
# table(that$Glosser)
# table(that$Glosser) %>% sum()
# 

langs.full %>%
  filter(Family %in% c("South Omotic", "Ta-Ne-Omotic")) -> langs.omotic

unique_languages_matched %>%
  inner_join(langs.omotic, join_by(Language_ID_final == ID)) -> this

unique_languages_matched %>%
  inner_join(langs.omotic, join_by(Language_ID_final == ID)) -> this

this %>%
  select(Glottocode.x) -> that


unique_languages_matched %>%
  right_join(langs.omotic, join_by(Language_ID_final == ID)) -> this

this %>% select(Glottocode.y, Glottocode.x) 


