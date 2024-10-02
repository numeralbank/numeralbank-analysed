#load libraries----
library(groundhog)
my.date <- "2024-08-01"
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
          "reshape2")
groundhog.library(pkgs,my.date)

options(tidyverse.quiet = TRUE)

#set working directory and load data----
#setwd(getSrcDirectory()[1]) # run this line if using plain R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #run this line if using RStudio
setwd('..')
setwd('..')

#load data
forms <- read.csv("cldf/forms.csv")
all_languages <- read.csv("cldf/languages.csv", na.strings=c("","NA"))
unique_languages <- read.csv("etc/for_glossing/languages_ac84f3f2c4e79b2225f31db1426dcdd3de3edf4a.csv", na.strings=c("","NA"))
parameters <- read.csv("cldf/parameters.csv")


forms %>%
  distinct(Language_ID) %>%
  nrow()
#8483 doculects

#filter unique glottocodes:

forms %>%
  filter(Language_ID %in% unique_languages$ID) -> forms_clean

forms_clean %>%
  distinct(Language_ID) %>%
  nrow()
#5087 doculects (they should be 5093, it seems there is missing data in forms.csv)

#The doculects with missing data for forms are:

unique_languages %>%
  filter(!(ID %in% forms_clean$Language_ID)) %>%
  dplyr::select(ID)


# ID
# 1                  sand-Ao
# 2         googleuninum-boy
# 3 barlowpacific-east2466-1
# 4         googleuninum-kok
# 5               sand-Lotha
# 6         googleuninum-wrz

#is macroarea correctly marked? - 21 missing cases
table(unique_languages$Macroarea,useNA = "ifany")

unique_languages %>%
  dplyr::filter(is.na(Macroarea)) -> no.macroarea

gc_africa <- c("mala1537","nucl1736","swah1254")
gc_papunesia <- c("aisi1234","anga1314","east2466","geel1240","kama1374","lowl1259","nucl1806")
gc_eurasia <- c("azer1255","nucl1720","raja1256","macr1269","sard1257","xxxx0054","xxxx0055")
gc_sa <- c("quec1387","span1267")
#gc_na <- c()
gc_australia <- c("pitt1246","yari1238")

unique_languages %>%
  mutate(Macroarea = ifelse(Glottocode %in% gc_africa, "Africa",
                            ifelse(Glottocode %in% gc_papunesia, "Papunesia",
                                   ifelse(Glottocode %in% gc_eurasia, "Eurasia",
                                          ifelse(Glottocode %in% gc_sa, "South America",
                                                 ifelse(Glottocode %in% gc_australia, "Australia",
                                                        Macroarea)))))) -> unique_languages

table(unique_languages$Macroarea,useNA = "ifany")


#rearrange and add columns
forms_clean %>%
  dplyr::select(ID,Language_ID, Source, Parameter_ID, NumberValue,Value,Form, Comment, Loan) %>%
  dplyr::rename(Comment_contributor = Comment) %>%
  mutate(Gloss = NA,
         Comment_glosser = NA,
         Alternate_gloss = NA) -> forms_final

#export files
if (!dir.exists("etc/for_glossing/Africa")) { dir.create("etc/for_glossing/Africa") }
if (!dir.exists("etc/for_glossing/Papunesia")) { dir.create("etc/for_glossing/Papunesia") }
if (!dir.exists("etc/for_glossing/Eurasia")) { dir.create("etc/for_glossing/Eurasia") }
if (!dir.exists("etc/for_glossing/South America")) { dir.create("etc/for_glossing/South America") }
if (!dir.exists("etc/for_glossing/North America")) { dir.create("etc/for_glossing/North America") }
if (!dir.exists("etc/for_glossing/Australia")) { dir.create("etc/for_glossing/Australia") }



my.languages <- unique_languages$ID
my.macroareas <- unique(unique_languages$Macroarea)
# 
# languagesList <- as.list(my.languages)
# 
# names(languagesList) = my.languages

for(area in my.macroareas){
  unique_languages %>%
    dplyr::filter(Macroarea == area)-> languages.in.area
  for(lang in languages.in.area$ID){
        forms_final %>%
      dplyr::filter(Language_ID == lang) -> this.dataset
    write.csv(this.dataset, paste0("etc/for_glossing/",area,"/",lang,".csv"))
  }
}



  