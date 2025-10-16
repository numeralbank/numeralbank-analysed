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

setwd("..")


#Ezequiel----


setwd("./Ezequiel")
temp.data <- NULL
temp = list.files( pattern="\\.csv$")
myfiles = lapply(temp, read.csv,row.names = 1)
length(myfiles)
#52 files

#Merge all datasets
do.call(rbind.data.frame, myfiles) -> temp.data

#Add glosser name
temp.data$Glosser <- "EK"

#basic checks
colnames(temp.data)

#unique numeral ID's:
temp.data %>%
  group_by(ID) %>%
  filter(n()>1)

#all Language_ID's present
-sort(-table(temp.data$Language_ID, useNA = "ifany"))

sum(is.na(temp.data$Language_ID))

#all Sources present
-sort(-table(temp.data$Source, useNA = "ifany"))

rbind(all.data,temp.data) -> all.data

setwd("..")


#Natalie----


setwd("./Natalie")
temp.data <- NULL
temp = list.files( pattern="\\.csv$")
myfiles = lapply(temp, read.csv)
# myfiles = lapply(temp, read.csv,row.names = 1)
length(myfiles)
#1158 files

myfiles %>%
  lapply(colnames) %>% 
  cbind.data.frame() %>% 
  t() %>%
  as.data.frame()-> all.columns

for(i in 1:ncol(all.columns)){
  print(table(all.columns[,i])) }

for(i in 1:length(myfiles)){
  myfiles[[i]]$X <- NULL
  myfiles[[i]]$Column1 <- NULL
  }


#Merge all datasets
do.call(rbind.data.frame, myfiles) -> temp.data

#Add glosser name
temp.data$Glosser <- "NK"

#basic checks
colnames(temp.data)

#unique numeral ID's:
temp.data %>%
  group_by(ID) %>%
  filter(n()>1) 

#TEMPO: kill superflous rows:

temp.data %>%
  filter(!(is.na(NumberValue))) -> temp.data

#all Language_ID's present
-sort(-table(temp.data$Language_ID, useNA = "ifany"))
sum(is.na(temp.data$Language_ID))

#all Sources present
-sort(-table(temp.data$Source, useNA = "ifany"))

rbind(all.data,temp.data) -> all.data
setwd("..")




#Mamta----


setwd("./Mamta")
temp.data <- NULL
temp = list.files( pattern="\\.csv$",
                   full.names = T)
myfiles.0 = lapply(temp, read.csv,row.names = 1)
names(myfiles.0) <- temp
length(myfiles.0)
#117 files from SAND


####

setwd("./Austroasiatic/")
temp.data <- NULL
temp.1 = list.files( pattern="\\.csv$")
myfiles.1 = lapply(temp.1, read.csv,row.names = 1)
names(myfiles.1) <- temp.1
length(myfiles.1)
#122 files for Austroasiatic
setwd("..")

setwd("./Dravidian/")
temp.2 = list.files( pattern="\\.csv$")
myfiles.2 = lapply(temp.2, read.csv,row.names = 1)
names(myfiles.2) <- temp.2
length(myfiles.2)
#39 files for Dravidian
setwd("..")

setwd("./Indo-Aryan/")
temp.3 = list.files( pattern="\\.csv$")
myfiles.3 = lapply(temp.3, read.csv,row.names = 1)
names(myfiles.3) <- temp.3
length(myfiles.3)
#113 files for Indo-Aryan
setwd("..")


setwd("./Sino-Tibetan/")
temp.4 = list.files( pattern="\\.csv$")
myfiles.4 = lapply(temp.4, read.csv) #issue with row.names
names(myfiles.4) <- temp.4
for(i in 1:length(myfiles.4)){
  myfiles.4[[i]]$X <- NULL
}
length(myfiles.4)
#276 files for Sino-Tibetan
setwd("..")

setwd("./Tai-Kadai/")
temp.5 = list.files( pattern="\\.csv$")
myfiles.5 = lapply(temp.5, read.csv,row.names = 1)
names(myfiles.5) <- temp.5
length(myfiles.5)
#54 files for Tai-Kadai
setwd("..")

setwd("./Ta-Ne Omotic/")
temp.6 = list.files( pattern="\\.csv$")
myfiles.6 = lapply(temp.6, read.csv,row.names = 1)
names(myfiles.6) <- temp.6
length(myfiles.6)
#16 files for Ta-Ne Omotic
setwd("..")

####

#Merge all datasets
myfiles <- c(myfiles.0,
             myfiles.1,
             myfiles.2,
             myfiles.3,
             myfiles.4,
             myfiles.5,
             myfiles.6)

####

#Merge all datasets
##do.call(rbind.data.frame, myfiles) -> temp.data
## Error in match.names(clabs, names(xi)) : 
##   names do not match previous names


myfiles %>%
  lapply(ncol) %>% 
  cbind.data.frame() %>%  
  t() -> all.ncols
table(all.ncols)

# all.ncols
# 12 
# 737  



myfiles %>%
  lapply(colnames) %>% 
  cbind.data.frame() %>% 
  t() %>%
  as.data.frame()-> all.columns

for(i in 1:ncol(all.columns)){
  print(table(all.columns[,i])) }

#there is a problem with the Gloss column in one language (./sand-Phom.csv)

colnames(myfiles[["./sand-Phom.csv"]])[10] <- "Gloss"

do.call(rbind.data.frame, myfiles) -> temp.data


#Add glosser name
temp.data$Glosser <- "KM"

#basic checks
colnames(temp.data)

#unique numeral ID's:
temp.data %>%
  group_by(ID) %>%
  filter(n()>1)

#TEMPO: Kill inconsistent rows:

temp.data %>%
  filter(!(is.na(NumberValue))) -> temp.data



#all Language_ID's present
-sort(-table(temp.data$Language_ID, useNA = "ifany"))
sum(is.na(temp.data$Language_ID))

#all Sources present
-sort(-table(temp.data$Source, useNA = "ifany"))

rbind(all.data,temp.data) -> all.data
setwd("..")

#Russell----


setwd("./Russell")
temp.data <- NULL
temp.1 = list.files( pattern="\\.csv$")
myfiles.1 = lapply(temp.1, read.csv)
names(myfiles.1) <- temp.1
length(myfiles.1)
#1001 files in the first folder
setwd("..")

setwd("./Russell_2/")
temp.2 = list.files( pattern="\\.csv$")
myfiles.2 = lapply(temp.2, read.csv)
names(myfiles.2) <- temp.2
length(myfiles.2)
#356 files in the second folder
setwd("..")

setwd("./Russell_3/")
temp.3 = list.files( pattern="\\.csv$")
myfiles.3 = lapply(temp.3, read.csv)
names(myfiles.3) <- temp.3
length(myfiles.3)
#714 files in the third folder
setwd("..")


setwd("./Russell_4/")
temp.4 = list.files( pattern="\\.csv$")
myfiles.4 = lapply(temp.4, read.csv)
names(myfiles.4) <- temp.4
length(myfiles.4)
#140 files in the third folder
setwd("..")


setwd("./Russell_4/Africa")
temp.5 = list.files( pattern="\\.csv$")
myfiles.5 = lapply(temp.5, read.csv)
names(myfiles.5) <- temp.5
length(myfiles.5)
#67 files in the third folder
setwd("..")
setwd("..")

####

#Merge all datasets
myfiles <- c(myfiles.1,
             myfiles.2,
             myfiles.3,
             myfiles.4, 
             myfiles.5)

##do.call(rbind.data.frame, myfiles) -> temp.data
## Error in (function (..., deparse.level = 1, make.row.names = TRUE, stringsAsFactors = FALSE,  : 
##                       numbers of columns of arguments do not match



myfiles %>%
  lapply(ncol) %>% 
  cbind.data.frame() %>%  
  t() -> all.ncols
table(all.ncols)
# all.ncols
# 13    14    15    22 16384 
# 2267     8     1     1     1 
all.ncols %>%
  as.data.frame() %>%
  filter(V1 > 13) 

# AN0106_numerals-romb1245-1.csv                   14
# AN0310_EDIT_abvd-1209-lawa1257_EDIT.csv          14
# AN0401_barlowpacific-sibu1258-1.csv              14
# AN0496_barlowpacific-baub1235-1.csv              14
# AN0504_barlowpacific-rara1235-1.csv              14
# AN0527_barlowpacific-lamp1243-1.csv           16384
# AN0709_EDIT_barlowpacific-cham1312-1_EDIT.csv    22
# PAP436_barlowpacific-mass1263-1.csv              14
# PAP579_numerals-hewa1241-1.csv                   14
# PAP698_barlowpacific-edol1239-3.csv              14
# AN0659_numerals-naka1263-1.csv                   14

# AN0834_numerals-kiss1246-1.csv                   14
# AN0988_numerals-nali1244-1.csv                   14
# AN1135_numerals-litz1237-1.csv                   14
# AN1212_numerals-bwat1240-1.csv                   15
# we have 15 issues now





colnames(myfiles[["AN0527_barlowpacific-lamp1243-1.csv"]])
myfiles[["AN0527_barlowpacific-lamp1243-1.csv"]] <- myfiles[["AN0527_barlowpacific-lamp1243-1.csv"]][1:25,1:13]


colnames(myfiles[["AN0709_EDIT_barlowpacific-cham1312-1_EDIT.csv"]])
myfiles[["AN0709_EDIT_barlowpacific-cham1312-1_EDIT.csv"]] <- myfiles[["AN0709_EDIT_barlowpacific-cham1312-1_EDIT.csv"]][,1:13]



#The ones with 14/15 columns:
colnames(myfiles[["AN0106_numerals-romb1245-1.csv"]])
colnames(myfiles[["AN0310_EDIT_abvd-1209-lawa1257_EDIT.csv"]])
colnames(myfiles[["AN0401_barlowpacific-sibu1258-1.csv"]])
colnames(myfiles[["AN0496_barlowpacific-baub1235-1.csv"]])
colnames(myfiles[["AN0504_barlowpacific-rara1235-1.csv"]])
colnames(myfiles[["PAP436_barlowpacific-mass1263-1.csv"]]) ## first 1 extra col
colnames(myfiles[["PAP579_numerals-hewa1241-1.csv"]])
colnames(myfiles[["PAP698_barlowpacific-edol1239-3.csv"]])
colnames(myfiles[["AN0659_numerals-naka1263-1.csv"]])
colnames(myfiles[["AN0834_numerals-kiss1246-1.csv"]])
colnames(myfiles[["AN0988_numerals-nali1244-1.csv"]])
colnames(myfiles[["AN1135_numerals-litz1237-1.csv"]])
colnames(myfiles[["AN1212_numerals-bwat1240-1.csv"]]) #last 2 extra cols




#Other than "PAP436_barlowpacific-mass1263-1.csv", they all have a spurious 14th (or 14th and 155h) column. We manually change it for that case:

myfiles[["PAP436_barlowpacific-mass1263-1.csv"]] <- myfiles[["PAP436_barlowpacific-mass1263-1.csv"]][,2:14]

#And we automatically filter cols 2:13 for all (col 14 is spureus in these cases, and col 1 is spureus for all datasets)


for(i in 1:length(myfiles)){
  myfiles[[i]] <- myfiles[[i]][,2:13]
}

#check column names

myfiles %>%
  lapply(colnames) %>% 
  cbind.data.frame() %>% 
  t() %>%
  as.data.frame()-> all.columns

for(i in 1:ncol(all.columns)){
  print(table(all.columns[,i])) }

#it all works!

do.call(rbind.data.frame, myfiles) -> temp.data




#Add glosser name
temp.data$Glosser <- "RB"

#basic checks
colnames(temp.data)

#kill empty rows
temp.data %>%
  filter(ID!= "") -> temp.data

#unique numeral ID's:
temp.data %>%
  group_by(ID) %>%
  filter(n()>1) 

#fix duplicaded name

temp.data %>%
  mutate(ID = ifelse(ID == "numerals-koal1240-2-thirty-1" & Gloss == "20[+]10", "numerals-koal1240-2-thirty-2",ID)) -> temp.data

#no duplicates!



#all Language_ID's present
-sort(-table(temp.data$Language_ID, useNA = "ifany"))
sum(is.na(temp.data$Language_ID))

#all Sources present
-sort(-table(temp.data$Source, useNA = "ifany"))

rbind(all.data,temp.data) -> all.data


colnames(all.data)


#Enock----

# setwd("./Enock/all_new/")
# temp.data <- NULL
# temp = list.files( pattern="\\.csv$")
# myfiles = lapply(temp, read.csv,row.names = 1)
# length(myfiles)

##############
read_csv_auto <- function(file) {
  first_line <- readLines(file, n = 1)
  if (grepl(";", first_line)) {
    read.csv(file, sep = ";", stringsAsFactors = FALSE, row.names = NULL, quote = "")
  } else {
    read.csv(file, sep = ",", stringsAsFactors = FALSE, row.names = NULL, quote = "")
  }
}

setwd("./Enock/all_new/")
temp.data.I <- NULL
temp.I = list.files( pattern="\\.csv$")


myfiles.I = lapply(temp.I, read_csv_auto)
names(myfiles.I) <- temp.I

# myfiles.I = lapply(temp.I, read.csv2,quote = "", 
#                    row.names = NULL, 
#                    stringsAsFactors = FALSE)

length(myfiles.I)
#934 files
setwd("..")
setwd("..")

setwd("./Enock/all_new/")
temp.data.II <- NULL
temp.II = list.files( pattern="\\.tsv$")

myfiles.II = lapply(temp.II, read_delim,quote = "")
names(myfiles.II) <- temp.II

length(myfiles.II)
#2 files
setwd("..")
setwd("..")


myfiles.I %>%
  lapply(ncol) %>% 
  cbind.data.frame() %>%  
  t() %>%
  as.data.frame() %>%
  rownames_to_column("Language_ID")-> all.ncols

table(all.ncols$V1)
# 8       9  12  13  14 
# 175     1 540 215   3 
# 


myfiles.II %>%
  lapply(ncol) %>% 
  cbind.data.frame() %>%  
  t() -> all.ncols

table(all.ncols)
# all.ncols
# 13 
# 2
myfiles.II <- purrr::map(myfiles.II, as.data.frame)


files <- c(myfiles.I, myfiles.II)
#936 files

#check column names

# # Ensure the list is named
# if (is.null(names(files))) names(files) <- paste0("df", seq_along(files))
# 
# # Sanitize column names in each df: replace "" with ".empty_1", ".empty_2", ... and ensure uniqueness
# clean_files <- imap(files, ~{
#   nm <- names(.x) %>% replace_na("") %>% trimws()
#   if (any(!nzchar(nm))) nm[!nzchar(nm)] <- paste0(".empty_", seq_len(sum(!nzchar(nm))))
#   nm <- make.unique(nm)              # handle duplicates within a df
#   names(.x) <- nm
#   .x
# })
# 
# # Build presence/absence matrix
# col_summary <- clean_files %>%
#   imap(~ tibble(file = .y, column = unique(names(.x)))) %>%  # unique() avoids dup counts
#   list_rbind() %>%
#   mutate(present = 1L) %>%
#   pivot_wider(names_from = column, values_from = present, values_fill = 0L)
# 
# col_summary
# 
# col_summary %>%
#   summarise(across(where(is.numeric), sum, na.rm = TRUE)) -> col_summary_sums
# 
# ncol(col_summary)
# # 35 columns
# 
# #fix initial "X." and final "."
# 
# for(i in 1:length(clean_files)){
#   colnames(clean_files[[i]]) <- sub("^X\\.", "", colnames(clean_files[[i]]))
#   colnames(clean_files[[i]]) <- sub("\\.$", "", colnames(clean_files[[i]]))
# }
# 
# # Build presence/absence matrix
# col_summary <- clean_files %>%
#   imap(~ tibble(file = .y, column = unique(names(.x)))) %>%  # unique() avoids dup counts
#   list_rbind() %>%
#   mutate(present = 1L) %>%
#   pivot_wider(names_from = column, values_from = present, values_fill = 0L)
# 
# col_summary
# ncol(col_summary)
# 
# # we reduced the ncol(col_summary) from 35 to 20
# #stats:
# col_summary %>%
#   summarise(across(where(is.numeric), sum, na.rm = TRUE)) -> col_summary_sums

#merge considering missing columns:

# cleanup:
clean_files <- purrr::map(files, ~{
  nm <- names(.x)
  nm[!nzchar(nm)] <- paste0(".empty_", seq_len(sum(!nzchar(nm))))  # fix empty names
  names(.x) <- make.unique(nm)
  dplyr::mutate(.x, dplyr::across(where(is.factor), as.character))
})

#convert all to character to avoid mismatches
files_char <- purrr::map(clean_files, ~ mutate(.x, across(everything(), as.character)))


merged <- dplyr::bind_rows(files_char, .id = "source")



colnames(all.data)
colnames(merged)

#extra columns
colnames(merged)[!(colnames(merged) %in% colnames(all.data))]

ncol(merged)
#35 columns


#fix initial "X." and final "."

for(i in 1:length(files_char)){
  colnames(files_char[[i]]) <- sub("^X\\.", "", colnames(files_char[[i]]))
  colnames(files_char[[i]]) <- sub("\\.$", "", colnames(files_char[[i]]))
}
  
   

merged <- dplyr::bind_rows(files_char, .id = "source")

#Add glosser name
merged$Glosser <- "EAT"


colnames(all.data)
colnames(merged)
ncol(merged)
#reduced from 36 to 21 columns. Still 8 too much (there are 13 including Glosser)

#extra columns
colnames(merged)[!(colnames(merged) %in% colnames(all.data))]

#remove unused columns:
merged %>%
  dplyr::select(-c(glottocode,Glottocode)) -> tmp


#explore unclear rows:
## source stands for filename, not Source. I remove it:
tmp %>%
  dplyr::select(-source) -> tmp

## row.names, X, ...1: superflouous columns
tmp %>%
  dplyr::select(-c(row.names,X,...1)) -> tmp


#merge obvious columns

## Alternate_Gloss / Alternate_gloss

conflicts <- tmp %>%
  filter(!is.na(Alternate_Gloss), Alternate_Gloss != "", !is.na(Alternate_gloss), Alternate_gloss != "")
if (nrow(conflicts) > 0) {
  warning(paste("There are", nrow(conflicts), "rows where both columns are filled."))
}


tmp <- tmp %>%
  mutate(
    # New unified column
    Alternate_Gloss_ok = case_when(
      !is.na(Alternate_Gloss) & Alternate_Gloss != "" & (is.na(Alternate_gloss) | Alternate_gloss == "") ~ Alternate_Gloss,
      (is.na(Alternate_Gloss) | Alternate_Gloss == "") & !is.na(Alternate_gloss) & Alternate_gloss != "" ~ Alternate_gloss,
      (is.na(Alternate_Gloss) | Alternate_Gloss == "") & (is.na(Alternate_gloss) | Alternate_gloss == "") ~ NA_character_,
      # if both have values (conflict)
      TRUE ~ {
        warning("Some rows have both 'Alternate_Gloss' and 'Alternate_gloss' filled.")
        Alternate_Gloss  
      }
    )
  ) %>%
  select(-Alternate_Gloss, -Alternate_gloss)  # remove originals


## Comment_Glosser / Comment_glosser

conflicts <- tmp %>%
  filter(!is.na(Comment_Glosser), Comment_Glosser != "", !is.na(Comment_glosser), Comment_glosser != "")
if (nrow(conflicts) > 0) {
  warning(paste("There are", nrow(conflicts), "rows where both columns are filled."))
}


tmp <- tmp %>%
  mutate(
    # New unified column
    Comment_Glosser_ok = case_when(
      !is.na(Comment_Glosser) & Comment_Glosser != "" & (is.na(Comment_glosser) | Comment_glosser == "") ~ Comment_Glosser,
      (is.na(Comment_Glosser) | Comment_Glosser == "") & !is.na(Comment_glosser) & Comment_glosser != "" ~ Comment_glosser,
      (is.na(Comment_Glosser) | Comment_Glosser == "") & (is.na(Comment_glosser) | Comment_glosser == "") ~ NA_character_,
      # if both have values (conflict)
      TRUE ~ {
        warning("Some rows have both 'Comment_Glosser' and 'Comment_glosser' filled.")
        Comment_Glosser  
      }
    )
  ) %>%
  select(-Comment_Glosser, -Comment_glosser)  # remove originals


tmp <- tmp %>%
  rename(
    Alternate_gloss = Alternate_Gloss_ok,
    Comment_glosser = Comment_Glosser_ok
  )

tmp -> temp.data
#########




#basic checks
colnames(temp.data)

#unique numeral ID's:
temp.data %>%
  group_by(ID) %>%
  filter(n()>1) -> duplicates.enock


#14,043 cases! Most are genuine duplications. Some are mixed up columns
write.csv(duplicates.enock,"duplicates.enock.csv")


#kill duplicate rows

temp.data %>%
  distinct() -> temp.data

#try again unique numeral ID's:
temp.data %>%
  group_by(ID) %>%
  filter(n()>1)  -> duplicates.enock.not.fully
write.csv(duplicates.enock.not.fully,"duplicates.enock.not.fully.csv")

#I pick only one per ID (this should not be done when the data is corrected):
temp.data %>%
  distinct(ID, .keep_all = T) -> temp.data

#all Language_ID's present
-sort(-table(temp.data$Language_ID, useNA = "ifany"))
sum(is.na(temp.data$Language_ID))
#381 cases of Language_ID = NA
temp.data %>%
  filter(is.na(temp.data$Language_ID))
sum(temp.data$Language_ID=="") -> Language_ID.na

write.csv(Language_ID.na,"Language_ID.na.csv")

temp.data %>%
  filter(Language_ID != "") -> temp.data


temp.data %>%
  filter(!is.na(Language_ID)) -> temp.data

#all Sources present
-sort(-table(temp.data$Source, useNA = "ifany"))

#keep only known sources or NA
temp.data <- temp.data %>%
  mutate(Source = str_replace_all(Source, '^"|"$', ''))

temp.data %>%
  filter(!(Source == "Chan2019" | is.na(Source))) -> wrong.source

write.csv(wrong.source, "wrong.source")
temp.data %>%
  filter(Source == "Chan2019" | is.na(Source)) -> temp.data

colnames(all.data)
rbind(all.data,temp.data) -> all.data




#organize data-----


# -sort(-table(all.data$ID))

all.data %>%
  group_by(ID) %>%
  filter(n()>1) -> all.duplicates

write.csv(all.duplicates,"all.duplicates.csv")

table(all.duplicates$Language_ID, all.duplicates$Glosser)


#TEMPO: some languages were coded twice. Keep only one:

all.data %>%
  filter(!(Language_ID == "numerals-fiji1242-1" & Glosser == "KM")) %>%
  filter(!(Language_ID == "numerals-furr1244-1" & Glosser == "NK")) %>%
  filter(!(Language_ID == "numerals-katl1237-1" & Glosser == "NK")) %>%
  filter(!(Language_ID == "numerals-nuuu1241-1" & Glosser == "NK")) %>%
  filter(!(Language_ID == "numerals-tega1236-1" & Glosser == "NK")) %>%
  filter(!(Language_ID == "numerals-dime1235-1" & Glosser == "EAT")) %>%
  filter(!(Language_ID == "numerals-gaam1241-1" & Glosser == "EAT")) %>%
  filter(!(Language_ID == "numerals-gaww1239-1" & Glosser == "EAT")) %>%
  filter(!(Language_ID == "numerals-kale1246-1" & Glosser == "EAT")) %>%
  filter(!(Language_ID == "numerals-mang1394-1" & Glosser == "EAT")) %>%
  filter(!(Language_ID == "numerals-nobi1240-1" & Glosser == "EAT"))-> all.data
  
#no duplicates!




write.csv(all.data,"all.data.csv")

