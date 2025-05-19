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
          "english")
groundhog.library(pkgs,my.date)

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
#960 files

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
#1000 files in the first folder
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

####
setwd("./Russell_4/")
temp.4 = list.files( pattern="\\.csv$")
myfiles.4 = lapply(temp.4, read.csv)
names(myfiles.4) <- temp.4
length(myfiles.4)
#140 files in the third folder
setwd("..")

####

#Merge all datasets
myfiles <- c(myfiles.1,
             myfiles.2,
             myfiles.3,
             myfiles.4)

##do.call(rbind.data.frame, myfiles) -> temp.data
## Error in (function (..., deparse.level = 1, make.row.names = TRUE, stringsAsFactors = FALSE,  : 
##                       numbers of columns of arguments do not match



myfiles %>%
  lapply(ncol) %>% 
  cbind.data.frame() %>%  
  t() -> all.ncols
table(all.ncols)
# all.ncols
#N two 14, one 15
# 13    14    15    22 16384 
# 2055    12     1     1     1 
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


# V1
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
## AN0659_numerals-naka1263-1.csv                   14
## AN0739_EDIT_barlowpacific-mung1269-1_EDIT.csv    14
# we had  12 issues 




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

#all works!

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


#no duplicates!



#all Language_ID's present
-sort(-table(temp.data$Language_ID, useNA = "ifany"))
sum(is.na(temp.data$Language_ID))

#all Sources present
-sort(-table(temp.data$Source, useNA = "ifany"))

rbind(all.data,temp.data) -> all.data





#Enock----





setwd("./Enock")
temp.data <- NULL
temp.1 = list.files( pattern="\\.csv$")
#kill duplicated (correct in original data by Enock!)

to.ignore <- which(temp %in% c("numerals-busa1253-1 2.csv", #DUPLICATED
                               "numerals-ajab1235-2.csv", #DUPLICATED
                               "numerals-ajab1235-1 2.csv", #DUPLICATED
                               "numerals-sele1249-1 2.csv", #DUPLICATED
                               "errors_word_numbers.csv", #Wrongly placed
                               "add.csv", #???
                               "Kwa_numerals - Forms_11_24.csv",
                               "Kwa_numerals - Forms_11_24-2.csv"))

if(length(to.ignore)>0){temp.1[-to.ignore] -> temp.1}


myfiles.1 = lapply(temp.1, read.csv2,row.names=1)
#myfiles = lapply(temp, read.csv2)
length(myfiles.1)
#98 files (OLD)
#56 files (NEW)
setwd("..")
do.call(rbind.data.frame, myfiles.1) -> temp.data.1



setwd("./Enock/Kru/")
temp.data.2 <- NULL
temp.2 = list.files( pattern="\\.csv$")

myfiles.2 = lapply(temp.2, read.csv)
#myfiles.3 = lapply(temp.3, read.csv2,row.names=1)
length(myfiles.2)
#26 files
setwd("..")
setwd("..")

myfiles.2 %>%
  lapply(ncol) %>% 
  cbind.data.frame() %>%  
  t() -> all.ncols

table(all.ncols)

# all.ncols
# 13 14 
# 25  1  1 


colnames(myfiles.2[[6]])
myfiles.2[[6]] <- myfiles.2[[6]][,c(1,3:14)]


for(i in 1:length(myfiles.2)){
  print(i)
  print(colnames(myfiles.2[[i]]))
}
colnames(myfiles.2[[18]]) 
myfiles.2[[18]]$X <- 0

do.call(rbind.data.frame, myfiles.2) -> temp.data.2

#temp.data.2 <- temp.data.2[,-1]
temp.data.2$X <- NULL


setwd("./Enock/Bantu/")
# setwd("./Enock/Bantu_retrieved")
temp.data.3 <- NULL
temp.3 = list.files( pattern="\\.csv$")
myfiles.3 = lapply(temp.3, read.csv)
#myfiles = lapply(temp, read.csv2,row.names=1)
length(myfiles.3)
#213 files
setwd("..")
setwd("..")
do.call(rbind.data.frame, myfiles.3) -> temp.data.3

colnames(temp.data.3)
colnames(temp.data.1)

temp.data.3 %>%
  rename(NumberValue = Parameter_ID) %>%
  rename(Alternate_gloss = Alternate_Gloss) %>%
  rename(Comment_glosser = Comment_Glosser)-> temp.data.3

# #devtools::install_github("benmarwick/words2number")
# library(words2number)
# # tmp <- tmp[1:200,]
# library(english)

# temp.data.3 %>%
#   mutate(Parameter_ID =
#            tryCatch({ english(NumberValue)},
#              error = function(e) NA)) %>%
#   filter(is.na(Parameter_ID)) -> this
# 
# temp.data.3 %>%
#   mutate(Parameter_ID =       sapply(NumberValue,
#                                             function(x) tryCatch({ english(x)
#                                               1},
#                                               error = function(e) 0)))  -> this
# 
# this %>%
#   filter(Parameter_ID == 0) -> errors_word_numbers


temp.data.3 %>%
  filter(is.na(NumberValue)) -> errors_word_numbers

write_csv(errors_word_numbers,"errors_word_numbers.csv")

temp.data.3 %>% 
  filter(!is.na(NumberValue)) -> temp.data.3


temp.data.3 %>%
  mutate(Parameter_ID = as.character(english(NumberValue))) -> temp.data.3

# #
# temp.data.3 %>%
#   mutate(Parameter_ID =
#            tryCatch({ english(NumberValue)},
#              error = function(e) NA)) %>%
#   filter(is.na(Parameter_ID)) -> this
# 
# temp.data.3 %>%
#   mutate(Parameter_ID =sapply(NumberValue,
#                                             function(x) tryCatch({ english(x)
#                                               1},
#                                               error = function(e) 0)))  -> this
# 
# this %>%
#   filter(Parameter_ID == 0) -> errors_word_numbers
# #


temp.data.3 %>%
  mutate(Loan = FALSE,
         Comment_contributor = NA,
         Source = NA) -> temp.data.3



setwd("./Enock/Mande/")
temp.data.4 <- NULL
temp.4 = list.files( pattern="\\.csv$")

myfiles.4 = lapply(temp.4, read.csv,quote = "", 
                   row.names = NULL, 
                   stringsAsFactors = FALSE)
#myfiles.3 = lapply(temp.3, read.csv2,row.names=1)
length(myfiles.4)
#50 files
setwd("..")
setwd("..")

myfiles.4 %>%
  lapply(ncol) %>% 
  cbind.data.frame() %>%  
  t() -> all.ncols

table(all.ncols)
# all.ncols
# 12 13 
# 30 20 
# 
# 
# colnames(myfiles.2[[6]])
# myfiles.2[[6]] <- myfiles.2[[6]][,c(1,3:14)]
# 

for(i in 1:length(myfiles.4)){
  print(i)
  print(colnames(myfiles.4[[i]]))
}

for(i in 1:length(myfiles.4)){
    myfiles.4[[i]]$X <- NULL
}


do.call(rbind.data.frame, myfiles.4) -> temp.data.4



setwd("./Enock/Central_Sudanic/")
temp.data.5 <- NULL
temp.5 = list.files( pattern="\\.tsv$")

myfiles.5 = lapply(temp.5, read_delim,quote = "")
#myfiles.3 = lapply(temp.3, read.csv2,row.names=1)
length(myfiles.5)
#1 file
setwd("..")
setwd("..")

myfiles.5 %>%
  lapply(ncol) %>% 
  cbind.data.frame() %>%  
  t() -> all.ncols

table(all.ncols)
# all.ncols
# 12 
# 1
# 
# 
# colnames(myfiles.2[[6]])
# myfiles.2[[6]] <- myfiles.2[[6]][,c(1,3:14)]
# 

for(i in 1:length(myfiles.5)){
  print(i)
  print(colnames(myfiles.5[[i]]))
}

for(i in 1:length(myfiles.5)){
  myfiles.5[[i]]$X <- NULL
}


do.call(rbind.data.frame, myfiles.5) -> temp.data.5




setwd("./Enock/Dogon/")
temp.data.6 <- NULL
temp.6 = list.files( pattern="\\.tsv$")

myfiles.6 = lapply(temp.6, read_delim,quote = "")
#myfiles.3 = lapply(temp.3, read.csv2,row.names=1)
length(myfiles.6)
#13 files
setwd("..")
setwd("..")

myfiles.6 %>%
  lapply(ncol) %>% 
  cbind.data.frame() %>%  
  t() -> all.ncols

table(all.ncols)
# all.ncols
# 12 
# 13
# 
# 
# colnames(myfiles.2[[6]])
# myfiles.2[[6]] <- myfiles.2[[6]][,c(1,3:14)]
# 

for(i in 1:length(myfiles.6)){
  print(i)
  print(colnames(myfiles.6[[i]]))
}

for(i in 1:length(myfiles.6)){
  myfiles.6[[i]]$X <- NULL
}


do.call(rbind.data.frame, myfiles.6) -> temp.data.6



setwd("./Enock/Nilotic/")
temp.data.7 <- NULL
temp.7 = list.files( pattern="\\.tsv$")

myfiles.7 = lapply(temp.7, read_delim,quote = "")
#myfiles.3 = lapply(temp.3, read.csv2,row.names=1)
length(myfiles.7)
#1 file
setwd("..")
setwd("..")

myfiles.7 %>%
  lapply(ncol) %>% 
  cbind.data.frame() %>%  
  t() -> all.ncols

table(all.ncols)
# all.ncols
# 12 
# 1
# 
# 
# colnames(myfiles.2[[6]])
# myfiles.2[[6]] <- myfiles.2[[6]][,c(1,3:14)]
# 

for(i in 1:length(myfiles.7)){
  print(i)
  print(colnames(myfiles.7[[i]]))
}

for(i in 1:length(myfiles.7)){
  myfiles.7[[i]]$X <- NULL
}


do.call(rbind.data.frame, myfiles.7) -> temp.data.7



setwd("./Enock/South_Omotic/")
temp.data.8 <- NULL
temp.8 = list.files( pattern="\\.tsv$")

myfiles.8 = lapply(temp.8, read_delim,quote = "")
#myfiles.3 = lapply(temp.3, read.csv2,row.names=1)
length(myfiles.8)
#1 file
setwd("..")
setwd("..")

myfiles.8 %>%
  lapply(ncol) %>% 
  cbind.data.frame() %>%  
  t() -> all.ncols

table(all.ncols)
# all.ncols
# 12 
# 1
# 
# 
# colnames(myfiles.2[[6]])
# myfiles.2[[6]] <- myfiles.2[[6]][,c(1,3:14)]
# 

for(i in 1:length(myfiles.8)){
  print(i)
  print(colnames(myfiles.8[[i]]))
}

for(i in 1:length(myfiles.8)){
  myfiles.8[[i]]$X <- NULL
}


do.call(rbind.data.frame, myfiles.8) -> temp.data.8



setwd("./Enock") #tsv files in the root 
temp.data.9 <- NULL
temp.9 = list.files( pattern="\\.tsv$")

myfiles.9 = lapply(temp.9, read_delim,quote = "")
#myfiles.3 = lapply(temp.3, read.csv2,row.names=1)
length(myfiles.9)
#3 files
setwd("..")
setwd("..")

myfiles.9 %>%
  lapply(ncol) %>% 
  cbind.data.frame() %>%  
  t() -> all.ncols

table(all.ncols)
# all.ncols
# 12 13 
# 1  2 
# 
# 
# colnames(myfiles.2[[6]])
# myfiles.2[[6]] <- myfiles.2[[6]][,c(1,3:14)]
# 

for(i in 1:length(myfiles.9)){
  print(i)
  print(colnames(myfiles.9[[i]]))
}

for(i in 1:length(myfiles.9)){
  myfiles.9[[i]]$...1<- NULL
}


do.call(rbind.data.frame, myfiles.9) -> temp.data.9




#Merge all datasets
temp.data <- rbind(temp.data.1,
                   temp.data.2,
                   temp.data.3,
                   temp.data.4,
                   temp.data.5,
                   temp.data.6,
                   temp.data.7,
                   temp.data.8,
                   temp.data.9)

###
# #transformation of the Kwa_numerals dataset "Kwa_numerals - Forms_11_24.csv"----
# kwa <- read.csv("Kwa_numerals - Forms_11_24.csv")
# 
# colnames(kwa)
# colnames(all.data)
# 
# kwa %>%
#   dplyr::select(Language, Glottocode,Concept,Form,Gloss,Alternate_gloss,Comment_glosser, Source) %>%
#   rename(Parameter_ID = Concept) -> tmp
# 
# #devtools::install_github("benmarwick/words2number")
# library(words2number)
# # tmp <- tmp[1:200,]
# 
# tryCatch({ eval(parse(text = x)) 
#   1},
#   error = function(e) 0)
# 
# 
# tmp %>%
#   mutate(NumberValue = 
#            tryCatch({ to_number(Parameter_ID)},
#              error = function(e) NA)) %>%
#   filter(is.na(NumberValue)) -> this
# 
# tmp %>%
#   mutate(NumberValue =       sapply(Parameter_ID,
#                                             function(x) tryCatch({ to_number(x) 
#                                               1},
#                                               error = function(e) 0)))  -> this
# 
# this %>%
#   filter(NumberValue == 0) -> errors_word_numbers
# 
# write_csv(errors_word_names,"errors_word_numbers.csv")
# 
#-----
###

#Add glosser name
temp.data$Glosser <- "EAT"

#basic checks
colnames(temp.data)

#unique numeral ID's:
temp.data %>%
  group_by(ID) %>%
  filter(n()>1)

#kill duplicate rows

temp.data %>%
  distinct() -> temp.data

#try again unique numeral ID's:
temp.data %>%
  group_by(ID) %>%
  filter(n()>1)  -> dups

#all Language_ID's present
-sort(-table(temp.data$Language_ID, useNA = "ifany"))
sum(is.na(temp.data$Language_ID))
sum(temp.data$Language_ID=="")

temp.data %>%
  filter(Language_ID != "") -> temp.data


#all Sources present
-sort(-table(temp.data$Source, useNA = "ifany"))


colnames(all.data)
rbind(all.data,temp.data) -> all.data




#organize data-----


# -sort(-table(all.data$ID))

all.data %>%
  group_by(ID) %>%
  filter(n()>1) -> all.duplicates

write.csv(all.duplicates,"glossed/all.duplicates.csv")

#no duplicates!



all.data -> tmp

# 
# #let's kill and merge extra doculects
# 
# -sort(-table(tmp$Glottocode))
# 
# tmp %>%
#   filter(!(Glottocode %in% c("phal1254","ende1246-1","nort2787-1"))) -> tmp
# 
# #merge duplicated doculects
# tmp %>%
#   mutate(Glottocode = str_trim(Glottocode)) -> tmp
# 
# -sort(-table(tmp$Glottocode))
# 
# 
# all.data <- tmp

write.csv(all.data,"glossed/all.data.csv")


