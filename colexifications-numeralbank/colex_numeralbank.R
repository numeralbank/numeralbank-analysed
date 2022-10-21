#load libraries and set working directory----
library(tidyverse)


#setwd(getSrcDirectory()[1]) #if using R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #if using RStudio
setwd('..')


#load data----
forms <- read.csv("cldf/forms.csv")
languages <- read.csv("cldf/languages.csv", na.strings=c("","NA"))
parameters <- read.csv("cldf/parameters.csv")
#data formatting----


#colexifications

colex.all <- read_tsv("colexifications-numeralbank/colex2lang_data.tsv")

colex.all %>%
  filter(CONCEPT_A == "ONE [1493]" | CONCEPT_A == "TWO [1498]" | CONCEPT_A == "THREE [492]" | CONCEPT_A == "FOUR [1500]" 
         | CONCEPT_A == "FIVE [493]" | CONCEPT_A == "SIX [1703]" | CONCEPT_A == "SEVEN [1704]" | CONCEPT_A == "EIGHT [1705]" 
         | CONCEPT_A == "NINE [1483]" | CONCEPT_A == "TEN [1515]" | CONCEPT_A == "TWENTY [1710]" | CONCEPT_A == "THIRTY [1715]"
         | CONCEPT_A == "FORTY [1716]" | CONCEPT_A == "FIFTY [1717]" | CONCEPT_A == "SIXTY [1718]"| CONCEPT_A == "SEVENTY [1721]" 
         | CONCEPT_A == "EIGHTY [1722]" | CONCEPT_A == "NINETY [1724]" | CONCEPT_A == "HUNDRED [1634]"
         | CONCEPT_B == "ONE [1493]" | CONCEPT_B == "TWO [1498]" | CONCEPT_B == "THREE [492]" | CONCEPT_B == "FOUR [1500]" 
         | CONCEPT_B == "FIVE [493]" | CONCEPT_B == "SIX [1703]" | CONCEPT_B == "SEVEN [1704]" | CONCEPT_B == "EIGHT [1705]" 
         | CONCEPT_B == "NINE [1483]" | CONCEPT_B == "TEN [1515]" | CONCEPT_B == "TWENTY [1710]" | CONCEPT_B == "THIRTY [1715]"
         | CONCEPT_B == "FORTY [1716]" | CONCEPT_B == "FIFTY [1717]" | CONCEPT_B == "SIXTY [1718]"| CONCEPT_B == "SEVENTY [1721]" 
         | CONCEPT_B == "EIGHTY [1722]" | CONCEPT_B == "NINETY [1724]" | CONCEPT_B == "HUNDRED [1634]") -> colex.num

  
table(colex.num$NUM_LANGUAGES)









###
#Twenty and something

colex.all %>%
  filter(CONCEPT_A == "TWENTY [1710]" | CONCEPT_B == "TWENTY [1710]") -> twenties

this <- unlist(strsplit(twenties$LANGUAGES, split="\\[|;"))
that <- grep("\\]",this)
colex <- as.vector(str_remove(this[that],"\\]"))

sum(languages$Glottocode %in% colex)

languages %>%
  filter(Glottocode %in% colex) -> languages.colex

#we calculate what proportion of the languages which colexify TWENTY and something else have a vigesimal base:
vigesimal.colex <- table(languages.colex$BestBase)["vigesimal"] 
known.colex <- sum(table(languages.colex$BestBase)) - table(languages.colex$BestBase)["unknown"]  

prop.colex <- vigesimal.colex/known.colex


#we calculate what proportion of all languages have a vigesimal base:
vigesimal.total <- table(languages$BestBase)["vigesimal"] 
known.total <- sum(table(languages$BestBase)) - table(languages$BestBase)["unknown"]  

prop.total <- vigesimal.total/known.total


prop.colex/prop.total # it's 85% more likely to have a vigesimal database if the language colexifies TWENTY and something else


#Five and hand----

colex.all %>%
  filter(CONCEPT_A == "FIVE [493]" & CONCEPT_B == "HAND [1277]") %>%
  select(LANGUAGES) %>%
  unlist()-> fivehand

this <- unlist(strsplit(fivehand, split="\\[|;"))
that <- grep("\\]",this)
colex <- as.vector(str_remove(this[that],"\\]"))

sum(languages$Glottocode %in% colex)

languages %>%
  filter(Glottocode %in% colex) -> languages.colex
	

#we calculate what proportion of the languages which colexify FIVE and HAND have a quinary base:
quinary.colex <- table(languages.colex$BestBase)["quinary"] 
known.colex <- sum(table(languages.colex$BestBase)) - table(languages.colex$BestBase)["unknown"]  

prop.colex <- quinary.colex/known.colex


#we calculate what proportion of all languages have a quinary base:
quinary.total <- table(languages$BestBase)["quinary"] 
known.total <- sum(table(languages$BestBase)) - table(languages$BestBase)["unknown"]  

prop.total <- quinary.total/known.total


prop.colex/prop.total # it's 60% more likely to have a vigesimal database if the language colexifies TWENTY and something else


#Finger and toe BESTBASE---- IDEA BY Melechenko for languages of the Caucasus, based on Edelman (1999) https://www.degruyter.com/document/doi/10.1515/9783110811193.221/html

colex.all %>%
  filter(CONCEPT_A == "FINGER [1303]" & CONCEPT_B == "TOE [1389]") %>%
  select(LANGUAGES) %>%
  unlist()-> fingertoe

this <- unlist(strsplit(fingertoe, split="\\[|;"))
that <- grep("\\]",this)
colex <- as.vector(str_remove(this[that],"\\]"))

sum(languages$Glottocode %in% colex)

languages %>%
  filter(Glottocode %in% colex) -> languages.colex


#we calculate what proportion of the languages which colexify FINGER and TOE have a vigesimal base:
vigesimal.colex <- table(languages.colex$BestBase)["vigesimal"] 
known.colex <- sum(table(languages.colex$BestBase)) - table(languages.colex$BestBase)["unknown"]  

prop.colex <- vigesimal.colex/known.colex


#we calculate what proportion of all languages have a quinary base:
vigesimal.total <- table(languages$BestBase)["vigesimal"] 
known.total <- sum(table(languages$BestBase)) - table(languages$BestBase)["unknown"]  

prop.total <- vigesimal.total/known.total


prop.colex/prop.total # it's 16% LESS likely to have a vigesimal database if the language colexifies FINGER and TOE, if we considerBestBase (see next chunk)



#Finger and toe BASE---- IDEA BY Melechenko for languages of the Caucasus, based on Edelman (1999) https://www.degruyter.com/document/doi/10.1515/9783110811193.221/html

colex.all %>%
  filter(CONCEPT_A == "FINGER [1303]" & CONCEPT_B == "TOE [1389]") %>%
  select(LANGUAGES) %>%
  unlist()-> fingertoe

this <- unlist(strsplit(fingertoe, split="\\[|;"))
that <- grep("\\]",this)
colex <- as.vector(str_remove(this[that],"\\]"))

sum(languages$Glottocode %in% colex)

languages %>%
  filter(Glottocode %in% colex) -> languages.colex


#we calculate what proportion of the languages which colexify FINGER and TOE have a vigesimal base:
vigesimal.colex <- table(languages.colex$Base)["vigesimal"] 
known.colex <- sum(table(languages.colex$Base)) - table(languages.colex$Base)["unknown"]  

prop.colex <- vigesimal.colex/known.colex


#we calculate what proportion of all languages have a quinary base:
vigesimal.total <- table(languages$Base)["vigesimal"] 
known.total <- sum(table(languages$Base)) - table(languages$Base)["unknown"]  

prop.total <- vigesimal.total/known.total


prop.colex/prop.total # it's 39% MORE likely to have a vigesimal database if the language colexifies FINGER and TOE, if we consider Base (lots of unknown here)



	
