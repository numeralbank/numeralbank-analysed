#load libraries----
library(groundhog)
my.date <- "2022-10-16"
pkgs <- c("tidyverse","ggplot2","reshape")
groundhog.library(pkgs,my.date)



#set working directory----
#setwd(getSrcDirectory()[1]) # run this line if using R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #run this line if using RStudio
setwd('..')

#load data----
forms <- read.csv("cldf/forms.csv")
languages <- read.csv("cldf/languages.csv", na.strings=c("","NA"))
parameters <- read.csv("cldf/parameters.csv")

#convert categorical variables to factor
new_factors <- c("Glottocode","Glottolog_Name","ISO639P3code","Macroarea","Family","BestBase","Base")
languages[,new_factors] <- lapply(languages[,new_factors] ,factor)


#String manipulation --this can be improved with lingpy----

#Define normalized Levenshtein distances

na.lev <- function(str1, str2) {
  norm <- max(nchar(str1), nchar(str2))
  ln <- adist(str1, str2)/norm
  return(ln)
}

#Eliminate diacritics

simp.forms <- forms
simp.forms$Form <- sapply(simp.forms$Form, str_replace_all,"ˈ","")
simp.forms$Form <- sapply(simp.forms$Form, str_replace_all,"ː","")
simp.forms$Form <- sapply(simp.forms$Form, str_replace_all,"ʔ","")
simp.forms$Form <- sapply(simp.forms$Form, str_replace_all,"ʰ","")
simp.forms$Form <- sapply(simp.forms$Form, str_replace_all,"̥","")
simp.forms$Form <- sapply(simp.forms$Form, str_replace_all,"ˌ","")
simp.forms$Form <- sapply(simp.forms$Form, str_replace_all,"̩","")

#Keep only complete cases with numerals 1-30
#intermediate: char to numeric in Parameter_ID --check for errors above 100 (I'm not using them here) 
# --- this can be extracted directly from the dataset merging
simp.forms %>%
  distinct(Parameter_ID) -> chrtonumeric


chrtonumeric$Numeric_ID <- c(1:100,
                             1000,1000000,1000000000,200,2000,400,800,111,900,4000,
                             9000,120,300,103,500,600,700,101,3000,210,
                             10000,150,1060,5000,100000,999,10000000,240,1001,6000,
                             7000,8000,144,133,140,201,102,123,250,2000000,
                             110,231,1221,202,1002,1060,1200,399,2002,108,
                             104,3041027,0,100000000,10000000000,100000000000,1000000000000)

simp.forms <- merge(simp.forms,chrtonumeric,by="Parameter_ID")

simp.forms %>%
  group_by(Language_ID) %>%
  filter(Numeric_ID <= 30) %>% #keep only numerals 1-30
  distinct(Numeric_ID, .keep_all = TRUE) %>% #keep only one form per number --check that it's the best form!
  filter(n()==30) -> tmp #keep only complete cases

my.df <- merge(tmp,languages,by.x="Language_ID",by.y="ID")


my.df.short <- select(my.df,c(ID,Name,Language_ID,Macroarea,Family,Parameter_ID,Numeric_ID,Form,Base,BestBase))

all.languages <- unique(my.df.short$Language_ID)


#reusability-----



#Function definition

#we define a function which calculates which numeral strings are completely included in other numeral strings of the same numeral system

reusability <- function(the.numbers, language = "unknown language") {
  max <- length(the.numbers)
  my.matrix <- matrix(0, nrow = max, ncol = max)
  colnames(my.matrix) <- 1:max
  rownames(my.matrix) <- 1:max
  my.counts <- as.data.frame(matrix(0,ncol=3,nrow=1))
  colnames(my.counts) <- c("numeral","morphemes","frequency")
  for(x in 1:(max-1)){
    for(y in (x+1):max){
       if(the.numbers[x] == the.numbers[y]){
           my.matrix[x,y] <- 1
           my.matrix[y,x] <- 1
       }else if(nchar(the.numbers[x])==nchar(the.numbers[y])){
        next
      }else{  
        l.index <- ifelse(nchar(the.numbers[x])>nchar(the.numbers[y]),x,y)
        s.index <- ifelse(nchar(the.numbers[x])>nchar(the.numbers[y]),y,x)
        long <- the.numbers[l.index]
        short <- the.numbers[s.index]
        my.matrix[l.index,s.index] <- as.numeric(any(grepl(short, long)))
      }
    }
  }
  the.sums <- rowSums(my.matrix)
  j<-1
  for(i in 1:max){
    
    if(the.sums[i] >0){
      my.counts[j,] <- c(i,the.sums[i],1)
      j<-j+1}
  }
  
  return(my.matrix)
}

#this functions gives the summary stats of the previous function

reusability2 <- function(the.numbers, language = "unknown language") {
  max <- length(the.numbers)
  my.matrix <- matrix(0, nrow = max, ncol = max)
  colnames(my.matrix) <- 1:max
  rownames(my.matrix) <- 1:max
  my.counts <- as.data.frame(matrix(NA,ncol=4,nrow=1))
  colnames(my.counts) <- c("language","numeral","morphemes","frequency")
  for(x in 1:(max-1)){
    for(y in (x+1):max){
      if(the.numbers[x] == the.numbers[y]){
        my.matrix[x,y] <- 1
        my.matrix[y,x] <- 1
      }else if(nchar(the.numbers[x])==nchar(the.numbers[y])){
        next
      }else{  
        l.index <- ifelse(nchar(the.numbers[x])>nchar(the.numbers[y]),x,y)
        s.index <- ifelse(nchar(the.numbers[x])>nchar(the.numbers[y]),y,x)
        long <- the.numbers[l.index]
        short <- the.numbers[s.index]
        my.matrix[l.index,s.index] <- as.numeric(any(grepl(short, long)))
      }
    }
  }
  the.sums <- rowSums(my.matrix)
  j<-1
  for(i in 1:max){
    
    if(the.sums[i] >0){
      my.counts[j,] <- c(language,i,the.sums[i],1)
    }else{
      my.counts[j,] <- c(language,i,1,1)
    }
    j<-j+1
  }
  
  return(my.counts)
}



#Global study----

#we study the reusability of numeral strings across the database

if (!dir.exists("results")) { dir.create("results") }

my.languages <- unique(my.df$Language_ID)



i<-1
result.reusability<- NULL
for(language in my.languages){
  df <- filter(my.df.short,Language_ID==language)
  the.numbers <- df$Form[order(df$Numeric_ID)]
  if(i==1){
    result.reusability <- reusability(the.numbers,language)} 
  else{result.reusability <- result.reusability + reusability(the.numbers,language)}
  i <- i+1  
}

result.reusability <- as.data.frame(result.reusability)

colnames(result.reusability) <- sprintf("S%02d", 1:30)
rownames(result.reusability) <- sprintf("C%02d", 1:30)

write.csv(result.reusability,"results/result.reusability.csv") #keep column names



#we now calculate the cumulative values


i<-1
result.reusability2<- NULL
for(language in my.languages){
  df <- filter(my.df.short,Language_ID==language)
  the.numbers <- df$Form[order(df$Numeric_ID)]
  if(i==1){
    result.reusability2 <- reusability2(the.numbers,language)} 
  else{
    result.reusability2 <- rbind(result.reusability2,reusability2(the.numbers,language))}
  i <- i+1  
}

result.reusability2 <- as.data.frame(result.reusability2)

result.reusability2 %>% 
  group_by(numeral,morphemes) %>%
  mutate(frequency = n()) %>%
  distinct(numeral,morphemes,.keep_all = T) ->this

write.csv(this,"results/counts.reusability.csv") #keep column names



#----

# Load data
reudf<-read.csv("results/result.reusability.csv") %>% 
  pivot_longer(cols=c(2:last_col()),names_to = "Atoms")

# Change format of data
colnames(reudf)[1]<-"Complex"
reudf$Atoms<-as.numeric(gsub("S","",reudf$Atoms))
reudf$Complex<-as.numeric(gsub("C","",reudf$Complex))

# Plot
reudf %>%
  ggplot(aes(x=Atoms,y=Complex,color=value))+
  geom_point(size=7)+
  scale_color_gradient(high="red",low="white")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        legend.position = "none")+
  scale_x_continuous(expand=c(0.05,0.05),breaks=c(1,5,10,15,20,25,30))+
  scale_y_continuous(expand=c(0.05,0.05),breaks=c(1,5,10,15,20,25,30))+
  geom_abline(slope=1,intercept=0)+
  labs(x="Contained numeral",y="Containing numeral")

ggsave("figures/numeralbank_reusability.png", width = 11, height=11, dpi=400)
  
