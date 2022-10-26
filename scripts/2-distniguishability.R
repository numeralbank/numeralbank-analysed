#load libraries----
library(groundhog)
my.date <- "2022-10-16"
pkgs <- c("tidyverse","ggplot2","cowplot","gridExtra")
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






#string manipulation ----

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
#Explore numerals present in each language


nrow(languages) #5336 > 4077
unique(languages$Glottocode) #4347 > 3271
unique(simp.forms$Language_ID) #5336 > 4077
#fewer in all nb than in chan? There were 5336 in numerals alone (chan), but only 4077 in all numeralbank-analysed

#1-30 -----

#Keep only complete cases ALL numerals 1-30
#intermediate: char to numeric in Parameter_ID --check for errors above 100 (I'm not using them here) 
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
  
  we.keep <- c(1:30)
  
simp.forms %>%
  group_by(Language_ID) %>%
  filter(Numeric_ID %in% we.keep) %>% #keep only numerals 1-30
  distinct(Numeric_ID, .keep_all = TRUE) %>% #keep only one form per number
  filter(n()==30) -> tmp #keep only complete cases


#merge datasets and keep only the relevant variables

my.df <- merge(tmp,languages,by.x="Language_ID",by.y="ID")

my.df.short <- select(my.df,c(ID,Language_ID,Numeric_ID,Macroarea,Family,Parameter_ID,Form,Base))

all.languages <- unique(my.df.short$Language_ID)


# Define function----

#this function takes one wordlist of consecutive numerals in a language and one window size w, 
#and retrieves the average Levenshtein distance of each numeral and their w closest neighbors

local_alignment <- function(the.numbers, language = "unknown language", w=3) {
  the.max <- length(the.numbers)
  correlations <- as.data.frame(matrix(rep(0,2*the.max),ncol=2))
  colnames(correlations) <- c("n","rho")
  for(num in 1:the.max){
    left.w <- max(c(1,(num-w)))    
    right.w <- min(c((num+w),the.max))
   xx <- left.w:right.w
   x <- abs(xx-num)
   y <- na.lev(the.numbers[xx],the.numbers[num])
   correlations[num,] <- c(num,cor(x,y, method="spearman"))
    }
  return(correlations) }


#Global sample----
#list of languages
my.languages <- unique(my.df.short$Language_ID)

#generate a dataset for each language and different window sizes w= 1:12 --run time about 5 min

my.data <- list(0)
for(my.w in 1:12){
  i<-1
  result.alignment<- as.list(0)
  for(language in my.languages){
    df <- filter(my.df.short,Language_ID==language)
    the.numbers <- df$Form[order(df$Numeric_ID)]
    result.alignment[[i]] <- local_alignment(the.numbers,language,w=my.w)
    names(result.alignment)[i] <- language
    i <- i+1  
  }
  my.data[[my.w]] <- bind_rows(result.alignment, .id="df")
  my.data[[my.w]]$w <- my.w
}
my.data.df <- do.call(rbind.data.frame, my.data)

colnames(my.data.df) <- c("Language_ID","Numeric_ID","rho","w")
tmp <- merge(my.data.df,my.df.short,by=c("Language_ID","Numeric_ID"))

tmp %>% select("Language_ID","Macroarea","Family","Numeric_ID","rho","w") -> dist_merged

write.csv(dist_merged,"results/distinguishability.csv")




#plot all languages
my.plots <- list(0)
for(my.w in 1:12){
p <- ggplot(subset(my.data.df, w==my.w), aes(Numeric_ID, rho, colour=Language_ID)) +
  geom_line(alpha=0.2) +
  theme(legend.position="none", panel.background = element_blank()) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_hline(yintercept=1, linetype="dotted") +
  geom_hline(yintercept=-1, linetype="dotted") +
  geom_vline(xintercept=10, linetype="dashed") +
  geom_vline(xintercept=20, linetype="dashed") +
  geom_vline(xintercept=5, linetype="dotted") +
  geom_vline(xintercept=15, linetype="dotted") +
  geom_vline(xintercept=25, linetype="dotted") +
  ggtitle(paste0("All languages, w= ",my.w))

my.plots[[my.w]] <- p
}

library(gridExtra)
n <- length(my.plots)
nCol <- floor(sqrt(n))
allplots <- do.call("grid.arrange", c(my.plots, ncol=nCol))

jpeg("figures/numeralbank_distinguishability.jpeg",width = 2000, height = 2000)
do.call("grid.arrange", c(my.plots, ncol=nCol))
dev.off()


