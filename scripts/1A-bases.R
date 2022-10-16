#load libraries----
library(tidyverse)
library(ggplot2)
library(cowplot) #function plot_grid


#set working directory
#setwd(getSrcDirectory()[1]) #if using R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #if using RStudio
setwd('..')

#load data----
forms <- read.csv("cldf/forms.csv")
languages <- read.csv("cldf/languages.csv", na.strings=c("","NA"))
parameters <- read.csv("cldf/parameters.csv")

#convert categorical variables to factor
new_factors <- c("Glottocode","Glottolog_Name","ISO639P3code","Macroarea","Family","BestBase","Base")
languages[,new_factors] <- lapply(languages[,new_factors] ,factor)





#visualize Base (from data) vs BestBase (inferred)----
tablebases <- as.data.frame(table(languages$Base, languages$BestBase))
colnames(tablebases) <- c("Base","BestBase","value")



tablebases %>%
  ggplot(aes(x=BestBase,y=Base,color=value,label=value))+
  geom_point(size=14)+
  scale_color_gradient(high="red",low="white")+
  geom_text(hjust=-1.5, vjust=0, colour="black",size=3)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18),
        legend.position = "none")+
  # scale_x_continuous(expand=c(0.05,0.05),breaks=c(1,5,10,15,20,25,30))+
  # scale_y_continuous(expand=c(0.05,0.05),breaks=c(1,5,10,15,20,25,30))+
  geom_abline(slope=1,intercept=0,lty=2)+
  labs(x="BestBase",y="Base") -> basesplot



#export plot
if (!dir.exists("figures")) { dir.create("figures") }
jpeg("figures/basevsbestbase.jpeg",width = 500, height = 500)
basesplot
dev.off()


#sort bases by frequency

languages$BestBase <- factor(languages$BestBase, levels = names(-sort(-table(languages$BestBase))))

table(languages$BestBase) %>%
  as.data.frame() -> freq.bestbases

colnames(freq.bestbases) <- c("BestBase","Freq")




#Map (based on a script by Hedvig)-----


variable_to_be_plotted <- "BestBase"
is_variable_ordered <- "Yes" #if the smaller value set is not categorical but has a order, set this to "Yes" and define the order below
variable_to_be_plotted_order_vec <-  c(as.character(freq.bestbases$BestBase)[-3],"unknown") #ADD NA AND SEND OTHER TO THE END

if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.


pacman::p_load(
  dplyr,#for data wrangling
  readr, #for reading in data files
  randomcoloR, #for creating large distinct color palette
  ggplot2, #for plotting
  maps, #necessary package for map plotting
  mapproj, #necessary package for map plotting
  viridis, #for color blind friendly palettes
  reshape2 #for making data wide/long with dcast (long -> wide) and melt (wide -> long)
)



options(tidyverse.quiet = TRUE)

cldf_wide_df <- languages


cat("There are", sum(is.na(cldf_wide_df$Longitude)), "languages in your dataset with missing longitude/latitude.")



#ordering values after specified order
if(is_variable_ordered == "Yes"){
  cldf_wide_df[[variable_to_be_plotted]] <- factor(cldf_wide_df[[variable_to_be_plotted]], levels = variable_to_be_plotted_order_vec)
}

##creating distinct color palette 
#This will create a set of distinct colors that corresponds to the number of unique values in the larger value parameter set and create an extra col in the df with HEX-codes corresponding to each color for each row.

if(is_variable_ordered == "Yes") {
  set.seed(17) #random color will randomise the order of the colors. I find that for Family_ID, setting the random seed to 17 looks quite nice for the larger families.
  
  n <- length(unique(cldf_wide_df[[variable_to_be_plotted]])) #counting how many distcint colors we need. Note that "NA" is also counted, and represents Isolates.
  
  # color_vector <- randomcoloR::distinctColorPalette(n)
  
  #manually, nice color palette from here: https://colorbrewer2.org/#type=qualitative&scheme=Set1&n=4
  
  color_vector <- c(
    "#377eb8", #blue
    "#4daf4a", #green
    "#984ea3", #purple
    "#e41a1c", #red
    "#808080") #grey
  
  cldf_wide_df$distinct_color_vec <- color_vector[as.factor(cldf_wide_df[[variable_to_be_plotted]])]
}


#worldmap
#rendering a base worldmap that is pacific centered

#fetching datasets
world <- ggplot2::map_data('world2', 
                           wrap=c(-25,335), #rewrapping the worldmap, i.e. shifting the center. I prefer this to world2 because I like to adjust the wrapping a bit differently, and world2 results in polygons leaking
                           ylim=c(-55,90)) #cutting out antarctica (not obligatory) and the northermost part where there are no language points in glottolog

lakes <- ggplot2:: map_data("lakes", 
                            wrap=c(-25,335), 
                            col="white", border="gray",  
                            ylim=c(-55,90))


#shifting the longlat of the dataframe to match the pacific centered map
cldf_wide_df_long_shifted <-cldf_wide_df %>% 
  dplyr::mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) %>% 
  dplyr::filter(!is.na(Longitude))



#BestBasemap
basemap <- ggplot(cldf_wide_df_long_shifted) +
  geom_polygon(data=world, aes(x=long, #plotting the landmasses
                               y=lat,group=group),
               colour="gray90",
               fill="gray90", size = 0.5) +
  geom_polygon(data=lakes, aes(x=long,#plotting lakes
                               y=lat,group=group),
               colour="gray90",
               fill="white", size = 0.3)  +
  theme(#all of theme options are set such that it makes the most minimal plot, no legend, not grid lines etc
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())   +
  coord_map(projection = "vandergrinten") + #a non-rectangular world map projection that is a decen compromise between area and distances accuracy
  ylim(-55,90) #cutting out antarctica (not obligatory) 


#Create a custom color scale
library(RColorBrewer)
myColors <- color_vector
names(myColors) <- levels(cldf_wide_df_long_shifted$BestBase)
colScale <- scale_colour_manual(name = "the.bases",values = myColors)
fillScale <- scale_fill_manual(name = "the.bases",values = myColors)

  p <- basemap + geom_point(stat = "identity", 
                            size = 2.5,
                            position = position_jitter(width = 1, height = 1 #jittering the points to prevent overplotting
                            ),
                            aes_string(x="Longitude", #using aes_string in order to be able to dynamically refer to arguments (see "fill")
                                       y="Latitude" , 
                                       fill = variable_to_be_plotted,
                                       color = variable_to_be_plotted), 
                            shape = 21, 
                            alpha = .3, 
                            stroke = 0.5, 
                            # color = "grey44"
                            ) +   
    # scale_fill_viridis_d(option = "C") +
    #scale_colour_viridis_d(option="C",na.value = "grey44") +
    theme(legend.position="none") +
  colScale +
    fillScale

    
  plot(p) 
  

#histogram
the.nas <- sum(is.na(languages$BestBase))
freq.bestbases <- freq.bestbases[c(1:2,4:5,3),]
freq.bestbases$BestBase <- as.character(freq.bestbases$BestBase)
freq.bestbases$Freq <- as.numeric(freq.bestbases$Freq)
freq.bestbases[nrow(freq.bestbases)+1,] <- c("NA",the.nas)
freq.bestbases$BestBase <- factor(freq.bestbases$BestBase, levels = variable_to_be_plotted_order_vec)

#color

color.key <- cldf_wide_df[!duplicated(cldf_wide_df$distinct_color_vec),][,c("distinct_color_vec","BestBase")]

to.plot <- merge(freq.bestbases[1:4,], color.key, all=F)
to.plot <- to.plot[order(to.plot$Freq,decreasing=T),]

to.plot <- cbind(freq.bestbases[1:4,],color_vector[1:4])
to.plot$Freq <- as.numeric(to.plot$Freq)
levels(to.plot$BestBase) = str_to_title(levels(to.plot$BestBase)) 
myColors <- color_vector
names(myColors) <- str_to_title(levels(cldf_wide_df_long_shifted$BestBase))
colScale <- scale_colour_manual(name = "the.bases",values = myColors)
fillScale <- scale_fill_manual(name = "the.bases",values = myColors)


q <- ggplot(data=to.plot, aes(x=BestBase,y=Freq, fill=BestBase)) +
  geom_bar(stat="identity") +
  theme(legend.position="none")+
  scale_y_continuous(trans = 'log10',expand=c(0,0))+ 
  theme_bw() + 
  scale_x_discrete(limits=rev)+
  theme(legend.position="none",axis.text=element_text(size=14),axis.title = element_text(size=16)) +
  ylab("Frequency") +
  xlab("") +
  # scale_fill_viridis_d(option = "C") +
  coord_flip() +
  colScale +
  fillScale


q  




#map + histogram (legend):
fn <- paste0("figures/worldmap_", variable_to_be_plotted, "with_legends.png")


plot1 <- p
right_column <- plot_grid(NULL, q, NULL, ncol=1, rel_heights = c(1,3,1))


plot_grid(p, right_column, nrow = 1, rel_widths = c(2,1)) +
  theme(plot.background = element_rect(fill = "white"))


ggsave(fn,  width =30, height = 20, units = "cm", dpi = 1200)






