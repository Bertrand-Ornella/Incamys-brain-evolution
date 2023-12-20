
############################# OLS regressions through time ##############################

setwd("C:/Users/eosac/Documents/Docs/ACADEMIA/R_stats/Caviomorpha")

### Used functions
library(geiger) #nexus file
library(nlme) # GLS analysis
library(ggplot2) #ggplot

#open data for analyses # change rowname of X to Species
Incamys.data<-read.csv("Incamys_dataset.csv", header=T)

#Delete a specific row
Incamys.data<-Incamys.data[c(1:15)]
Incamys.data<-subset(Incamys.data, Families1 !="Other")
Incamys.data<-na.omit(Incamys.data)

#Transform data to log10
Incamys.data$Brain_volume_cm3<-log10(Incamys.data$Brain_volume_cm3)
names(Incamys.data)[names(Incamys.data) == "Brain_volume_cm3"] <- "Brain"
Incamys.data$Body_mass_g<-log10(Incamys.data$Body_mass_g)
names(Incamys.data)[names(Incamys.data) == "Body_mass_g"] <- "Body"

#OLS regression
OLSline_Br_B <-gls(Brain ~ Body, data=Incamys.data)
summary(OLSline_Br_B)

gls.fit.OLS <- predict(OLSline_Br_B) #predict values for brain size
predframe.OLS <- with(Incamys.data, data.frame(Species_name, Body, Brain = gls.fit.OLS))

#Make graph with PGLS corrected regressions -- Brain/Body crown vs. stem
ggplot(Incamys.data, aes(Body, Brain, color = Families1)) +
  geom_point(data = dplyr::filter(Incamys.data, Regression_point == "Fossil"),
             size = 2, aes(color = "#3D41B6", shape = Shape_point)) +
  geom_point(data = dplyr::filter(Incamys.data, Regression_point == "Notoungulata"),
             size = 2, aes(color = "#FF8F39", shape = Shape_point)) +
  geom_point(data = dplyr::filter(Incamys.data, Regression_point == "Liptoterna"),
             size = 2, aes(color = "#24A399", shape = Shape_point)) +
  geom_point(data = dplyr::filter(Incamys.data, Regression_point == "Cingulata Xenarthra"),
             size = 2, aes(color = "#FFCC39", shape = Shape_point)) +
  geom_point(data = dplyr::filter(Incamys.data, Regression_point == "stem Platyrrhini"),
             size = 2, aes(color = "plum", shape = Shape_point)) +
  
  geom_line(data = dplyr::filter(predframe.OLS), color = "black",
            linetype = 1.5) +
 
  scale_shape_manual(name = "", values = c(15,8,17,12,16,9),
                     labels = c("Do_mi","In_bo","Ne_ac","Ne_au","Other","Pr_pr")) +
  
  theme_minimal() + 
  #theme(legend.position = "top") +
  scale_color_manual(name = "", values = c("#24A399","#3D41B6","#FF8F39","#FFCC39","plum"),
                     labels = c("Liptoterna","Fossil","Notoungulata","Cingulata Xenarthra","stem Platyrrhini")) +
  
  labs(x = "log10(Body mass)", y = "log10(Endocranial volume)") +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12,face = "bold")) 
  
  geom_text(data = dplyr::filter(Incamys.data, Families1 == "Caviomorpha"), color = "#3D41B6",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1)+ 
  geom_text(data = dplyr::filter(Incamys.data, Families1 == "Notoungulata"), color = "#FF8F39",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1) +           
  geom_text(data = dplyr::filter(Incamys.data, Families1 == "Liptoterna"), color = "#24A399",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1) +
  geom_text(data = dplyr::filter(Incamys.data, Families1 == "Xenarthra"), color = "#FFCC39",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1) +
  geom_text(data = dplyr::filter(Incamys.data, Families1 == "Primates"), color = "plum",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1)


### END! :)


