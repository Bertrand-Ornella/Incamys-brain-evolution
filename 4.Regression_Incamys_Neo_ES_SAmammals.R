
############################# OLS regressions through time ##############################

setwd("C:/Users/eosac/Documents/Docs/ACADEMIA/R_stats/Caviomorpha")

### Used functions
library(geiger) #nexus file
library(nlme) # GLS analysis
library(ggplot2) #ggplot

#open data for analyses # change rowname of X to Species
Incamys.data<-read.csv("Incamys_dataset.csv", header=T)

#Delete a specific row
Incamys.data<-Incamys.data[c(1:11,18:19)]
Incamys.data<-subset(Incamys.data, Families1 !="Other")
Incamys.data<-na.omit(Incamys.data)

#Transform data to log10
Incamys.data$Neocortex_surface_mm2<-log10(Incamys.data$Neocortex_surface_mm2)
names(Incamys.data)[names(Incamys.data) == "Neocortex_surface_mm2"] <- "Neo"
Incamys.data$Brain_surface_mm2<-log10(Incamys.data$Brain_surface_mm2)
names(Incamys.data)[names(Incamys.data) == "Brain_surface_mm2"] <- "Brain"

#OLS regression
OLSline_Neo_B <-gls(Neo ~ Brain, data=Incamys.data)
summary(OLSline_Neo_B)

gls.fit.OLS <- predict(OLSline_Neo_B) #predict values for Neo size
predframe.OLS <- with(Incamys.data, data.frame(Species_name, Brain, Neo = gls.fit.OLS))

#Make graph with PGLS corrected regressions -- Neo/Brain crown vs. stem
ggplot(Incamys.data, aes(Brain, Neo, color = Regression_point)) +
  
  geom_point(data = dplyr::filter(Incamys.data, Regression_point == "Fossil"),
             size = 2, aes(color = "#3D41B6", shape = Shape_point)) +
  geom_point(data = dplyr::filter(Incamys.data, Regression_point == "Notoungulata"),
             size = 2, aes(color = "#FF8F39", shape = Shape_point)) +
  
  geom_line(data = dplyr::filter(predframe.OLS), color = "black",
            linetype = 1.5) +
 
  scale_shape_manual(name = "", values = c(8,12,16,9),
                     labels = c("In_bo","Ne_au","Other","Pr_pr")) +
  
  theme_minimal() + 
  #theme(legend.position = "top") +
  scale_color_manual(name = "", values = c("#3D41B6","#FF8F39"),
                     labels = c("Fossil","Notoungulata")) +
  
  labs(x = "log10(Endocranial surface area)", y = "log10(Neocortical surface area)") +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12,face = "bold")) 
  
  geom_text(data = dplyr::filter(Incamys.data, Families1 == "Fossil"), color = "#3D41B6",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1)+ 
  geom_text(data = dplyr::filter(Incamys.data, Families1 == "Notoungulata"), color = "#FF8F39",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1) +           



### END! :)


