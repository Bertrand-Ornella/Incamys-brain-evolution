
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
gls.fit.OLS <- predict(OLSline_Br_B) #predict values for brain size
predframe.OLS <- with(Incamys.data, data.frame(Species_name, Body, Brain = gls.fit.OLS))

#Regression OLS Oligocene
Oli<- Incamys.data[ which(Incamys.data$Epoch=='Oligocene'), ]
Oli_Br_B <-gls(Brain ~ Body, data=Oli)
Oli.fit.OLS <- predict(Oli_Br_B) #predict values for brain size
predframe.Oli <- with(Oli, data.frame(Species, Body, Brain = Oli.fit.OLS))

#Regression OLS Miocene
Mio<- Incamys.data[ which(Incamys.data$Epoch=='Miocene'), ]
Mio_Br_B <-gls(Brain ~ Body, data=Mio)
Mio.fit.OLS <- predict(Mio_Br_B) #predict values for brain size
predframe.Mio <- with(Mio, data.frame(Species, Body, Brain = Mio.fit.OLS))

#Regression OLS Pliocene
Pli<- Incamys.data[ which(Incamys.data$Epoch=='Pliocene'), ]
Pli_Br_B <-gls(Brain ~ Body, data=Pli)
Pli.fit.OLS <- predict(Pli_Br_B) #predict values for brain size
predframe.Pli <- with(Pli, data.frame(Species, Body, Brain = Pli.fit.OLS))

#Regression OLS Pleistocene
Ple<- Incamys.data[ which(Incamys.data$Epoch=='Pleistocene'), ]
Ple_Br_B <-gls(Brain ~ Body, data=Ple)
Ple.fit.OLS <- predict(Ple_Br_B) #predict values for brain size
predframe.Ple <- with(Ple, data.frame(Species, Body, Brain = Ple.fit.OLS))

#Make graph with PGLS corrected regressions -- Brain/Body crown vs. stem
ggplot(Incamys.data, aes(Body, Brain, color = Epoch)) +
  geom_point(data = dplyr::filter(Incamys.data, Epoch == "Oligocene"),
             size = 2, aes(color = "darkslategray2", shape = Shape_point)) +
  geom_point(data = dplyr::filter(Incamys.data, Epoch == "Miocene"),
             size = 2, aes(color = "darkslategray3", shape = Shape_point)) +
  geom_point(data = dplyr::filter(Incamys.data, Epoch == "Pliocene"),
             size = 2, aes(color = "darkslategray4", shape = Shape_point)) +
  geom_point(data = dplyr::filter(Incamys.data, Epoch == "Pleistocene"),
             size = 2, aes(color = "darkslategray", shape = Shape_point)) +
  
  geom_line(data = dplyr::filter(predframe.OLS), color = "black",
            linetype = 1.5) +
  
  scale_shape_manual(name = "", values = c(15,8,17,12,16,9),
                     labels = c("Do_mi","In_bo","Ne_ac","Ne_au","Other","Pr_pr")) +
  
  theme_minimal() + 
  #theme(legend.position = "top") +
  
  scale_color_manual(name = "", values = c("darkslategray","darkslategray2","darkslategray3","darkslategray4"),
                     labels = c("Pleistocene","Oligocene","Miocene","Pliocene")) +
  labs(x = "log10(Body mass)", y = "log10(Endocranial volume)") +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12,face = "bold")) 
  
  geom_text(data = dplyr::filter(Incamys.data, Epoch == "Oligocene"), color = "darkslategray2",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1)+ 
  geom_text(data = dplyr::filter(Incamys.data, Epoch == "Miocene"), color = "darkslategray3",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1) +           
  geom_text(data = dplyr::filter(Incamys.data, Epoch == "Pliocene"), color = "darkslategray4",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1) +
  geom_text(data = dplyr::filter(Incamys.data, Epoch == "Pleistocene"), color = "darkslategray",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1) 

geom_line(data = dplyr::filter(predframe.Oli), color = "#3D41B6",
          linetype = 1.5) +
  geom_line(data = dplyr::filter(predframe.Mio), color = "#FF8F39",
            linetype = 1.5) +
  geom_line(data = dplyr::filter(predframe.Pli), color = "#FFCC39",
            linetype = 1.5) +
  geom_line(data = dplyr::filter(predframe.Ple), color = "#24A399",
            linetype = 1.5)
  

### END! :)


