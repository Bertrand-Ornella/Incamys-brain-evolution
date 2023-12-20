
############################# PGLS and OLS regressions ##############################

### Used functions
library(geiger) #nexus file
library(nlme) # GLS analysis
library(ggplot2) #ggplot

setwd("C:/Users/eosac/Documents/Docs/ACADEMIA/R_stats/Caviomorpha")

################################ Open data for regressions extant #################################

#open data for making regression extant 
Incamys.data.extant<-read.csv("Incamys.data_Extant_OB_EV_BM.csv", header=T, row.names = 1)

#Open tree for making regression extant
tree_Incamys.extant<-read.nexus("Incamys_Extant_OB_EV_BM_tree.nex")

#Check if the data and tree have the same names
name.check(tree_Incamys.extant, Incamys.data.extant, data.names=NULL)

#Transform data to log10
Incamys.data.extant$Olfactory_bulbs_mm3<-log10(Incamys.data.extant$Olfactory_bulbs_mm3)
names(Incamys.data.extant)[names(Incamys.data.extant) == "Olfactory_bulbs_mm3"] <- "OB"
Incamys.data.extant$Body_mass_mg<-log10(Incamys.data.extant$Body_mass_mg)
names(Incamys.data.extant)[names(Incamys.data.extant) == "Body_mass_mg"] <- "Body"

############################ Open data for regressions ALL ####################################

#Open data and tree (see 1.Incamys_OB_BM_regression)
Incamys.data<-read.csv("Incamys.data_OB.csv", header=T, row.names = 1)
tree_Incamys<-read.nexus("Incamys_OB_tree.nex")

#Transform data to log10
Incamys.data$Olfactory_bulbs_mm3<-log10(Incamys.data$Olfactory_bulbs_mm3)
names(Incamys.data)[names(Incamys.data) == "Olfactory_bulbs_mm3"] <- "OB"
Incamys.data$Body_mass_mg<-log10(Incamys.data$Body_mass_mg)
names(Incamys.data)[names(Incamys.data) == "Body_mass_mg"] <- "Body"

############################# Regression lines for extant data #####################

#Create model PGLS regression line for each Group
PGLS_line_Br_B_ex <-gls(OB ~ Body, correlation=corPagel (1,phy=tree_Incamys.extant), data=Incamys.data.extant)
summary(PGLS_line_Br_B_ex)

#PGLS regression
pgls.fit.PGLS.ex <- predict(PGLS_line_Br_B_ex) #predict values for OB size
predframe.PGLS.extant <- with(Incamys.data.extant, data.frame(Species_name, Body, OB = pgls.fit.PGLS.ex))

#OLS regression
OLS_line_Br_B_ex <-gls(OB ~ Body, data=Incamys.data.extant)
summary(OLS_line_Br_B_ex)

gls.fit.Ext <- predict(OLS_line_Br_B_ex) #predict values for OB size
predframe.OLS.extant <- with(Incamys.data.extant, data.frame(Species_name, Body, OB = gls.fit.Ext))

############################# Regression lines for ALL data #####################

#Create model PGLS regression line for each Group
PGLS_line_Br_B <-gls(OB ~ Body, correlation=corPagel (1,phy=tree_Incamys), data=Incamys.data)
summary(PGLS_line_Br_B)

#PGLS regression
pgls.fit.PGLS <- predict(PGLS_line_Br_B) #predict values for OB size
predframe.PGLS <- with(Incamys.data, data.frame(Species_name, Body, OB = pgls.fit.PGLS))

#OLS regression
OLS_line_Br_B <-gls(OB ~ Body, data=Incamys.data)
summary(OLS_line_Br_B)

gls.fit.all <- predict(OLS_line_Br_B) #predict values for OB size
predframe.OLS <- with(Incamys.data, data.frame(Species_name, Body, OB = gls.fit.all))

############### Make graph with PGLS corrected regressions -- OB/Body

ggplot(Incamys.data, aes(Body, OB, color = Regression_point)) +
  geom_point(data = dplyr::filter(Incamys.data, Regression_point == "Extant"),
             size = 2, aes(color = "lemonchiffon3", shape = Shape_point)) +
  geom_point(data = dplyr::filter(Incamys.data, Regression_point == "Fossil"),
             size = 2, aes(color = "indianred2", shape = Shape_point)) +
  geom_point(data = dplyr::filter(Incamys.data, Regression_point == "Ischyromyidae_Sciuroidea"),
             size = 2, aes(color = "darkslategray4", shape = Shape_point)) +
  
  geom_line(data = dplyr::filter(predframe.PGLS.extant), color = "black",
            linetype = 1.5) +
  geom_line(data = dplyr::filter(predframe.OLS.extant), color = "black",
            linetype = "dashed") +
  
  
  geom_line(data = dplyr::filter(predframe.PGLS), color = "red",
            linetype = 1.5) +
  geom_line(data = dplyr::filter(predframe.OLS), color = "red",
            linetype = "dashed") +
  
  theme_minimal() + 
  #theme(legend.position = "top") +
  scale_color_manual(name = "", values = c("darkslategray4","indianred2","lemonchiffon3"),
                     labels = c("Ischyromyidae_Sciuroidea","Fossil","Extant")) +
  
  labs(x = "log10(Body mass)", y = "log10(Olfactory bulb volume)") +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12,face = "bold")) + 
  
  scale_shape_manual(name = "", values = c(8,12,16,9),
                     labels = c("In_bo","Ne_au","Other","Pr_pr")) 
  
  geom_text(data = dplyr::filter(Incamys.data, Regression_point == "Extant"), color = "lemonchiffon3",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1)+ 
  geom_text(data = dplyr::filter(Incamys.data, Regression_point == "Fossil"), color = "indianred2",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1) +           
  geom_text(data = dplyr::filter(Incamys.data, Regression_point == "Ischyromyidae_Sciuroidea"), color = "darkslategray4",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1)

### END! :)


