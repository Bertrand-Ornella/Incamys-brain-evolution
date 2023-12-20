
############################# PGLS and OLS regressions ##############################

### Used functions
library(geiger) #nexus file
library(nlme) # GLS analysis
library(ggplot2) #ggplot

setwd("C:/Users/eosac/Documents/Docs/ACADEMIA/R_stats/Caviomorpha")

#################### Make tree and data for regression lines extant ###################

#open data for analyses # change rowname of X to Species
Incamys.data<-read.csv("Incamys_dataset.csv", header=T, row.names = 1)

#Delete a specific row and taxon in tree
Incamys.data<-subset(Incamys.data, Regression_point =="Extant")
Incamys.data<-Incamys.data[c(1:14,17:18)]
Incamys.data<-na.omit(Incamys.data)
species<-Incamys.data$Species_name

#Open tree
tree<-read.nexus("Incamys_tree.nex")
plot(tree,cex=.3)

#Create an arbitrary branch length
tree<-compute.brlen(tree)

#Import calibrated tree to take out NAs
tree_Incamys<-drop.tip(tree,tree$tip.label[-match(species, tree$tip.label)])

#Open dataset to match with the tree and save tree for analyses
data1<-read.csv("Incamys_dataset.csv", header=T, row.names = 1)
data1<-subset(data1, Regression_point =="Extant")
Incamys.data<-data1[c(1:14,17:18)]
Incamys.data<-na.omit(Incamys.data)

#Check if the data and tree have the same names
##Make that the name in the dataset are exactly the same as in the tree
name.check(tree_Incamys, Incamys.data, data.names=NULL)

#Save dataset
write.csv(Incamys.data,'Incamys.data_Extant_Neo_ES_BM.csv')

#Save tree
write.nexus(tree_Incamys, file = "Incamys_Extant_Neo_ES_BM_tree.nex")

################################ Open data for regressions extant #################################

#open data for making regression extant 
Incamys.data.extant<-read.csv("Incamys.data_Extant_Neo_ES_BM.csv", header=T, row.names = 1)

#Open tree for making regression extant
tree_Incamys.extant<-read.nexus("Incamys_Extant_Neo_ES_BM_tree.nex")

#Check if the data and tree have the same names
name.check(tree_Incamys.extant, Incamys.data.extant, data.names=NULL)

#Transform data to log10
Incamys.data.extant$Neocortex_surface_mm2<-log10(Incamys.data.extant$Neocortex_surface_mm2)
names(Incamys.data.extant)[names(Incamys.data.extant) == "Neocortex_surface_mm2"] <- "Neo"
Incamys.data.extant$Brain_surface_mm2<-log10(Incamys.data.extant$Brain_surface_mm2)
names(Incamys.data.extant)[names(Incamys.data.extant) == "Brain_surface_mm2"] <- "Brain"

############################ Open data for regressions ALL ####################################

#Open data and tree (see 1.Incamys_OB_BM_regression)
Incamys.data<-read.csv("Incamys.data_Neo.csv", header=T, row.names = 1)
tree_Incamys<-read.nexus("Incamys_Neo_tree.nex")

#Transform data to log10
Incamys.data$Neocortex_surface_mm2<-log10(Incamys.data$Neocortex_surface_mm2)
names(Incamys.data)[names(Incamys.data) == "Neocortex_surface_mm2"] <- "Neo"
Incamys.data$Brain_surface_mm2<-log10(Incamys.data$Brain_surface_mm2)
names(Incamys.data)[names(Incamys.data) == "Brain_surface_mm2"] <- "Brain"

############################# Regression lines for extant data #####################

#Create model PGLS regression line for each Group
PGLS_line_Br_B_ex <-gls(Neo ~ Brain, correlation=corPagel (1,phy=tree_Incamys.extant), data=Incamys.data.extant)
summary(PGLS_line_Br_B_ex)

#PGLS regression
pgls.fit.PGLS.ex <- predict(PGLS_line_Br_B_ex) #predict values for Neo size
predframe.PGLS.extant <- with(Incamys.data.extant, data.frame(Species_name, Brain, Neo = pgls.fit.PGLS.ex))

#OLS regression
OLS_line_Br_B_ex <-gls(Neo ~ Brain, data=Incamys.data.extant)
summary(OLS_line_Br_B_ex)

gls.fit.Ext <- predict(OLS_line_Br_B_ex) #predict values for Neo size
predframe.OLS.extant <- with(Incamys.data.extant, data.frame(Species_name, Brain, Neo = gls.fit.Ext))

############################# Regression lines for ALL data #####################

#Create model PGLS regression line for each Group
PGLS_line_Br_B <-gls(Neo ~ Brain, correlation=corPagel (1,phy=tree_Incamys), data=Incamys.data)
summary(PGLS_line_Br_B)

#PGLS regression
pgls.fit.PGLS <- predict(PGLS_line_Br_B) #predict values for Neo size
predframe.PGLS <- with(Incamys.data, data.frame(Species_name, Brain, Neo = pgls.fit.PGLS))

#OLS regression
OLS_line_Br_B <-gls(Neo ~ Brain, data=Incamys.data)
summary(OLS_line_Br_B)

gls.fit.all <- predict(OLS_line_Br_B) #predict values for Neo size
predframe.OLS <- with(Incamys.data, data.frame(Species_name, Brain, Neo = gls.fit.all))

############### Make graph with PGLS corrected regressions -- Neo/Brain

ggplot(Incamys.data, aes(Brain, Neo, color = Regression_point)) +
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
  
  labs(x = "log10(Endocranial surface area)", y = "log10(Neocortical surface area)") +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12,face = "bold")) + 
  
  scale_shape_manual(name = "", values = c(8,12,16,9),
                     labels = c("In_bo","Ne_au","Other","Pr_pr")) +
  
  geom_text(data = dplyr::filter(Incamys.data, Regression_point == "Extant"), color = "lemonchiffon3",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1)+ 
  geom_text(data = dplyr::filter(Incamys.data, Regression_point == "Fossil"), color = "indianred2",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1) +           
  geom_text(data = dplyr::filter(Incamys.data, Regression_point == "Ischyromyidae_Sciuroidea"), color = "darkslategray4",
            aes(label = Abbreviations), hjust = -0.3, vjust = 1.1)

### END! :)


