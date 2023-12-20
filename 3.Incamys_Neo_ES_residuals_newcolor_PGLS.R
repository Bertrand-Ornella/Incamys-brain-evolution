
#################  Make plot residuals Incamys PL vs. Body  ############

### Used functions
library(nlme) # GLS analysis
library(tibble) #make dataframe
library(ggpubr) #ggboxplot
library(car) #Levene's test
library(coin) #one_way test
library(rcompanion) #pairwisePermutationTest
library(onewaytests) #Welsh test

setwd("C:/Users/eosac/Documents/Docs/ACADEMIA/R_stats/Caviomorpha")

#Open data and tree (see 1.Incamys_PL_BM_regression)
Incamys.data<-read.csv("Incamys.data_Neo.csv", header=T, row.names = 1)
tree_Incamys<-read.nexus("Incamys_Neo_tree.nex")

#Transform data to log10
Incamys.data$Neocortex_surface_mm2<-log10(Incamys.data$Neocortex_surface_mm2)
names(Incamys.data)[names(Incamys.data) == "Neocortex_surface_mm2"] <- "Neo"
Incamys.data$Brain_surface_mm2<-log10(Incamys.data$Brain_surface_mm2)
names(Incamys.data)[names(Incamys.data) == "Brain_surface_mm2"] <- "Brain"

#Create model PGLS regression line for each Group
PGLS_Neo_Br <-gls(Neo ~ Brain, correlation=corPagel (1,phy=tree_Incamys), data=Incamys.data)
summary(PGLS_Neo_Br)

#### Now do the predicted and residuals
Incamys.data$predicted <- predict(PGLS_Neo_Br)
Incamys.data$residuals <- residuals(PGLS_Neo_Br)

#export dataframe
write.csv(Incamys.data,'Neo_ES_Incamys_PGLS.csv')

############### Bloxplot residuals Neo vs. Brain Incamys ############

#Open data with PEQ & residuals
Incamys.data<-read.csv("Neo_ES_Incamys_PGLS.csv", header=T, row.names = 1)

##ggplot - boxplot - residuals
ggboxplot(Incamys.data,x="Families", y="residuals", fill="Families", 
          palette=c("lemonchiffon3","lemonchiffon3","lemonchiffon3","lemonchiffon3","darkslategray4"),
          order=c("Cavioidea","Erethizontoidea","Chinchilloidea","Octodontoidea","Ischyromyidae_Sciuroidea"),
          xlab =FALSE,legend.title = "")+
  
  geom_point(aes(color=Fossil_point, shape=Shape_point), size = 2) +
  
  scale_color_manual(name = "", values = c("indianred2","#000000"),
                     labels = c("Fossil","Other")) +
  
  scale_shape_manual(name = "", values = c(8,12,16,9),
                     labels = c("In_bo","Ne_au","Other","Pr_pr")) +
  
  #geom_text(aes(label = Abbreviations), hjust = 0, nudge_x = 0.05)+
  font("x.text", size = 8)+
  rotate_x_text(90)+
  theme(legend.position = "right")+
  labs(x='Families', y='Residuals Neo vs. ES')

sink("Neo_ES_ANOVA_Incamys_residuals.txt")
shapiro.test(Incamys.data$residuals) #p_value < 0.05 No normally distributed -> levene
leveneTest(residuals ~ Families, data = Incamys.data) #p-value < 0.05 No equality of variances -> welsh
#bartlett.test(residuals ~ Families, data = Incamys.data) #variance are equal
oneway.test(residuals ~ Families, data = Incamys.data, var.equal = TRUE) #differences found p-value < 0.05
Incamys.data$Families = factor(Incamys.data$Families, 
                               levels = c("Cavioidea","Erethizontoidea","Chinchilloidea","Octodontoidea","Ischyromyidae_Sciuroidea")) 
PT_test<-pairwisePermutationTest(residuals ~ Families, data = Incamys.data,method="fdr")
PT_test
#Welch.test<-welch.test(residuals~Families,data=Incamys.data) #p-value<0.05 - yes statistical diff found
##paircomp(Welch.test) #p-value<0.05 - yes statistical diff
sink()

## END!


