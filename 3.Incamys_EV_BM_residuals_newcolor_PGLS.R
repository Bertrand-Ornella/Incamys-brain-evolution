
#################  Make plot residuals Incamys Brain vs. Body  ############

### Used functions
library(nlme) # GLS analysis
library(tibble) #make dataframe
library(ggpubr) #ggboxplot
library(car) #Levene's test
library(coin) #one_way test
library(rcompanion) #pairwisePermutationTest
library(onewaytests) #Welsh test

setwd("C:/Users/eosac/Documents/Docs/ACADEMIA/R_stats/Caviomorpha")

#open data for analyses # change rowname of X to Species
Incamys.data<-read.csv("Incamys_dataset.csv", header=T, row.names = 1)

#Delete a specific row and taxon in tree
Incamys.data<-Incamys.data[-c(40:62),]

#Open tree
tree_Incamys<-read.nexus("Incamys_tree.nex")
plot(tree_Incamys,cex=.3)

#Create an arbitrary branch length
tree_Incamys<-compute.brlen(tree_Incamys)

#Check if the data and tree have the same names
##Make that the name in the dataset are exactly the same as in the tree
name.check(tree_Incamys, Incamys.data, data.names=NULL)

#Transform data to log10
Incamys.data$Brain_volume_cm3<-log10(Incamys.data$Brain_volume_cm3)
names(Incamys.data)[names(Incamys.data) == "Brain_volume_cm3"] <- "Brain"
Incamys.data$Body_mass_g<-log10(Incamys.data$Body_mass_g)
names(Incamys.data)[names(Incamys.data) == "Body_mass_g"] <- "Body"

#GLS to obtain EQ and residuals
PGLS_Br_Bo <-gls(Brain ~ Body, correlation=corPagel (1,phy=tree_Incamys), data=Incamys.data)
summary(PGLS_Br_Bo) 

#values obtained from model above
a <- 0.5953788 #slope
b <--0.9317841 #intercept

Exp_b<-exp(b)
Exp_b

#Open dataset without log10
Incamys.data<-read.csv("Incamys_dataset.csv", header=T, row.names = 1)

#Delete a specific row and taxon in tree
Incamys.data<-Incamys.data[-c(40:62),]

BM <-Incamys.data$Body_mass_g
Ec <- Exp_b*(BM)^a #Expected brain size
Ei <-Incamys.data$Brain_volume_cm3
PEQ <-Ei/Ec

#Save PEQ as column in dataset
PEQ_df<-tibble(PEQ)
colnames(PEQ_df)<-c("PEQ")
Incamys.data$PEQ=PEQ

#### Now do the predicted and residuals
Incamys.data$predicted <- predict(PGLS_Br_Bo)
Incamys.data$residuals <- residuals(PGLS_Br_Bo)

#export dataframe
write.csv(Incamys.data,'EV_BM_Incamys_PGLS.csv')

############### Bloxplot residuals Brain vs. Body Incamys ############

#Open data with PEQ & residuals
Incamys.data<-read.csv("EV_BM_Incamys_PGLS.csv", header=T, row.names = 1)

##ggplot - boxplot - residuals
ggboxplot(Incamys.data,x="Families", y="residuals", fill="Families", 
          palette=c("lemonchiffon3","lemonchiffon3","lemonchiffon3","lemonchiffon3","darkslategray4"),
          order=c("Cavioidea","Erethizontoidea","Chinchilloidea","Octodontoidea","Ischyromyidae_Sciuroidea"),
          xlab =FALSE,legend.title = "")+
  
  geom_point(aes(color=Fossil_point, shape=Shape_point), size = 2) +
  
  scale_color_manual(name = "", values = c("indianred2","#000000"),
                     labels = c("Fossil","Other")) +
  
  scale_shape_manual(name = "", values = c(15,8,17,12,16,9),
                     labels = c("Do_mi","In_bo","Ne_ac","Ne_au","Other","Pr_pr")) +

  #geom_text(aes(label = Abbreviations), hjust = 0, nudge_x = 0.05)+
  font("x.text", size = 8)+
  rotate_x_text(90)+
  theme(legend.position = "right")+
  labs(x='Families', y='Residuals EV vs. BM')

geom_point(data = dplyr::filter(Incamys.data, Fossil_point == "Fossil"),
           size = 2, aes(color = "#FF0000")) +
  geom_point(data = dplyr::filter(Incamys.data, Fossil_point == "Extant"),
             size = 2, aes(color = "#000000")) +
  geom_point(data = dplyr::filter(Incamys.data, Fossil_point == "Ischyromyidae_Sciuroidea"),
             size = 2, aes(color = "#000000")) +
  

sink("EV_BM_ANOVA_Incamys_residuals.txt")
shapiro.test(Incamys.data$residuals) #p_value < 0.05 No normally distributed -> levene
#leveneTest(residuals ~ Group, data = Incamys.data) #p-value < 0.05 No equality of variances -> welsh
bartlett.test(residuals ~ Families, data = Incamys.data) #variance are equal
oneway.test(residuals ~ Families, data = Incamys.data, var.equal = TRUE) #differences found p-value < 0.05
Incamys.data$Families = factor(Incamys.data$Families, 
        levels = c("Cavioidea","Erethizontoidea","Chinchilloidea","Octodontoidea","Ischyromyidae_Sciuroidea")) 
PT_test<-pairwisePermutationTest(residuals ~ Families, data = Incamys.data,method="fdr")
PT_test
#Welch.test<-welch.test(residuals~Families,data=Incamys.data) #p-value<0.05 - yes statistical diff found
##paircomp(Welch.test) #p-value<0.05 - yes statistical diff
sink()


##ggplot - boxplot - PEQ
ggboxplot(Incamys.data,x="Families", y="PEQ", fill="Families", 
          palette=c("#FF8F39","#FFCC39","#3D41B6","#24A399","plum"),
          order=c("Cavioidea","Erethizontoidea","Chinchilloidea","Octodontoidea","Ischyromyidae_Sciuroidea"),
          xlab =FALSE,legend.title = "")+
  geom_point() + 
  #geom_text(aes(label = Abbreviations), hjust = 0, nudge_x = 0.05)+
  font("x.text", size = 8)+
  rotate_x_text(90)+
  theme(legend.position = "right")+
  labs(x='Families', y='PEQ')

sink("EV_BM_ANOVA_Incamys_PEQ.txt")
shapiro.test(Incamys.data$PEQ) #p_value < 0.05 No normally distributed -> levene
#leveneTest(PEQ ~ Group, data = Incamys.data) #p-value < 0.05 No equality of variances -> welsh
bartlett.test(PEQ ~ Families, data = Incamys.data) #variance are equal
oneway.test(PEQ ~ Families, data = Incamys.data, var.equal = TRUE) #differences found p-value < 0.05
Incamys.data$Families = factor(Incamys.data$Families, 
                               levels = c("Cavioidea","Erethizontoidea","Chinchilloidea","Octodontoidea","Ischyromyidae_Sciuroidea")) 
PT_test<-pairwisePermutationTest(PEQ ~ Families, data = Incamys.data,method="fdr")
PT_test
#Welch.test<-welch.test(PEQ~Families,data=Incamys.data) #p-value<0.05 - yes statistical diff found
##paircomp(Welch.test) #p-value<0.05 - yes statistical diff
sink()

## END!


