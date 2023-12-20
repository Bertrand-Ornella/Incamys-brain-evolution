
#################  Make plot residuals Incamys Brain vs. Body  ############

### Used functions
library(nlme) # GLS analysis
library(tibble) #make dataframe
library(ggpubr) #ggboxplot
library(car) #Levene's test
library(coin) #one_way test
library(rcompanion) #pairwisePermutationTest
library(onewaytests) #Welsh test
library(tibble)

setwd("C:/Users/eosac/Documents/Docs/ACADEMIA/R_stats/Caviomorpha")

#open data for analyses # change rowname of X to Species
Incamys.data<-read.csv("Incamys_dataset.csv", header=T, row.names = 1)

#Delete a specific row
Incamys.data<-Incamys.data[c(1:15)]
Incamys.data<-subset(Incamys.data, Families1 !="Other")
Incamys.data<-na.omit(Incamys.data)

#Transform data to log10
Incamys.data$Olfactory_bulbs_mm3<-log10(Incamys.data$Olfactory_bulbs_mm3)
names(Incamys.data)[names(Incamys.data) == "Olfactory_bulbs_mm3"] <- "OB"
Incamys.data$Brain_volume_mm3<-log10(Incamys.data$Brain_volume_mm3)
names(Incamys.data)[names(Incamys.data) == "Brain_volume_mm3"] <- "Brain"

#GLS to obtain residuals
OLS_OB_Br <-gls(OB ~ Brain, data=Incamys.data)
summary(OLS_OB_Br) 

#### Now do the predicted and residuals
Incamys.data$predicted <- predict(OLS_OB_Br)
Incamys.data$residuals <- residuals(OLS_OB_Br)

#export dataframe
write.csv(Incamys.data,'OB_EV_Incamys_SA_mamm_OLS.csv')

############### Bloxplot residuals Brain vs. Body Incamys ############

#Open data with PEQ & residuals
Incamys.data<-read.csv("OB_EV_Incamys_SA_mamm_OLS.csv", header=T, row.names = 1)

##ggplot - boxplot - residuals
ggboxplot(Incamys.data,x="Families1", y="residuals", fill="Families1",
          palette=c("lavender","lavender","lavender","lavender"),
          order=c("Caviomorpha","Xenarthra","Notoungulata","Primates"),
          xlab =FALSE,legend.title = "")+

  geom_point(aes(color=Fossil_point, shape=Shape_point), size = 2) +
  
  scale_color_manual(name = "", values = c("indianred2","#000000"),
                     labels = c("Fossil","Other")) +
  
  scale_shape_manual(name = "", values = c(8,12,16,9),
                     labels = c("In_bo","Ne_au","Other","Pr_pr")) +
  
  
  geom_text(aes(label = Abbreviations), hjust = 0, nudge_x = 0.05)+
  font("x.text", size = 8)+
  rotate_x_text(90)+
  theme(legend.position = "right")+
  labs(x='Families1', y='Residuals OB vs. EV')

#add = "jitter" in ggboxplot ()

#Only select categories that have more than 2 taxa to run test
Incamys.data<-subset(Incamys.data, Families1 =="Notoungulata"|Families1 =="Caviomorpha"|
                       Families1 =="Xenarthra")

sink("OB_EV_ANOVA_Incamys_SAMamm_residuals.txt")
shapiro.test(Incamys.data$residuals) #p_value < 0.05 No normally distributed -> levene
#leveneTest(residuals ~ Group, data = Incamys.data) #p-value < 0.05 No equality of variances -> welsh
bartlett.test(residuals ~ Families1, data = Incamys.data) #variance are equal
oneway.test(residuals ~ Families1, data = Incamys.data, var.equal = TRUE) #differences found p-value < 0.05
Incamys.data$Families1 = factor(Incamys.data$Families1, 
                               levels = c("Notoungulata","Caviomorpha","Xenarthra")) 
PT_test<-pairwisePermutationTest(residuals ~ Families1, data = Incamys.data,method="fdr")
PT_test
#Welch.test<-welch.test(residuals~Families1,data=Incamys.data) #p-value<0.05 - yes statistical diff found
##paircomp(Welch.test) #p-value<0.05 - yes statistical diff
sink()

## END!


