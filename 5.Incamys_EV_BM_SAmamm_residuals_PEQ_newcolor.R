
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
Incamys.data<-Incamys.data[c(1:14)]
Incamys.data<-subset(Incamys.data, Families1 !="Other")

#Transform data to log10
Incamys.data$Brain_volume_cm3<-log10(Incamys.data$Brain_volume_cm3)
names(Incamys.data)[names(Incamys.data) == "Brain_volume_cm3"] <- "Brain"
Incamys.data$Body_mass_g<-log10(Incamys.data$Body_mass_g)
names(Incamys.data)[names(Incamys.data) == "Body_mass_g"] <- "Body"

#GLS to obtain EQ and residuals
OLS_Br_Bo <-gls(Brain ~ Body, data=Incamys.data)
summary(OLS_Br_Bo) 

#values obtained from model above
a <- 0.5373975 #slope
b <--0.8307481 #intercept

Exp_b<-exp(b)
Exp_b

#Open dataset without log10
Incamys.data<-read.csv("Incamys_dataset.csv", header=T, row.names = 1)

#Delete a specific row
Incamys.data<-Incamys.data[c(1:14)]
Incamys.data<-subset(Incamys.data, Families1 !="Other")

BM <-Incamys.data$Body_mass_g
Ec <- Exp_b*(BM)^a #Expected brain size
Ei <-Incamys.data$Brain_volume_cm3
PEQ <-Ei/Ec

#Save PEQ as column in dataset
PEQ_df<-tibble(PEQ)
colnames(PEQ_df)<-c("PEQ")
Incamys.data$PEQ=PEQ

#### Now do the predicted and residuals
Incamys.data$predicted <- predict(OLS_Br_Bo)
Incamys.data$residuals <- residuals(OLS_Br_Bo)

#export dataframe
write.csv(Incamys.data,'EV_BM_Incamys_SA_mamm_OLS.csv')

############### Bloxplot residuals Brain vs. Body Incamys ############

#Open data with PEQ & residuals
Incamys.data<-read.csv("EV_BM_Incamys_SA_mamm_OLS.csv", header=T, row.names = 1)

##ggplot - boxplot - residuals
ggboxplot(Incamys.data,x="Families1", y="residuals", fill="Families1",
          palette=c("lavender","lavender","lavender","lavender","lavender"),
          order=c("Caviomorpha","Xenarthra","Notoungulata","Primates","Liptoterna"),
          xlab =FALSE,legend.title = "")+
  
  geom_point(aes(color=Fossil_point, shape=Shape_point), size = 2) +
  
  scale_color_manual(name = "", values = c("indianred2","#000000"),
                     labels = c("Fossil","Other")) +
  
  scale_shape_manual(name = "", values = c(15,8,17,12,16,9),
                     labels = c("Do_mi","In_bo","Ne_ac","Ne_au","Other","Pr_pr")) +
  
  geom_text(aes(label = Abbreviations), hjust = 0, nudge_x = 0.05)+
  font("x.text", size = 8)+
  rotate_x_text(90)+
  theme(legend.position = "right")+
  labs(x='Families1', y='Residuals EV vs. BM')

#add = "jitter" in ggboxplot ()

#Only select categories that have more than 2 taxa to run test
Incamys.data<-subset(Incamys.data, Families1 =="Notoungulata"|Families1 =="Caviomorpha"|
                       Families1 =="Xenarthra"| Families1 =="Liptoterna")

sink("EV_BM_ANOVA_Incamys_SAMamm_residuals.txt")
shapiro.test(Incamys.data$residuals) #p_value < 0.05 No normally distributed -> levene
#leveneTest(residuals ~ Group, data = Incamys.data) #p-value < 0.05 No equality of variances -> welsh
bartlett.test(residuals ~ Families1, data = Incamys.data) #variance are equal
oneway.test(residuals ~ Families1, data = Incamys.data, var.equal = TRUE) #differences found p-value < 0.05
Incamys.data$Families1 = factor(Incamys.data$Families1, 
                               levels = c("Notoungulata","Caviomorpha","Liptoterna","Xenarthra")) 
PT_test<-pairwisePermutationTest(residuals ~ Families1, data = Incamys.data,method="fdr")
PT_test
#Welch.test<-welch.test(residuals~Families1,data=Incamys.data) #p-value<0.05 - yes statistical diff found
##paircomp(Welch.test) #p-value<0.05 - yes statistical diff
sink()

#Open data with PEQ & residuals
Incamys.data<-read.csv("EV_BM_Incamys_SA_mamm_OLS.csv", header=T, row.names = 1)

##ggplot - boxplot - EQ
ggboxplot(Incamys.data,x="Families1", y="PEQ", fill="Families1", 
          palette=c("#3D41B6","#FFCC39","#FF8F39","plum","#24A399"),
          order=c("Caviomorpha","Xenarthra","Notoungulata","Primates","Liptoterna"),
          xlab =FALSE,legend.title = "")+
  geom_point() + 
  #geom_text(aes(label = Abbreviations), hjust = 0, nudge_x = 0.05)+
  font("x.text", size = 8)+
  rotate_x_text(90)+
  theme(legend.position = "right")+
  labs(x='Families1', y='EQ')

#Only select categories that have more than 2 taxa to run test
Incamys.data<-subset(Incamys.data, Families1 =="Notoungulata"|Families1 =="Caviomorpha"|
                       Families1 =="Xenarthra"| Families1 =="Liptoterna")

sink("EV_BM_ANOVA_Incamys_SAMamm_EQ.txt")
shapiro.test(Incamys.data$PEQ) #p_value < 0.05 No normally distributed -> levene
leveneTest(PEQ ~ Families1, data = Incamys.data) #p-value < 0.05 No equality of variances -> welsh
#bartlett.test(PEQ ~ Families1, data = Incamys.data) #variance are equal
oneway.test(PEQ ~ Families1, data = Incamys.data, var.equal = TRUE) #differences found p-value < 0.05
Incamys.data$Families1 = factor(Incamys.data$Families1, 
                                levels = c("Notoungulata","Caviomorpha","Liptoterna","Xenarthra")) 
PT_test<-pairwisePermutationTest(PEQ ~ Families1, data = Incamys.data,method="fdr")
PT_test
#Welch.test<-welch.test(PEQ~Families1,data=Incamys.data) #p-value<0.05 - yes statistical diff found
##paircomp(Welch.test) #p-value<0.05 - yes statistical diff
sink()

## END!


