
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

#setwd("C:/Users/eosac/Documents/Docs/ACADEMIA/R_stats/Caviomorpha")
setwd("C:/Users/eosac/OneDrive - Institut Català de Paleontologia Miquel Crusafont/Documents/Docs/ACADEMIA/R_stats/Caviomorpha")


############### Bloxplot residuals Brain vs. Body Incamys ############

#Open data with PEQ & residuals
Incamys.data<-read.csv("EV_BM_Incamys_SA_mamm_OLS_NEW.csv", header=T, row.names = 1)

##ggplot - boxplot - residuals
ggboxplot(Incamys.data,x="Epoch", y="residuals", fill="Epoch",
          palette=c("lavender","lavender","lavender","lavender"),
          order=c("Oligocene","Miocene","Pliocene","Pleistocene"),
          xlab =FALSE,legend.title = "")+
  
  geom_point(aes(color=Fossil_point, shape=Shape_point), size = 2) +
  
  scale_color_manual(name = "", values = c("#3D41B6","#24A399","#FF8F39","plum","#FFCC39"),
                     labels = c("Caviomorpha","Liptoterna","Notoungulata","Primates","Xenarthra")) +
  
  scale_shape_manual(name = "", values = c(15,8,17,12,16,9),
                     labels = c("Do_mi","In_bo","Ne_ac","Ne_au","Other","Pr_pr")) +
  
  geom_text(aes(label = Abbreviations), hjust = 0, nudge_x = 0.05)+
  font("x.text", size = 8)+
  rotate_x_text(90)+
  theme(legend.position = "right")+
  labs(x='Epoch', y='Residuals EV vs. BM')

#add = "jitter" in ggboxplot ()

sink("EV_BM_ANOVA_Incamys_SAMamm_time_residuals.txt")
shapiro.test(Incamys.data$residuals) #p_value < 0.05 No normally distributed -> levene
#leveneTest(residuals ~ Group, data = Incamys.data) #p-value < 0.05 No equality of variances -> welsh
bartlett.test(residuals ~ Epoch, data = Incamys.data) #variance are equal
oneway.test(residuals ~ Epoch, data = Incamys.data, var.equal = TRUE) #differences found p-value < 0.05
Incamys.data$Epoch = factor(Incamys.data$Epoch, 
                            levels = c("Oligocene","Miocene","Pliocene","Pleistocene")) 
PT_test<-pairwisePermutationTest(residuals ~ Epoch, data = Incamys.data,method="fdr")
PT_test
#Welch.test<-welch.test(residuals~Epoch,data=Incamys.data) #p-value<0.05 - yes statistical diff found
##paircomp(Welch.test) #p-value<0.05 - yes statistical diff
sink()


##ggplot - boxplot - EQ
ggboxplot(Incamys.data,x="Epoch", y="PEQ", fill="Epoch",
          palette=c("darkslategray2","darkslategray3","darkslategray4","darkslategray"),
          order=c("Oligocene","Miocene","Pliocene","Pleistocene"),
          xlab =FALSE,legend.title = "")+
  geom_point() + 
  geom_text(aes(label = Abbreviations), hjust = 0, nudge_x = 0.05)+
  font("x.text", size = 8)+
  rotate_x_text(90)+
  theme(legend.position = "right")+
  labs(x='Epoch', y='EQ')

sink("EV_BM_ANOVA_Incamys_SAMamm_time_EQ.txt")
shapiro.test(Incamys.data$PEQ) #p_value < 0.05 No normally distributed -> levene
leveneTest(PEQ ~ Epoch, data = Incamys.data) #p-value < 0.05 No equality of variances -> welsh
#bartlett.test(PEQ ~ Epoch, data = Incamys.data) #variance are equal
oneway.test(PEQ ~ Epoch, data = Incamys.data, var.equal = TRUE) #differences found p-value < 0.05
Incamys.data$Epoch = factor(Incamys.data$Epoch, 
                            levels = c("Oligocene","Miocene","Pliocene","Pleistocene")) 
PT_test<-pairwisePermutationTest(PEQ ~ Epoch, data = Incamys.data,method="fdr")
PT_test
#Welch.test<-welch.test(PEQ~Epoch,data=Incamys.data) #p-value<0.05 - yes statistical diff found
##paircomp(Welch.test) #p-value<0.05 - yes statistical diff
sink()

## END!

#good code for size

geom_point(aes(color=Fossil_point, shape=Shape_point, size = Shape_point)) +
  
  scale_size_manual(name = "", values = c(3,3,3,3,2,3),
                    labels = c("Do_mi","In_bo","Ne_ac","Ne_au","Other","Pr_pr")) +
