
######################## Choice of regression OLS check OB vs BM ##########################

### Used functions
library(geiger) #nexus file
library(nlme) # GLS analysis

setwd("C:/Users/eosac/Documents/Docs/ACADEMIA/R_stats/Caviomorpha")

#open data for analyses # change rowname of X to Species
Incamys.data<-read.csv("Incamys_dataset.csv", header=T, row.names = 1)

#Delete a specific row and taxon in tree
Incamys.data<-Incamys.data[-c(40:62),]
Incamys.data<-Incamys.data[c(1:14,17:18)]
Neo.na<-na.omit(Incamys.data)
species<-Neo.na$Species_name

#Open tree
tree<-read.nexus("Incamys_tree.nex")
plot(tree,cex=.3)

#Create an arbitrary branch length
tree<-compute.brlen(tree)

#Import calibrated tree to take out NAs
tree_Incamys<-drop.tip(tree,tree$tip.label[-match(species, tree$tip.label)])

#Open dataset to match with the tree and save tree for analyses
data1<-read.csv("Incamys_dataset.csv", header=T, row.names = 1)
data1<-data1[-c(40:62),]
Neo1<-data1[c(1:14,17:18)]
Incamys.data<-na.omit(Neo1)

#Check if the data and tree have the same names
##Make that the name in the dataset are exactly the same as in the tree
name.check(tree_Incamys, Incamys.data, data.names=NULL)

#Save dataset
write.csv(Incamys.data,'Incamys.data_Neo.csv')

#Save tree
write.nexus(tree_Incamys, file = "Incamys_Neo_tree.nex")

### END! :)
