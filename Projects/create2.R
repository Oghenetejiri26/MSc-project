#
#Code for MSc projects (August 7th)
#

setwd("/Users/teejay/Documents/data_bfmi_AIL")

#Load the mapping results and genetic map
pvalues <- read.table("pvalues_d70.txt", sep= "\t", row.names = 1, header = TRUE)
genotypes <-  read.table("genotypes_gen28.txt", sep = "\t", check.names = FALSE, na.strings=c("","NA", "_") )
map <- genotypes[,1:3]
genotypes <- genotypes[,-c(1:3)]

pvalues[1:10,]
#Cponvert pvalues to LOD scores
LODs <- -log10(pvalues)
LODs[1:10,]

#Plot our QTL profile for model 1
plot(LODs[,"Model2"])
#computr the 95% threshold for significant effect 
threshold <- -log10(0.05/22000)

#Add lodscores to the map(so we know where the markers are)
mapL <- cbind(map, LODs)

#Look at which markers are above our thresholds
#Determine the Proximal.Top and Distal Marker 
mapL[which(mapL[,"Model2"] > threshold),]
mapL[which(mapL[,"Chr"] == 1),]


#How does my marker affect the bodyweight
phenotypes = read.table("allPhenotypes.txt", sep = "\t", header = TRUE, row.names = 1)
gen28 <- which(phenotypes["Gen."] == 28)

#Take only individuals that are Gen 28
children <- phenotypes[gen28, ]

#Take only individuals that are Genotyped
children <- children[colnames(genotypes), ]

#From previous (compute the months/season of birth)
toSeason <- function(month){
    if(month == 12 | month == 1 | month == 2){ return("winter") }
    if(month == 3 | month == 4 | month == 5){ return("spring") }
    if(month == 6 | month == 7| month == 8){ return("summer") }
    if(month == 9 | month == 10 | month == 11){ return("fall") }
      return(NA)
}
splitted <- strsplit(children[, "W.dat"], ".", fixed=TRUE)
bmonths <- as.numeric(unlist(lapply(splitted, "[", 2)))
children <- cbind(children,bSeason = unlist(lapply(bmonths, toSeason)))

#Take our day bodyweights
Y <- children[,"d70"]
littersize <- as.factor(children[, "WG"])
mother <- as.factor(children[, "Mutter"])
season <- as.factor(children[, "bSeason"])

#Adjust the bodyweights based on the model we used foer QTL mapping
m2 <- lm(Y ~ littersize + season)

#Compute the adjusted Bodyweight
newY <- rep(NA, length(Y))
adj <- residuals(m1)
newY[as.numeric(names(adj))] <- adj + mean(Y)

#Show the adjusted bodyweight relative to the marker(MAKE SURE TO USE THE TOPMARKER)
aa <- boxplot(newY ~ as.character(genotypes["UNC5048297",]))
aa$stats
