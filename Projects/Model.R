#
#Code for MSc projects (July 14th)
#

setwd("/Users/teejay/Documents/data_bfmi_AIL")

#Loading in the different files 
phenotypes = read.table("allPhenotypes.txt", sep = "\t", header = TRUE, row.names = 1)

gen28 <- which(phenotypes[,"Gen."] == 28) #stores generation animals in a variable

length(gen28) #gives the number of mice in the generation 

#From previopus
children <- phenotypes[gen28, ]

toSeason <- function(month) {
    if(month == 12 | month == 1 | month == 2){ return("winter") }
    if(month == 3 | month == 4 | month == 5){ return("spring") }
    if(month == 6 | month == 7| month == 8){ return("summer") }
    if(month == 9 | month == 10 | month == 11){ return("fall") }
      return(NA)
 }

 toSeason(6)
 strsplit(phenotypes[, "W.dat"], ".", fixed=TRUE)

 splitted <- strsplit(offspring[, "W.dat"], ".", fixed=TRUE)
 lapply(splitted, "[", 2)
 unlist(lapply(splitted, "[", 2))
 bmonths <- as.numeric(unlist(lapply(splitted, "[", 2)))

 children <- cbind(children,bSeason = unlist(lapply(bmonths, toSeason)))

#Take out some of the variables for modeling 
Y <- children[, "d21"]
littersize <- children[, "WG"]
littersize2 <- children[, "WG2"]
parity <- children[, "W.Label"]
parity2 <- children[, "W.Anzahl"]
cage <- children[, "Kerb"]
color <- children[, "Farbe"]
father <- children[, "Vater"]
mother <- as.factor(children[, "Mutter"])

#Create models 
m1 <- lm(Y ~ mother)
anova(m1)
png("boxplotMopther.png")
  boxplot(Y ~ mother)
dev.off()  

m2 <- lm(Y ~ mother + littersize2)
anova(m2)
AIC(m1,m2)


m3 <- lm(Y ~ mother + as.factor(littersize))
AIC(m2,m3)#Conclusion: Littersize shopuld be included as a categorical variable

m4<- lm(Y ~ mother + as.factor(littersize) + color)
AIC(m3,m4)#Conclusion: color has no effect

m4<- lm(Y ~ mother + as.factor(littersize) + season)
AIC(m3,m4)#Conclusion: Although,it improves the model, season is not signific ant enough, so we dont account for it.

m4 <- lm(Y ~ mother + as.factor(littersize) + parity)
AIC(m3,m4)#Conclusion:


#Conclusion: The minimal model at 211 days is: lm(Y ~ mother + as.factor(littersize))


unique(father)
unique(mother)

m1 <- lm(Y ~ mother + as.factor(littersize))
m2 <- lm(Y ~ mother + as.factor(littersize) + as.factor(parity))
AIC(m1,m2)

m2 <- lm(Y ~ mother + as.factor(littersize) + as.factor(season))
AIC(m1.m2)

# Conclusion: The minimal model at 21 days is lm(Y ~ mother + as.factor(littersize) + as.factor(season))

genotypes = read.table("genotypes_gen28.txt", sep = "\t", check.names = FALSE )
map <- genotypes[,1;3]
genotypes <- genotypes[,-c(1;3)]

children <- [colnames(genotypes),]

Y <- children[, "d21"]
littersize <- as.factor[children[, "WG"]]
season <- as.factor[children[, "bSeason"]]
mother <- as.factor[children[, "Mutter"]]

pV <- c()
for(x in 1:nrow(genotypes)){
  marker <- factor(geneotypes[x,], levels = c("A", "H", "B"))
  if(length(na.omit(unique(marker))) > 1) {
    mm <- lm(Y ~ mother + littersize + marker)
    mnm <- lm(Y ~ littersize + season + mareker)
    pM <- anova(mm)["marker", "Pr(>F)"]
    pNM <- anova(mnm)["marker", "Pr(>F)"]
    pV  <- rbind(pV, c(pM, pNM))
  }else{
    pV <-rbind(pV, c(NA,NA))
  }
}
LODs <- log10(pV)