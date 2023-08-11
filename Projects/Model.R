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

 splitted <- strsplit(children[, "W.dat"], ".", fixed=TRUE)
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
season <- as.factor(children[, "bSeason"])

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

# build a model without the mother because 1/2 of your genome comes from the mother
unique(father)
unique(mother)

m1 <- lm(Y ~  as.factor(littersize))
m2 <- lm(Y ~  as.factor(littersize) + as.factor(parity))
AIC(m1,m2)

m2 <- lm(Y ~  as.factor(littersize) + as.factor(season))
AIC(m1,m2)

# Conclusion: The minimal model at 21 days is lm(Y ~  as.factor(littersize) + as.factor(season))

genotypes = read.table("genotypes_gen28.txt", sep = "\t", check.names = FALSE )
map <- genotypes[,1:3]
genotypes <- genotypes[,-c(1:3)]

children <- children[colnames(genotypes),]

for(day in c("d21","d28","d35","d42","d49","d56","d63","d70")){
Y <- children[,day]
littersize <- as.factor(children[, "WG"])
mother <- as.factor(children[, "Mutter"])
season <- as.factor(children[, "bSeason"])

pV <- c()
for(x in 1:nrow(genotypes)){
  marker <- factor(genotypes[x,], levels = c("A", "H", "B"))
  if(length(na.omit(unique(marker))) > 1) {
    mm <- lm(Y ~ mother + littersize + marker)
    mnm <- lm(Y ~ littersize + season + marker)
    pM <- anova(mm)["marker", "Pr(>F)"]
    pNM <- anova(mnm)["marker", "Pr(>F)"]
    pV  <- rbind(pV, c(pM, pNM))
  }else{
    pV <-rbind(pV, c(NA,NA))
  }
  cat("Scanning", x, "\n")
}

rownames(pV) <- rownames(genotypes)
colnames(pV) <- c("Model1", "Model2")
write.table(pV,file = paste0("pvalues_", day, ".txt"),
            sep="\t", quote=FALSE, na= "")
}    

pvalues <- read.table("pvalues_d21.txt", sep= "\t", rownames = 1, header = TRUE)
pvalues[1:10]

LODs <- -log10(pV)
m1bf <- p.adjust(pV[,1], "Bonferroni")
b1bh <- p.adjust(pV[,1], "BH")
threshold = -log10(0.05) #bonferonni correction

LODs_m1bf <- -log10(m1bf)
LODs_m1bh <- -log10(b1bh)

#plot(LODs[,1])
#plot(LODs[,2])

plot(LODs_m1bh, col = "green", pch=18)
points(LODs_m1bf, col = "red", pch=18)
albine(h = threshold)

map[which(LODs_m1bh > threshold),]