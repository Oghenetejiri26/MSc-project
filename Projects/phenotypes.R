#
#Code for MSc projects (June 16th)
#

setwd("/Users/teejay/Documents/data_bfmi_AIL")

#Loading in the different files 
phenotypes = read.table("allPhenotypes.txt", sep = "\t", header = TRUE, row.names = 1)

phenotypes[,"Gen."] #writes out all the generation numbers e.g [1] 28 28 28 27 28 26

phenotypes[,"Gen."] == 28 #writes out all the mice born in Gen 28 e.g  [1]  TRUE  TRUE  TRUE  FALSE

which(phenotypes[,"Gen."] == 28) #e.g  writes out only animals born in Gen 28[1]   1   2   3   4   5

gen28 <- which(phenotypes[,"Gen."] == 28) #stores generation animals in a variable
gen27 <- which(phenotypes[,"Gen."] == 27)
gen26 <- which(phenotypes[,"Gen."] == 26)

length(gen28) #gives the number of mice in the generation 
length(gen27)
length(gen26)

offspring <- phenotypes[gen28, ]
table(offspring[, "sex"])  #gives the number of sex male & female

parents <- phenotypes[gen27, ]
table(parents[,"sex"])

#Do some 
boxplot(offspring[, "d21"] ~ offspring[, "WG2"], notch=TRUE)
boxplot(parents[, "d21"] ~ parents[, "sex"], notch=TRUE)  #plots a graph 
boxplot(parents[, "d70"] ~ parents[, "sex"], notch=TRUE)

anova(lm(offspring[, "d21"] ~ offspring[, "WG2"],)) #calculates Anova
anova(lm(offspring[, "d21"] ~ offspring[, "W.Label"] + offspring[, "WG2"])) 
anova(lm(offspring[, "d21"] ~ offspring[, "W.Label"] + offspring[, "WG2"] + offspring[, "Farbe"])) 
anova(lm(offspring[, "d21"] ~ offspring[, "W.Label"] + offspring[, "WG2"] + offspring[, "praep1"]))

#build a bigger model includimg 3 predictors
res <- anova(lm(offspring[, "d21"] ~ offspring[, "W.Label"] + offspring[, "WG2"] + offspring[, "praep1"]))

#compute variance explained:sum of Squares / Sum of Sum of Squares
round((res[, "Sum Sq"] / sum(res[, "Sum Sq"])) * 100, 1)
 pie(round((res[, "Sum Sq"] / sum(res[, "Sum Sq"])) * 100, 1))

#WG2 as numeric
 m1 <- lm(offspring[, "d21"] ~ offspring[, "WG2"])
 #WG2 as factor
 m2 <- lm(offspring[, "d21"] ~ as.factor(offspring[, "WG2"]))

 m3 <- lm(offspring[, "d21"] ~ offspring[, "WG2"] + offspring[, "W.Label"])

 m4 <- lm(offspring[, "d21"] ~ offspring[, "WG2"] + offspring[, "W.Label"] + offspring[, "praep1"])
 AIC(m3,m4) #Concl: Model 3 is prefered

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

unlist(lapply(bmonths, toSeason))

offspring <- cbind(offspring, bSeason = unlist(lapply(bmonths, toSeason)))

m5 <- lm(offspring[, "d21"] ~ offspring[, "WG2"] + offspring[, "W.Label"] + offspring[, "bSeason"])
AIC(m3,m5)

table(offspring[,"Futter"])