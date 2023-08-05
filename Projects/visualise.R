#
#Code for MSc projects (July 21st)
#

setwd("/Users/teejay/Documents/data_bfmi_AIL")

#Loading in the different files 
phenotypes = read.table("allPhenotypes.txt", sep = "\t", header = TRUE, row.names = 1)

gen28 <- which(phenotypes[,"Gen."] == 28) #stores generation animals in a variable

length(gen28) #gives the number of mice in the generation 

#From previopus
children <- phenotypes[gen28, ]

genotypes = read.table("genotypes_gen28.txt", sep = "\t", check.names = FALSE )
map <- genotypes[,1:3]
genotypes <- genotypes[,-c(1:3)]

children <- children[colnames(genotypes),]

model1 =c()
model2 = c()
for(day in c("d21","d28","d35","d42","d49","d56","d63","d70")){
    pV = read.table(paste0("pvalues_", day, ".txt"), sep="\t")
    model1 = cbind(model1, pV[,1])
    model2 = cbind(model2, pV[,2])
}

image(1:nrow(model1), 1:ncol(model1), -log10(model1), xlab = "marker", ylab= "day", yaxt ="n")
axis(2,at = 1:ncol(model1), c("d21","d28","d35","d42","d49","d56","d63","d70"), las=2)
abline(h=seq(1.5,8.5,1))