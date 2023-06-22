#
#Code for MSc projects (June 22nd)
#

setwd("/Users/teejay/Documents/data_bfmi_AIL")

#Loading in the different files 
phenotypes = read.table("allPhenotypes.txt", sep = "\t", header = TRUE, row.names = 1)

phenotypes[,"Gen."]


gen28 <- which(phenotypes[,"Gen."] == 28)
gen27 <- which(phenotypes[,"Gen."] == 27)

offspring <- phenotypes[gen28, ]
parents <- phenotypes[gen27, ]
parents[1:5]
boxplot(offspring[, "d70"] ~ offspring[, "WG2"], notch=TRUE)

hist(offspring[, "d21"],breaks = 20)

for(day in c(21,28,35,42,49,56,63,70)){
    png(paste0("hist",day,".png"))
    hist(offspring[, paste0("d", day)],breaks = 20, col = "pink")
    dev.off()
}

boxplot(parents[, "d70"] ~ parents[, "WG2"], notch=TRUE)

hist(parents[, "d21"],breaks = 20)

iif <- which(parents[,"sex"] == "f")
iim <- which(parents[,"sex"] == "m")

for(day in c(21,28,35,42,49,56,63,70)){
    bw = parents[,paste0("d",day)]

    png(paste0("histg27",day,".png"))
    plot(x = c(min(bw)-2,max(bw)+2), y = c(0,20), t ="n")
    hist(parents[iif,paste0("d",day)], col = "pink",add = TRUE)
    hist(parents[iim, paste0("d",day)], add = TRUE,  col = rgb(0,0,255,125,max = 255))
    dev.off()
}

