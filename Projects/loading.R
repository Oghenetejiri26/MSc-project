#
#Code for MSc projects
#

setwd("/Users/teejay/Documents/data_bfmi_AIL")

phenotypes = read.table("allPhenotypes.txt", sep = "\t", header = TRUE, row.names = 1)

genotypes = read.table("genotypes_gen28.txt", sep ="\t", check.names = FALSE)

map = read.table("map.txt")

#setup for plotting the markers across the chromosomes
nchr = length(table(map[ ,"Chr"]))

maxchr = max(map[ ,"Mb_NCBI38"])

onX <- which(map[, "Chr"] == "X")
onY <- which(map[, "Chr"] == "Y")
onM <- which(map[, "Chr"] == "M")

png("Figure1.png", width = 1024, height = 786)
plot(x= c(0, nchr), y = c(0, maxchr), t = "n", 
     xlab = "Chromosome", ylab = "Position (bp)")

for(x in 1:nrow(map))  {
       #Every time for each element 
       chr <- map[x, "Chr"]
       if(x %in% onX) chr  <- 20
       if(x %in% onY) chr  <- 21
       if(x %in% onM) chr  <- 22
      points(x = chr, y = map[x, "Mb_NCBI38"], pch ="_")
}
dev.off()
#Materials and methods : Using the table function''citation''