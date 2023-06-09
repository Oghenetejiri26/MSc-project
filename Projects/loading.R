#
#Code for MSc projects
#

setwd("/Users/teejay/Documents/data_bfmi_AIL")

phenotypes = read.table("allPhenotypes.txt", sep = "\t", header = TRUE, row.names = 1)

genotypes = read.table("genotypes_gen28.txt", sep ="\t", check.names = FALSE)

map = read.table("map.txt")
table(map[ ,"Chr"])

#Materials and methods : Using the table function''citation''