#
#Code for MSc projects (August 11th)
#

setwd("/Users/teejay/Documents/data_bfmi_AIL")

mr <- read.table("model1.txt", sep = "\t")
regions <- mr[, c(3,4,6)]
colnames(regions) <- c("Chr", "Proximal", "Distal")

#We need to install biomaRt (do this only once)
if(!require("BiocManager", quietly = TRUE))
install.packages("BiocManager")

BiocManager::install("biomaRt")

#Load the packaGE 
library(biomaRt)
bio.mart <- useMart("ENSEMBL_MART_ENSEMBL",
                     host = "https://nov2020.archive.ensembl.org",
                     dataset = "mmusculus_gene_ensembl")

listAttributes(bio.mart)[1:50,]
listFilters(bio.mart)[1:50,]

filter = c("chromosomal_region", "biotype")
attributes = c("ensembl_gene_id",
"mgi_symbol","mgi_description","gene_biotype",
"chromosome_name","start_position","end_position")

for(i in 1:nrow(regions)){
    r = paste0(regions[i,"Chr"], ":", regions[i,"Proximal"], ":", regions[i,"Distal"])

    value = list(r, "protein_coding")

    pcg <- getBM(attributes,filter,value,bio.mart)
    iix <- grep("predicted",pcg[, "mgi_description"])
    if(length(iix) > 0 ) pcg <- pcg [-iix, ]
    iix <- grep("RIKEN cDNA", pcg[, "mgi_description"])
    if(length(iix) > 0) pcg <- pcg[-iix, ]
    write.table(pcg, file = paste0 ("PCG_", gsub(":", "_",r),".txt"), sep ="\t", quote=FALSE,row.names=FALSE)
}                   