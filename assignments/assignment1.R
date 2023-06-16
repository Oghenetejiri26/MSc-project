
  ###
  # \file assignments.R
  #
  # Copyright (c) 2023, Oghenetejiri Umweni, 
  # last modified June, 2023
  # first written June, 2023
  #
  

setwd("~/Desktop/Msc Project")
x = 15
result <- runif(1)*50 #Random number betwen 0 and 50
if (result < 5){
    print("lower")
}else{
    print("higher")
}
if(x < 20){
    print("teejay")
}else{
    print("NO")
}

#A0.4 How do we check if something is between 0 and 10, (and error otherwise)

  answer <- runif(1)*10 #Random number between 0 and 10

if (answer >= 0 && answer <= 10) {
  print("The value is between 0 and 10.")
} else {
  print("Error: The value is not between 0 and 10.")
}

#A0.5 Use a for and a while loop from 1 to 1000 and add up all the numbers in a new variable
sum <- 0

for (i in 1:1000) {
  sum <- sum + i
}

print(sum)

sum <- 0
i <- 1

while (i <= 1000) {
  sum <- sum + i
  i <- i + 1
}

print(sum)

#A0.6 Use cat and paste to print out a triangle of #
triangle <- 12

for (i in 1:triangle) {
  cat(paste(rep("#", i), collapse = ""), "\n")
}

#A1.0 Read the two data sets into the R environment (phenotypes.txt, genotypes.txt)
setwd("/Users/teejay/Documents/data_bfmi_AIL")

phenotypes = read.table("allPhenotypes.txt", sep = "\t", header = TRUE, row.names = 1)

genotypes = read.table("genotypes_gen28.txt", sep ="\t", check.names = FALSE)

#A1.1 Study the help of read.table (?read.table) and load in only half of the genotypes.txt file
data <- read.table("genotypes_gen28.txt", sep ="\t", nrows = n/2)
