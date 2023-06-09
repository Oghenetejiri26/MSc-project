
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