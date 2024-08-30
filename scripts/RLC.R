## Random Linear Combinations and Benford's Law 
### a Monte Carlo analysis 

# from Kossovsky section 16 
# simulating data and applying BL 

# shopper is buying 2 items of random quantity from a list of products 

list <- c(2.25, 3.25, 4.75, 7.75, 9.50, 10.25, 25, 35, 37) # prices of available products 

set.seed(100)

sample(list, 2, replace  = T) 
# obmena by byla udelat to jen pomoci sample with replacement, to by taky melo fungovat 

M <- 1000
total <- c()
for (i in 1:M){
  prods <- c()
  prods <- sample(list, 2, replace  = T) 
  coef <- sample(1:6, 2, replace = T) #koeficienty pro RLC 
  total <- c(total, prods[1]*coef[1]+prods[2]*coef[2])
}
  
# a total jsou nase data 

as.numeric(substr(total[1], 1, 2)) #getting the 1st digit

first_digits <- c()
for (j in 1:M){ 
  first_digits <- c(first_digits, as.numeric(substr(total[j], 1, 1)))
}

hist(first_digits, freq = F, probability = T)


# podminka MOO - mela bych spocitat jeste ty percentily ale for the sake of this demo asi skipnu a vystacim si s timto 
minimum <- list[1]*1+list[1]*1
maximum <- list[9]*6+list[9]*6

log(maximum) - log(minimum) > 3 #splnuje 


# first and second digits 
first_second_digits <- c()
for (j in 1:M){ 
  first_second_digits <- c(first_second_digits, as.numeric(substr(total[j], 1, 2)))
}

hist(first_second_digits, freq = F, probability = T) # jak odfiltrovat ty, co maji desetinnou carku?

