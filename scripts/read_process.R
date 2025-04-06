library(readr)
library(stringr)
library(dplyr)
library(ggplot2)

data_dir <- "../data-input"

load_csv <- function(file_path){
  data <- read_csv2(file_path, col_names = F)
  nazev <- str_split_1(data[1,1]$X1, " ")[1]
  data <- data[4:nrow(data),]
  names(data) <- c(nazev, "Petr_Pavel", "Andrej_Babis")
  data <- data |> mutate(Petr_Pavel = as.numeric(Petr_Pavel), 
                         Andrej_Babis = as.numeric(Andrej_Babis),
                         celkem = Petr_Pavel + Andrej_Babis)
  return(data)
}

data_obecni <- load_csv(file.path(data_dir, "CR-prezidentske-2023", "export (1).csv"))
data_okresni <- load_csv(file.path(data_dir, "CR-prezidentske-2023", "export (2).csv"))
data_kraj <- load_csv(file.path(data_dir, "CR-prezidentske-2023", "export (3).csv"))

# validace dat 
sum(data_obecni$celkem) # z CSU: 5 759 197 - melo byt odevzdano, bylo odevzdano na obecni urovni: 5 736 265 (kde je 23k?)
sum(data_okresni$celkem) # 5 759 197
(sum(data_kraj$celkem) - sum(data_obecni$celkem)) # 22932 rozdil co nam chybi 

radek <- data_okresni |> filter(Okres == "Zahraničí") # nalezeno chybejicich 23k hlasu <- 
names(radek) <- names(data_obecni)

data_obecni <- rbind(data_obecni, (radek))

numbers <- as.character(c(unname(data_obecni$Andrej_Babis)))  #unname(data_obecni$Petr_Pavel)))#, unname(data_obecni$Andrej_Babis))) #, unname(data_obecni$celkem)

first_digits<- as.numeric(substr(numbers, 1,1))
first_two_digits <- as.numeric(substr(numbers, 1,2))
first_three_digits <- as.numeric(substr(numbers, 1,3))
first_four_digits <- as.numeric(substr(numbers, 1,4))
first_five_digits <- as.numeric(substr(numbers, 1,5))



x2 <- seq(min(first_digits), max(first_digits), length = 50)
fun <- log10(1 + 1/x2)

hist(first_digits, freq = F, probability = T) 
lines(x2, fun, col = 2, lwd = 2)

# first_digits_df <- data.frame(table(first_digits)) |> 
#   mutate(Total =  sum(first_digits_df$Freq), 
#          rel_freq = Freq/SumFreq, 
#          BL = log10(1 + (1/as.numeric(first_digits))))

vec <- first_two_digits[first_two_digits > 9 & first_two_digits < 100]


BL_first_table <- function(vec){
  Total <- length(vec)
  data.frame(table('digit' = vec), Total) |> 
    mutate(RelFreq = Freq/Total, 
           BL = log10(1 + (1/as.numeric(digit))), 
           diff = RelFreq-BL)
}

table(first_two_digits[first_two_digits>9 & first_two_digits < 100])

BL_first_table(first_digits) |> 
  ggplot() + geom_bar(aes(x=digit, y=RelFreq, fill = T), stat = 'identity', position = 'dodge') + 
  geom_bar(aes(x=digit, y=BL, , alpha = 0.5),  stat = 'identity', position = 'dodge')

BL_first_table(first_two_digits[first_two_digits > 9 & first_two_digits < 100]) |> 
  ggplot() + geom_bar(aes(x=X, y=RelFreq, fill = T), stat = 'identity', position = 'dodge') + 
  geom_bar(aes(x=X, y=BL),  stat = 'identity', position = 'dodge')

tableA <- BL_first_table(first_two_digits[first_two_digits > 9 & first_two_digits < 100]) 

plot(x=tableA$X, y=tableA$RelFreq)
lines(x=tableA$X, y=tableA$BL, type = 'p', col = 'red')


hist(first_two_digits[first_two_digits>9], freq = F, probability = T)
hist(first_three_digits[first_three_digits>99], freq = F, probability = T) 
hist(first_four_digits[first_four_digits>999], freq = F, probability = T)
hist(first_five_digits[first_five_digits>9999], freq = F, probability = T)


log10(1 + 1/as.numeric(tableA$X)) 

BL_first_table(first_digits)

# Histogram
hist(x, prob = TRUE, col = "white",
     ylim = c(0, max(fun)),
     main = "Histogram with normal curve")
lines(x2, fun, col = 2, lwd = 2)

digits <- data.frame(
  first_digits = first_digits, 
  first_two_digits = first_two_digits
) |> mutate(first_two_digits = case_when(first_two_digits>9 ~ first_two_digits, 
                                         T~NA))




