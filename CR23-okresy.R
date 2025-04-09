cfg <- config::get()
source(cfg$library)

# load data ----------------

data_obecni <- load_csv(file.path(cfg$data, "CR-prezidentske-2023", "export (1).csv"))
data_okresni <- load_csv(file.path(cfg$data, "CR-prezidentske-2023", "export (2).csv"))
data_kraj <- load_csv(file.path(cfg$data, "CR-prezidentske-2023", "export (3).csv"))

# validate data ----------------
sum(data_obecni$celkem) # z CSU: 5 759 197 - melo byt odevzdano, bylo odevzdano na obecni urovni: 5 736 265 (kde je 23k?)
sum(data_okresni$celkem) # 5 759 197
(sum(data_kraj$celkem) - sum(data_obecni$celkem)) # 22932 rozdil co nam chybi 

radek <- data_okresni |> filter(Okres == "Zahraničí") # nalezeno chybejicich 23k hlasu <- 
names(radek) <- names(data_obecni)

data_obecni <- rbind(data_obecni, (radek))


# First significant digits  ----------------

numbers <- data_obecni$Andrej_Babis #unname(data_obecni$Petr_Pavel)))#, unname(data_obecni$Andrej_Babis))) #, unname(data_obecni$celkem)

first_digits <- digits(numbers)
first_two_digits <- digits(numbers,2)
first_three_digits <- digits(numbers,3)
first_four_digits <- digits(numbers,4)
first_five_digits <- digits(numbers,5)

# x2 <- seq(min(first_sign_digits), max(first_sign_digits), length = 50)
# fun <- P_BL_leading_digit(x2)
# hist(first_sign_digits, freq = F, probability = T, breaks = 11) 
# lines(x2, fun, col = 2, lwd = 2)

BL_leading_digit_table(first_digits) |> 
  ggplot() + geom_bar(aes(x=digit, y=RelFreq, fill = T), stat = 'identity', position = 'dodge') + 
    geom_bar(aes(x=digit, y=BL, , alpha = 0.5),  stat = 'identity', position = 'dodge')
