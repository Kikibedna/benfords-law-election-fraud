cfg <- config::get()
source(cfg$library)

# load data ----------------

data_obecni <- load_csv(file.path(cfg$data, "CR-prezidentske-2023", "export (1).csv"))
data_okresni <- load_csv(file.path(cfg$data, "CR-prezidentske-2023", "export (2).csv"))
data_kraj <- load_csv(file.path(cfg$data, "CR-prezidentske-2023", "export (3).csv"))

# validate data ----------------

# sum(data_obecni$celkem) # z CSU: 5 759 197 - melo byt odevzdano, bylo odevzdano na obecni urovni: 5 736 265 (kde je 23k?)
# sum(data_okresni$celkem) # 5 759 197
# (sum(data_kraj$celkem) - sum(data_obecni$celkem)) # 22932 rozdil co nam chybi 

radek <- data_okresni |> filter(Okres == "Zahraničí") # nalezeno chybejicich 23k hlasu <- 
names(radek) <- names(data_obecni)

data_obecni <- rbind(data_obecni, (radek))


# CELKEM  ----------------

## First significant digits  ----------------

numbers <- data_obecni$celkem #unname(data_obecni$Petr_Pavel)))#, unname(data_obecni$Andrej_Babis))) #, unname(data_obecni$celkem)

OMV(data_obecni$celkem)
OMV(data_obecni$Andrej_Babis)
OMV(data_obecni$Petr_Pavel)

OOM(data_obecni$celkem) #very liberal, influenced by outliers! 
OOM(data_obecni$Andrej_Babis) 
OOM(data_obecni$Petr_Pavel) 

max(data_obecni$celkem)

first_digits <- digits(numbers, first = 1)
first_two_digits <- digits(numbers,first = 2)
# first_three_digits <- digits(numbers,first = 3)
# first_four_digits <- digits(numbers,first = 4)
# first_five_digits <- digits(numbers,first = 5)

# x2 <- seq(min(first_sign_digits), max(first_sign_digits), length = 50)
# fun <- P_BL_FD(x2)
# hist(first_sign_digits, freq = F, probability = T, breaks = 11) 
# lines(x2, fun, col = 2, lwd = 2)
plot_BL_RelFreq_bar(first_digits)
save_png("CZ23-celkem-first_digits")



### chisq test -------

table_obecniCR2 <- BL_leading_digit_table(first_digits) |> mutate(frac = (diff^2)/BL, 
                                                                  ndpi = BL*Freq, 
                                                                  npi = BL*Total, 
                                                                  absdiff = Freq-npi)
sum(table_obecniCR2$absdiff)

# assumption of the chiqs test
prod(table_obecniCR2$ndpi > 5) == 1 
prod(table_obecniCR2$npi > 5) == 1 

n <- table_obecniCR2$Total |> unique()

G <- n * sum(table_obecniCR2$frac)
alpha <- 0.05

G > pchisq(q = 1-alpha, df = 8) # toto je confusing melo by to byt ze kdyz je G vetsi nez to chi tak zamitam null :) 

chisq.test(x=table_obecniCR2$Freq, p=table_obecniCR2$BL, rescale.p = F)

# je-li p-hodnota > alpha: nezamitam H0 
# je-li p-hodnota <= alpha: zamitam H1

# H0: empirical distribution follows the theoretical distr. 
# H!: empirical distribution does not follow the theoretical distr. 
# result: false, the G (test statistics) falls into the acceptance region, not the rejection region 
# which means we reject the alternative hypothesis and we can say this distribiton follows the BL :) 

# now on the pvalue <--


## First two digits --------

plot_BL_RelFreq_bar(first_two_digits)
save_png("CZ23-celkem-first_two_digits")


compliance_test_chisq(first_two_digits)


## Last digits --------

last_digits <- digits(numbers, last = 1)
last_two_digits <- digits(numbers,last = 2)
# last_three_digits <- digits(numbers,last = 3)
# last_four_digits <- digits(numbers,last = 4)
# last_five_digits <- digits(numbers,last = 5)

BL_last_digit_table(last_digits)

plot_BL_RelFreq_bar(last_digits, last = T)
save_png("CZ23-celkem-last_digits")

compliance_test_chisq(last_digits, last = T)


## Last two digits --------

BL_last_digit_table(last_two_digits)

plot_BL_RelFreq_bar(last_two_digits, last = T)
save_png("CZ23-celkem-last_two_digits")

compliance_test_chisq(last_two_digits, last = T)




# DUALPLOT --------

PP <- data_obecni$Petr_Pavel 
AB <- data_obecni$Andrej_Babis 

OMV(PP)
OMV(AB)


dualplot_BL_RelFreq_bar(A = digits(data_obecni$Petr_Pavel, first = 1), A_title = "Petr Pavel", 
                        B = digits(data_obecni$Andrej_Babis, first = 1), B_title = "Andrej Babiš", 
                        last = F)
save_png("CZ23-dual-first_digits")

dualplot_BL_RelFreq_bar(digits(data_obecni$Petr_Pavel, first = 2), "Petr Pavel", 
                        digits(data_obecni$Andrej_Babis, first = 2), "Andrej Babiš", 
                        last = F, max = 0.045)
save_png("CZ23-dual-first_two_digits")

dualplot_BL_RelFreq_bar(digits(data_obecni$Petr_Pavel, last = 1), "Petr Pavel", 
                        digits(data_obecni$Andrej_Babis, last = 1), "Andrej Babiš", 
                        last = T, max = 0.11)
save_png("CZ23-dual-last_digits")

dualplot_BL_RelFreq_bar(digits(data_obecni$Petr_Pavel, last = 2), "Petr Pavel", 
                        digits(data_obecni$Andrej_Babis, last = 2), "Andrej Babiš", 
                        last = T, max = 0.015)
save_png("CZ23-dual-last_two_digits")
