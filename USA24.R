cfg <- config::get()
source(cfg$library)

data <- read.csv(file.path(cfg$data, "USA-2024", "2024_US_County_Level_Presidential_Results.csv"), header = T, sep = ",")
data |> names()

# Celkem okrsky ------------

OMV(data$total_votes)
OOM(data$total_votes)

first_digits <- digits(data$total_votes, first = 1)
BL_leading_digit_table(first_digits)
compliance_test_chisq(first_digits)
plot_BL_RelFreq_bar(first_digits)
save_png("USA24-celkem-first_digits")

last_digits <- digits(data$total_votes, last = 1)
BL_last_digit_table(last_digits)
compliance_test_chisq(last_digits, last = T)
plot_BL_RelFreq_bar(last_digits, last = T)
save_png("USA24-celkem-last_digits")

first_two_digits <- digits(data$total_votes, first = 2)
BL_leading_digit_table(first_two_digits)
compliance_test_chisq(first_two_digits)
plot_BL_RelFreq_bar(first_two_digits)
save_png("USA24-celkem-first_two_digits")

last_two_digits <- digits(data$total_votes, last = 2)
BL_last_digit_table(last_two_digits)
compliance_test_chisq(last_two_digits, last = T)
plot_BL_RelFreq_bar(last_two_digits, last = T)
save_png("USA24-celkem-last_two_digits")


OMV(data$votes_dem)
OOM(data$votes_dem)

OMV(data$votes_gop)
OOM(data$votes_gop)


dualplot_BL_RelFreq_bar(A = digits(data$votes_gop, first = 1), A_title = "Donald Trump", 
                        B = digits(data$votes_dem, first = 1), B_title = "Kamala Harris", 
                        last = F)
save_png("USA24-dual-first_digits")

dualplot_BL_RelFreq_bar(A = digits(data$votes_gop, first = 2), A_title = "Donald Trump", 
                        B = digits(data$votes_dem, first = 2), B_title = "Kamala Harris", 
                        last = F, max = 0.052)
save_png("USA24-dual-first_two_digits")

dualplot_BL_RelFreq_bar(A = digits(data$votes_gop, last = 1), A_title = "Donald Trump", 
                        B = digits(data$votes_dem, last = 1), B_title = "Kamala Harris", 
                        last = T, max = 0.12)
save_png("USA24-dual-last_digits")

dualplot_BL_RelFreq_bar(A = digits(data$votes_gop, last = 2), A_title = "Donald Trump", 
                        B = digits(data$votes_dem, last = 2), B_title = "Kamala Harris", 
                        last = T, max = 0.0155)
save_png("USA24-dual-last_two_digits")

# Celkem staty ------------

data |> View()

A <- data |> group_by(state_name) |> summarise(gop = sum(votes_gop), dem = sum(votes_dem), total = sum(total_votes))

OMV(A$total)
OOM(A$total)

A <- data |> group_by(county_name) |> summarise(n = n(), gop = sum(votes_gop), dem = sum(votes_dem), total = sum(total_votes))
A |> View()

OMV(A$total)
OOM(A$total)



