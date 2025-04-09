cfg <- config::get()
source(cfg$library)

# get data from czso ----------------

library(czso)

# catalogue <- czso_get_catalogue()
# catalogue %>%
#   filter(str_detect(title, "[Vv]ol")) %>% # & str_detect(title, "2023")
#   select(dataset_id, title, description) |> View()

pr2023pet1 <- czso_get_table("pr2023pet1")

# data_okrsky <- read.csv( "data-input/CR-prezidentske-2023/pet1.csv")
# data_okrsky |> View()

dta_2 <- pr2023pet1 |> filter(KOLO == 2) |> mutate(neplatne_hlasy = as.numeric(ODEVZ_OBAL) - as.numeric(PL_HL_CELK)) 
hist(as.numeric(dta_2$HLASY_04)) # Pavel
hist(as.numeric(dta_2$HLASY_07)) # Babis
hist(as.numeric(dta_2$VYD_OBALKY))
hist(as.numeric(dta_2$ODEVZ_OBAL))
hist(as.numeric(dta_2$PL_HL_CELK))
hist(as.numeric(dta_2$neplatne_hlasy))

first_digits<- digits(dta_2$HLASY_04)
hist(first_digits)

BL_leading_digit_table(first_digits) |> 
  ggplot() + geom_bar(aes(x=digit, y=RelFreq, fill = T), stat = 'identity', position = 'dodge') + 
  geom_bar(aes(x=digit, y=BL, , alpha = 0.5),  stat = 'identity', position = 'dodge')

