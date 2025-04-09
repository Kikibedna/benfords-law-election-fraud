library(readr)
library(stringr)
library(dplyr)
library(ggplot2)

# load 
load_csv <- function(file_path){ # CSU data load and preprocess
data <- read_csv2(file_path, col_names = F)
nazev <- str_split_1(data[1,1]$X1, " ")[1]
data <- data[4:nrow(data),]
names(data) <- c(nazev, "Petr_Pavel", "Andrej_Babis")
data <- data |> mutate(Petr_Pavel = as.numeric(Petr_Pavel), 
                       Andrej_Babis = as.numeric(Andrej_Babis),
                       celkem = Petr_Pavel + Andrej_Babis)
return(data)
}

# process 
digits <- function(vec, stop=1, start=1){ # vec to first digit numeric vec 
  if(stop > 1){
    dgts<-as.numeric(substr(x=as.character(vec), start=start, stop=stop))
    return(dgts[(dgts>=10^(stop-1) & dgts< 10^(stop))])
  }
  else {
    return(as.numeric(substr(x=as.character(vec), start=start, stop=stop)))
  }
}

BL_leading_digit_table <- function(vec){
  vec <- vec[vec>0]
  Total <- length(vec)
  data.frame(table('digit' = vec), Total) |> 
    mutate(RelFreq = Freq/Total, 
           BL = log10(1 + (1/as.numeric(digit))), 
           diff = RelFreq-BL)
}

P_BL_leading_digit <- function(x){
  log10(1 + 1/x)
}

# analyze 
OMV <- function(X){ # OMV > 3 condition to use first digit BL 
  data.frame(log90th = log10(quantile(X, 0.9)),
             log10th = log10(quantile(X, 0.1)),
             OMV =  unname(log10(quantile(X, 0.9)) - log10(quantile(X, 0.1))),
             suitable = unname(log10(quantile(X, 0.9)) - log10(quantile(X, 0.1))>3))
}

