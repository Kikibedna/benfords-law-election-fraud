library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

# load --------------
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


# process  -------------- 
digits_on_position <- function(vec, position, na.rm = F){
  result <- c()
  for(i in 1:length(vec)){
    result[i]<-as.numeric(str_split_i(as.character(vec[i]), pattern = "", position))
  }
  
  if(na.rm) {
    result <- result[!is.na(result)]
  }
  
  if(position == 1) {
    return(result[result>0])
  }
  return(result)
}

digits <- function(vec, first=0, last=0){ # vec to first digit numeric vec
  if(last>0){
    return(as.numeric(substr(as.character(vec),
                             start = nchar(as.character(vec)) - (last-1),
                             stop = nchar(as.character(vec)))))
  }
  if(first>0){
    if(first==1){
      return(digits_on_position(vec, 1))
    }else {
      dgts<-as.numeric(substr(x=as.character(vec),
                              start=1,
                              stop=first))
      return(dgts[(dgts>=10^(first-1) & dgts< 10^(first))])
    }
  }
  if(first < 0 | last < 0) {
    stop("params first or last must be >= 0")
  }
  if(first == 0 & last == 0){
    stop("select first or last by setting one of the params > 0")
  }
}


P_BL_FD <- function(x){ # Probability of a digit / Benford's Law First Digit Law  
  #X any number (1-999999) works
  log10(1 + 1/x)
}

P_BL_SD<-function(x){
  vec <- c()
  for(i in 1:9){
    vec <- c(vec, log10(1+1/(10*i+x)))
    print(log10(1+1/(10*i+x)))
  }
  sum(vec)
}

P_BL_general <- function(position, X = NA){
  if(position >= 5){
    data.frame(digit = 0:9,
               BL = 1/10)
  }
  
  number <- (10^(position-1)):(10^(position)-1)
  table <- data.frame(number,
                      BL = P_BL_FD(number)) |> 
    mutate(digit = digits_on_position(number, position)) |> 
    group_by(digit) |> summarise(BL = sum(BL))
  
  if(is.na(X)){
    return(table)
  } else if(X < 0 & X > 9) {
    return(0)
  } else {
    return(table$BL[table$digit == X])
  }
  
}

BL_general_digit_table <- function(vec, position){
  Total <- length(vec)
  data.frame(table('digit' = vec), Total) |> 
    mutate(RelFreq = Freq/Total, 
           digit = as.numeric(as.character(digit))) |> 
    left_join(P_BL_general(position)) |> 
    mutate(diff = RelFreq-BL)
}

BL_digits_table <- function(vec){
  vec <- vec[vec>0]
  Total <- length(vec)
  data.frame(table('digit' = vec), Total) |> 
    mutate(RelFreq = Freq/Total, 
           BL = P_BL_FD(as.numeric(as.character(digit))),
           diff = RelFreq-BL)
}

BL_leading_digit_table <- function(vec){
  vec <- vec[vec>0]
  Total <- length(vec)
  data.frame(table('digit' = vec), Total) |> 
    mutate(RelFreq = Freq/Total, 
           BL = P_BL_FD(as.numeric(as.character(digit))),
           diff = RelFreq-BL)
}

BL_last_digit_table <- function(vec){
  len <- str_length(as.character(vec[1])) 
  Total <- length(vec)
  data.frame(table('digit' = vec), Total) |> 
    mutate(RelFreq = Freq/Total, 
           BL = dunif(x = as.numeric(as.character(digit)), min = 0, max = 10^len),
           diff = RelFreq-BL)
}


# analyze 
print.OMV <- function(x, ...) {
  cat("\n", x$method, "\n\n")
  cat("90th quantile: ", x$log90th)
  cat(", 10th quantile: ", x$log10th, "\n") 
  cat("OMV = ", x$OMV)
  cat(" > 3: ", x$suitable, "\n")
}

print.OOM <- function(x, ...) {
  cat("\n", x$method, "\n\n")
  cat("max : ", x$log10max)
  cat(", min : ", x$log10min, "\n") 
  cat("OOM = ", x$OOM)
  cat(" > 3: ", x$suitable, "\n")
}


OMV <- function(X){ # OMV > 3 condition to use first digit BL 
  X <- X[!is.na(X) & !X == 0] 
  log90th <- unname(log10(quantile(X, 0.9)))
  log10th <- unname(log10(quantile(X, 0.1)))
  result <- data.frame(method = "Order of Magnitude of Variance OMV",
                       log90th,
                       log10th, 
                       OMV = unname(log90th-log10th),
                       suitable = (log90th-log10th>3))
  class(result) <- "OMV"
  return(result)
}

OOM <- function(X){ # OMV > 3 condition to use first digit BL 
  X <- X[!is.na(X) & !X == 0] 
  log10max <- unname(log10(max(X)))
  log10min <- unname(log10(min(X)))
  result <- data.frame(method = "Order of Magnitude OOM",
                       log10max,
                       log10min,
                       OOM = log10max-log10min,
                       suitable = (log10max-log10min >3))
  class(result) <- "OOM"
  return(result)
}

compliance_test_chisq <- function(BL_table){
  if(!prod(BL_table$Freq > 5)){
    #stop("Assumption of the test `n · πd > 5 for all d` is not met.")
    return(data.frame(p.value = "unreliable"))
  }
  cat("H0: teoreticke = namerene cetnosti\n\n")
  cat("p-hodnota > alpha: nezamitam H0\n")
  cat("p-hodnota <= alpha: zamitam H0\n")
  chisq.test(x=BL_table$Freq, p=BL_table$BL, rescale.p = F)
}


# graphics  --------------

barvy <- list(
  leading_digits = c(
    BL =  "#AFAFFF", #'#cccccc',
    RelFreq = 'blue3'#'darkblue'
  ),
  last_digits = c(
    BL = '#cccccc',
    RelFreq = '#7F1734'
  )
)

# barvy <- list(
#   leading_digits = c(
#     BL = '#cccccc',
#     RelFreq = '#009881'#"#009881"#'darkblue'
#   ),
#   last_digits = c(
#     BL = '#cccccc',
#     RelFreq = '#A50063'#"#A50063" #'#7F1734'
#   )
# ) 


save_png <- function(name, plot = last_plot(), width = 18, height = 9) { #tady by se hodilo dat credit do prace... 
  base_dir <- getwd()
  img_dir <- config::get("graphics") 
  pth <- file.path(base_dir, img_dir, paste0(name, ".png"))
  ggsave(plot = plot, 
         pth, 
         # device = "ragg_png",
         width = width, height = height, units = "cm", dpi = 300)
}



plot_BL_RelFreq_bar <- function(vec, title = NULL, last = F, dual = F, position = F, legend.position = "bottom", 
                                caption = NULL){
  
  colors = barvy$leading_digits
  
  if(as.logical(position)){
    BL_table <- BL_general_digit_table(vec, position)
  } else if(last){
    BL_table <- BL_last_digit_table(vec)
  } else {
    BL_table <- BL_digits_table(vec)
  }
  BL_table <- BL_table |> mutate(digit = as.numeric(as.character(digit))) |> 
    select(-diff)

  
  test <- compliance_test_chisq(BL_table)
  if(test$p.value == "unreliable"){
    subtitle.text <- "Goodness of fit test p-value: unreliable"
  } else {
    subtitle.text <- paste("Goodness of fit test p-value:", round(test$p.value, 5))
  }
  
  n.breaks <- case_when(
    log10(max(BL_table$digit)+1) <= 1 ~ 10, 
    dual & log10(max(BL_table$digit)+1) >= 2 & log10(max(BL_table$digit)+1) < 3 ~ 25/2,
    log10(max(BL_table$digit)+1) >= 2 & log10(max(BL_table$digit)+1) < 3 ~ 25,
    T ~ 100, 
  )
  
  BL_table |> 
    pivot_longer(cols = c("BL", "RelFreq"), names_to = "law", values_to = 'relative frequency') |> 
    ggplot(aes(x=digit, y=`relative frequency`, fill = as.factor(law))) + 
    geom_bar(stat = 'identity', position = 'dodge') + 
    theme_minimal() + 
    ggtitle(title) + labs(subtitle = subtitle.text,
                          caption = caption) + 
    scale_x_continuous(n.breaks = n.breaks) +
    scale_fill_manual(values=colors, 
                      labels = c(RelFreq = 'Observed relative frequency', 
                                 BL = "Benford's Law"), 
                      name = NULL)  + 
    theme(legend.position = legend.position, 
          legend.box.just = "right",
          #legend.box.background = element_rect(fill = "white", color="white", size=3), 
          text=element_text(size=12, family="serif"), 
          plot.subtitle = element_text(size = 10, hjust = 0.98), 
          plot.caption = element_text(size = 10, hjust = 1), 
          plot.caption.position = "plot") 
}

dualplot_BL_RelFreq_bar <- function(A, A_title, B, B_title, max = 0.32, last = F, position = F){
  plot_BL_RelFreq_bar(A, title = A_title, last = last, dual = T, position = position) + 
    plot_BL_RelFreq_bar(B, title = B_title, last = last, dual = T, position = position) + 
    plot_layout(guides = 'collect', axes = "collect", axis_titles = "collect_y") & 
    theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5, size=12)) & 
    ylim(0, max)
}




sample_barvy<-c(
        # barvy, co se mi libi
         "#aaaaaa","#7F1734", # last digits 
         "#cccccc","darkblue", # first digits
         "black","cornflowerblue", 
         
        # skolni / fakultni barvy
         "#dddddd", "#009881", "#6AD1E3",
         "#dddddd", "#A50063", "#EC298A",
         "#dddddd", "#00659B", "#00AEEF",
         "#F05A22" 
         )


# Sample workflow ------------

# OMV(data$total_votes)
# OOM(data$total_votes)
# 
# first_digits <- digits(data$total_votes, first = 1)
# BL_leading_digit_table(first_digits)
# compliance_test_chisq(first_digits)
# plot_BL_RelFreq_bar(first_digits)
# save_png("USA24-celkem-first_digits")
# 
# last_digits <- digits(data$total_votes, last = 1)
# BL_last_digit_table(last_digits)
# compliance_test_chisq(last_digits, last = T)
# plot_BL_RelFreq_bar(last_digits, last = T)
# save_png("USA24-celkem-last_digits")
# 
# first_two_digits <- digits(data$total_votes, first = 2)
# BL_leading_digit_table(first_two_digits)
# compliance_test_chisq(first_two_digits)
# plot_BL_RelFreq_bar(first_two_digits)
# save_png("USA24-celkem-first_two_digits")
# 
# last_two_digits <- digits(data$total_votes, last = 2)
# BL_last_digit_table(last_two_digits)
# compliance_test_chisq(last_two_digits, last = T)
# plot_BL_RelFreq_bar(last_two_digits, last = T)
# save_png("USA24-celkem-last_two_digits")
# 
# 
# OMV(data$votes_dem)
# OOM(data$votes_dem)
# 
# OMV(data$votes_gop)
# OOM(data$votes_gop)
# 
# 
# dualplot_BL_RelFreq_bar(A = digits(data$votes_gop, first = 1), A_title = "Republicans", 
#                         B = digits(data$votes_dem, first = 1), B_title = "Democrats", 
#                         last = F)
# save_png("USA24-dual-first_digits")
# 
# dualplot_BL_RelFreq_bar(A = digits(data$votes_gop, first = 2), A_title = "Republicans", 
#                         B = digits(data$votes_dem, first = 2), B_title = "Democrats", 
#                         last = F, max = 0.052)
# save_png("USA24-dual-first_two_digits")
# 
# dualplot_BL_RelFreq_bar(A = digits(data$votes_gop, last = 1), A_title = "Republicans", 
#                         B = digits(data$votes_dem, last = 1), B_title = "Democrats", 
#                         last = T, max = 0.12)
# save_png("USA24-dual-last_digits")
# 
# dualplot_BL_RelFreq_bar(A = digits(data$votes_gop, last = 2), A_title = "Republicans", 
#                         B = digits(data$votes_dem, last = 2), B_title = "Democrats", 
#                         last = T, max = 0.0155)
# save_png("USA24-dual-last_two_digits")






# testing ------------


