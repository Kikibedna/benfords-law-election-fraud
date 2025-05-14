library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(czso)

# citation()
# citation('readr')
# citation('stringr')
# citation('dplyr')
# citation('ggplot2')
# citation('tidyr')
# citation('patchwork')
# citation('czso')


# load --------------

# CSU data load and preprocess
read_csv_czso <- function(path, type = 'counts'){
  dta <- read.csv(path, sep = ';', dec = ',')
  if(type == 'counts'){
    data <- dta[3:nrow(dta),]
    names(data) <- c("Obec","Petr_Pavel","Andrej_Babis")
    data <- data |> mutate(Petr_Pavel = as.numeric(Petr_Pavel), 
                           Andrej_Babis = as.numeric(Andrej_Babis))
    
  } else if(type == 'ucast'){
    data <- dta[2:nrow(dta),]
    names(data) <- c("Obec", "Ucast_rel", "Volici_seznam", 
                     "Vydane_obalky", "Odevzdane_obalky", 
                     "Platne_hlasy", "Platne_hlasy_rel")
  }
  return(data)
}

# process  -------------- 
digits_on_position <- function(vec, position, na.rm = F){ # from vector of numbers to vector of digits in the corresponding position (numeric)
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

digits <- function(vec, first=0, last=0){ # from vector of numbers to vector of digits in the first leading position (numeric)
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


P_BL_FD <- function(x){ # Probability of first significant digits / Benford's First Digit Law  
  #works for any number (X = 1-999999) 
  log10(1 + 1/x)
}

P_BL_SD<-function(x){ # Probability of second significant digits / Benford's Second Digit Law  
  vec <- c()
  for(i in 1:9){
    vec <- c(vec, log10(1+1/(10*i+x)))
    print(log10(1+1/(10*i+x)))
  }
  sum(vec)
}


P_BL_general <- function(position, X = NA){ # Probability of any digit in any position / Benford's Law   
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

BL_general_digit_table <- function(vec, position){ # returns a frequency table of digits in given position with corresponding BL probabilities
  Total <- length(vec)
  data.frame(table('digit' = vec), Total) |> 
    mutate(RelFreq = Freq/Total, 
           digit = as.numeric(as.character(digit))) |> 
    left_join(P_BL_general(position)) |> 
    mutate(diff = RelFreq-BL)
}

BL_digits_table <- function(vec){# returns a frequency table of first significant digits with BL probabilities
  vec <- vec[vec>0]
  Total <- length(vec)
  data.frame(table('digit' = vec), Total) |> 
    mutate(RelFreq = Freq/Total, 
           BL = P_BL_FD(as.numeric(as.character(digit))),
           diff = RelFreq-BL)
}

BL_last_digit_table <- function(vec){# returns a frequency table of last digits with BL probabilities
  len <- str_length(as.character(vec[1])) 
  Total <- length(vec)
  data.frame(table('digit' = vec), Total) |> 
    mutate(RelFreq = Freq/Total, 
           BL = dunif(x = as.numeric(as.character(digit)), min = 0, max = 10^len),
           diff = RelFreq-BL)
}


# analyze -------------------
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


OMV <- function(X){ # OMV > 3 condition to use BL 
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

OOM <- function(X){ # OOM is a less strict OMV, outliers must be removed before the analysis 
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

compliance_test_chisq <- function(BL_table, hints = F){ # Goodness of fit test used for assessing the compliance of data with BL 
  if(!prod(BL_table$Freq > 5)){
    #stop("Assumption of the test `n · πd > 5 for all d` is not met.")
    return(data.frame(p.value = "unreliable"))
  }
  if(hints){
    cat("H0: teoreticke = namerene cetnosti\n\n")
    cat("p-hodnota > alpha: nezamitam H0\n")
    cat("p-hodnota <= alpha: zamitam H0\n")
  }
  chisq.test(x=BL_table$Freq, p=BL_table$BL, rescale.p = F)
}

# digital development pattern 
intervals <- list(
  '1' = c(1, 10),
  '2' = c(10, 100),
  '3' = c(100, 1000),
  '4' = c(1000, 10000),
  '5' = c(10000, 100000), 
  '6' = c(100000, 1000000), 
  '7' = c(1000000, 10000000)
)

calculate_digit_percentages <- function(data, intervals, digits_on_position) {# 
  results <- lapply(intervals, function(interval) {
    interval_data <- data[data >= interval[1] & data < interval[2]]
    digit_counts <- table(digits_on_position(interval_data, 1))
    digit_percentages <- (digit_counts / length(interval_data))
    overall_data <- length(interval_data)
    list(digit_percentages = digit_percentages, overall_data = overall_data)
  })
  
  results
}

ddp_table <- function(intervals, data){ #digital development pattern table - relative frequencies of digits from numbers in intervals based on their integral powers of ten 
  results <- calculate_digit_percentages(data, intervals, digits_on_position)
  table <- tibble(
    "Left Border Point" = sapply(intervals, function(x) x[1]),
    "Right Border Point" = sapply(intervals, function(x) x[2]),
    "Digit 1" = sapply(results, function(x) x$digit_percentages["1"]),
    "Digit 2" = sapply(results, function(x) x$digit_percentages["2"]),
    "Digit 3" = sapply(results, function(x) x$digit_percentages["3"]),
    "Digit 4" = sapply(results, function(x) x$digit_percentages["4"]),
    "Digit 5" = sapply(results, function(x) x$digit_percentages["5"]),
    "Digit 6" = sapply(results, function(x) x$digit_percentages["6"]),
    "Digit 7" = sapply(results, function(x) x$digit_percentages["7"]),
    "Digit 8" = sapply(results, function(x) x$digit_percentages["8"]),
    "Digit 9" = sapply(results, function(x) x$digit_percentages["9"]),
    "# of Data points" = sapply(results, function(x) x$overall_data),
    "% Overall Data" = (sapply(results, function(x) x$overall_data) / sum(sapply(results, function(x) x$overall_data))) * 100)
  
  table <- t(table)
  colnames(table) <- as.character(1:length(intervals))
  return(table)
}


# graphics  --------------

barvy <- list(
  leading_digits = c(
    BL =  "#B3C6E7", 
    RelFreq = 'darkblue'
  ),
  scatter = c(
    'A' = '#009881',
    'B' = '#A50063', 
    'C' = "#F05A22", 
    'D' = '#00AEEF', 
    'E' = '#00659B'
  ), 
  VSE = c(green = "#009881",
          aqua = "#6AD1E3",
          gray = "#dddddd",
          darkpink = "#A50063",
          pink = "#EC298A",
          blue = "#00659B",
          lightblue = "#00AEEF",
          orange = "#F05A22"
  )
)

save_pdf <- function(name, plot = last_plot(), width = 18, height = 9) {
  base_dir <- getwd()
  img_dir <- config::get("graphics") 
  pth <- file.path(base_dir, img_dir, paste0(name, ".pdf"))
  ggsave(plot = plot, 
         filename = pth, 
         device = "pdf",  
         width = width, height = height, units = "cm", dpi = 300)
}

save_png <- function(name, plot = last_plot(), width = 18, height = 9) {  
  base_dir <- getwd()
  img_dir <- config::get("graphics") 
  pth <- file.path(base_dir, img_dir, paste0(name, ".png"))
  ggsave(plot = plot, 
         pth, 
         # device = "ragg_png",
         width = width, height = height, units = "cm", dpi = 300)
}

theme_BL <- function(legend.position, text.size = 12, subtitle.text.size = 10, ...){
  theme_minimal() + theme(legend.position = legend.position, 
        legend.box.just = "right",
        text=element_text(size=text.size, family="serif"), 
        plot.subtitle = element_text(size = subtitle.text.size, hjust = 0.98), 
        plot.caption = element_text(size = subtitle.text.size, hjust = 1), 
        plot.caption.position = "plot", 
        plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        panel.background = element_rect(fill = "#eeeeee", color = "#eeeeee"),
        panel.grid.major = element_line(color = '#ffffff'), #, linetype = 'dotted'
        panel.grid.minor = element_line(color = '#ffffff'))
}

plot_BL_RelFreq_bar <- function(vec, last = F, dual = F, position = F, legend.position = "bottom", 
                                caption = NULL, title = NULL){
  
  colors = barvy$leading_digits
  
  # frequency table, BL  
  if(as.logical(position)){
    BL_table <- BL_general_digit_table(vec, position)
  } else if(last){
    BL_table <- BL_last_digit_table(vec)
  } else {
    BL_table <- BL_digits_table(vec)
  }
  BL_table <- BL_table |> mutate(digit = as.numeric(as.character(digit))) |> 
    select(-diff)
  
  # p-value, compliance test 
  test <- compliance_test_chisq(BL_table)
  if(test$p.value == "unreliable"){ #for n_k < 5 is the goodness of fit test p-value unrealiable
    subtitle.text <- "Goodness of fit test p-value: unreliable"
  } else {
    subtitle.text <- paste("Goodness of fit test p-value:", round(test$p.value, 5))
  }
  
  max <- max(c(BL_table$RelFreq,BL_table$BL)) + max(c(BL_table$RelFreq,BL_table$BL))/10 #determining the axes 
  
  n.breaks <- case_when(
    log10(max(BL_table$digit)+1) <= 1 ~ 10, 
    dual & log10(max(BL_table$digit)+1) >= 2 & log10(max(BL_table$digit)+1) < 3 ~ 25/2,
    log10(max(BL_table$digit)+1) >= 2 & log10(max(BL_table$digit)+1) < 3 ~ 25,
    T ~ 100, 
  )
  
  # BL bar plot with p-value
  BL_table |> 
    pivot_longer(cols = c("BL", "RelFreq"), names_to = "law", values_to = 'relative frequency') |> 
    ggplot(aes(x=digit, y=`relative frequency`, fill = as.factor(law))) + 
    geom_bar(stat = 'identity', position = 'dodge') + 
    ggtitle(title) + labs(subtitle = subtitle.text,
                          caption = caption) + 
    scale_x_continuous(n.breaks = n.breaks) +
    ylim(0, max) + 
    scale_fill_manual(values=colors, 
                      labels = c(RelFreq = 'Observed relative frequency', 
                                 BL = "Benford's Law"), 
                      name = NULL)  + 
    theme_BL(legend.position = legend.position)
}

# dual plot for two candidates, two BL bar plots 
dualplot_BL_RelFreq_bar <- function(A, A_title, B, B_title, max = 0.32, last = F, position = F, legend.position = 'bottom'){
  #two plots 
  plot_BL_RelFreq_bar(A, title = A_title, last = last, dual = T, position = position) + 
  plot_BL_RelFreq_bar(B, title = B_title, last = last, dual = T, position = position) + 
  
  #patchwork 
    plot_layout(guides = 'collect', axes = "collect", axis_titles = "collect_y") & 
    theme(legend.position = legend.position, plot.title = element_text(hjust = 0.5, size=12)) & 
    ylim(0, max)
}

# scatterplots with correlation, 
plot_scatter<-function(A,B,Alab,Blab, # the main two candidates 
                       X,Xlab,        # other variable
                       colors=barvy$scatter, 
                       C=NULL,Clab=NULL,D=NULL,Dlab=NULL,E=NULL,Elab=NULL #other candidates
                       ){
  if(!is.null(C) & !is.null(D) & !is.null(E)){  # all candidates in Belarus 
    data.frame(A,B,C,D,E,X) |> 
      pivot_longer(cols = c(A,B,C,D,E), 
                   values_to = "votes", names_to = "candidate")  |> 
      ggplot(aes(x = X, y = votes, color = candidate)) + 
      geom_point(alpha = 0.1) + 
      scale_color_manual(values=colors, 
                         labels = c(A = Alab,
                                    B = Blab,
                                    C = Clab,
                                    D = Dlab, 
                                    E = Elab),
                         name = NULL) + 
      labs(subtitle =
             paste("Correlation: ",
                   round(cor(c(A,B,C,D,E), # corr label for all points
                             c(X,X,X,X,X),
                             use = "complete.obs"), 2)
             )
      ) + xlab(Xlab) + theme_BL(legend.position = "bottom")
  }else{ #only two candidates (Czechia and Belarus)
    data.frame(A,B,X) |> 
      pivot_longer(cols = c(A, B), 
                   values_to = "votes", names_to = "candidate")  |> 
      ggplot(aes(x = X, y = votes, color = candidate)) + 
      geom_point(alpha = 0.1) + 
      scale_color_manual(values=colors, 
                         labels = c(A = Alab,
                                    B = Blab),
                         name = NULL) + 
      labs(subtitle =
             paste("Correlation: ",
                   round(cor(c(A,B),# corr label for all points
                             c(X,X),
                             use = "complete.obs"), 2)
             )
      ) + xlab(Xlab) + theme_BL(legend.position = "bottom")
  }
}

# scatterplots for each candidate
plot_scatter_facet<-function(A,B,Alab,Blab,X,Xlab,colors=barvy$scatter, C=NULL,Clab=NULL, D=NULL, Dlab =NULL){
  if(!is.null(C) & !is.null(D)){
    data.frame(A,B,C,D,X) |> 
      pivot_longer(cols = c(A, B,C,D), 
                   values_to = "votes", names_to = "candidate")  |> 
      ggplot(aes(x = X, y = votes, color = candidate)) + 
      geom_point(alpha = 0.1) + 
      facet_grid(.~candidate, labeller = labeller(candidate = c(A = Alab, B = Blab, 
                                                                C = Clab, D = Dlab)))+
      scale_color_manual(values=colors, 
                         labels = c(A = Alab,
                                    B = Blab,
                                    C = Clab,
                                    D = Dlab),
                         name = NULL) + 
      labs(subtitle = 
             paste(Alab, "correlation: ",  # corr label per candidate 
                   round(cor(X, 
                             A, 
                             use = "complete.obs"), 2), "\n",
                   Blab, "correlation: ", 
                   round(cor(X, 
                             B, 
                             use = "complete.obs"), 2), "\n",
                   Clab, "correlation: ", 
                   round(cor(X, 
                             C, 
                             use = "complete.obs"), 2), "\n",
                   Dlab, "correlation: ", 
                   round(cor(X, 
                             D, 
                             use = "complete.obs"), 2)
             )
      ) + xlab(Xlab) + theme_BL(legend.position = "bottom")
  }else{
    data.frame(A,B,X) |> 
      pivot_longer(cols = c(A, B), 
                   values_to = "votes", names_to = "candidate")  |> 
      ggplot(aes(x = X, y = votes, color = candidate)) + 
      geom_point(alpha = 0.1) + 
      facet_grid(candidate~., labeller = labeller(candidate = c(A = Alab, B = Blab)))+
      scale_color_manual(values=colors, 
                         labels = c(A = Alab,
                                    B = Blab),
                         name = NULL) + 
      labs(subtitle = 
             paste(Alab, "correlation: ", # corr label per candidate
                   round(cor(X, 
                             A, 
                             use = "complete.obs"), 2), "\n",
                   Blab, "correlation: ", 
                   round(cor(X, 
                             B, 
                             use = "complete.obs"), 2)
             )
      ) + xlab(Xlab) + theme_BL(legend.position = "bottom")
  }
}