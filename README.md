# Benford's Law and the Statistical Detection of Election Fraud

This repository was created as an part of author's bachelor thesis. 
In this thesis, the author explores the applicability of Benford’s Law as a forensic tool for detecting anomalies in electoral data, with Benford's Law as a part of a designed reproducible methodology, as can be seen in an implementation in R in this repository. 

This repository contains the following: 
- `functions.R` file with all the functions used  
- `.qmd` files for all three analysed countries (Belarus, the USA and Czechia) 
- `data-input` directory, with data for the analysis 
- `fig` directory contains the created outputs of the analysis (.pdf figures)
- `outputs` directory contains exported tables (.csv files)
- `config.yml` file with the repository setup 


`data-input` is not shared, due to copyright issues. However, the data used is described below: 
- [Czechia](https://www.volby.cz/pls/prez2023nss/pe71?xjazyk=EN#/0-01-2023-0), downloaded from [volby.cz](https://www.volby.cz/pls/prez2023nss/pe71?xjazyk=EN#/0-01-2023-0) on 2025-04-18, by the Czech Statistical Office 
- [the USA](https://doi.org/10.5281/zenodo.14223604), available on [GitHub](https://github.com/tonmcg/US_County_Level_Election_Results_08-24), downloaded on 2025-05-01, by Tony McGovern, Stephen Larson, Bill Morris and Matt Hodges 
- [Belarus](https://www.kaggle.com/datasets/vadimkonovod/2020-belarusian-presidential-election/data), downloaded from [Kaggle](https://www.kaggle.com/datasets/vadimkonovod/2020-belarusian-presidential-election/data) on 2025-04-18 by Honest People and Zubr 

