# - - - - - - - - - - - - - - - -
# TTP prediction marker project
# Data Management Code
# Modified by: C.K.
# Date created: 2/22/2018
# Date modified : 2/22/2018
# - - - - - - - - - - - - - - - -


# load data 
library(readxl)
ttprawdat <- read_xlsx("../data/TTP Database only labREV_LS 1242018-HP.xlsx", sheet =  1)

# clean data
## Identify outcome columns
library(dplyr)
ttpselected <- ttprawdat %>%
  # exclude patients with value of 2nd column as 1 
  filter(`TPE( 1), plasma (P) and platelet (PP) prior to sample collection` != 1) %>%
  # rows 93 to 102 are duplicate values so for now exclude those people
  filter(`Serial #` < 92) %>%
  # select variables to use
  select(Age, `Sex ( F=1, M=0)`, `Race (W/AA)`, `ABO type`, `CNS Sign/Symps (1/0)`, `Abd pain (1/0)`, `Chest pain (1/0)`, `Disease 1`, HTN, DM, Preg, Neoplasia, HIV, HSV, HCV, SLE, Transplant, Smoking, `Rec Drugs`, `Time to plt stabilization`, pltstab7, outcome1, mort, wbc, Neutrophil, culture, hb, hct, plt, ldh, cr, pt, ptt, fibr, ddimer, protein, alb, tbili, ibili,  trop, inhibitor, hnp, histone, pai, vwfag, VWFCBA, activevwf, VWFRatio, ic3b, c59, c4d, bb, vincristineTHIS, CyclophosphamideTHIS, `rituxan this`, `eculizumab this`, `bortezomib-this`) 


colnames(ttpselected) <- c("age","sex","race","btype","cmb_cns","cmb_abd","cmb_chst","disease","htn","dm","preg","neoplasia","hiv","hsv","hcv","sle","transplant","smoking","rec_drug","time_plt_stbl","plt_stb_7","outcome1","mort","sbc","neutrophil","culture","hb","hct","plt","ldh","cr","pt","ptt","fibr","ddimer","protein","alb","tbili","ibili","trop","inhib","hnp","histone","pai","vwfag","vwfcba","activevwf","vwfratio","ic3b","c59","c4d","bb","vincristine","cyclophosphamide","rituxan","eculizumab","bortezomib")

# change variable type (i.e. from character to numeric and etc)
ttpselected <- ttpselected %>%
  # change certain variables
  mutate_at(., .vars = c("time_plt_stbl", "ptt","protein", "alb", "trop", "inhib", "rituxan", "ddimer","sbc","neutrophil", "outcome1"), .funs=as.numeric ) %>%
  # Mutate at variables
  mutate_at(., .vars = c("sex","race","btype","cmb_cns","cmb_abd","cmb_chst","disease","htn","dm","preg","neoplasia","hiv","hsv","hcv","sle","transplant","smoking","rec_drug","outcome1","mort","culture", "vincristine","cyclophosphamide","rituxan","eculizumab","bortezomib"), .funs = as.character) %>%
  # create outcome2 based on whether or not outcome 1 is > 0
  mutate(outcome2 = as.character(ifelse(outcome1 > 0, 1, 0)))

# save dataset
saveRDS(ttpselected, "C:\\Users\\ck\\Dropbox\\Research\\Huy\\TTPpm\\data\\ttpselected.RDS")
rm(ttprawdat, ttpselected)
# to read dataset
temp <- readRDS("C:\\Users\\ck\\Dropbox\\Research\\Huy\\TTPpm\\data\\ttpselected.RDS")
