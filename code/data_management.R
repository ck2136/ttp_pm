# - - - - - - - - - - - - - - - -
# TTP prediction marker project
# Data Management Code
# Modified by: C.K.
# Date created: 2/22/2018
# Date modified : 2/22/2018
# - - - - - - - - - - - - - - - -


# load data 
library(readxl)
ttprawdat <- read_xlsx("~/Documents/Projects/Huy/ttp_pm/data/TTP Database only labREV_LS 1242018-HP.xlsx", sheet =  1)

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


# To create data for categorical variables...
ttpselectedcat <- ttprawdat %>%
  # exclude patients with value of 2nd column as 1 
  filter(`TPE( 1), plasma (P) and platelet (PP) prior to sample collection` != 1) %>%
  # rows 93 to 102 are duplicate values so for now exclude those people
  filter(`Serial #` < 92) %>%
  # select variables to use
  select(Age, `Sex ( F=1, M=0)`, `Race (W/AA)`, `ABO type`, `CNS Sign/Symps (1/0)`, `Abd pain (1/0)`, `Chest pain (1/0)`, `Disease 1`, HTN, DM, Preg, Neoplasia, HIV, HSV, HCV, SLE, Transplant, Smoking, `Rec Drugs`, `Time to plt stabilization`, pltstab7, outcome1, mort, wbc, Neutrophil, culture, hb, hct, plt, ldh, cr, pt, ptt, fibr, ddimer, protein, alb, tbili, ibili, `Low Haptoglobin (1/0)`, `Trop (nml <0.04; nml = 1; high = 2`, `HNP Hi =1; Low =2 (Range 1.8-13.7)`, `histone (hi=1; low=2; range 0.15-6.912)`, `PAI (Hi=1; low = 2) Range 53.3-2160`, `Vwf Ag (%?) Hi=1; low = 2 range 59-273.5`, `Vwf CBA (%?) Hi=1; low = 2 range (45.95-286)`, `Active VwF Hi=1; low = 2 range (44.21-187.9)`, `Vwf Ratio Hi=1; low = 2 range (0.55-2.94)`, `ic3b Hi=1; low = 2 range (6.06-15.7)`, `C5-9 Hi=1; low = 2 range (0.2-2.7)`, `C4d Hi=1; low = 2 range (1.4-4.1)`, `Bb Hi=1; low = 2 range (0.8-1.1)`, vincristineTHIS, CyclophosphamideTHIS, `rituxan this`, `eculizumab this`, `bortezomib-this`) 


colnames(ttpselectedcat) <- c("age","sex","race","btype","cmb_cns","cmb_abd","cmb_chst","disease","htn","dm","preg","neoplasia","hiv","hsv","hcv","sle","transplant","smoking","rec_drug","time_plt_stbl","plt_stb_7","outcome1","mort","sbc","neutrophil","culture","hb","hct","plt","ldh","cr","pt","ptt","fibr","ddimer","protein","alb","tbili","ibili","trop","inhib","hnp","histone","pai","vwfag","vwfcba","activevwf","vwfratio","ic3b","c59","c4d","bb","vincristine","cyclophosphamide","rituxan","eculizumab","bortezomib")

# change variable type (i.e. from character to numeric and etc)
ttpselectedcat <- ttpselectedcat %>%
  # change certain variables
  mutate_at(., .vars = c("time_plt_stbl", "ptt","protein", "alb", "inhib", "rituxan", "ddimer","sbc","neutrophil", "outcome1"), .funs=as.numeric )  %>%
  # create outcome2 based on whether or not outcome 1 is > 0
  mutate(outcome2 = as.character(ifelse(outcome1 > 0, 1, 0))) %>%
  mutate_at(., .vars = c("trop","hnp","histone","pai","vwfag","vwfcba","activevwf","vwfratio","ic3b","c59","c4d","bb"),
            .funs = funs(ifelse(. == 2, 0, ifelse(. == 0, 1, 2)))) %>%
  # Mutate at variables
  mutate_at(., .vars = c("trop","hnp","histone","pai","vwfag","vwfcba","activevwf","vwfratio","ic3b","c59","c4d","bb"), .funs = as.character)

# save dataset
saveRDS(ttpselectedcat, "~/Documents/Projects/Huy/ttp_pm/data/ttpselectedcat.RDS")
rm(ttprawdat, ttpselected, ttpselectedcat, temp)
# to read dataset
temp <- readRDS("~/Documents/Projects/Huy/ttp_pm/data/ttpselectedcat.RDS")


# since there are missing data... let's try some imputation to fill in the categorical variables

library(caret)
library(RANN)
preProcValues<- preProcess(ttpselectedcat, c("medianImpute"))
ttptransformed <- predict(preProcValues, ttpselectedcat)
