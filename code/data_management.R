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
  select(Age, `Sex ( F=1, M=0)`, `Race (W/AA)`, `ABO type`, `CNS Sign/Symps (1/0)`, `Abd pain (1/0)`, `Chest pain (1/0)`, `Disease 1`, HTN, DM, Preg, Neoplasia, HIV, HSV, HCV, SLE, Transplant, Smoking, `Rec Drugs`, `Time to plt stabilization`, pltstab7, LOSthis, outcome1, mort, wbc, Neutrophil, culture, hb, hct, plt, ldh, cr, pt, ptt, fibr, ddimer, protein, alb, tbili, ibili,  trop, inhibitor, A13Igg, hnp, histone, pai, vwfag, VWFCBA, activevwf, VWFRatio, ic3b, c59, c4d, bb, vincristineTHIS, CyclophosphamideTHIS, `rituxan this`, `eculizumab this`, `bortezomib-this`, `Slope  of adm-day2`, `Slope Adm to Day 5`, `Slope LDH Adm to Day 2`, `Slope LDH to Day 5`) 


colnames(ttpselected) <- c("age","sex","race","btype","cmb_cns","cmb_abd","cmb_chst","disease","htn","dm","preg","neoplasia","hiv","hsv","hcv","sle","transplant","smoking","rec_drug","time_plt_stbl","plt_stb_7", "los" ,"outcome1","mort","sbc","neutrophil","culture","hb","hct","plt","ldh","cr","pt","ptt","fibr","ddimer","protein","alb","tbili","ibili","trop","inhib", "a13igg","hnp","histone","pai","vwfag","vwfcba","activevwf","vwfratio","ic3b","c59","c4d","bb","vincristine","cyclophosphamide","rituxan","eculizumab","bortezomib", "plt_slp_2", "plt_slp_5", "ldh_slp_2", "ldh_slp_5")

# change variable type (i.e. from character to numeric and etc)
ttpselected <- ttpselected %>%
  # change certain variables
  mutate_at(., .vars = c("time_plt_stbl", "ptt","protein", "alb", "trop", "inhib", "rituxan", "ddimer","sbc","neutrophil", "outcome1"), .funs=as.numeric ) %>%
  # Mutate at variables
  mutate_at(., .vars = c("sex","race","btype","cmb_cns","cmb_abd","cmb_chst","disease","htn","dm","preg","neoplasia","hiv","hsv","hcv","sle","transplant","smoking","rec_drug","outcome1","mort","culture", "vincristine","cyclophosphamide","rituxan","eculizumab","bortezomib"), .funs = as.character) %>%
  # create outcome2 based on whether or not outcome 1 is > 0
  mutate(outcome2 = as.character(ifelse(outcome1 > 0, 1, 0))) %>%
  # to add survival interpretation
  mutate(time = ifelse(is.na(time_plt_stbl), los, time_plt_stbl),
         status = ifelse(!is.na(time_plt_stbl), 1, 0))

# save dataset
saveRDS(ttpselected, "~/Documents/Projects/Huy/ttp_pm/data/ttpselected.RDS")
rm(ttpselected)
# to read dataset
temp <- readRDS("~/Documents/Projects/Huy/ttp_pm/data/ttpselected.RDS")



# To create data for categorical variables...
ttpselectedcat <- ttprawdat %>%
  # exclude patients with value of 2nd column as 1 
  filter(`TPE( 1), plasma (P) and platelet (PP) prior to sample collection` != 1) %>%
  # rows 93 to 102 are duplicate values so for now exclude those people
  filter(`Serial #` < 92) %>%
  # select variables to use
  dplyr::select(Age, `Sex ( F=1, M=0)`, `Race (W/AA)`, `ABO type`, `CNS Sign/Symps (1/0)`, `Abd pain (1/0)`, `Chest pain (1/0)`, `Disease 1`, HTN, DM, Preg, Neoplasia, HIV, HSV, HCV, SLE, Transplant, Smoking, `Rec Drugs`, `Time to plt stabilization`, pltstab7, LOSthis, outcome1, mort, wbc, Neutrophil, culture, hb, hct, plt, ldh, cr, pt, ptt, fibr, ddimer, protein, alb, tbili, ibili, `Low Haptoglobin (1/0)`, `Trop (nml <0.04; nml = 1; high = 2`, `HNP Hi =1; Low =2 (Range 1.8-13.7)`, `histone (hi=1; low=2; range 0.15-6.912)`, `PAI (Hi=1; low = 2) Range 53.3-2160`, `Vwf Ag (%?) Hi=1; low = 2 range 59-273.5`, `Vwf CBA (%?) Hi=1; low = 2 range (45.95-286)`, `Active VwF Hi=1; low = 2 range (44.21-187.9)`, `Vwf Ratio Hi=1; low = 2 range (0.55-2.94)`, `ic3b Hi=1; low = 2 range (6.06-15.7)`, `C5-9 Hi=1; low = 2 range (0.2-2.7)`, `C4d Hi=1; low = 2 range (1.4-4.1)`, `Bb Hi=1; low = 2 range (0.8-1.1)`, vincristineTHIS, CyclophosphamideTHIS, `rituxan this`, `eculizumab this`, `bortezomib-this`) 


colnames(ttpselectedcat) <- c("age","sex","race","btype","cmb_cns","cmb_abd","cmb_chst","disease","htn","dm","preg","neoplasia","hiv","hsv","hcv","sle","transplant","smoking","rec_drug","time_plt_stbl","plt_stb_7","los", "outcome1","mort","sbc","neutrophil","culture","hb","hct","plt","ldh","cr","pt","ptt","fibr","ddimer","protein","alb","tbili","ibili","trop","inhib","hnp","histone","pai","vwfag","vwfcba","activevwf","vwfratio","ic3b","c59","c4d","bb","vincristine","cyclophosphamide","rituxan","eculizumab","bortezomib")

# change variable type (i.e. from character to numeric and etc)
ttpselectedcat <- ttpselectedcat %>%
  # change certain variables
  mutate_at(., .vars = c("time_plt_stbl", "ptt","protein", "alb", "inhib", "rituxan", "ddimer","sbc","neutrophil", "outcome1"), .funs=as.numeric )  %>%
  # create outcome2 based on whether or not outcome 1 is > 0
  mutate(outcome2 = as.character(ifelse(outcome1 > 0, 1, 0))) %>%
  mutate_at(., .vars = c("trop","hnp","histone","pai","vwfag","vwfcba","activevwf","vwfratio","ic3b","c59","c4d","bb"),
            .funs = funs(ifelse(. == 2, 0, ifelse(. == 0, 1, 2)))) %>%
  # Mutate at variables
  mutate_at(., .vars = c("trop","hnp","histone","pai","vwfag","vwfcba","activevwf","vwfratio","ic3b","c59","c4d","bb"), .funs = as.character)%>%
  # to add survival interpretation
  mutate(time = ifelse(is.na(time_plt_stbl), los, time_plt_stbl),
         status = ifelse(!is.na(time_plt_stbl), 1, 0))

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


# In order to do survival analysis we need to add status variable that indicates event or censoring
