# - - - - - - - - - - - - - - - -
# TTP prediction marker project
# Temporary File
# Modified by: C.K.
# Date created: 3/6/2018
# Date modified : 3/6/2018
# - - - - - - - - - - - - - - - -


# load data with focus on survival analysis
rm(list=ls())
temp <- readRDS("~/Documents/Projects/Huy/ttp_pm/data/ttpselected.RDS")

# kaplan-meier calculation (non parametric survival probabilities)
library(survival)
result.km <- survfit(Surv(temp$time, temp$status) ~ 1, conf.type = "log-log")
# To calculated the potential follow-up time
temp$fu <- 1 - temp$status
result.rkm <- survfit(Surv(temp$time, temp$fu) ~ 1, conf.type = "log-log")

plot(result.km, conf.int = T, mark = "|", xlab = "Time", 
     ylab = "Non stabilization probability")
title("Non Stabilization Probability of TTP patients")

# using ggplot
library(survival)
library(survminer)


# Customized survival curves
ggsurvplot(result.km, data = temp,
           surv.median.line = "hv", # Add medians survival
           
           # Change legends: title & labels
           #legend.title = "Sex",
           #legend.labs = c("Male", "Female"),
           # Add p-value and tervals
           #pval = TRUE,
           
           conf.int = TRUE,
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           
           # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
           # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
           #palette = c("#E7B800", "#2E9FDF"),
           ggtheme = theme_bw() # Change ggplot2 theme
)

result.km
summary(result.km)$table[7] # median survival time
# median time to event (platelet stabilizaiton is 6 units(days?))
result.rkm
summary(result.rkm)$table[[7]]
# the potential follow-up time is 38 units(days?)


# Visualize the hazard funciton non parametrically
library(muhaz)
# we want to check the hazard at 5 and 10 months interval
result.pe10 <- pehaz(temp$time, temp$status, width = 10, 
                     max.time = 60)
result.pe5 <- pehaz(temp$time, temp$status, width = 5, 
                     max.time = 60)

result.smooth <- muhaz(temp$time, temp$status, bw.smooth = 10, 
                       b.cor = "left", max.time = 60)

plot(result.pe10, ylim = c(0, 0.3), col = "blacK")
title("Hazard Function Estimation")
lines(result.pe5, col = "green")
lines(result.smooth, col = "red")

# we can clearly see a decreasing hazard function... in this case an exponential would not be appropriate. 

haz <- result.smooth$haz.est
times <- result.smooth$est.grid
surv <- exp(-cumsum(haz[1:(length(haz) - 1)] * diff(times)))
plot(result.km, conf.int = T, mark = "|", xlab = "Time in months", 
     xlim = c(0, 60), ylab = "Survival Probability")
title("Fitted Survival Curve with KM estimate \n and Hazard estimated curve")
lines(surv ~ times[1:(length(times) - 1)], col = "blue")

# using cox-ph to determine the effect of covariates on hazard... 

# first univariate modeling
model.coxph <- coxph(Surv(time, status) ~ age + vincristine, data = temp)
summary(model.coxph)$waldtest[[3]]  # this gets the p-value from the wald-test
summary(model.coxph)$waldtest[[3]] < 0.05 # this determines if the variable statistically significantly affects the hazard 

setdiff(colnames(temp), c("eculizumab", "cyclophosphamide", "transplant"))

tmp <- sapply(setdiff(colnames(temp), c("eculizumab", "cyclophosphamide", "transplant")), function(x) {
  try(
    c(summary(coxph(Surv(time, status) ~ get(x), data = temp))$waldtest[[3]] < 0.05,
      round(summary(coxph(Surv(time, status) ~ get(x), data = temp))$waldtest[[3]], 3)), TRUE)
})

# to select p-values based on variables that had p <= 0.05
library(dplyr)
library(magrittr)
tmp1 <- tmp %>%
  t(.) %>%
  set_colnames(c("Sign","p"))  %>%
  as.data.frame() %>%
  cbind(rownames(.)) %>%
  set_colnames(c("Sign","p","Varn"))  %>%
  select(p, Varn) %>%
  mutate(p = as.numeric(p)) %>%
  filter(., p <= 0.05)

# vincristine, age are the only two variables that were univariately associated... using coxph

# residual analysis

# here we want to make sure that the assumption of proportional hazard along with linearity of the association between covvariates and survival times are met.

result.0.coxph <- coxph(Surv(time, status) ~ 1, data = temp)
rr.0 <- residuals(result.0.coxph, type = "martingale")
par(mfrow = c(1, 2))
scatter.smooth(temp$vincristine, rr.0, xlab = "Vincristine use", 
               ylab = "martingale resid")
title("Martingale residuals \nversus Vincristine use")
scatter.smooth(temp$age, rr.0, xlab = "age", ylab = "martingale resid")
title("Martingale residuals \nversus age")

# it doesn't seem like the two martingale residual plots indicate a strong trend (aside the fact that towards the higher end of age there seems to be a drop but this may be due to small sample sizes).

par(mfrow = c(2, 2))
result.sch.resid <- cox.zph(model.coxph, transform = "rank")
result.sch.resid

plot(result.sch.resid[1])
plot(result.sch.resid[2])
plot(result.sch.resid[3])

# based on the plot and the p-values the assumption of proportional hazard doesn't seem to be violated. 

# Next steps... now try to use mlr to do some feature selection using the survival models described in their page 
# 