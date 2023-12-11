rm(list = ls())
library(dplyr)
library(lme4)
library(lmerTest)
library(interactions)
library(sjPlot)
library(sjmisc)
library(rcompanion)
library(emmeans)
library(car)
library(ggplot2)
library(effectsize)
library(multcomp)
library(mgcv)
#install.packages("Hmisc")
library("Hmisc")
#install.packages("lavaan")
library("lavaan")
library(corrplot)
#library(tidyverse)
#library(lmSubsets)
theme_set(theme_sjplot())

psva_data= read.csv( file="/Users/sabhabib/research/PSVA/data/main/final2/summary_data_participant_v5.csv",stringsAsFactors = FALSE)


# sem using lavaan
m1<- '
review_trust ~ a*gender
review_purchase_likelihood ~ b*review_trust
ind1 := a*b
'

f1<- sem(m1, data=psva_data, std.lv=T, se="boot", bootstrap=100)
summary(f1,fit=T,standardized=T, rsquare=T)

parameterEstimates(f1)
