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
#install.packages("Hmisc")
library("Hmisc")
#install.packages("corrplot")
library(corrplot)
#library(tidyverse)
#library(lmSubsets)
theme_set(theme_sjplot())

psva_data= read.csv( file="/Users/sabhabib/research/PSVA/data/main/pilot3/summary_data.csv",stringsAsFactors = FALSE)


# linear regression as each pid has only one column
model_lm<-lm(voice_attractiveness~gender+review_valence+emotion+age+review_valence*gender
             +review_valence*emotion+emotion*age+age*gender+review_valence*age+emotion*gender
               , data=psva_data, REML= FALSE)
anova(model_lm)
plot_model(model_lm, type = "pred", terms = c( "review_valence","emotion","gender"))

#emmip graph generation
emmip(model_lm,correct~condition,CIs=TRUE,xlab="Norming Accuracy", ylab= "Agree Scores (em means)" , CIarg = list(lwd = 0.75, alpha = 0.75), cov.reduce = FALSE)+font_size(axis_title.x=14, axis_title.y=14, labels.x=14, labels.y=14,offset.x=-1,offset.y=2)
f<-emmip(model_lm, appearance~difficulty ,CIs=TRUE,xlab="Viewing Platform Condition", ylab= "Agreement (em means)" , CIarg = list(lwd = 0.75, alpha = 0.75), cov.reduce = FALSE,plotit=FALSE)
ggplot(f, aes(x=difficulty, y=yvar, group= appearance, shape=appearance)) + 
  geom_errorbar(aes(ymin=LCL, ymax=UCL), colour="black", width=.2,position=position_dodge(0.2)) +
  #geom_line() +
  geom_point(size=3,position=position_dodge(width=0.2)) +
  labs(x="Viewing Platform Condition", y="Agreement Score (em means)")+
  font_size(axis_title.x=14, axis_title.y=14, labels.x=14, labels.y=14,offset.x=-1,offset.y=2) 


