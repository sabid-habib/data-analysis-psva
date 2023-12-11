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
#install.packages("corrplot")
library(corrplot)
#library(tidyverse)
#library(lmSubsets)
theme_set(theme_sjplot())

psva_data= read.csv( file="/Users/sabhabib/research/PSVA/data/main/final2/summary_data_participant_v5.csv",stringsAsFactors = FALSE)


# linear regression as each pid has only one column
model_lm<-lm(va_perceived_trust~gender+review_valence+emotion+age+review_valence*gender
             +review_valence*emotion+emotion*age+age*gender+age*participant_age+gender*participant_age+age*participant_gender+review_valence*age+emotion*gender
             , data=psva_data, REML= FALSE)
anova(model_lm)
####################Residual vs fitted plot###################
residuals_plot <- ggplot(data = psva_data, aes(x = fitted(model_lm), y = resid(model_lm))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()


print(residuals_plot)

####################Residual vs fitted plot (method 2)###################
res <- resid(model_lm)
plot(density(res)) # plot density to see if normally distributed
#produce residual vs. fitted plot
plot(fitted(model_lm), res)

#add a horizontal line at 0 
abline(0,0)
#############################Breusch-Pegan test#################################
# Perform Breusch-Pagan test to check for homoscedasticity

bp_test <- bptest(model_lm)
print(bp_test)
##################################Shapiro-wilk test##########################
shap<-shapiro.test(residuals(model_lm))
shap
plot(qqnorm(residuals(model_lm)), qqline(residuals(model_lm)))

library(nortest)
ad.test(residuals(model_lm))

################################Q-Q plot##################################
ggplot() +
  geom_qq(aes(sample = residuals(model_lm))) +
  geom_abline(color = "red") +
  coord_fixed()


################## Functions for the graphs ###########################
interaction_graph <- function(model,data, target, response, xlab, ylab, shape_label){
  t=emmip(model, as.formula(paste(target,"~",response)),CIs=TRUE, CIarg = list(lwd = 0.75, alpha = 0.75), cov.reduce = FALSE,plotit = FALSE)
  ggplot(t, aes(x=t[[response]], y=yvar, group=t[[target]], shape=t[[target]])) + 
    geom_errorbar(aes(ymin=LCL, ymax=UCL,linetype=NULL), colour="black", width=.2,position=position_dodge(0.2)) +
    #geom_line() + #removed it because for categorical variable I don't need lines
    geom_point(size=6,position=position_dodge(0.18)) +
    labs(x=xlab, y=ylab,
         linetype="Voice Genders")+
    #scale_linetype_manual(values=c("solid", "longdash", "dotted"))+
    scale_colour_discrete("Content types") +
    guides(color=guide_legend("Content Types"),
           shape = guide_legend(title = shape_label)) +
    font_size(axis_title.x=19, axis_title.y=19, labels.x=18, labels.y=18,offset.x=-1,offset.y=2, base.theme = theme_blank()) +
    theme(axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1)
          , legend.text=element_text(size=18),
          legend.title=element_text(size=19))
}

main_effect_graph <- function(model,data, effect, xlab, ylab) {
  f<-emmip(model, as.formula(paste("~",effect)),CIs=TRUE,xlab=xlab, ylab= ylab , CIarg = list(lwd = 0.75, alpha = 0.75), cov.reduce = FALSE,plotit=FALSE)
  ggplot(f, aes(x=f[[effect]], y=yvar, group=1)) + 
    geom_errorbar(aes(ymin=LCL, ymax=UCL,linetype=NULL), colour="black", width=.2,position=position_dodge(0.1)) +
    geom_point(size=3) +
    labs(x=xlab, y=ylab)+
    scale_colour_discrete("Content types") +
    font_size(axis_title.x=19, axis_title.y=19, labels.x=18, labels.y=18,offset.x=-1,offset.y=2, base.theme = theme_blank())+
    theme(axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1)
          , legend.text=element_text(size=18),
          legend.title=element_text(size=19))
}


#############################################VA Perceived Enjoyment###########################################
# linear regression as each pid has only one column
model_lm<-lm(va_perceived_enjoyment~gender+emotion+review_valence+age
             +gender*age
             , data=psva_data, REML= FALSE)
#model_lm<-lm(voice_persuasiveness~disposition_trust+review_valence+emotion+gender+participant_gender+gender*participant_gender+age+participant_age+age*gender+age*participant_age+emotion*gender+emotion*age             , data=psva_data, REML= FALSE)
anova(model_lm)
eta_squared(model_lm)
summary(model_lm)
plot_model(model_lm, type = "pred", terms = c( "age","participant_gender"))

############posthoc##############

# posthoc pairwise test
em2=emmeans(model_lm, ~gender*age,lmer.df = "satterthwaite")
summary(em2)
d<-summary(pairs(em2))
pairs(em2)
#calculate effects in addition to model results
d$effect=d$estimate/sigmaHat(model_lm)


################ Generate graph for main effect #################
main_effect_graph(model_lm,psva_data,"emotion", "Voice Emotion Tones","VA Enjoyment (emmeans)")

################ Generate graph for interaction effect #################
interaction_graph(model_lm,psva_data,"age","gender", "Voice Gender","Voice Perceived Attractiveness (emmeans)", "Age Group")


