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
library(abind)
#install.packages("abind")
library("psych")

pilot_data= read.csv(file="/Users/sabhabib/research/PSVA/data/main/pilot3/pilot3-raw.csv", stringsAsFactors = FALSE)
filtered=dplyr::select(pilot_data,contains("PID"), contains("condition"), contains("list.audios")
                       , contains("V2"),contains("SV"),contains("PUPENJ"),contains("DIS"),contains("V_AGE")
                       ,contains("EMO"),contains("D1"),contains("D2"),contains("D3")
                       ,contains("D4"),contains("D5"),contains("D6"))
write.csv(filtered,"/Users/sabhabib/research/PSVA/data/main/pilot3/primary-filter.csv", row.names = FALSE)

#Preprocess review questions from list
# review_data dataframe is for separately keeping track of review questions. 
# there will be 6 questions for each participants. attentions checks are checked 
# and removed from data record here. we can use this dataframe later for score calculation


attentive_data<-setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("pid","attention-score"))
review_data<-setNames(data.frame(matrix(ncol = 9, nrow = 0)), c("pid","serial","stimulus","q1","q2","q3","q4"
                                                                ,"q5","q6"))
review_ind<-0

# separating review data with order of q1 to q6 for each participant. Q_serial is for keeping 
# track of question serials/order as the participants came across

for(i in 1:33){
  file_list=unlist(strsplit(toString(filtered[i,3]),","))
  attention=0
  # calculate attention score for reviews. 8 iterations for 8 questions
  for(j in 1:8){
    serial=as.numeric(substring(file_list[j],2))
    if(serial>96){
      if(serial%%2==1){
        if(filtered[i,3+(j-1)*6+2]!=7){
          attention=attention+1
        }
      } else{
        if(filtered[i,3+(j-1)*6+5]!=1){
          attention=attention+1
        }
      }
    }
  }
  # calculate attention scores for supplementary ques
  if(filtered$PSVTRS_3!=7){
    attention=attention+1
  }
  if(filtered$PUPENJ_3!=1){
    attention=attention+1
  }
  if(filtered$VPSVATTDATTRC_6!=7){
    attention=attention+1
  }
  
  
  attentive_data[i,1]<-i
  attentive_data[i,2]<-attention
  if(attention>0){
    next
  }
  q_serial<-0
  for(j in 1:8){
    serial=as.numeric(substring(file_list[j],2))
    if(serial<=96){
      review_ind<-review_ind+1
      review_data[review_ind,3]<-serial
      review_data[review_ind,1]<-i
      q_serial<-q_serial+1
      review_data[review_ind,2]<-q_serial
      for(k in 1:6){
        review_data[review_ind,k+3]<-filtered[i,3+(j-1)*6+k]
      }
    }
  }
}

# write attentive data to a file
write.csv(attentive_data,"/Users/sabhabib/research/PSVA/data/main/pilot3/attentive_data.csv", row.names = FALSE)

##calculate chronbachs alpha
#Review: Perceived usefulness
pu_rv4=review_data$q4
pu_rv5=review_data$q5
pu_rv6=review_data$q6
pu_rv=data.frame(pu_rv4,pu_rv5,pu_rv6)
alpha(pu_rv, check.keys=TRUE)
#VA: Perceived trustworthiness
tr_va1=filtered$PSVTRS_1
tr_va2=filtered$PSVTRS_2
tr_va4=filtered$PSVTRS_4
tr_va5=filtered$PSVTRS_5
tr_va6=filtered$PSVTRS_6
tr_va=data.frame(tr_va1,tr_va2,tr_va4,tr_va5,tr_va6)
alpha(tr_va, check.keys=TRUE)
#VA: Perceived usefulness
pen_va1=filtered$PUPENJ_1
pen_va2=filtered$PUPENJ_2
pen_va4=filtered$PUPENJ_4
pen_va5=filtered$PUPENJ_5
pu_va=data.frame(pen_va1,pen_va2,pen_va4,pen_va5)
alpha(pu_va, check.keys=TRUE)

#VA: Perceived enjoyment
pen_va6=filtered$PUPENJ_6
pen_va7=filtered$PUPENJ_7
pen_va=data.frame(pen_va6,pen_va7)
alpha(pen_va, check.keys=TRUE)

#Voice: Persuasiveness
ps_v1=filtered$VPSVATTDATTRC_1
ps_v2=filtered$VPSVATTDATTRC_2
ps_v=data.frame(ps_v1,ps_v2)
alpha(ps_v, check.keys=TRUE)

#Voice: Attractiveness
att_v4=filtered$VPSVATTDATTRC_4
att_v5=filtered$VPSVATTDATTRC_5
att_v7=filtered$VPSVATTDATTRC_7
att_v=data.frame(att_v4,att_v5,att_v7)
alpha(att_v, check.keys=TRUE)
###
configs_pilot1= read.csv( file="/Users/sabhabib/research/PSVA/data/main/pilot2/file-config-1.csv")

# use later for combining config symbols
configs<-configs_pilot1

# code for preparing summary data
participants<-33
summary_data<-setNames(data.frame(matrix(ncol = 26, nrow =participants)), c("pid","gender", "review_valence","emotion",
                                                                "age","review_purchase_likelihood", "review_trust","review_relevance",
                                                                "review_perceived_usefulness", "va_perceived_trust", "va_persuasiveness",
                                                                "va_perceived_usefulness", "va_perceived_enjoyment", "voice_persuasiveness",
                                                                "voice_attitude_toward", "voice_attractiveness","perceived_emotion","perceived_age"
                                                                ,"perceived_age_difference","disposition_trust","participant_gender"
                                                                ,"participant_age","participant_education","participant_race"
                                                                ,"participant_income","participant_employment"))

for(i in 1:participants){
  condition <- as.numeric(filtered[i,2])
  
  #skip if attention check failed
  if (attentive_data[i,2]!=0){
    next
  }
  # entry pid
  summary_data$pid[i]<-i
  
  # entry config data
  summary_data$gender[i]<-configs$gender[configs$condition==condition]
  summary_data$age[i]<-configs$age[configs$condition==condition]
  summary_data$review_valence[i]<-configs$val[configs$condition==condition]
  summary_data$emotion[i]<-configs$emotion[configs$condition==condition]
  
  # calculate review scores
  ## aggregate scores across six questions
  aggregated_review_data<-aggregate(review_data[,c("q1","q2","q3","q4","q5","q6")],by=list(pid=review_data$pid),FUN=mean)
  summary_data$review_purchase_likelihood[i]<- aggregated_review_data$q1[aggregated_review_data$pid==i]
  summary_data$review_trust[i]<- aggregated_review_data$q2[aggregated_review_data$pid==i]
  summary_data$review_relevance[i]<- aggregated_review_data$q3[aggregated_review_data$pid==i]
  ## calculate review usefulness score
  review_usefulness<-(aggregated_review_data$q4[aggregated_review_data$pid==i]+aggregated_review_data$q5[aggregated_review_data$pid==i]+aggregated_review_data$q6[aggregated_review_data$pid==i])/3
  summary_data$review_perceived_usefulness[i]<- review_usefulness
  
  # calculate voice assistant scores
  summary_data$va_perceived_trust[i]<-(filtered$PSVTRS_1[i]+filtered$PSVTRS_2[i]+filtered$PSVTRS_4[i]+filtered$PSVTRS_5[i]+filtered$PSVTRS_6[i])/5
  summary_data$va_persuasiveness[i]<-filtered$PSVTRS_7[i]
  summary_data$va_perceived_usefulness[i]<-(filtered$PUPENJ_1[i]+filtered$PUPENJ_2[i]+filtered$PUPENJ_4[i]+filtered$PUPENJ_5[i])/4
  summary_data$va_perceived_enjoyment[i]<-(filtered$PUPENJ_6[i]+filtered$PUPENJ_7[i])/2
  
  # calculate voice scores
  summary_data$voice_persuasiveness[i]<-(filtered$VPSVATTDATTRC_1[i]+filtered$VPSVATTDATTRC_2[i])/2
  summary_data$voice_attitude_toward[i]<-filtered$VPSVATTDATTRC_3[i]
  summary_data$voice_attractiveness[i]<-(filtered$VPSVATTDATTRC_4[i]+filtered$VPSVATTDATTRC_5[i]+filtered$VPSVATTDATTRC_7[i])/3
  
  # populate perceived age,emotion, age difference of voice
  summary_data$perceived_emotion[i]<-filtered$V_EMO[i]
  summary_data$perceived_age[i]<-filtered$V_AGE[i]
  summary_data$perceived_age_difference[i]<-filtered$V_AGEDIFF[i]
  
  # populate participant gender,age,education, race, income,employment
  summary_data$participant_gender[i]<-filtered$D1[i]
  summary_data$participant_age[i]<-filtered$D2[i]
  summary_data$participant_education[i]<-filtered$D3[i]
  summary_data$participant_race[i]<-filtered$D4[i]
  summary_data$participant_income[i]<-filtered$D5[i]
  summary_data$participant_employment[i]<-filtered$D6[i]
  
  # calculate disposition to trust
  summary_data$disposition_trust[i]<-(filtered$DIS_1[i]+filtered$DIS_2[i]+filtered$DIS_3[i]+filtered$DIS_5[i])/4
  
  
}
test<-summary_data
# get rid of the NA column
summary_data<-summary_data[!is.na(summary_data$pid),]

#round values to two decimal
numeric_columns <- sapply(summary_data, is.numeric)  # Identify numeric columns

summary_data[numeric_columns] <- lapply(summary_data[numeric_columns], function(x) round(x, 2))

# modify strings of summary data
summary_data$emotion[summary_data$emotion=="p"]<-"positive"
summary_data$emotion[summary_data$emotion=="n"]<-"negative"
summary_data$emotion[summary_data$emotion=="x"]<-"neutral"
summary_data$age[summary_data$age=="m"]<-"Middle Aged Adult"
summary_data$age[summary_data$age=="y"]<-"Younger Adult"
summary_data$review_valence[summary_data$review_valence=="p"]<-"positive"
summary_data$review_valence[summary_data$review_valence=="n"]<-"negative"
summary_data$gender[summary_data$gender=="f"]<-"female"
summary_data$gender[summary_data$gender=="m"]<-"male"
summary_data$participant_gender[summary_data$participant_gender=="1"]<-"Female"
summary_data$participant_gender[summary_data$participant_gender=="2"]<-"Male"
summary_data$participant_gender[summary_data$participant_gender=="3"]<-"Non-binary"
summary_data$participant_gender[summary_data$participant_gender=="4"]<-"Other"
summary_data$participant_education[summary_data$participant_education=="1"]<-"No high school"
summary_data$participant_education[summary_data$participant_education=="2"]<-"High School degree or equivalent"
summary_data$participant_education[summary_data$participant_education=="3"]<-"Some College"
summary_data$participant_education[summary_data$participant_education=="4"]<-"Two-year degree"
summary_data$participant_education[summary_data$participant_education=="5"]<-"Four-year degree"
summary_data$participant_education[summary_data$participant_education=="6"]<-"Graduate degree"
summary_data$participant_race[summary_data$participant_race=="1"]<-"Hispanic or Latino"
summary_data$participant_race[summary_data$participant_race=="2"]<-"American Indian or Alaska Native"
summary_data$participant_race[summary_data$participant_race=="3"]<-"Asian"
summary_data$participant_race[summary_data$participant_race=="4"]<-"Black or African American"
summary_data$participant_race[summary_data$participant_race=="5"]<-"Native Hawaiian or Other Pacific Islander"
summary_data$participant_race[summary_data$participant_race=="6"]<-"White"
summary_data$participant_race[summary_data$participant_race=="7"]<-"Other"
summary_data$participant_employment[summary_data$participant_employment=="1"]<-"Full time"
summary_data$participant_employment[summary_data$participant_employment=="2"]<-"Part time"
summary_data$participant_employment[summary_data$participant_employment=="3"]<-"Unemployed- looking for work"
summary_data$participant_employment[summary_data$participant_employment=="4"]<-"Unemployed- not looking for work"
summary_data$participant_employment[summary_data$participant_employment=="5"]<-"Retired"
summary_data$participant_employment[summary_data$participant_employment=="6"]<-"Student"
summary_data$participant_employment[summary_data$participant_employment=="7"]<-"Other"
summary_data$participant_income[summary_data$participant_income=="1"]<-"Below 25,000"
summary_data$participant_income[summary_data$participant_income=="2"]<-"25,000-49,999"
summary_data$participant_income[summary_data$participant_income=="3"]<-"50,000-99,999"
summary_data$participant_income[summary_data$participant_income=="4"]<-"100,000-199,999"
summary_data$participant_income[summary_data$participant_income=="5"]<-"200,000 or above"

# write to csv
write.csv(summary_data,"/Users/sabhabib/research/PSVA/data/main/pilot3/summary_data_participant.csv", row.names = FALSE)


