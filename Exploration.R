library(knitr) #for creating nicer tables
library(tidyverse) # for various packages
library(dplyr) # FOR PIPING
library(readr) #for read txt file
library(nnet) #Multinomial logistic regression
library(MASS) #Ordinal logistic regression

#LOAD FULL DATA
load("34891-0001-Data.rda")
data_full<-read.csv("project_data.csv")

#SELECT VARIABLES FOR HONESTY, TRUST AND RISK OF TERRORISM
data_short<- subset(da34891.0001,select = c(1:2,124:137,
                                       167,170,254:264,
                                       277:278,283:284,
                                       289:290,301:303,
                                       341,343,359:360,
                                       399))  
#ATTACH THE DATASET
attach(data_short)

#CALCULATE WHETHER SOMEONE HAS SEEN SOMEONE ELSE EVER DO EMERGENCY MANAGEMENT
seen_sm1 <- sapply(data_short[3:16], as.numeric)
seen_true<-apply(seen_sm1, 1, function(r) any(r == 1))
data_short$saw_sm1<-seen_true

#REMOVE CREATION DATA AND VARIABLES
rm(seen_true, seen_sm1, da34891.0001)
data_short<-data_short[ -c(3:16)]

#CREATE VARIABLES FOR ALL, FED, STATE, LOCAL HONESTY WITH PUBLIC
all_vars<- sapply(data_short[5:15], as.numeric)
data_short$all_honest<-rowMeans(all_vars,na.rm=TRUE)

state_vars<- sapply(data_short[5:7], as.numeric)
data_short$state_honest<-rowMeans(state_vars,na.rm=TRUE)

local_vars<- sapply(data_short[8:11], as.numeric)
data_short$local_honest<-rowMeans(local_vars,na.rm=TRUE)

fed_vars<- sapply(data_short[12:15], as.numeric)
data_short$fed_honest<-rowMeans(fed_vars,na.rm=TRUE)

#REMOVE VARIABLES USED FOR CREATION 
rm(all_vars,state_vars,local_vars,fed_vars)
data_short<-data_short[ -c(5:15)]

#CONVERT RISK VARIABLES TO NUMERIC
data_short$Q7A_2<-as.numeric(Q7A_2)
data_short$Q7B_2<-as.numeric(Q7B_2)
data_short$Q7C_2<-as.numeric(Q7C_2)

#CREATE AN OVERALL PROTECTION SCORE
all_protect<- sapply(data_short[13:15], as.numeric)
data_short$all_protect<-rowMeans(all_protect,na.rm=TRUE)

#REMOVE VECTOR
rm(all_protect)


#CREATE BINARY VARIABLES FOR THINKING AT RISK OF TERRORISM, RENAME VARIABLES FOR LOCAL,
#FED, STATE PROTECTION, WHETHER GOT INFO FROM OFFICIAL SOURCES, EDUCATION, RACE
data_short <- data_short %>%
  mutate( terr_6mo=case_when(Q7A_2 == 4 | Q7A_2 == 5 ~ 1,
                             Q7A_2 == 1 | Q7A_2 == 2 ~ 0),
          terr_life=case_when(Q7B_2 == 4 | Q7B_2 == 5 ~ 1,
                              Q7B_2 == 1 | Q7B_2 == 2 | Q7B_2 == 3 ~ 0),
          terr_srs=case_when(Q7C_2 == 4 | Q7C_2 == 5 ~ 1,
                             Q7C_2 == 1 | Q7C_2 == 2 ~ 0),
          local_protect=Q9A_2,
          state_protect=Q9A_3,
          fed_protect=Q9A_4,
          off_info=Q4B,
          believe=Q4E,
          education=Q16,
          race=Q18
          )

#DROP VARIABLES USED TO CALCULATE RISK
data_short<-data_short[ -c(2:15)]

#ATTACH THE DATASET
attach(data_short)

#CONVERT PROTECTION AND HONESTY TO FACTORS
data_short$all_protect_f<-factor(round(data_short$all_protect), 
                                 levels=c(1,2,3,4,5),
                                 labels=c("1","2","3","4","5"))

data_short$all_honest_f<-factor(round(all_honest), 
                                   levels=c(1,2,3,4,5),
                                   labels=c("1","2","3","4","5"))

data_short$local_honest_f<-factor(round(data_short$local_honest), 
                                 levels=c(1,2,3,4,5),
                                 labels=c("1","2","3","4","5"))

data_short$state_honest_f<-factor(round(data_short$state_honest), 
                                 levels=c(1,2,3,4,5),
                                 labels=c("1","2","3","4","5"))

data_short$fed_honest_f<-factor(round(data_short$fed_honest), 
                                 levels=c(1,2,3,4,5),
                                 labels=c("1","2","3","4","5"))

#OLD CODE FOR MAKING FACTORS
#factor_vars <- data_short[c(6:9,13:15,20)]
#data_short[,factor_vars] <-factor(round(data_short[,factor_vars]), 
                               #  levels=c(1,2,3,4,5),
                                # labels=c("1","2","3","4","5"))
#data_short[,factor_vars] <- lapply(data_short[,factor_vars] , factor)

str(data_short)

#SAVE DATA TO CSV FILE
write.csv(data_short, file = "project_data.csv")

attach(data_short)

data_short <- data_short %>%
  mutate( all_protect_bin=case_when(all_protect_f == 4 | all_protect_f == 5 ~ 1,
                                    all_protect_f == 1 | all_protect_f == 2 | all_protect_f ==3 ~ 0),
          all_honest_bin=case_when(all_honest_f == 4 | all_honest_f == 5 ~ 1,
                                   all_honest_f == 1 | all_honest_f == 2 | all_protect_f ==3 ~ 0),
  )

data_short <- data_short %>%
  mutate( local_protect_bin=case_when(local_protect == "(4) 4" | local_protect == "(5) Extremely sure" ~ 1,
                                      local_protect == "(1) Not at all sure" | local_protect == "(2) 2" | 
                                      local_protect == "(3) 3" ~ 0),
          local_honest_bin=case_when(local_honest_f == 4 | local_honest_f == 5 ~ 1,
                                     local_honest_f == 1 | local_honest_f == 2 | local_honest_f ==3 ~ 0),
          state_protect_bin=case_when(state_protect == "(4) 4" | state_protect == "(5) Extremely sure" ~ 1,
                                      state_protect == "(1) Not at all sure" | state_protect == "(2) 2" | 
                                      state_protect == "(3) 3" ~ 0),
          state_honest_bin=case_when(state_honest_f == 4 | state_honest_f == 5 ~ 1,
                                     state_honest_f == 1 | state_honest_f == 2 | state_honest_f ==3 ~ 0),
          fed_protect_bin=case_when(fed_protect == "(4) 4" | fed_protect == "(5) Extremely sure" ~ 1,
                                    fed_protect == "(1) Not at all sure" | fed_protect == "(2) 2" | 
                                    fed_protect == "(3) 3" ~ 0),
          fed_honest_bin=case_when(fed_honest_f == 4 | fed_honest_f == 5 ~ 1,
                                   fed_honest_f == 1 | fed_honest_f == 2 | fed_honest_f ==3 ~ 0)
  )

data_short <- data_short [-c(42:47)]

#RUN PRELIM MODEL FOR PROTECTION PREDICTED BY HONESTY
mod_all <- polr(all_protect_f ~ all_honest_f,Hess=TRUE) 
summary(mod_all)

attach(data_short)

#LOGISTIC REGRESSION MODELS

#ALL MODEL
#unadjusted
all_model <- glm(all_protect_bin ~ all_honest_bin, family = binomial)
summary(all_model)

exp(cbind("Odds ratio" = coef(all_model), confint.default(all_model, level = 0.95)))

#adjusted
all_model_adj <- glm(all_protect_bin ~ all_honest_bin + COMB_AGE + education 
                     + believe + terr_life, 
                     family = binomial)
summary(all_model_adj)

exp(cbind("Odds ratio" = coef(all_model_adj), confint.default(all_model_adj, level = 0.95)))


#LOCAL MODEL
#unadjusted
local_model <- glm(local_protect_bin ~ local_honest_bin, family = binomial)

exp(cbind("Odds ratio" = coef(local_model), confint.default(local_model, level = 0.95)))

#adjusted
local_model_adj <- glm(local_protect_bin ~ local_honest_bin + COMB_AGE + education 
                     + believe + terr_life, 
                     family = binomial)

exp(cbind("Odds ratio" = coef(local_model_adj), confint.default(local_model_adj, level = 0.95)))


#STATE MODEL
#unadjusted
state_model <- glm(state_protect_bin ~ state_honest_bin, family = binomial)

exp(cbind("Odds ratio" = coef(state_model), confint.default(state_model, level = 0.95)))

#adjusted
state_model_adj <- glm(state_protect_bin ~ state_honest_bin + COMB_AGE + education 
                       + believe + terr_life, 
                       family = binomial)

exp(cbind("Odds ratio" = coef(state_model_adj), confint.default(state_model_adj, level = 0.95)))


#FEDERAL MODEL
#unadjusted
fed_model <- glm(fed_protect_bin ~ fed_honest_bin, family = binomial)

exp(cbind("Odds ratio" = coef(fed_model), confint.default(fed_model, level = 0.95)))

#adjusted
fed_model_adj <- glm(fed_protect_bin ~ fed_honest_bin + COMB_AGE + education 
                       + believe + terr_life, 
                       family = binomial)
summary(fed_model_adj)

exp(cbind("Odds ratio" = coef(fed_model_adj), confint.default(fed_model_adj, level = 0.95)))


#ODDS RATIOS FOR OVERALL MODEL
exp(cbind("Odds ratio" = coef(mod_all), confint.default(mod_all, level = 0.95)))

#THE ODDS OF TRUSTING THE GOVERNMENT CAN PROTECT YOU FROM TERRORISM (4,5) ARE 21.3 TIMES 
#THE ODDS OF NOT TRUSTING THE GOVERNMENT CAN PROTECT YOU FROM TERRORISM (1,2,3) AMONG PEOPLE WHO
#THINK THE GOVERNMENT IS HONEST WITH THE PUBLIC ABOUT TERRORISM (4,5) VS. THOSE WHO DO 
#NOT BELIEVE THAT THEY ARE HONEST WITH THE PUBLIC (1,2,3).

#RUN LOCAL MODEL FOR PROTECTION PREDICTED BY HONESTY
mod_local <- polr(local_protect_f ~ local_honest_f,Hess=TRUE) 
summary(mod_local)

str(mod_all)

mod_all_adj <- polr(local_protect_f ~ local_honest_f + comb_age + education 
                    + believe + terr_life, Hess=TRUE) 
summary(mod_local)

str(mod_all)

attach(data_full)
