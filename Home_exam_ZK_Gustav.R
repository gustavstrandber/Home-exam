data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")

library(gridExtra) 
library(psych) 
library(tidyverse) 
library(ggplot2)
library(lm.beta)
library(dplyr)

data_sample_gender = data_sample_1 #made a new dataset for mutating sex. See below. 
  
data_sample_gender %>%
  mutate(sex=factor("sex")) #change to factor format to enable analysis with same type of variables. Also to check the levels of the column.

data_sample_gender %>% #summary of data.
  summary()

data_sample_gender %>% #describe data to check if the data is normal or have error. See list below. 
  describe()

#prepare coffiency table.

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	


#check errors in data set by looking at summary:
#1 check sex: OK!
#2 STAI_trait must be 20 - 80: Issue! Min is 4.2
#3 pain must be between 1-10: Issue: Max is 55
#4 pain_cat must be between 0-52: OK!
#6 mindfullness must be 0-6: Ok!
#7 seems like pain and STAI_trait is not normally distributed when looking as skew and curtosis.

#Other issues, but which are not relevant to this particular study.
#error in income? First participant only have 3628 in income, which is a huge diference from the others. Is this an error? 
#outliers in iq? Min is 54 which is very low and max is 146, which is very high... Something to consider for assumption checks.
#about these issues: even though these values are suspicious, we do not know enough about how the data is collected 
#to remove them. Also there could be errors in all of the variables, but if it is not crystal clear that
#there is an error, we should not remove them. It could always be outliers.

#Now, filter data by faulty values.

data_sample_gender %>%
  filter(STAI_trait <20)  #id_34 has faulty data in the STAI_trait variable.

data_sample_gender %>%
  filter(pain >10)      #id_88 has faulty data in pain variable. 

#exclude faulty data by using dplyr's slice function. Also create new object for removing rows, since we want the original set to remain intact.

excluded_data <- data_sample_gender %>% slice(-c(34, 88))
excluded_data

excluded_data %>%
  summary()

excluded_data %>%
  describe()      #it now seems like all of the data is normal, by what we can tell from the descriptives. 

#to answer this research question you will need to conduct a hierarchical regression, building a model containing age and sex as predictors of pain.

model1 = lm(pain ~ sex + age, data = excluded_data)

#then building a newmodel with the predictors: age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures.

model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = excluded_data)

#cooks distance for booth models, to check for outliers.

model1 %>% 
  plot(which = 4) #id 8,23 and 46 seems to be outliers

final_model %>% 	  
  plot(which = 4)	#46, 73 and 85 seems to be outliers

excluded_data %>%
  slice(c(8, 23, 46)) #slice these out to check the data. It seems fine, and does not seem to need exclusion. And also, if we were to exclude them, cooks distance would give us new outliers.

#But we could try to do a new cooks distance check using a new model anyway. 

exclude3 <- excluded_data %>% slice(-c(8, 23, 46))
exclude3

model5 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = exclude3)

model5 %>%
  plot(which = 4) #as expected we had new outliers, so lets keep the excluded_data dataset, since the outliers seems normal. 

#Normality = The residuals of the model must be normally distributed
#(of the residuals) I have choosen to only look at skew and kurtosis. Other methods to use is Q-Q plot or histograms, however, I am not expereienced enough to judge normality from these mehtods and rather trust skwe and kurtosis.

describe(residuals(model1)) #skew 0.1 kurtosis -0.02, looks normal

describe(residuals(final_model)) #skew .0.18 - 0.02, looks normal


#Linearity (of the relationship) = the relationship between the outcome variable and the predictor(s) must be linear

model1 %>%
  residualPlots() #this is fine, since non of variables are significant

final_model %>%
  residualPlots() #this is fine, since no of variables are significant. 

#Homogeneity of variance (also called homoscedasticity) = The variance of the residuals are similar at all values of the predictor(s)

model1 %>%
  plot(which = 3)

model1 %>% #not significant, so that is good p = .10
  ncvTest()

model1 %>% #not signifcant, that is good p = .19
  bptest()

final_model %>%
  plot(which = 3)

final_model %>% #not significant, so that is good p = .79
  ncvTest()

final_model %>% #not signifcant, that is good p = .89
  bptest()


#No excess multicollinearity = None of the predictors can be linearly determined by the other predictor(s) Can't be above three.

model1 %>% # 1 for booth sex and age, so fine
  vif()

final_model%>% #there is an problem where cortisol_serum (5.07) and cortisol_saliva. (4.8)
  vif()

#therefore we create a new model to check out if the two factors are correlated
model3 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = excluded_data)
final_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = excluded_data)

model3 %>% #now it is fine (1.45)
  vif()

final_model %>% #this one aswell (1.37)
  vif()

#it seems like the two factors being highly correlated, which is also is supported in the theoretical framework. 
#also, theory states that cortisol_serum is a better predictor for stress, so lets remove saliva.

#data analysis time! 

#coefficiency table to find p-values, std.beta values and direction/strength of the linear reggression. 

coef_table(model1) #in this model people with younger age will experiece more pain. Beacuse of higher std beta and negative coeffiecient. 
coef_table(final_model) #in this model cortisol_saliva seems to account for the biggest effect size. Due to high std beta and low p.value. 
#Beacuse of positive coefficient, it means higher pain catastrophy, higher pain. 

#equation model 1
# Y = 8.24 + 0.30 * sex -0.09 * age

# equation model 2
#???? = 1.47 - 0.04 * age + 0.16 * sex + 0.01 * STAI_trait + 0.11 * pain_cat -0.28 * mindfulness + 0.57 * coritsol_serum 

#R-squared test to check how much of the variance can be explained by each model:

summary(model1)
summary(final_model)

#use AIC or ANOVA? depends if the models are nested. AIC is the more established way for model comparisson. 

#AIC-if variance is bigger than two, it is a significant difference in the models. The value is bigger than two. Smaller value means less error and better model fit. Lets go for n.02.

AIC(model1) #574
AIC(final_model) #479 = smaller number, better model fit

#use ANOVA if data is nested, but AIC is always better. ANOVA checks residual error and DF. If F-test is signficant, then there is a difference in the two models.
#ANOVA is probably the better choice since it is nested data. 

anova(model1, final_model) #F = *** significant, therefore there is a sigifnicant difference in how good the models are. 


###############################################################################################################################
#                                                         ASIGNMENT 2                                                         #
###############################################################################################################################

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")

library(gridExtra) 
library(tidyverse) 
library(ggplot2)
library(dplyr)
library (titanic)
library (gridExtra)
library (magrittr)
library(lsr)  
library(sciplot)
library(lm.beta)
library(psych)  
library(car) 
library(lmtest) 
library(sandwich)	
library(boot)
library(lmboot)

#load coeffiency table
coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	


data_sample_1 %>%
  mutate(sex=factor("sex"))

summary(data_sample_1)

data_set = data_sample_1 %>% slice(-c(34, 88))
data_set

initial_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_set)

#model diagnostics

#cooks distance

initial_model %>% 
  plot(which = 4) #some outliers

data_set %>%
  slice(c(46,84,85)) #slice the data and assess it. Looks fine though.

#normality
describe(residuals(initial_model)) #skew and curtosis looks fine. S = 0.17. K = 0.08

#linearity
initial_model %>%
  residualPlots() #weight is almost significant, but fine .098

#homogenity
initial_model%>%
  plot(which = 3) #can't intepret, lets use tests

initial_model %>% 
  ncvTest() #not significant, p.87

initial_model %>% #not significant p.87
  bptest()

#multicollinearity
initial_model %>%
  vif() #nothing over 3, so its fine.

#all diagnostics 

#backward regression. Issue with this approach is that we exclude cases, making the model seem better that what it actually is. 

backward_model = step(initial_model, direction = "backward")

#model diagnostics

#cooks distance

backward_model %>% 
  plot(which = 4) 

cooks.distance(backward_model)

data_set %>%
  slice(c(46,102,115))

data_set %>%
  slice(c(46,84,85)) #slice the data and assess it. Looks fine though.

#normality
describe(residuals(backward_model)) #skew and curtosis looks fine. S = -.19. K = .05

#linearity
backward_model %>%
  residualPlots() #fine

#homogenity
backward_model %>%
  plot(which = 3) #can't intepret, lets use tests

backward_model %>% 
  ncvTest() #not significant, p.74

backward_model %>% #not significant p.78
  bptest()

#multicollinearity
backward_model %>%
  vif() #nothing over 3, so its fine.

coef_table(backward_model)

coef_table(initial_model)


#compare models

#model from assignment 1

theory_based_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_set)

summary(initial_model)
summary(theory_based_model)
summary(backward_model)


AIC(initial_model)
AIC(theory_based_model) #479
AIC(backward_model) #476, slightly better fit from the looks of it #but maybe not as grounded in theory

anova(theory_based_model, initial_model, backward_model) #not significant


home_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")

home_sample_2 %>%
  mutate(sex=factor("sex"))

summary(home_sample_2)
describe(home_sample_2) #Sex is a bit of in kurtosis (2) and mean (1.47). This basically tells us that gender is not normally dist. But that is fine. M = 75, F = 85. 

#fit models on new data
predict_backward_model = predict(backward_model, home_sample_2)
predict_theory_model = predict(theory_based_model, home_sample_2)
predict_initial_model = predict(initial_model, home_sample_2)

#to find which is the best, I will asses the errors for each model
rss_theory = sum((home_sample_2[,"pain"]-predict_theory_model)^2)
rss_theory #243.82

rss_backward = sum((home_sample_2[,"pain"]-predict_backward_model)^2)
rss_backward #249.56

rss_initial = sum((home_sample_2[,"pain"]-predict_initial_model)^2)
rss_initial

#better prediction with theory on new sample, since smaller error. 

#theory based is probably better. Read litterature a little bit. 

######################################################################################################################################
#                                                         ASSINGMENT 3                                                               #
######################################################################################################################################

########################################################################################################
#                                         DATA WRANGLING DATASET 3 AND 4                               #
########################################################################################################

data_file_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_3.csv")
data_file_4 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_4.csv")

library(tidyverse)
library(psych)
library(lmerTest) 
library(r2glmm)
library(MuMIn)
library(lme4)
library(cAIC4)

summary(data_file_3) #sex has three levels instead of 2. #min income is negative. 
describe(data_file_3)

data_file_3 = data_file_3 %>% #convert to factor
  mutate(sex = factor(sex))

data_file_3 = data_file_3 %>% #convert to factor
  mutate(hospital = factor(hospital))

data_file_3_alter = data_file_3 #new object

data_file_3_alter = data_file_3_alter%>% #convert woman to female
  mutate(sex = recode(sex, 
                      "woman" = "female"))

data_file_3_alter$household_income[data_file_3_alter$household_income < 0] = 70033.64 #make -income to mean, so participant do not have to be removed

summary(data_file_3_alter) #we good
describe(data_file_3_alter)

data_3 = data_file_3_alter # new model shorter name

summary(data_file_4) #no issues
describe(data_file_4)

data_file_4 = data_file_4 %>% #convert to factor
  mutate(sex = factor(sex))

data_file_4 = data_file_4 %>% #convert to factor
  mutate(hospital = factor(hospital))

data_4 = data_file_4 #shorter name

summary(data_3)

##################################################################################################################
#                                         DATA WRANGLING ASSIGMENT 1                                             #
##################################################################################################################
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")

data_sample_gender = data_sample_1 #made a new dataset for mutating sex. See below. 

data_sample_gender = data_sample_gender %>%
  mutate(sex=factor(sex)) #change to factor format to enable analysis with same type of variables. Also to check the levels of the column.

excluded_data <- data_sample_gender %>% slice(-c(34, 88))

summary(excluded_data)

##################################################################################################################
#                                           PACKAGE AND TABLE LOADING                                            #
##################################################################################################################

#load coeffiency table
coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

#load for std.beta
stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

#########################################################################################################
#                                     NEW MODEL, COMPARE TO THEORY BASED                                #
#########################################################################################################

model_random_intercept = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_3)

summary(model_random_intercept) #coefficients and p-values 
confint(model_random_intercept) #confidence intervals. If value goes from + to -, IT IS NOT SIGNIFICANT. 
stdCoef.merMod (model_random_intercept) #standardized beta

r.squaredGLMM(model_random_intercept) #R2M = 0.385 R2C = 0.463


#########################################################################################################
#                                       COMPARE MODELS                                                  #
#########################################################################################################

final_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = excluded_data)

summary(final_model)

AIC(final_model)
AIC(model_random_intercept)


#########################################################################################################
#                     PREDCIT MODEL ON DATA 4, ALSO HOW MUCH OF VARIANCE IS EXPLAINED                  #
#########################################################################################################

prediction = predict(model_random_intercept, data_4, allow.new.levels = TRUE)

summary(prediction)
describe(prediction)

rss_random_int = sum((data_4[,"pain"]- prediction)^2)
rss_random_int

mod_mean = lm (pain ~ 1, data_4)

tss_random_int <- sum((data_4$pain - predict(mod_mean))^2)
tss_random_int

R2 = 1 - (rss_random_int/tss_random_int)
R2

#########################################################################################################
#                                NEW MODEL WITH MOST INFLUENTAL PREDICTOR                               #                      
#########################################################################################################

stdCoef.merMod(model_random_intercept) #most influential is cortisol_serum, since largest std.beta. 0.34


model_random_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_3)

##########################################################################################################
#                                       VISUALIZE NEW MODEL                                              #
##########################################################################################################

data_3 = data_3 %>% 		
  mutate(pred_slope = predict(model_random_slope))

data_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 2) +		
  geom_line(color='black', aes(y=pred_slope, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)

###########################################################################################################
#                                   COMPARE RANDOM INTERCEPT AND SLOPE                                    #
###########################################################################################################

sum(residuals(model_random_intercept)^2) #random intercept #224.32.LESS MISSTAKE
sum(residuals(model_random_slope)^2) #has random slope and intercept #286.15

cAIC(model_random_intercept)$caic #621.43 #Predict better
cAIC(model_random_slope)$caic #664.54

anova(model_random_intercept, model_random_slope) #p is significant 














