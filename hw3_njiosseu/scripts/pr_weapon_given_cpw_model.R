#HW 3 Ludovic Njiosseu
#B1
#B2
#B2.1 Read the data from data/sqf_08_16.csv, and restrict to stops where the suspected crime is ‘cpw’.

require(tidyverse)
require(ROCR)

#getwd()
sqf_08_16 <- read_csv("data/sqf_08_16.csv")

#B2.2
sqf_08_16_B2 <- sqf_08_16 %>%
  filter(suspected.crime == "cpw") %>%
  filter(year == 2008) %>%
  mutate(precinct = factor(precinct),
         location.housing = factor(location.housing), 
         suspect.build = factor(suspect.build, levels = c("thin", "medium", "muscular", "heavy", "unknown")),
         suspect.sex = factor(suspect.sex),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
         month = factor(month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October",
                                          "November", "December"))
  )

summary(sqf_08_16_B2)
table(sqf_08_16_B2$location.housing)

#Train a logistic regression model
#real-valued attributes standardized by subtracting the mean and dividing by the standard deviation
B2 <- glm(found.weapon ~ 1 + precinct + location.housing + additional.report + additional.investigation + additional.proximity + 
            additional.evasive + additional.associating + additional.direction + additional.highcrime + additional.time +
            additional.sights + additional.other + stopped.bc.object + stopped.bc.desc + stopped.bc.casing + stopped.bc.lookout +
            stopped.bc.clothing + stopped.bc.drugs + stopped.bc.furtive + stopped.bc.violent + stopped.bc.bulge + stopped.bc.other + 
            scale(suspect.age) + suspect.build + suspect.sex + scale(suspect.height) + scale(suspect.weight) + inside + radio.run + 
            scale(observation.period) + day + month + time.period, 
          family = binomial, data = sqf_08_16_B2)
summary(B2)

#B3
#Store the ten largest and ten smallest coefficient names and estimates in a dataframe with two columns: 
#coefficient_name and coefficient_estimate. Order the rows of this dataframe alphabetically according to 
#coefficient_name. Save this dataframe to data/question_b2_coefficients.csv within your submission directory.

question_b2_coefficients <- B2$coefficients %>%
  enframe() %>%
  rename(coefficient_name = name, coefficient_estimate = value) %>%
  filter(!is.na(coefficient_estimate)) %>%
  filter(coefficient_name != "(Intercept)") %>% #Count NAs? Count Intercept? What do you think, Sameen?
  arrange(coefficient_estimate) %>% 
  slice(1:10, (n()-9):n()) %>%
  arrange(coefficient_name)

write.csv(question_b2_coefficients, "hw3_njiosseu/data/question_b2_coefficients.csv", row.names = F)

#B3.1
#If your model were used to predict the ex-ante probability that this person were carrying a weapon, 
#what would this probability be? What if this person were a woman, everything else being equal? 
#Report both of these numbers in your writeup.
#https://www.icpsr.umich.edu/icpsrweb/NACJD/studies/21660/variables?q=
#precinct +                             6
#location.housing +                     transit
#additional.report
#additional.investigation + 
#additional.proximity + 
#additional.evasive + 
#additional.associating + 
#additional.direction + 
#additional.highcrime +                 he was near a part of the station known for having a high incidence of weapon offenses
#additional.time +
#additional.sights
#additional.other + 
#stopped.bc.object
#stopped.bc.desc + 
#stopped.bc.casing + 
#stopped.bc.lookout +
#stopped.bc.clothing + 
#stopped.bc.drugs + 
#stopped.bc.furtive + 
#stopped.bc.violent + 
#stopped.bc.bulge                       was stopped because he had a suspicious bulge in his coat
#stopped.bc.other + 
#scale(suspect.age)                     30 #Convert to z score
#suspect.build                          medium
#suspect.sex +                          male
#scale(suspect.height) +                6 #Convert to z score
#scale(suspect.weight) +                165 #Convert to z score
#inside +                               inside
#radio.run +                            FALSE
#scale(observation.period) +            10 #Convert to z score
#day +                                  "Saturday"
#month +                                "October"
#time.period                            6 (NOT SCALED)

#timetest <- sqf_08_16_B2 %>%
#  filter(time.period >=5) %>%
#  select(timestamp, time.period)

#precincttest <- sqf_08_16_B2 %>%
# filter(location.housing == "transit") %>%
#  select(precinct, location.housing)

#table(sqf_08_16_B2$inside)
#Do we need to do anything to scale the numeric variables?
newdata = data.frame(precinct = factor(6), location.housing = "transit", additional.report = FALSE, additional.investigation = FALSE, additional.proximity = FALSE, 
                     additional.evasive = FALSE, additional.associating = FALSE, additional.direction = FALSE, additional.highcrime = TRUE, 
                     additional.time = FALSE, additional.sights = FALSE, additional.other = FALSE, stopped.bc.object = FALSE, stopped.bc.desc = FALSE, 
                     stopped.bc.casing = FALSE, stopped.bc.lookout = FALSE, stopped.bc.clothing = FALSE, stopped.bc.drugs = FALSE, stopped.bc.furtive = FALSE, 
                     stopped.bc.violent = FALSE, stopped.bc.bulge = TRUE, stopped.bc.other = FALSE, suspect.age =scale(30, mean(sqf_08_16_B2$suspect.age,na.rm = TRUE),sd(sqf_08_16_B2$suspect.age,na.rm= TRUE)), suspect.build = "medium", suspect.sex = "male",
                     suspect.height= scale(6, mean(sqf_08_16_B2$suspect.height,na.rm = TRUE),sd(sqf_08_16_B2$suspect.height,na.rm= TRUE)), suspect.weight = scale(165, mean(sqf_08_16_B2$suspect.weight,na.rm = TRUE),sd(sqf_08_16_B2$suspect.weight,na.rm= TRUE)),inside = TRUE, radio.run = FALSE,
                     observation.period = scale(10, mean(sqf_08_16_B2$observation.period,na.rm = TRUE),sd(sqf_08_16_B2$observation.period,na.rm= TRUE)), day = "Saturday", month = "October", 
                     time.period = 6)
#str(newdata)
predict(B2, newdata, type="response")

#table(sqf_08_16_B2$suspect.sex)

newdata_woman = data.frame(precinct = factor(6), location.housing = "transit", additional.report = FALSE, additional.investigation = FALSE, additional.proximity = FALSE, 
                           additional.evasive = FALSE, additional.associating = FALSE, additional.direction = FALSE, additional.highcrime = TRUE, 
                           additional.time = FALSE, additional.sights = FALSE, additional.other = FALSE, stopped.bc.object = FALSE, stopped.bc.desc = FALSE, 
                           stopped.bc.casing = FALSE, stopped.bc.lookout = FALSE, stopped.bc.clothing = FALSE, stopped.bc.drugs = FALSE, stopped.bc.furtive = FALSE, 
                           stopped.bc.violent = FALSE, stopped.bc.bulge = TRUE, stopped.bc.other = FALSE, suspect.age =scale(30, mean(sqf_08_16_B2$suspect.age,na.rm = TRUE),sd(sqf_08_16_B2$suspect.age,na.rm= TRUE)), suspect.build = "medium", suspect.sex = "female",
                           suspect.height= scale(6, mean(sqf_08_16_B2$suspect.height,na.rm = TRUE),sd(sqf_08_16_B2$suspect.height,na.rm= TRUE)), suspect.weight = scale(165, mean(sqf_08_16_B2$suspect.weight,na.rm = TRUE),sd(sqf_08_16_B2$suspect.weight,na.rm= TRUE)),inside = TRUE, radio.run = FALSE,
                           observation.period = scale(10, mean(sqf_08_16_B2$observation.period,na.rm = TRUE),sd(sqf_08_16_B2$observation.period,na.rm= TRUE)), day = "Saturday", month = "October", 
                           time.period = 6)
predict(B2, newdata_woman, type="response")

#B3.2 Compute the AUC of this model on all data from 2009, using the ROCR package (as in lecture). Report this number in your writeup.
sqf_08_16_B3 <- sqf_08_16 %>%
  filter(suspected.crime == "cpw") %>%
  filter(year == 2009) %>%
  mutate(precinct = factor(precinct),
         location.housing = factor(location.housing), 
         suspect.build = factor(suspect.build, levels = c("thin", "medium", "muscular", "heavy", "unknown")),
         suspect.sex = factor(suspect.sex),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
         month = factor(month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October",
                                          "November", "December"))
  )

pred_B3 <- predict(B2, sqf_08_16_B3, type = "response")
summary(pred_B3)

#prediction method from ROCR
#(probabilities, targets)
ROCR_B3 <- prediction(pred_B3, sqf_08_16_B3$found.weapon)
auc_ROCR_B3 <- performance(ROCR_B3, measure = "auc")

#roc_ROCR_B3 <- performance(ROCR_B3, measure = "tpr", x.measure = "fpr")
#plot(roc_ROCR_B3, main = "ROC curve", colorize = F)
#abline(a = 0, b = 1)


#B3.3
#sampling (with replacement) 10,000 random pairs of true (weapon is found) and false (weapon is not found) 
#examples from 2009, and computing the proportion of pairs where your model predicts that the true example is
#more likely to find a weapon than the false example. Confirm that your answer is approximately equal
#to the answer computed in Question B3.2, and report this number in your write up.
#B3.3 proportions of true vs false examples

weapons<-filter(sqf_08_16_B3, found.weapon==TRUE)
no_weapons<-filter(sqf_08_16_B3, found.weapon==FALSE)

set.seed(1234)
weapons.found<-sample_n(weapons,10000, replace = TRUE)
weapons.notfound<-sample_n(no_weapons,10000,replace = TRUE)

#run the model
weapons.found$predicted<-predict(B2, weapons.found, type = "response")
weapons.notfound$predicted2<-predict(B2, weapons.notfound, type = "response")

#higher probability for true case
sum(ans<-ifelse(weapons.found$predicted>weapons.notfound$predicted2,1,0), na.rm=TRUE)/10000




