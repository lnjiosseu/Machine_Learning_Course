#MDML HW 3 Ludovic Njiosseu

#Clearing the environment 
rm(list = ls())

require(tidyverse)
require(ggplot2)
require(grid)
require(gridExtra)
getwd()

#A1
#First, read the data from data/poll_data.tsv, and call the resulting tibble poll_data.
poll_data <- read_tsv("data/poll_data.tsv")

#table(poll_data$vote_2008, useNA = "ifany")

#4. Convert vote_2008 into a factor, making "john mcCain" the reference category.
poll_data <- poll_data %>%
  mutate(vote_2008 = factor(vote_2008, levels = c("john mcCain", "barack obama")),
         state = factor(state),
         sex = factor(sex),
         race = factor(race),
         age = factor(age, levels = c("18-29", "30-44", "45-64", "65+")),
         education = factor(education, levels = c("didn't graduate from HS", "high school graduate", "some college", "college graduate")),
         party = factor(party),
         ideology = factor(ideology),
         state_contestedness = factor(state_contestedness))

#table(poll_data$state_contestedness)

#A2
#Explore data
#summary(poll_data)
#table(poll_data$state_contestedness, poll_data$vote_2008)
#table(poll_data$state_contestedness, poll_data$state)
#table(poll_data$state_contestedness, poll_data$ideology)

#john mcCain :4281, barack obama:5719, class bias shouldn't be too bad
#Change all other variables to factors as well

#A2.1
#Fit a binary logistic regression model that estimates individuals’ probabilities of voting for Obama
#in the 2008 presidential election, using all the other features in the dataset.
#Should we use test vs training datasets?
#Intercept? Yes, mentioned in A2.2
#Binary logistic regression model
A2 <- glm(vote_2008 ~ 1 + state + sex + race + age + education + party + ideology + state_contestedness, 
          family = binomial, data = poll_data)
summary(A2)

#A2.NoInt <- glm(vote_2008 ~ state + sex + race + age + education + party + ideology + state_contestedness, 
#          family = binomial, data = poll_data)
#  summary(A2.NoInt)

A2_pred <- predict(A2, type = 'response')
head(A2_pred)
summary(A2_pred)
#predicted <- plogis(predict(A2, poll_data, type="response"))  # predicted scores per person, converted into prediction probability scores bound between 0 and 1

#A2.2 Store the coefficient names and the estimates (including the intercept) in a tibble with two columns: 
#coefficient_name and coefficient_estimate. Order the rows of this tibble alphabetically according to 
#coefficient_name and save it to data/question_a2_coefficients.csv within your submission directory.  
#str(A2)
#A2$coefficients
#question_a2_coefficients <- summary(A2)$coefficients %>% select()
#colnames(question_a2_coefficients)

#Make the dataframe with names for the variables at all levels as a column
question_a2_coefficients <- A2$coefficients %>%
  enframe() %>%
  rename(coefficient_name = name, coefficient_estimate = value) %>%
  arrange(coefficient_name)

write.csv(question_a2_coefficients, "hw3_njiosseu/data/question_a2_coefficients.csv", row.names = F)

#A2.3
#Create a tibble with 3 columns: variable, number_of_levels, number_of_fitted_coefficients. 
#Store the 8 predictor variable names into the variable column, the number of unique values each variable takes 
#on in the corresponding row in the number_of_levels column, and the number of fitted coefficients for that 
#variable in the number_of_fitted_coefficients column. 
#Save this tibble to data/question_a2_levels_vs_coefficients.csv within your submission directory.

str(A2)
A2$model
str(A2$xlevels)

#Get the number of levels per (factor type) variable
#question_a2_levels <- cbind(rownames(A2$xlevels), data.frame(summary(A2)$xlevels, row.names = NULL))
question_a2_levels <- A2$xlevels %>% 
  enframe(name = "variable", value = "level") %>% 
  unnest() %>% #pulls out each level
  group_by(variable) %>%
  summarize(number_of_levels = n())

#Adjusting names to merge the dataframes in the next step
#See all the factor levels  
allCombos <- A2$xlevels %>% 
  enframe(name = "variable", value = "level") %>% #pull out levels into data frame
  unnest() %>% 
  unite("comparisonLevels", variable:level, sep = "", remove = F) #unite the variable and level names
#See all the coefficients   
coefficients <- A2$coefficients %>%
  enframe(name = "variable", value = "coefficient") %>%
  unnest()
#Combine all the factor levels with all the coefficients that the model made  
allCombos <- left_join(allCombos, coefficients, by = c("comparisonLevels" = "variable")) #Join the tables to see which levels per factor actually have coefficients

#Count the coefficients
allCombos2 <- allCombos %>%
  group_by(variable) %>%
  summarize(number_of_fitted_coefficients = sum(!is.na(coefficient)))

#add coefficient count to the question_a2_levels df
question_a2_levels_vs_coefficients <- left_join(question_a2_levels, allCombos2, by = c("variable"))

write.csv(question_a2_levels_vs_coefficients, "hw3_njiosseu/data/question_a2_levels_vs_coefficients.csv", row.names = F)

#A3
#A3.1 Use the logistic regression model from Question A2 to calculate predicted probabilities of voting
#for Obama in 2008 for all of the individuals in the dataset. 
#Save these probabilities as a column called predicted_probability.

#summary(A2$fitted.values)
predicted_probability <- A2$fitted.values
#length(unique(A2$fitted.values))
#length(A2$fitted.values)
summary(predicted_probability) #These are the same
summary(A2_pred)


poll_data2 <- poll_data
poll_data2$predicted_probability <- A2$fitted.values

#vote_test <- poll_data2 %>%
#  group_by(vote_2008) %>%
#  summarize(meanValue = mean(fitted.values))


#A3.2
#Convert the probabilistic predictions for each individual into binary predictions based on the candidate they 
#are most likely to vote for. Save these binary predictions as a column called predictions_point_5.
poll_data2 <- poll_data2 %>%
  mutate(predictions_point_5 = ifelse(predicted_probability >= .5, 1, 0)) #where 1 = Obama, 0 = McCain
#A3.3
#Repeat the previous step, but now convert each individual’s prediction to a binary prediction for Obama
#only if the individual’s probability of voting for Obama is at least 0.7. Save these binary predictions as
#a column called predictions_point_7.
poll_data2 <- poll_data2 %>%
  mutate(predictions_point_7 = ifelse(predicted_probability >= .7, 1, 0)) #where 1 = Obama, 0 = McCain

#A3.4
#Write just the three columns predicted_probability, predictions_point_5, and predictions_point_7
#to data/question_a3.csv
justTheThree <- poll_data2[,c("predicted_probability", "predictions_point_5", "predictions_point_7")]

write.csv(justTheThree, "hw3_njiosseu/data/question_a3.csv", row.names = F)

#A3.5
#Compute the accuracy, precision, and recall metrics for the binary predictions produced in steps 2 and
#3 (without using a package that does it automatically). Report these values and any notable differences 
#between the metrics corresponding to the predictions in steps 2 and 3 in your writeup.
Accuracy <- poll_data2 %>%
  mutate(correct_point_5 = case_when(predictions_point_5 == 1 & vote_2008 == "barack obama" ~ 1,
                                     predictions_point_5 == 0 & vote_2008 == "john mcCain" ~ 1, 
                                     TRUE ~ 0),
         correct_point_7 = case_when(predictions_point_7 == 1 & vote_2008 == "barack obama" ~ 1,
                                     predictions_point_7 == 0 & vote_2008 == "john mcCain" ~ 1, 
                                     TRUE ~ 0)) %>%
  #group_by(vote_2008) %>%
  summarize(accuracy_point_5 = sum(correct_point_5, na.rm = T)/n(),
            accuracy_point_7 = sum(correct_point_7, na.rm = T)/n())


Precision_5 <- poll_data2 %>%
  mutate(four_outcomes_point_5 = case_when(predictions_point_5 == 1 & vote_2008 == "barack obama" ~ "TP",
                                           predictions_point_5 == 0 & vote_2008 == "john mcCain" ~ "TN",
                                           predictions_point_5 == 1 & vote_2008 == "john mcCain" ~ "FP",
                                           predictions_point_5 == 0 & vote_2008 == "barack obama" ~ "FN")) %>%
  count(four_outcomes_point_5) %>%
  pivot_wider(names_from = four_outcomes_point_5, values_from = n) %>%
  mutate(Precision_5 = TP / sum(TP+FP, na.rm = T),
         Recall_5 = TP / sum(TP+FN, na.rm = T))

Precision_7 <- poll_data2 %>%  
  mutate(four_outcomes_point_7 = case_when(predictions_point_7 == 1 & vote_2008 == "barack obama" ~ "TP",
                                           predictions_point_7 == 0 & vote_2008 == "john mcCain" ~ "TN",
                                           predictions_point_7 == 1 & vote_2008 == "john mcCain" ~ "FP",
                                           predictions_point_7 == 0 & vote_2008 == "barack obama" ~ "FN")) %>%
  count(four_outcomes_point_7)  %>%
  pivot_wider(names_from = four_outcomes_point_7, values_from = n) %>%
  mutate(Precision_7 = TP / sum(TP+FP, na.rm = T),
         Recall_7 = TP / sum(TP+FN, na.rm = T))
#OLD
#group_by(vote_2008) %>% #I don't think we need to group by outcome. Is this the right approach to accuracy vs precision?
#Precision = TP / (TP+FP), TP = expected Obama & observed Obama, FP =  Expected McCain & observed Obama
#Recall = TP / (TP+FN) FN = Expected Obama & observed McCain

#10-10-2019
#Precision = actually and predicted true / predicted true
#TP + FP / count of predicted > threshold
#Recall = actually true and predicted true / actually true
#TP + FP / count of votes from data

#A4
#1 Read in poll_data_full.tsv, and call the resulting tibble poll_data_full   
poll_data_full <- read_tsv("data/poll_data_full.tsv") 

#table(poll_data_full$vote_2008, useNA = "ifany")


poll_data_full <- poll_data_full %>%
  mutate(vote_2008 = factor(vote_2008, levels = c("john mcCain", "barack obama", "other")),
         vote_2008_mp = recode(vote_2008, 'john mcCain' = "majorParty", 'barack obama' = "majorParty", other = "other"),
         state = factor(state),
         sex = factor(sex),
         race = factor(race),
         age = factor(age, levels = c("18-29", "30-44", "45-64", "65+")),
         education = factor(education, levels = c("didn't graduate from HS", "high school graduate", "some college", "college graduate")),
         party = factor(party),
         ideology = factor(ideology),
         state_contestedness = factor(state_contestedness))

#table(poll_data_full$vote_2008_mp, useNA = "ifany")
#A4.1
#Using all available features, build a binary logistic regression model to predict whether an individual voted
#for a major-party candidate in the 2008 elections (both Obama and McCain are major party candidates). 
#Save these predictions as a column called pr_major.
#'all available predictors' -- included vote_2008 but it didn't converge
A4 <- glm(vote_2008_mp ~ 1 + state + sex + race + age + education + party + ideology + state_contestedness, 
          family = binomial, data = poll_data_full)

#poll_data_full$pr_major <- predict(A4, type = 'response') #this is predicting probability of voting for other
poll_data_full$pr_major <- (1 - predict(A4, type = 'response')) #factors are 0 for MP and 1 for other
#summary(poll_data_full$pr_major)

#A4.2
#Filter poll_data_full to only individuals who actually voted for major party candidates. 
#On this subset, use all features to build a binary logistic regression model to predict whether 
#an individual voted for Obama. This model allows us to estimate Pr(voted Obama | voted major party candidate). 

poll_data_full_subset <- poll_data_full %>%
  filter(vote_2008_mp == 'majorParty')

A4.2 <- glm(vote_2008 ~ 1 + state + sex + race + age + education + party + ideology + state_contestedness, 
            family = binomial, data = poll_data_full_subset)

#Using this model, generate estimates of Pr(voted Obama | voted major party candidate) for every individual 
#in poll_data_full. Save these predictions as a column called pr_obama_given_major.
poll_data_full$pr_obama_given_major <- predict(A4.2, poll_data_full, type = 'response')
#these predictions are on the probability scale

#summary(poll_data_full$pr_obama_given_major)

#summary(poll_data_full$pr_major)

#test <- poll_data_full %>%
filter(vote_2008_mp == "other")

#summary(test$pr_obama_given_major)
#hist(test$pr_obama_given_major)

#A4.3
##Use pr_major and pr_obama_given_major to compute three numbers for each individual in poll_data_full: 
##the probability that the individual votes for Obama, the probability that the individual votes for McCain, 
##and the probability that the individual votes for ‘Other’. 
##Save these predictions as columns pr_obama, pr_mccain, and pr_other. 
#  #pr_other: 1 - pr_major
#  #pr_obama: already have it as pr_obama_given_major
#  #pr_mccain: 1 - pr_obama
#poll_data_full <- poll_data_full %>%
#    mutate(pr_other = 1 - pr_major,
#           pr_obama = pr_obama_given_major,
#           pr_mccain = 1 - pr_obama_given_major, 
#           predictions = case_when(pr_other >= .5 ~ "Other", 
#                                   pr_other < .5 && pr_obama >= .5 ~ "Obama",
#                                   pr_other < .5 && pr_mccain >= .5 ~ "McCain"))
#
#summary(poll_data_full$pr_other)
#table(poll_data_full$predictions)
##Next, generate categorical predictions for each individual based on these probabilities, and save these 
##categorical predictions in a column called predictions. 
##Report the accuracy of your classifier in your writeup.

#A4.3 Use pr_major and pr_obama_given_major to compute three numbers for each individual in poll_data_full: the probability that the individual votes for Obama, the probability that the
#individual votes for McCain, and the probability that the individual votes for ‘Other’. Save these predictions as columns pr_obama, pr_mccain, and pr_other.

#A4.3 Used Bayes Rule
#pr (Obama|Major ) = Pr(Obama and Major)/Pr(Major)
#Pr(obama) would be Pr (obama |major)*pr major
poll_data_full$pr_other <- (1 - poll_data_full$pr_major)
summary(poll_data_full$pr_other)
poll_data_full$pr_obama <- poll_data_full$pr_obama_given_major * poll_data_full$pr_major
poll_data_full$pr_mccain <- (1-poll_data_full$pr_obama_given_major) * poll_data_full$pr_major

#test to see if add up to 1
test<-poll_data_full$pr_other+poll_data_full$pr_obama+poll_data_full$pr_mcCain
table(test)
#      generate categorical predictions
#for each individual based on these probabilities, and save these categorical predictions in a column called predictions.
#my hunch ; take max of the three probabilities (other, mc cain, obama) and give that column name as the predicted value)
poll_data_full$predictions <- ifelse(poll_data_full$pr_obama> poll_data_full$pr_mccain, "Obama", 
                                     ifelse(poll_data_full$pr_mcCain>poll_data_full$pr_other, "mcCain", "other"))
#Report the accuracy of your classifier in your writeup.
Accuracy4.3 <- poll_data_full %>%
  mutate(correct_pred = case_when(predictions == "Obama" & vote_2008 == "barack obama" ~ 1,
                                  predictions == "mcCain" & vote_2008 == "john mcCain" ~ 1, 
                                  predictions == "other" & vote_2008 == "other" ~ 1,
                                  TRUE ~ 0)) %>%
  #group_by(vote_2008) %>%
  summarize(accuracy_pred = sum(correct_pred, na.rm = T)/n())

#  accuracy_pred
#  1         0.842

#A4.4Write just the six columns pr_major, pr_obama_given_major, pr_obama, pr_mccain, pr_other, and predictions 
#(for every individual in poll_data_full) out to data/question_a4.csv within your submission directory.
justTheSix <- poll_data_full[, c("pr_major", "pr_obama_given_major", "pr_obama", "pr_mccain", "pr_other", "predictions")]

write.csv(justTheSix, "hw3_njiosseu/data/question_a4.csv", row.names = F)

#A4.5
#Create one figure with two subplots: a histogram of pr_major, and a histogram of pr_obama_given_major. 
#Save this figure to figures/question_a4.png within your submission, and briefly describe the histograms in your writeup.

hist_pr_major <- qplot(poll_data_full$pr_major, geom = "histogram", xlab = "Probability of Major Party Vote", ylab = "Count")
hist_pr_obama_given_major <- qplot(poll_data_full$pr_obama_given_major, geom = "histogram", xlab = "Probability of Obama Vote, Given Major Party Vote", ylab = "Count")

grid.arrange(
  hist_pr_major, hist_pr_obama_given_major
)

ggsave("hw3_njiosseu/figures/question_a4.png")