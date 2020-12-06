#B4
standardize <- function(x) {
  x.std <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  x.std
}
require(tidyverse)
require(ROCR)
#frisked is the outcome variable
#predictor variables are in relevant columns (frisked is also there).
relevant_cols <- c('id', 'year', 'frisked',
                   'suspect.race', 'suspect.age', 'suspect.build', 
                   'suspect.sex', 'suspect.height', 'suspect.weight',
                   'precinct', 'inside', 'location.housing', 
                   'observation.period', 'officer.uniform', 'additional.report', 
                   'additional.investigation', 'additional.proximity', 
                   'additional.evasive', 'additional.associating', 
                   'additional.direction', 'additional.highcrime', 
                   'additional.time', 'additional.sights', 'additional.other', 
                   'radio.run', 'day', 'month', 'time.period')

#make year a factor?
#getwd()
#choosing subset of data for creating model

sqf_08_16 <- read_csv("data/sqf_08_16.csv")

sqf_08_16_B4<- sqf_08_16 %>%
  filter(suspected.crime=='cpw') %>% #do we filter this or not? CJB: Yes.
  select(relevant_cols)%>%
  filter(year == 2008| year ==2012| year == 2016) %>%
  mutate(precinct = factor(precinct),
         year = factor(year, levels = c("2008", "2012", "2016")),
         location.housing = factor(location.housing), 
         suspect.build = factor(suspect.build, levels = c("thin", "medium", "muscular", "heavy", "unknown")),
         suspect.sex = factor(suspect.sex),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
         month = factor(month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October",
                                          "November", "December")),
         suspect.race = factor(suspect.race, levels = c("asian", "black", "hispanic", "native.american", "other", "white")) 
  )

#B4.1
#target variable 'frisked'
table(sqf_08_16_B4$frisked)

#include suspect.race, suspect.hispanic; year, if we want to...

#What data to use? Years 2008, 2012, 2016

#Create a train-test split of the data either randomly (e.g., train on a random 50% of rows and test on the other half) or 
#temporally (e.g. 2008-2010 data for training and 2011 for testing).

#Logistic regression

#B4.2
#include suspect.race, suspect.hispanic; year, if we want to...(removing hispanic as it is related to race)
#predictor Variables: all previous + added race, hispanic and year
#removed stopped and frisked as they would have happened afterwards.
#CJB: Not sure what you mean:
#1) Hispanic is usually coded in addition to other racial categories to try to be accurate to human experience; a person can 
#be black and hispanic or white and hispanic, for example.
#2) they would be stopped before frisked, I think, and frisked is the target variable.

#B4.3
#Create a train-test split of the data either randomly (e.g., train on a random 50% of rows and test on the 
# other half) or temporally (e.g. 2008-2010 data for training and 2011 for testing).

sample <- sample.int(n = nrow(sqf_08_16_B4), size = .50*nrow(sqf_08_16_B4), replace = F)
train <- sqf_08_16_B4[sample, ]
test  <- sqf_08_16_B4[-sample, ]

#scaling numeric variables in test and train data.
#CJB: What package does standarize come from? It's not in the tidyverse. Is there a reason you chose this rather than scale?
train <- train %>% mutate(suspect.height = standardize(suspect.height),
                          suspect.weight = standardize(suspect.weight),
                          suspect.age = standardize(suspect.age),
                          observation.period = standardize(observation.period))

test <- test %>% mutate(suspect.height = standardize(suspect.height),
                        suspect.weight = standardize(suspect.weight),
                        suspect.age = standardize(suspect.age),
                        observation.period = standardize(observation.period))

#model 
B4 <- glm( frisked ~ 1 + precinct + location.housing + additional.report + additional.investigation + 
             additional.proximity + 
             additional.evasive + additional.associating + additional.direction + additional.highcrime 
           + additional.time +
             additional.sights + additional.other + scale(suspect.age) + suspect.build + suspect.sex 
           + scale(suspect.height) + 
             scale(suspect.weight) + inside + radio.run + 
             scale(observation.period) + day + month + time.period + suspect.race +   year ,
           family = binomial, data = train)
summary(B4)
#CJB: Whole dataset, not train?

#prediction
test <- test %>% mutate(predicted.probability =  
                          predict(B4, test, type='response'))

threshold <- 0.5
test <- test %>% mutate(prediction = case_when(
  predicted.probability < threshold ~ F,
  predicted.probability >= threshold ~ T
))

table(test$prediction, test$frisked)
cat('At the threshold of', threshold, 'the precision is',
    nrow(filter(test, prediction==T, frisked==T))/nrow(filter(test, prediction==T)),
    ' and the recall is ',
    nrow(filter(test, prediction==T, frisked==T))/nrow(filter(test, frisked==T)))

plot.data <- test %>% arrange( desc(predicted.probability) ) %>%
  mutate(numstops = row_number(), percent.outcome = cumsum( frisked )/sum( frisked ),
         stops = numstops/n()) %>% select(stops, percent.outcome)

theme_set(theme_bw())
p <- ggplot(data=plot.data, aes(x=stops, y=percent.outcome))
p <- p + geom_line()
p <- p + scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1),
                       labels=c('0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_y_continuous("Percent of frisked", limits=c(0, 1), labels=scales::percent)
p
ggsave("hw3_njiosseu/figures/recall_at_k.png", p)

# make calibration plot (uncomment code)
plot.data <- test %>% mutate(calibration = round(100*predicted.probability)) %>%
  group_by(calibration) %>% summarize(model.estimate = mean(predicted.probability),
                                      numstops = n(),
                                      empirical.estimate = mean(frisked))

# create and save plot
p <- ggplot(data = plot.data, aes(y=empirical.estimate, x=model.estimate))
p <- p + geom_point(alpha=0.5, aes(size=numstops))
p <- p + scale_size_area(guide='none', max_size=15)
p <- p + geom_abline(intercept=0, slope=1, linetype="dashed")
p <- p + scale_y_log10('Empirical probability \n', limits=c(.1,1), breaks=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1),
                       labels=c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
p <- p + scale_x_log10('\nModel estimated probability', limits=c(.1,1), breaks=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1),
                       labels=c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
p
ggsave("hw3_njiosseu/figures/calibration.png", p)
