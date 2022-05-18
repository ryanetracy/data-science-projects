################
# final project 
################


pckgs <- c('car', 'rstatix', 'emmeans', 'effectsize', 'interactions', 'effsize', 'psych', 'gmodels',
          'varhandle', 'correlation', 'ggcorrplot', 'GGally', 'ggpubr', 'readxl', 'tidyverse')

for (i in 1:length(pckgs)) {
  if(!(pckgs[[i]] %in% installed.packages())) {
    install.packages(pckgs[[i]])
  }
  lapply(pckgs, library, character.only = T)
}


# read in the data
df <- read_excel('IBMEmployees.xlsx', sheet = 1)


### there are two big questions to answer with this data set:
### 1. how serious is the gender pay gap at IBM? which variables are the biggest contributors to this pay gap?
### 2. what factors contribute to employee attrition? how do they contribute (i.e., which ones have positive vs negative effects)?

# view 20 random rows
df %>% sample_n(20)
# column names
colnames(df)


# info
str(df) #also indicates no missing data (all numerical rows have 1470 values)
sum(is.na(df))

# convert employee id to factor
df$EmployeeID <- as.factor(df$EmployeeID)

# none are factors, let's see the levels of each of the string variables 
# (over18, gender, maritalstatus, educationfield, department, jobrole, businesstravel, overtime, attrition)
unique(df$Over18) # only one level -- not a useful variable
unique(df$Gender)
unique(df$MaritalStatus)
unique(df$EducationField)
unique(df$Department)
unique(df$JobRole)
unique(df$BusinessTravel)
unique(df$OverTime)
unique(df$Attrition)

# convert these to factors
df$Gender <- as.factor(df$Gender)
df$MaritalStatus <- factor(df$MaritalStatus, levels = c('Single', 'Married', 'Divorced'))
df$EducationField <- as.factor(df$EducationField)
df$Department <- as.factor(df$Department)
df$JobRole <- as.factor(df$JobRole)
df$BusinessTravel <- factor(df$BusinessTravel, levels = c('Non-Travel', 'Travel_Rarely', 'Travel_Frequently'))
df$OverTime <- factor(df$OverTime, levels = c('No', 'Yes'))
df$Attrition <- factor(df$Attrition, levels = c('No', 'Yes'))

# is everyone actually over 18?
df %>%
  get_summary_stats(Age)

# drop the over18 column
df <- df %>%
  select(-Over18)

# some of these seem like ordinal scales
# education
unique(df$Education)
# job level
unique(df$JobLevel)
# stock option level
unique(df$StockOptionLevel)
# work life balance
unique(df$WorkLifeBalance)
# job involvement
unique(df$JobInvolvement)
# performance rating
unique(df$PerformanceRating) # this is actually a binary variable -- transform it

# might have to set some of these as factors (education, job level, stock option level)
df$Education <- factor(df$Education, levels = c(1,2,3,4,5))
df$JobLevel <- factor(df$JobLevel, levels = c(1,2,3,4,5))
df$StockOptionLevel <- factor(df$StockOptionLevel, levels = c(0,1,2,3))

# transform the performance rating variable; 3 and 4 make no sense, so 3 is 'low' and 4 is 'high'
df$PerformanceRating <- factor(df$PerformanceRating, levels = c(3,4), labels = c('Low', 'High'))




#### question 1: is there a severe gender pay gap? (monthly income, hourly rate, daily rate, monthly rate, salary hike)

# get some descriptive statistics about the female vs male employees
# how many males/females are there?
plyr::count(df$Gender) %>% mutate(prop = freq / sum(freq))

# now get some more descriptions, looking at the categorical variables (proportions are proportions of each gender in each category)
# marital status
df %>% group_by(Gender) %>% count(MaritalStatus) %>% mutate(prop = n / sum(n))
# education field
df %>% group_by(Gender) %>% count(EducationField) %>% mutate(prop = n / sum(n))
# department
df %>% group_by(Gender) %>% count(Department) %>% mutate(prop = n / sum(n))
# job role
df %>% group_by(Gender) %>% count(JobRole) %>% mutate(prop = n / sum(n))
# business travel
df %>% group_by(Gender) %>% count(BusinessTravel) %>% mutate(prop = n / sum(n))
# attrition
df %>% group_by(Gender) %>% count(Attrition) %>% mutate(prop = n / sum(n))

# what is the average monthly income of males vs females?
df %>% group_by(Gender) %>% get_summary_stats(MonthlyIncome, type = 'mean_sd')
# averages seem even, what about the medians?
df %>% group_by(Gender) %>% get_summary_stats(MonthlyIncome, type = 'median')
# median income is higher for females than males

# check the distributions
ggplot(df, aes(MonthlyIncome, fill = Gender)) +
  geom_histogram(color = 'black', binwidth = 500, alpha  = .5) +
  geom_density(aes(y = after_stat(count * 500), color = Gender),
               alpha = 0) +
  geom_rug(aes(color = Gender)) +
  theme_classic() +
  labs(y = 'Frequency',
       x = 'Monthly Income') +
  scale_fill_manual(values = c('red', 'blue')) +
  scale_color_manual(values = c('red', 'blue')) +
  # add a vertical line for males and females mean income
  geom_vline(xintercept = mean(df$MonthlyIncome[df$Gender == 'Male']), color = 'blue', linetype = 1) +
  geom_vline(xintercept = mean(df$MonthlyIncome[df$Gender == 'Female']), color = 'red', linetype = 1) +
  # now for the median incomes (dashed lines)
  geom_vline(xintercept = median(df$MonthlyIncome[df$Gender == 'Male']), color = 'blue', linetype = 2) +
  geom_vline(xintercept = median(df$MonthlyIncome[df$Gender == 'Female']), color = 'red', linetype = 2)


# what about hourly rates?
df %>% group_by(Gender) %>% get_summary_stats(HourlyRate, type = 'mean_sd')
df %>% group_by(Gender) %>% get_summary_stats(HourlyRate, type = 'median')

ggplot(df, aes(HourlyRate, fill = Gender)) +
  geom_histogram(color = 'black', binwidth = 7, alpha  = .5) +
  geom_density(aes(y = after_stat(count * 7), color = Gender),
               alpha = 0) +
  geom_rug(aes(color = Gender)) +
  theme_classic() +
  labs(y = 'Frequency',
       x = 'Hourly Rate') +
  scale_fill_manual(values = c('red', 'blue')) +
  scale_color_manual(values = c('red', 'blue')) +
  #add a vertical line for males and females mean income
  geom_vline(xintercept = mean(df$HourlyRate[df$Gender == 'Male']), color = 'blue', linetype = 1) +
  geom_vline(xintercept = mean(df$HourlyRate[df$Gender == 'Female']), color = 'red', linetype = 1) +
  #now for the median incomes (dashed lines)
  geom_vline(xintercept = median(df$HourlyRate[df$Gender == 'Male']), color = 'blue', linetype = 2) +
  geom_vline(xintercept = median(df$HourlyRate[df$Gender == 'Female']), color = 'red', linetype = 2)


# daily rates?
df %>% group_by(Gender) %>% get_summary_stats(DailyRate, type = 'mean_sd')
df %>% group_by(Gender) %>% get_summary_stats(DailyRate, type = 'median')

ggplot(df, aes(DailyRate, fill = Gender)) +
  geom_histogram(color = 'black', binwidth = 25, alpha  = .5) +
  geom_density(aes(y = after_stat(count * 25), color = Gender),
               alpha = 0) +
  geom_rug(aes(color = Gender)) +
  theme_classic() +
  labs(y = 'Frequency',
       x = 'Daily Rate') +
  scale_fill_manual(values = c('red', 'blue')) +
  scale_color_manual(values = c('red', 'blue')) +
  # add a vertical line for males and females mean income
  geom_vline(xintercept = mean(df$DailyRate[df$Gender == 'Male']), color = 'blue', linetype = 1) +
  geom_vline(xintercept = mean(df$DailyRate[df$Gender == 'Female']), color = 'red', linetype = 1) +
  # now for the median incomes (dashed lines)
  geom_vline(xintercept = median(df$DailyRate[df$Gender == 'Male']), color = 'blue', linetype = 2) +
  geom_vline(xintercept = median(df$DailyRate[df$Gender == 'Female']), color = 'red', linetype = 2)


# monthly rates?
df %>% group_by(Gender) %>% get_summary_stats(MonthlyRate, type = 'mean_sd')
df %>% group_by(Gender) %>% get_summary_stats(MonthlyRate, type = 'median')

ggplot(df, aes(MonthlyRate, fill = Gender)) +
  geom_histogram(color = 'black', binwidth = 500, alpha  = .5) +
  geom_density(aes(y = after_stat(count * 500), color = Gender),
               alpha = 0) +
  geom_rug(aes(color = Gender)) +
  theme_classic() +
  labs(y = 'Frequency',
       x = 'Monthly Rate') +
  scale_fill_manual(values = c('red', 'blue')) +
  scale_color_manual(values = c('red', 'blue')) +
  # add a vertical line for males and females mean income
  geom_vline(xintercept = mean(df$MonthlyRate[df$Gender == 'Male']), color = 'blue', linetype = 1) +
  geom_vline(xintercept = mean(df$MonthlyRate[df$Gender == 'Female']), color = 'red', linetype = 1) +
  # now for the median incomes (dashed lines)
  geom_vline(xintercept = median(df$MonthlyRate[df$Gender == 'Male']), color = 'blue', linetype = 2) +
  geom_vline(xintercept = median(df$MonthlyRate[df$Gender == 'Female']), color = 'red', linetype = 2)


# salary hike?
df %>% group_by(Gender) %>% get_summary_stats(PercentSalaryHike, type = 'mean_sd')
df %>% group_by(Gender) %>% get_summary_stats(PercentSalaryHike, type = 'median')

ggplot(df, aes(PercentSalaryHike, fill = Gender)) +
  geom_histogram(color = 'black', binwidth = 1, alpha  = .5) +
  geom_density(aes(y = after_stat(count * 1), color = Gender),
               alpha = 0) +
  geom_rug(aes(color = Gender)) +
  theme_classic() +
  labs(y = 'Frequency',
       x = 'Percent Salary Hike') +
  scale_fill_manual(values = c('red', 'blue')) +
  scale_color_manual(values = c('red', 'blue')) +
  # add a vertical line for males and females mean income
  geom_vline(xintercept = mean(df$PercentSalaryHike[df$Gender == 'Male']), color = 'blue', linetype = 1) +
  geom_vline(xintercept = mean(df$PercentSalaryHike[df$Gender == 'Female']), color = 'red', linetype = 1) +
  # now for the median incomes (dashed lines)
  geom_vline(xintercept = median(df$PercentSalaryHike[df$Gender == 'Male']), color = 'blue', linetype = 2) +
  geom_vline(xintercept = median(df$PercentSalaryHike[df$Gender == 'Female']), color = 'red', linetype = 2)




# some of the continuous variables of interest might be more telling than others
# things like age, total working years, training times, and years at company, current role, since last promotion, and with current manager
df %>%
  select(MonthlyIncome, Age, TotalWorkingYears, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager, TrainingTimesLastYear) %>%
  ggpairs()

# most of these look like they have a positive relationship with monthly income (or at the very least some type of linear one)
# create a dataframe with just these to run regression analyses
dfReg <- df %>%
  select(EmployeeID, Gender, MonthlyIncome, Age, TotalWorkingYears, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager, TrainingTimesLastYear) %>%
  mutate(IsMale = ifelse(Gender == 'Female', 0, 1)) %>%
  select(-Gender)
head(dfReg)

# create a helper function to run a few different regression models (and get diagnostics)
getModel <- function(model) {
  s <- summary(model)
  print(s)
  
  # confidence intervals
  cis <- confint(model)
  print(cis)
  
  # standardized estimates
  betas <- standardize_parameters(model)
  print(betas)
  
  # model checks
  par(mfrow = c(2,2))
  plot(model)
  
  par(mfrow = c(1,1))
  
  # check for outliers
  outs <- outlierTest(model)
  print('outlier test')
  print(outs)
  
  # check for autocorrelation
  auto <- durbinWatsonTest(model)
  print('durbin watson test')
  print(auto)
  
  # added variable plots
  avPlots(model)
  
  # multicollinearity
  vif(model)
}

# start by looking at whether there is a pay gap between males and females
m0 <- lm(MonthlyIncome ~ IsMale, data = dfReg)
summary(m0)
# no significant differences found

# likely not when controlling for different factors
m1 <- lm(MonthlyIncome ~ IsMale + Age + TotalWorkingYears + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + TrainingTimesLastYear, data = dfReg)
getModel(m1)

# so according to the regression models, there's no difference in pay between males and females (viz. montly income)
# explore additional company-level factors related to employees' jobs
# job satisfaction, work life balance, job involvement, performance rating, environment satisfaction
addRegs <- df %>%
  select(EmployeeID, JobSatisfaction, JobInvolvement, WorkLifeBalance, EnvironmentSatisfaction, PerformanceRating) %>%
  mutate(Performance = ifelse(PerformanceRating == 'High', 1, 0)) %>%
  select(-PerformanceRating) %>%
  # merge this with the dfReg dataframe, matching by employee id
  left_join(dfReg, by = c('EmployeeID' = 'EmployeeID'))

# explore one more model
m2 <- lm(MonthlyIncome ~ IsMale + Age + TotalWorkingYears + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + TrainingTimesLastYear + JobSatisfaction + JobInvolvement + WorkLifeBalance + EnvironmentSatisfaction + Performance, data = addRegs)
getModel(m2)


# there seems to be very little relationship between gender and pay
# instead, maybe some of these significant predictors (age, total working years, years at company, years in current role, years with manager) may interact with gender
# check with a moderation analysis
m3 <- lm(MonthlyIncome ~ IsMale*Age + IsMale*TotalWorkingYears + IsMale*YearsAtCompany + IsMale*YearsInCurrentRole + IsMale*YearsWithCurrManager, data = addRegs)
getModel(m3)

# some of these moderations were significant
# specifically gender and years at company and years in current role, though they go in opposite directions
sim_slopes(m3,
           pred = 'YearsAtCompany',
           modx = 'IsMale')

interact_plot(m3,
              pred = 'YearsAtCompany',
              modx = 'IsMale',
              colors = 'blue',
              interval = T,
              int.type = 'confidence',
              x.label = 'Years at Company',
              y.label = 'Monthly Income',
              modx.labels = c('Female', 'Male'),
              legend.main = 'Gender') +
  theme_classic() +
  theme(legend.position = c(.15, .8))

# ggsave('years at company x gender regression plot.jpg', device = 'jpeg', units = 'cm')

sim_slopes(m3,
           pred = 'YearsInCurrentRole',
           modx = 'IsMale')

interact_plot(m3,
              pred = 'YearsInCurrentRole',
              modx = 'IsMale',
              colors = 'green',
              interval = T,
              int.type = 'confidence',
              x.label = 'Years in Current Role',
              y.label = 'Monthly Income',
              modx.labels = c('Female', 'Male'),
              legend.main = 'Gender') +
  theme_classic() +
  theme(legend.position = c(.15, .8))

# ggsave('years in role x gender regression plot.jpg', device = 'jpeg', units = 'cm')

# from these moderations, we see two critical things:
# 1. for the effect of years at company, males who spend more time at IBM increase their pay; females see no pay increase over time
# 2. for the effect of years in current role, males who spend many years in their current job see a decrease in pay; this is the opposite for females (though individually these slopes are nonsignificant)

# what is the role of gender in predicting years at company/years in current role?
m3.1 <- lm(YearsAtCompany ~ IsMale, data = addRegs)
summary(m3.1)

m3.2 <- lm(YearsInCurrentRole ~ IsMale, data = addRegs)
summary(m3.2)

# gender doesn't predict either outcome variable


# maybe some of the categorical variables can shed more light on pay differences between males and females

# sticking with the monthly income variable, visualize some of the trends of different variables as they relate to gender
# marital status x gender
df %>%
  group_by(Gender, MaritalStatus) %>%
  get_summary_stats(MonthlyIncome, type = 'mean_ci') %>%
  ggplot(aes(MaritalStatus, mean, fill = Gender)) +
  geom_bar(stat = 'identity', position = position_dodge(.9), color = 'black', alpha = .8) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = .25, alpha = .5, position = position_dodge(.9)) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 8500, 500)) +
  expand_limits(y = 8500) +
  scale_fill_manual(values = c('#ffb612', '#0073cf')) +
  labs(y = 'Monthly Income')

# business travel x gender
df %>%
  group_by(Gender, BusinessTravel) %>%
  get_summary_stats(MonthlyIncome, type = 'mean_ci') %>%
  ggplot(aes(BusinessTravel, mean, fill = Gender)) +
  geom_bar(stat = 'identity', position = position_dodge(.9), color = 'black', alpha = .8) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = .25, alpha = .5, position = position_dodge(.9)) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 8500, 500)) +
  expand_limits(y = 8500) +
  scale_fill_manual(values = c('#ffb612', '#0073cf')) +
  labs(y = 'Monthly Income')

# education field x gender
df %>%
  group_by(Gender, EducationField) %>%
  get_summary_stats(MonthlyIncome, type = 'mean_ci') %>%
  ggplot(aes(EducationField, mean, fill = Gender)) +
  geom_bar(stat = 'identity', position = position_dodge(.9), color = 'black', alpha = .8) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = .25, alpha = .5, position = position_dodge(.9)) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 18500, 500)) + # # #  changed due to extremely high variance of the female - HR cell
  expand_limits(y = 18500) +
  scale_fill_manual(values = c('#ffb612', '#0073cf')) +
  labs(y = 'Monthly Income')

# education x gender
df %>%
  group_by(Gender, Education) %>%
  get_summary_stats(MonthlyIncome, type = 'mean_ci') %>%
  ggplot(aes(Education, mean, fill = Gender)) +
  geom_bar(stat = 'identity', position = position_dodge(.9), color = 'black', alpha = .8) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = .25, alpha = .5, position = position_dodge(.9)) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 12500, 500)) +
  expand_limits(y = 12500) +
  scale_fill_manual(values = c('#ffb612', '#0073cf')) +
  labs(y = 'Monthly Income')

# job role x gender
df %>%
  group_by(Gender, JobRole) %>%
  get_summary_stats(MonthlyIncome, type = 'mean_ci') %>%
  ggplot(aes(JobRole, mean, fill = Gender)) +
  geom_bar(stat = 'identity', position = position_dodge(.9), color = 'black', alpha = .8) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = .25, alpha = .5, position = position_dodge(.9)) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 18500, 1000)) +
  expand_limits(y = 18500) +
  scale_fill_manual(values = c('#ffb612', '#0073cf')) +
  labs(y = 'Monthly Income') +
  # rotate x axis labels
  theme(axis.text.x = element_text(angle = 45, vjust = .5))


# from these plots, it doesn't really look like there's any type of pay gap between males and females based on these technical factors
# where there are differences, there's also a high degree of variance, suggesting that traditional analyses based on categorical features would be unreliable (i.e., ANOVA)
# it's possible that there is a difference between men and woman research directors
df %>%
  select(MonthlyIncome, Gender, JobRole) %>%
  filter(JobRole == 'Research Director') %>%
  t_test(MonthlyIncome ~ Gender)

df %>%
  select(MonthlyIncome, Gender, JobRole) %>%
  filter(JobRole == 'Research Director') %>%
  group_by(Gender) %>%
  get_summary_stats(MonthlyIncome, type = 'mean_sd')

# this difference is significant, though not likely to mean much since there are no other differences in pay across the job roles (plus it's a product of multiple comparisons that is ignored by running t-tests)


# the next thing to look at is chi-squared tests to explore whether gender and job roles at the company are independent
# for this, there is a critical comparison between stock option levels and gender, where there can be possible differences
# other things we can look at are job role and gender and performance evaluation and gender

CrossTable(df$StockOptionLevel, df$Gender, expected = T, chisq = T)

# from this, we see that while there are indeed numeric differences between stock option levels and genders, these differences are not significant
# because the p-value is > .05, a person's gender and stock option levels are independent of each other

CrossTable(df$JobRole, df$Gender, expected = T, chisq = T)

# so despite the design being somewhat unbalanced, there is an association between gender and job role, chi^2(8) = 16.03, p = .04

CrossTable(df$PerformanceRating, df$Gender, expected = T, chisq = T)

# no association between gender and performance rating






#### question 2: what factors contribute to attrition?
str(df) # take a look at the data again

# convert attrition to a binary variable for logistic regression analyses
df$Attrition <- ifelse(df$Attrition == 'Yes', 1, 0)


# looking at the columns of the dataset, it is likely the case that things like satisfaction (3 types), work life balance, and involvement might predict attrition
# it's also likely the case that levels of business travel (none, rarely, often), working years, distance from home, and years at company (4 types) might also predict attrition

# let's set aside these columns in a separate dataframe
# we'll have to do some feature engineering on the business travel factor, creating two dummy columns for travel rarely (1 = yes, 0 = no) and travel often (1 = yes, 0 = no)

dfAtt <- df %>%
  select(Age, Gender, TotalWorkingYears, DistanceFromHome, BusinessTravel, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager,
         OverTime, PercentSalaryHike, EnvironmentSatisfaction, JobSatisfaction, RelationshipSatisfaction, WorkLifeBalance, JobInvolvement, PerformanceRating, Attrition) %>%
  # dummy code the factor variables
  mutate(TravelRarely = ifelse(BusinessTravel == 'Travel_Rarely', 1, 0),
         TravelFrequently = ifelse(BusinessTravel == 'Travel_Frequently', 1, 0),
         WorkedOT = ifelse(OverTime == 'Yes', 1, 0),
         HighPerformance = ifelse(PerformanceRating == 'High', 1, 0),
         IsMale = ifelse(Gender == 'Male', 1, 0)) %>%
  select(-c(BusinessTravel, OverTime, PerformanceRating, Gender))


str(dfAtt)


# model the outcome variable
dfAtt %>%
  ggplot(aes(Attrition)) +
  geom_bar(stat = 'count', fill = 'navy') +
  theme_classic()


# create a helper function to test logistic regression models
getLogModel <- function(model) {
  # create the null function
  nullMod <- glm(Attrition ~ 1, family = binomial(), data = dfAtt)
  
  # compare the null and alternative models
  modComp <- anova(nullMod, model, test = 'Chisq')
  print('Model Comparison:')
  print(modComp)
  
  # print the model summary
  s <- summary(model)
  print(s)
  
  # odds ratios
  ors <- exp(model$coefficients)
  print('Odds Ratios:')
  print(ors)
  
  # confidence intervals of odds ratios
  ors95 <- exp(confint(model))
  print('95% CIs:')
  print(ors95)
}

# what is the effect of age?
glm1 <- glm(Attrition ~ Age, family = binomial(), data = dfAtt)
getLogModel(glm1)

# what about total working years?
glm2 <- glm(Attrition ~ TotalWorkingYears, family = binomial(), data = dfAtt)
getLogModel(glm2)

# so far, age and total working years predict a decrease in attrition likelihood 
# (5.1% less likely as age increases; 7.5% less likely as working years increases)


# what about metrics of distance from work (also including travel)
glm3 <- glm(Attrition ~ DistanceFromHome + TravelRarely + TravelFrequently, family = binomial(), data = dfAtt)
getLogModel(glm3)

# each of hese predicted an increase in the likelihood of attrition
# distance: 2.5% increase in attrition
# travel rarely: 106.8% increase
# travel frequently: 289.6% increase

# looks like travel has a significant impact on attrition likelihood

# does this hold when accounting for things like job satisfaction?
glm3.1 <- glm(Attrition ~ DistanceFromHome + TravelRarely + TravelFrequently + JobSatisfaction + JobInvolvement, family = binomial(), data = dfAtt)
getLogModel(glm3.1)

# sure does

# what about moderations between these factors?
glm3.2 <- glm(Attrition ~ DistanceFromHome + TravelRarely + TravelFrequently + JobSatisfaction + JobInvolvement + TravelRarely:JobSatisfaction + TravelRarely:JobInvolvement + TravelFrequently:JobSatisfaction + TravelFrequently:JobInvolvement, family = binomial(), data = dfAtt)
getLogModel(glm3.2)

# seems like the effect of travel depends on job satisfaction
sim_slopes(glm3.2, pred = 'TravelRarely', modx = 'JobSatisfaction')

interact_plot(glm3.2, 
              pred = 'TravelRarely', 
              modx = 'JobSatisfaction', 
              colors = 'red',
              interval = T,
              int.type = 'confidence',
              x.label = 'Travel Rarely',
              legend.main = 'Job\nSatisfaction',
              pred.labels = c('No', 'Yes')) + 
  theme_classic() +
  theme(legend.position = c(.85, .17))

# ggsave('rare travel x job satisfaction regression plot.jpg', device = 'jpeg', units = 'cm')


sim_slopes(glm3.2, pred = 'TravelFrequently', modx = 'JobSatisfaction')

interact_plot(glm3.2, 
              pred = 'TravelFrequently', 
              modx = 'JobSatisfaction', 
              colors = 'green',
              interval = T,
              int.type = 'confidence',
              x.label = 'Travel Frequently',
              legend.main = 'Job\nSatisfaction',
              pred.labels = c('No', 'Yes')) + 
  theme_classic() +
  theme(legend.position = c(.15, .8))

# ggsave('frequent travel x job satisfaction regression plot.jpg', device = 'jpeg', units = 'cm')


# for both of these, the effect of travel only predicts an increase in attrition when job satisfaction is low
# for people on the low end of job satisfaction, making them travel (even rarely) predicts more attrition
# as travel requirements increase, it doesn't seem to matter how much someone likes their job -- attrition increases


# what about the role of personal variables in predicting attrition?
# things like work-life balance, relationship/job/environment satisfaction, distance from home, and age?
glm4 <- glm(Attrition ~ WorkLifeBalance + RelationshipSatisfaction + JobSatisfaction + EnvironmentSatisfaction + DistanceFromHome + Age, family = binomial(), data = dfAtt)
getLogModel(glm4)

# increases in each of these (except relationship satisfaction) predicts a decrease in attrition; distance predicts an increase in the likelihood
# are there any moderations with distance from home?

glm4.1 <- glm(Attrition ~ WorkLifeBalance*DistanceFromHome + RelationshipSatisfaction*DistanceFromHome + JobSatisfaction*DistanceFromHome + EnvironmentSatisfaction*DistanceFromHome + Age*DistanceFromHome, family = binomial(), data = dfAtt)
getLogModel(glm4.1)

# nothing meaningful; try travel frequently
glm4.2 <- glm(Attrition ~ WorkLifeBalance*TravelFrequently + RelationshipSatisfaction*TravelFrequently + JobSatisfaction*TravelFrequently + EnvironmentSatisfaction*TravelFrequently + Age*TravelFrequently, family = binomial(), data = dfAtt)
getLogModel(glm4.2)

# looks like the role of personal variables on attrition aren't affected by travel

# create one final model with all predictors
glm5 <- glm(Attrition ~ ., family = binomial(), data = dfAtt)
getLogModel(glm5)


# test model assumptions

# linearity of the logit
modPreds <- predict(glm5, type = 'response')

linLogit <- dfAtt %>%
  select(-Attrition) %>%
  mutate(logit = modPreds / (1 - modPreds)) %>%
  gather(key = 'predictor', value = 'predictedVals', -logit)

linLogit %>%
  ggplot(aes(logit, predictedVals)) +
  geom_point(alpha = .5, size = .75, shape = 4) +
  geom_smooth(method = 'loess', color = 'blue') +
  theme_classic() +
  facet_wrap(~ predictor)

# outliers/influence
influencePlot(glm5)

# multicollinearity
vif(glm5)
