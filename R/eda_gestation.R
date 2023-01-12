#       _/_/    _/  _/      _/_/      _/   
#    _/    _/  _/  _/    _/    _/  _/_/    
#       _/    _/_/_/_/    _/_/_/    _/     
#    _/          _/          _/    _/      
# _/_/_/_/      _/    _/_/_/    _/_/_/     

# Exploratory Data Analysis of Gestation data

library(tidyverse)
library(mosaicData)
# if you don't have mosaicData, install it

data(Gestation)

# Activity 1 - Quick look at the data

# number of observations
count(Gestation)

# number of observations per racial group
count(Gestation, race)

# number of observations by racial group and level of mother's education
Gestation_n_race_ed <- count(Gestation, race, ed)
Gestation_n_race_ed

# Activity 2 - Further summary statistics

# mean age of mothers across all births
# ensure you use a human friendly name for the value you're creating
summarise(Gestation,mean = mean(age, na.rm=TRUE))

# calculate both mothers' mean age and babies' mean weight
summarise(Gestation, 
          `Mean age` = mean(age, na.rm=T),
          `Mean wt`  = mean(wt,na.rm=T))


# Activity 3 - Grouped summaries

# make a new data frame containing only id, age and race variables
gest_iar <- Gestation %>% select(id,age,race)
gest_iar

# calculate the mean age by race
gest_iar %>% group_by(race) %>% summarise(mean_age = mean(age, na.rm=T))

# Activity 4 - Extensions
# Activity 4a - Correlation

# Calculate the correlation between age and weight across all births
Gestation %>% ggplot() + geom_point(aes(x=age,y=wt))
age_wt_data <- Gestation %>% select(age,wt)
## drop nas
age_wt_data <- drop_na(age_wt_data)

##find correlation
age_wt_data %>% summarise(cor = cor(age, wt), na.rm=T)

# Calculate the correlation between age and weight for each race group
Gestation %>% group_by(race) %>% summarise(cor=cor(age,wt),na.rm=T)

# Activity 4b - Multiple summary statistics

# Calculate the sample mean of the ages and weights of the mothers in each race group
Gestation %>% group_by(race) %>% summarise_at(.vars = c('age', 'wt'), .fun = mean, na.rm=T)

# Activity 4c - Pivoting wider

# Make a wide table from the summary data frame calculated in Activity 1 that has the number of observations for each combination of mother's education level and race. Make each row is an education level and each column a race group.
pivot_gest <- Gestation_n_race_ed %>%  pivot_wider(id_cols=ed,names_from=c(race),values_from=c(n))
# Hint: Look at the help file for `pivot_wider` for what to do with missing cells (where there is no combination of these variables) and set the argument to be 0.

# Activity 4d - Multiple summary statistics

# Calculate the mean, standard deviation, minimum, maximum and proportion of values missing for the mothers' ages for each race group.
# Hint: you *can* use summarise_at() for this but you could also just summarise()
Gestation %>% group_by(race) %>% summarise(mean_age = mean(age,na.rm=T),
                                           sd_age = sd(age,na.rm=T),
                                           min_age = min(age,na.rm=T),
                                           max_age = max(age,na.rm=T),
                                           is_na = sum(is.na(age)))

                                           