#https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Living_conditions_in_Europe_-_income_distribution_and_income_inequality

#https://ec.europa.eu/eurostat/web/income-and-living-conditions/data/database

#https://plotly.com/ggplot2/legend/
#https://github.com/plotly/plotly.R/issues/1049

if(!require(eurostat)) install.packages("eurostat", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(fpp2)) install.packages("fpp2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")

library(eurostat) # package used for querrying EUROSTAT
library(dplyr) # package used for data-manipulation
library(ggplot2) # package used for plotting
library(fpp2) # package used for forecasting
library(lubridate) #For handling dates
library(ggplot2)   #For plotting of results
library(plotly) #interactive plotting

# clear old variables from memory
rm(list=ls())

################################################################################
################################################################################
################################################################################

#Persons at risk of poverty or social exclusion by age and sex - EU 2020 strategy


# Y_LT6	Less than 6 years
# Y6-10	From 6 to 10 years
# Y6-11	From 6 to 11 years
# Y11-15	From 11 to 15 years
# Y12-17	From 12 to 17 years
# Y15-19	From 15 to 19 years
# Y15-24	From 15 to 24 years
# Y15-29	From 15 to 29 years
# Y_LT16	Less than 16 years
# Y16-19	From 16 to 19 years
# Y16-24	From 16 to 24 years
# Y16-29	From 16 to 29 years
# Y16-64	From 16 to 64 years
# Y_GE16	16 years or over
# Y_LT18	Less than 18 years
# Y18-24	From 18 to 24 years
# Y18-59	From 18 to 59 years
# Y18-64	From 18 to 64 years
# Y18-74	From 18 to 74 years
# Y_GE18	18 years or over
# Y20-24	From 20 to 24 years
# Y20-29	From 20 to 29 years
# Y25-29	From 25 to 29 years
# Y25-49	From 25 to 49 years
# Y25-59	From 25 to 59 years
# Y25-54	From 25 to 54 years
# Y50-59	From 50 to 59 years
# Y50-64	From 50 to 64 years
# Y55-64	From 55 to 64 years
# Y_GE55	55 years or over
# Y_LT60	Less than 60 years
# Y_GE60	60 years or over
# Y_LT65	Less than 65 years
# Y65-74	From 65 to 74 years
# Y_GE65	65 years or over
# Y_LT75	Less than 75 years
# Y_GE75	75 years or over

age <- c('Y_LT6', 'Y6-10', 'Y6-11', 'Y11-15', 'Y12-17', 'Y15-19', 'Y15-24', 
         'Y15-29', 'Y_LT16', 'Y16-19', 'Y16-24', 'Y16-29', 'Y16-64', 'Y_GE16',
         'Y_LT18', 'Y18-24', 'Y18-29', 'Y18-49', 'Y18-59', 'Y18-64', 'Y18-74', 
         'Y_GE18', 'Y20-24', 'Y20-29', 'Y25-29', 'Y25-34',
         'Y25-49', 'Y25-54', 'Y25-59', 
         'Y35-44', 'Y45-54',
         'Y50-59', 'Y50-64', 'Y55-64', 'Y_GE55', 'Y_LT60', 'Y_GE60',
         'Y_LT65', 'Y65-74', 'Y_GE65', 'Y_LT75', 'Y_GE75', 'Y_GE85')
age_description <- c('Less than 6 years', 'From 6 to 10 years', 'From 6 to 11 years',
                     'From 11 to 15 years', 'From 12 to 17 years', 'From 15 to 19 years',
                     'From 15 to 24 years', 'From 15 to 29 years', 'Less than 16 years',
                     'From 16 to 19 years', 'From 16 to 24 years', 'From 16 to 29 years',
                     'From 16 to 64 years', '16 years or over', 'Less than 18 years',
                     'From 18 to 24 years', 'From 18 to 29 years', 'From 18 to 49 years', 
                     'From 18 to 59 years', 'From 18 to 64 years', 'From 18 to 74 years', '18 years or over',
                     'From 20 to 24 years', 'From 20 to 29 years', 'From 25 to 29 years', 'From 25 to 34 years',
                     'From 25 to 49 years', 'From 25 to 54 years', 'From 25 to 59 years', 
                     'From 35 to 44 years', 'From 45 to 54 years',
                     'From 50 to 59 years', 'From 50 to 64 years',
                     'From 55 to 64 years', '55 years or over', 'Less than 60 years',
                     '60 years or over',
                     'Less than 65 years', 'From 65 to 74 years', '65 years or over',
                     'Less than 75 years', '75 years or over', '85 years or over')

df_age <- data.frame(age, age_description)

getAgeDescription <- function(age_value) {
  df_age %>% filter(age == age_value)
}

dt_ilc_peps01 <- get_eurostat("ilc_peps01", stringsAsFactors = FALSE)

dt_ilc_peps01_it <- dt_ilc_peps01 %>% filter(geo == 'IT')

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% filter(unit == 'PC')

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% filter(sex == 'F')

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% mutate(year = year(time))

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% select(age, year, values)

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% filter(age != 'TOTAL')

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_peps01_it[[4]][dt_ilc_peps01_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% select(age_description, year, values)

colnames(dt_ilc_peps01_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_peps01_it, aes(year, values, color=age)) +
    ggtitle('Italy - Women at risk of poverty or social exclusion by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_peps01_it <- dt_ilc_peps01 %>% filter(geo == 'IT')

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% filter(unit == 'PC')

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% filter(sex == 'M')

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% mutate(year = year(time))

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% select(age, year, values)

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% filter(age != 'TOTAL')

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_peps01_it[[4]][dt_ilc_peps01_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_peps01_it <- dt_ilc_peps01_it %>% select(age_description, year, values)

colnames(dt_ilc_peps01_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_peps01_it, aes(year, values, color=age)) +
    ggtitle('Italy - Men at risk of poverty or social exclusion by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_peps01_eu <- dt_ilc_peps01 %>% filter(geo == 'EU')

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% filter(unit == 'PC')

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% filter(sex == 'F')

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% mutate(year = year(time))

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% select(age, year, values)

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% filter(age != 'TOTAL')

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_peps01_eu[[4]][dt_ilc_peps01_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% select(age_description, year, values)

colnames(dt_ilc_peps01_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_peps01_eu, aes(year, values, color=age)) +
    ggtitle('Europe - Women at risk of poverty or social exclusion by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_peps01_eu <- dt_ilc_peps01 %>% filter(geo == 'EU')

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% filter(unit == 'PC')

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% filter(sex == 'M')

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% mutate(year = year(time))

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% select(age, year, values)

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% filter(age != 'TOTAL')

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_peps01_eu[[4]][dt_ilc_peps01_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_peps01_eu <- dt_ilc_peps01_eu %>% select(age_description, year, values)

colnames(dt_ilc_peps01_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_peps01_eu, aes(year, values, color=age)) +
    ggtitle('Europe - Men at risk of poverty or social exclusion by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
##Persons at risk of poverty or social exclusion by most frequent activity
##status (population aged 18 and over) - EU 2020 strategy
################################################################################

# Y18-24	From 18 to 24 years
# Y18-59	From 18 to 59 years
# Y18-64	From 18 to 64 years
# Y18-74	From 18 to 74 years
# Y_GE18	18 years or over
# Y25-49	From 25 to 49 years
# Y25-54	From 25 to 54 years
# Y25-59	From 25 to 59 years
# Y50-59	From 50 to 59 years
# Y50-64	From 50 to 64 years
# Y_GE55	55 years or over
# Y_GE60	60 years or over
# Y65-74	From 65 to 74 years
# Y_GE65	65 years or over
# Y_GE75	75 years or over

# POP	Population
# EMP	Employed persons
# SAL	Employees
# NSAL	Employed persons except employees
# NEMP	Not employed persons
# UNE	Unemployed persons
# RET	Retired persons
# INAC_OTH	Other persons outside the labour force (former name: inactive persons)

dt_ilc_peps02 <- get_eurostat("ilc_peps02", stringsAsFactors = FALSE)

dt_ilc_peps02_it <- dt_ilc_peps02 %>% filter(geo == 'IT')

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% filter(sex == 'F') 

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% filter(age == 'Y_GE18') 

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% mutate(year = year(time))

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% select(wstatus, year, values)

dt_ilc_peps02_it <- dt_ilc_peps02_it %>%
  mutate(status = case_when(
    .$wstatus == 'POP' ~ 'Population',
    .$wstatus == 'EMP' ~ 'Employed persons',
    .$wstatus == 'SAL' ~ 'Employees',
    .$wstatus == 'NSAL' ~ 'Employed persons except employees',
    .$wstatus == 'NEMP' ~ 'Not employed persons',
    .$wstatus == 'UNE' ~ 'Unemployed persons',
    .$wstatus == 'RET' ~ 'Retired persons',
    TRUE ~ 'Other persons outside the labour force (former name: inactive persons)'))

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% select(status, year, values)

ggplotly(
  ggplot(dt_ilc_peps02_it, aes(year, values, color=status)) +
    ggtitle('Italy - Women (18 years or over) at risk of poverty or social exclusion by most frequent activity status') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_peps02_it <- dt_ilc_peps02 %>% filter(geo == 'IT')

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% filter(sex == 'F') 

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% filter(wstatus == 'UNE') 

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% mutate(year = year(time))

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% select(age, year, values)

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_peps02_it[[4]][dt_ilc_peps02_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% select(age_description, year, values)

colnames(dt_ilc_peps02_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_peps02_it, aes(year, values, color=age)) +
    ggtitle('Italy - Unemployed women (18 years or over) at risk of poverty or social exclusion by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_peps02_it <- dt_ilc_peps02 %>% filter(geo == 'IT')

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% filter(sex == 'M') 

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% filter(age == 'Y_GE18') 

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% mutate(year = year(time))

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% select(wstatus, year, values)

dt_ilc_peps02_it <- dt_ilc_peps02_it %>%
  mutate(status = case_when(
    .$wstatus == 'POP' ~ 'Population',
    .$wstatus == 'EMP' ~ 'Employed persons',
    .$wstatus == 'SAL' ~ 'Employees',
    .$wstatus == 'NSAL' ~ 'Employed persons except employees',
    .$wstatus == 'NEMP' ~ 'Not employed persons',
    .$wstatus == 'UNE' ~ 'Unemployed persons',
    .$wstatus == 'RET' ~ 'Retired persons',
    TRUE ~ 'Other persons outside the labour force (former name: inactive persons)'))

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% select(status, year, values)

ggplotly(
  ggplot(dt_ilc_peps02_it, aes(year, values, color=status)) +
    ggtitle('Italy - Men (18 years or over) at risk of poverty or social exclusion by most frequent activity status') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_peps02_it <- dt_ilc_peps02 %>% filter(geo == 'IT')

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% filter(sex == 'M') 

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% filter(wstatus == 'UNE') 

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% mutate(year = year(time))

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% select(age, year, values)

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_peps02_it[[4]][dt_ilc_peps02_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_peps02_it <- dt_ilc_peps02_it %>% select(age_description, year, values)

colnames(dt_ilc_peps02_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_peps02_it, aes(year, values, color=age)) +
    ggtitle('Italy - Unemployed men (18 years or over) at risk of poverty or social exclusion by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_peps02_eu <- dt_ilc_peps02 %>% filter(geo == 'EU')

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% filter(sex == 'F') 

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% filter(age == 'Y_GE18') 

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% mutate(year = year(time))

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% select(wstatus, year, values)

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% select(wstatus, year, values)

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>%
  mutate(status = case_when(
    .$wstatus == 'POP' ~ 'Population',
    .$wstatus == 'EMP' ~ 'Employed persons',
    .$wstatus == 'SAL' ~ 'Employees',
    .$wstatus == 'NSAL' ~ 'Employed persons except employees',
    .$wstatus == 'NEMP' ~ 'Not employed persons',
    .$wstatus == 'UNE' ~ 'Unemployed persons',
    .$wstatus == 'RET' ~ 'Retired persons',
    TRUE ~ 'Other persons outside the labour force (former name: inactive persons)'))

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% select(status, year, values)

ggplotly(
  ggplot(dt_ilc_peps02_eu, aes(year, values, color=status)) +
    ggtitle('Europe - Women (18 years or over) at risk of poverty or social exclusion by most frequent activity status') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_peps02_eu <- dt_ilc_peps02 %>% filter(geo == 'EU')

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% filter(sex == 'F') 

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% filter(wstatus == 'UNE') 

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% mutate(year = year(time))

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% select(age, year, values)

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_peps02_eu[[4]][dt_ilc_peps02_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% select(age_description, year, values)

colnames(dt_ilc_peps02_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_peps02_eu, aes(year, values, color=age)) +
    ggtitle('Europe - Unemployed women (18 years or over) at risk of poverty or social exclusion by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_peps02_eu <- dt_ilc_peps02 %>% filter(geo == 'EU')

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% filter(sex == 'M') 

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% filter(age == 'Y_GE18') 

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% mutate(year = year(time))

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% select(wstatus, year, values)

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>%
  mutate(status = case_when(
    .$wstatus == 'POP' ~ 'Population',
    .$wstatus == 'EMP' ~ 'Employed persons',
    .$wstatus == 'SAL' ~ 'Employees',
    .$wstatus == 'NSAL' ~ 'Employed persons except employees',
    .$wstatus == 'NEMP' ~ 'Not employed persons',
    .$wstatus == 'UNE' ~ 'Unemployed persons',
    .$wstatus == 'RET' ~ 'Retired persons',
    TRUE ~ 'Other persons outside the labour force (former name: inactive persons)'))

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% select(status, year, values)

ggplotly(
  ggplot(dt_ilc_peps02_eu, aes(year, values, color=status)) +
    ggtitle('Europe - Men (18 years or over) at risk of poverty or social exclusion by most frequent activity status') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
  )

dt_ilc_peps02_eu <- dt_ilc_peps02 %>% filter(geo == 'EU')

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% filter(sex == 'M') 

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% filter(wstatus == 'UNE') 

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% mutate(year = year(time))

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% select(age, year, values)

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_peps02_eu[[4]][dt_ilc_peps02_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_peps02_eu <- dt_ilc_peps02_eu %>% select(age_description, year, values)

colnames(dt_ilc_peps02_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_peps02_eu, aes(year, values, color=age)) +
    ggtitle('Europe - Unemployed men (18 years or over) at risk of poverty or social exclusion by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)
################################################################################
######Persons at risk of poverty or social exclusion by income quantile and 
######household composition - EU 2020 strategy
################################################################################

# TOTAL	Total
# A1	Single person
# A1_LT65	One adult younger than 65 years
# A1_GE65	One adult 65 years or over
# A1_DCH	Single person with dependent children
# A1F	Single female
# A1M	Single male
# A2	Two adults
# A2_2LT65	Two adults younger than 65 years
# A2_GE1_GE65	Two adults, at least one aged 65 years or over
# A2_1DCH	Two adults with one dependent child
# A2_2DCH	Two adults with two dependent children
# A2_GE3DCH	Two adults with three or more dependent children
# A_GE2_NDCH	Two or more adults without dependent children
# A_GE2_DCH	Two or more adults with dependent children
# A_GE3	Three or more adults
# A_GE3_DCH	Three or more adults with dependent children
# HH_NDCH	Households without dependent children
# HH_DCH	Households with dependent children

dt_ilc_peps03 <- get_eurostat("ilc_peps03", stringsAsFactors = FALSE)

dt_ilc_peps03_it <- dt_ilc_peps03 %>% filter(geo == 'IT')

dt_ilc_peps03_it <- dt_ilc_peps03_it %>% filter(quantile == 'TOTAL') 

dt_ilc_peps03_it <- dt_ilc_peps03_it %>% mutate(year = year(time))

dt_ilc_peps03_it <- dt_ilc_peps03_it %>% select(hhtyp, year, values)

dt_ilc_peps03_it <- dt_ilc_peps03_it %>%
  mutate(household_composition = case_when(
    .$hhtyp == 'A1' ~ 'Single person',
    .$hhtyp == 'A1_LT65' ~ 'One adult younger than 65 years',
    .$hhtyp == 'A1_GE65' ~ 'One adult 65 years or over',
    .$hhtyp == 'A1_DCH' ~ 'Single person with dependent children',
    .$hhtyp == 'A1F' ~ 'Single female',
    .$hhtyp == 'A1M' ~ 'Single male',
    .$hhtyp == 'A2' ~ 'Two adults',
    .$hhtyp == 'A2_2LT65' ~ 'Two adults younger than 65 years',
    .$hhtyp == 'A2_GE1_GE65' ~ 'Two adults, at least one aged 65 years or over',
    .$hhtyp == 'A2_1DCH' ~ 'Two adults with one dependent child',
    .$hhtyp == 'A2_2DCH' ~ 'Two adults with two dependent children',
    .$hhtyp == 'A2_GE3DCH' ~ 'Two adults with three or more dependent children',
    .$hhtyp == 'A_GE2_NDCH' ~ 'Two or more adults without dependent children',
    .$hhtyp == 'A_GE2_DCH' ~ 'Two or more adults with dependent children',
    .$hhtyp == 'A_GE3' ~ 'Three or more adults',
    .$hhtyp == 'A_GE3_DCH' ~ 'Three or more adults with dependent children',
    .$hhtyp == 'HH_NDCH' ~ 'Households without dependent children',
    TRUE ~ 'Households with dependent children'))

dt_ilc_peps03_it <- dt_ilc_peps03_it %>% select(household_composition, year, values)

ggplotly(
  ggplot(dt_ilc_peps03_it, aes(year, values, color=household_composition)) +
    ggtitle('Italy - Persons at risk of poverty or social exclusion by household composition') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################

dt_ilc_peps03_eu <- dt_ilc_peps03 %>% filter(geo == 'EU')

dt_ilc_peps03_eu <- dt_ilc_peps03_eu %>% filter(quantile == 'TOTAL') 

dt_ilc_peps03_eu <- dt_ilc_peps03_eu %>% mutate(year = year(time))

dt_ilc_peps03_eu <- dt_ilc_peps03_eu %>% select(hhtyp, year, values)

dt_ilc_peps03_eu <- dt_ilc_peps03_eu %>%
  mutate(household_composition = case_when(
    .$hhtyp == 'A1' ~ 'Single person',
    .$hhtyp == 'A1_LT65' ~ 'One adult younger than 65 years',
    .$hhtyp == 'A1_GE65' ~ 'One adult 65 years or over',
    .$hhtyp == 'A1_DCH' ~ 'Single person with dependent children',
    .$hhtyp == 'A1F' ~ 'Single female',
    .$hhtyp == 'A1M' ~ 'Single male',
    .$hhtyp == 'A2' ~ 'Two adults',
    .$hhtyp == 'A2_2LT65' ~ 'Two adults younger than 65 years',
    .$hhtyp == 'A2_GE1_GE65' ~ 'Two adults, at least one aged 65 years or over',
    .$hhtyp == 'A2_1DCH' ~ 'Two adults with one dependent child',
    .$hhtyp == 'A2_2DCH' ~ 'Two adults with two dependent children',
    .$hhtyp == 'A2_GE3DCH' ~ 'Two adults with three or more dependent children',
    .$hhtyp == 'A_GE2_NDCH' ~ 'Two or more adults without dependent children',
    .$hhtyp == 'A_GE2_DCH' ~ 'Two or more adults with dependent children',
    .$hhtyp == 'A_GE3' ~ 'Three or more adults',
    .$hhtyp == 'A_GE3_DCH' ~ 'Three or more adults with dependent children',
    .$hhtyp == 'HH_NDCH' ~ 'Households without dependent children',
    TRUE ~ 'Households with dependent children'))

dt_ilc_peps03_eu <- dt_ilc_peps03_eu %>% select(household_composition, year, values)

ggplotly(
  ggplot(dt_ilc_peps03_eu, aes(year, values, color=household_composition)) +
    ggtitle('Europe - Persons at risk of poverty or social exclusion by household composition') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
####Persons at risk of poverty or social exclusion by tenure status - EU 2020 strategy
################################################################################

# TOTAL	Total
# OWN_L	Owner, with mortgage or loan
# OWN_NL	Owner, no outstanding mortgage or housing loan
# RENT_MKT	Tenant, rent at market price
# RENT_FR	Tenant, rent at reduced price or free

dt_ilc_peps07 <- get_eurostat("ilc_peps07", stringsAsFactors = FALSE)

dt_ilc_peps07_it <- dt_ilc_peps07 %>% filter(geo == 'IT')

dt_ilc_peps07_it <- dt_ilc_peps07_it %>% filter(unit == 'PC') 

dt_ilc_peps07_it <- dt_ilc_peps07_it %>% mutate(year = year(time))

dt_ilc_peps07_it <- dt_ilc_peps07_it %>% select(tenure, year, values)

dt_ilc_peps07_it <- dt_ilc_peps07_it %>% filter(tenure != 'TOTAL')

dt_ilc_peps07_it <- dt_ilc_peps07_it %>%
  mutate(tenure_status = case_when(
    .$tenure == 'OWN_L' ~ 'Owner, with mortgage or loann',
    .$tenure == 'OWN_NL' ~ 'Owner, no outstanding mortgage or housing loan',
    .$tenure == 'RENT_MKT' ~ 'Tenant, rent at market price',
    TRUE ~ 'Tenant, rent at reduced price or free'))

dt_ilc_peps07_it <- dt_ilc_peps07_it %>% select(tenure_status, year, values)

ggplotly(
  ggplot(dt_ilc_peps07_it, aes(year, values, color=tenure_status)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    ggtitle('Italy - Persons at risk of poverty or social exclusion by tenure status') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_peps07_eu <- dt_ilc_peps07 %>% filter(geo == 'EU')

dt_ilc_peps07_eu <- dt_ilc_peps07_eu %>% filter(unit == 'PC') 

dt_ilc_peps07_eu <- dt_ilc_peps07_eu %>% mutate(year = year(time))

dt_ilc_peps07_eu <- dt_ilc_peps07_eu %>% select(tenure, year, values)

dt_ilc_peps07_eu <- dt_ilc_peps07_eu %>% filter(tenure != 'TOTAL')

dt_ilc_peps07_eu <- dt_ilc_peps07_eu %>%
  mutate(tenure_status = case_when(
    .$tenure == 'OWN_L' ~ 'Owner, with mortgage or loann',
    .$tenure == 'OWN_NL' ~ 'Owner, no outstanding mortgage or housing loan',
    .$tenure == 'RENT_MKT' ~ 'Tenant, rent at market price',
    TRUE ~ 'Tenant, rent at reduced price or free'))

dt_ilc_peps07_eu <- dt_ilc_peps07_eu %>% select(tenure_status, year, values)

ggplotly(
  ggplot(dt_ilc_peps07_eu, aes(year, values, color=tenure_status)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    ggtitle('Europe - Persons at risk of poverty or social exclusion by tenure status') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
####Persons at risk of poverty or social exclusion by degree of urbanisation - EU 2020 strategy
################################################################################

# DEG1	Cities
# DEG2	Towns and suburbs
# DEG3	Rural areas

dt_ilc_peps13 <- get_eurostat("ilc_peps13", stringsAsFactors = FALSE)

dt_ilc_peps13_it <- dt_ilc_peps13 %>% filter(geo == 'IT')

dt_ilc_peps13_it <- dt_ilc_peps13_it %>% filter(unit == 'PC') 

dt_ilc_peps13_it <- dt_ilc_peps13_it %>% mutate(year = year(time))

dt_ilc_peps13_it <- dt_ilc_peps13_it %>% select(deg_urb, year, values)

dt_ilc_peps13_it <- dt_ilc_peps13_it %>%
  mutate(urbanisation_degree = case_when(
    .$deg_urb == 'DEG1' ~ 'Cities',
    .$deg_urb == 'DEG2' ~ 'Towns and suburbs',
    TRUE ~ 'Rural areas'))

dt_ilc_peps13_it <- dt_ilc_peps13_it %>% select(urbanisation_degree, year, values)

ggplotly(
  ggplot(dt_ilc_peps13_it, aes(year, values, color=urbanisation_degree)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    ggtitle('Italy - Persons at risk of poverty or social exclusion by degree of urbanisation') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_peps13_eu <- dt_ilc_peps13 %>% filter(geo == 'EU')

dt_ilc_peps13_eu <- dt_ilc_peps13_eu %>% filter(unit == 'PC') 

dt_ilc_peps13_eu <- dt_ilc_peps13_eu %>% mutate(year = year(time))

dt_ilc_peps13_eu <- dt_ilc_peps13_eu %>% select(deg_urb, year, values)

dt_ilc_peps13_eu <- dt_ilc_peps13_eu %>% select(deg_urb, year, values)

dt_ilc_peps13_eu <- dt_ilc_peps13_eu %>%
  mutate(urbanisation_degree = case_when(
    .$deg_urb == 'DEG1' ~ 'Cities',
    .$deg_urb == 'DEG2' ~ 'Towns and suburbs',
    TRUE ~ 'Rural areas'))

dt_ilc_peps13_eu <- dt_ilc_peps13_eu %>% select(urbanisation_degree, year, values)

ggplotly(
  ggplot(dt_ilc_peps13_eu, aes(year, values, color=urbanisation_degree)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    ggtitle('Europe - Persons at risk of poverty or social exclusion by degree of urbanisation') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


################################################################################
##Children at risk of poverty or social exclusion by educational attainment 
##level of their parents (population aged 0 to 17 years) - EU 2020 strategy
################################################################################

##AGE
# Y_LT6	Less than 6 years
# Y_LT18	Less than 18 years
# Y6-11	From 6 to 11 years
# Y12-17	From 12 to 17 years

##ISCED11
# ED0-2	Less than primary, primary and lower secondary education (levels 0-2)
# ED3_4	Upper secondary and post-secondary non-tertiary education (levels 3 and 4)
# ED5-8	Tertiary education (levels 5-8)

dt_ilc_peps60 <- get_eurostat("ilc_peps60", stringsAsFactors = FALSE)

dt_ilc_peps60_it <- dt_ilc_peps60 %>% filter(geo == 'IT')

dt_ilc_peps60_it <- dt_ilc_peps60_it %>% filter(age == 'Y_LT18') 

dt_ilc_peps60_it <- dt_ilc_peps60_it %>% mutate(year = year(time))

dt_ilc_peps60_it <- dt_ilc_peps60_it %>% select(isced11, year, values)

dt_ilc_peps60_it <- dt_ilc_peps60_it %>%
  mutate(educational_attainment = case_when(
    .$isced11 == 'ED0-2' ~ 'Less than primary, primary and lower secondary education (levels 0-2)',
    .$isced11 == 'ED3_4' ~ 'Upper secondary and post-secondary non-tertiary education (levels 3 and 4)',
    TRUE ~ 'Tertiary education (levels 5-8)'))

dt_ilc_peps60_it <- dt_ilc_peps60_it %>% select(educational_attainment, year, values)

ggplotly(
  ggplot(dt_ilc_peps60_it, aes(year, values, color=educational_attainment)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    ggtitle('Italy - Children at risk of poverty or social exclusion by educational attainment level of their parents') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_peps60_eu <- dt_ilc_peps60 %>% filter(geo == 'EU')

dt_ilc_peps60_eu <- dt_ilc_peps60_eu %>% filter(age == 'Y_LT18') 

dt_ilc_peps60_eu <- dt_ilc_peps60_eu %>% mutate(year = year(time))

dt_ilc_peps60_eu <- dt_ilc_peps60_eu %>% select(isced11, year, values)

dt_ilc_peps60_eu <- dt_ilc_peps60_eu %>%
  mutate(educational_attainment = case_when(
    .$isced11 == 'ED0-2' ~ 'Less than primary, primary and lower secondary education (levels 0-2)',
    .$isced11 == 'ED3_4' ~ 'Upper secondary and post-secondary non-tertiary education (levels 3 and 4)',
    TRUE ~ 'Tertiary education (levels 5-8)'))

dt_ilc_peps60_eu <- dt_ilc_peps60_eu %>% select(educational_attainment, year, values)

ggplotly(
  ggplot(dt_ilc_peps60_eu, aes(year, values, color=educational_attainment)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    ggtitle('Europe - Children at risk of poverty or social exclusion by educational attainment level of their parents') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
##Persons by risk of poverty, material deprivation, work intensity of the 
##household, age and sex of the person - intersections of EU 2020 Strategy 
##poverty target indicator
################################################################################

##age
# TOTAL	Total
# Y_LT18	Less than 18 years
# Y18-29	From 18 to 29 years
# Y18-49	From 18 to 49 years
# Y18-59	From 18 to 59 years
# Y18-64	From 18 to 64 years
# Y_GE18	18 years or over
# Y25-54	From 25 to 54 years
# Y50-64	From 50 to 64 years
# Y55-64	From 55 to 64 years
# Y_GE65	65 years or over

##lev_depr
# SEV	Severe
# NSEV	Non severe

##sex
# T	Total
# M	Males
# F	Females

##workint
# WI02-1_NAP	Not very low work intensity (0.2-1) and not applicable
# WI0-02	Very low work intensity (0-0.2)

##yn_rskpov
# YES_ARP	At risk of poverty
# NO_ARP	Not at risk of poverty

dt_ilc_pees01 <- get_eurostat("ilc_pees01", stringsAsFactors = FALSE)

dt_ilc_pees01_it <- dt_ilc_pees01 %>% filter(geo == 'IT')

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(unit == 'PC') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(sex == 'F') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(lev_depr == 'SEV') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(yn_rskpov == 'YES_ARP') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% mutate(year = year(time))

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% select(workint, age, year, values)

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(age == 'TOTAL')

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% select(workint, year, values)

dt_ilc_pees01_it <- dt_ilc_pees01_it %>%
  mutate(work_intensity = case_when(
    .$workint == 'WI02-1_NAP' ~ 'Not very low work intensity (0.2-1) and not applicable',
    TRUE ~ 'Very low work intensity (0-0.2)'))

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% select(work_intensity, year, values)

ggplotly(
  ggplot(dt_ilc_pees01_it, aes(year, values, color=work_intensity)) +
    ggtitle('Italy - Women at risk of poverty, material deprivation, work intensity of the household') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_pees01_it <- dt_ilc_pees01 %>% filter(geo == 'IT')

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(unit == 'PC') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(sex == 'F') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(lev_depr == 'SEV') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(yn_rskpov == 'YES_ARP') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% mutate(year = year(time))

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(workint == 'WI02-1_NAP')

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% select(age, year, values)

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_pees01_it[[4]][dt_ilc_pees01_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% select(age_description, year, values)

colnames(dt_ilc_pees01_it) <- c('age', 'year', 'values')

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(age != 'TOTAL')

ggplotly(
  ggplot(dt_ilc_pees01_it, aes(year, values, color=age)) +
    ggtitle('Italy - Women at risk of poverty, with not very low household work intensity and material deprivation, by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


dt_ilc_pees01_it <- dt_ilc_pees01 %>% filter(geo == 'IT')

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(unit == 'PC') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(sex == 'M') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(lev_depr == 'SEV') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(yn_rskpov == 'YES_ARP') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% mutate(year = year(time))

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% select(workint, age, year, values)

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(age == 'TOTAL')

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% select(workint, year, values)

dt_ilc_pees01_it <- dt_ilc_pees01_it %>%
  mutate(work_intensity = case_when(
    .$workint == 'WI02-1_NAP' ~ 'Not very low work intensity (0.2-1) and not applicable',
    TRUE ~ 'Very low work intensity (0-0.2)'))

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% select(work_intensity, year, values)

ggplotly(
  ggplot(dt_ilc_pees01_it, aes(year, values, color=work_intensity)) +
    ggtitle('Italy - Men at risk of poverty, material deprivation, work intensity of the household') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_pees01_it <- dt_ilc_pees01 %>% filter(geo == 'IT')

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(unit == 'PC') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(sex == 'M') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(lev_depr == 'SEV') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(yn_rskpov == 'YES_ARP') 

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% mutate(year = year(time))

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(workint == 'WI02-1_NAP')

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% select(age, year, values)

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_pees01_it[[4]][dt_ilc_pees01_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% select(age_description, year, values)

colnames(dt_ilc_pees01_it) <- c('age', 'year', 'values')

dt_ilc_pees01_it <- dt_ilc_pees01_it %>% filter(age != 'TOTAL')

ggplotly(
  ggplot(dt_ilc_pees01_it, aes(year, values, color=age)) +
    ggtitle('Italy - Men at risk of poverty, with not very low household work intensity and material deprivation, by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_pees01_eu <- dt_ilc_pees01 %>% filter(geo == 'EU')

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(unit == 'PC') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(sex == 'F') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(lev_depr == 'SEV') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(yn_rskpov == 'YES_ARP') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% mutate(year = year(time))

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% select(workint, age, year, values)

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(age == 'TOTAL')

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% select(workint, year, values)

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>%
  mutate(work_intensity = case_when(
    .$workint == 'WI02-1_NAP' ~ 'Not very low work intensity (0.2-1) and not applicable',
    TRUE ~ 'Very low work intensity (0-0.2)'))

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% select(work_intensity, year, values)

ggplotly(
  ggplot(dt_ilc_pees01_eu, aes(year, values, color=work_intensity)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    ggtitle('Europe - Women at risk of poverty, material deprivation, work intensity of the household') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


dt_ilc_pees01_eu <- dt_ilc_pees01 %>% filter(geo == 'EU')

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(unit == 'PC') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(sex == 'F') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(lev_depr == 'SEV') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(yn_rskpov == 'YES_ARP') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% mutate(year = year(time))

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(workint == 'WI02-1_NAP')

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% select(age, year, values)

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_pees01_eu[[4]][dt_ilc_pees01_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% select(age_description, year, values)

colnames(dt_ilc_pees01_eu) <- c('age', 'year', 'values')

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(age != 'TOTAL')

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% 
  filter(age %in% c('Less than 18 years', '65 years or over', 'From 55 to 64 years'))

ggplotly(
  ggplot(dt_ilc_pees01_eu, aes(year, values, color=age)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    ggtitle('Europe - Women at risk of poverty, with not very low household work intensity and material deprivation, by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


dt_ilc_pees01_eu <- dt_ilc_pees01 %>% filter(geo == 'EU')

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(unit == 'PC') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(sex == 'M') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(lev_depr == 'SEV') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(yn_rskpov == 'YES_ARP') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% mutate(year = year(time))

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% select(workint, age, year, values)

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(age == 'TOTAL')

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% select(workint, year, values)

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>%
  mutate(work_intensity = case_when(
    .$workint == 'WI02-1_NAP' ~ 'Not very low work intensity (0.2-1) and not applicable',
    TRUE ~ 'Very low work intensity (0-0.2)'))

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% select(work_intensity, year, values)

ggplotly(
  ggplot(dt_ilc_pees01_eu, aes(year, values, color=work_intensity)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    ggtitle('Europe - Men at risk of poverty, material deprivation, work intensity of the household') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_pees01_eu <- dt_ilc_pees01 %>% filter(geo == 'EU')

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(unit == 'PC') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(sex == 'M') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(lev_depr == 'SEV') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(yn_rskpov == 'YES_ARP') 

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% mutate(year = year(time))

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(workint == 'WI02-1_NAP')

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% select(age, year, values)

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_pees01_eu[[4]][dt_ilc_pees01_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% select(age_description, year, values)

colnames(dt_ilc_pees01_eu) <- c('age', 'year', 'values')

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% filter(age != 'TOTAL')

dt_ilc_pees01_eu <- dt_ilc_pees01_eu %>% 
  filter(age %in% c('Less than 18 years', '65 years or over', 'From 55 to 64 years'))

ggplotly(
  ggplot(dt_ilc_pees01_eu, aes(year, values, color=age)) +
    geom_smooth(formula = y ~ x, method = "lm") +
    ggtitle('Europe - Men at risk of poverty, with not very low household work intensity and material deprivation, by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###########################IN-WORK POVERTY######################################
################################################################################

################################################################################
##In-work at-risk-of-poverty rate by age and sex - EU-SILC survey
################################################################################

##AGE
# Y16-19	From 16 to 19 years
# Y16-24	From 16 to 24 years
# Y16-29	From 16 to 29 years
# Y18-24	From 18 to 24 years
# Y18-64	From 18 to 64 years
# Y_GE18	18 years or over
# Y20-24	From 20 to 24 years
# Y20-29	From 20 to 29 years
# Y25-29	From 25 to 29 years
# Y25-54	From 25 to 54 years
# Y55-64	From 55 to 64 years
# Y_GE65	65 years or over

##wstatus
# EMP	Employed persons
# SAL	Employees
# NSAL	Employed persons except employees


dt_ilc_iw01 <- get_eurostat("ilc_iw01", stringsAsFactors = FALSE)

dt_ilc_iw01_it <- dt_ilc_iw01 %>% filter(geo == 'IT')

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% filter(sex == 'F') 

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% filter(wstatus == 'EMP') 

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% mutate(year = year(time))

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% select(age, year, values)

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_iw01_it[[4]][dt_ilc_iw01_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% select(age_description, year, values)

colnames(dt_ilc_iw01_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_iw01_it %>% filter(age != 'From 16 to 19 years'), aes(year, values, color=age)) +
    ggtitle('Italy - Employed women at risk of poverty rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)



dt_ilc_iw01_it <- dt_ilc_iw01 %>% filter(geo == 'IT')

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% filter(sex == 'M') 

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% filter(wstatus == 'EMP') 

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% mutate(year = year(time))

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% select(age, year, values)

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_iw01_it[[4]][dt_ilc_iw01_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% select(age_description, year, values)

colnames(dt_ilc_iw01_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_iw01_it %>% filter(age != 'From 16 to 19 years'), aes(year, values, color=age)) +
    ggtitle('Italy - Employed men at risk of poverty rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)



dt_ilc_iw01_it <- dt_ilc_iw01 %>% filter(geo == 'IT')

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% filter(sex == 'F') 

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% filter(wstatus == 'SAL') 

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% mutate(year = year(time))

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% select(age, year, values)

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_iw01_it[[4]][dt_ilc_iw01_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% select(age_description, year, values)

colnames(dt_ilc_iw01_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_iw01_it %>% filter(age != '65 years or over'), aes(year, values, color=age)) +
    ggtitle('Italy - Employees women at risk of poverty rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


dt_ilc_iw01_it <- dt_ilc_iw01 %>% filter(geo == 'IT')

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% filter(sex == 'M') 

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% filter(wstatus == 'SAL') 

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% mutate(year = year(time))

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% select(age, year, values)

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_iw01_it[[4]][dt_ilc_iw01_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% select(age_description, year, values)

colnames(dt_ilc_iw01_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_iw01_it %>% filter(age != '65 years or over'), aes(year, values, color=age)) +
    ggtitle('Italy - Employees men at risk of poverty rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)



dt_ilc_iw01_it <- dt_ilc_iw01 %>% filter(geo == 'IT')

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% filter(sex == 'F') 

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% filter(wstatus == 'NSAL') 

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% mutate(year = year(time))

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% select(age, year, values)

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_iw01_it[[4]][dt_ilc_iw01_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% select(age_description, year, values)

colnames(dt_ilc_iw01_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_iw01_it %>% filter(age != 'From 16 to 24 years') %>% 
           filter(age != 'From 18 to 24 years') %>% 
           filter(age != 'From 20 to 24 years'), aes(year, values, color=age)) +
    ggtitle('Italy - Employed women except employees at risk of poverty rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


dt_ilc_iw01_it <- dt_ilc_iw01 %>% filter(geo == 'IT')

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% filter(sex == 'M') 

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% filter(wstatus == 'NSAL') 

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% mutate(year = year(time))

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% select(age, year, values)

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_iw01_it[[4]][dt_ilc_iw01_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_iw01_it <- dt_ilc_iw01_it %>% select(age_description, year, values)

colnames(dt_ilc_iw01_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_iw01_it %>% filter(age != 'From 16 to 24 years') %>% 
           filter(age != 'From 18 to 24 years') %>% 
           filter(age != 'From 20 to 24 years'), aes(year, values, color=age)) +
    ggtitle('Italy - Employed men except employees at risk of poverty rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)
################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_iw01_eu <- dt_ilc_iw01 %>% filter(geo == 'EU')

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% filter(sex == 'F') 

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% filter(wstatus == 'EMP') 

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% mutate(year = year(time))

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% select(age, year, values)

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_iw01_eu[[4]][dt_ilc_iw01_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% select(age_description, year, values)

colnames(dt_ilc_iw01_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_iw01_eu %>% filter(age != '65 years or over'), aes(year, values, color=age)) +
    ggtitle('Europe - Employed women at risk of poverty rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


dt_ilc_iw01_eu <- dt_ilc_iw01 %>% filter(geo == 'EU')

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% filter(sex == 'M') 

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% filter(wstatus == 'EMP') 

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% mutate(year = year(time))

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% select(age, year, values)

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_iw01_eu[[4]][dt_ilc_iw01_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% select(age_description, year, values)

colnames(dt_ilc_iw01_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_iw01_eu %>% filter(age != 'From 16 to 19 years'), aes(year, values, color=age)) +
    ggtitle('Europe - Employed men at risk of poverty rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


dt_ilc_iw01_eu <- dt_ilc_iw01 %>% filter(geo == 'EU')

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% filter(sex == 'F') 

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% filter(wstatus == 'SAL') 

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% mutate(year = year(time))

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% select(age, year, values)

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_iw01_eu[[4]][dt_ilc_iw01_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% select(age_description, year, values)

colnames(dt_ilc_iw01_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_iw01_eu, aes(year, values, color=age)) +
    ggtitle('Europe - Employees women at risk of poverty rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_iw01_eu <- dt_ilc_iw01 %>% filter(geo == 'EU')

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% filter(sex == 'M') 

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% filter(wstatus == 'SAL') 

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% mutate(year = year(time))

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% select(age, year, values)

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_iw01_eu[[4]][dt_ilc_iw01_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% select(age_description, year, values)

colnames(dt_ilc_iw01_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_iw01_eu %>% filter(age != 'From 16 to 19 years') %>% filter(age != '65 years or over'), aes(year, values, color=age)) +
    ggtitle('Europe - Employees men at risk of poverty rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_iw01_eu <- dt_ilc_iw01 %>% filter(geo == 'EU')

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% filter(sex == 'F') 

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% filter(wstatus == 'NSAL') 

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% mutate(year = year(time))

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% select(age, year, values)

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_iw01_eu[[4]][dt_ilc_iw01_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% select(age_description, year, values)

colnames(dt_ilc_iw01_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_iw01_eu, aes(year, values, color=age)) +
    ggtitle('Europe - Employed women except employees at risk of poverty rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_iw01_eu <- dt_ilc_iw01 %>% filter(geo == 'EU')

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% filter(sex == 'M') 

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% filter(wstatus == 'NSAL') 

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% mutate(year = year(time))

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% select(age, year, values)

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_iw01_eu[[4]][dt_ilc_iw01_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_iw01_eu <- dt_ilc_iw01_eu %>% select(age_description, year, values)

colnames(dt_ilc_iw01_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_iw01_eu %>% filter(age != 'From 16 to 29 years')
         %>% filter(age != 'From 20 to 29 years')
         %>% filter(age != '65 years or over'), aes(year, values, color=age)) +
    ggtitle('Europe - Employed men except employees at risk of poverty rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
####In-work at-risk-of-poverty rate by type of contract - EU-SILC survey
################################################################################
dt_ilc_iw05 <- get_eurostat("ilc_iw05", stringsAsFactors = FALSE)

dt_ilc_iw05_it <- dt_ilc_iw05 %>% filter(geo == 'IT')

dt_ilc_iw05_it <- dt_ilc_iw05_it %>% filter(sex == 'F') 

dt_ilc_iw05_it <- dt_ilc_iw05_it %>% mutate(year = year(time))

dt_ilc_iw05_it <- dt_ilc_iw05_it %>% select(wstatus, year, values)

dt_ilc_iw05_it <- dt_ilc_iw05_it %>%
  mutate(contract_type = case_when(
    .$wstatus == 'SAL_PERM' ~ 'Employees with a permanent job',
    TRUE ~ 'Employees with a temporary job'))

dt_ilc_iw05_it <- dt_ilc_iw05_it %>% select(contract_type, year, values)

ggplotly(
  ggplot(dt_ilc_iw05_it, aes(year, values, color=contract_type)) +
    ggtitle('Italy - Women at risk of poverty rate by contract type') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


dt_ilc_iw05_it <- dt_ilc_iw05 %>% filter(geo == 'IT')

dt_ilc_iw05_it <- dt_ilc_iw05_it %>% filter(sex == 'M') 

dt_ilc_iw05_it <- dt_ilc_iw05_it %>% mutate(year = year(time))

dt_ilc_iw05_it <- dt_ilc_iw05_it %>% select(wstatus, year, values)

dt_ilc_iw05_it <- dt_ilc_iw05_it %>%
  mutate(contract_type = case_when(
    .$wstatus == 'SAL_PERM' ~ 'Employees with a permanent job',
    TRUE ~ 'Employees with a temporary job'))

dt_ilc_iw05_it <- dt_ilc_iw05_it %>% select(contract_type, year, values)

ggplotly(
  ggplot(dt_ilc_iw05_it, aes(year, values, color=contract_type)) +
    ggtitle('Italy - Men at risk of poverty rate by contract type') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_iw05_eu <- dt_ilc_iw05 %>% filter(geo == 'EU')

dt_ilc_iw05_eu <- dt_ilc_iw05_eu %>% filter(sex == 'F') 

dt_ilc_iw05_eu <- dt_ilc_iw05_eu %>% mutate(year = year(time))

dt_ilc_iw05_eu <- dt_ilc_iw05_eu %>% select(wstatus, year, values)

dt_ilc_iw05_eu <- dt_ilc_iw05_eu %>%
  mutate(contract_type = case_when(
    .$wstatus == 'SAL_PERM' ~ 'Employees with a permanent job',
    TRUE ~ 'Employees with a temporary job'))

dt_ilc_iw05_eu <- dt_ilc_iw05_eu %>% select(contract_type, year, values)

ggplotly(
  ggplot(dt_ilc_iw05_eu, aes(year, values, color=contract_type)) +
    ggtitle('Europe - Women at risk of poverty rate by contract type') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_iw05_eu <- dt_ilc_iw05 %>% filter(geo == 'EU')

dt_ilc_iw05_eu <- dt_ilc_iw05_eu %>% filter(sex == 'M') 

dt_ilc_iw05_eu <- dt_ilc_iw05_eu %>% mutate(year = year(time))

dt_ilc_iw05_eu <- dt_ilc_iw05_eu %>% select(wstatus, year, values)

dt_ilc_iw05_eu <- dt_ilc_iw05_eu %>%
  mutate(contract_type = case_when(
    .$wstatus == 'SAL_PERM' ~ 'Employees with a permanent job',
    TRUE ~ 'Employees with a temporary job'))

dt_ilc_iw05_eu <- dt_ilc_iw05_eu %>% select(contract_type, year, values)

ggplotly(
  ggplot(dt_ilc_iw05_eu, aes(year, values, color=contract_type)) +
    ggtitle('Europe - Men at risk of poverty rate by contract type') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


################################################################################
##Material and social deprivation rate by age, 
##sex and most frequent activity status
################################################################################

##WSTATUS
# EMP	Employed persons
# SAL	Employees
# NSAL	Employed persons except employees
# NEMP	Not employed persons
# UNE	Unemployed persons
# RET	Retired persons
# INAC_OTH	Other persons outside the labour force (former name: inactive persons)

##AGE
# Y16-19	From 16 to 19 years
# Y16-24	From 16 to 24 years
# Y16-29	From 16 to 29 years
# Y16-64	From 16 to 64 years
# Y_GE16	16 years or over
# Y18-24	From 18 to 24 years
# Y18-59	From 18 to 59 years
# Y18-64	From 18 to 64 years
# Y18-74	From 18 to 74 years
# Y_GE18	18 years or over
# Y20-24	From 20 to 24 years
# Y20-29	From 20 to 29 years
# Y25-29	From 25 to 29 years
# Y25-34	From 25 to 34 years
# Y25-49	From 25 to 49 years
# Y25-54	From 25 to 54 years
# Y25-59	From 25 to 59 years
# Y35-44	From 35 to 44 years
# Y45-54	From 45 to 54 years
# Y50-64	From 50 to 64 years
# Y55-64	From 55 to 64 years
# Y_GE55	55 years or over
# Y_GE60	60 years or over
# Y65-74	From 65 to 74 years
# Y_GE65	65 years or over
# Y_GE75	75 years or over
# Y_GE85	85 years or over

# We focus on UNE	Unemployed persons

dt_ilc_mdsd01 <- get_eurostat("ilc_mdsd01", stringsAsFactors = FALSE)

dt_ilc_mdsd01_it <- dt_ilc_mdsd01 %>% filter(geo == 'IT')

dt_ilc_mdsd01_it <- dt_ilc_mdsd01_it %>% filter(wstatus == 'UNE') 

dt_ilc_mdsd01_it <- dt_ilc_mdsd01_it %>% filter(sex == 'F') 

dt_ilc_mdsd01_it <- dt_ilc_mdsd01_it %>% mutate(year = year(time))

dt_ilc_mdsd01_it <- dt_ilc_mdsd01_it %>% select(age, year, values)

dt_ilc_mdsd01_it <- dt_ilc_mdsd01_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_mdsd01_it[[4]][dt_ilc_mdsd01_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_mdsd01_it <- dt_ilc_mdsd01_it %>% select(age_description, year, values)

colnames(dt_ilc_mdsd01_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_mdsd01_it, aes(year, values, color=age)) +
    ggtitle('Italy - Material and social deprivation rate for unemployed women by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_mdsd01_it <- dt_ilc_mdsd01 %>% filter(geo == 'IT')

dt_ilc_mdsd01_it <- dt_ilc_mdsd01_it %>% filter(wstatus == 'UNE') 

dt_ilc_mdsd01_it <- dt_ilc_mdsd01_it %>% filter(sex == 'M') 

dt_ilc_mdsd01_it <- dt_ilc_mdsd01_it %>% mutate(year = year(time))

dt_ilc_mdsd01_it <- dt_ilc_mdsd01_it %>% select(age, year, values)

dt_ilc_mdsd01_it <- dt_ilc_mdsd01_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_mdsd01_it[[4]][dt_ilc_mdsd01_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_mdsd01_it <- dt_ilc_mdsd01_it %>% select(age_description, year, values)

colnames(dt_ilc_mdsd01_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_mdsd01_it, aes(year, values, color=age)) +
    ggtitle('Italy - Material and social deprivation rate for unemployed men by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_mdsd01_eu <- dt_ilc_mdsd01 %>% filter(geo == 'EU')

dt_ilc_mdsd01_eu <- dt_ilc_mdsd01_eu %>% filter(wstatus == 'UNE') 

dt_ilc_mdsd01_eu <- dt_ilc_mdsd01_eu %>% filter(sex == 'F') 

dt_ilc_mdsd01_eu <- dt_ilc_mdsd01_eu %>% mutate(year = year(time))

dt_ilc_mdsd01_eu <- dt_ilc_mdsd01_eu %>% select(age, year, values)

dt_ilc_mdsd01_eu <- dt_ilc_mdsd01_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_mdsd01_eu[[4]][dt_ilc_mdsd01_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_mdsd01_eu <- dt_ilc_mdsd01_eu %>% select(age_description, year, values)

colnames(dt_ilc_mdsd01_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_mdsd01_eu, aes(year, values, color=age)) +
    ggtitle('Europe - Material and social deprivation rate for unemployed women by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


dt_ilc_mdsd01_eu <- dt_ilc_mdsd01 %>% filter(geo == 'EU')

dt_ilc_mdsd01_eu <- dt_ilc_mdsd01_eu %>% filter(wstatus == 'UNE') 

dt_ilc_mdsd01_eu <- dt_ilc_mdsd01_eu %>% filter(sex == 'M') 

dt_ilc_mdsd01_eu <- dt_ilc_mdsd01_eu %>% mutate(year = year(time))

dt_ilc_mdsd01_eu <- dt_ilc_mdsd01_eu %>% select(age, year, values)

dt_ilc_mdsd01_eu <- dt_ilc_mdsd01_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_mdsd01_eu[[4]][dt_ilc_mdsd01_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_mdsd01_eu <- dt_ilc_mdsd01_eu %>% select(age_description, year, values)

colnames(dt_ilc_mdsd01_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_mdsd01_eu, aes(year, values, color=age)) +
    ggtitle('Europe - Material and social deprivation rate for unemployed men by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


################################################################################
###Severe material deprivation rate by age and sex
################################################################################
dt_ilc_mddd11 <- get_eurostat("ilc_mddd11", stringsAsFactors = FALSE)

dt_ilc_mddd11_it <- dt_ilc_mddd11 %>% filter(geo == 'IT')

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% filter(unit == 'PC')

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% filter(age != 'TOTAL') 

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% filter(sex == 'F') 

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% mutate(year = year(time))

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% select(age, year, values)

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_mddd11_it[[4]][dt_ilc_mddd11_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% select(age_description, year, values)

colnames(dt_ilc_mddd11_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_mddd11_it, aes(year, values, color=age)) +
    ggtitle('Italy - Severe material deprivation rate for women by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_mddd11_it <- dt_ilc_mddd11 %>% filter(geo == 'IT')

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% filter(unit == 'PC')

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% filter(age != 'TOTAL') 

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% filter(sex == 'M') 

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% mutate(year = year(time))

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% select(age, year, values)

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_mddd11_it[[4]][dt_ilc_mddd11_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_mddd11_it <- dt_ilc_mddd11_it %>% select(age_description, year, values)

colnames(dt_ilc_mddd11_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_mddd11_it, aes(year, values, color=age)) +
    ggtitle('Italy - Severe material deprivation rate for men by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_mddd11_eu <- dt_ilc_mddd11 %>% filter(geo == 'EU')

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% filter(unit == 'PC')

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% filter(age != 'TOTAL') 

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% filter(sex == 'F') 

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% mutate(year = year(time))

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% select(age, year, values)

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_mddd11_eu[[4]][dt_ilc_mddd11_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% select(age_description, year, values)

colnames(dt_ilc_mddd11_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_mddd11_eu, aes(year, values, color=age)) +
    ggtitle('Europe - Severe material deprivation rate for women by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_mddd11_eu <- dt_ilc_mddd11 %>% filter(geo == 'EU')

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% filter(unit == 'PC')

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% filter(age != 'TOTAL') 

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% filter(sex == 'M') 

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% mutate(year = year(time))

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% select(age, year, values)

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_mddd11_eu[[4]][dt_ilc_mddd11_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_mddd11_eu <- dt_ilc_mddd11_eu %>% select(age_description, year, values)

colnames(dt_ilc_mddd11_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_mddd11_eu, aes(year, values, color=age)) +
    ggtitle('Europe - Severe material deprivation rate for men by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
##########################ECONIMIC STRAIN#######################################
################################################################################

################################################################################
##Inability to keep home adequately warm - EU-SILC survey
################################################################################

##INCGRP
# B_MD60	Below 60% of median equivalised income
# A_MD60	Above 60% of median equivalised income
# TOTAL	Total

##HHTYP
# TOTAL	Total
# A1	Single person
# A1_LT65	One adult younger than 65 years
# A1_GE65	One adult 65 years or over
# A1_DCH	Single person with dependent children
# A1F	Single female
# A1M	Single male
# A2	Two adults
# A2_2LT65	Two adults younger than 65 years
# A2_GE1_GE65	Two adults, at least one aged 65 years or over
# A2_1DCH	Two adults with one dependent child
# A2_2DCH	Two adults with two dependent children
# A2_GE3DCH	Two adults with three or more dependent children
# A_GE3	Three or more adults
# A_GE3_DCH	Three or more adults with dependent children
# HH_NDCH	Households without dependent children
# HH_DCH	Households with dependent children

dt_ilc_mdes01 <- get_eurostat("ilc_mdes01", stringsAsFactors = FALSE)

dt_ilc_mdes01_it <- dt_ilc_mdes01 %>% filter(geo == 'IT')

dt_ilc_mdes01_it <- dt_ilc_mdes01_it %>% filter(incgrp == 'TOTAL')

dt_ilc_mdes01_it <- dt_ilc_mdes01_it %>% mutate(year = year(time))

dt_ilc_mdes01_it <- dt_ilc_mdes01_it %>% select(hhtyp, year, values)

dt_ilc_mdes01_it <- dt_ilc_mdes01_it %>% filter(hhtyp != 'TOTAL')

dt_ilc_mdes01_it <- dt_ilc_mdes01_it %>%
  mutate(household_composition = case_when(
    .$hhtyp == 'A1' ~ 'Single person',
    .$hhtyp == 'A1_LT65' ~ 'One adult younger than 65 years',
    .$hhtyp == 'A1_GE65' ~ 'One adult 65 years or over',
    .$hhtyp == 'A1_DCH' ~ 'Single person with dependent children',
    .$hhtyp == 'A1F' ~ 'Single female',
    .$hhtyp == 'A1M' ~ 'Single male',
    .$hhtyp == 'A2' ~ 'Two adults',
    .$hhtyp == 'A2_2LT65' ~ 'Two adults younger than 65 years',
    .$hhtyp == 'A2_GE1_GE65' ~ 'Two adults, at least one aged 65 years or over',
    .$hhtyp == 'A2_1DCH' ~ 'Two adults with one dependent child',
    .$hhtyp == 'A2_2DCH' ~ 'Two adults with two dependent children',
    .$hhtyp == 'A2_GE3DCH' ~ 'Two adults with three or more dependent children',
    .$hhtyp == 'A_GE2_NDCH' ~ 'Two or more adults without dependent children',
    .$hhtyp == 'A_GE2_DCH' ~ 'Two or more adults with dependent children',
    .$hhtyp == 'A_GE3' ~ 'Three or more adults',
    .$hhtyp == 'A_GE3_DCH' ~ 'Three or more adults with dependent children',
    .$hhtyp == 'HH_NDCH' ~ 'Households without dependent children',
    TRUE ~ 'Households with dependent children'))

dt_ilc_mdes01_it <- dt_ilc_mdes01_it %>% select(household_composition, year, values)

ggplotly(
  ggplot(dt_ilc_mdes01_it, aes(year, values, color=household_composition)) +
    ggtitle('Italy - Inability to keep home adequately warm by household composition') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_mdes01_eu <- dt_ilc_mdes01 %>% filter(geo == 'EU')

dt_ilc_mdes01_eu <- dt_ilc_mdes01_eu %>% filter(incgrp == 'TOTAL')

dt_ilc_mdes01_eu <- dt_ilc_mdes01_eu %>% mutate(year = year(time))

dt_ilc_mdes01_eu <- dt_ilc_mdes01_eu %>% select(hhtyp, year, values)

dt_ilc_mdes01_eu <- dt_ilc_mdes01_eu %>% filter(hhtyp != 'TOTAL')

dt_ilc_mdes01_eu <- dt_ilc_mdes01_eu %>%
  mutate(household_composition = case_when(
    .$hhtyp == 'A1' ~ 'Single person',
    .$hhtyp == 'A1_LT65' ~ 'One adult younger than 65 years',
    .$hhtyp == 'A1_GE65' ~ 'One adult 65 years or over',
    .$hhtyp == 'A1_DCH' ~ 'Single person with dependent children',
    .$hhtyp == 'A1F' ~ 'Single female',
    .$hhtyp == 'A1M' ~ 'Single male',
    .$hhtyp == 'A2' ~ 'Two adults',
    .$hhtyp == 'A2_2LT65' ~ 'Two adults younger than 65 years',
    .$hhtyp == 'A2_GE1_GE65' ~ 'Two adults, at least one aged 65 years or over',
    .$hhtyp == 'A2_1DCH' ~ 'Two adults with one dependent child',
    .$hhtyp == 'A2_2DCH' ~ 'Two adults with two dependent children',
    .$hhtyp == 'A2_GE3DCH' ~ 'Two adults with three or more dependent children',
    .$hhtyp == 'A_GE2_NDCH' ~ 'Two or more adults without dependent children',
    .$hhtyp == 'A_GE2_DCH' ~ 'Two or more adults with dependent children',
    .$hhtyp == 'A_GE3' ~ 'Three or more adults',
    .$hhtyp == 'A_GE3_DCH' ~ 'Three or more adults with dependent children',
    .$hhtyp == 'HH_NDCH' ~ 'Households without dependent children',
    TRUE ~ 'Households with dependent children'))

dt_ilc_mdes01_eu <- dt_ilc_mdes01_eu %>% select(household_composition, year, values)

ggplotly(
  ggplot(dt_ilc_mdes01_eu, aes(year, values, color=household_composition)) +
    ggtitle('Europe - Inability to keep home adequately warm by household composition') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


################################################################################
##Inability to afford a meal with meat, chicken, fish 
##(or vegetarian equivalent) every second day - EU-SILC survey
###############################################################################
dt_ilc_mdes03 <- get_eurostat("ilc_mdes03", stringsAsFactors = FALSE)

dt_ilc_mdes03_it <- dt_ilc_mdes03 %>% filter(geo == 'IT')

dt_ilc_mdes03_it <- dt_ilc_mdes03_it %>% filter(incgrp == 'TOTAL')

dt_ilc_mdes03_it <- dt_ilc_mdes03_it %>% mutate(year = year(time))

dt_ilc_mdes03_it <- dt_ilc_mdes03_it %>% select(hhtyp, year, values)

dt_ilc_mdes03_it <- dt_ilc_mdes03_it %>% filter(hhtyp != 'TOTAL')

dt_ilc_mdes03_it <- dt_ilc_mdes03_it %>%
  mutate(household_composition = case_when(
    .$hhtyp == 'A1' ~ 'Single person',
    .$hhtyp == 'A1_LT65' ~ 'One adult younger than 65 years',
    .$hhtyp == 'A1_GE65' ~ 'One adult 65 years or over',
    .$hhtyp == 'A1_DCH' ~ 'Single person with dependent children',
    .$hhtyp == 'A1F' ~ 'Single female',
    .$hhtyp == 'A1M' ~ 'Single male',
    .$hhtyp == 'A2' ~ 'Two adults',
    .$hhtyp == 'A2_2LT65' ~ 'Two adults younger than 65 years',
    .$hhtyp == 'A2_GE1_GE65' ~ 'Two adults, at least one aged 65 years or over',
    .$hhtyp == 'A2_1DCH' ~ 'Two adults with one dependent child',
    .$hhtyp == 'A2_2DCH' ~ 'Two adults with two dependent children',
    .$hhtyp == 'A2_GE3DCH' ~ 'Two adults with three or more dependent children',
    .$hhtyp == 'A_GE2_NDCH' ~ 'Two or more adults without dependent children',
    .$hhtyp == 'A_GE2_DCH' ~ 'Two or more adults with dependent children',
    .$hhtyp == 'A_GE3' ~ 'Three or more adults',
    .$hhtyp == 'A_GE3_DCH' ~ 'Three or more adults with dependent children',
    .$hhtyp == 'HH_NDCH' ~ 'Households without dependent children',
    TRUE ~ 'Households with dependent children'))

dt_ilc_mdes03_it <- dt_ilc_mdes03_it %>% select(household_composition, year, values)

ggplotly(
  ggplot(dt_ilc_mdes03_it, aes(year, values, color=household_composition)) +
    ggtitle('Italy - Inability to afford a meal with meat, chicken, fish every second day by household composition') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_mdes03_eu <- dt_ilc_mdes03 %>% filter(geo == 'EU')

dt_ilc_mdes03_eu <- dt_ilc_mdes03_eu %>% filter(incgrp == 'TOTAL')

dt_ilc_mdes03_eu <- dt_ilc_mdes03_eu %>% mutate(year = year(time))

dt_ilc_mdes03_eu <- dt_ilc_mdes03_eu %>% select(hhtyp, year, values)

dt_ilc_mdes03_eu <- dt_ilc_mdes03_eu %>% filter(hhtyp != 'TOTAL')

dt_ilc_mdes03_eu <- dt_ilc_mdes03_eu %>%
  mutate(household_composition = case_when(
    .$hhtyp == 'A1' ~ 'Single person',
    .$hhtyp == 'A1_LT65' ~ 'One adult younger than 65 years',
    .$hhtyp == 'A1_GE65' ~ 'One adult 65 years or over',
    .$hhtyp == 'A1_DCH' ~ 'Single person with dependent children',
    .$hhtyp == 'A1F' ~ 'Single female',
    .$hhtyp == 'A1M' ~ 'Single male',
    .$hhtyp == 'A2' ~ 'Two adults',
    .$hhtyp == 'A2_2LT65' ~ 'Two adults younger than 65 years',
    .$hhtyp == 'A2_GE1_GE65' ~ 'Two adults, at least one aged 65 years or over',
    .$hhtyp == 'A2_1DCH' ~ 'Two adults with one dependent child',
    .$hhtyp == 'A2_2DCH' ~ 'Two adults with two dependent children',
    .$hhtyp == 'A2_GE3DCH' ~ 'Two adults with three or more dependent children',
    .$hhtyp == 'A_GE2_NDCH' ~ 'Two or more adults without dependent children',
    .$hhtyp == 'A_GE2_DCH' ~ 'Two or more adults with dependent children',
    .$hhtyp == 'A_GE3' ~ 'Three or more adults',
    .$hhtyp == 'A_GE3_DCH' ~ 'Three or more adults with dependent children',
    .$hhtyp == 'HH_NDCH' ~ 'Households without dependent children',
    TRUE ~ 'Households with dependent children'))

dt_ilc_mdes03_eu <- dt_ilc_mdes03_eu %>% select(household_composition, year, values)

ggplotly(
  ggplot(dt_ilc_mdes03_eu, aes(year, values, color=household_composition)) +
    ggtitle('Europe - Inability to afford a meal with meat, chicken, fish every second day by household composition') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
#########################DURABLES###############################################
################################################################################

#We are going to consider only situation with children

# A2_1DCH	Two adults with one dependent child
# A2_2DCH	Two adults with two dependent children
# A2_GE3DCH	Two adults with three or more dependent children
# A_GE3_DCH	Three or more adults with dependent children
# HH_DCH	Households with dependent children

################################################################################
##Persons who cannot afford a computer - EU-SILC survey
################################################################################
dt_ilc_mddu03 <- get_eurostat("ilc_mddu03", stringsAsFactors = FALSE)

dt_ilc_mddu03_it <- dt_ilc_mddu03 %>% filter(geo == 'IT')

dt_ilc_mddu03_it <- dt_ilc_mddu03_it %>% filter(incgrp == 'TOTAL')

dt_ilc_mddu03_it <- dt_ilc_mddu03_it %>% mutate(year = year(time))

dt_ilc_mddu03_it <- dt_ilc_mddu03_it %>% 
  filter(hhtyp %in% c('A2_1DCH', 'A2_2DCH', 'A2_GE3DCH', 'A_GE3_DCH', 'HH_DCH'))

dt_ilc_mddu03_it <- dt_ilc_mddu03_it %>% select(hhtyp, year, values)

dt_ilc_mddu03_it <- dt_ilc_mddu03_it %>%
  mutate(household_composition = case_when(
    .$hhtyp == 'A1' ~ 'Single person',
    .$hhtyp == 'A1_LT65' ~ 'One adult younger than 65 years',
    .$hhtyp == 'A1_GE65' ~ 'One adult 65 years or over',
    .$hhtyp == 'A1_DCH' ~ 'Single person with dependent children',
    .$hhtyp == 'A1F' ~ 'Single female',
    .$hhtyp == 'A1M' ~ 'Single male',
    .$hhtyp == 'A2' ~ 'Two adults',
    .$hhtyp == 'A2_2LT65' ~ 'Two adults younger than 65 years',
    .$hhtyp == 'A2_GE1_GE65' ~ 'Two adults, at least one aged 65 years or over',
    .$hhtyp == 'A2_1DCH' ~ 'Two adults with one dependent child',
    .$hhtyp == 'A2_2DCH' ~ 'Two adults with two dependent children',
    .$hhtyp == 'A2_GE3DCH' ~ 'Two adults with three or more dependent children',
    .$hhtyp == 'A_GE2_NDCH' ~ 'Two or more adults without dependent children',
    .$hhtyp == 'A_GE2_DCH' ~ 'Two or more adults with dependent children',
    .$hhtyp == 'A_GE3' ~ 'Three or more adults',
    .$hhtyp == 'A_GE3_DCH' ~ 'Three or more adults with dependent children',
    .$hhtyp == 'HH_NDCH' ~ 'Households without dependent children',
    TRUE ~ 'Households with dependent children'))

dt_ilc_mddu03_it <- dt_ilc_mddu03_it %>% select(household_composition, year, values)

ggplotly(
  ggplot(dt_ilc_mddu03_it, aes(year, values, color=household_composition)) +
    ggtitle('Italy - Persons who cannot afford a computer by household composition') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_mddu03_eu <- dt_ilc_mddu03 %>% filter(geo == 'EU')

dt_ilc_mddu03_eu <- dt_ilc_mddu03_eu %>% filter(incgrp == 'TOTAL')

dt_ilc_mddu03_eu <- dt_ilc_mddu03_eu %>% mutate(year = year(time))

dt_ilc_mddu03_eu <- dt_ilc_mddu03_eu %>% 
  filter(hhtyp %in% c('A2_1DCH', 'A2_2DCH', 'A2_GE3DCH', 'A_GE3_DCH', 'HH_DCH'))

dt_ilc_mddu03_eu <- dt_ilc_mddu03_eu %>% select(hhtyp, year, values)

dt_ilc_mddu03_eu <- dt_ilc_mddu03_eu %>%
  mutate(household_composition = case_when(
    .$hhtyp == 'A1' ~ 'Single person',
    .$hhtyp == 'A1_LT65' ~ 'One adult younger than 65 years',
    .$hhtyp == 'A1_GE65' ~ 'One adult 65 years or over',
    .$hhtyp == 'A1_DCH' ~ 'Single person with dependent children',
    .$hhtyp == 'A1F' ~ 'Single female',
    .$hhtyp == 'A1M' ~ 'Single male',
    .$hhtyp == 'A2' ~ 'Two adults',
    .$hhtyp == 'A2_2LT65' ~ 'Two adults younger than 65 years',
    .$hhtyp == 'A2_GE1_GE65' ~ 'Two adults, at least one aged 65 years or over',
    .$hhtyp == 'A2_1DCH' ~ 'Two adults with one dependent child',
    .$hhtyp == 'A2_2DCH' ~ 'Two adults with two dependent children',
    .$hhtyp == 'A2_GE3DCH' ~ 'Two adults with three or more dependent children',
    .$hhtyp == 'A_GE2_NDCH' ~ 'Two or more adults without dependent children',
    .$hhtyp == 'A_GE2_DCH' ~ 'Two or more adults with dependent children',
    .$hhtyp == 'A_GE3' ~ 'Three or more adults',
    .$hhtyp == 'A_GE3_DCH' ~ 'Three or more adults with dependent children',
    .$hhtyp == 'HH_NDCH' ~ 'Households without dependent children',
    TRUE ~ 'Households with dependent children'))

dt_ilc_mddu03_eu <- dt_ilc_mddu03_eu %>% select(household_composition, year, values)

ggplotly(
  ggplot(dt_ilc_mddu03_eu, aes(year, values, color=household_composition)) +
    ggtitle('Europe - Persons who cannot afford a computer by household composition') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
##Persons who cannot afford internet connection for personal use 
##at home by age, sex and income group
################################################################################
dt_ilc_mddu07a <- get_eurostat("ilc_mddu07a", stringsAsFactors = FALSE)

dt_ilc_mddu07a_it <- dt_ilc_mddu07a %>% filter(geo == 'IT')

dt_ilc_mddu07a_it <- dt_ilc_mddu07a_it %>% filter(incgrp == 'TOTAL')

dt_ilc_mddu07a_it <- dt_ilc_mddu07a_it %>% filter(age == 'Y16-29')

dt_ilc_mddu07a_it <- dt_ilc_mddu07a_it %>% filter(sex != 'T')

dt_ilc_mddu07a_it <- dt_ilc_mddu07a_it %>% mutate(year = year(time))

dt_ilc_mddu07a_it <- dt_ilc_mddu07a_it %>% select(sex, year, values)

ggplotly(
  ggplot(dt_ilc_mddu07a_it, aes(year, values, color=sex)) +
    ggtitle('Italy - Persons between 16 and 19 who cannot afford internet connection for personal use at home, by sex') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


dt_ilc_mddu07a_it <- dt_ilc_mddu07a %>% filter(geo == 'IT')

dt_ilc_mddu07a_it <- dt_ilc_mddu07a_it %>% filter(incgrp == 'B_MD60')

dt_ilc_mddu07a_it <- dt_ilc_mddu07a_it %>% filter(age == 'Y16-29')

dt_ilc_mddu07a_it <- dt_ilc_mddu07a_it %>% filter(sex != 'T')

dt_ilc_mddu07a_it <- dt_ilc_mddu07a_it %>% mutate(year = year(time))

dt_ilc_mddu07a_it <- dt_ilc_mddu07a_it %>% select(sex, year, values)

ggplotly(
  ggplot(dt_ilc_mddu07a_it, aes(year, values, color=sex)) +
    ggtitle('Italy - Persons, below 60% of median equivalised income, between 16 and 19 who cannot afford internet connection for personal use at home, by sex') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_mddu07a_eu <- dt_ilc_mddu07a %>% filter(geo == 'EU')

dt_ilc_mddu07a_eu <- dt_ilc_mddu07a_eu %>% filter(incgrp == 'TOTAL')

dt_ilc_mddu07a_eu <- dt_ilc_mddu07a_eu %>% filter(age == 'Y16-29')

dt_ilc_mddu07a_eu <- dt_ilc_mddu07a_eu %>% filter(sex != 'T')

dt_ilc_mddu07a_eu <- dt_ilc_mddu07a_eu %>% mutate(year = year(time))

dt_ilc_mddu07a_eu <- dt_ilc_mddu07a_eu %>% select(sex, year, values)

ggplotly(
  ggplot(dt_ilc_mddu07a_it, aes(year, values, color=sex)) +
    ggtitle('Europe - Persons between 16 and 19 who cannot afford internet connection for personal use at home, by sex') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

dt_ilc_mddu07a_eu <- dt_ilc_mddu07a %>% filter(geo == 'EU')

dt_ilc_mddu07a_eu <- dt_ilc_mddu07a_eu %>% filter(incgrp == 'B_MD60')

dt_ilc_mddu07a_eu <- dt_ilc_mddu07a_eu %>% filter(age == 'Y16-29')

dt_ilc_mddu07a_eu <- dt_ilc_mddu07a_eu %>% filter(sex != 'T')

dt_ilc_mddu07a_eu <- dt_ilc_mddu07a_eu %>% mutate(year = year(time))

dt_ilc_mddu07a_eu <- dt_ilc_mddu07a_eu %>% select(sex, year, values)

ggplotly(
  ggplot(dt_ilc_mddu07a_it, aes(year, values, color=sex)) +
    ggtitle('Europe - Persons, below 60% of median equivalised income, between 16 and 19 who cannot afford internet connection for personal use at home, by sex') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
#############################HOUSING DEPRIVATION################################
################################################################################

################################################################################
##Severe housing deprivation rate by age, sex and poverty status - EU-SILC survey
################################################################################
dt_ilc_mdho06a <- get_eurostat("ilc_mdho06a", stringsAsFactors = FALSE)

dt_ilc_mdho06a_it <- dt_ilc_mdho06a %>% filter(geo == 'IT')

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% filter(incgrp == 'TOTAL')

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% filter(age == 'TOTAL')

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% filter(sex != 'T')

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% mutate(year = year(time))

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% select(sex, year, values)

ggplotly(
  ggplot(dt_ilc_mdho06a_it, aes(year, values, color=sex)) +
    ggtitle('Italy - Severe housing deprivation rate by sex') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)



dt_ilc_mdho06a_it <- dt_ilc_mdho06a %>% filter(geo == 'IT')

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% filter(incgrp == 'TOTAL')

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% filter(sex == 'M')

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% mutate(year = year(time))

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% select(age, year, values)

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_mdho06a_it[[4]][dt_ilc_mdho06a_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% select(age_description, year, values)

colnames(dt_ilc_mdho06a_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_mdho06a_it, aes(year, values, color=age)) +
    ggtitle('Italy - Men with severe housing deprivation rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)



dt_ilc_mdho06a_it <- dt_ilc_mdho06a %>% filter(geo == 'IT')

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% filter(incgrp == 'TOTAL')

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% filter(sex == 'F')

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% mutate(year = year(time))

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% select(age, year, values)

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_mdho06a_it[[4]][dt_ilc_mdho06a_it[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_mdho06a_it <- dt_ilc_mdho06a_it %>% select(age_description, year, values)

colnames(dt_ilc_mdho06a_it) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_mdho06a_it, aes(year, values, color=age)) +
    ggtitle('Italy - Women with severe housing deprivation rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_ilc_mdho06a_eu <- dt_ilc_mdho06a %>% filter(geo == 'EU')

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% filter(incgrp == 'TOTAL')

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% filter(age == 'TOTAL')

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% filter(sex != 'T')

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% mutate(year = year(time))

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% select(sex, year, values)

ggplotly(
  ggplot(dt_ilc_mdho06a_eu, aes(year, values, color=sex)) +
    ggtitle('Europe - Severe housing deprivation rate by sex') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


dt_ilc_mdho06a_eu <- dt_ilc_mdho06a %>% filter(geo == 'EU')

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% filter(incgrp == 'TOTAL')

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% filter(sex == 'M')

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% mutate(year = year(time))

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% select(age, year, values)

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_mdho06a_eu[[4]][dt_ilc_mdho06a_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% select(age_description, year, values)

colnames(dt_ilc_mdho06a_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_mdho06a_eu, aes(year, values, color=age)) +
    ggtitle('Europe - Men with severe housing deprivation rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)



dt_ilc_mdho06a_eu <- dt_ilc_mdho06a %>% filter(geo == 'EU')

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% filter(incgrp == 'TOTAL')

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% filter(sex == 'F')

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% mutate(year = year(time))

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% select(age, year, values)

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% mutate(age_description = age)

for (age_value in age){
  dt_ilc_mdho06a_eu[[4]][dt_ilc_mdho06a_eu[[1]] == age_value] <- getAgeDescription(age_value)$age_description
}

dt_ilc_mdho06a_eu <- dt_ilc_mdho06a_eu %>% select(age_description, year, values)

colnames(dt_ilc_mdho06a_eu) <- c('age', 'year', 'values')

ggplotly(
  ggplot(dt_ilc_mdho06a_eu, aes(year, values, color=age)) +
    ggtitle('Europe - Women with severe housing deprivation rate by age') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
#################################HOUSE PRICE INDEX##############################
################################################################################
#https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=prc_hpi_a&lang=en

##purchase
# TOTAL	Total
# DW_NEW	Purchases of new dwellings
# DW_EXST	Purchases of existing dwellings

##unit
# I10_A_AVG	Annual average index, 2010=100
# I15_A_AVG	Annual average index, 2015=100
# RCH_A_AVG	Annual average rate of change

dt_prc_hpi_a <- get_eurostat("prc_hpi_a", stringsAsFactors = FALSE)

dt_prc_hpi_a_it <- dt_prc_hpi_a %>% filter(geo == 'IT')

dt_prc_hpi_a_it <- dt_prc_hpi_a_it %>% filter(unit == 'RCH_A_AVG')

dt_prc_hpi_a_it <- dt_prc_hpi_a_it %>% mutate(year = year(time))

dt_prc_hpi_a_it <- dt_prc_hpi_a_it %>% select(purchase, year, values)

dt_prc_hpi_a_it <- dt_prc_hpi_a_it %>% filter(purchase != 'TOTAL')

dt_prc_hpi_a_it <- dt_prc_hpi_a_it %>%
  mutate(purchase_type = case_when(
    .$purchase == 'DW_NEW' ~ 'Purchases of new dwellings',
    TRUE ~ 'Purchases of existing dwellings'))

dt_prc_hpi_a_it <- dt_prc_hpi_a_it %>% select(purchase_type, year, values)

ggplotly(
  ggplot(dt_prc_hpi_a_it, aes(year, values, color=purchase_type)) +
    ggtitle('Italy - House price index') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_prc_hpi_a_eu <- dt_prc_hpi_a %>% filter(geo == 'EU')

dt_prc_hpi_a_eu <- dt_prc_hpi_a_eu %>% filter(unit == 'RCH_A_AVG')

dt_prc_hpi_a_eu <- dt_prc_hpi_a_eu %>% mutate(year = year(time))

dt_prc_hpi_a_eu <- dt_prc_hpi_a_eu %>% select(purchase, year, values)

dt_prc_hpi_a_eu <- dt_prc_hpi_a_eu %>%
  mutate(purchase_type = case_when(
    .$purchase == 'DW_NEW' ~ 'Purchases of new dwellings',
    .$purchase == 'TOTAL' ~ 'Total',
    TRUE ~ 'Purchases of existing dwellings'))

dt_prc_hpi_a_eu <- dt_prc_hpi_a_eu %>% select(purchase_type, year, values)

ggplotly(
  ggplot(dt_prc_hpi_a_eu, aes(year, values, color=purchase_type)) +
    ggtitle('Europe - House price index') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###########################CONSUMER PRICE INDEX#################################
################################################################################

# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Consumer_price_index_(CPI)

#https://ec.europa.eu/eurostat/web/hicp/data/database


################################################################################
###HICP - annual data (average index and rate of change)
################################################################################

##unit
# INX_A_AVG	Annual average index
# RCH_A_AVG	Annual average rate of change

##coicop filterd
# CP01	Food and non-alcoholic beverages
# CP02	Alcoholic beverages, tobacco and narcotics
# CP03	Clothing and footwear
# CP04	Housing, water, electricity, gas and other fuels
# CP05	Furnishings, household equipment and routine household maintenance
# CP06	Health
# CP07	Transport
# CP08	Communications
# CP09	Recreation and culture
# CP10	Education
# CP11	Restaurants and hotels
# CP12	Miscellaneous goods and services


dt_prc_hicp_aind <- get_eurostat("prc_hicp_aind", stringsAsFactors = FALSE)

dt_prc_hicp_aind_it <- dt_prc_hicp_aind %>% filter(geo == 'IT')

dt_prc_hicp_aind_it <- dt_prc_hicp_aind_it %>% filter(unit == 'RCH_A_AVG')

dt_prc_hicp_aind_it <- dt_prc_hicp_aind_it %>% 
  filter(coicop %in% c('CP01', 'CP02', 'CP03', 'CP04', 'CP05', 'CP06',
                       'CP07', 'CP08', 'CP09', 'CP10', 'CP11', 'CP12'))

dt_prc_hicp_aind_it <- dt_prc_hicp_aind_it %>% mutate(year = year(time))

dt_prc_hicp_aind_it <- dt_prc_hicp_aind_it %>% select(coicop, year, values)

dt_prc_hicp_aind_it <- dt_prc_hicp_aind_it %>%
  mutate(type = case_when(
    .$coicop == 'CP01' ~ 'Food and non-alcoholic beverages',
    .$coicop == 'CP02' ~ 'Alcoholic beverages, tobacco and narcotics',
    .$coicop == 'CP03' ~ 'Clothing and footwear',
    .$coicop == 'CP04' ~ 'Housing, water, electricity, gas and other fuels',
    .$coicop == 'CP05' ~ 'Furnishings, household equipment and routine household maintenance',
    .$coicop == 'CP06' ~ 'Health',
    .$coicop == 'CP07' ~ 'Transport',
    .$coicop == 'CP08' ~ 'Communications',
    .$coicop == 'CP09' ~ 'Recreation and culture',
    .$coicop == 'CP10' ~ 'Education',
    .$coicop == 'CP11' ~ 'Restaurants and hotels',
    TRUE ~ 'Miscellaneous goods and services'))

dt_prc_hicp_aind_it <- dt_prc_hicp_aind_it %>% select(type, year, values)

ggplotly(
  ggplot(dt_prc_hicp_aind_it, aes(year, values, color=type)) +
    ggtitle('Italy - Consumer price index (Annual average rate of change)') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_prc_hicp_aind_eu <- dt_prc_hicp_aind %>% filter(geo == 'EU')

dt_prc_hicp_aind_eu <- dt_prc_hicp_aind_eu %>% filter(unit == 'RCH_A_AVG')

dt_prc_hicp_aind_eu <- dt_prc_hicp_aind_eu %>% 
  filter(coicop %in% c('CP01', 'CP02', 'CP03', 'CP04', 'CP05', 'CP06',
                       'CP07', 'CP08', 'CP09', 'CP10', 'CP11', 'CP12'))

dt_prc_hicp_aind_eu <- dt_prc_hicp_aind_eu %>% mutate(year = year(time))

dt_prc_hicp_aind_eu <- dt_prc_hicp_aind_eu %>% select(coicop, year, values)

dt_prc_hicp_aind_eu <- dt_prc_hicp_aind_eu %>%
  mutate(type = case_when(
    .$coicop == 'CP01' ~ 'Food and non-alcoholic beverages',
    .$coicop == 'CP02' ~ 'Alcoholic beverages, tobacco and narcotics',
    .$coicop == 'CP03' ~ 'Clothing and footwear',
    .$coicop == 'CP04' ~ 'Housing, water, electricity, gas and other fuels',
    .$coicop == 'CP05' ~ 'Furnishings, household equipment and routine household maintenance',
    .$coicop == 'CP06' ~ 'Health',
    .$coicop == 'CP07' ~ 'Transport',
    .$coicop == 'CP08' ~ 'Communications',
    .$coicop == 'CP09' ~ 'Recreation and culture',
    .$coicop == 'CP10' ~ 'Education',
    .$coicop == 'CP11' ~ 'Restaurants and hotels',
    TRUE ~ 'Miscellaneous goods and services'))

dt_prc_hicp_aind_eu <- dt_prc_hicp_aind_eu %>% select(type, year, values)

ggplotly(
  ggplot(dt_prc_hicp_aind_eu, aes(year, values, color=type)) +
    ggtitle('Europe - Consumer price index (Annual average rate of change)') +
    geom_line() +
    labs(x="Year", y="Percentage") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
############################SOCIAL PROTECTION###################################
################################################################################

##https://ec.europa.eu/eurostat/web/social-protection/data/database

################################################################################
###Expenditure: main results
################################################################################

##unit
# MIO_EUR	Million euro
# MEUR_KP10	Million euro (at constant 2010 prices)
# EUR_HAB	Euro per inhabitant
# EUR_HAB_KP10	Euro per inhabitant (at constant 2010 prices)
# MIO_NAC	Million units of national currency
# MNAC_KP10	Million units of national currency at constant 2010 prices
# MIO_PPS	Million purchasing power standards (PPS)
# PPS_HAB	Purchasing power standard (PPS) per inhabitant
# PC_GDP	Percentage of gross domestic product (GDP)
# PC_BEN	Percentage of total benefits
# PC_EXP	Percentage of total expenditure

##spdeps
# TOTALNOREROUTE	Total expenditure
# SPBENEFNOREROUTE	Social protection benefits
# ADMIN	Administration costs
# OTHER	Other expenditure
# SICK	Sickness/Health care
# DISA	Disability
# OLD	Old age
# SURVIV	Survivors
# FAM	Family/Children
# UNEMPLOY	Unemployment
# HOUSE	Housing
# EXCLU	Social exclusion n.e.c.
# SICKDISA	Sickness / healthcare and disability
# OLDSURVIV	Old age and survivors
# HOUSEEXCLU	Housing and Social exclusion n.e.c.

dt_spr_exp_sum <- get_eurostat("spr_exp_sum", stringsAsFactors = FALSE)

dt_spr_exp_sum_it <- dt_spr_exp_sum %>% filter(geo == 'IT')

dt_PC_GDP <- dt_spr_exp_sum_it %>% filter(unit == 'PC_GDP')

dt_PC_GDP <- dt_PC_GDP %>% mutate(year = year(time))

dt_PC_GDP <- dt_PC_GDP %>% select(spdeps, year, values)

dt_PC_GDP <- dt_PC_GDP %>%
  mutate(expenses = case_when(
    .$spdeps == 'TOTALNOREROUTE' ~ 'Total expenditure',
    .$spdeps == 'SPBENEFNOREROUTE' ~ 'Social protection benefits',
    .$spdeps == 'ADMIN' ~ 'Administration costs',
    .$spdeps == 'OTHER' ~ 'Other expenditure',
    .$spdeps == 'SICK' ~ 'Sickness/Health care',
    .$spdeps == 'DISA' ~ 'Disability',
    .$spdeps == 'OLD' ~ 'Old age',
    .$spdeps == 'SURVIV' ~ 'Survivors',
    .$spdeps == 'FAM' ~ 'Family/Children',
    .$spdeps == 'UNEMPLOY' ~ 'Unemployment',
    .$spdeps == 'HOUSE' ~ 'Housing',
    .$spdeps == 'EXCLU' ~ 'Social exclusion n.e.c.',
    .$spdeps == 'SICKDISA' ~ 'Sickness/healthcare and disability',
    .$spdeps == 'OLDSURVIV' ~ 'Old age and survivors',
    TRUE ~ 'Miscellaneous goods and services'))

dt_PC_GDP <- dt_PC_GDP %>% select(expenses, year, values)

ggplotly(
  ggplot(dt_PC_GDP, aes(year, values, color=expenses)) +
    ggtitle('Italy - Social protection (Expenditure)') +
    geom_line() +
    labs(x="Year", y="Percentage of gross domestic product") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_spr_exp_sum_eu <- dt_spr_exp_sum %>% filter(geo == 'EU28')
dt_PC_GDP <- dt_spr_exp_sum_eu %>% filter(unit == 'PC_GDP')

dt_PC_GDP <- dt_PC_GDP %>% mutate(year = year(time))

dt_PC_GDP <- dt_PC_GDP %>% select(spdeps, year, values)

dt_PC_GDP <- dt_PC_GDP %>%
  mutate(expenses = case_when(
    .$spdeps == 'TOTALNOREROUTE' ~ 'Total expenditure',
    .$spdeps == 'SPBENEFNOREROUTE' ~ 'Social protection benefits',
    .$spdeps == 'ADMIN' ~ 'Administration costs',
    .$spdeps == 'OTHER' ~ 'Other expenditure',
    .$spdeps == 'SICK' ~ 'Sickness/Health care',
    .$spdeps == 'DISA' ~ 'Disability',
    .$spdeps == 'OLD' ~ 'Old age',
    .$spdeps == 'SURVIV' ~ 'Survivors',
    .$spdeps == 'FAM' ~ 'Family/Children',
    .$spdeps == 'UNEMPLOY' ~ 'Unemployment',
    .$spdeps == 'HOUSE' ~ 'Housing',
    .$spdeps == 'EXCLU' ~ 'Social exclusion n.e.c.',
    .$spdeps == 'SICKDISA' ~ 'Sickness/healthcare and disability',
    .$spdeps == 'OLDSURVIV' ~ 'Old age and survivors',
    TRUE ~ 'Miscellaneous goods and services'))

dt_PC_GDP <- dt_PC_GDP %>% select(expenses, year, values)

ggplotly(
  ggplot(dt_PC_GDP, aes(year, values, color=expenses)) +
    ggtitle('Europe - Social protection (Expenditure)') +
    geom_line() +
    labs(x="Year", y="Percentage of gross domestic product") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)



# PC_BEN	Percentage of total benefits

dt_PC_BEN <- dt_spr_exp_sum_it %>% filter(unit == 'PC_BEN')

dt_PC_BEN <- dt_PC_BEN %>% mutate(year = year(time))

dt_PC_BEN <- dt_PC_BEN %>% select(spdeps, year, values)

dt_PC_BEN <- dt_PC_BEN %>%
  mutate(expenses = case_when(
    .$spdeps == 'TOTALNOREROUTE' ~ 'Total expenditure',
    .$spdeps == 'SPBENEFNOREROUTE' ~ 'Social protection benefits',
    .$spdeps == 'ADMIN' ~ 'Administration costs',
    .$spdeps == 'OTHER' ~ 'Other expenditure',
    .$spdeps == 'SICK' ~ 'Sickness/Health care',
    .$spdeps == 'DISA' ~ 'Disability',
    .$spdeps == 'OLD' ~ 'Old age',
    .$spdeps == 'SURVIV' ~ 'Survivors',
    .$spdeps == 'FAM' ~ 'Family/Children',
    .$spdeps == 'UNEMPLOY' ~ 'Unemployment',
    .$spdeps == 'HOUSE' ~ 'Housing',
    .$spdeps == 'EXCLU' ~ 'Social exclusion n.e.c.',
    .$spdeps == 'SICKDISA' ~ 'Sickness/healthcare and disability',
    .$spdeps == 'OLDSURVIV' ~ 'Old age and survivors',
    TRUE ~ 'Miscellaneous goods and services'))

dt_PC_BEN <- dt_PC_BEN %>% select(expenses, year, values)

ggplotly(
  ggplot(dt_PC_BEN, aes(year, values, color=expenses)) +
    ggtitle('Italy - Social protection (Expenditure)') +
    geom_line() +
    labs(x="Year", y="Percentage of total benefits") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_PC_BEN <- dt_spr_exp_sum_eu %>% filter(unit == 'PC_BEN')

dt_PC_BEN <- dt_PC_BEN %>% mutate(year = year(time))

dt_PC_BEN <- dt_PC_BEN %>% select(spdeps, year, values)

dt_PC_BEN <- dt_PC_BEN %>%
  mutate(expenses = case_when(
    .$spdeps == 'TOTALNOREROUTE' ~ 'Total expenditure',
    .$spdeps == 'SPBENEFNOREROUTE' ~ 'Social protection benefits',
    .$spdeps == 'ADMIN' ~ 'Administration costs',
    .$spdeps == 'OTHER' ~ 'Other expenditure',
    .$spdeps == 'SICK' ~ 'Sickness/Health care',
    .$spdeps == 'DISA' ~ 'Disability',
    .$spdeps == 'OLD' ~ 'Old age',
    .$spdeps == 'SURVIV' ~ 'Survivors',
    .$spdeps == 'FAM' ~ 'Family/Children',
    .$spdeps == 'UNEMPLOY' ~ 'Unemployment',
    .$spdeps == 'HOUSE' ~ 'Housing',
    .$spdeps == 'EXCLU' ~ 'Social exclusion n.e.c.',
    .$spdeps == 'SICKDISA' ~ 'Sickness/healthcare and disability',
    .$spdeps == 'OLDSURVIV' ~ 'Old age and survivors',
    TRUE ~ 'Miscellaneous goods and services'))

dt_PC_BEN <- dt_PC_BEN %>% select(expenses, year, values)

ggplotly(
  ggplot(dt_PC_BEN, aes(year, values, color=expenses)) +
    ggtitle('Europe - Social protection (Expenditure)') +
    geom_line() +
    labs(x="Year", y="Percentage of total benefits") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


# PC_EXP	Percentage of total expenditure

dt_PC_BEN <- dt_spr_exp_sum_it %>% filter(unit == 'PC_EXP')

dt_PC_BEN <- dt_PC_BEN %>% mutate(year = year(time))

dt_PC_BEN <- dt_PC_BEN %>% select(spdeps, year, values)

dt_PC_BEN <- dt_PC_BEN %>%
  mutate(expenses = case_when(
    .$spdeps == 'TOTALNOREROUTE' ~ 'Total expenditure',
    .$spdeps == 'SPBENEFNOREROUTE' ~ 'Social protection benefits',
    .$spdeps == 'ADMIN' ~ 'Administration costs',
    .$spdeps == 'OTHER' ~ 'Other expenditure',
    .$spdeps == 'SICK' ~ 'Sickness/Health care',
    .$spdeps == 'DISA' ~ 'Disability',
    .$spdeps == 'OLD' ~ 'Old age',
    .$spdeps == 'SURVIV' ~ 'Survivors',
    .$spdeps == 'FAM' ~ 'Family/Children',
    .$spdeps == 'UNEMPLOY' ~ 'Unemployment',
    .$spdeps == 'HOUSE' ~ 'Housing',
    .$spdeps == 'EXCLU' ~ 'Social exclusion n.e.c.',
    .$spdeps == 'SICKDISA' ~ 'Sickness/healthcare and disability',
    .$spdeps == 'OLDSURVIV' ~ 'Old age and survivors',
    TRUE ~ 'Miscellaneous goods and services'))

dt_PC_BEN <- dt_PC_BEN %>% select(expenses, year, values)

ggplotly(
  ggplot(dt_PC_BEN, aes(year, values, color=expenses)) +
    ggtitle('Italy - Social protection (Expenditure)') +
    geom_line() +
    labs(x="Year", y="Percentage of total expenditure") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_PC_BEN <- dt_spr_exp_sum_eu %>% filter(unit == 'PC_EXP')

dt_PC_BEN <- dt_PC_BEN %>% mutate(year = year(time))

dt_PC_BEN <- dt_PC_BEN %>% select(spdeps, year, values)

dt_PC_BEN <- dt_PC_BEN %>%
  mutate(expenses = case_when(
    .$spdeps == 'TOTALNOREROUTE' ~ 'Total expenditure',
    .$spdeps == 'SPBENEFNOREROUTE' ~ 'Social protection benefits',
    .$spdeps == 'ADMIN' ~ 'Administration costs',
    .$spdeps == 'OTHER' ~ 'Other expenditure',
    .$spdeps == 'SICK' ~ 'Sickness/Health care',
    .$spdeps == 'DISA' ~ 'Disability',
    .$spdeps == 'OLD' ~ 'Old age',
    .$spdeps == 'SURVIV' ~ 'Survivors',
    .$spdeps == 'FAM' ~ 'Family/Children',
    .$spdeps == 'UNEMPLOY' ~ 'Unemployment',
    .$spdeps == 'HOUSE' ~ 'Housing',
    .$spdeps == 'EXCLU' ~ 'Social exclusion n.e.c.',
    .$spdeps == 'SICKDISA' ~ 'Sickness/healthcare and disability',
    .$spdeps == 'OLDSURVIV' ~ 'Old age and survivors',
    TRUE ~ 'Miscellaneous goods and services'))

dt_PC_BEN <- dt_PC_BEN %>% select(expenses, year, values)

ggplotly(
  ggplot(dt_PC_BEN, aes(year, values, color=expenses)) +
    ggtitle('Europe - Social protection (Expenditure)') +
    geom_line() +
    labs(x="Year", y="Percentage of total expenditure") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
#########Pensions
################################################################################

##unit
# MIO_EUR	Million euro
# EUR_HAB_KP10	Euro per inhabitant (at constant 2010 prices)
# MIO_NAC	Million units of national currency
# MIO_PPS	Million purchasing power standards (PPS)
# PPS_HAB	Purchasing power standard (PPS) per inhabitant
# PC_GDP	Percentage of gross domestic product (GDP)

##spdepb
# TOTAL	Total
# SCPOLDPEN	Old age pension
# SCPANTPEN	Anticipated old age pension
# SCPPARPEN	Partial pension
# SCPDISPEN	Disability pension
# SCPEARLYRED	Early retirement benefit due to reduced capacity to work
# SCPSURVPEN	Survivors pension
# SCPEARLYMARK	Early retirement benefit for labour market reasons

dt_spr_exp_pens <- get_eurostat("spr_exp_pens", stringsAsFactors = FALSE)

dt_spr_exp_pens_it <- dt_spr_exp_pens %>% filter(geo == 'IT')

dt_spr_exp_pens_it <- dt_spr_exp_pens_it %>% filter(spdepm == 'TOTAL')

dt_spr_exp_pens_it <- dt_spr_exp_pens_it %>% filter(unit == 'PC_GDP')

dt_spr_exp_pens_it <- dt_spr_exp_pens_it %>% mutate(year = year(time))

dt_spr_exp_pens_it <- dt_spr_exp_pens_it %>% select(spdepb, year, values)

dt_spr_exp_pens_it <- dt_spr_exp_pens_it %>% filter(spdepb != 'TOTAL')

dt_spr_exp_pens_it <- dt_spr_exp_pens_it %>%
  mutate(type = case_when(
    .$spdepb == 'SCPOLDPEN' ~ 'Old age pension',
    .$spdepb == 'SCPANTPEN' ~ 'Anticipated old age pension',
    .$spdepb == 'SCPPARPEN' ~ 'Partial pension',
    .$spdepb == 'SCPDISPEN' ~ 'Disability pension',
    .$spdepb == 'SCPEARLYRED' ~ 'Early retirement benefit due to reduced capacity to work',
    .$spdepb == 'SCPSURVPEN' ~ 'Survivors pension',
    TRUE ~ 'Early retirement benefit for labour market reasons'))

dt_spr_exp_pens_it <- dt_spr_exp_pens_it %>% select(type, year, values)

ggplotly(
  ggplot(dt_spr_exp_pens_it, aes(year, values, color=type)) +
    ggtitle('Italy - Pensions') +
    geom_line() +
    labs(x="Year", y="Percentage of gross domestic product") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_spr_exp_pens_eu <- dt_spr_exp_pens %>% filter(geo == 'EU28')

dt_spr_exp_pens_eu <- dt_spr_exp_pens_eu %>% filter(spdepm == 'TOTAL')

dt_spr_exp_pens_eu <- dt_spr_exp_pens_eu %>% filter(unit == 'PC_GDP')

dt_spr_exp_pens_eu <- dt_spr_exp_pens_eu %>% mutate(year = year(time))

dt_spr_exp_pens_eu <- dt_spr_exp_pens_eu %>% select(spdepb, year, values)

dt_spr_exp_pens_eu <- dt_spr_exp_pens_eu %>% filter(spdepb != 'TOTAL')

dt_spr_exp_pens_eu <- dt_spr_exp_pens_eu %>%
  mutate(type = case_when(
    .$spdepb == 'SCPOLDPEN' ~ 'Old age pension',
    .$spdepb == 'SCPANTPEN' ~ 'Anticipated old age pension',
    .$spdepb == 'SCPPARPEN' ~ 'Partial pension',
    .$spdepb == 'SCPDISPEN' ~ 'Disability pension',
    .$spdepb == 'SCPEARLYRED' ~ 'Early retirement benefit due to reduced capacity to work',
    .$spdepb == 'SCPSURVPEN' ~ 'Survivors pension',
    TRUE ~ 'Early retirement benefit for labour market reasons'))

dt_spr_exp_pens_eu <- dt_spr_exp_pens_eu %>% select(type, year, values)

ggplotly(
  ggplot(dt_spr_exp_pens_eu, aes(year, values, color=type)) +
    ggtitle('Europe - Pensions') +
    geom_line() +
    labs(x="Year", y="Percentage of gross domestic product") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)


################################################################################
################GOVERNMENT FINANCE AND STATISTICS###############################
################################################################################

# https://ec.europa.eu/eurostat/web/government-finance-statistics/overview
# https://ec.europa.eu/eurostat/web/government-finance-statistics/data/database

################################################################################
##General government expenditure by function (COFOG)
################################################################################

##sector
# S13	General government
# S1311	Central government
# S1312	State government
# S1313	Local government
# S1314	Social security funds

##unit
# MIO_EUR	Million euro
# MIO_NAC	Million units of national currency
# PC_GDP	Percentage of gross domestic product (GDP)
# PC_TOT	Percentage of total

##na_item
# P2	Intermediate consumption
# D1	Compensation of employees
# D3	Subsidies
# P2_D29_D5_D8	Intermediate consumption; other taxes on production; current taxes on income, wealth, etc.; adjustment for the change in pension entitlements
# D29_D5_D8	Other taxes on production; current taxes on income, wealth, etc; adjustment for the change in pension entitlements
# D4	Property income
# D4_S1311	Property income, of which payable to subsector S1311
# D4_S1312	Property income, of which payable to subsector S1312
# D4_S1313	Property income, of which payable to subsector S1313
# D4_S1314	Property income, of which payable to subsector S1314
# D62_D632	Social benefits other than social transfers in kind and social transfers in kind - purchased market production
# D62	Social benefits other than social transfers in kind
# D632	Social transfers in kind - purchased market production
# D7	Other current transfers
# D7_S1311	Other current transfers,of which payable to subsector S1311
# D7_S1312	Other current transfers,of which payable to subsector S1312
# D7_S1313	Other current transfers,of which payable to subsector S1313
# D7_S1314	Other current transfers,of which payable to subsector S1314
# D9	Capital transfers
# D92	Investment grants
# D9_S1311	Capital transfers, of which payable to subsector S1311
# D9_S1312	Capital transfers, of which payable to subsector S1312
# D9_S1313	Capital transfers, of which payable to subsector S1313
# D9_S1314	Capital transfers, of which payable to subsector S1314
# OP5ANP	Gross capital formation and acquisitions less disposals of non-financial non-produced assets
# P5	Gross capital formation
# P51G	Gross fixed capital formation
# NP	Acquisitions less disposals of non-financial non-produced assets
# TE	Total general government expenditure
# P3	Final consumption expenditure
# P31	Individual consumption expenditure
# P32	Collective consumption expenditure

##sector
# S13	General government
# S1311	Central government
# S1312	State government
# S1313	Local government
# S1314	Social security funds

##cofog99
# TOTAL	Total
# GF01	General public services
# GF0101	Executive and legislative organs, financial and fiscal affairs, external affairs
# GF0102	Foreign economic aid
# GF0103	General services
# GF0104	Basic research
# GF0105	R&D General public services
# GF0106	General public services n.e.c.
# GF0107	Public debt transactions
# GF0108	Transfers of a general character between different levels of government
# GF02	Defence
# GF0201	Military defence
# GF0202	Civil defence
# GF0203	Foreign military aid
# GF0204	R&D Defence
# GF0205	Defence n.e.c.
# GF03	Public order and safety
# GF0301	Police services
# GF0302	Fire-protection services
# GF0303	Law courts
# GF0304	Prisons
# GF0305	R&D Public order and safety
# GF0306	Public order and safety n.e.c.
# GF04	Economic affairs
# GF0401	General economic, commercial and labour affairs
# GF0402	Agriculture, forestry, fishing and hunting
# GF0403	Fuel and energy
# GF0404	Mining, manufacturing and construction
# GF0405	Transport
# GF0406	Communication
# GF0407	Other industries
# GF0408	R&D Economic affairs
# GF0409	Economic affairs n.e.c.
# GF05	Environmental protection
# GF0501	Waste management
# GF0502	Waste water management
# GF0503	Pollution abatement
# GF0504	Protection of biodiversity and landscape
# GF0505	R&D Environmental protection
# GF0506	Environmental protection n.e.c.
# GF06	Housing and community amenities
# GF0601	Housing development
# GF0602	Community development
# GF0603	Water supply
# GF0604	Street lighting
# GF0605	R&D Housing and community amenities
# GF0606	Housing and community amenities n.e.c.
# GF07	Health
# GF0701	Medical products, appliances and equipment
# GF0702	Outpatient services
# GF0703	Hospital services
# GF0704	Public health services
# GF0705	R&D Health
# GF0706	Health n.e.c.
# GF08	Recreation, culture and religion
# GF0801	Recreational and sporting services
# GF0802	Cultural services
# GF0803	Broadcasting and publishing services
# GF0804	Religious and other community services
# GF0805	R&D Recreation, culture and religion
# GF0806	Recreation, culture and religion n.e.c.
# GF09	Education
# GF0901	Pre-primary and primary education
# GF0902	Secondary education
# GF0903	Post-secondary non-tertiary education
# GF0904	Tertiary education
# GF0905	Education not definable by level
# GF0906	Subsidiary services to education
# GF0907	R&D Education
# GF0908	Education n.e.c.
# GF10	Social protection
# GF1001	Sickness and disability
# GF1002	Old age
# GF1003	Survivors
# GF1004	Family and children
# GF1005	Unemployment
# GF1006	Housing
# GF1007	Social exclusion n.e.c.
# GF1008	R&D Social protection
# GF1009	Social protection n.e.c.

dt_gov_10a_exp <- get_eurostat("gov_10a_exp", stringsAsFactors = FALSE)

dt_gov_10a_exp_it <- dt_gov_10a_exp %>% filter(geo == 'IT')

dt_gov_10a_exp_it <- dt_gov_10a_exp_it %>% filter(unit == 'PC_GDP')

dt_gov_10a_exp_it <- dt_gov_10a_exp_it %>% filter(na_item == 'TE')

dt_gov_10a_exp_it <- dt_gov_10a_exp_it %>% filter(sector == 'S13')

dt_gov_10a_exp_it <- dt_gov_10a_exp_it %>% mutate(year = year(time))

dt_gov_10a_exp_it <- dt_gov_10a_exp_it %>% select(cofog99, year, values)

dt_gov_10a_exp_it <- dt_gov_10a_exp_it %>% 
  filter(cofog99 %in% c('GF01', 'GF02', 'GF03', 'GF04', 'GF05', 'GF06', 'GF07', 'GF08', 'GF09', 'GF10'))

dt_gov_10a_exp_it <- dt_gov_10a_exp_it %>%
  mutate(type = case_when(
    .$cofog99 == 'GF01' ~ 'General public services',
    .$cofog99 == 'GF02' ~ 'Defence',
    .$cofog99 == 'GF03' ~ 'Public order and safety',
    .$cofog99 == 'GF04' ~ 'Economic affairs',
    .$cofog99 == 'GF05' ~ 'Environmental protection',
    .$cofog99 == 'GF06' ~ 'Housing and community amenities',
    .$cofog99 == 'GF07' ~ 'Health',
    .$cofog99 == 'GF08' ~ 'Recreation, culture and religion',
    .$cofog99 == 'GF09' ~ 'Education',
    TRUE ~ 'Social protection'))

dt_gov_10a_exp_it <- dt_gov_10a_exp_it %>% select(type, year, values)

ggplotly(
  ggplot(dt_gov_10a_exp_it, aes(year, values, color=type)) +
    ggtitle('Italy - Government finance and statistics') +
    geom_line() +
    labs(x="Year", y="Percentage of gross domestic product") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)

################################################################################
###################COMPARE WITH EUROPE##########################################
################################################################################
dt_gov_10a_exp_eu <- dt_gov_10a_exp %>% filter(geo == 'EU27_2020')

dt_gov_10a_exp_eu <- dt_gov_10a_exp_eu %>% filter(unit == 'PC_GDP')

dt_gov_10a_exp_eu <- dt_gov_10a_exp_eu %>% filter(na_item == 'TE')

dt_gov_10a_exp_eu <- dt_gov_10a_exp_eu %>% filter(sector == 'S13')

dt_gov_10a_exp_eu <- dt_gov_10a_exp_eu %>% mutate(year = year(time))

dt_gov_10a_exp_eu <- dt_gov_10a_exp_eu %>% select(cofog99, year, values)

dt_gov_10a_exp_eu <- dt_gov_10a_exp_eu %>% 
  filter(cofog99 %in% c('GF01', 'GF02', 'GF03', 'GF04', 'GF05', 'GF06', 'GF07', 'GF08', 'GF09', 'GF10'))

dt_gov_10a_exp_eu <- dt_gov_10a_exp_eu %>%
  mutate(type = case_when(
    .$cofog99 == 'GF01' ~ 'General public services',
    .$cofog99 == 'GF02' ~ 'Defence',
    .$cofog99 == 'GF03' ~ 'Public order and safety',
    .$cofog99 == 'GF04' ~ 'Economic affairs',
    .$cofog99 == 'GF05' ~ 'Environmental protection',
    .$cofog99 == 'GF06' ~ 'Housing and community amenities',
    .$cofog99 == 'GF07' ~ 'Health',
    .$cofog99 == 'GF08' ~ 'Recreation, culture and religion',
    .$cofog99 == 'GF09' ~ 'Education',
    TRUE ~ 'Social protection'))

dt_gov_10a_exp_eu <- dt_gov_10a_exp_eu %>% select(type, year, values)

ggplotly(
  ggplot(dt_gov_10a_exp_eu, aes(year, values, color=type)) +
    ggtitle('Europe - Government finance and statistics') +
    geom_line() +
    labs(x="Year", y="Percentage of gross domestic product") + 
    theme_bw() +
    theme(legend.position = 'top')
) %>%
  layout(legend = list(
    orientation = "h", x = 0, y =-0.2
  )
)
