.libPaths("H:/myCitrixFiles/Documents/R/win-library/3.2")

library(dplyr)
library(ggplot2)
library(tidyr)

#read data
datafiles <- list.files('data')

read_data <- function(x)
{
  temp <- read.csv(x)
  
  temp <- temp %>%
    mutate_all(., function(col){gsub(',', '.', col)})
  
  temp
}

for(i in 1:length(datafiles))
{
  filename <- strsplit(datafiles[i], '[.]')[[1]][1] 
  assign(filename, read_data(paste0('data/', datafiles[i])))
}


patients <- read.csv('data/patient_record.csv')

time_name <- function(x)
{
  strsplit(x, '_')[[1]][1]
}

tidy_sys <- patients %>%
  select(Patient.Number, Session, Temperature..C.., Hypertensive.Hypotensive, contains('_Systolic')) %>%
  gather(key = 'Time', value = Systolic_BP, contains('_Systolic')) %>%
  separate(Time, c('Time', 'garbage'), sep = '_') %>%
  select(-garbage) %>%
  mutate(Time = gsub('X', '', Time))







### ----------------------------- OLD -------------------------------- ###

test_data <- read.csv('test.csv')

test_mut2 <- test_data %>%
  mutate_all(., function(col){gsub(',', '.', col)})

test_mut <- test_data %>%
  mutate(BP_Systolic = as.numeric(gsub(',', '.', as.character(Systolic.Blood.Pressure..mmHg.))),
         BP_Diastolic = as.numeric(gsub(',', '.', as.character(Diastolic.Blood.Pressure..mmHg.))),
         Time_numeric = row_number())

test_mut$Time = factor(test_mut$Time, levels=c('Pre', '1st', '2nd', '3rd', '4th', 'Post'))

ggplot(test_mut, aes(x=Time)) +
  geom_point(aes(y=BP_Systolic), colour='orange') +
  geom_line(aes(y=BP_Systolic), colour='orange') +
  

lm(BP_Systolic ~ Time_numeric, test_mut)
