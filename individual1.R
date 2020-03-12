library(readxl)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
defense <- read_excel('cyclonesFootball2019.xlsx', sheet='Defensive')
offense <- read_excel('cyclonesFootball2019.xlsx', sheet='Offensive')
bio<- read_excel('cyclonesFootball2019.xlsx', sheet='Biography')


#PART ONE: Cleaning Data 

#Question 1 and 2
defClean<- defense %>% 
  mutate(Name =as.factor(Name)) %>%
  mutate(Opponent_Opponent = as.factor(Opponent_Opponent)) %>%
  mutate_at(vars(contains("Tackles")), funs(as.numeric)) %>%
  mutate_at(vars(contains("Turnover")), funs(as.numeric)) %>%
  mutate_at(vars(contains("Pass")), funs(as.numeric)) 
  
str(defense)

offClean<- offense %>% 
  mutate(Name =as.factor(Name)) %>%
  mutate(Opponent_Opponent = as.factor(Opponent_Opponent)) %>%
  mutate_at(vars(contains("Rushing")), funs(as.numeric)) %>%
  mutate_at(vars(contains("Receiving")), funs(as.numeric)) %>%
  mutate_at(vars(contains("Passing")), funs(as.numeric)) 
 
str(offense)

bioClean<- bio %>% 
  mutate(Name =as.factor(Name)) %>%
  mutate_at(vars(contains("Weight")), funs(as.numeric)) %>%
  separate(Height, into = c("ft", "inches")) %>%
  mutate(Height = as.numeric(ft)*12 + as.numeric(inches)) 
  
str(bioClean)

#Question 3

