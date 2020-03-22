library(readxl)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
defense <- read_excel('cyclonesFootball2019.xlsx', sheet='Defensive')
offense <- read_excel('cyclonesFootball2019.xlsx', sheet='Offensive')
bio<- read_excel('cyclonesFootball2019.xlsx', sheet='Biography')


#PART ONE: Cleaning Data 

#Question 1-3
defClean<- defense %>% 
  mutate(Name =as.factor(Name)) %>%
  mutate(Opponent_Opponent = as.factor(Opponent_Opponent)) %>%
  mutate_at(vars(contains("Tackles")), funs(as.numeric)) %>%
  mutate_at(vars(contains("Turnover")), funs(as.numeric)) %>%
  mutate_at(vars(contains("Pass")), funs(as.numeric)) 

str(defClean)

offClean<- offense %>% 
  mutate(Name =as.factor(Name)) %>%
  mutate(Opponent_Opponent = as.factor(Opponent_Opponent)) %>%
  mutate_at(vars(contains("Rushing")), funs(as.numeric)) %>%
  mutate_at(vars(contains("Receiving")), funs(as.numeric)) %>%
  mutate_at(vars(contains("Passing")), funs(as.numeric)) 

str(offClean)

bioClean<- bio %>% 
  mutate(Name =as.factor(Name)) %>%
  mutate_at(vars(contains("Weight")), funs(as.numeric)) %>%
  separate(Height, into = c("ft", "inches")) %>%
  mutate(Height = as.numeric(ft)*12 + as.numeric(inches)) %>%
  select(-c(ft, inches)) %>%
  select(1:2, Height, everything())

str(bioClean)

##PART TWO: TIDYING(1)

defClean = defClean %>%
  pivot_longer(Tackles_Solo:Pass_PB, names_to = 'Statistic')

defClean %>%
  ggplot(aes(value)) + geom_histogram(binwidth = .5) + facet_wrap(~Statistic) + ggtitle("Defensive Statistic Histograms")

dat2_3 = defClean %>%
  filter(Opponent_Opponent == "West Virginia" | Opponent_Opponent == "Kansas State", Statistic == "Tackles_Solo") %>%
  pivot_wider(names_from = Opponent_Opponent, values_from =  value)

dat2_3$`West Virginia`[is.na(dat2_3$`West Virginia`)] = 0
dat2_3$`Kansas State`[is.na(dat2_3$`Kansas State`)] = 0
dat2_3 = dat2_3 %>%
  filter(`West Virginia` != 0 | `Kansas State` != 0)

p = dat2_3 %>%
  ggplot(aes(x = `West Virginia`, y = `Kansas State`)) + geom_count() + coord_cartesian(xlim = c(0,6))
p = p + ggtitle("Scatter Plot of Solo Tackles For Each Player\n Against West Virginia and Kansas State")
p

#PART TWO: TIDYING(2)

#Question 1

bioClean <- bioClean %>%
  separate(col=Hometown, into=c('City', 'State'), sep=',') 

head(bioClean, n=3)

#Question 2
table(bioClean$State)


#PART THREE: JOINING DATA FRAMES

#Question 3
offClean <- offClean %>%
  pivot_longer(Rushing_ATT:Passing_INT, names_to = 'Statistic')

offClean
