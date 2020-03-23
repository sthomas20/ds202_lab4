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

#You can see that based on the perfomance since Purdy is the only offensive player with significant passing statistics obvioulsy because he is the main qb of the team. Therefore you can see that the higher recieving 

defOff<- full_join(offClean, defClean, by= c('Opponent_Opponent', 'Name'))
defOff2<- select(defOff, -c('Passing_CMP-ATT')) %>% 
  distinct(defOff, Name, Opponent_Opponent, .keep_all = TRUE)
View(new)

avgISU<- defOff2 %>%
  mutate(Rushing_ATT = sum(Rushing_ATT, na.rm = TRUE)) %>%
  mutate(Rushing_YDS = sum(Rushing_YDS, na.rm = TRUE)) %>%
  mutate(Rushing_TD = sum(Rushing_TD, na.rm = TRUE)) %>%
  mutate(Receiving_REC = sum(Receiving_REC, na.rm = TRUE)) %>%
  mutate(Receiving_TD = sum(Receiving_TD, na.rm = TRUE)) %>%
  mutate(Passing_YDS = sum(Passing_YDS, na.rm = TRUE)) %>%
  mutate(Passing_TD = sum(Passing_TD, na.rm = TRUE)) %>%
  mutate(Passing_INT = sum(Passing_INT, na.rm = TRUE)) %>%
  mutate(Tackles_Solo = sum(Tackles_Solo, na.rm = TRUE)) %>%
  mutate(Tackles_ASST = sum(Tackles_ASST, na.rm = TRUE)) %>%
  mutate(Tackles_TFL = sum(Tackles_TFL, na.rm = TRUE)) %>%
  mutate(Tackles_Sack = sum(Tackles_Sack, na.rm = TRUE)) %>%
  mutate(Turnover_FF = sum(Turnover_FF, na.rm = TRUE)) %>%
  mutate(Turnover_FR = sum(Turnover_FR, na.rm = TRUE)) %>%
  mutate(Turnover_INT = sum(Turnover_INT, na.rm = TRUE)) %>%
  mutate(Pass_QBH = sum(Pass_QBH, na.rm = TRUE)) %>%
  mutate(Pass_PB = sum(Pass_PB, na.rm = TRUE))
  
new<- head(avgISU, 1)
new1<- select(new, -c(Name, Opponent_Opponent))
new2<- new1 %>%
  pivot_longer(1:18, names_to = 'Type', values_to = 'Statistics')
new3<- new2 %>%
  mutate(Name = "ISU TEAM")

purdyB<- select(defOff, -c('Passing_CMP-ATT')) %>%
  filter(Name %in% c('Purdy, Brock')) %>%
  mutate(Rushing_ATT = sum(Rushing_ATT, na.rm = TRUE)) %>%
  mutate(Rushing_YDS = sum(Rushing_YDS, na.rm = TRUE)) %>%
  mutate(Rushing_TD = sum(Rushing_TD, na.rm = TRUE)) %>%
  mutate(Receiving_REC = sum(Receiving_REC, na.rm = TRUE)) %>%
  mutate(Receiving_TD = sum(Receiving_TD, na.rm = TRUE)) %>%
  mutate(Passing_YDS = sum(Passing_YDS, na.rm = TRUE)) %>%
  mutate(Passing_TD = sum(Passing_TD, na.rm = TRUE)) %>%
  mutate(Passing_INT = sum(Passing_INT, na.rm = TRUE)) %>%
  mutate(Tackles_Solo = sum(Tackles_Solo, na.rm = TRUE)) %>%
  mutate(Tackles_ASST = sum(Tackles_ASST, na.rm = TRUE)) %>%
  mutate(Tackles_TFL = sum(Tackles_TFL, na.rm = TRUE)) %>%
  mutate(Tackles_Sack = sum(Tackles_Sack, na.rm = TRUE)) %>%
  mutate(Turnover_FF = sum(Turnover_FF, na.rm = TRUE)) %>%
  mutate(Turnover_FR = sum(Turnover_FR, na.rm = TRUE)) %>%
  mutate(Turnover_INT = sum(Turnover_INT, na.rm = TRUE)) %>%
  mutate(Pass_QBH = sum(Pass_QBH, na.rm = TRUE)) %>%
  mutate(Pass_PB = sum(Pass_PB, na.rm = TRUE))

purdyB1<- head(purdyB, 1)
purdyB2<- select(purdyB1, -c(Opponent_Opponent))
purdyB3<- purdyB2 %>%
  pivot_longer(2:19, names_to = 'Type', values_to = 'Statistics')



completeISU<- full_join(new3, purdyB3, by=c('Type', 'Name', 'Statistics'))

completeISU

ggplot(data=completeISU, aes(x=Type, y = Statistics, fill = Name)) + geom_bar(stat='identity', position = position_dodge()) + theme(axis.text.x = element_text(angle =45, vjust = 0.5)) + xlab("Statistic") + ylab('Count') + ggtitle("Purdy's vs Team Performance ")



#Question 4
#How does Redshirt Freshman performance compare to Redshirt senior Performance 
age<- full_join(avgISU, bioClean, by= c('Name'))

View(age)




freshman<- age %>%
filter(Class %in% c('Freshman')) 

freshman1<- select(freshman, -c(Name, Opponent_Opponent))
freshman2<- freshman1 %>%
  pivot_longer(1:18, names_to = 'Type', values_to = 'Statistics')
freshman3<- freshman2 %>%
  mutate(Name = "FRESHMAN")
freshman3

junior<- age %>%
  filter(Class %in% c('Junior')) 

junior1<- select(junior, -c(Name, Opponent_Opponent))
junior2<- junior1 %>%
  pivot_longer(1:18, names_to = 'Type', values_to = 'Statistics')
junior3<- junior2 %>%
  mutate(Name = "JUNIOR")
junior3


redfreshman<- age %>%
  filter(Class %in% c('Redshirt Freshman')) 


redfreshman1<- select(redfreshman, -c(Name, Opponent_Opponent))
redfreshman1
redfreshman2<- redfreshman1 %>%
  pivot_longer(1:18, names_to = 'Type', values_to = 'Statistics')
redfreshman2
redfreshman3<- redfreshman2 %>%
  mutate(Name = "REDSHIRT FRESHMAN")
redfreshman3

redjunior<- age %>%
  filter(Class %in% c('Redshirt Junior')) 

redjunior1<- select(redjunior, -c(Name, Opponent_Opponent))
redjunior2<- redjunior1 %>%
  pivot_longer(1:18, names_to = 'Type', values_to = 'Statistics')
redjunior3<- redjunior2 %>%
  mutate(Name = "REDSHIRT JUNIOR")
redjunior3

redsenior<- age %>%
  filter(Class %in% c('Redshirt Senior')) 

redsenior1<- select(redsenior, -c(Name, Opponent_Opponent))
redsenior2<- redsenior1 %>%
  pivot_longer(1:18, names_to = 'Type', values_to = 'Statistics')
redsenior3<- redsenior2 %>%
  mutate(Name = "REDSHIRT SENIOR")
redsenior3





completeClass<- full_join(redfreshman3, redsenior3, by=c('Type', 'Name', 'Statistics'))
completeClass

ggplot(data=redfreshman3, aes(x=Type, y = Statistics, fill = Name)) + geom_bar(stat='identity', position = position_dodge()) + theme(axis.text.x = element_text(angle =45, vjust = 0.5)) + xlab("Statistic") + ylab('Count') + ggtitle("Purdy's vs Team Performance ")


#How does defense statistics compare to offensive?
  defClean %>%
  ggplot(aes(value)) + geom_histogram(binwidth = .5) + facet_wrap(~Statistic) + ggtitle("Defensive Statistics")

offClean %>%
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~Statistic) + ggtitle("Offensive Statistics")


