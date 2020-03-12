library(readxl)
defense = read_excel('cyclonesFootball2019.xlsx', sheet='Defensive')
offense = read_excel('cyclonesFootball2019.xlsx', sheet = 'Offensive')
bio = read_excel('cyclonesFootball2019.xlsx', sheet = 'Biography')
str(defense)




defClean<- defense %>% 
  mutate(Name =as.factor(Name)) %>%
  mutate(Opponent_Opponent = as.factor(Opponent_Opponent)) %>%
  mutate_at(vars(contains("Tackles")), funs(as.numeric)) %>%
  mutate_at(vars(contains("Turnover")), funs(as.numeric)) %>%
  mutate_at(vars(contains("Pass")), funs(as.numeric)) 


bioClean<- bio %>% 
  mutate(Name =as.factor(Name)) %>%
  mutate_at(vars(contains("Weight")), funs(as.numeric))

bioClean %>%
  separate(Height, into = c("ft", "inches")) %>%
  mutate(Height = as.numeric(ft)*12 + as.numeric(inches))
bioClean[,ft] = bioClean[,Height]

str(bioClean)