library(readxl)
defense = read_excel('cyclonesFootball2019.xlsx', sheet='Defensive')
offense = read_excel('cyclonesFootball2019.xlsx', sheet = 'Offensive')
bio = read_excel('cyclonesFootball2019.xlsx', sheet = 'Biography')
str(defense)

as_numeric = function(x) as.numeric(x)
new_offense +3

bio %>%
  separate(Height, into= c("ft","inches")) %>%
  mutate(Height = as.numeric(ft)*12 +as.numeric(inches)) 
