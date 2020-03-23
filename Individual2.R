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



defClean = defClean %>%
  pivot_longer(Tackles_Solo:Pass_PB, names_to = 'Statistic')

defClean %>%
  ggplot(aes(value)) + geom_histogram(binwidth = .5) +facet_wrap(~Statistic) + ggtitle("Defensive Statistic Histograms")

dat2_3 = defClean %>%
  filter(Opponent_Opponent == "West Virginia" | Opponent_Opponent == "Kansas State", Statistic == "Tackles_Solo") %>%
  pivot_wider(names_from = Opponent_Opponent, values_from =  value)

dat2_3$`West Virginia`[is.na(dat2_3$`West Virginia`)] = 0
dat2_3$`Kansas State`[is.na(dat2_3$`Kansas State`)] = 0
dat2_3 = dat2_3 %>%
  filter(`West Virginia` != 0 | `Kansas State` != 0)

p = dat2_3 %>%
  ggplot(aes(x = `West Virginia`, y = `Kansas State`)) + geom_count() + coord_cartesian(xlim = c(0,6))
p = p + ggtitle("Scatter Plot of Solo Tackles For Each Player Against West Virginia and Against Kansas State")
p

join = offClean %>%
  left_join(bioClean, b= "Name")
join$Name = as.factor(join$Name)
join$Receiving_YDS[is.na(join$Receiving_YDS)] = 0

dat3_2 = join %>%
  group_by(join$Name) %>%
  summarise(
    Total_Receiving_Yards = sum(Receiving_YDS),
    Weight = mean(Weight)
  ) %>%
  drop_na()

dat3_2 %>%
  ggplot(aes(x= Weight, y = Total_Receiving_Yards)) + geom_point() + xlim(c(0,300)) + xlab("Weight (lbs)") + ylab("Total Receiving Yards") + ggtitle("ISU Individual Receiving Yards\nin the 2019 Season Vs. Weight")

