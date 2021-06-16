library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
#install.packages("gmodels")
library(gmodels)
library(scales)
data <- read_csv("C:\\Users\\Vaishnavi\\Desktop\\Voters.csv")
  glimpse(data)

# people participated in the survey
Fig1 <- data %>% drop_na(indian_origin) %>% 
  ggplot(aes(x = indian_origin, fill = gender )) + geom_bar(position = "dodge" ) +
  ggtitle("Type of voters participated in this survey") + 
  labs(x = "Do you live in India?", y = "No of participants", fill = "Gender")

#NRI participants
Fig2 <- data %>% drop_na(`NRI Voter`) %>% 
  ggplot(aes(x = `NRI Voter`, fill = gender )) + geom_bar(position = "dodge" ) +
  ggtitle("NRI voters participated in this survey") + 
  labs(x = "Have you enrolled yourself as a Overseas Elector?",
       y = "No of participants", fill = "Gender")

#Population divided in age groups
Fig3 <- data %>% drop_na(age_group) %>% 
  ggplot(aes(x = age_group, fill = employment))+ geom_bar(position = "dodge") +
  ggtitle("Age Group of Voters") + 
  labs(x = "Age group", y = "No of participants", fill = "Employment Status")

Fig4 <- data %>% drop_na(age_group) %>% 
  ggplot(aes(x = age_group, fill = voter_type_cat))+ geom_bar(position = "dodge") +
  ggtitle("Age Group of Voters") + 
  labs(x = "Age group", y = "No of participants", fill = "Employment Status")

Fig5 <- data %>% drop_na(elections_type_cat) %>% 
  ggplot(aes(x = elections_type_cat, fill = NOTA_vote)) + geom_bar(position = "dodge")

Fig6 <- data %>% drop_na(`good practice`) %>%  
  ggplot(aes(x = `good practice`, fill = employment)) + geom_bar(position = "dodge")

Fig7 <- data %>% drop_na(research) %>% ggplot(aes(x = research, fill = age_group)) + 
  geom_bar(position = "dodge")

Fig8 <- data %>% drop_na(age_group, source_research_cat) %>% 
  ggplot(aes(x = age_group, fill = source_research_cat)) + 
  geom_bar(position = "dodge")


Fig9 <- ggplot(data, aes(x= age_group,  fill = choice_factor_cat)) + 
  geom_bar(aes(y = ..count../ sum(..count..)*100),position = 'dodge', stat = "count") + 
  ggtitle("Proportion of Gender")+ xlab("Age groups")+ ylab("Percentage(%)") + fill("Main factor for choosing a candidate")


table(data$age_group, data$choice_factor_cat)

percentage_data <- data %>% count(choice_factor_cat) %>% mutate(percentage = (n/sum(n))*100)

fig10 <- data %>% ggplot(aes(x = elections_type_cat, fill = choice_factor_cat )) + geom_bar(position = 'dodge') 

fig11 <- data %>%  ggplot(aes(x = expectations_cat, fill = gender)) + geom_bar(position = 'dodge')

fig12 <- data %>% ggplot(aes(x= `current_voting _system_cat`, fill = employment)) + geom_bar(position = 'dodge')
fig13 <- data %>% drop_na(root_cause_cat) %>% 
  ggplot(aes(fill = root_cause_cat, x= age_group )) + 
  geom_bar(position = 'dodge')
fig14 <- data %>%drop_na(suggestion_cat) %>% 
  ggplot(aes(y = suggestion_cat)) + geom_bar(position = 'dodge')


fig15<- data %>% drop_na(suggestion_cat) %>% 
  group_by(suggestion_cat) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(suggestion_cat))
ggplot(data=fig15)+
  geom_bar(aes(x="", y=per, fill= suggestion_cat), stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void() +
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=scales::percent(per)))

#marginal distribution on rows: Age group
CrossTable(data$age_group, data$choice_factor_cat, digits = 2, format="SPSS", prop.r = T, prop.c = F, 
           prop.t = F, prop.chisq = F, dnn = c("Age Group", "Main factor for choosing a political candidate"))
#Conclusion: from 18-25 age group 61.54% think Education is the main factor for
#from 25-35 age group 80% think Experience is the main factor
#from 35-40 age group 62% think education 
#from 50 and above group 83% think education


#marginal distribution on columns: Factor
CrossTable(data$age_group, data$choice_factor_cat, digits = 2, format="SPSS", prop.r = F, prop.c = T, 
           prop.t = F, prop.chisq = F, dnn = c("Age Group", "Main factor for choosing a political candidate"))
#conclusion:
#1. age/Gender is the main factor acc to 18-25 age group



#marginal distribution on rows: Gender
CrossTable(data$gender, data$root_cause_cat, digits = 2, format="SPSS", prop.r = T, prop.c = F, 
           prop.t = F, prop.chisq = F, dnn = c("Gender", " Root cause of the flawed voting system"))

#marginal distribution on columns: Root cause
CrossTable(data$gender, data$root_cause_cat, digits = 2, format="SPSS", prop.r = F, prop.c = T, 
           prop.t = F, prop.chisq = F, dnn = c("Gender", " Root cause of the flawed voting system"))

#Marginal distribution on both rows and columns:gender and suugestion_cat
CrossTable(data$gender, data$suggestion_cat, digits = 2, format="SPSS", prop.r = T, prop.c = T, 
           prop.t = F, prop.chisq = F, dnn = c("Gender", " Suggestions for making voting system better"))


#Marginal distribution on both rows and columns: Gender and electiontype
CrossTable(data$gender, data$elections_type_cat, digits = 2, format="SPSS", prop.r = T, prop.c = T, 
           prop.t = F, prop.chisq = F, dnn = c("Gender", "Election type"))

CrossTable(data$employment, data$elections_type_cat, digits = 2,  format="SPSS", prop.r = T, prop.c = F, 
           prop.t = F, prop.chisq = F, dnn = c("Employment status", "Election type"))
CrossTable(data$`good practice`,data$age_category, digits = 2,  format="SPSS", prop.r = T, prop.c = F, 
           prop.t = F, prop.chisq = F, dnn = c("Voting is s good practice", "Voter type"))

good_practice <- table(data$`good practice`,data$age_category)
warning =F
test1 <- chisq.test(table(data$`good practice`,data$age_category))
test1$observed
test1$expected
test1$residuals
test2 <- chisq.test(table(data$`current_voting _system_cat`,data$age_category))
test3 <- chisq.test(table(data$gender, data$voter_type_cat), correct = F)
test4 <- chisq.test(table(data$indian_origin, data$gender), correct = F)
test5 <- chisq.test(table(data$employment, data$gender), correct = F)
test6 <- chisq.test(table(data$`NRI Voter`, data$gender), correct = F)
test7 <- chisq.test(table(data$NOTA_vote, data$education), correct = F)
test8 <- chisq.test(table(data$indian_origin, data$voter_type_cat), correct = F)

test9 <- chisq.test(table(data$age_group, data$`good practice`), correct = F)


t.test(data$`age_Voters Electoral List`)
t.test(data$`age_Voters Electoral List` ~ data$gender, data, var.equal = TRUE)
data$indian_origin[data$indian_origin = "No"]

data %>% group_by(indian_origin) 
df <- data %>% summarise(indian_origin, voter_type_cat) %>% filter(voter_type_cat == "Inactive/Inconsistent")
test8 <- chisq.test(table(df$indian_origin, df$voter_type_cat), correct = F)
t.test(data$NOTA_vote, paired = TRUE)
cor.test(data$NOTA_vote, data$gender)                                                      
glmer(NOTA_vote ~ gender, data, family = binomial)
