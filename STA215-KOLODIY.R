title: "STA215-KOLODIY"
author: "Anna Kolodiy"
date: "`r Sys.Date()`"
output: html_document


#install.packages("ggplot2")
#install.packages("psych")

library(ggplot2)
library(psych)

#Load Data
data <- read.csv("raw_data.csv")

#Table 1 - Descriptive Statistics
#Code to View the Major Descriptive Statistics of Gender

table(data$gender)
describe(data$gender)
summary(data$gender)
data$gender <- factor(data$gender, 
                      levels = c(1, 2), 
                      labels = c("Woman", "Man"))

#Code to View the Major Descriptive Statistics of Abortion View

table(data$abortion)
describe(data$abortion)
summary(data$abortion)

#Code to View the Major Descriptive Statistics of Age
#Convert Age to Numeric
class(data$age) 
#reformat NA
data$age[data$age == ""] <- NA
data$age[data$age == "NA"] <- NA
data$age <- as.numeric(data$age)

mean(data$age)
sd(data$age)
table(data$age)
describe(data$age)
summary(data$age)

#Code to View the Major Descriptive Statistics of Term
#Convert Term to Numeric
class(data$term) 
#reformat NA
data$term[data$term == ""] <- NA
data$term[data$term == "NA"] <- NA
data$term <- as.numeric(data$term)

mean(data$term)
sd(data$term)
table(data$term)
describe(data$term)
summary(data$term)

#Table 2 Contingency Table

contingency_table <- table(data$gender, data$abortion)
print(contingency_table)

chisq.test(contingency_table)

#Figure 1 - Boxplot (Gender vs Voting Record)
#Convert Voting Record to Numeric
class(data$voting_record) 
#reformat NA
data$voting_record[data$voting_record == ""] <- NA
data$voting_record[data$voting_record == "NA"] <- NA
data$voting_record <- as.numeric(data$voting_record)

ggplot(data = data, aes(y = voting_record, group = gender, fill = gender)) + 
  geom_boxplot() + 
  labs(y = "Vote Participation Percentage", x = "Gender", fill = "Gender") + 
  ggtitle("Relationship Between Gender and Vote Participation in House of Representatives") + 
  theme_classic() +
  scale_x_continuous(breaks = c()) +
  scale_fill_manual(values = c("Woman"="#d87bab","Man"="#AAEAE2"))


#Figure 2 - Scatterplot (Age vs Years in Office)

ggplot(data, aes(x = age, y = term)) +
  geom_point(color = "#9577A4") + 
  labs(x = "Age", y = "Years in Office") + 
  ggtitle("Relationship Between Age and Years in Office in House of Representatives") + 
  theme_classic() + 
  theme(plot.title = element_text(size = 12)) +
  geom_smooth(method = "lm", color = "black")


#Figure 3 - Residual Plot (Regression Residuals)
data_clean <- data[!is.na(data$age) & !is.na(data$term), ]

linear_age <- lm(term ~ age, data = data_clean)
data_clean$residuals <- residuals(linear_age)

ggplot(data_clean, aes(x = age, y = residuals)) + 
  geom_point(color = "#9577A4") + 
  labs(x = "Age", y = "Residuals") + 
  ggtitle("Residuals of Linear Regression Between Age and Years in Office") + 
  theme_classic() + 
  theme(plot.title = element_text(size = 12)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
