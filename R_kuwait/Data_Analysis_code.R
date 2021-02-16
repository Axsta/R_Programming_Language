View(Pilot_SN_Data)

#assigning name to the dataset
Pilot <- Pilot_SN_Data


#1.Renaming the data
names(Pilot)[names(Pilot) == "q01"] <- "Gender"
names(Pilot)[names(Pilot) == "q02"] <- "Age"
names(Pilot)[names(Pilot) == "q03"] <- "Status_In_Kuwait"
names(Pilot)[names(Pilot) == "q04"] <- "Education"
names(Pilot)[names(Pilot) == "q05"] <- "Occupation"
names(Pilot)[names(Pilot) == "q06_1"] <- "Twitter"
names(Pilot)[names(Pilot) == "q06_02"] <- "Facebook"
names(Pilot)[names(Pilot) == "q06_03"] <- "Youtube"
names(Pilot)[names(Pilot) == "q06_04"] <- "Instagram"
names(Pilot)[names(Pilot) == "q06_05"] <- "Whatsapp"
names(Pilot)[names(Pilot) == "q06_06"] <- "LinkedIn"
names(Pilot)[names(Pilot) == "q08"] <- "Business_IG"
names(Pilot)[names(Pilot) == "q09"] <- "Buy_from_IG"

#output
View(Pilot)


#2.Bar plot for the age group
counts <- table(Pilot$Age)
names(counts)[names(counts) == "1"] <- "Under 17"
names(counts)[names(counts) == "2"] <- "18-24"
names(counts)[names(counts) == "3"] <- "25-34"
names(counts)[names(counts) == "4"] <- "35-44"
names(counts)[names(counts) == "5"] <- "45-54"
names(counts)[names(counts) == "6"] <- "55 and older"
names(counts)[names(counts) == "7"] <- "Unknown"
names(counts)[names(counts) == "8"] <- "Unknown"
barplot(counts, main = "Participants by Age", xlab = "Age Group", ylab = "Count", col = "Blue")


#3.Frequency table for aAge and AgeR

library(plyr)

cross_tab = table(Pilot$Age, Pilot$AgeR)
cross_tab

#4. Bar plot for multiple columns (Social Network)

value <- c(table(Pilot$Twitter)[1], table(Pilot$Facebook)[1], table(Pilot$Youtube)[1], table(Pilot$Instagram)[1], table(Pilot$Whatsapp)[1],table(Pilot$LinkedIn)[1])
names(value) <- c("Twitter", "Facebook", "Youtube", "Instagram", "Whatsapp", "LinkedIn")

barplot(value, main = "Social Media Usage", xlab = "Social Network", ylab = "Usage", col = "Red")


#5. Chi-square test to determine if Buy from IG depends on Age

test <- chisq.test(table(Pilot$Age, Pilot$`Buy_from_IG`))
test

test$statistic
test$p.value


#6. Warning sign in Chi-squared test

table(Pilot$Age, Pilot$`Buy_from_IG`)


#7. comparison of approximation by replacing Age with AgeR

#Age
test$observed
test$expected

#AgeR
test <- chisq.test(table(Pilot$Age, Pilot$`Buy_from_IG`))
test


test$observed
test$expected



#8.Logistic Regression

#converting multiple columns to factors
cat <- c("Business_IG", "Buy_from_IG", "Gender",  "AgeR",  "Status_In_Kuwait", "Education",  "Occupation")

Pilot[cat] <- lapply(Pilot[cat], factor)


# performing Logistic regression
data = glm(formula = Business_IG ~ Buy_from_IG + Gender + AgeR + Status_In_Kuwait + Education + Occupation , data = Pilot, family = binomial)

print(summary(data))
