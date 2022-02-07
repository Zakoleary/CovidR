library(Hmisc)

data<- read.csv("C:/Users/zakol/OneDrive/Desktop/covid_r/COVID19_line_list_data.csv")
describe(data) #uses hmisc - can see some null values


#cleaning up values to only be 0 or 1
data$death_dummy <- as.integer(data$death != 0)
unique(data$death_dummy)
#[1] 0 1

#death rate total dataset
sum(data$death_dummy) / nrow(data)
#[1] 0.05806452

#ASK - people who are older are more susceptible to dying from COVID?
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)

#Some values need to be removed by adding na.rm = TRUE
mean(dead$age, na.rm = TRUE)
#[1] 68.58621
mean(alive$age, na.rm = TRUE)
#[1] 48.07229

#Test for significance 
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.95)
#95 percent confidence interval:
#-24.28669 -16.74114 -> person alive is much younger
#p-value < 2.2e-16 -> if p-value < 0.05 reject null hypothesis
#p-value is 0 so reject the null hypothesis. People who die from covid are much older than people who live.

#ASK - Gender has no effect 
men = subset(data, gender =="male")
women = subset(data, gender == "female")

mean(men$death_dummy, na.rm = TRUE)
#[1] 0.08461538
mean(women$death_dummy, na.rm = TRUE)
#[1] 0.03664921

#Test for significance 
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.95)
#95 percent confidence interval:
# 0.01744083 0.07849151 -> men have from 1.7% to 7.8% higher chance of dying from COVID.
#p-value = 0.002105 < 0.05, so we reject the null hypothesis.
