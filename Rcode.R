#Current working directory
getwd()

#Setting working directory
setwd("C:/Users/shaon/Documents/Academics/Data Mgmt/Module6")

#Data Import for analysis
salaries <- read.csv("Salaries.csv")

#variable study
str(salaries)

#summary of data
summary(salaries)

salaries_cont <- salaries[,c(3,4,6)]

#study of outliers present in every variable
par(mar=c(9,5,1,1))
outliers <- boxplot(salaries_cont,las=2)

n<-length(salaries_cont)

var_analysis<- data.frame(variable= character(n),
                          lower_limit = double(n),
                          upper_limit = double(n),
                          no_of_outliers = double(n),
                          percentage_of_outliers = double(n),
                          stringsAsFactors=FALSE)

for ( i in 1:n){
  
  var_analysis$variable[i] = colnames(salaries_cont[i])
  var_analysis$lower_limit[i] = round(as.numeric(quantile(salaries_cont[,i],p=c(0.25))- 1.5*IQR(salaries_cont[,i])),2)
  var_analysis$upper_limit[i] = round(as.numeric(quantile(salaries_cont[,i],p=c(0.75))+ 1.5*IQR(salaries_cont[,i])),2)
  var_analysis$no_of_outliers[i] = sum((salaries_cont[,i]) < (quantile(salaries_cont[,i],p=c(0.25)) - 1.5*IQR(salaries_cont[,i])) |
                                         (salaries_cont[,i]) > (quantile(salaries_cont[,i],p=c(0.75)) + 1.5*IQR(salaries_cont[,i])))
  var_analysis$percentage_of_outliers[i] = round((var_analysis$no_of_outliers[i] /nrow(salaries_cont))*100,2)
}


variable_summary_analysis<- data.frame(variable= character(n),
                                       Q1 = double(n),
                                       Mean = double(n),
                                       Median = double(n),
                                       Q3 = double(n),
                                       IQR = double(n),
                                       stringsAsFactors=FALSE)

for ( i in 1:n){
  
  variable_summary_analysis$variable[i] = colnames(salaries_cont[i])
  variable_summary_analysis$Q1[i] = round(quantile(salaries_cont[,i],p=c(0.25)),4)
  variable_summary_analysis$Mean[i] = round(mean(salaries_cont[,i]),4)
  variable_summary_analysis$Median[i] =round(median(salaries_cont[,i]),4)
  variable_summary_analysis$Q3[i]= round(quantile(salaries_cont[,i],p=c(0.75)),4)
  variable_summary_analysis$IQR[i] = round(quantile(salaries_cont[,i],p=c(0.75)) -quantile(salaries_cont[,i],p=c(0.25)),4)
}

par(mar=c(9,5,1,1))

hist(salaries_cont$salary, 
     col="peachpuff", 
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Salary",
     #ylim =c(0,1),
     main = "Histogram of salary")
lines(density(salaries_cont$salary), # density plot
      lwd = 2, 
      col = "chocolate3")

library(ggplot2)
ggplot(salaries, aes(x = sex, y = salary, color = sex)) +
  geom_boxplot()+
  geom_point(size = 2, position = position_jitter(width = 0.2)) +
  stat_summary(fun.y = mean, geom = "point", shape = 20, size = 6, color = "black")+
  theme_classic() +
  facet_grid(.~rank)


##Modelling 


#Adding dummy variables
library('fastDummies')
data1 <- dummy_cols(salaries, select_columns = c('rank', 'discipline','sex'))

#data_new<-data1[,-c(1,2,5)]

model1 <- lm(salary~., data = salaries)
summary(model1)

stepwise1<-step(model1, direction = "backward")

model_step_backward <- lm(formula = salary ~ rank + discipline + yrs.since.phd + yrs.service, 
                       data = salaries)
summary(model_step_backward)



range(salaries$yrs.service)

library(dplyr)
data_new<- salaries %>% 
  mutate(service_category = case_when(
    between(yrs.service, 0, 20)~"upto20",
    between(yrs.service, 20, 40)~"20_40",
    between(yrs.service, 40, 60)~"40_60"))

data_new$service_category <- as.factor(data_new$service_category)
levels(data_new$service_category)

data_new$service_category <- relevel(data_new$service_category, ref = "upto20")
levels(data_new$service_category)
table(data_new$service_category)

model2 <- lm(formula = salary ~ rank + discipline + yrs.since.phd + sex + service_category, data = data_new)
summary(model2)

stepwise2<-step(model2, direction = "backward")

model3 <- lm(formula = salary ~ rank + discipline + yrs.since.phd + service_category, 
          data = data_new)
summary(model3)
