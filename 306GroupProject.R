#load required libraries
library(tidyverse)
library(leaps)
library(lubridate)
library(cowplot)
library(ggcorrplot)
library(car)
library(ggplot2)

data <- read.csv("data/Sleep_Efficiency.csv") |>
  relocate(Sleep.efficiency) |>
  na.omit(data) |>
  select(-c(Bedtime,Wakeup.time,ID))

data$Smoking.status <- ifelse(data$Smoking.status=="Yes",1,0) |>
  as.factor()
data$Gender <- ifelse(data$Gender=="Male",1,0) |>
  as.factor()
df <- read.csv("data/Sleep_Efficiency.csv") |>
  relocate(Sleep.efficiency) |>
  na.omit(data) |>
  select(Sleep.efficiency,Bedtime,Wakeup.time)

#view cleaned dataset size and summary statistics
head(data)
n <- nrow(data)
dim(data)
summary(data)

#one-hot encode all non-numeric variables to show correlation matrix
options(repr.plot.width=20,repr.plot.height=10)
model.matrix(~0+., data=data, contrasts.arg = list(Smoking.status = contrasts(data$Smoking.status, contrasts = FALSE))) |> 
  cor(use="pairwise.complete.obs") |> 
  ggcorrplot(show.diag=FALSE, type="upper", lab=TRUE, lab_size=3)

#create groups based on categorical variables for visualization
plot_age <-ggplot(data, aes(Age, Sleep.efficiency, color = Gender:Smoking.status)) + geom_point() +
  scale_color_manual(values = c("coral", "mediumseagreen", "cornflowerblue", "orchid"),labels=c('Female Nonsmoker', 'Female Smoker', 'Male Nonsmoker', 'Male Smoker')) +
  ggtitle("Sleep Efficiency and Age") + ylab("Sleep Efficiency") + xlab("Age (years)")
plot_dur <-ggplot(data, aes(Sleep.duration, Sleep.efficiency, color = Gender:Smoking.status)) + geom_point() +
  scale_color_manual(values = c("coral", "mediumseagreen", "cornflowerblue", "orchid"),labels=c('Female Nonsmoker', 'Female Smoker', 'Male Nonsmoker', 'Male Smoker')) +
  ggtitle("Sleep Efficiency and Sleep Duration") + ylab("Sleep Efficiency") + xlab("Sleep duration (hours)")
plot_rem <-ggplot(data, aes(REM.sleep.percentage, Sleep.efficiency, color = Gender:Smoking.status)) + geom_point() +
  scale_color_manual(values = c("coral", "mediumseagreen", "cornflowerblue", "orchid"),labels=c('Female Nonsmoker', 'Female Smoker', 'Male Nonsmoker', 'Male Smoker')) +
  ggtitle("Sleep Efficiency and REM Sleep") + ylab("Sleep Efficiency") + xlab("REM sleep percentage (%)")
plot_deep <-ggplot(data, aes(Deep.sleep.percentage, Sleep.efficiency, color = Gender:Smoking.status)) + geom_point() +
  scale_color_manual(values = c("coral", "mediumseagreen", "cornflowerblue", "orchid"),labels=c('Female Nonsmoker', 'Female Smoker', 'Male Nonsmoker', 'Male Smoker')) +
  ggtitle("Sleep Efficiency and Deep Sleep") + ylab("Sleep Efficiency") + xlab("Deep sleep percentage (%)")
plot_light <-ggplot(data, aes(Light.sleep.percentage, Sleep.efficiency, color = Gender:Smoking.status)) + geom_point() +
  scale_color_manual(values = c("coral", "mediumseagreen", "cornflowerblue", "orchid"),labels=c('Female Nonsmoker', 'Female Smoker', 'Male Nonsmoker', 'Male Smoker')) +
  ggtitle("Sleep Efficiency and Light Sleep") + ylab("Sleep Efficiency") + xlab("Light sleep percentage (%)")
plot_awake <-ggplot(data, aes(Awakenings, Sleep.efficiency, color = Gender:Smoking.status)) + geom_point() +
  scale_color_manual(values = c("coral", "mediumseagreen", "cornflowerblue", "orchid"),labels=c('Female Nonsmoker', 'Female Smoker', 'Male Nonsmoker', 'Male Smoker')) +
  ggtitle("Sleep Efficiency and Awakenings") + ylab("Sleep Efficiency") + xlab("# of times the subject woke up during the night")
plot_caf <-ggplot(data, aes(Caffeine.consumption, Sleep.efficiency, color = Gender:Smoking.status)) + geom_point() +
  scale_color_manual(values = c("coral", "mediumseagreen", "cornflowerblue", "orchid"),labels=c('Female Nonsmoker', 'Female Smoker', 'Male Nonsmoker', 'Male Smoker')) +
  ggtitle("Sleep Efficiency and Caffeine Consumption") + ylab("Sleep Efficiency") + xlab("Caffeine Consumption (mg)")
plot_alc <-ggplot(data, aes(Alcohol.consumption, Sleep.efficiency, color = Gender:Smoking.status)) + geom_point() +
  scale_color_manual(values = c("coral", "mediumseagreen", "cornflowerblue", "orchid"),labels=c('Female Nonsmoker', 'Female Smoker', 'Male Nonsmoker', 'Male Smoker')) +
  ggtitle("Sleep Efficiency and Alcohol Consumption") + ylab("Sleep Efficiency") + xlab("Alcohol Consumption (oz)")
plot_exer <-ggplot(data, aes(Exercise.frequency, Sleep.efficiency, color = Gender:Smoking.status)) + geom_point() +
  scale_color_manual(values = c("coral", "mediumseagreen", "cornflowerblue", "orchid"),labels=c('Female Nonsmoker', 'Female Smoker', 'Male Nonsmoker', 'Male Smoker')) +
  ggtitle("Sleep Efficiency and Exercise Frequency") + ylab("Sleep Efficiency") + xlab("# of times the subject exercises each week")

#plot all scatterplots
options(repr.plot.width=20,repr.plot.height=20)
legend<-get_legend(plot_age)

plot_grid(plot_grid(   
  plot_grid(
    plot_age + theme(legend.position = 'none'), 
    plot_dur + theme(legend.position = 'none'),
    plot_rem + theme(legend.position = 'none'), 
    ncol=3
  ),
  plot_grid(
    plot_deep + theme(legend.position = 'none'), 
    plot_light + theme(legend.position = 'none'),
    plot_awake + theme(legend.position = 'none'), 
    ncol=3
  ),
  
  plot_grid(
    plot_caf + theme(legend.position = 'none'), 
    plot_alc + theme(legend.position = 'none'), 
    plot_exer + theme(legend.position = 'none'), 
    ncol=3
  ),
  
  ncol=1),
  
  plot_grid(legend), rel_widths = c(1, 0.1))

#plot scatterplot and barplots of each categorical variables
options(repr.plot.width=10,repr.plot.height=8)
#create scatterplot partitioned by groups
plot_gender <- ggplot(data, aes(x=Gender)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count", show.legend=FALSE) +
  geom_text(aes( label = scales::percent(..count../n), y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_x_discrete(labels=c("0" = "Female", "1" = "Male"))+
  ggtitle("Observation Counts of Gender Groups")

plot_smoke <- ggplot(data, aes(x=Smoking.status)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count", show.legend=FALSE) +
  geom_text(aes( label = scales::percent(..count../n), y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_x_discrete(labels=c('0' = "Nonsmoker",'1' = "Smoker"))+
  ggtitle("Observation Counts of Smoking Status Groups")

plot_genderbysmoke <- ggplot(data, aes(x=Gender,  group=Smoking.status)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count", show.legend=FALSE) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  facet_grid(~Smoking.status, labeller=as_labeller(c('0' = "Nonsmoker",'1' = "Smoker"))) +
  scale_x_discrete(labels=c("0" = "Female", "1" = "Male"))+
  ggtitle("Observation Counts of Smoking Status Groups by Gender")

plot_smokebygender <- ggplot(data, aes(x=Smoking.status,  group=Gender)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count", show.legend=FALSE) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  facet_grid(~Gender, labeller=as_labeller(c("0" = "Female", "1" = "Male"))) +
  scale_x_discrete(labels=c('0' = "Nonsmoker",'1' = "Smoker"))+
  ggtitle("Observation Counts of Gender Groups by Smoking Status")

#create boxplots to view if there are any siginificant differences between groups
boxplot_gendersmoke <- ggplot(data, aes(Gender:Smoking.status, Sleep.efficiency, color = Gender:Smoking.status)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE, show.legend=FALSE) + 
  scale_color_manual(values = c("coral", "mediumseagreen", "cornflowerblue", "orchid"),labels=c('Female Nonsmoker', 'Female Smoker', 'Male Nonsmoker', 'Male Smoker')) +
  scale_x_discrete(labels=c('Female Nonsmoker', 'Female Smoker', 'Male Nonsmoker', 'Male Smoker')) +
  ggtitle("Sleep Efficiency by Gender and Smoking Status Groups")
boxplot_gender <- ggplot(data, aes(Gender, Sleep.efficiency, color = Gender)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE, show.legend=FALSE) + 
  scale_color_manual(values = c("mediumseagreen", "cornflowerblue"),labels=c('Female','Male')) +
  scale_x_discrete(labels=c('Female','Male')) +
  ggtitle("Sleep Efficiency by Genders")
boxplot_smoke <- ggplot(data, aes(Smoking.status, Sleep.efficiency, color = Smoking.status)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=FALSE, show.legend=FALSE) + 
  scale_color_manual(values = c("coral", "orchid"),labels=c('Nonsmoker', 'Smoker')) +
  scale_x_discrete(labels=c('Nonsmoker', 'Smoker')) +
  ggtitle("Sleep Efficiency by Smoking Status")

plot_grid(boxplot_gender,plot_gender)
plot_grid(boxplot_smoke,plot_smoke)
options(repr.plot.width=15,repr.plot.height=8)
plot_grid(plot_genderbysmoke,boxplot_gendersmoke,plot_smokebygender, ncol=3)

#model selection - creating training and testing sets and rmse function
set.seed(2023)

step_data <- data |>
  select(-c(Smoking.status,Gender))
step_data$ID <- 1:n
step_data_train <- sample_n(step_data,size=0.5*n,replace=FALSE)
step_data_test <- anti_join(step_data,step_data_train,by="ID")

step_data_train <- step_data_train |> 
  select(-ID)
step_data_test <- step_data_test |> 
  select(-ID)

rmse <- function(u,v) {sqrt(mean((u-v)^2))}
head(step_data_train)

#create full model
full_model <- lm(Sleep.efficiency~.,data=step_data_train)
pre_full_model <- predict(full_model,step_data_test)

full_model_RMSE <- tibble(Model="Full Model",
                          RMSE=rmse(step_data_train$Sleep.efficiency,pre_full_model))
full_model_RMSE

#forward and backward selection
forward_select <- summary(regsubsets(x=Sleep.efficiency~.,
                                     nvmax=9,
                                     data=step_data_train,
                                     method="forward"))
forward_cp <- tibble(var=1:8,cp=forward_select$cp) |>
  arrange(cp)
forward_select
forward_cp
forward_model <- lm(Sleep.efficiency~Light.sleep.percentage+Awakenings+Age+Exercise.frequency,data=step_data_train)
pre_forward_model <- predict(forward_model,step_data_test)
forward_model_RMSE <- tibble(Model="Forward Model",
                             RMSE=rmse(step_data_train$Sleep.efficiency,pre_forward_model))
forward_model_RMSE
backward_select <- summary(regsubsets(x=Sleep.efficiency~.,
                                      nvmax=11,
                                      data=step_data_train,
                                      method="backward"))
backward_cp <- tibble(var=1:10,cp=backward_select$cp) |>
  arrange(cp)
backward_select
backward_cp
backward_model <- lm(Sleep.efficiency~Deep.sleep.percentage+Awakenings+REM.sleep.percentage+Age+Exercise.frequency,data=step_data_train)
pre_backward_model <- predict(backward_model,step_data_test)
backward_model_RMSE <- tibble(Model="Backward Model",
                              RMSE=rmse(step_data_train$Sleep.efficiency,pre_backward_model))
backward_model_RMSE

#rmse comparison of models
rbind(full_model_RMSE,forward_model_RMSE,backward_model_RMSE) |>
  arrange(RMSE)

#linear and quadratic model fitting
model1 <- lm(Sleep.efficiency~Deep.sleep.percentage+Awakenings+REM.sleep.percentage+Age+Exercise.frequency+Smoking.status + Gender + Alcohol.consumption,data=data)
model2 <- lm(Sleep.efficiency~Deep.sleep.percentage+Awakenings+REM.sleep.percentage+Age+Exercise.frequency+Smoking.status+ Alcohol.consumption,data=data)
model2b <- lm(Sleep.efficiency~Deep.sleep.percentage+Awakenings+REM.sleep.percentage+Age+Exercise.frequency+Smoking.status+ Alcohol.consumption,data=data)
#summary(model1)
summary(model2)
#summary(model2b)
#based on the above, we should not use gender, as it doesnot help our case, also caffeine consumption might not be neccessary
#model3 <- lm(Sleep.efficiency~Deep.sleep.percentage+Awakenings+REM.sleep.percentage+Age+Exercise.frequency+Smoking.status+Smoking.status*Alcohol.consumption,data=data)
#model4 <- lm(Sleep.efficiency~Deep.sleep.percentage+Awakenings+REM.sleep.percentage+Age+Exercise.frequency+Smoking.status+Smoking.status*Alcohol.consumption+Deep.sleep.percentage*Awakenings,data=data)
#summary(model3)
#summary(model4)
#model5 <- lm(Sleep.efficiency~Deep.sleep.percentage+Awakenings+REM.sleep.percentage+Age+Exercise.frequency+Smoking.status+Smoking.status*Alcohol.consumption+Deep.sleep.percentage*Awakenings+REM.sleep.percentage*Awakenings,data=data)

#summary(model5)
model6 <- lm(Sleep.efficiency~Deep.sleep.percentage+Awakenings+REM.sleep.percentage+Age+Exercise.frequency+Smoking.status+Smoking.status*Alcohol.consumption+Deep.sleep.percentage*Awakenings+REM.sleep.percentage*Awakenings+Caffeine.consumption+Caffeine.consumption*Sleep.duration+Smoking.status*Sleep.duration+Alcohol.consumption*Sleep.duration,data=data)
summary(model6)


#model6 is best model with interaction, model 2 is the best one to choose if no interaction.
model7<- lm(Sleep.efficiency~Light.sleep.percentage+Awakenings+Age+Exercise.frequency,data=data)
summary(model7)
#Forward Selection model

# model1q is the best quadratic model without interactions, model2q with interactions
model1q <- lm(Sleep.efficiency~I(Awakenings^2)+I(Deep.sleep.percentage^2)+I(Alcohol.consumption^2)+Deep.sleep.percentage+Awakenings+REM.sleep.percentage+Age+Exercise.frequency+Smoking.status+ Alcohol.consumption,data=data)
summary(model1q)
model2q <- lm(Sleep.efficiency~I(Awakenings^2)+I(Deep.sleep.percentage^2)+I(Alcohol.consumption^2)+Deep.sleep.percentage+Awakenings+REM.sleep.percentage+Age+Exercise.frequency+Smoking.status+ Alcohol.consumption+Deep.sleep.percentage*Awakenings+REM.sleep.percentage*Awakenings+Caffeine.consumption+Caffeine.consumption*Sleep.duration+Smoking.status*Sleep.duration+Alcohol.consumption*Sleep.duration,data=data)
summary(model2q)

#checking rmse of fitted values of model
rmse(model2$fitted.values, data$Sleep.efficiency)
rmse(model6$fitted.values, data$Sleep.efficiency)
rmse(model7$fitted.values, data$Sleep.efficiency)
rmse(model1q$fitted.values, data$Sleep.efficiency)
rmse(model2q$fitted.values, data$Sleep.efficiency)

#error analysis - residual plots
p1 <- data.frame(data$Deep.sleep.percentage, model2$fitted.values, model2$residuals)
head(p1)
plot1 <- ggplot(p1, aes(x=p1$model2.fitted.values, y = p1$model2.residuals)) + geom_point(aes(colour = p1$data.Deep.sleep.percentage))
plot1

p2 <- data.frame(data$Deep.sleep.percentage, model6$fitted.values, model6$residuals)
head(p1)
plot2 <- ggplot(p2, aes(x=p2$model6.fitted.values, y = p2$model6.residuals)) + geom_point(aes(colour = p2$data.Deep.sleep.percentage))
plot2

pq1 <- data.frame(data$Deep.sleep.percentage, model1q$fitted.values, model1q$residuals)
head(pq1)
plotq1 <- ggplot(pq1, aes(x=pq1$model1q.fitted.values, y = pq1$model1q.residuals)) + geom_point(aes(colour = pq1$data.Deep.sleep.percentage))
plotq1

p2q <- data.frame(data$Deep.sleep.percentage, model2q$fitted.values, model2q$residuals)
head(p1)
plot2q <- ggplot(p2q, aes(x=p2q$model2q.fitted.values, y = p2q$model2q.residuals)) + geom_point(aes(colour = p2q$data.Deep.sleep.percentage))
plot2q

p7 <- data.frame(data$Deep.sleep.percentage, model7$fitted.values, model7$residuals)
head(p7)
plot7 <- ggplot(p7, aes(x=p7$model7.fitted.values, y = p7$model7.residuals)) + geom_point(aes(colour = p7$data.Deep.sleep.percentage))
plot7
