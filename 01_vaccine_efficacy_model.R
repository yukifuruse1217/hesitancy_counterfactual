library(ggplot2)
library(MASS)
# install.packages('maxLik')
library(maxLik)
#install.packages('LaplacesDemon')
library(LaplacesDemon)
library(dplyr)
library(magrittr)
library(lubridate)

### vaccine waning ###

VE_waning_csv <- read.table(file = 'VE_waning.csv', sep = ',', header = TRUE)
VE_waning_csv$temp <- VE_waning_csv$days - 14

# Perform logistic regression analysis for Inf_Young_Alpha

VE_waning_csv$inf_young_alpha <- VE_waning_csv$inf_young_alpha / 100
model <- glm(data=VE_waning_csv, inf_young_alpha ~ temp, family = binomial(link = "logit"))

date_model <- c(0:730)
newdata <- data.frame(temp=date_model)

inf_young_alpha_temp <- predict(model, newdata = newdata)

newdata$inf_young_alpha_model <- invlogit(inf_young_alpha_temp)

ggplot() +
  geom_point(data=VE_waning_csv, aes(x=temp, y=inf_young_alpha)) +
  geom_point(data=newdata, aes(x=temp, y=inf_young_alpha_model), color = "red") +
  xlim(0,270) +
  ylim(0,1)



# Perform logistic regression analysis for Inf_Young_Delta

VE_waning_csv$inf_young_delta <- VE_waning_csv$inf_young_delta / 100
model <- glm(data=VE_waning_csv, inf_young_delta ~ temp, family = binomial(link = "logit"))

#date_model <- c(0:365)
#newdata <- data.frame(temp=date_model)

inf_young_delta_temp <- predict(model, newdata = newdata)

newdata$inf_young_delta_model <- invlogit(inf_young_delta_temp)

ggplot() +
  geom_point(data=VE_waning_csv, aes(x=temp, y=inf_young_delta)) +
  geom_point(data=newdata, aes(x=temp, y=inf_young_delta_model), color = "red") +
  xlim(0,270) +
  ylim(0,1)




# Perform logistic regression analysis for Inf_Old_Alpha

VE_waning_csv$inf_old_alpha <- VE_waning_csv$inf_old_alpha / 100
model <- glm(data=VE_waning_csv, inf_old_alpha ~ temp, family = binomial(link = "logit"))

#date_model <- c(0:365)
#newdata <- data.frame(temp=date_model)

inf_old_alpha_temp <- predict(model, newdata = newdata)

newdata$inf_old_alpha_model <- invlogit(inf_old_alpha_temp)

ggplot() +
  geom_point(data=VE_waning_csv, aes(x=temp, y=inf_old_alpha)) +
  geom_point(data=newdata, aes(x=temp, y=inf_old_alpha_model), color = "red") +
  xlim(0,270) +
  ylim(0,1)




# Perform logistic regression analysis for Inf_Old_Delta

VE_waning_csv$inf_old_delta <- VE_waning_csv$inf_old_delta / 100
model <- glm(data=VE_waning_csv, inf_old_delta ~ temp, family = binomial(link = "logit"))

#date_model <- c(0:365)
#newdata <- data.frame(temp=date_model)

inf_old_delta_temp <- predict(model, newdata = newdata)

newdata$inf_old_delta_model <- invlogit(inf_old_delta_temp)

ggplot() +
  geom_point(data=VE_waning_csv, aes(x=temp, y=inf_old_delta)) +
  geom_point(data=newdata, aes(x=temp, y=inf_old_delta_model), color = "red") +
  xlim(0,270) +
  ylim(0,1)











# Perform logistic regression analysis for Hosp_Young_Alpha

VE_waning_csv$hosp_young_alpha <- VE_waning_csv$hosp_young_alpha / 100
model <- glm(data=VE_waning_csv, hosp_young_alpha ~ temp, family = binomial(link = "logit"))

# date_model <- c(0:365)
# newdata <- data.frame(temp=date_model)

hosp_young_alpha_temp <- predict(model, newdata = newdata)

newdata$hosp_young_alpha_model <- invlogit(hosp_young_alpha_temp)

ggplot() +
  geom_point(data=VE_waning_csv, aes(x=temp, y=hosp_young_alpha)) +
  geom_point(data=newdata, aes(x=temp, y=hosp_young_alpha_model), color = "red") +
  xlim(0,270) +
  ylim(0,1)


newdata$hosp_young_alpha_model2 <- newdata$hosp_young_alpha_model



# Perform logistic regression analysis for Hosp_Young_Delta

VE_waning_csv$hosp_young_delta <- VE_waning_csv$hosp_young_delta / 100
model <- glm(data=VE_waning_csv, hosp_young_delta ~ temp, family = binomial(link = "logit"))

#date_model <- c(0:365)
#newdata <- data.frame(temp=date_model)

hosp_young_delta_temp <- predict(model, newdata = newdata)

newdata$hosp_young_delta_model <- invlogit(hosp_young_delta_temp)

ggplot() +
  geom_point(data=VE_waning_csv, aes(x=temp, y=hosp_young_delta)) +
  geom_point(data=newdata, aes(x=temp, y=hosp_young_delta_model), color = "red") +
  xlim(0,270) +
  ylim(0,1)


newdata$hosp_young_delta_model2 <- newdata$hosp_young_delta_model


# Perform logistic regression analysis for Hosp_Old_Alpha

VE_waning_csv$hosp_old_alpha <- VE_waning_csv$hosp_old_alpha / 100
model <- glm(data=VE_waning_csv, hosp_old_alpha ~ temp, family = binomial(link = "logit"))

#date_model <- c(0:365)
#newdata <- data.frame(temp=date_model)

hosp_old_alpha_temp <- predict(model, newdata = newdata)

newdata$hosp_old_alpha_model <- invlogit(hosp_old_alpha_temp)

ggplot() +
  geom_point(data=VE_waning_csv, aes(x=temp, y=hosp_old_alpha)) +
  geom_point(data=newdata, aes(x=temp, y=hosp_old_alpha_model), color = "red") +
  xlim(0,270) +
  ylim(0,1)


newdata$hosp_old_alpha_model2 <- newdata$hosp_old_alpha_model


# Perform logistic regression analysis for Hosp_Old_Delta

VE_waning_csv$hosp_old_delta <- VE_waning_csv$hosp_old_delta / 100
model <- glm(data=VE_waning_csv, hosp_old_delta ~ temp, family = binomial(link = "logit"))

#date_model <- c(0:365)
#newdata <- data.frame(temp=date_model)

hosp_old_delta_temp <- predict(model, newdata = newdata)

newdata$hosp_old_delta_model <- invlogit(hosp_old_delta_temp)

ggplot() +
  geom_point(data=VE_waning_csv, aes(x=temp, y=hosp_old_delta)) +
  geom_point(data=newdata, aes(x=temp, y=hosp_old_delta_model), color = "red") +
  xlim(0,270) +
  ylim(0,1)


newdata$hosp_old_delta_model2 <- newdata$hosp_old_delta_model

























# Perform logistic regression analysis for trans_alpha
VE_waning_csv$trans_alpha <- VE_waning_csv$trans_alpha / 100
model <- glm(data=VE_waning_csv, trans_alpha ~ temp, family = binomial(link = "logit"))

#date_model <- c(0:365)
#newdata <- data.frame(temp=date_model)

trans_alpha_temp <- predict(model, newdata = newdata)

newdata$trans_alpha_model <- invlogit(trans_alpha_temp)

ggplot() +
  geom_point(data=VE_waning_csv, aes(x=temp, y=trans_alpha)) +
  geom_point(data=newdata, aes(x=temp, y=trans_alpha_model), color = "red") +
  xlim(0,270) +
  ylim(0,1)





# Perform logistic regression analysis for trans_delta
VE_waning_csv$trans_delta <- VE_waning_csv$trans_delta / 100
model <- glm(data=VE_waning_csv, trans_delta ~ temp, family = binomial(link = "logit"))

#date_model <- c(0:365)
#newdata <- data.frame(temp=date_model)

trans_delta_temp <- predict(model, newdata = newdata)

newdata$trans_delta_model <- invlogit(trans_delta_temp)

ggplot() +
  geom_point(data=VE_waning_csv, aes(x=temp, y=trans_delta)) +
  geom_point(data=newdata, aes(x=temp, y=trans_delta_model), color = "red") +
  xlim(0,270) +
  ylim(0,1)










newdata$week <- newdata$temp / 7

week <- c(0:100)
ve_waning_model <- data.frame(week)
ve_waning_model <- left_join(ve_waning_model, newdata, by=c("week"="week")) ### Temp days is after 14 days!










