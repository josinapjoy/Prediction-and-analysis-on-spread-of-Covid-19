#PREDICTION & ANALYSIS ON SPREAD OF NOVEL COVID-19----------
  
              "CAPSTONE PROJECT REPORT June 2020
            
                          Submitted by :

                      JOSINA P JOY
                      KRISHNAMRAJU NALIMELA
                      SUBHALAXMI BABOO

                              To

The Amity University Online in partial fulfillment of the requirements
            for the award of the Post Graduate Diploma

                              In

              BUSINESS ANALYTICS & INTELLIGENCE"

#Required Packages & Libraries----------
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggthemes")
install.packages("prophet")

library(tidyverse)
library(lubridate)
library(ggthemes)
library(prophet)
library(dygraphs)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(xts)

#Getting data from JHU & Data Preparation---------

confirmed <-
  read.csv(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
    stringsAsFactors = F
  )
deaths <-
  read.csv(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
    stringsAsFactors = F
  )
recovered <-
  read.csv(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
    stringsAsFactors = F
  )

#Get evolution data by country
data_confirmed_sub <- confirmed %>%
  pivot_longer(names_to = "date", cols = 5:ncol(confirmed)) %>%
  select(-Province.State, -Lat,-Long) %>%
  group_by(`Country.Region`, date) %>%
  summarise("confirmed" = sum(value, na.rm = T))

data_deaths_sub <- deaths %>%
  pivot_longer(names_to = "date", cols = 5:ncol(deaths)) %>%
  select(-Province.State, -Lat,-Long) %>%
  group_by(`Country.Region`, date) %>%
  summarise("deaths" = sum(value, na.rm = T))

data_recovered_sub <- recovered %>%
  pivot_longer(names_to = "date", cols = 5:ncol(recovered)) %>%
  select(-Province.State, -Lat,-Long) %>%
  group_by(`Country.Region`, date) %>%
  summarise("recovered" = sum(value, na.rm = T))

COVID19GlobalData <- data_confirmed_sub %>%
  full_join(data_recovered_sub) %>%
  full_join(data_deaths_sub) %>%
  ungroup() %>%
  mutate(date = as.POSIXct(date, format = "X%m.%d.%y")) %>%
  arrange(date) %>%
  group_by(`Country.Region`, date) %>%
  replace_na(list(deaths = 0, confirmed = 0)) %>%
  rename(Country = Country.Region) %>%
  ungroup()

head(COVID19GlobalData)

write.csv(COVID19GlobalData,"COVID19GlobalData.csv")


#EDA----------------------------------------
#Number of observations (rows) and variables, and a head of the first cases.
glimpse(COVID19GlobalData)

#Today's Report by Country::::
TodayData <-
  COVID19GlobalData  %>% filter(date %in% max(COVID19GlobalData$date)) %>%
  arrange(desc(confirmed))
head(TodayData)

#Top 10's by Countries

Top10_Confirmed <-
  TodayData %>% select(Country, date, confirmed) %>%
  arrange(desc(confirmed)) %>% head(10) %>%
  ggplot(aes(x = reorder(`Country`, confirmed), y = confirmed)) +
  geom_bar(stat = "identity", fill  = "Orange", width = 0.8) +
  theme_economist() +
  scale_y_continuous(breaks = seq(0, 2000000, by = 200000)) +
  coord_flip() +
  labs(x = "", y = "", title = "Top 10 Countries by Confirmed Cases") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(
    axis.title = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size = 11, face = "bold")
  )

Top10_Deaths <- TodayData %>% select(Country, date, deaths) %>%
  arrange(desc(deaths)) %>% head(10) %>%
  ggplot(aes(x = reorder(`Country`, deaths), y = deaths)) +
  geom_bar(stat = "identity", fill  = "red", width = 0.8) +
  theme_economist() +
  scale_y_continuous(breaks = seq(0, 220000, by = 20000)) +
  coord_flip() +
  labs(x = "", y = "", title = "Top 10 Countries by Death Cases") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(
    axis.title = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size = 11, face = "bold")
  )

Top10_Recovered <-
  TodayData %>% select(Country, date, recovered) %>%
  arrange(desc(recovered)) %>% head(10) %>%
  ggplot(aes(x = reorder(`Country`, recovered), y = recovered)) +
  geom_bar(stat = "identity", fill  = "green", width = 0.8) +
  theme_economist() +
  scale_y_continuous(breaks = seq(0, 2200000, by = 50000)) +
  coord_flip() +
  labs(x = "", y = "", title = "Top 10 Countries by Recovered Cases") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(
    axis.title = element_text(size = 14, colour = "black"),
    axis.text.y = element_text(size = 11, face = "bold")
  )


#--------------TS__Prophet_Modeling---------------
head(COVID19GlobalData)

COVID19GlobalData$date <- ymd(COVID19GlobalData$date)
class(COVID19GlobalData$date)
str(COVID19GlobalData)

#Global Confirmed Cases Forecasting----------------
coviddata.world <- COVID19GlobalData %>% group_by(date) %>%
  summarise(
    Country = 'World',
    confirmed = sum(confirmed, na.rm = 1),
    deaths = sum(deaths, na.rm = 1),
    recovered = sum(recovered, na.rm = 1)
  )

qplot(date, confirmed, data = coviddata.world, main = 'COVID-19 Global Confirmed Cases') +
  scale_y_continuous(breaks = seq(0, 10000000, by = 900000))

# Confirmed cases Dataframe for modeling
ds <- coviddata.world$date
y <- coviddata.world$confirmed
df <- data.frame(ds, y)

# Prophet modeling
m <- prophet(df)
future <- make_future_dataframe(m, periods = 40)
forecast <- predict(m, future)

plot(m, forecast) +
  scale_y_continuous(breaks = seq(0, 17000000, by = 900000))
dyplot.prophet(m, forecast, main = "ACTUAL & PREDICTED - GLOBAL CONFIRMED CASES") %>%
  dyOptions(maxNumberWidth = 20)

prophet_plot_components(m, forecast)

#Actual vs Predicted plot
predicted <- forecast$yhat[1:158]
actual <- m$history$y

plot(actual, predicted) +
  abline(lm(predicted ~ actual), col = 'red')

summary(lm(predicted ~ actual))
Accuracymodel1 <- accuracy((lm(predicted ~ actual)))
Accuracymodel1
str(forecast)


#Global Recovered Cases Forecasting----------------

Coviddata.world
ds <- coviddata.world$date
y <- coviddata.world$recovered
df <- data.frame(ds, y)
m <- prophet(df)
future <- make_future_dataframe(m, periods = 40)
forecast <- predict(m, future)
plot(m, forecast)
dyplot.prophet(m, forecast, main = "ACTUAL & PREDICTED GLOBAL RECOVERED CASES ") %>%
  dyOptions(maxNumberWidth = 20)

#Global Deaths Cases Forecasting----------------

ds <- coviddata.world$date
y <- coviddata.world$deaths
df <- data.frame(ds, y)
m <- prophet(df)
future <- make_future_dataframe(m, periods = 40)
forecast <- predict(m, future)
plot(m, forecast)
dyplot.prophet(m, forecast, main = "ACTUAL & PREDICTED GLOBAL DECEASED CASES") %>%
  dyOptions(maxNumberWidth = 20)

#India Confirmed Cases Prediction--------------------------------------------------------

coviddata.India <- COVID19GlobalData %>% filter(Country == "India")
qplot(date, confirmed, data = coviddata.India, main = 'COVID-19 CONFIRMED CASES IN INDIA') +
  scale_y_continuous(breaks = seq(0, 900000, by = 50000))

# New Dataframe
ds <- coviddata.India$date
y <- coviddata.India$confirmed
df <- data.frame(ds, y)
df
#Using Prophet
m <- prophet(df)
future <- make_future_dataframe(m, periods = 40)
forecast <- predict(m, future)
# Plot
plot(m, forecast)
dyplot.prophet(m, forecast, main = "ACTUAL & PREDICTED CONFIRMED CASES IN INDIA") %>%
  dyOptions(maxNumberWidth = 20)
prophet_plot_components(m, forecast)

predicted <- forecast$yhat[1:158]
actual <- m$history$y
plot(actual, predicted)
abline(lm(predicted ~ actual), col = "red")
AccuracyIndia <- accuracy((lm(predicted ~ actual)))
AccuracyIndia
summary(lm(predicted ~ actual))
str(forecast)

#India Recovered Cases Prediction-------------------------------------------------------

ds <- coviddata.India$date
y <- coviddata.India$recovered
df <- data.frame(ds, y)
m <- prophet(df)
future <- make_future_dataframe(m, periods = 40)
forecast <- predict(m, future)
plot(m, forecast)
dyplot.prophet(m, forecast, main = "ACTUAL & PREDICTED RECOVERED CASES IN INDIA") %>%
  dyOptions(maxNumberWidth = 20)

#India deaths Cases Prediction-------------------------------------------------------

ds <- coviddata.India$date
y <- coviddata.India$deaths
df <- data.frame(ds, y)
m <- prophet(df)
future <- make_future_dataframe(m, periods = 40)
forecast <- predict(m, future)
plot(m, forecast)
dyplot.prophet(m, forecast, main = "ACTUAL & PREDICTED DECEASED CASES IN INDIA") %>%
  dyOptions(maxNumberWidth = 20)


#US Confirmed Cases--------------------------------------------------------


coviddata.US <- COVID19GlobalData %>% filter(Country == "US")
qplot(date, confirmed, data = coviddata.US, main = 'COVID-19 CONFIRMED CASES IN THE US')

# New Dataframe
ds <- coviddata.US$date
y <- coviddata.US$confirmed
df <- data.frame(ds, y)
df
#Using Prophet
m <- prophet(df)
future <- make_future_dataframe(m, periods = 40)
forecast <- predict(m, future)
# Plot
plot(m, forecast)
dyplot.prophet(m, forecast, main = "ACTUAL & PREDICTED CONFIRMED CASES IN THE US") %>%
  dyOptions(maxNumberWidth = 20)

prophet_plot_components(m, forecast)

predicted <- forecast$yhat[1:158]
actual <- m$history$y
plot(actual, predicted)
abline(lm(predicted ~ actual), col = 'red')
AccuracyUS <- accuracy((lm(predicted ~ actual)))
AccuracyUS
summary(lm(predicted ~ actual))
str(forecast)

#US Recovered Cases-------------------------------------------------------

ds <- coviddata.US$date
y <- coviddata.US$recovered
df <- data.frame(ds, y)
df
m <- prophet(df)
future <- make_future_dataframe(m, periods = 40)
forecast <- predict(m, future)
# Plot
plot(m, forecast)
dyplot.prophet(m, forecast, main = "ACTUAL & PREDICTED RECOVERED CASES IN THE US") %>%
  dyOptions(maxNumberWidth = 20)

#US Death Cases-------------------------------------------------------

ds <- coviddata.US$date
y <- coviddata.US$deaths
df <- data.frame(ds, y)
df
m <- prophet(df)
future <- make_future_dataframe(m, periods = 40)
forecast <- predict(m, future)
plot(m, forecast)
dyplot.prophet(m, forecast, main = "ACTUAL & PREDICTED DECEASED CASES IN THE US") %>%
  dyOptions(maxNumberWidth = 20)


#--------------ARIMA Model---------------------------

coviddata.world.timeseries <-
  xts(coviddata.world$confirmed, order.by = coviddata.world$date)
acf(coviddata.world.timeseries)
pacf(coviddata.world.timeseries)

fit_arima <- arima(coviddata.world.timeseries, order = c(0, 2, 0))
# Next 40 days forecasted values
forecast(fit_arima, 40)
# plotting the graph with Next 40 Days forecasted values
plot(
  forecast(fit_arima, 40),
  xlab = "No of Days",
  ylab = "Total Positive Cases",
  main = "COVID -19 Forecasted Confirmed Global Cases using ARIMA",
  col.main = "darkgreen"
)

summary(fit_arima)
accuracy(fit_arima)

 
#--------------AUTO ARIMA Model--------------------
#creating TS object for ARIMA Model 

coviddata.world.timeseries <-
  xts(coviddata.world$confirmed, order.by = coviddata.world$date)
fit_auto.arima <- auto.arima(coviddata.world.timeseries)
# Next 40 days forecasted values
forecast(fit_auto.arima, 40)
# plotting the graph with Next 40 Days forecasted values
plot(
  forecast(fit_auto.arima, 40),
  xlab = "No of Days",
  ylab = "Total Positive Cases",
  main = "COVID -19 Forecasted Confirmed Global Cases using AUTO ARIMA",
  col.main = "darkgreen"
)

summary(fit_auto.arima)
accuracy(fit_auto.arima)


#--------------COMPARISON OF TIME SERIES MODELS-----------------------
#GLOBAL CONFIRMED CASES
#MODEL 1 : PROPHET---------------------------------------

prophet_model_accuracy <- accuracy((lm(predicted ~ actual)))
prophet_model_accuracy

#MODEL 2 : EXPONENTIAL SMOOTHING--------------------------
#CREATING TS OBJECT

coviddata.world.timeseries <-
  xts(coviddata.world$confirmed, order.by = coviddata.world$date)

ets(coviddata.world.timeseries)
ets_model_accuracy <- accuracy(ets(coviddata.world.timeseries))
ets_model_accuracy

#MODEL 3 : AUTO ARIMA MODEL-------------------------------

auto.arima(coviddata.world.timeseries)
auto_arima_accuracy <-
  accuracy(auto.arima(coviddata.world.timeseries))
auto_arima_accuracy

#MODEL 4 : ARIMA----------------------------------------

arima(coviddata.world.timeseries, order = c(0, 2, 0))
arima_model_accuracy <-
  accuracy(arima(coviddata.world.timeseries, order = c(0, 2, 0)))

#Comparing MAPE,ACF,RMSE,MASE VALUES OF EACH MODELS---------------------------

prophet_model_accuracy
ets_model_accuracy
auto_arima_accuracy
arima_model_accuracy
