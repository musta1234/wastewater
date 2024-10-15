<<<<<<< HEAD
#Model 7: ARIMA(1,1,1) with drift
usa_model7 <- arima(usa_ts, order=c(1,1,1), include.drift=TRUE)
=======
install.packages("seasonal")
library(seasonal)
library(dplyr)
x <- c("urca", "fracdiff")
for (y in x) install.packages(y, dep = TRUE)
sapply(x, library, character.only=TRUE)

c <- c("forecast", "seasonal", "purrr", "fable", "fabletools", "feasts",
       "tsibble", "tsibbledata")
for (y in c) install.packages(y, dep = TRUE)
sapply(c, library, character.only=TRUE)
>>>>>>> 8e070e8707607ae722305055c538a82ccc14d9d7

# select all numeric variables from dataframe usa_dataset
usa_ts <- 
  usa_dataset[ , c("date", "subvariant", "viral_load", "hosp_current", 
                   "hosp_new", "deaths") 
  ] %>% 
  ts(frequency = 52.18, start = c(2022, 1)) #%>% View()

View(usa_ts)

usa_tsibble_key <- 
  usa_dataset %>% 
  select(date, subvariant, viral_load, hosp_current, hosp_new, deaths) %>%
  #convert subvariant to factor
  mutate(subvariant = factor(subvariant, 
            levels = c("BA.1", "BA.2", "BA.4 BA.5", "XBB", "JN.1"))
         ) %>% 
  as_tsibble(key = subvariant, index = date) #%>% View() 

View(usa_tsibble_key)

usa_tsibble <- 
  usa_dataset %>% 
  select(date, subvariant, viral_load, hosp_current, hosp_new, deaths) %>%
  #convert subvariant to factor
  mutate(subvariant = factor(subvariant, 
                             levels = c("BA.1", "BA.2", "BA.4 BA.5", "XBB", "JN.1"))
  ) %>% 
  as_tsibble(index = date) #%>% View() 

View(usa_tsibble)

######## STL decomposition and naive ARIMA ############

usa_ts[, 'viral_load'] %>%
  stl(t.window=52, s.window="periodic", robust=TRUE) %>%
  #seasadj() %>% 
  forecast(method= "naive") %>%
  autoplot()

usa_ts[, 'viral_load'] %>%
  stl(t.window=52, s.window="periodic", robust=TRUE) %>%
  #seasadj() %>% 
  forecast(method = "arima") %>%
  autoplot()

############################# Check features ##############
usa_tsibble %>%
  features(viral_load, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = linearity, color = subvariant)
             ) +
  geom_point() 

usa_tsibble %>%
  features(viral_load, coef_hurst) 

usa_tsibble %>%
  features(viral_load, feat_spectral ) 

usa_tsibble %>%
  features(viral_load, box_pierce ) 

usa_tsibble %>%
  features(viral_load, ljung_box ) 

usa_tsibble_key %>%
  features(viral_load, feature_set(pkgs = "feasts") ) 

# run classical decomposition of time series data usa_ts[ , 'viral_load']
usa_decomp_ww <- decompose(usa_ts[, c('viral_load')], type='additive')
autoplot(usa_decomp_ww)

usa_decomp_hosp <- decompose(usa_ts[, c('hosp_new')], )
autoplot(usa_decomp_hosp)

# run X11 decomposition of time series data usa_ts[ , 'viral_load']
#this dos not work. Requies monthly or quaterly data
usa_decomp_x11 <- seas(usa_ts[, c('date', 'viral_load')], x11 = "", outlier = "auto")
autoplot(usa_decomp_x11)
usa_tsibble %>%
  model(x11 = X_13ARIMA_SEATS(.data$viral_load ~ x11()))

#####################################################
fit1 <- 
  usa_tsibble %>% 
  model(trend_model = TSLM(hosp_new ~ trend()))

fit1_key <-
  usa_tsibble_key %>% 
  model(trend_model = TSLM(hosp_new ~ trend()))

fit1
fit1_key

fit1 |> forecast(h = 12)

fit1_key |> forecast(h = 12) |> autoplot()

fc <- holt(usa_ts[, c('hosp_new')], h=1, phi=0.9,
           damped = TRUE)

round(accuracy(fc),2)

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") 

tsCV(usa_ts[, c('hosp_new')], holt, h=1,
     damped = TRUE) %>% 
  {.^2} %>%        #square the errors
  mean(na.rm=TRUE)


ets(usa_ts[, c('hosp_new')]) %>%
  summary()


diff(log(usa_ts[, c('hosp_new')]), difference = 1) %>%
  plot()
  Box.test(., lag=10, type="Ljung-Box")

############################## ARIMA ############################
# Run AR(1) model on usa_ts[ , 'hosp_new']
  usa_ar1 <- 
    arima(usa_ts[, c('hosp_new')], order=c(1,0,0))
  autoplot(forecast(usa_ar1, h=12))
  
  usa_ar2 <- 
    arima(usa_ts[, c('hosp_new')], order=c(2,0,0))
  autoplot(forecast(usa_ar2, h=12))
  
  usa_ar12 <- 
    arima(usa_ts[, c('hosp_new')], order=c(12,0,0))
  autoplot(forecast(usa_ar12, h=12))
  
  autoplot(residuals(usa_ar1)) +
    autoplot(residuals(usa_ar2)) +
    autoplot(residuals(usa_ar12))
  
  usa_ar0_1 <- 
    arima(usa_ts[, c('hosp_new')], order=c(0,1,0))
  autoplot(forecast(usa_ar0_1, h=12))
  
  usa_ar0_2 <- 
    arima(usa_ts[, c('hosp_new')], order=c(0,2,0))
  autoplot(forecast(usa_ar0_2, h=12))
  
  usa_ar1_1 <-
    arima(usa_ts[, c('hosp_new')], order=c(1,1,0))
  autoplot(forecast(usa_ar1_1, h=12))
  
 
usa_ar_auto <- 
  auto.arima(usa_ts[, c('hosp_new')])

autoplot(forecast(usa_ar_auto, h=12))
  #ARIMA(3,1,2)(0,1,0)[52] 

##################################################

usa_arx_vl <- auto.arima(
  usa_ts[1:100, c('hosp_new')],        # Hospitalization time series
  xreg = usa_ts[1:100, c('viral_load')],  # Wastewater SARS-CoV-2 levels
  seasonal = TRUE                  # If seasonal effects are expected
)

# Summary of the model
summary(usa_arx_vl)

# Forecasting the next 12 weeks
autoplot(forecast(usa_arx_vl, h = 13,
                  xreg = usa_ts[101:113, c('viral_load')]))


#################################
#Model 7: ARIMA(1,1,1) with drift
usa_model7 <- arima(usa_ts[, c('date', 'hosp_new')], order=c(1,1,1), include.drift=TRUE)
summary(usa_model7)




View(usa_model6)
#write code to test usa_model6$residuals for autocorrelation
usaresiduals <- as.vector(usa_model6$residuals)
acf(ts(usaresiduals), lag.max=20, main="ACF of residuals")
#write code to test usa_model6$residuals for normality
qqnorm(usaresiduals)
qqline(usaresiduals)
shapiro.test(usaresiduals)
#write code to test usa_model6$residuals for homoscedasticity
plot(usaresiduals ~ usa_model6$fitted.values, main="Residuals vs Fitted Values", xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")
#write code to test usa_model6$residuals for independence
plot(usaresiduals, main="Residuals vs Index", xlab="Index", ylab="Residuals")
abline(h=0, col="red")


