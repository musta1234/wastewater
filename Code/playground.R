install.packages("seasonal")
library(seasonal)
library(dplyr)

c <- c("forecast", "seasonal", "purrr", "fable", "fabletools", "feasts",
       "tsibble", "tsibbledata")
for (y in c) install.packages(y, dep = TRUE)
sapply(c, library, character.only=TRUE)

# select all numeric variables from dataframe usa_dataset

usa_tsibble <- 
  usa_dataset %>% 
  select(date, subvariant, viral_load, hosp_current, hosp_new, deaths) %>%
  #convert subvariant to factor
  mutate(subvariant = factor(subvariant, 
            levels = c("BA.1", "BA.2", "BA.4 BA.5", "XBB", "JN.1"))
         ) %>% 
  as_tsibble() #%>% View()

View(usa_tsibble)

usa_ts <- 
  usa_dataset[ , c("date", "subvariant", "viral_load", "hosp_current", 
                  "hosp_new", "deaths") 
              ] %>% 
  ts(frequency = 52.18, start = c(2022, 1)) #%>% View()

View(usa_ts)

# run classical decomposition of time series data usa_ts[ , 'viral_load']
usa_decomp_ww <- decompose(usa_ts[, c('viral_load')], type='additive')
autoplot(usa_decomp_ww)

usa_decomp_hosp <- decompose(usa_ts[, c('hosp_new')], )
autoplot(usa_decomp_hosp)

# run X11 decomposition of time series data usa_ts[ , 'viral_load']

usa_decomp_x11 <- seas(usa_ts[, c('date', 'viral_load')], x11 = "", outlier = "auto")

autoplot(usa_decomp_x11)

library(fable)
library(feasts)
library(tsibble)
usa_tsibble %>%
  model(x11 = X_13ARIMA_SEATS(.data$viral_load ~ x11()))


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


