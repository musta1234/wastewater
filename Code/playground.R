install.packages("seasonal")
library(seasonal)
# select all numeric variables from dataframe usa_dataset
usa_ts <- 
  cbind(date = usa_dataset[,1],
        usa_dataset[, sapply(usa_dataset, is.numeric)]
        ) %>% ts(frequency = 52, start = c(2022, 1)) #%>% View()

# run classical decomposition of time series data usa_ts[ , 'viral_load']
usa_decomp_ww <- decompose(usa_ts[, c('viral_load')], type='additive')
autoplot(usa_decomp_ww)

usa_decomp_hosp <- seas(usa_ts[, c('hosp_new')], )
autoplot(usa_decomp_hosp)

# run X11 decomposition of time series data usa_ts[ , 'viral_load']

usa_decomp_x11 <- seas(usa_ts[, c('viral_load')], x11 = "", outlier = "auto")

autoplot(usa_decomp_x11)



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


