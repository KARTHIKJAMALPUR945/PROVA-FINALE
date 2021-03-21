library(readr)
library(bsts)

df <- read.csv("F:/PROVA FINALE/FCD_aggregated_10k.csv")
gilt <-ts(df$speed, start = c(2015), frequency=525960)

plot(gilt, main='Raw data',
     xlab ='Date',
     ylab ='Speed')
par(mar=c(1,1,1,1))
gilt[is.na(gilt)] <- 0
ll_ss <- list()
ll_ss <- AddLocalLevel(state.specification = ll_ss, y = gilt)
ll_fit <- bsts(gilt, state.specification = ll_ss, niter = 1e3)



plot(ll_fit, main='Expectation of posterior', xlab='Date',ylab ='speed')

ll_fit

ll_pred <- predict(ll_fit, horizon = 300)
plot(ll_pred, plot.original = 9999, main = 'local linear forecasts', xlab='Date idx[]', ylab='speed[]')
library(MLmetrics)


llt_ss <- list()
llt_ss <- AddLocalLinearTrend(state.specification = llt_ss, y = gilt)
llt_fit <- bsts(gilt, state.specification = llt_ss, niter = 1e3)

llt_pred <- predict(llt_fit, horizon = 300)
plot(llt_pred, plot.original=900,
     main='Local linear trend forecasts',
     xlab='Date idx[]',
     ylab='speed[]')



lts_ss <- list()
lts_ss <- AddLocalLinearTrend(lts_ss, y=gilt)
lts_ss <- AddSeasonal(lts_ss, gilt, nseasons = 300)
lts_fit <- bsts(gilt, state.specification = lts_ss, niter = 1e3)

plot(lts_fit, 'components',
     xlab = 'Date',
     ylab = 'speed')

lts_pred<-predict(lts_fit, horizon = 300)
plot(lts_pred, plot.original=90, 
     mainxlab = 'Date', ylab= 'Speed')


CompareBstsModels(lwd = 4, list(level= ll_fit, trend = llt_fit, seasonal=lts_fit), colors = c("forestgreen","firebrick", "blue4"), xlab = 'Date')







