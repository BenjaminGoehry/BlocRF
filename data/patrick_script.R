rm(list=objects())
#library(ranger)
#library(gamwave)
#library(mgcv)
#library(yarrr)
#library(R39Toolbox)
library(rangerts)
library(ggplot2)
library(ggthemr)
library(latex2exp)
library(RColorBrewer)
library(yarrr)
library(ggplot2)
library(ggthemr)
library(tidyr)
library(reshape2)
library(scales)
rmse<-function(y, ychap){
  return(round(sqrt(mean((y-ychap)^2,na.rm=TRUE)),digits=5))
}
mae<-function(y,ychap){
  eps <- y - ychap
  return(round(mean(abs(eps),na.rm=TRUE),digits=10))
}
mape<-function(y,ychap){
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=10))
}



Data <-  readRDS(file="~/Documents/Time\ series\ data/Building/UnivLab_Patrick.RDS")

#Data$LoadNoise.24.min <- Data$Load.24 + rnorm(nrow(Data), 0, min(Data$Load)) #diffinv(rnorm(nrow(Data)-1, 0, sd=sd(Data$Load)))
#Data$LoadNoise.24.sd <- Data$Load.24 + rnorm(nrow(Data), 0, sd(Data$Load))
#Data$Noise.1 <- as.character( (as.numeric(Data$Hour) + rpois(nrow(Data), 6))%%24)

a <- 50*24
b <- 57*24
plot(Data$timestamp[a:b], Data$Load[a:b], type='l', ylab='Load', xlab='')

#######descriptive analytics
#weekly_profile
weekly_profile <- tapply(Data$Load, Data$DayType:Data$InstantF,mean)
plot(weekly_profile, type='l', xlab='Hour', ylab='Load Profile', axes=F)
axis(1, at=seq(13, 7*24+12, by=24), label=substr(names(weekly_profile), 1,3)%>%unique)
axis(2)
abline(v=seq(1,7*24, by=24), lty='dashed', col='grey')
box()
dev.off()

mycols_fill <- c(rgb(132,186,91, maxColorValue = 255),rgb(114,147,203, maxColorValue = 255))#, )
mycols_line <- c( rgb(62,150,81, maxColorValue = 255),rgb(57,106,177, maxColorValue = 255))#, )

data_weekly_profile <- data.frame(Load = weekly_profile, date_w = names(weekly_profile), 1,3) 
data_weekly_profile$date_w <- as.character(data_weekly_profile$date_w) #with(data_weekly_profile, reorder(date_w, date_w, function(x) -length(x)))
data_weekly_profile$date_w <- factor(data_weekly_profile$date_w, levels = unique(data_weekly_profile$date_w))

ggplot(data_weekly_profile, aes(x=date_w, y=Load, group=1)) + geom_line(size=3) + 
  xlab("Hour") + ylab("Load Profile") +
  #scale_colour_manual(values = mycols_line) +
  scale_x_discrete(breaks=names(weekly_profile)[seq(1, length(weekly_profile), by=24)], label=substr(names(weekly_profile), 1,3)%>%unique)+
  scale_y_continuous(breaks=pretty_breaks())+
  #geom_hline(yintercept=mean(RF_a), color="black", size=0.5)+
  #labs(colour = "Variant")+
  theme( axis.text=element_text(size=20),axis.title=element_text(size=40,face="bold"),
        legend.title = element_text(size=20), legend.text = element_text(size=20))


#days
col <- piratepal(palette="basel", length.out = 7) 
col.tr <- adjustcolor(col,alpha = .2)

l <- levels(Data$DayType)

for(j in c(1,2,4,7))
{
  sel <- which(Data$DayType==l[j])
  y <- Data[sel,]$Load
  plot(y[1:24], col=col.tr[j], type='l', ylim=range(Data$Load), main=l[j], xlab='Hour', ylab='Load')
  for(i in c(1:(length(y)/24)))
  {
    lines(y[((i-1)*24+1):(i*24)], col=col.tr[j])
  }
  lines(weekly_profile[which(substr(names(weekly_profile),1,3)==l[j])], col=col[j], lwd=2)
}


plot(Data$Temperature, Data$Load, pch=4, col= adjustcolor(col[1],alpha = .5), xlab='Temperature', ylab='Load')
plot(Data$InstantWeek, Data$Load, pch=20, cex=0.5)

sel <- which( Data$Hour=="15")
plot(Data$Temperature[sel], Data$Load[sel], pch=4, col='grey')

plot(pmin(0,Data$Temperature-15), Data$Load, pch=4, col='grey')
plot(Data$InstantWeek^2, Data$Load, pch=4, col='grey')
plot(pmin(0,Data$Temperature-15)*Data$Time, Data$Load, pch=4, col='grey')
#3 derniers plot ne donne rien de significatif contrairement à la conso fr


####################breaks

ScheduleF <- Data$Schedule%>%as.factor
summary(ScheduleF)/24

par(mfrow=c(1,1))
plot(Data$timestamp, Data$Load, type='l', xlab='Date', ylab='Load')
sel <- which(Data$Schedule=="Break")
points(Data$timestamp[sel], Data$Load[sel], col='red', pch=4)
sel <- which(Data$Schedule=="Holiday")
points(Data$timestamp[sel], Data$Load[sel], col='blue', pch=4)
legend('topleft', c('Break', 'Holiday'), col=c('red', 'blue'), pch=4, bty='n')


plot(Data$timestamp, Data$Load, type='l')
sel <- which(Data$Schedule=="Summer")
points(Data$timestamp[sel], Data$Load[sel], col='red', pch=4)

sel <- which(Data$Schedule=="Regular")
points(Data$timestamp[sel], Data$Load[sel], col='blue', pch=4)


####################let"s try a model  #61*24 pour yannig
learningSet <- 1:(nrow(Data)-61*24)
Data0 <- Data[learningSet, ] 
DataBis <- Data[-learningSet, ]
Data1 <- DataBis[1:(nrow(DataBis)/2),]  #validation set
Data2 <- DataBis[(nrow(DataBis)/2+1):(nrow(DataBis)),]  #test set

plot(Data$timestamp, Data$Load, type='l', xlab='', ylab='Load')
lines(Data1$timestamp, Data1$Load, col='red')
lines(Data2$timestamp, Data2$Load, col='green')

names(Data)
rg0 <- ranger(Load ~ DayType + Hour + Toy + Temperature + BankHolidays + BridgeDays + Schedule, data=Data0)

rg0 <- ranger(Load ~ DayType + Hour + Toy + Temperature + Schedule, data=Data0)
rg0$forecast <- predict(rg0, data=Data1)$prediction
rmse(Data1$Load, rg0$forecast )
rmse(Data1$Load, rg0$forecast )/mean(Data1$Load)
mape(Data1$Load, rg0$forecast )
#R39Toolbox::rmse(obs=Data1$Load, fit=rg0$forecast )
#R39Toolbox::rmse(obs=Data1$Load, fit=rg0$forecast )/ mean(Data1$Load)
#R39Toolbox::mape(obs=Data1$Load, fit=rg0$forecast )

#eq <- Load ~ DayType + Hour + Toy + Temperature + Schedule  #eq1
eq <- Load ~  Toy + DayType + Hour + Temperature + Schedule + Load.24 + Load.168  +InstantWeek


Monte_carlo_round <- 5
mtry_tree <- c(1:9)

mape_mtry_1 <- NULL
rmse_mtry_1 <- NULL

mape_mtry_2 <- NULL
rmse_mtry_2 <- NULL

error_mtry_oob <- NULL
for(j in c(1:Monte_carlo_round)){
  error_RF_1_rmse <- NULL
  error_RF_1_mape <- NULL
  
  error_RF_2_rmse <- NULL
  error_RF_2_mape <- NULL
  
  error_oob <- NULL
  for(m in mtry_tree){
    RF <- rangerts::ranger(eq, data=Data0,mtry=m)
    RF_forecast_1 <- predict(RF, data=Data1)$prediction
    RF.rmse_1 <-rmse(Data1$Load, RF_forecast_1)
    error_RF_1_rmse <- c(error_RF_1_rmse, RF.rmse_1)
    RF.mape_1 <-mape(Data1$Load, RF_forecast_1)
    error_RF_1_mape <- c(error_RF_1_mape, RF.mape_1)
    
    RF_forecast_2 <- predict(RF, data=Data2)$prediction
    RF.rmse_2 <-rmse(Data2$Load, RF_forecast_2)
    error_RF_2_rmse <- c(error_RF_2_rmse, RF.rmse_2)
    RF.mape_2 <-mape(Data2$Load, RF_forecast_2)
    error_RF_2_mape <- c(error_RF_2_mape, RF.mape_2)
    
    error_oob <- c(error_oob,RF$prediction.error)
    print(m)
  }
  mape_mtry_1 <- rbind(mape_mtry_1, error_RF_1_mape)
  rmse_mtry_1 <- rbind(rmse_mtry_1, error_RF_1_rmse)
  
  mape_mtry_2 <- rbind(mape_mtry_2, error_RF_2_mape)
  rmse_mtry_2 <- rbind(rmse_mtry_2, error_RF_2_rmse)
  
  error_mtry_oob <- rbind(error_mtry_oob, error_oob)
  print(j)
}

RF_error_data_1 <- data.frame(m = as.factor(rep(c(1:10), each = Monte_carlo_round)), rmse = c(mape_mtry_1))
ggplot(RF_error_data_1, aes(x=m, y=rmse)) + geom_boxplot() + ggtitle(TeX("RMSE for Breiman according to the parameter m_{try}")) +
  xlab( TeX("m_{try}")) + ylab("RMSE") +
  theme(plot.title = element_text(lineheight=1, face="bold",
                                  size = 10)) +
  theme(text = element_text(size=10))


(colMeans(mape_mtry_1)-min(colMeans(mape_mtry_1)))/min(colMeans(mape_mtry_1))
(colMeans(rmse_mtry_1)-min(colMeans(rmse_mtry_1)))/min(colMeans(rmse_mtry_1))
(colMeans(error_mtry_oob)-min(colMeans(error_mtry_oob)))/min(colMeans(error_mtry_oob))
which.min(colMeans(error_mtry))
which.min(colMeans(error_mtry_1))
which.min(colMeans(error_mtry_2))

#mtry: 2 pour mape, 3 pour rmse
#only_schedule: 2 pour les deux, mtry=3 avec oob error     3.06%       20.84802rmse    75.40597 oob
#only sched w/o instantweek, 2 pour les deux, 3 pour oob     #3.42% mape  23.18503rmse  75.0 oob meme sur test plus mauvais que précédent
#only sched w/o instantweek w/o toy 3 pour les deux, 2 pour oob bien plus mauvais que le précédent
#only shed w/o hour & daytype: 2 pour les deux et 2 pour oob 3.37%   22.33   76.46
#w/o toy mtry = 3 (pour les trois) 3.71%mape
Monte_carlo_round <- 10
mtry_tree <- 2
block_length_seq <- 24#seq(6, 90, by = 6)
for(j in c(1:Monte_carlo_round)){
  for(m in mtry_tree){
    pred_RF_circ <- NULL
    pred_RF_nono <- NULL
    pred_RF_moving <- NULL
    pred_RF_circ_1 <- NULL
    pred_RF_nono_1 <- NULL
    pred_RF_moving_1 <- NULL
    pred_RF_circ_2 <- NULL
    pred_RF_nono_2 <- NULL
    pred_RF_moving_2 <- NULL
    
    error_mape_RF_circ <- c()
    error_mape_RF_nono <- c()
    error_mape_RF_moving <- c()
    
    error_rmse_RF_circ <- c()
    error_rmse_RF_nono <- c()
    error_rmse_RF_moving <- c()
    
    error_mae_RF_circ <- c()
    error_mae_RF_nono <- c()
    error_mae_RF_moving <- c()
    
    error_mape_RF_circ_1 <- c()
    error_mape_RF_nono_1 <- c()
    error_mape_RF_moving_1 <- c()
    
    error_rmse_RF_circ_1 <- c()
    error_rmse_RF_nono_1 <- c()
    error_rmse_RF_moving_1 <- c()
    
    error_mae_RF_circ_1 <- c()
    error_mae_RF_nono_1 <- c()
    error_mae_RF_moving_1 <- c()
    
    error_mape_RF_circ_2 <- c()
    error_mape_RF_nono_2 <- c()
    error_mape_RF_moving_2 <- c()
    
    error_rmse_RF_circ_2 <- c()
    error_rmse_RF_nono_2 <- c()
    error_rmse_RF_moving_2 <- c()
    
    error_mae_RF_circ_2 <- c()
    error_mae_RF_nono_2 <- c()
    error_mae_RF_moving_2 <- c()
    
    RF_circ_importance<- c()
    RF_nono_importance<- c()
    RF_moving_importance <- c()
    RF_circ_importance_block<- c()
    RF_nono_importance_block<- c()
    RF_moving_importance_block <- c()
    
    RF <- rangerts::ranger(eq, data=Data0,mtry=m, importance = "permutation")
    RF_forecast <- predict(RF, data=DataBis)$prediction
    RF_forecast_1 <- predict(RF, data=Data1)$prediction
    RF_forecast_2 <- predict(RF, data=Data2)$prediction
    RF_importance <- RF$variable.importance
    RF <- NULL
    RF.mape <-mape(DataBis$Load, RF_forecast)
    RF.mae <-mae(DataBis$Load, RF_forecast)
    RF.rmse <-rmse(DataBis$Load, RF_forecast)
    
    RF.mape_1 <-mape(Data1$Load, RF_forecast_1)
    RF.mae_1 <-mae(Data1$Load, RF_forecast_1)
    RF.rmse_1 <-rmse(Data1$Load, RF_forecast_1)
    RF.mape_2 <-mape(Data2$Load, RF_forecast_2)
    RF.mae_2 <-mae(Data2$Load, RF_forecast_2)
    RF.rmse_2 <-rmse(Data2$Load, RF_forecast_2)
    RF_forecast <- NULL
    
    for(i in block_length_seq){
      #train
      RF_circ <- rangerts::ranger(eq, data=Data0,  mtry=m, bootstrap.ts = "circular", block.size = i, importance = "permutation")
      RF_circ_forecast <- predict(RF_circ, data=DataBis)$prediction
      RF_circ_forecast_1 <- predict(RF_circ, data=Data1)$prediction
      RF_circ_forecast_2 <- predict(RF_circ, data=Data2)$prediction
      RF_circ.mape <-mape(DataBis$Load, RF_circ_forecast)
      RF_circ.mae <-mae(DataBis$Load, RF_circ_forecast)
      RF_circ.rmse <-rmse(DataBis$Load, RF_circ_forecast)
      RF_circ.mape_1 <-mape(Data1$Load, RF_circ_forecast_1)
      RF_circ.mae_1 <-mae(Data1$Load, RF_circ_forecast_1)
      RF_circ.rmse_1 <-rmse(Data1$Load, RF_circ_forecast_1)
      RF_circ.mape_2 <-mape(Data2$Load, RF_circ_forecast_2)
      RF_circ.mae_2 <-mae(Data2$Load, RF_circ_forecast_2)
      RF_circ.rmse_2 <-rmse(Data2$Load, RF_circ_forecast_2)
      RF_circ_forecast <- NULL
      RF_circ_importance_temp <- RF_circ$variable.importance
      RF_circ <- NULL
      RF_circ_block <- rangerts::ranger(eq, data=Data0,  mtry=m,block.size = i, bootstrap.ts = "circular",importance="block_permutation")
      RF_circ_block_importance_temp <- RF_circ_block$variable.importance
      RF_circ_block <- NULL
      
      RF_nono <- rangerts::ranger(eq, data=Data0,  mtry=m, block.size = i, bootstrap.ts = "nonoverlapping", importance = "permutation")
      RF_nono_forecast <- predict(RF_nono, data=DataBis)$prediction
      RF_nono_forecast_1 <- predict(RF_nono, data=Data1)$prediction
      RF_nono_forecast_2 <- predict(RF_nono, data=Data2)$prediction
      RF_nono.mape <-mape(DataBis$Load, RF_nono_forecast)
      RF_nono.mae <-mae(DataBis$Load, RF_nono_forecast)
      RF_nono.rmse <-rmse(DataBis$Load, RF_nono_forecast)
      RF_nono.mape_1 <-mape(Data1$Load, RF_nono_forecast_1)
      RF_nono.mae_1 <-mae(Data1$Load, RF_nono_forecast_1)
      RF_nono.rmse_1 <-rmse(Data1$Load, RF_nono_forecast_1)
      RF_nono.mape_2 <-mape(Data2$Load, RF_nono_forecast_2)
      RF_nono.mae_2 <-mae(Data2$Load, RF_nono_forecast_2)
      RF_nono.rmse_2 <-rmse(Data2$Load, RF_nono_forecast_2)
      RF_nono_forecast <- NULL
      RF_nono_importance_temp <- RF_nono$variable.importance
      RF_nono <- NULL
      
      RF_nono_block <- rangerts::ranger(eq, data=Data0,  mtry=m,block.size = i, bootstrap.ts = "nonoverlapping",importance="block_permutation")
      RF_nono_block_importance_temp <- RF_nono_block$variable.importance
      RF_nono_block <- NULL
      
      
      # 
      RF_moving <- rangerts::ranger(eq, data=Data0,  mtry=m,block.size = i, bootstrap.ts = "moving", importance = "permutation")
      RF_moving_forecast <- predict(RF_moving, data=DataBis)$prediction
      RF_moving_forecast_1 <- predict(RF_moving, data=Data1)$prediction
      RF_moving_forecast_2 <- predict(RF_moving, data=Data2)$prediction
      RF_moving.mape <-mape(DataBis$Load, RF_moving_forecast)
      RF_moving.mae <-mae(DataBis$Load, RF_moving_forecast)
      RF_moving.rmse <-rmse(DataBis$Load, RF_moving_forecast)
      RF_moving.mape_1 <-mape(Data1$Load, RF_moving_forecast_1)
      RF_moving.mae_1 <-mae(Data1$Load, RF_moving_forecast_1)
      RF_moving.rmse_1 <-rmse(Data1$Load, RF_moving_forecast_1)
      RF_moving.mape_2 <-mape(Data2$Load, RF_moving_forecast_2)
      RF_moving.mae_2 <-mae(Data2$Load, RF_moving_forecast_2)
      RF_moving.rmse_2 <-rmse(Data2$Load, RF_moving_forecast_2)
      RF_moving_forecast <- NULL
      RF_moving_importance_temp <- RF_moving$variable.importance
      RF_moving <- NULL
      
      RF_moving_block <- rangerts::ranger(eq, data=Data0,  mtry=m,block.size = i, bootstrap.ts = "moving",importance="block_permutation")
      RF_moving_block_importance_temp <- RF_moving_block$variable.importance
      RF_moving_block <- NULL

      #predictions
      pred_RF_circ <- rbind(pred_RF_circ, RF_circ_forecast)
      pred_RF_moving <- rbind(pred_RF_moving, RF_moving_forecast)
      pred_RF_nono <- rbind(pred_RF_nono, RF_nono_forecast)
      
      pred_RF_circ_1 <- rbind(pred_RF_circ_1, RF_circ_forecast_1)
      pred_RF_moving_1 <- rbind(pred_RF_moving_1, RF_moving_forecast_1)
      pred_RF_nono_1 <- rbind(pred_RF_nono_1, RF_nono_forecast_1)
      
      pred_RF_circ_2 <- rbind(pred_RF_circ_2, RF_circ_forecast_2)
      pred_RF_moving_2 <- rbind(pred_RF_moving_2, RF_moving_forecast_2)
      pred_RF_nono_2 <- rbind(pred_RF_nono_2, RF_nono_forecast_2)
      
      #error
      error_mape_RF_circ <- c(error_mape_RF_circ, RF_circ.mape)
      error_mape_RF_nono <- c(error_mape_RF_nono, RF_nono.mape)
      error_mape_RF_moving <- c(error_mape_RF_moving, RF_moving.mape)
      
      error_mae_RF_circ <- c(error_mae_RF_circ, RF_circ.mae)
      error_mae_RF_nono <- c(error_mae_RF_nono, RF_nono.mae)
      error_mae_RF_moving <- c(error_mae_RF_moving, RF_moving.mae)
      
      error_rmse_RF_circ <- c(error_rmse_RF_circ, RF_circ.rmse)
      error_rmse_RF_nono <- c(error_rmse_RF_nono, RF_nono.rmse)
      error_rmse_RF_moving <- c(error_rmse_RF_moving, RF_moving.rmse)
      
      
      error_mape_RF_circ_1 <- c(error_mape_RF_circ_1, RF_circ.mape_1)
      error_mape_RF_nono_1 <- c(error_mape_RF_nono_1, RF_nono.mape_1)
      error_mape_RF_moving_1 <- c(error_mape_RF_moving_1, RF_moving.mape_1)

      error_mae_RF_circ_1 <- c(error_mae_RF_circ_1, RF_circ.mae_1)
      error_mae_RF_nono_1 <- c(error_mae_RF_nono_1, RF_nono.mae_1)
      error_mae_RF_moving_1 <- c(error_mae_RF_moving_1, RF_moving.mae_1)

      error_mape_RF_circ_2 <- c(error_mape_RF_circ_2, RF_circ.mape_2)
      error_mape_RF_nono_2 <- c(error_mape_RF_nono_2, RF_nono.mape_2)
      error_mape_RF_moving_2 <- c(error_mape_RF_moving_2, RF_moving.mape_2)

      error_mae_RF_circ_2 <- c(error_mae_RF_circ_2, RF_circ.mae_2)
      error_mae_RF_nono_2 <- c(error_mae_RF_nono_2, RF_nono.mae_2)
      error_mae_RF_moving_2 <- c(error_mae_RF_moving_2, RF_moving.mae_2)

      error_rmse_RF_circ_1 <- c(error_rmse_RF_circ_1, RF_circ.rmse_1)
      error_rmse_RF_nono_1 <- c(error_rmse_RF_nono_1, RF_nono.rmse_1)
      error_rmse_RF_moving_1 <- c(error_rmse_RF_moving_1, RF_moving.rmse_1)

      error_rmse_RF_circ_2 <- c(error_rmse_RF_circ_2, RF_circ.rmse_2)
      error_rmse_RF_nono_2 <- c(error_rmse_RF_nono_2, RF_nono.rmse_2)
      error_rmse_RF_moving_2 <- c(error_rmse_RF_moving_2, RF_moving.rmse_2)

      #importance
      RF_circ_importance <- rbind(RF_circ_importance, RF_circ_importance_temp)
      RF_nono_importance <- rbind(RF_nono_importance, RF_nono_importance_temp)
      RF_moving_importance <- rbind(RF_moving_importance, RF_moving_importance_temp)
      #block importance
      RF_moving_importance_block <- rbind(RF_moving_importance_block, RF_moving_block_importance_temp)
      RF_nono_importance_block <- rbind(RF_nono_importance_block, RF_nono_block_importance_temp)
      RF_circ_importance_block <- rbind(RF_circ_importance_block, RF_circ_block_importance_temp)
      
    }
    results <- list()
    results$bloc_seq <- block_length_seq
    results$length_data_1 <- nrow(Data1)
    results$length_data_2 <- nrow(Data2)
    
    #predictions
    results$RF.circ_forecast <- pred_RF_circ
    results$RF.nono_forecast <- pred_RF_nono
    results$RF.moving_forecast <- pred_RF_moving
    results$RF.circ_forecast_1 <- pred_RF_circ_1
    results$RF.nono_forecast_1 <- pred_RF_nono_1
    results$RF.moving_forecast_1 <- pred_RF_moving_1
    results$RF.circ_forecast_2 <- pred_RF_circ_2
    results$RF.nono_forecast_2 <- pred_RF_nono_2
    results$RF.moving_forecast_2 <- pred_RF_moving_2
    
    ####RMSE
    results$RF.rmse <-RF.rmse
    results$RF_circ.rmse <-error_rmse_RF_circ
    results$RF_nono.rmse <-error_rmse_RF_nono
    results$RF_moving.rmse <-error_rmse_RF_moving
    
    results$RF.rmse_1 <-RF.rmse_1
    results$RF_circ.rmse_1 <-error_rmse_RF_circ_1
    results$RF_nono.rmse_1 <-error_rmse_RF_nono_1
    results$RF_moving.rmse_1 <-error_rmse_RF_moving_1
    
    results$RF.rmse_2 <-RF.rmse_2
    results$RF_circ.rmse_2 <-error_rmse_RF_circ_2
    results$RF_nono.rmse_2 <-error_rmse_RF_nono_2
    results$RF_moving.rmse_2 <-error_rmse_RF_moving_2
    ####MAPE
    results$RF.mape <-RF.mape
    results$RF_circ.mape <-error_mape_RF_circ
    results$RF_nono.mape <-error_mape_RF_nono
    results$RF_moving.mape <-error_mape_RF_moving
    
    results$RF.mape_1 <-RF.mape_1
    results$RF_circ.mape_1 <-error_mape_RF_circ_1
    results$RF_nono.mape_1 <-error_mape_RF_nono_1
    results$RF_moving.mape_1 <-error_mape_RF_moving_1
    
    results$RF.mape_2 <-RF.mape_2
    results$RF_circ.mape_2 <-error_mape_RF_circ_2
    results$RF_nono.mape_2 <-error_mape_RF_nono_2
    results$RF_moving.mape_2 <-error_mape_RF_moving_2
    
    ####mae
    results$RF.mae <-RF.mae
    results$RF_circ.mae <-error_mae_RF_circ
    results$RF_nono.mae <-error_mae_RF_nono
    results$RF_moving.mae <-error_mae_RF_moving
    
    results$RF.mae_1 <-RF.mae_1
    results$RF_circ.mae_1 <-error_mae_RF_circ_1
    results$RF_nono.mae_1 <-error_mae_RF_nono_1
    results$RF_moving.mae_1 <-error_mae_RF_moving_1
    
    results$RF.mae_2 <-RF.mae_2
    results$RF_circ.mae_2 <-error_mae_RF_circ_2
    results$RF_nono.mae_2 <-error_mae_RF_nono_2
    results$RF_moving.mae_2 <-error_mae_RF_moving_2
    
    #importance
    results$RF.importance <- RF_importance
    results$RF_circ.importance <- RF_circ_importance
    results$RF_nono.importance <- RF_nono_importance
    results$RF_moving.importance <- RF_moving_importance
    #block permut
    results$RF_moving.importance_block <- RF_moving_importance_block
    results$RF_circ.importance_block <- RF_circ_importance_block
    results$RF_nono.importance_block <- RF_nono_importance_block
    
    nomF <- paste0("Result.Monte_carlo_", j, ".mtry_", m, ".RDS")
    saveRDS(results, file=paste0("~/Documents/randomForest/rf_Time_series/patrick_results/", nomF))
    print(m)
    results <- NULL
  }
  print(j)
}


RF_a <- c()
RF_circ_a <- c()
RF_moving_a <- c()
RF_nono_a <- c()
for(j in c(1:Monte_carlo_round)){
  nomF <- paste0("Result.Monte_carlo_", j, ".mtry_", 2, ".only_schedule.RDS")
  file <- readRDS(paste0("~/Documents/randomForest/rf_Time_series/patrick_results/", nomF))
  RF_a <- c(RF_a, file$RF.rmse_2)
  RF_circ_a <- rbind(RF_circ_a, file$RF_circ.rmse_2[which.min(file$RF_circ.rmse_1)])
  RF_moving_a <- rbind(RF_moving_a, file$RF_moving.rmse_2[which.min(file$RF_moving.rmse_1)])
  RF_nono_a <- rbind(RF_nono_a, file$RF_nono.rmse_2[which.min(file$RF_nono.rmse_1)])
}

data_rf <- cbind(RF_a,RF_circ_a, RF_moving_a, RF_nono_a)# RF_stat_a, RF_season_1_a)#, RF_season_2_a)

block_error_data <- data.frame(variant = as.factor(rep(c("I.I.D", "Circular", "Moving", "Non-overlapping")   , each = Monte_carlo_round)), 
                               rmse = c(data_rf),
                               group = c(rep("Original", Monte_carlo_round), rep("New", Monte_carlo_round*3) ))
mycols_fill <- c(rgb(132,186,91, maxColorValue = 255),rgb(114,147,203, maxColorValue = 255))#, )
mycols_line <- c( rgb(62,150,81, maxColorValue = 255),rgb(57,106,177, maxColorValue = 255))#, )

ggthemr("fresh")
ggplot(block_error_data, aes(x=variant, y=rmse, fill=group)) + geom_boxplot() + #ggtitle(TeX("RMSE for each block bootstrap variant under best length block (Extra-Trees, m_{try} = 13)")) +
  xlab("Variant") + ylab("RMSE") +
  scale_fill_manual(values = mycols_fill) +
  scale_colour_manual(values = mycols_line) +
  scale_y_continuous(breaks=pretty_breaks())+
  theme(legend.position="none")+
  theme(text = element_text(size=10),plot.title = element_text(lineheight=1, face="bold",
                                                               size = 10),axis.text=element_text(size=20),axis.title=element_text(size=40,face="bold")) 



#ici amélioration sur tous les dep bootstraps
(mean(RF_a)-mean(RF_moving_a))/mean(RF_a)  
(mean(RF_a)-mean(RF_nono_a))/mean(RF_a)  
(mean(RF_a)-mean(RF_circ_a))/mean(RF_a)

(median(RF_a)-median(RF_moving_a))/median(RF_a)  
(median(RF_a)-median(RF_nono_a))/median(RF_a)  
(median(RF_a)-median(RF_circ_a))/median(RF_a)

error_mtry <- list()
for(m in mtry_tree){
  RF_a <- NULL
  error_block_dependent <- NULL
  RF_circ_a <- c()
  RF_moving_a <- c()
  RF_stat_a <- c()
  RF_nono_a <- c()
  RF_season_1_a <- c()
  RF_season_2_a <- c()
  for(j in c(1:Monte_carlo_round)){
    nomF <- paste0("Result.Monte_carlo_", j, ".mtry_", m, ".only_schedule.RDS")
    file <- readRDS(paste0("~/Documents/randomForest/rf_Time_series/patrick_results/", nomF))
    RF_a <- c(RF_a, file$RF.rmse_2)
    RF_circ_a <- rbind(RF_circ_a, file$RF_circ.rmse_2)
    RF_moving_a <- rbind(RF_moving_a, file$RF_moving.rmse_2)
    RF_nono_a <- rbind(RF_nono_a, file$RF_nono.rmse_2)
  }
  error_block_dependent <- cbind(colMeans(RF_circ_a), colMeans(RF_moving_a), colMeans(RF_nono_a),  rep(mean(RF_a), length(block_length_seq)))# rep(m,339))
  
  error_mtry[[m]] <- error_block_dependent
}


#"Circ", "Moving", "Nono", "Stat",
data_error_block_dependent <- data.frame(Block_size = rep(block_length_seq,4), rmse = c(unlist(lapply(error_mtry,c))), 
                                         name = rep(c("Circ", "Moving", "Nono",  "Independent"), each =length(block_length_seq)),
                                         mtry = rep(paste0("mtry = ",mtry_tree), each = 4*length(block_length_seq)))

mycols_fill <- c(rgb(114,147,203, maxColorValue = 255), rgb(225,151,76, maxColorValue = 255), rgb(132,186,91, maxColorValue = 255))
mycols_line <- c(rgb(57,106,177, maxColorValue = 255), "black",rgb(218,124,48, maxColorValue = 255), rgb(62,150,81, maxColorValue = 255))
ggplot(data_error_block_dependent, aes(x=Block_size, y=rmse)) + geom_line(aes(colour = name),size=3) + 
  xlab("Block size") + ylab("RMSE") +
  scale_colour_manual(values = mycols_line) +
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=pretty_breaks())+
  #geom_hline(yintercept=mean(RF_a), color="black", size=0.5)+
  labs(colour = "Variant")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=40,face="bold"),
        legend.title = element_text(size=20), legend.text = element_text(size=20),legend.key.size = unit(1, "cm"),
        legend.key.width = unit(1,"cm"))

#boxplot des performances pour block_size choisti avant
m<-2
RF_a <- NULL
RF_circ_a <- NULL
RF_nono_a <- NULL
RF_moving_a <- NULL
for(j in c(1:Monte_carlo_round)){
  nomF <- paste0("Result.Monte_carlo_", j, ".mtry_", 2, ".only_schedule.RDS")
  file <- readRDS(paste0("~/Documents/randomForest/rf_Time_series/patrick_results/", nomF))
  RF_a <- c(RF_a, file$RF.mape_2)
  RF_circ_a <- rbind(RF_circ_a, file$RF_circ.mape_2)
  RF_moving_a <- rbind(RF_moving_a, file$RF_moving.mape_2)
  RF_nono_a <- rbind(RF_nono_a, file$RF_nono.mape_2)
}
error_block_dependent <- cbind(RF_a, RF_circ_a,RF_moving_a,RF_nono_a)

mycols_fill <- c(rgb(132,186,91, maxColorValue = 255),rgb(114,147,203, maxColorValue = 255))#, )
mycols_line <- c( rgb(62,150,81, maxColorValue = 255),rgb(57,106,177, maxColorValue = 255))#, )

block_error_data <- data.frame(variant = as.factor(rep(c("I.I.D", "Circular", "Moving", "Non-overlapping")   , each = Monte_carlo_round)), 
                               rmse = c(error_block_dependent),
                               group = c(rep("Original", Monte_carlo_round), rep("New", Monte_carlo_round*3) ))
ggplot(block_error_data, aes(x=variant, y=rmse, fill=group)) + geom_boxplot() + #ggtitle(TeX("RMSE for each block bootstrap variant under best length block (Extra-Trees, m_{try} = 13)")) +
  xlab("Variant") + ylab("RMSE") +
  scale_fill_manual(values = mycols_fill) +
  scale_colour_manual(values = mycols_line) +
  scale_y_continuous(breaks=pretty_breaks())+
  theme(legend.position="none")+
  theme(text = element_text(size=10),plot.title = element_text(lineheight=1, face="bold",
                                                               size = 10),axis.text=element_text(size=10),axis.title=element_text(size=18,face="bold")) 

(mean(RF_a)-mean(RF_moving_a))/mean(RF_a)  
(mean(RF_a)-mean(RF_circ_a))/mean(RF_a)  
(mean(RF_a)-mean(RF_nono_a))/mean(RF_a)

RF_i <- c()
RF_moving_i <- c()
RF_moving_i_block <- c()
RF_nono_i <- c()
RF_nono_i_block <- c()
RF_circ_i <- c()
RF_circ_i_block <- c()
for(j in c(1:Monte_carlo_round)){
  nomF <- paste0("Result.Monte_carlo_", j, ".mtry_", 2, ".imp_24_noise.RDS")
  file <- readRDS(paste0("~/Documents/randomForest/rf_Time_series/patrick_results/", nomF))
  RF_i <- rbind(RF_i, file$RF.importance)
  RF_nono_i <- rbind(RF_nono_i, file$RF_nono.importance)#[4,])
  RF_nono_i_block <- rbind(RF_nono_i_block, file$RF_nono.importance_block)#[4,])
  RF_moving_i <- rbind(RF_moving_i, file$RF_moving.importance)#[4,])
  RF_moving_i_block <- rbind(RF_moving_i_block, file$RF_moving.importance_block)#[4,])
  RF_circ_i <- rbind(RF_circ_i, file$RF_circ.importance)#[4,])
  RF_circ_i_block <- rbind(RF_circ_i_block, file$RF_circ.importance_block)#[4,])
}

imp <- sort(colMeans(RF_i), decreasing = TRUE)
nono_imp <- sort(colMeans(RF_nono_i), decreasing = TRUE)
nono_imp_block <- sort(colMeans(RF_nono_i_block), decreasing = TRUE)

importance_RF_nono <- data.frame(names = as.factor(rep(colnames(RF_nono_i)   , each = Monte_carlo_round)), importance = c(RF_nono_i))
importance_block_RF_nono <- data.frame(names = as.factor(rep(colnames(RF_nono_i_block)   , each = Monte_carlo_round)), importance = c(RF_nono_i_block))
levels(importance_RF_nono$names)[4] <- "Load_168"
levels(importance_RF_nono$names)[5] <- "Load_24"
levels(importance_block_RF_nono$names)[4] <- "Load_168"
levels(importance_block_RF_nono$names)[5] <- "Load_24"

ggplot(importance_RF_nono, aes(x=names, y=importance)) + geom_boxplot() + #ggtitle(TeX("Variable importance for the moving block bootstrap (Extra-Trees, m_{try} = 13)")) +
  xlab("Variable") + ylab("Importance") +
  scale_y_continuous(breaks=pretty_breaks())+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=40,face="bold")) + coord_flip()

ggplot(importance_block_RF_nono, aes(x=names, y=importance)) + geom_boxplot() + #ggtitle(TeX("Variable importance for the nono block bootstrap (Extra-Trees, m_{try} = 13)")) +
  xlab("Variable") + ylab("Importance") +
  scale_y_continuous(breaks=pretty_breaks())+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=40,face="bold")) + coord_flip()


importance_RF_moving <- data.frame(names = as.factor(rep(colnames(RF_moving_i)   , each = Monte_carlo_round)), importance = c(RF_moving_i))
importance_block_RF_moving <- data.frame(names = as.factor(rep(colnames(RF_moving_i_block)   , each = Monte_carlo_round)), importance = c(RF_moving_i_block))
levels(importance_RF_moving$names)[4] <- "Load_168"
levels(importance_RF_moving$names)[5] <- "Load_24"
levels(importance_block_RF_moving$names)[4] <- "Load_168"
levels(importance_block_RF_moving$names)[5] <- "Load_24"

ggplot(importance_RF_moving, aes(x=names, y=importance)) + geom_boxplot() + #ggtitle(TeX("Variable importance for the moving block bootstrap (Extra-Trees, m_{try} = 13)")) +
  xlab("Variable") + ylab("Importance") +
  scale_y_continuous(breaks=pretty_breaks())+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=40,face="bold")) + coord_flip()

ggplot(importance_block_RF_moving, aes(x=names, y=importance)) + geom_boxplot() + #ggtitle(TeX("Variable importance for the moving block bootstrap (Extra-Trees, m_{try} = 13)")) +
  xlab("Variable") + ylab("Importance") +
  scale_y_continuous(breaks=pretty_breaks())+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=40,face="bold")) + coord_flip()



importance_RF_circ <- data.frame(names = as.factor(rep(colnames(RF_circ_i)   , each = Monte_carlo_round)), importance = c(RF_circ_i))
importance_block_RF_circ <- data.frame(names = as.factor(rep(colnames(RF_circ_i_block)   , each = Monte_carlo_round)), importance = c(RF_circ_i_block))
ggplot(importance_RF_circ, aes(x=names, y=importance)) + geom_boxplot() + #ggtitle(TeX("Variable importance for the circ block bootstrap (Extra-Trees, m_{try} = 13)")) +
  xlab("Variable") + ylab("Importance") +
  theme(plot.title = element_text(lineheight=1, face="bold",
                                  size = 10)) +
  theme(text = element_text(size=5))

ggplot(importance_block_RF_circ, aes(x=names, y=importance)) + geom_boxplot() + #ggtitle(TeX("Variable importance for the circ block bootstrap (Extra-Trees, m_{try} = 13)")) +
  xlab("Variable") + ylab("Importance") +
  theme(plot.title = element_text(lineheight=1, face="bold",
                                  size = 10)) +
  theme(text = element_text(size=5))
