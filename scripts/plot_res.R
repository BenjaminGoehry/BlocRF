
res <- readRDS(res, file="Results/mlbench.friedman1/AR_05.RDS")

col <- piratepal("basel")
plot(block.size, res$mse_rf_mb, type='l', ylim=range(res$mse_rf_iid, res$mse_rf_mb, res$mse_rf_cir), col=col[1])
lines(block.size, res$mse_rf_cir, col=col[2])
lines(block.size, res$mse_rf_nov, col=col[3])
abline(h=res$mse_rf_iid)
legend('topright', col=c('black', col[1:3]), c('iid', 'moving block', 'circular','non-overlapping'), bty='n', lty=1)



res <- readRDS(res, file="Results/cos/cos_deux_period.RDS")

names(res)
col <- piratepal("basel")
plot(block.size, res$mse_forecast_iid, type='b', pch=20, ylim=range(res$mse_forecast_iid, res$mse_forecast_moving, res$mse_forecast_circular, res$mse_forecast_nov))
lines(block.size, res$mse_forecast_moving, type='b', pch=20, col=col[1])
lines(block.size, res$mse_forecast_circular, type='b', pch=20, col=col[2])
lines(block.size, res$mse_forecast_nov, type='b', pch=20, col=col[3])
legend('topright', col=c('black', col[1:3]), c('iid', 'moving block', 'circular','non-overlapping'), bty='n', lty=1)


names(res)
col <- piratepal("basel")
plot(block.size, res$mse_fit_iid, type='b', pch=20, ylim=range(res$mse_fit_iid, res$mse_fit_moving, res$mse_fit_circular, res$mse_fit_nov))
lines(block.size, res$mse_fit_moving, type='b', pch=20, col=col[1])
lines(block.size, res$mse_fit_circular, type='b', pch=20, col=col[2])
lines(block.size, res$mse_fit_nov, type='b', pch=20, col=col[3])
legend('topright', col=c('black', col[1:3]), c('iid', 'moving block', 'circular','non-overlapping'), bty='n', lty=1)






