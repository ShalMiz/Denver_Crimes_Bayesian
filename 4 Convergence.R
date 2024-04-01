### Convergence
library(coda)

rownames(beta_mc_P) <- c("Beta 0", "Beta 1", "Beta 2", "Beta 3", "Beta 4", "Beta 5", "Beta 6", "Beta 7", "Beta 8")
beta_mc_P_draws <- mcmc(t(beta_mc_P)[5000:10000,])
pdf(paste0("beta_mc_P_ACF.pdf"),width=8, height=8)
autocorr.plot(beta_mc_P_draws)
dev.off()

rownames(beta_mc_8) <- c("Beta 0", "Beta 1", "Beta 2", "Beta 3", "Beta 4", "Beta 5", "Beta 6", "Beta 7", "Beta 8")
beta_mc_8_draws <- mcmc(t(beta_mc_8)[1000:5000,])
pdf(paste0("beta_mc_8_ACF.pdf"),width=8, height=8)
autocorr.plot(beta_mc_8_draws)
dev.off()

summary(myprobit)
summary(beta_mc_P_draws)

rownames(beta_mc_32) <- c("Beta 0", "Beta 1", "Beta 2", "Beta 3", "Beta 4", "Beta 5", "Beta 6", "Beta 7", "Beta 8")
beta_mc_32_draws <- mcmc(t(beta_mc_32)[1000:5000,])
pdf(paste0("beta_mc_32_ACF.pdf"),width=8, height=8)
autocorr.plot(beta_mc_32_draws)
dev.off()
summary(beta_mc_32_draws)

###comparing lambda values
summary(lambda_post_32[,1])

