# Step2: AFT-BART-NP-HTE Model
library(rpart.plot)
library(rpart)
mydata_trt_1 <- X %>%  
  filter(W == 1) %>% 
  select(-W) 

mydata_trt_0 <- X %>%  
  filter(W == 0) %>% 
  select(-W) 


set.seed(20250603)
# Train AFTrees
AFTrees_mydata_trt_1 = AFTrees(
  x.train = mydata_trt_1 %>% select(-Time, -Event, -ID) %>% as.data.frame(),
  y.train =  mydata_trt_1$Time,
  status = mydata_trt_1$Event,
  nskip = 10000,
  ndpost = 5000,
  k = 2, 
  ntree = 200,
  x.test = mydata_trt_0 %>% select(-Time, -Event, -ID) %>% as.data.frame(),
  nonparametric = F
)

AFTrees_mydata_trt_0 = AFTrees(
  x.train = mydata_trt_0 %>% select(-Time, -Event, -ID) %>% as.data.frame(),
  y.train =  mydata_trt_0$Time,
  status = mydata_trt_0$Event,
  nskip = 10000,
  ndpost = 5000,
  k = 2,
  ntree = 200,
  x.test = mydata_trt_1 %>% select(-Time, -Event, -ID) %>% as.data.frame(),
  nonparametric = F
)


# visualize ISTE
trt_1_mat = rbind(t(AFTrees_mydata_trt_1$m.train), t(AFTrees_mydata_trt_1$m.test))
trt_0_mat = rbind(t(AFTrees_mydata_trt_0$m.train), t(AFTrees_mydata_trt_0$m.test))

iste_mat = trt_1_mat[order(c(mydata_trt_1$ID,mydata_trt_1_imp$ID)),] - 
  trt_0_mat[order(c(mydata_trt_0$ID,mydata_trt_0_imp$ID)),]

library(ggplot2)
library(reshape2)
library(plyr)
library(gridExtra)
library(grid)
library(lattice)
library(Matrix)

iste_mean = apply(iste_mat, 1, mean)
############ Compared to actual received
sum(X$W==1)
sum(iste_mean[1:1431]>0)

bf_t <- ifelse(iste_mean > 0,
               0,
               abs(iste_mean))
bf_t <- bf_t[1:1431]

mean(bf_t)

bf_f <- ifelse(iste_mean < 0,
               0,
               abs(iste_mean))
bf_f <- bf_f[1432:2859]
mean(bf_f)
bf <- c(bf_t, bf_f)
mean(bf)
median(bf)
exp(mean(bf))

iste_abs = abs(iste_mean)
median(iste_abs)

########## Compared to all torsemide ##############
exp(abs(sum(iste_mean[iste_mean < 0])/2859))

######### Compared to all furosemide ############
exp(abs(sum(iste_mean[iste_mean >0])/2859))

iste_mat_ordered = iste_mat[order(iste_mean, decreasing = F),]

# Credible Interval -----
ub = apply(iste_mat_ordered, 1, function(x) quantile(x, 0.975))
est = apply(iste_mat_ordered, 1, mean)
lb = apply(iste_mat_ordered, 1, function(x) quantile(x, 0.025))
dat_iste = data.frame(index = 1:nrow(iste_mat_ordered), est = est, ub = ub, lb = lb)

fig_iste = ggplot(dat_iste, aes(x = index,y = est)) +
  geom_smooth(aes(ymin = lb, ymax = ub),stat = "identity", fill = alpha(5, 0.3), colour = 4) +
  xlab("") + ylab("") + ggtitle("(b) ISTE of torsemide v. furosemide inascending order") + theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14, face = "bold")) + 
  scale_x_continuous(breaks = round(seq(0, dim(dat_iste)[1], by = dim(dat_iste)[1]/5),0)) + 
  geom_hline(yintercept = 0, color = 10, size = 1.2, linetype = 2)

yleft = textGrob("CATE in log time scale", rot = 90, gp = gpar(fontsize = 15, fontface = "bold"))
bottom = textGrob("Patients Index", gp = gpar(fontsize = 15, fontface = "bold"))

uni = grid.arrange(arrangeGrob(fig_iste, nrow = 1, ncol = 1), left = yleft, bottom = bottom)

ggsave("D:/hp/Desktop/Research in Yale/TF project/20250603plot/Figure_1_CATE_Mortality_main.jpeg", plot = uni, width = 21, height = 12, 
       units = "cm", dpi = 1600)

# Histogram -----
dat_hist = data.frame(cate = iste_mean)
fig_hist = ggplot(dat_hist, aes(x = cate)) + 
  geom_histogram(aes(y = ..density..), colour = 4, fill = alpha(5, 0.3)) +
  geom_density(lwd = 1.2, linetype = 1, colour = 4)  +
  theme_minimal() +
  xlab("") + ylab("") + ggtitle("(a) Distribution of ISTE for torsemide v. furosemide") + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14, face = "bold")) + 
  scale_x_continuous(breaks = round(seq(-3, 3, by = 1), 1), limits = c(-2.5, 2.5)) + 
  geom_vline(xintercept = 0, color = 10, size = 1.2, linetype = 2)

yleft = textGrob("Density", rot = 90, gp = gpar(fontsize = 15, fontface = "bold"))
bottom = textGrob("CATE in log time scale", gp = gpar(fontsize = 15, fontface = "bold"))

uni = grid.arrange(arrangeGrob(fig_hist, nrow = 1, ncol = 1), left = yleft, bottom = bottom)

ggsave("D:/hp/Desktop/Research in Yale/TF project/20250603plot/Figure_S2_CATE_HF_hist_main.jpeg", plot = uni, width = 21, height = 12, units = "cm", dpi = 1600)

#t.test(iste_mean, mu = 0)

avg_iste_draws <- colMeans(iste_mat)  

mean_avg_iste <- mean(avg_iste_draws)
ci_avg_iste   <- quantile(avg_iste_draws, probs = c(0.025, 0.975))

prob_avg_lt_zero <- mean(avg_iste_draws < 0)   # P(mean ISTE < 0 | data)
prob_avg_gt_zero <- mean(avg_iste_draws > 0)   # P(mean ISTE > 0 | data)

iste_cri <- t(apply(iste_mat, 1, quantile, probs = c(0.025, 0.975)))

cat("average ISTE posterior mean =", round(mean_avg_iste, 3), "\n")
cat("average ISTE 95% CrI = [", round(ci_avg_iste[1], 3), ", ", round(ci_avg_iste[2], 3), "]\n")
cat("P(average ISTE < 0) =", round(prob_avg_lt_zero, 3), 
    ";  P(average ISTE > 0) =", round(prob_avg_gt_zero, 3), "\n")

# ISTE was used as dependent variables in an exploratory linear regression analysis
# To estimate standard errors and uncertainty intervals for the regression coefficients, we drew for each individual 5000 posterior MCMC samples of ISTE from the AFT-BART models.
result_lm_all <- NULL
for (i in 1:5000){ # 5000 posterior MCMC samples of ISTE from the AFT-BART model
  iste <- iste_mat[,i]
  result_lm <- X %>%
    select(-W, - Time, -Event) %>% 
    mutate(iste = iste) %>% 
    lm(iste ~ ., data = .) %>% 
    broom::tidy()
  result_lm_all <- result_lm_all %>% 
    bind_rows(result_lm)
}
# The regression coefficients and 95% credible intervals for all covariates and the intercept from the AFT-BART-NP model
result_lm_all <- na.omit(result_lm_all)
result_lm_all %>% 
  group_by(term) %>% 
  summarise(estimate = mean(estimate), 
            lower = quantile(estimate, 0.025),
            upper = quantile(estimate, 0.975))

result_sum <- data.frame(term = rep(NA,nrow(result_lm)), estimate = rep(NA,nrow(result_lm)), 
                         lower = rep(NA,nrow(result_lm)), upper = rep(NA,nrow(result_lm)))
for (i in 1:nrow(result_lm)) {
  result_sum$term[i] = result_lm$term[i]
  result_sum$estimate[i] = mean(result_lm_all$estimate[result_lm_all$term == result_lm$term[i]])
  result_sum$lower[i] = quantile(result_lm_all$estimate[result_lm_all$term == result_lm$term[i]], 0.025)
  result_sum$upper[i] = quantile(result_lm_all$estimate[result_lm_all$term == result_lm$term[i]], 0.975)
}
result_sum <- na.omit(result_sum)
result_sum
result_sum[result_sum$lower*result_sum$upper > 0,]

dat_bart_iste <- X %>%
  mutate(iste = apply(iste_mat, 1, mean)) %>%
  select(-W, - Event, -Time, -ID)


# CART Model ------
cart <- rpart(iste ~ ., data = dat_bart_iste, control = rpart.control(cp = 0.01, maxdepth = 4), 
              model = TRUE)
rpart.plot(cart, type = 2, extra = 101, under = TRUE, tweak = 1.2)
tmp = printcp(cart)
rsq.val = 1 - tmp[, c(3,4)] 
rsq.val 

# summary(cart)
library(treevalues)
treeval.plot(rpart::rpart(iste ~ .,
                          data = dat_bart_iste,
                          model = TRUE, control=rpart.control(maxdepth = 2)), 
             permute=TRUE, inferenceType=2, digits=4
)
library(rpart.plot)
rpart.plot(cart, type = 2, yesno = 2, nn = TRUE, extra = 1, tweak = 1)

treeval.plot(rpart::rpart(iste ~ .,
                          data = dat_bart_iste,
                          model = TRUE, control=rpart.control(maxdepth = 3)), 
             permute=TRUE, inferenceType=2, digits=3
)

treeval.plot(rpart::rpart(iste ~ .,
                          data = dat_bart_iste,
                          model = TRUE, control=rpart.control(maxdepth = 4)), 
             permute=TRUE, inferenceType=2, digits=2
)

treeval.plot(rpart::rpart(iste ~ .,
                          data = dat_bart_iste %>% select(iste,HXAFIBN,EARESMerged) %>% as.data.frame(),
                          model = TRUE, control=rpart.control(maxdepth = 3)), 
             permute=TRUE, inferenceType=2, digits=4
)

treeval.plot(rpart::rpart(iste ~ .,
                          data = dat_bart_iste %>% select(iste,HXAFIBN,EARESMerged, BMRAN, LDPRIORN, Beta_blocker) %>% as.data.frame(),
                          model = TRUE, control=rpart.control(maxdepth = 4)), 
             permute=TRUE, inferenceType=2, digits=2
)


############# 3 level tree ##################
iste_sg1 <- colMeans(iste_mat)
quantile(iste_sg1, c(0.025,0.975))
iste_sg2 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0),])
quantile(iste_sg2, c(0.025,0.975))
iste_sg3 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 1),])
quantile(iste_sg3, c(0.025,0.975))
iste_sg4 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0) & (dat_bart_iste$EARESMerged < -0.2542),])
quantile(iste_sg4, c(0.025,0.975))
iste_sg5 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0) & (dat_bart_iste$EARESMerged >= -0.2542),])
quantile(iste_sg5, c(0.025,0.975))

iste_sg6 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 1) & (dat_bart_iste$EARESMerged < -0.2594),])
quantile(iste_sg6, c(0.025,0.975))
iste_sg7 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 1) & (dat_bart_iste$EARESMerged >= -0.2594),])
quantile(iste_sg7, c(0.025,0.975))

############## 4 level tree ################
iste_sg1 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0) & (dat_bart_iste$EARESMerged < -0.2542) & (dat_bart_iste$BMRAN == 1),])
quantile(iste_sg1, c(0.025,0.975))
iste_sg2 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0) & (dat_bart_iste$EARESMerged < -0.2542) & (dat_bart_iste$BMRAN == 0),])
quantile(iste_sg2, c(0.025,0.975))

iste_sg3 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0) & (dat_bart_iste$EARESMerged >= -0.2542) & (dat_bart_iste$BMRAN == 1),])
quantile(iste_sg3, c(0.025,0.975))
iste_sg4 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0) & (dat_bart_iste$EARESMerged >= -0.2542) & (dat_bart_iste$BMRAN == 0),])
quantile(iste_sg4, c(0.025,0.975))

iste_sg5 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 1) & (dat_bart_iste$EARESMerged < -0.2594) & (dat_bart_iste$BMRAN == 1),])
quantile(iste_sg5, c(0.025,0.975))
iste_sg6 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 1) & (dat_bart_iste$EARESMerged < -0.2594) & (dat_bart_iste$BMRAN == 0),])
quantile(iste_sg6, c(0.025,0.975))

iste_sg7 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 1) & (dat_bart_iste$EARESMerged >= -0.2594) & (dat_bart_iste$BMRAN == 1),])
quantile(iste_sg7, c(0.025,0.975))
iste_sg8 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 1) & (dat_bart_iste$EARESMerged >= -0.2594) & (dat_bart_iste$BMRAN == 0),])
quantile(iste_sg8, c(0.025,0.975))

############## 5 level tree #################

##### left
iste_sg1 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0) & (dat_bart_iste$EARESMerged < -0.2542) & (dat_bart_iste$BMRAN == 1),])
quantile(iste_sg1, c(0.025,0.975))
iste_sg2 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0) & (dat_bart_iste$EARESMerged < -0.2542) & (dat_bart_iste$Beta_blocker == 0),])
quantile(iste_sg2, c(0.025,0.975))
iste_sg3 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0) & (dat_bart_iste$EARESMerged < -0.2542) & (dat_bart_iste$Beta_blocker == 1),])
quantile(iste_sg3, c(0.025,0.975))
iste_sg4 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0) & (dat_bart_iste$EARESMerged >= -0.2542) & (dat_bart_iste$BMRAN == 1),])
quantile(iste_sg4, c(0.025,0.975))
iste_sg5 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0) & (dat_bart_iste$EARESMerged >= -0.2542) & (dat_bart_iste$BMRAN == 0) & (dat_bart_iste$LDPRIORN == 0),])
quantile(iste_sg5, c(0.025,0.975))
iste_sg6 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 0) & (dat_bart_iste$EARESMerged >= -0.2542) & (dat_bart_iste$BMRAN == 0) & (dat_bart_iste$LDPRIORN == 1),])
quantile(iste_sg6, c(0.025,0.975))

##### right
iste_sg7 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 1) & (dat_bart_iste$EARESMerged < -0.2594) & (dat_bart_iste$BMRAN == 1),])
quantile(iste_sg7, c(0.025,0.975))
iste_sg8 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 1) & (dat_bart_iste$EARESMerged < -0.2594) & (dat_bart_iste$BMRAN == 0),])
quantile(iste_sg8, c(0.025,0.975))
iste_sg9 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 1) & (dat_bart_iste$EARESMerged >= -0.2594) & (dat_bart_iste$BMRAN == 1),])
quantile(iste_sg9, c(0.025,0.975))
iste_sg10 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 1) & (dat_bart_iste$EARESMerged >= -0.2594) & (dat_bart_iste$BMRAN == 0) & (dat_bart_iste$LDPRIORN == 0),])
quantile(iste_sg10, c(0.025,0.975))
iste_sg11 <- colMeans(iste_mat[(dat_bart_iste$HXAFIBN == 1) & (dat_bart_iste$EARESMerged >= -0.2594) & (dat_bart_iste$BMRAN == 0) & (dat_bart_iste$LDPRIORN == 1),])
quantile(iste_sg11, c(0.025,0.975))





