getwd()

### Colostrum data

# Mixture model for cut-off estimation
#############
summary(data$RID_con)
# Min 3.6, Max 418.8 -> step = (418.8 - 3.6)/100 - 4.152
summary(log(data$RID_con))
# Min 1.281, Max 6.037 -> step = (6.037 - 1.281)/100 - 0.05
summary(data$IR_con)
# Min -90.88, Max 226.93 -> step = (226.93 + 90.88)/100 - 3.1781
summary(log(data$IR_con+100))
# Min - 2.21 Max 5.790 -> step = (5.79 - 2.21)/100 - 0.04
# Min 1.281, Max 6.037 -> step = (6.037 - 1.281)/100
summary(data$dBr_con)
# Min 9, Max 40.8 -> step = (40.8-9)/100 - 0.318
summary(log(data$dBr_con))
# Min 2.197 Max 3.709 -> step = (3.709 - 2.197)/100 - 0.02


results = run.jags("mix_model", 
                   data=list(N=dim(data)[1],
                             RID=log(data$RID_con),
                             IR=log(data$IR_con+100),
                             dBr=log(data$dBr_con),
                             alpha=c(1,1)),
                   n.chains=2)


posterior <- summary(results)
posterior <- data.frame(posterior)
row.names(posterior)
posterior$ID <- rownames(posterior)
which.max(posterior[grepl("Y_RID", posterior$ID),]$Median)
which.max(posterior[grepl("Y_IR", posterior$ID),]$Median) 
which.max(posterior[grepl("Y_dBr", posterior$ID),]$Median)



library(ggplot2)

# Predictive Values
# Predictives Values
# PPV and NPV for RID

ppv_RID <- round((round(posterior[grepl("se_RID", posterior$ID), 1:3],3)[45,] * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] ) / (round(posterior[grepl("se_RID", posterior$ID), 1:3],3)[45,] * round(posterior[grepl("P", posterior$ID),1:3],2)[1,]  +  (1 - round(posterior[grepl("sp_RID", posterior$ID), 1:3],3)[45,]) * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)
npv_RID <- round((round(posterior[grepl("sp_RID", posterior$ID), 1:3],3)[45,] * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]) / ((1 - round(posterior[grepl("se_RID", posterior$ID), 1:3],3)[45,]) * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + round(posterior[grepl("sp_RID", posterior$ID), 1:3],3)[45,] * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)

# PPV and NPV for IR
round(posterior[grepl("se_IR", posterior$ID), 1:3],3)[66,]
round(posterior[grepl("sp_IR", posterior$ID), 1:3],3)[66,]
round(posterior[grepl("P", posterior$ID),1:3],2)[1,]
round(posterior[grepl("P", posterior$ID),1:3],2)[2,]


ppv_IR <- round((round(posterior[grepl("se_IR", posterior$ID), 1:3],3)[66,] * round(posterior[grepl("P", posterior$ID),1:3],2)[1,]) / (round(posterior[grepl("se_IR", posterior$ID), 1:3],3)[66,] * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + (1 - round(posterior[grepl("sp_IR", posterior$ID), 1:3],3)[66,]) * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)
npv_IR <- round((round(posterior[grepl("sp_IR", posterior$ID), 1:3],3)[66,] * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]) / ((1 - round(posterior[grepl("se_IR", posterior$ID), 1:3],3)[66,]) * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + round(posterior[grepl("sp_IR", posterior$ID), 1:3],3)[66,] * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)
npv_IR

# PPV and NPV for dBr
round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,]
round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3)[39,]
round(posterior[grepl("P", posterior$ID),1:3],2)[1,]
round(posterior[grepl("P", posterior$ID),1:3],2)[2,]


ppv_dBr <- round((round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,] * round(posterior[grepl("P", posterior$ID),1:3],2)[1,]) / (round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,] * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + (1 - round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3))[66,] * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)
npv_dBr <- round((round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3)[39,] * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]) / ((1 - round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,]) * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3)[66,] * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)

# 2 tests RID-TIR
# Series combination (AND rule)
se_RID_TIR_series <- round(posterior[grepl("se_RID", posterior$ID), 1:3],3)[45,] * round(posterior[grepl("se_IR", posterior$ID), 1:3],3)[66,]
sp_RID_TIR_series <- 1 - (1 - round(posterior[grepl("sp_RID", posterior$ID), 1:3],3)[45,]) * (1 - round(posterior[grepl("sp_IR", posterior$ID), 1:3],3)[66,])

ppv_RID_TIR_series <- round((se_RID_TIR_series * round(posterior[grepl("P", posterior$ID),1:3],2)[1,]) / (se_RID_TIR_series * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + (1 - sp_RID_TIR_series) * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)
npv_RID_TIR_series <- round((sp_RID_TIR_series * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]) / ((1 - se_RID_TIR_series) * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + sp_RID_TIR_series * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)

# Parallel combination (OR rule)
se_RID_TIR_parallel <- 1 - (1 - round(posterior[grepl("se_RID", posterior$ID), 1:3],3)[45,]) * (1 - round(posterior[grepl("se_IR", posterior$ID), 1:3],3)[66,])
sp_RID_TIR_parallel <- round(posterior[grepl("sp_RID", posterior$ID), 1:3],3)[45,] * round(posterior[grepl("sp_IR", posterior$ID), 1:3],3)[66,]

ppv_RID_TIR_parallel <- round((se_RID_TIR_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[1,]) / (se_RID_TIR_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + (1 - sp_RID_TIR_parallel) * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)
npv_RID_TIR_parallel <- round((sp_RID_TIR_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]) / ((1 - se_RID_TIR_parallel) * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + sp_RID_TIR_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)



# 2 tests TIR-dBr
round(posterior[grepl("se_IR", posterior$ID), 1:3],3)[66,]
round(posterior[grepl("sp_IR", posterior$ID), 1:3],3)[66,]

round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,]
round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3)[39,]

round(posterior[grepl("P", posterior$ID),1:3],2)[1,]
round(posterior[grepl("P", posterior$ID),1:3],2)[2,]

# Series combination (AND rule) for TIR and dBr
se_TIR_dBr_series <- round(posterior[grepl("se_IR", posterior$ID), 1:3],3)[66,] * round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,]
sp_TIR_dBr_series <- 1 - (1 - round(posterior[grepl("sp_IR", posterior$ID), 1:3],3)[66,]) * (1 - round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3)[39,])

ppv_TIR_dBr_series <- round((se_TIR_dBr_series * round(posterior[grepl("P", posterior$ID),1:3],2)[1,]) / (se_TIR_dBr_series * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + (1 - sp_TIR_dBr_series) * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)
npv_TIR_dBr_series <- round((sp_TIR_dBr_series * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]) / ((1 - se_TIR_dBr_series) * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + sp_TIR_dBr_series * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)


# Parallel combination (OR rule) for TIR and dBr
se_TIR_dBr_parallel <- 1 - (1 - round(posterior[grepl("se_IR", posterior$ID), 1:3],3)[66,]) * (1 - round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,])
sp_TIR_dBr_parallel <- round(posterior[grepl("sp_IR", posterior$ID), 1:3],3)[66,] * round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3)[39,]

ppv_TIR_dBr_parallel <- round((se_TIR_dBr_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[1,]) / (se_TIR_dBr_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + (1 - sp_TIR_dBr_parallel) * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)
npv_TIR_dBr_parallel <- round((sp_TIR_dBr_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]) / ((1 - se_TIR_dBr_parallel) * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + sp_TIR_dBr_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)



# 2 test RID-dBr
# Series combination (AND rule) for RID and dBr

round(posterior[grepl("se_RID", posterior$ID), 1:3],3)[45,]
round(posterior[grepl("sp_RID", posterior$ID), 1:3],3)[45,]

round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,]
round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3)[39,]

round(posterior[grepl("P", posterior$ID),1:3],2)[1,]
round(posterior[grepl("P", posterior$ID),1:3],2)[2,]

se_RID_dBr_series <- round(posterior[grepl("se_RID", posterior$ID), 1:3],3)[45,] * round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,]
sp_RID_dBr_series <- 1 - (1 - round(posterior[grepl("sp_RID", posterior$ID), 1:3],3)[45,]) * (1 - round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3)[39,])

ppv_RID_dBr_series <- round((se_RID_dBr_series * round(posterior[grepl("P", posterior$ID),1:3],2)[1,]) / (se_RID_dBr_series * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + (1 - sp_RID_dBr_series) * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)
npv_RID_dBr_series <- round((sp_RID_dBr_series * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]) / ((1 - se_RID_dBr_series) * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + sp_RID_dBr_series * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)



# Parallel combination (OR rule) for RID and dBr
se_RID_dBr_parallel <- 1 - (1 - round(posterior[grepl("se_RID", posterior$ID), 1:3],3)[45,]) * (1 - round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,])
sp_RID_dBr_parallel <- round(posterior[grepl("sp_RID", posterior$ID), 1:3],3)[45,] * round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3)[39,]

ppv_RID_dBr_parallel <- round((se_RID_dBr_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[1,]) / (se_RID_dBr_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + (1 - sp_RID_dBr_parallel) * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)
npv_RID_dBr_parallel <- round((sp_RID_dBr_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]) / ((1 - se_RID_dBr_parallel) * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + sp_RID_dBr_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)

# All 3 tests
# Series interpretation

round(posterior[grepl("se_RID", posterior$ID), 1:3],3)[45,]
round(posterior[grepl("sp_RID", posterior$ID), 1:3],3)[45,]

round(posterior[grepl("se_IR", posterior$ID), 1:3],3)[66,]
round(posterior[grepl("sp_IR", posterior$ID), 1:3],3)[66,]

round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,]
round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3)[39,]

round(posterior[grepl("P", posterior$ID),1:3],2)[1,]
round(posterior[grepl("P", posterior$ID),1:3],2)[2,]


se_all_series <- round(posterior[grepl("se_RID", posterior$ID), 1:3],3)[45,] * round(posterior[grepl("se_IR", posterior$ID), 1:3],3)[66,] * round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,]
sp_all_series <- 1 - (1 - round(posterior[grepl("sp_RID", posterior$ID), 1:3],3)[45,]) * (1 - round(posterior[grepl("sp_IR", posterior$ID), 1:3],3)[66,]) * (1 - round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3)[39,])

ppv_all_series <- round((se_all_series * round(posterior[grepl("P", posterior$ID),1:3],2)[1,]) / (se_all_series * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + (1 - sp_all_series) * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)
npv_all_series <- round((sp_all_series * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]) / ((1 - se_all_series) * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + sp_all_series * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)

# Parallel
se_all_parallel <- 1 - (1 - round(posterior[grepl("se_RID", posterior$ID), 1:3],3)[45,]) * (1 - round(posterior[grepl("se_IR", posterior$ID), 1:3],3)[66,]) * (1 - round(posterior[grepl("se_dBr", posterior$ID), 1:3],3)[39,])
sp_all_parallel <- round(posterior[grepl("sp_RID", posterior$ID), 1:3],3)[45,] * round(posterior[grepl("sp_IR", posterior$ID), 1:3],3)[66,] * round(posterior[grepl("sp_dBr", posterior$ID), 1:3],3)[39,]

ppv_all_parallel <- round((se_all_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[1,]) / (se_all_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + (1 - sp_all_parallel) * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)
npv_all_parallel <- round((sp_all_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]) / ((1 - se_all_parallel) * round(posterior[grepl("P", posterior$ID),1:3],2)[1,] + sp_all_parallel * round(posterior[grepl("P", posterior$ID),1:3],2)[2,]),2)




# Figure for RID
# Parameters
mean1 <- exp(posterior[grepl("lambda_RID", posterior$ID),2][1]) # high-quality
sd1 <- (sqrt((exp(1/posterior[grepl("gamma_RID", posterior$ID),2][1]) - 1) * exp(2*posterior[grepl("lambda_RID", posterior$ID),2][1] + 1/posterior[grepl("gamma_RID", posterior$ID),2][1])))

mean2 <- exp(posterior[grepl("lambda_RID", posterior$ID),2][2])  # low-quality
sd2 <- (sqrt((exp(1/posterior[grepl("gamma_RID", posterior$ID),2][2]) - 1) * exp(2*posterior[grepl("lambda_RID", posterior$ID),2][2] + 1/posterior[grepl("gamma_RID", posterior$ID),2][2])))

# Generating data
x <- seq(min(mean1 - 3*sd1, mean2 - 3*sd2), max(mean1 + 3*sd1, mean2 + 3*sd2), length.out = 1000)
y1 <- dnorm(x, mean = mean1, sd = sd1)
y2 <- dnorm(x, mean = mean2, sd = sd2)
rid_den <- data.frame(x, y1, y2)

# Plotting
rid_plot <- ggplot(rid_den) + 
  geom_line(aes(x = x, y = y1), colour = "red", size = 1) + 
  geom_line(aes(x = x, y = y2), colour = "blue", size = 1) + 
  labs(title = "(A) Density estimates for RID", x = "RID values (g/L)", y = "Probability density") +
  theme_minimal() +
  scale_colour_manual(values = c("Distribution 1" = "red", "Distribution 2" = "blue")) +
  geom_vline(xintercept = 34.15, color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = 34.15, y = max(rid_den$y1, rid_den$y2) * 0.9, 
           label = "34.15 g/L", color = "black", angle = 0, vjust = -0.5, hjust = -0.1)

rid_plot
png(filename = "Figure1A.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(rid_plot)
dev.off()

rid_plot

# ROC curve for RID
se_rid_l_c <- posterior[grepl("se_RID", posterior$ID),]$Lower95
se_rid_m_c <- posterior[grepl("se_RID", posterior$ID),]$Median
se_rid_u_c <- posterior[grepl("se_RID", posterior$ID),]$Upper95

sp_rid_l_c <- posterior[grepl("sp_RID", posterior$ID),]$Lower95
sp_rid_m_c <- posterior[grepl("sp_RID", posterior$ID),]$Median
sp_rid_u_c <- posterior[grepl("sp_RID", posterior$ID),]$Upper95

fpr_rid_l_c <- 1 - sp_rid_l_c
fpr_rid_m_c <- 1 - sp_rid_m_c
fpr_rid_u_c <- 1 - sp_rid_u_c


roc_data <- data.frame(FPR = fpr_rid_m_c,
                       TPR = se_rid_m_c,
                       FPR_lower = fpr_rid_l_c,
                       TPR_lower = se_rid_l_c,
                       FPR_upper = fpr_rid_u_c,
                       TPR_upper = se_rid_u_c)

roc_RID <- ggplot() +
  geom_line(data = roc_data, aes(x = FPR, y = TPR), color = "blue") +
  geom_line(data = roc_data, aes(x = FPR_lower, y = TPR_lower), color = "blue", linetype = "dashed") +
  geom_line(data = roc_data, aes(x = FPR_upper, y = TPR_upper), color = "blue", linetype = "dashed") +
  geom_ribbon(data = roc_data, aes(x = FPR, ymin = TPR_lower, ymax = TPR_upper), alpha = 0.2, fill = "blue") +
  labs(x = "False Positive Rate", y = "True Positive Rate", title = "(A) ROC Curve for RID in colostrum samples with 95% CI") +
  theme_minimal() +
  geom_abline(linetype = "dotted", color = "red") + # No-discrimination line 
  annotate("text", x = 0.6, y = 0.1,
           label = "AUC = 0.92 (0.88 - 0.96)",
           color = "black", size = 5, hjust = 0)


png(filename = "Figure_2A.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(roc_RID)
dev.off()

roc_RID

# Figure for TIR
# Parameters
mean1 <- exp(posterior[grepl("lambda_IR", posterior$ID),2][1]) - 100 # high-quality
sd1 <- (sqrt((exp(1/posterior[grepl("gamma_IR", posterior$ID),2][1]) - 1) * exp(2*posterior[grepl("lambda_IR", posterior$ID),2][1] + 1/posterior[grepl("gamma_IR", posterior$ID),2][1])))

mean2 <- exp(posterior[grepl("lambda_IR", posterior$ID),2][2]) - 100  # low-quality
sd2 <- (sqrt((exp(1/posterior[grepl("gamma_IR", posterior$ID),2][2]) - 1) * exp(2*posterior[grepl("lambda_IR", posterior$ID),2][2] + 1/posterior[grepl("gamma_IR", posterior$ID),2][2])))

# Generating data
x <- seq(min(mean1 - 3*sd1, mean2 - 3*sd2), max(mean1 + 3*sd1, mean2 + 3*sd2), length.out = 1000)
y1 <- dnorm(x, mean = mean1, sd = sd1)
y2 <- dnorm(x, mean = mean2, sd = sd2)
ir_den <- data.frame(x, y1, y2)

# Plotting
tir_plot <- ggplot(ir_den) + 
  geom_line(aes(x = x, y = y1), colour = "red", size = 1) + 
  geom_line(aes(x = x, y = y2), colour = "blue", size = 1) + 
  labs(title = "(B) Density estimates for TIR", x = "TIR values (g/L)", y = "Probability density") +
  theme_minimal() +
  scale_colour_manual(values = c("Distribution 1" = "red", "Distribution 2" = "blue")) +
  geom_vline(xintercept = 22.74, color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = 22.74, y = max(rid_den$y1, rid_den$y2) * 0.9, 
           label = "22.74 g/L", color = "black", angle = 0, vjust = +0.5, hjust = -0.1)
tir_plot

png(filename = "Figure1B.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(tir_plot)
dev.off()

tir_plot

# ROC curve for TIR
se_tir_l_c <- posterior[grepl("se_IR", posterior$ID),]$Lower95
se_tir_m_c <- posterior[grepl("se_IR", posterior$ID),]$Median
se_tir_u_c <- posterior[grepl("se_IR", posterior$ID),]$Upper95

sp_tir_l_c <- posterior[grepl("sp_IR", posterior$ID),]$Lower95
sp_tir_m_c <- posterior[grepl("sp_IR", posterior$ID),]$Median
sp_tir_u_c <- posterior[grepl("sp_IR", posterior$ID),]$Upper95

fpr_tir_l_c <- 1 - sp_tir_l_c
fpr_tir_m_c <- 1 - sp_tir_m_c
fpr_tir_u_c <- 1 - sp_tir_u_c

# Combine into a data frame for ggplot
roc_data <- data.frame(FPR = fpr_tir_m_c,
                       TPR = se_tir_m_c,
                       FPR_lower = fpr_tir_l_c,
                       TPR_lower = se_tir_l_c,
                       FPR_upper = fpr_tir_u_c,
                       TPR_upper = se_tir_u_c)

roc_TIR <- ggplot() +
  geom_line(data = roc_data, aes(x = FPR, y = TPR), color = "blue") +
  geom_line(data = roc_data, aes(x = FPR_lower, y = TPR_lower), color = "blue", linetype = "dashed") +
  geom_line(data = roc_data, aes(x = FPR_upper, y = TPR_upper), color = "blue", linetype = "dashed") +
  geom_ribbon(data = roc_data, aes(x = FPR, ymin = TPR_lower, ymax = TPR_upper), alpha = 0.2, fill = "blue") +
  labs(x = "False Positive Rate", y = "True Positive Rate", title = "(B) ROC Curve for TIR in colostrum samples with 95% CI") +
  theme_minimal() +
  geom_abline(linetype = "dotted", color = "red") + # No-discrimination line
  annotate("text", x = 0.6, y = 0.1,
           label = "AUC = 0.82 (0.76 - 0.88)",
           color = "black", size = 5, hjust = 0)


png(filename = "Figure_2B.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(roc_TIR)
dev.off()

roc_TIR

# Figure for dBrix
# Parameters
mean1 <- exp(posterior[grepl("lambda_dBr", posterior$ID),2][1]) # high-quality
sd1 <- (sqrt((exp(1/posterior[grepl("gamma_dBr", posterior$ID),2][1]) - 1) * exp(2*posterior[grepl("lambda_dBr", posterior$ID),2][1] + 1/posterior[grepl("gamma_dBr", posterior$ID),2][1])))

mean2 <- exp(posterior[grepl("lambda_dBr", posterior$ID),2][2])  # low-quality
sd2 <- (sqrt((exp(1/posterior[grepl("gamma_dBr", posterior$ID),2][2]) - 1) * exp(2*posterior[grepl("lambda_dBr", posterior$ID),2][2] + 1/posterior[grepl("gamma_dBr", posterior$ID),2][2])))

# Generating data
x <- seq(min(mean1 - 3*sd1, mean2 - 3*sd2), max(mean1 + 3*sd1, mean2 + 3*sd2), length.out = 1000)
y1 <- dnorm(x, mean = mean1, sd = sd1)
y2 <- dnorm(x, mean = mean2, sd = sd2)
dBr_den <- data.frame(x, y1, y2)


dBr_plot <- ggplot(dBr_den) + 
  geom_line(aes(x = x, y = y1), colour = "red", size = 1) + 
  geom_line(aes(x = x, y = y2), colour = "blue", size = 1) + 
  labs(title = "(C) Density estimates for dBrix", x = "dBr values (% Brix)", y = "Probability density") +
  theme_minimal() +
  scale_colour_manual(values = c("Distribution 1" = "red", "Distribution 2" = "blue")) +
  geom_vline(xintercept = 19.62, color = "black", linetype = "dashed", size = 1)  +
  annotate("text", x = 19.62, y = max(dBr_den$y1, dBr_den$y2) * 0.9, 
           label = "19.62 % Brix", color = "black", angle = 0, vjust = -0.5, hjust = -0.1)

png(filename = "Figure1C.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(dBr_plot)
dev.off()


dBr_plot

# ROC curve for dBr colostrum
se_dBr_l_c <- posterior[grepl("se_dBr", posterior$ID),]$Lower95
se_dBr_m_c <- posterior[grepl("se_dBr", posterior$ID),]$Median
se_dBr_u_c <- posterior[grepl("se_dBr", posterior$ID),]$Upper95

sp_dBr_l_c <- posterior[grepl("sp_dBr", posterior$ID),]$Lower95
sp_dBr_m_c <- posterior[grepl("sp_dBr", posterior$ID),]$Median
sp_dBr_u_c <- posterior[grepl("sp_dBr", posterior$ID),]$Upper95

fpr_dBr_l_c <- 1 - sp_dBr_l_c
fpr_dBr_m_c <- 1 - sp_dBr_m_c
fpr_dBr_u_c <- 1 - sp_dBr_u_c

# Combine into a data frame for ggplot
roc_data <- data.frame(FPR = fpr_dBr_m_c,
                       TPR = se_dBr_m_c,
                       FPR_lower = fpr_dBr_l_c,
                       TPR_lower = se_dBr_l_c,
                       FPR_upper = fpr_dBr_u_c,
                       TPR_upper = se_dBr_u_c)

roc_dBr <- ggplot() +
  geom_line(data = roc_data, aes(x = FPR, y = TPR), color = "blue") +
  geom_line(data = roc_data, aes(x = FPR_lower, y = TPR_lower), color = "blue", linetype = "dashed") +
  geom_line(data = roc_data, aes(x = FPR_upper, y = TPR_upper), color = "blue", linetype = "dashed") +
  geom_ribbon(data = roc_data, aes(x = FPR, ymin = TPR_lower, ymax = TPR_upper), alpha = 0.2, fill = "blue") +
  labs(x = "False Positive Rate", y = "True Positive Rate", title = "(C) ROC Curve for dBrix in colostrum samples with 95% CI") +
  theme_minimal() +
  geom_abline(linetype = "dotted", color = "red") + # No-discrimination line
  annotate("text", x = 0.6, y = 0.1,
           label = "AUC = 0.94 (0.91 - 0.97)",
           color = "black", size = 5, hjust = 0)


png(filename = "Figure_3C.jpeg", width = 3500, height = 1600, units = "px", res = 300, bg = "transparent")
print(roc_dBr)
dev.off()

roc_dBr

library(gridExtra)
png(filename = "Figure_1_combined.jpeg", width = 3500, height = 4800, units = "px", res = 300, bg = "transparent")
grid.arrange(rid_plot, tir_plot, dBr_plot, ncol = 1)
dev.off()

library(gridExtra)
png(filename = "Figure_2_combined.jpeg", width = 3500, height = 4800, units = "px", res = 300, bg = "transparent")
grid.arrange(roc_RID, roc_TIR, roc_dBr, ncol = 1)
dev.off()


