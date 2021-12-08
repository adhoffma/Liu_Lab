library("relaimpo")
library("ggplot2")
library("dplyr")

reg_data <- read.csv("Fig ex6BC.csv")

colnames(reg_data) <- c("Patient_ID", 
                        "Neutralization_80ul_NormPct",
                        "RBD_IgG_Rep1",
                        "RBD_IgG_Rep2",
                        "RBD_IgG_Rep3",
                        "RBD_IgG_Rep4",
                        "Mean_RBD",
                        "ACE2_Counts")

# Model with RBD only
reg_fit_80 <- lm(Neutralization_80ul_NormPct ~ Mean_RBD, data = reg_data)

# Model with RBD and ACE2
reg_fit_80_2 <- lm(Neutralization_80ul_NormPct ~ Mean_RBD + ACE2_Counts, data = reg_data)

# Calculate difference between no-ACE2 model and model with ACE2
anova(reg_fit_80, reg_fit_80_2)

# Calculate relative importance of ACE2 regressor in the model with ACE2 and RBD
calc.relimp(reg_fit_80_2,type=c("lmg","last","first","pratt"), rela=TRUE)

ggplot(reg_data, aes(x = Mean_RBD, y = Neutralization_80ul_NormPct)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  labs(title = paste("Adj R2 = ",signif(summary(reg_fit_80)$adj.r.squared, 3),
                     " P =",signif(summary(reg_fit_80)$coef[2,4], 3))) +
  xlab("RBD-IgG (ug/mL)") +
  ylab("RBD Binding to Cells")

reg_data <- reg_data %>% mutate(fitted_vals = -(Mean_RBD * reg_fit_80_2$coefficients[2]) - (ACE2_Counts * reg_fit_80_2$coefficients[3]))

ggplot(reg_data, aes(x = fitted_vals, y = Neutralization_80ul_NormPct)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  labs(title = paste("Adj R2 = ",signif(summary(reg_fit_80_2)$adj.r.squared, 3),
                     " P =",signif(summary(reg_fit_80_2)$coef[2,4], 3))) +
  xlab("RBD-IgG + ACE2+ EVs") +
  ylab("RBD Binding to Cells")

