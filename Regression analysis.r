library("readr")
library("relaimpo")

reg_data <- read_csv("P:/Covid-19/Covid data for regression.csv")

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