library(dplyr)
library(ggplot2)
library(forcats)

mod_igg_data <- read.csv("Fig ex6A.csv")

mod_igg_data <- mod_igg_data %>% 
  filter(Patient_Type != "Group 6") %>%
  mutate(Patient_Class2 = ifelse(Patient_Type == "Group 1", "Sero-Negative", 
                                 ifelse(Patient_Type == "Acute", "Acute", 
                                        ifelse(Patient_Type == "Group 2", "Asymptomatic", 
                                               ifelse(Patient_Type == "Group 3", "Outpatient", 
                                                      ifelse(Patient_Type == "Group 4", "Inpatient",
                                                             ifelse(Patient_Type == "Group 5", "ICU", "Unknown"))))))) %>%
  mutate(Patient_Class = ifelse(Patient_Type == "Group 1", "Sero-Negative", ifelse(Patient_Type == "Acute", "Acute", "Convalescent")))


ggplot(data = mod_igg_data, aes(x = fct_relevel(Patient_Class, "Sero-Negative", "Acute", "Convalescent"), 
                                y = IgG_Levels, 
                                color = fct_relevel(Patient_Class, "Sero-Negative", "Acute", "Convalescent"))) +
  geom_point(aes(size = 0.8)) + 
  scale_color_manual(values = ace2_cols) +
  scale_y_log10() +
  stat_summary(geom="errorbar", color="red", width=0.4) +
  stat_summary(fun=mean, geom="point", color="red") +
  theme_bw() +
  xlab("Patient type") +
  ylab("IgG Levels") +
  theme(legend.position = "none")

t.test(IgG_Levels ~ Patient_Class, data = mod_igg_data %>% filter(Patient_Class %in% c("Acute", "Sero-Negative")), alternative = "greater")
t.test(IgG_Levels ~ Patient_Class, data = mod_igg_data %>% filter(Patient_Class %in% c("Acute", "Convalescent")), alternative = "greater")
t.test(IgG_Levels ~ Patient_Class, data = mod_igg_data %>% filter(Patient_Class %in% c("Convalescent", "Sero-Negative")), alternative = "greater")

