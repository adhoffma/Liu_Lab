library(dplyr)
library(ggplot2)
library(forcats)

ovh_data <- read.csv("Fig ex1B.csv")

summ_ovh_data <- ovh_data %>% group_by(Patient_Type, Sample_Name, Stain) %>% summarise(Mean_488_Count = mean(Count_488)) 

norm_ovh_data <- summ_ovh_data %>% spread(Stain, Mean_488_Count) %>% 
  filter(Patient_Type != "Group_6") %>%
  rowwise() %>%
  mutate(Norm_ACE2_Counts = (`ACE2-AF488`/`IgG2a-AF488`) ) %>%
  mutate(Patient_Class = ifelse(Patient_Type == "Group_1", "Sero-Negative", 
                                ifelse(Patient_Type == "Acute", "Acute", 
                                       ifelse(Patient_Type == "Group_2", "Outpatient", 
                                              ifelse(Patient_Type == "Group_3", "Outpatient", 
                                                     ifelse(Patient_Type == "Group_4", "Hospitalized",
                                                            ifelse(Patient_Type == "Group_5", "Hospitalized", "Unknown")))))))



ovh_cols <- c("Outpatient" = "#00BA38", "Hospitalized" = "#00BA38")
ovh_shapes <- c("Outpatient" = 17, "Hospitalized" = 16)

ggplot(data = norm_ovh_data, aes(x =  fct_relevel(Patient_Class, "Hospitalized", "Outpatient"), 
                                 y = Norm_ACE2_Counts,
                                 color = fct_relevel(Patient_Class, "Hospitalized", "Outpatient"))) + 
  geom_point(aes(size = 0.8, shape = fct_relevel(Patient_Class, "Hospitalized", "Outpatient"))) +
  scale_shape_manual(values = ovh_shapes) +
  scale_color_manual(values = ovh_cols) +
  stat_summary(geom="errorbar", color="red", width=0.4) +
  stat_summary(fun=mean, geom="point", color="red") +
  theme_bw() +
  xlab("Patient type") +
  ylab("Normalized ACE2+ counts") +
  theme(legend.position = "none")

t.test(Norm_ACE2_Counts ~ Patient_Class, data = norm_ovh_data, alternative = "greater")
