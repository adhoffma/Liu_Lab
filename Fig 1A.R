library(dplyr)
library(ggplot2)
library(forcats)
library(openxlsx)

new_ace2_data <- read.csv("Fig1A.csv")

summ_ace2_data <- new_ace2_data %>% group_by(Patient_Type, Sample_Name, Stain) %>% summarise(Mean_488_Count = mean(X488_Count))

norm_ace2_data <- summ_ace2_data %>% spread(Stain, Mean_488_Count) %>% 
  filter(Patient_Type != "Group_6") %>%
  rowwise() %>%
  mutate(Norm_ACE2_Counts = (`ACE2-AF488`/`IgG2a-AF488`) ) %>%
  mutate(Patient_Class = ifelse(Patient_Type == "Group_1", "Sero-Negative", ifelse(Patient_Type == "Acute", "Acute", "Convalescent")))

ace2_cols <- c("Sero-Negative" = "#619CFF", "Acute" = "#064500", "Convalescent" = "#00BA38")

ggplot(data = norm_ace2_data, aes(x =  fct_relevel(Patient_Class, "Sero-Negative", "Acute", "Convalescent"), 
                                  y = Norm_ACE2_Counts,
                                  color = fct_relevel(Patient_Class, "Sero-Negative", "Acute", "Convalescent"))) + 
  geom_point(aes(size = 0.8)) +
  scale_color_manual(values = ace2_cols) +
  stat_summary(geom="errorbar", color="red", width=0.4) +
  stat_summary(fun=mean, geom="point", color="red") +
  theme_bw() +
  xlab("Patient type") +
  ylab("Normalized ACE2+ counts") +
  theme(legend.position = "none")

t.test(Norm_ACE2_Counts ~ Patient_Class, data = norm_ace2_data %>% filter(Patient_Class %in% c("Acute", "Sero-Negative")), alternative = "greater")
t.test(Norm_ACE2_Counts ~ Patient_Class, data = norm_ace2_data %>% filter(Patient_Class %in% c("Acute", "Convalescent")), alternative = "greater")
t.test(Norm_ACE2_Counts ~ Patient_Class, data = norm_ace2_data %>% filter(Patient_Class %in% c("Convalescent", "Sero-Negative")), alternative = "greater")

norm_calc <- norm_ace2_data %>% group_by(Patient_Class) %>% summarise(n = n(), 
                                                         mean = mean(Norm_ACE2_Counts), 
                                                         sd = sd(Norm_ACE2_Counts), 
                                                         se = (sd(Norm_ACE2_Counts))/(sqrt(n())))

summary_ace2_data <- summ_ace2_data %>% mutate(Patient_Class = ifelse(Patient_Type == "Group_1", "Sero-Negative", ifelse(Patient_Type == "Acute", "Acute", "Convalescent")))

summ_calc <- summary_ace2_data %>% 
  group_by(Stain, Patient_Class) %>% 
  summarise(n = n(), 
            mean = mean(Mean_488_Count),  
            sd = sd(Mean_488_Count), 
            se = (sd(Mean_488_Count))/(sqrt(n())))

raw_ace2_data <- new_ace2_data %>% mutate(Patient_Class = ifelse(Patient_Type == "Group_1", "Sero-Negative", ifelse(Patient_Type == "Acute", "Acute", "Convalescent")))

raw_calc <- raw_ace2_data %>% group_by(Stain, Patient_Class) %>% summarise(n = n(), 
                                                        mean = mean(X488_Count),  
                                                        sd = sd(X488_Count), 
                                                        se = (sd(X488_Count))/(sqrt(n())))

write.xlsx(list(norm = norm_calc, raw = raw_calc, summ = summ_calc), "Fig1A_calcs.xlsx")
