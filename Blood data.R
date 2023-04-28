Blood_data <- read.csv("data/Human Biology/Blood data.csv")
Blood_data <- subset(Blood_data, select = c("ID", "cohort", "blood_type", "rhesus", "sex", "nationality"))
library("gapminder")
library(hexbin)
library(tidyverse)

Blood_data[Blood_data == ""] <- NA

#graphing rhesus against 
Blood_data %>% 
  filter(!is.na(rhesus),
         sex == "M") %>% 
  ggplot(mapping = aes(`blood_type`, fill = blood_type)) +
  geom_bar() +
  theme_bw() + 
  facet_wrap( ~ rhesus) +
  labs(x = "Blood type",
       y = "Frequency",
       fill = "Blood types") +
  scale_fill_manual(values = c("#005724", "#7FC6BC", "#F9DC5C", "#661F4E"))

Blood_data %>% 
  filter(!is.na(rhesus),
         sex == "F") %>% 
  ggplot(mapping = aes(`blood_type`, fill = blood_type)) +
  geom_bar() +
  theme_bw() + 
  facet_wrap( ~ rhesus) +
  labs(x = "Blood type",
       y = "Frequency",
       fill = "Blood types") +
  scale_fill_manual(values = c("#005724", "#7FC6BC", "#F9DC5C", "#661F4E"))

#comparing distribution of blood types of males and females
counts <- Blood_data %>% 
  group_by(sex) %>% 
  count(blood_type) %>% 
  mutate(ratios = ifelse(sex == "M", n/244, n/469)) %>% 
  mutate(percentages = round(ratios * 100, 1))
  
counts %>% 
  ggplot(mapping = aes(x = "" , y = percentages, fill = blood_type)) +
  geom_bar (stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_bw() + 
  facet_wrap( ~ sex) +
  labs(fill = "Blood types") +
  scale_fill_manual(values = c("#005724", "#7FC6BC", "#F9DC5C", "#661F4E"))

#comparing distribution of Rhesus factor in males and females
counts_rhesus <- Blood_data %>% 
  filter(!is.na(rhesus)) %>% 
  group_by(sex) %>% 
  count(rhesus) %>% 
  mutate(ratios = ifelse(sex == "M", n/242, n/462)) %>% 
  mutate(percentages = round(ratios * 100, 1))

counts_rhesus %>% 
  ggplot(mapping = aes(x = "" , y = percentages, fill = rhesus)) +
  geom_bar (stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_bw() + 
  facet_wrap( ~ sex) +
  labs(fill = "Blood types") +
  scale_fill_manual(labels = c("negative", "positive")) +
  scale_fill_manual(values = c("#7FC6BC", "#661F4E" ))

#contingency table and chi square of independence test blood type and sex
blood_sex <- c(Blood_data$sex)
blood_type <- c(Blood_data$blood_type)
blood_contingency <- table(blood_sex, blood_type)
Sigtest_t_sex <- chisq.test(blood_contingency)

#contingency table and chi square of independence test rhesus and sex
blood_rhesus <- c(Blood_data$rhesus)
blood_contingency_rhesus <- table(blood_sex, blood_rhesus)
Sigtest_r_sex <- chisq.test(blood_contingency_rhesus)

#investigating the difference in dutch population and students
overall <- Blood_data %>% 
  count(blood_type) %>% 
  mutate(ratios = n/713) %>% 
  mutate(percentages = round(ratios * 100, 1))

overall_r <- Blood_data %>% 
  filter(!is.na(rhesus)) %>% 
  count(rhesus) %>% 
  mutate(ratios = n/704) %>% 
  mutate(percentages = round(ratios * 100, 1))

#data frames with the information about the dutch population (sourced from sanquin, the dutch blood bank)
Dpop_overall_t <- data.frame(blood_type = c("A", "AB", "B", "O"), percentages = c(43, 3, 9, 45)) 
Dpop_overall_r <- data.frame(rhesus = c("+", "-"), percentages = c(85, 15)) 

Dpop_overall_t %>% 
  ggplot(mapping = aes(x = "" , y = percentages, fill = blood_type)) +
  geom_bar (stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_bw() + 
  labs(fill = "Blood types") +
  scale_fill_manual(values = c("#005724", "#7FC6BC", "#F9DC5C", "#661F4E"))

overall %>% 
  ggplot(mapping = aes(x = "" , y = percentages, fill = blood_type)) +
  geom_bar (stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_bw() + 
  labs(fill = "Blood types") +
  scale_fill_manual(values = c("#005724", "#7FC6BC", "#F9DC5C", "#661F4E"))

Dpop_overall_r %>% 
  ggplot(mapping = aes(x = "" , y = percentages, fill = rhesus)) +
  geom_bar (stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_bw() + 
  labs(fill = "Blood types") +
  scale_fill_manual(labels = c("negative", "positive")) +
  scale_fill_manual(values = c("#7FC6BC", "#661F4E" ))

overall_r %>% 
  ggplot(mapping = aes(x = "" , y = percentages, fill = rhesus)) +
  geom_bar (stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_bw() + 
  labs(fill = "Blood types") +
  scale_fill_manual(labels = c("negative", "positive")) +
  scale_fill_manual(values = c("#7FC6BC", "#661F4E" ))

chisq.test(Dpop_overall_t$percentages, overall$percentages)
binom.test(Dpop_overall_r$percentages, overall_r$per centages)

#creating separate data frames for international and non international biology
Blood_data_worse <- Blood_data %>% 
  filter(cohort < "2018-2019")

Blood_data_better <- Blood_data %>% 
  filter(cohort >= "2018-2019")


#Graphs for English taught curriculum and dutch taught curriculum

#pie charts with the distribution of blood types
English_counts <- Blood_data_better%>% 
  group_by(blood_type) %>% 
  count(blood_type) %>% 
  mutate(ratios = n/194) %>% 
  mutate(percentages = round(ratios * 100, 1))

English_counts %>% 
  ggplot(mapping = aes(x = "" , y = percentages, fill = blood_type)) +
  geom_bar (stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_bw() + 
  labs(fill = "Blood types") +
  scale_fill_manual(values = c("#005724", "#7FC6BC", "#F9DC5C", "#661F4E"))

Dutch_counts <- Blood_data_worse%>% 
  group_by(blood_type) %>% 
  count(blood_type) %>% 
  mutate(ratios = n/519) %>% 
  mutate(percentages = round(ratios * 100, 1))

Dutch_counts %>% 
  ggplot(mapping = aes(x = "" , y = percentages, fill = blood_type)) +
  geom_bar (stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_bw() + 
  labs(fill = "Blood types") +
  scale_fill_manual(values = c("#005724", "#7FC6BC", "#F9DC5C", "#661F4E"))

#pie charts with the distribution of Rhesus factor
Dutch_counts_rhesus <- Blood_data_worse%>% 
  group_by(rhesus) %>% 
  count(rhesus) %>% 
  mutate(ratios = n/519) %>% 
  mutate(percentages = round(ratios * 100, 1))

Dutch_counts_rhesus %>% 
  ggplot(mapping = aes(x = "" , y = percentages, fill = rhesus)) +
  geom_bar (stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_bw() + 
  labs(fill = "Blood types") +
  scale_fill_manual(labels = c("negative", "positive")) +
  scale_fill_manual(values = c("#7FC6BC", "#661F4E" ))

English_counts_rhesus <- Blood_data_better%>% 
  filter(!is.na(rhesus)) %>% 
  group_by(rhesus) %>% 
  count(rhesus) %>% 
  mutate(ratios = n/185) %>% 
  mutate(percentages = round(ratios * 100, 1))

English_counts_rhesus %>% 
  ggplot(mapping = aes(x = "" , y = percentages, fill = rhesus)) +
  geom_bar (stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_bw() + 
  labs(fill = "Blood types") +
  scale_fill_manual(labels = c("negative", "positive")) +
  scale_fill_manual(values = c("#7FC6BC", "#661F4E" ))

#bar charts with the distributions
Blood_data_worse %>% 
  ggplot(mapping = aes(`blood_type`, fill = blood_type)) +
  geom_bar() +
  theme_bw() + 
  labs(x = "Blood type",
       y = "Frequency",
       fill = "Blood types") +
  scale_fill_manual(values = c("#005724", "#7FC6BC", "#F9DC5C", "#661F4E"))

Blood_data_better %>% 
  ggplot(mapping = aes(`blood_type`, fill = blood_type)) +
  geom_bar() +
  theme_bw() + 
  labs(x = "Blood type",
       y = "Frequency",
       fill = "Blood types") +
  scale_fill_manual(values = c("#005724", "#7FC6BC", "#F9DC5C", "#661F4E"))

#chi square test for blood type and Rhesus factor

INTvD <- chisq.test(Dutch_counts$n, English_counts$n)

INTvDr <- binom.test(English_counts_rhesus$n, English_counts_rhesus$n)

