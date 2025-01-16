#################################################
### Applied Health Economics and Econometrics ###
####           Homework Assignment           ####
###  The Role of Income in Franchise Choice   ###
#####       Elena Balzli, 20-451-704        #####
########         17. Januar 2025         ########
#################################################


# working directory
setwd("/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics")

# Packages
install.packages("dplyr")
install.packages("stargazer")
install.packages("kableExtra")
library(stargazer)
library(haven)
library(dplyr)
library(ggplot2)
library(car)
library(tidyr)
library(tidyverse)
library(broom)
library(kableExtra)

# data# tidyversedata
shp24_data <- read.csv("shp24_methods.csv")
View(shp24_data)

###### 2. Data preperation ###### 
data <- shp24_data %>%
  select(pc189, iptotn, pc01, age, sex)
nrow(data)
View(data)

##### 2.1 Cleaning pc189 (Franchise Choice) ##### 
table(data$pc189)
data1 <- data %>%
  mutate(pc189 = ifelse(pc189 %in% c("does not know", "inapplicable", "no answer", "7", ""), NA, pc189))

data1 <- data1 %>%
  mutate(pc189 = case_when(
    pc189 == 1 ~ "300 CHF",
    pc189 %in% c(2, 3) ~ "500 – 1'000 CHF",
    pc189 %in% c(4, 5) ~ "1'500 – 2'000 CHF",
    pc189 %in% c(6) ~ "2'500 CHF"
  )) %>%
  mutate(pc189 = factor(pc189, 
                                levels = c("300 CHF", "500 – 1'000 CHF", 
                                           "1'500 – 2'000 CHF", "2'500 CHF"),
                                ordered = TRUE))

table(data1$pc189)

##### 2.2 Cleaning iptotn (Income) ##### 
table(data$iptotn)
data1 <- data1 %>%
  mutate(iptotn = ifelse(!grepl("^[0-9]", iptotn), NA, as.numeric(iptotn)))


data1 <- data1 %>%
  mutate(iptotn_grouped = case_when(
    iptotn < 20000 ~ "<20'000 CHF",
    iptotn >= 20000 & iptotn < 50000 ~ "20'000–50'000 CHF",
    iptotn >= 50000 & iptotn < 100000 ~ "50'000–100'000 CHF",
    iptotn >= 100000 ~ ">100'000 CHF"
  )) %>%
  mutate(iptotn_grouped = factor(iptotn_grouped, 
                                 levels = c("<20'000 CHF", "20'000–50'000 CHF", 
                                            "50'000–100'000 CHF", ">100'000 CHF"),
                                 ordered = TRUE))
table(data1$iptotn)
table(data1$iptotn_grouped)

##### 2.3 Cleaning pc01 (Health Status) ##### 
table(data$pc01)

data1 <- data1 %>%
  mutate(pc01 = ifelse(pc01 %in% c("inapplicable", "does not know", "no answer"), NA, pc01)) %>%
  mutate(pc01 = factor(pc01, 
                       levels = c("very well", "well", "so, so (average)", 
                                  "not very well", "not well at all"),
                       ordered = TRUE))

table(data1$pc01)

##### 2.4 Cleaning age ##### 
table(data$age)

data1 <- data1 %>%
  mutate(age = ifelse(age %in% c("does not know", "inapplicable", "no answer"), NA, as.numeric(age))) %>%
  mutate(age = case_when(
    age >= 0 & age <= 24 ~ "0 – 24 years",
    age >= 25 & age <= 34 ~ "25 – 34 years",
    age >= 35 & age <= 44 ~ "35 – 44 years",
    age >= 45 & age <= 54 ~ "45 – 54 years",
    age >= 55 & age <= 64 ~ "55 – 64 years",
    age > 65 ~ "65+ years"
  )) %>%
  mutate(age = factor(age, 
                              levels = c("0 – 24 years", "25 – 34 years", 
                                         "35 – 44 years", "45 – 54 years", 
                                         "55 – 64 years", "65+ years"),
                              ordered = TRUE))


table(data1$age)

##### 2.5 Cleaning Gender ##### 
table(data$sex)

data1 <- data1 %>%
  mutate(sex = case_when(
    sex %in% c("inapplicable", "other") ~ NA,
    TRUE ~ sex
  )) %>%
  mutate(sex = factor(sex, levels = c("man", "woman")))

table(data1$sex)

##### 1.6 Filtering complete cases ##### 
colSums(is.na(data1))
nrow(data1)
data1 <- data1 %>%
  filter(!is.na(pc189),!is.na(iptotn),!is.na(pc01),!is.na(age), !is.na(sex))


colSums(is.na(data))
nrow(data1)

######  3. Descriptive analysis ###### 
##### 3.1 Distribution of variables ##### 
#### 3.1.1 Distribution of franchise choices #### 
pc189 <- data1 %>%
  count(pc189) %>%
  mutate(Freq = n / sum(n) * 100)

'ggplot(pc189, aes(x = "", y = Freq, fill = pc189)) +
  geom_bar(stat = "identity", position = "stack", width = 0.3) +
  geom_text(aes(label = paste0(round(Freq, 1), "%")), position = position_stack(vjust = 0.5), size = 3.75) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100), 
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("300 CHF" = "#999966", "500–1000 CHF" = "#CCCC66", "1500–2000 CHF" = "#FFFF99", "2500 CHF" = "#CCCC99")) +
  labs(title = "Franchise Choice Distribution", 
       x = NULL,
       y = NULL,
       fill = "Franchise Choice") +
  theme_light() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  theme(text = element_text(family = "Times New Roman", size = 12)
  )'

pc189_d <- ggplot(pc189, aes(x = pc189, y = n)) +
  geom_bar(stat = "identity", aes(fill = pc189), width = 0.7, fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq), family = "Times New Roman"), 
            vjust = 0.5, size = 4, color = "black", position = position_stack(vjust = 0.5)) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = "'", scientific = FALSE),
    limits = c(0, 20000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Franchise Choice Distribution",
    x = "Franchise Choice",
    y = "Number of Observations"
  ) +
  theme_light() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.position = "none", # Remove legend for simplicity
    axis.text.x = element_text(size = 10, angle = 10, hjust = 0.5, vjust = 0.5)
  )

speicherort <- "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/pc189_d.png"
ggsave(speicherort, plot = pc189_d, width = 5.5, height = 5 , units = "in")


#### 3.1.2 Distribution of net income #### 
iptotn <- data1 %>%
  count(iptotn_grouped) %>%
  mutate(Freq = n / sum(n) * 100)

iptotn_d <- ggplot(iptotn, aes(x = iptotn_grouped, y = n)) +
  geom_bar(stat = "identity", aes(fill = iptotn_grouped), width = 0.7, fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq), family = "Times New Roman"), 
            vjust = 0.5, size = 4, color = "black", position = position_stack(vjust = 0.5)) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = "'", scientific = FALSE),
    limits = c(0, 20000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Net Income Distribution",
    x = "Net Incomes",
    y = "Number of Observations"
  ) +
  theme_light() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.position = "none", # Remove legend for simplicity
    axis.text.x = element_text(size = 10, angle = 10, hjust = 0.5, vjust = 0.5)
  )

speicherort <- "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/iptotn_d.png"
ggsave(speicherort, plot = iptotn_d, width = 5.5, height = 5 , units = "in")

#### 3.1.3 Distribution of health status #### 
pc01 <- data1 %>%
  count(pc01) %>%
  mutate(Freq = n / sum(n) * 100)

pc01_d <- ggplot(pc01, aes(x = pc01, y = n)) +
  geom_bar(stat = "identity", aes(fill = pc01), width = 0.7, fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq), 
                vjust = ifelse(n < 3000, -0.5, 5)),
            size = 4, color = "black", family = "Times New Roman") +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = "'", scientific = FALSE),
    limits = c(0, 32000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Health Status Distribution",
    x = "Health Status",
    y = "Number of Observations"
  ) +
  theme_light() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.position = "none", # Remove legend for simplicity
    axis.text.x = element_text(size = 10, angle = 10, hjust = 0.5, vjust = 0.5)
  )

speicherort <- "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/pc01_d.png"
ggsave(speicherort, plot = pc01_d, width = 5.5, height = 5 , units = "in")

#### 3.1.4 Distribution of age #### 
age <- data1 %>%
  count(age) %>%
  mutate(Freq = n / sum(n) * 100)

age_d <- ggplot(age, aes(x = age, y = n)) +
  geom_bar(stat = "identity", aes(fill = age), width = 0.7, fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq), family = "Times New Roman"), 
            vjust = 0.5, size = 4, color = "black", position = position_stack(vjust = 0.5)) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = "'", scientific = FALSE),
    limits = c(0, 15000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Age Distribution",
    x = "Age",
    y = "Number of Observations"
  ) +
  theme_light() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.position = "none", # Remove legend for simplicity
    axis.text.x = element_text(size = 10, angle = 12, hjust = 0.5, vjust = 0.5)
  )

speicherort <- "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/age_d.png"
ggsave(speicherort, plot = age_d, width = 5.5, height = 5 , units = "in")

#### 3.1.5 Distribution of gender ####  
sex <- data1 %>%
  count(sex) %>%
  mutate(Freq = n / sum(n) * 100)

sex_d <- ggplot(sex, aes(x = sex, y = n)) +
  geom_bar(stat = "identity", aes(fill = sex), width = 0.7, fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Freq), family = "Times New Roman"), 
            vjust = 0.5, size = 4, color = "black", position = position_stack(vjust = 0.5)) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = "'", scientific = FALSE),
    limits = c(0, 25000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Number of Observations"
  ) +
  theme_light() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.position = "none", # Remove legend for simplicity
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5, vjust = 0.5)
  )

speicherort <- "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/sex_d.png"
ggsave(speicherort, plot = sex_d, width = 5.5, height = 5 , units = "in")






##### 3.2 Distributions within Franchise Choices ##### 
#### 3.2.1 Net Income Distribution Within Franchise Choices #### 
iptotn_x_pc189 <- data1 %>%
  group_by(pc189, iptotn_grouped) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pc189) %>%
  mutate(Freq = n / sum(n) * 100)
head(iptotn_x_pc189)

iptotn_x_pc189_d <- ggplot(iptotn_x_pc189, aes(x = pc189, y = Freq, fill = iptotn_grouped)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), 
            position = position_stack(vjust = 0.5), size = 4, family = "Times New Roman") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Net Income Distribution Within Franchise Choices",
    x = "Franchise Choice",
    y = ""
  ) +
  scale_fill_manual(values = c("<20'000 CHF" = "#999966", "20'000–50'000 CHF" = "#CCCC66", "50'000–100'000 CHF" = "#FFFF99",
                               ">100'000 CHF" = "#CCCC99"), name = "Gender") +
  theme_light() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.position = "right",
    axis.text.x = element_text(angle = 10, hjust = 0.5, vjust = 0.5)
  )

speicherort <- "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/iptotn_x_pc189_d.png"
ggsave(speicherort, plot = iptotn_x_pc189_d, width = 8, height = 6 , units = "in")

#### 3.2.2 Health Status Distribution Within Franchise Choices #### 
pc01_x_pc189 <- data1 %>%
  group_by(pc189, pc01) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pc189) %>%
  mutate(Freq = n / sum(n) * 100)
head(pc01_x_pc189)

pc01_x_pc189_d <- ggplot(pc01_x_pc189, aes(x = pc189, y = Freq, fill = pc01)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), 
            position = position_stack(vjust = 0.5), size = 4, family = "Times New Roman") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(
    title = "Health Status Distribution Within Franchise Choices",
    x = "Franchise Choice",
    y = ""
  ) +
  scale_fill_manual(values = c("very well" = "#999966", "well" = "#CCCC66", "so, so (average)" = "#FFFF99",
                               "not very well" = "#CCCC99", "not well at all" = "#999933"), name = "Health Status") +
  theme_light() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.position = "right",
    axis.text.x = element_text(angle = 10, hjust = 0.5, vjust = 0.5)
  )

speicherort <- "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/pc01_x_pc189_d.png"
ggsave(speicherort, plot = pc01_x_pc189_d, height = 6, width = 8, units = "in")

filtered_health_status <- pc01_x_pc189 %>%
  filter(pc01 %in% c("not very well", "not well at all")) %>%
  select(pc189, pc01, Freq)

health_status_table <- filtered_health_status %>%
  pivot_wider(
    names_from = pc01,  # Use "not very well" and "not well at all" as columns
    values_from = Freq, # Use the Freq column for values
    names_sort = TRUE
  )

# Display the table
health_status_table


#### 3.2.3 Age Distribution Within Franchise Choices #### 
age_x_pc189 <- data1 %>%
  group_by(pc189, age) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pc189) %>%
  mutate(Freq = n / sum(n) * 100)
head(age_x_pc189)

age_x_pc189_d <- ggplot(age_x_pc189, aes(x = pc189, y = Freq, fill = age)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), 
            position = position_stack(vjust = 0.5), size = 4, family = "Times New Roman") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(
    title = "Age Distribution Within Franchise Choices",
    x = "Franchise Choice",
    y = ""
  ) +
  scale_fill_manual(values = c("0 – 24 years" = "#999966", "25 – 34 years" = "#CCCC66", 
                               "35 – 44 years" = "#FFFF99", "45 – 54 years" = "#CCCC99", 
                               "55 – 64 years" = "#999933", "65+ years" = "#FFFF33"), name = "Age") +
  theme_light() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.position = "right",
    axis.text.x = element_text(angle = 10, hjust = 0.5, vjust = 0.5)
  )

speicherort <- "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/age_x_pc189_d.png"
ggsave(speicherort, plot = age_x_pc189_d, width = 7.5, height = 6 , units = "in")



#### 3.2.4 Gender Distribution Within Franchise Choices #### 
gender_x_pc189 <- data1 %>%
  group_by(pc189, sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pc189) %>%
  mutate(Freq = n / sum(n) * 100)
head(gender_x_pc189)

gender_x_pc189_d <- ggplot(gender_x_pc189, aes(x = pc189, y = Freq, fill = sex)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Freq)), 
            position = position_stack(vjust = 0.5), size = 4, family = "Times New Roman") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Gender Distribution Within Franchise Choices",
    x = "Franchise Choice",
    y = ""
  ) +
  scale_fill_manual(values = c("man" = "lightblue", "woman" = "pink"), name = "Gender") +
  theme_light() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.position = "right",
    axis.text.x = element_text(angle = 10, hjust = 0.5, vjust = 0.5)
  )

speicherort <- "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/gender_x_pc189_d.png"
ggsave(speicherort, plot = gender_x_pc189_d, width = 5.5, height = 5 , units = "in")

###### 4. Regression Models ###### 
#####  4.1 Descriptive - violin plot ##### 
str(data1$pc189)
data2 <- data1 %>%
  mutate(pc189 = as.numeric(pc189))
str(data2$pc189)

str(data1$iptotn_grouped)
data2 <- data2 %>%
  mutate(iptotn_grouped = as.numeric(iptotn_grouped))
str(data2$iptotn_grouped)

cor(data2$iptotn, data2$pc189, use = "complete.obs")

ggplot(data2, aes(x = iptotn, y = as.factor(pc189))) +
  geom_violin(fill = "lightblue", alpha = 0.7) +
  labs(
    title = "Income Distribution by Franchise Choice",
    x = "Net Income (CHF)",
    y = "Franchise Choice"
  ) +
  scale_x_continuous(limits = c(0, 150000)) +
  theme_minimal()

#####  4.2 Linear Regression Models ##### 
#### 4.2.1 Model 1: simple ####

model1 <- lm(pc189 ~ iptotn_grouped, data = data2)
summary(model1)

stargazer(model1, 
          title = "Regression Model 1",
          align = TRUE,
          summary = FALSE,
          out = "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/Table 1.html")

model1 <- summary(model1)
coefficients1 <- model1$coefficients
results1 <- data.frame(
  Variable = rownames(coefficients1),
  Estimate = coefficients1[, "Estimate"],
  `Std. Error` = coefficients1[, "Std. Error"],
  `t value` = coefficients1[, "t value"],
  `Pr(>|t|)` = coefficients1[, "Pr(>|t|)"]
)

#### 4.2.2 Model 2: with Confounders ####
str(data2$age)
data2 <- data2 %>%
  mutate(age = as.numeric(age))
str(data2$age)

str(data2$pc01)
data2 <- data2 %>%
  mutate(pc01 = as.numeric(pc01))
str(data2$pc01)

model2 <- lm(pc189 ~ iptotn_grouped + age + pc01, data = data2)
summary(model2)

stargazer(model2, 
          title = "Regression Model 2",
          align = TRUE,
          summary = FALSE,
          out = "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/Table 2.html")


#### 4.2.3 Model 3: with Control "Gender" ####

model3 <- lm(pc189 ~ iptotn_grouped + age + pc01 + sex, data = data2)
summary(model3)

stargazer(model3, 
          type = "latex",  # Use "text" to view it in console, "html" for HTML tables
          title = "Regression Results", 
          dep.var.labels = c("Franchise Choice"),
          covariate.labels = c("Net Income Grouped", "Age", "Health Status", "Gender - Woman"),
          digits = 3,
          out = "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/Table 3.html")

#### 4.2.4 Comparison Model 1 and Model 3 ####
stargazer(model1, model3, type = "text",
          title = "Comparison of Models 1 and 3",
          covariate.labels = c("Net Income (Grouped)", "Age", "Health Status", "Gender - Woman"),
          dep.var.labels = c("Model 1: Franchise Choice", "Model 3: Franchise Choice"),
          column.labels = c("Model 1", "Model 3"),
          add.lines = list(c("Signif. codes", "*** p<0.001, ** p<0.01, * p<0.05")),
          keep.stat = c("n", "rsq", "adj.rsq", "f", "ser"),
          out = "/Users/elenabalzli/Documents/HS24/Applied Health Economics and Econometrics/Datenanalyse/Table 4.html")
