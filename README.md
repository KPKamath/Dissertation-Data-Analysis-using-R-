# DISSERTATION TITLE - "Examining the role of informative interventions on factors of green consumerism and green advertisement scepticism"

# ABSTRACT

This dissertation examined the impact of positive framing (intervention 1), greenwashing awareness information (intervention 2) and general product utility information (control condition) on measures of green consumerism, including environmental concern, attitude towards green products, self-reported green purchase behaviour, perceived environmental knowledge and scepticism towards green advertisements in relations to bamboo products. An online experiment was conducted to test the effects these two interventions and control condition. Furthermore, this dissertation also tested the moderating influence of the interventions on relationship between the measures of green consumerism and scepticism towards green advertisements. The findings showed that greenwashing awareness considerably impacted the consumers purchase behaviour, attitude and scepticism. However, the two interventions failed to have a significant effect on perceived environmental knowledge and concern. Contrary to what was expected, there was no significant moderating impacts present. One of the key limitations of this dissertation is that some of the variables were self-reported measures like the purchase behaviour and the future studies can make use of real time purchase data. The outcomes of this dissertation can be applied by the marketers to carefully frame the advertisement content since additional information does not necessarily make a difference in consumer behaviour.


 # R Code used for analysis

library(fastDummies)

library(dplyr) 

library(ggplot2) 

library(corrgram)

library(psych)

library(car)

library(lavaan)

library(ltm)

library(mvnormtest)

library(tidyverse)

library(effectsize)

data<- read.csv('Data_Final.csv')

head(data)

str(data)

summary(data)

#Making items into constructs

SCEP <- data[,c("Scep_1", "Scep_2", "Scep_3", "Scep_4", "Scep_5")]

Envcon <- data[,c("Envcon_1", "Envcon_2", "Envcon_3", "Envcon_4", "Envcon_5")] 

Atgp <- data[,c("Atgp_1", "Atgp_2", "Atgp_3", "Atgp_4", "Atgp_5") ]

greenPB <- data[,c("greenPB_1", "greenPB_2", "greenPB_3", "greenPB_4", "greenPB_5") ]

Pek <- data[,c("Pek_1", "Pek_2", "Pek_3", "Pek_4") ]

#Gender frequencies

table(data$Gender)

prop.table(table(data$Gender)) * 100  # For percentages

#Country frequencies

table(data$Country)

prop.table(table(data$Country)) * 100  # For percentages

#Create age groups

data$AgeGroup <- cut(data$Age, 

                     breaks = c(0, 18, 25, 35, 45, 55, Inf),
                     
                     labels = c("Under 18", "18-25", "26-35", "36-45", "46-55", "56+"),
                     
                     right = FALSE)

age_freq <- table(data$AgeGroup)

print(age_freq)

#Percentage table for age

age_percent <- prop.table(age_freq) * 100

print(age_percent)

Green_purchase2_freq <- table(data$greenPB2)

print(Green_purchase2_freq)

green_percent <- prop.table(Green_purchase2_freq) * 100

print(green_percent)

#Factor analysis

model <- '

  #Latent variables
  
  env_concern =~ Envcon_1 + Envcon_2 + Envcon_3 + Envcon_4 + Envcon_5
  
  green_skepticism =~ Scep_1 + Scep_2 + Scep_3 + Scep_4 + Scep_5
  
  attitude_green_products =~ Atgp_1 + Atgp_2 + Atgp_3 + Atgp_4 + Atgp_5
  
  green_buying_behaviour =~ greenPB_1 + greenPB_2 + greenPB_3 + greenPB_4 + greenPB_5
  
  env_knowledge =~ Pek_1 + Pek_2 + Pek_3 + Pek_4
'

fit <- cfa(model, data = data)

summary(fit, fit.measures = TRUE, standardized = TRUE)

#Cronbach Alpha for each construct

alpha_Envcon<- cronbach.alpha(Envcon)

alpha_Envcon

alpha_Atgp<- cronbach.alpha(Atgp)

alpha_Atgp

alpha_Pek <- cronbach.alpha(Pek)

alpha_Pek

alpha_greenPB <- cronbach.alpha(greenPB)

alpha_greenPB

alpha_Scep <- cronbach.alpha(Scep)

alpha_Scep

#Inter item correlation

inter_item<- cor(Pek, use= "pairwise.complete.obs")

upp_triangle<- inter_item[upper.tri(inter_item)]

avg_item<- mean(upp_triangle)

avg_item

inter_item1<- cor(Envcon, use= "pairwise.complete.obs")

upp_triangle1<- inter_item1[upper.tri(inter_item1)]

avg_item1<- mean(upp_triangle1)

avg_item1

inter_item2<- cor(greenPB, use= "pairwise.complete.obs")

upp_triangle2<- inter_item2[upper.tri(inter_item2)]

avg_item2<- mean(upp_triangle2)

avg_item2

inter_item3<- cor(Atgp, use= "pairwise.complete.obs")

upp_triangle3<- inter_item2[upper.tri(inter_item3)]

avg_item3<- mean(upp_triangle3)

avg_item3

inter_item4<- cor(Scep, use= "pairwise.complete.obs")

upp_triangle4<- inter_item4[upper.tri(inter_item4)]

avg_item4<- mean(upp_triangle4)

avg_item4

#descriptive stats for each construct

data$Scep <- rowMeans(data[, c("Scep_1", "Scep_2", "Scep_3", "Scep_4", "Scep_5")], na.rm = TRUE)

data$Envcon <- rowMeans(data[, c("Envcon_1", "Envcon_2", "Envcon_3", "Envcon_4", "Envcon_5")], na.rm = TRUE)

data$Atgp <- rowMeans(data[, c("Atgp_1", "Atgp_2", "Atgp_3", "Atgp_4", "Atgp_5")], na.rm = TRUE)

data$greenPB <- rowMeans(data[, c("greenPB_1", "greenPB_2", "greenPB_3", "greenPB_4", "greenPB_5")], na.rm = TRUE)

data$Pek <- rowMeans(data[, c("Pek_1", "Pek_2", "Pek_3", "Pek_4")], na.rm = TRUE)

summary(data$Scep)

sd(data$Scep, na.rm = TRUE)

var(data$Scep, na.rm = TRUE)

summary(data$Envcon)

sd(data$Envcon, na.rm = TRUE)

var(data$Envcon, na.rm = TRUE)

summary(data$Atgp)

sd(data$Atgp, na.rm = TRUE)

var(data$Atgp, na.rm = TRUE)

summary(data$greenPB)

sd(data$greenPB, na.rm = TRUE)

var(data$greenPB, na.rm = TRUE)

summary(data$Pek)

sd(data$Pek, na.rm = TRUE)

var(data$Pek, na.rm = TRUE)

#Stats for Each variable and seperate intervention groups

data$Interventions <- factor(data$Interventions, levels = c(1, 2, 3),

                             labels = c("Positive framing", "Green advertisement awareness", "Control group"))


summary_stats <- data %>%

  group_by(Interventions) %>%
  
  summarise(
  
    N = n(),
    
    Mean = mean(Scep, na.rm = TRUE), #Keep changing the variable
    
    S.D = sd(Scep, na.rm = TRUE),
    
    Std.Error = sd(Scep, na.rm = TRUE) / sqrt(n()),
    
    CI_Lower = Mean - qt(1 - (0.05 / 2), N - 1) * Std.Error,
    
    CI_Upper = Mean + qt(1 - (0.05 / 2), N - 1) * Std.Error,
    
    Min = min(Scep, na.rm = TRUE),
    
    Max = max(Scep, na.rm = TRUE)
  )
  

#View the summary statistics

print(summary_stats) #Keep changing the variable

#ANOVA For between and within intervention groups

get_anova_results <- function(anova_model) {
  
  anova_table <- summary(anova_model)[[1]]
  
  sum_sq_between <- anova_table[1, "Sum Sq"]
  
  df_between <- anova_table[1, "Df"]
  
  
 mean_sq_between <- anova_table[1, "Mean Sq"]
 
  f_value <- anova_table[1, "F value"]
  
  p_value <- anova_table[1, "Pr(>F)"]
  
  sum_sq_within <- anova_table[2, "Sum Sq"]
  
  df_within <- anova_table[2, "Df"]
  
  mean_sq_within <- anova_table[2, "Mean Sq"]
  
  total_sum_sq <- sum(sum_sq_between, sum_sq_within)
  
  total_df <- df_between + df_within
  
  eta_squared <- sum_sq_between / total_sum_sq
  
  #Create a data frame
  
  result <- data.frame(
  
    `Sum of Squares` = c(sum_sq_between, sum_sq_within, total_sum_sq),
    
    df = c(df_between, df_within, total_df),
    
    `Mean Square` = c(mean_sq_between, mean_sq_within, NA),
    
    F = c(f_value, NA, NA),
    
    Sig. = c(p_value, NA, NA),
    
    `Coefficient (Eta-squared)` = c(eta_squared, NA, NA)
  
  )
  
  #Name the rows
 
  rownames(result) <- c("Between Groups", "Within Groups", "Total")
  
  return(result)

}

#Apply the function to each ANOVA model

env_concern_results <- get_anova_results(anova_env_concern)

env_concern_results

green_skepticism_results <- get_anova_results(anova_green_skepticism)

green_skepticism_results

tukey_result3 <- TukeyHSD(anova_green_skepticism)

tukey_result3

att_green_products_results <- get_anova_results(anova_att_green_products)

att_green_products_results

tukey_result2 <- TukeyHSD(anova_att_green_products)

tukey_result2

green_buying_behaviour_results <- get_anova_results(anova_green_buying_behaviour)

green_buying_behaviour_results

tukey_result <- TukeyHSD(anova_green_buying_behaviour)

tukey_result

env_knowledge_results <- get_anova_results(anova_env_knowledge)

env_knowledge_results

anova_Scep <- aov(Scep ~ factor(Interventions), data = data)

#Summary of the ANOVA

summary(anova_SKEP)

TukeyHSD(anova_SKEP)


#Regression to test moderating influence

data$INT_1_F <- as.factor(data$INT_1)

model <- lm(Scep ~ Envcon * data$INT_1_F + Atgp * data$INT_1_F + greenPB * data$INT_1_F + Pek * data$INT_1_F, data = data)

summary(model)

vif_values <- vif(model)

vif_values

data$INT_2_F <- as.factor(data$INT_2)

model1 <- lm(Scep ~ Envcon * data$INT_2_F + Atgp * data$INT_2_F + greenPB * data$INT_2_F + Pek * data$INT_2_F, data = data)

summary(model1)

vif_values1 <- vif(model1)

vif_values1

data$Control_F <- as.factor(data$Control)

model2 <- lm(Scep ~ Envcon * data$Control_F + Atgp * data$Control_F + greenPB * data$Control_F + Pek * data$Control_F, data = data)

summary(model2)

vif_values2 <- vif(model2)

vif_values2

#Linearity

par(mfrow=c(2,2))

plot(model$fitted.values, model$residuals,

     main = "Residuals vs Fitted",
     
     xlab = "Fitted values",
     
     ylab = "Residuals")

abline(h = 0, col = "red")  # Add a horizontal line at 0

plot(model1$fitted.values, model1$residuals,

     main = "Residuals vs Fitted",
     
     xlab = "Fitted values",
     
     ylab = "Residuals")

abline(h = 0, col = "red")

plot(model2$fitted.values, model2$residuals,

     main = "Residuals vs Fitted",
     
     xlab = "Fitted values",
     
     ylab = "Residuals")

abline(h = 0, col = "red")

#Multi collinearity

model4 <- lm(Scep ~ Envcon + Pek + greenPB + Atgp, data=data)

vif_values4 <- vif(model4)

#Print the VIF values

print(vif_values4)

shapiro.test(data$Scep)

shapiro.test(data$Envcon)

shapiro.test(data$Pek)

shapiro.test(data$Atgp)

shapiro.test(data$greenPB)

skewness_value <- skewness(data$greenPB) #Keep changing the variables

skewness_value

library(MASS)

bc <- boxcox(data$greenPB~ 1)  #Keep changing the variables

lambda <- bc$x[which.max(bc$y)]

ab_boxcox <- (ab^lambda - 1) / lambda

shapiro.test(ab_boxcox)












