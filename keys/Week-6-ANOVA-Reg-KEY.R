# Week 6 Multivariate Statistics KEY
# Caitlin Mothes, 2/22/2023


#load in packages
library(tidyverse)
library(lterdatasampler)
library(car)

# data set
data("pie_crab")



# EXERCISE 1 ANOVA ----------------------------------------

## Carry out the ANOVa (this is a part of the lesson, not required in this answer!)

# create site subset to do full ANOVA
pie_sites <- pie_crab %>% 
  filter(site %in% c("GTM", "DB", "PIE"))

pie_anova <- aov(size ~ site, data = pie_sites)

summary(pie_anova)
# Df Sum Sq Mean Sq F value Pr(>F)    
# site         2  521.5  260.75   60.02 <2e-16 ***
#   Residuals   83  360.6    4.34                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(pie_anova)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = size ~ site, data = pie_sites)
# 
# $site
#             diff       lwr       upr   p adj
# GTM-DB  -3.200786 -4.507850 -1.893722 3.0e-07
# PIE-DB   2.899929  1.592865  4.206992 2.9e-06
# PIE-GTM  6.100714  4.771306  7.430123 0.0e+00

## EXERCIZE 1 PART 1 BOXPLOT -----------------------------------
pie_sites %>% 
  ggplot(aes(x = reorder(site, latitude), y = size))+
  geom_boxplot()



## EXERCISE 2 SIMPLE LINEAR REGRESSION ---------------------------

pie_lm <- lm(size ~ water_temp_sd, data = pie_crab)

#view the results of the linear model
summary(pie_lm)

# all:
#   lm(formula = size ~ water_temp_sd, data = pie_crab)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.9428 -2.6948 -0.2145  2.6573  8.8070 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   13.93728    1.15338  12.084   <2e-16 ***
#   water_temp_sd  0.09938    0.15716   0.632    0.528    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.506 on 390 degrees of freedom
# Multiple R-squared:  0.001024,	Adjusted R-squared:  -0.001537 
# F-statistic: 0.3999 on 1 and 390 DF,  p-value: 0.5275

# FIGURE
pie_crab %>% 
  ggplot(aes(x = water_temp_sd, y = size))+
  geom_point()+
  geom_smooth(method = "lm")


# EXERCISE 3 MULTIPLE LINEAR REGRESSION ------------------------------


## Check for correlations among predictors
pie_crab %>% 
  select(latitude, air_temp_sd, water_temp_sd) %>% 
  cor()

#               latitude air_temp_sd water_temp_sd
# latitude      1.00000000   0.7932130    0.04188273
# air_temp_sd   0.79321301   1.0000000    0.40970338
# water_temp_sd 0.04188273   0.4097034    1.00000000


# Latitude and air temp have a pretty strong correlation, up to the students if they want to drop
# it for the analysis. Either way is fine, I just want them to know how to assess the correlations

pie_mlm <- lm(size ~ latitude + air_temp_sd + water_temp_sd, data = pie_crab)

summary(pie_mlm)

# Call:
#   lm(formula = size ~ latitude + air_temp_sd + water_temp_sd, data = pie_crab)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.7515 -1.8897  0.0506  1.9301  6.6746 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -3.96880    1.54818  -2.564   0.0107 *  
#   latitude       0.55940    0.06413   8.723   <2e-16 ***
#   air_temp_sd   -0.41713    0.30559  -1.365   0.1730    
# water_temp_sd  0.15927    0.16174   0.985   0.3254    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.832 on 388 degrees of freedom
# Multiple R-squared:  0.3516,	Adjusted R-squared:  0.3466 
# F-statistic: 70.13 on 3 and 388 DF,  p-value: < 2.2e-16
