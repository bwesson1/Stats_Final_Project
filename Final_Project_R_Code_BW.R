# Set working directory to the directory with the dataset

setwd("C:/Users/bradj/OneDrive - University of Oklahoma/Documents/Spring 2025 OU Classes/PSY-3003 Advanced Statistics/Final_Project")

# Import dataset

originaldata <- read.csv("OverallWellBeing_UnitedStates_Dataset.csv")

# View data

View(originaldata)

# Keep relevant variables

data <- originaldata[, c("HAPPY", "SAT1", "SAT2", "SAT3", "SAT4", "SAT5", "RES1", "RES2", "RES3", "RES4", "RES5", "RES6",
                         "HHINC", "AGE", "SEX", "PHYS_HEALTH", "MENT_HEALTH")]

# View cleaned data

View(data)

# Remove missing values

data <- na.omit(data)

# Create single "well-being" variable using HAPPY, SAT1-5, and RES1-6 variables

# Create mean scores for each scale

data$LifeSatisfaction <- rowMeans(data[, c("SAT1", "SAT2", "SAT3", "SAT4", "SAT5")], na.rm = TRUE)
data$Resilience <- rowMeans(data[, c("RES1", "RES2", "RES3", "RES4", "RES5", "RES6")], na.rm = TRUE)

# Standardize each component (z-score)

data$Happiness_z <- scale(data$HAPPY)
data$LifeSatisfaction_z <- scale(data$LifeSatisfaction)
data$Resilience_z <- scale(data$Resilience)

# Combine to one well-being score

data$WELLBEING <- rowMeans(data[, c("Happiness_z", "LifeSatisfaction_z", "Resilience_z")], na.rm = TRUE)

# Keep relevant variables

data <- data[, c("WELLBEING", "HHINC", "AGE", "SEX", "PHYS_HEALTH", "MENT_HEALTH")]

# Set 98 and 99 as NA in HHINC and MENT_HEALTH variables and 3 in SEX variable

data$HHINC[data$HHINC %in% c(98, 99)] <- NA
data$MENT_HEALTH[data$MENT_HEALTH %in% c(98, 99)] <- NA
data$SEX[data$SEX %in% c(3)] <- NA

# Remove missing values

data <- na.omit(data)

# Check normality of all variables

library(jmv)
descriptives(data,
             vars = vars(WELLBEING, HHINC, AGE, SEX, PHYS_HEALTH, MENT_HEALTH),
             sd = TRUE, range = TRUE,
             skew = TRUE, kurt = TRUE, hist = TRUE)

# Trim top and bottom 1% for normality for WELLBEING variable

data <- data[data$WELLBEING > quantile(data$WELLBEING, 0.01, na.rm = TRUE) &
                       data$WELLBEING < quantile(data$WELLBEING, 0.99, na.rm = TRUE), ]

# Normality is met for all variables

# Frequencies for variables   MAY NOT BE NEEDED FOR THE PROJECT********************************
library(sjmisc)
frq(data, WELLBEING, HHINC, AGE, SEX, PHYS_HEALTH, MENT_HEALTH)

# Assumptions for multiple regression

# Check linearity

plot(data$HHINC,data$WELLBEING,
     abline(lm(data$WELLBEING~data$HHINC)))

plot(data$AGE,data$WELLBEING,
     abline(lm(data$WELLBEING~data$AGE)))
     
plot(data$SEX,data$WELLBEING,
     abline(lm(data$WELLBEING~data$SEX)))

plot(data$PHYS_HEALTH,data$WELLBEING,
     abline(lm(data$WELLBEING~data$PHYS_HEALTH)))

plot(data$MENT_HEALTH,data$WELLBEING,
     abline(lm(data$WELLBEING~data$MENT_HEALTH)))

# Assumption of linearity is met

# Check normality of residuals and homoscedasticity

normality <- lm(WELLBEING~HHINC + AGE + SEX + PHYS_HEALTH + MENT_HEALTH, data = data)

library(olsrr)
ols_plot_resid_hist(normality)
ols_plot_resid_fit(normality)

# normality of residuals is met because the residuals are normally distributed around 0.

# homoscedasticity is met because the data points are in an oval shape which indicates that the variance is similar at all levels of the independent variable.


# check multicollinearity

library(jmv)

multicollinearity <- corrMatrix(data,
                                vars = vars(WELLBEING, HHINC, AGE, SEX, PHYS_HEALTH, MENT_HEALTH),
                                n = TRUE)
multicollinearity

ols_coll_diag(normality)

# VIF is less than 5, VIF = 1.08 which indicates the absence of multicollinearity which means the assumption is met.

# Additionally, an r value between two independent variables is not > .8 which indicates the absence of multicollinearity meaning that the assumption is met.

# Multiple Regression

MultiRegression <- linReg(data,
                          dep = WELLBEING, covs = vars(HHINC, AGE, PHYS_HEALTH, MENT_HEALTH),
                          factors = vars(SEX),
                          blocks = list(list("HHINC", "AGE", "SEX", "PHYS_HEALTH", "MENT_HEALTH")),
                          refLevels = list(
                            list(
                              var = "SEX",
                              ref = "1")),
                          modelTest = TRUE, stdEst = TRUE)

MultiRegression

# Interpretation results

# Household Income did not significantly predict well-being (B = 4.89e^-7, SE = .001, SB = 5.30e^-6, p = .99),

# Age significantly and positively predicted well-being (B = .001, SE = .001, SB = .055, p = .0000023),

# Women (2) significantly reported lower levels of well-being than men (1) with (B = -.014, SE = .006, SB = -.052, p = .022),

# Physical health significantly and positively predicted well-being (B = .009, SE = .004, SB = .035, p = .012),

# Mental health significantly and positively predicted well-being (B = .081, SE = .003, SB = .341, p < .0000001),

# Together, household income, age, sex, physical health, and mental health 
# explained a *moderate* proportion of the variance in hyperactivity (*(F(5, 7144) = 202.88, p < .0000001, R2 = .12)*).

# This means that:

# Income has no effect on well-being.
# When age gets higher, well-being increases.
# Females experience lower levels of well-being than males.
# When physical health increases, well-being also increases.
# When mental health increases, well-being also increases.

model <- lm(WELLBEING ~ HHINC + AGE + SEX + PHYS_HEALTH + MENT_HEALTH, data = data)

library(ggplot2)
library(broom)

# Tidy the model and remove intercept
model_df <- tidy(model, conf.int = TRUE) %>%
  filter(term != "(Intercept)")

# Clean up variable names for nicer labels
model_df$term <- recode(model_df$term,
                        "HHINC" = "Household Income",
                        "AGE" = "Age",
                        "SEX" = "Sex (ref: Men)",
                        "PHYS_HEALTH" = "Physical Health",
                        "MENT_HEALTH" = "Mental Health")

# Create the coefficient plot
ggplot(model_df, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(color = "#0072B2", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "#0072B2") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  coord_flip() +
  labs(title = "Predictors of Well-Being in the U.S.",
       x = "Predictor",
       y = "Regression Coefficient") +
  theme_minimal(base_size = 14)



