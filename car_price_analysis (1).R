############################################################
# Car Price Analysis — Multiple Linear Regression in R
# STA 6244 Final Project
#
# Objective: Identify which vehicle attributes (horsepower,
# engine size, torque, top speed, acceleration, fuel type)
# most influence car prices using multiple regression with
# log-transformed response.
############################################################

# --- 0. Packages ---------------------------------------------------------

# install.packages(c("tidyverse", "janitor", "stringr", "GGally",
#                     "psych", "car", "MASS", "broom", "scales"))

library(tidyverse)
library(janitor)
library(stringr)
library(GGally)
library(psych)
library(car)
library(MASS)
library(broom)
library(scales)

# --- 1. Data Import & Cleaning -------------------------------------------
#
# The raw dataset contains text-based ranges (e.g. "$12,000-$15,000",
# "70-85 hp"). We parse these into numeric midpoints for modeling.

cars_raw <- read.csv(
  "Cars Datasets 2025.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "latin1"
)

# Clean column names to snake_case
cars <- cars_raw %>% clean_names()

# Helper: parse text ranges like "70-85 hp" or "$12,000-$15,000" into
# their numeric midpoint
parse_range_mean <- function(x) {
  x_clean <- str_remove_all(x, ",")
  nums_list <- str_extract_all(x_clean, "\\d+\\.?\\d*")

  sapply(nums_list, function(v) {
    if (length(v) == 0) return(NA_real_)
    mean(as.numeric(v))
  })
}

# Build analysis-ready dataframe
cars_model <- cars %>%
  mutate(
    Price_USD = parse_range_mean(cars_prices),
    Engine_CC = parse_range_mean(cc_battery_capacity),
    HP        = parse_range_mean(horse_power),
    TopSpeed  = parse_range_mean(total_speed),
    ZeroTo100 = parse_range_mean(performance_0_100_km_h),
    Torque    = parse_range_mean(torque),
    Seats     = parse_range_mean(as.character(seats)),
    FuelType  = factor(fuel_types),
    logPrice  = log(Price_USD)
  ) %>%
  filter(!is.na(Price_USD))

# Group fuel types into broader categories for cleaner analysis
cars_model <- cars_model %>%
  mutate(FuelGroup = case_when(
    str_detect(FuelType, "Electric")          ~ "Electric",
    str_detect(FuelType, "Hybrid")            ~ "Hybrid",
    str_detect(FuelType, "Diesel")            ~ "Diesel",
    str_detect(FuelType, "Petrol|Gasoline|Gas") ~ "Petrol",
    str_detect(FuelType, "CNG")               ~ "CNG",
    TRUE                                      ~ "Other"
  ))

cat("Dataset ready:", nrow(cars_model), "vehicles\n")

# --- 2. Exploratory Data Analysis ----------------------------------------

## 2.1 Summary Statistics -------------------------------------------------
# Provides an overview of central tendency and spread for key variables.

summary_vars <- cars_model %>%
  dplyr::select(Price_USD, HP, Torque, Engine_CC, TopSpeed, ZeroTo100, Seats)

summary(summary_vars)
psych::describe(summary_vars)

## 2.2 Price Distribution -------------------------------------------------
# Price is heavily right-skewed, motivating a log transformation.

ggplot(cars_model, aes(x = Price_USD)) +
  geom_histogram(bins = 50, fill = "darkorange", color = "white") +
  scale_x_log10(labels = comma) +
  labs(title = "Distribution of Car Prices (Log Scale)",
       x = "Price (USD, log10)", y = "Count")

ggplot(cars_model, aes(x = logPrice)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of log(Price) — Near-Normal After Transformation",
       x = "log(Price USD)", y = "Count")

## 2.3 Predictor Distributions --------------------------------------------

ggplot(cars_model, aes(x = HP)) +
  geom_histogram(bins = 30, fill = "green4", color = "black") +
  labs(title = "Distribution of Horsepower", x = "HP", y = "Count")

ggplot(cars_model, aes(x = Engine_CC)) +
  geom_histogram(bins = 30, fill = "green4", color = "black") +
  labs(title = "Distribution of Engine Displacement", x = "CC", y = "Count")

ggplot(cars_model, aes(x = TopSpeed)) +
  geom_histogram(bins = 30, fill = "green4", color = "black") +
  labs(title = "Distribution of Top Speed", x = "Top Speed (km/h)", y = "Count")

## 2.4 Skewness & Outlier Overview ----------------------------------------

psych::skew(summary_vars, na.rm = TRUE)

summary_vars_long <- summary_vars %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

ggplot(summary_vars_long, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  labs(title = "Boxplots of Numeric Variables — Outlier Overview",
       x = "", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 2.5 Correlation Analysis -----------------------------------------------
# HP, Torque, and Engine_CC show strong positive correlations with Price.
# ZeroTo100 (acceleration time) is negatively correlated — faster cars
# (lower seconds) tend to be more expensive.

numeric_vars <- cars_model %>%
  dplyr::select(Price_USD, logPrice, HP, Torque, Engine_CC, TopSpeed, ZeroTo100)

cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
round(cor_matrix, 3)

GGally::ggpairs(numeric_vars)

## 2.6 Key Scatterplots ---------------------------------------------------

ggplot(cars_model, aes(x = HP, y = Price_USD)) +
  geom_point(alpha = 0.6) +
  scale_y_log10(labels = comma) +
  labs(title = "Price vs Horsepower", x = "HP", y = "Price (USD, log scale)")

ggplot(cars_model, aes(x = Torque, y = Price_USD)) +
  geom_point(alpha = 0.6) +
  scale_y_log10(labels = comma) +
  labs(title = "Price vs Torque", x = "Torque (Nm)", y = "Price (USD, log scale)")

ggplot(cars_model, aes(x = ZeroTo100, y = Price_USD)) +
  geom_point(alpha = 0.6) +
  scale_y_log10(labels = comma) +
  labs(title = "Price vs 0-100 km/h Time", x = "Seconds", y = "Price (USD, log scale)")

## 2.7 Price by Fuel Type -------------------------------------------------
# Electric and Hybrid vehicles show higher median prices compared to
# Petrol and Diesel.

ggplot(cars_model, aes(x = FuelGroup, y = Price_USD)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10(labels = comma) +
  labs(title = "Price Distribution by Fuel Type",
       x = "Fuel Type", y = "Price (USD, log scale)")

# --- 3. ANOVA: Price Differences by Fuel Type ----------------------------
# Tests whether mean log(price) differs significantly across fuel groups.

anova_fuel_log <- aov(logPrice ~ FuelGroup, data = cars_model)
summary(anova_fuel_log)

# Pairwise comparisons — which fuel groups differ significantly?
tukey_fuel <- TukeyHSD(anova_fuel_log)
tukey_fuel

# --- 4. Regression Models ------------------------------------------------
#
# We build three models and compare:
# 1) Raw price (baseline — expect violated assumptions)
# 2) Log-price full model
# 3) Log-price stepwise (AIC-selected — final model)

## 4.1 Model 1: Raw Price (Baseline) --------------------------------------
# This model serves as a reference. We expect heteroscedasticity and
# non-normal residuals due to the skewed price distribution.

model_raw <- lm(
  Price_USD ~ HP + Engine_CC + Torque + TopSpeed + ZeroTo100 + FuelGroup,
  data = cars_model
)

summary(model_raw)
par(mfrow = c(2, 2)); plot(model_raw); par(mfrow = c(1, 1))
vif(model_raw)

## 4.2 Model 2: Log-Price Full Model --------------------------------------
# Log-transforming the response addresses skewness and stabilizes variance.

model_log_full <- lm(
  logPrice ~ HP + Engine_CC + Torque + TopSpeed + ZeroTo100 + FuelGroup,
  data = cars_model
)

summary(model_log_full)
par(mfrow = c(2, 2)); plot(model_log_full); par(mfrow = c(1, 1))
vif(model_log_full)

## 4.3 Model 3: Stepwise AIC Selection (Final Model) ----------------------
# Stepwise selection (both directions) drops non-significant predictors
# to improve parsimony while maintaining fit.

model_log_step <- stepAIC(
  model_log_full,
  direction = "both",
  trace = FALSE
)

summary(model_log_step)
formula(model_log_step)
vif(model_log_step)

# Model fit summary (R-squared, AIC, etc.)
model_fit <- glance(model_log_step)
model_fit

# --- 5. Final Model Diagnostics ------------------------------------------
# Checking assumptions: normality, homoscedasticity, influential points.

## 5.1 Diagnostic Plots ---------------------------------------------------

par(mfrow = c(2, 2)); plot(model_log_step); par(mfrow = c(1, 1))

## 5.2 Residual Distribution ----------------------------------------------
# Residuals should be approximately normal and centered at zero.

resid_df <- data.frame(residuals = resid(model_log_step))

ggplot(resid_df, aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Residual Distribution — Final Log-Price Model",
       x = "Residual", y = "Count")

## 5.3 Standardized Residuals vs Fitted ------------------------------------
# Looking for constant spread (homoscedasticity) and no patterns.

ggplot(
  data.frame(fitted = fitted(model_log_step),
             std_resid = rstandard(model_log_step)),
  aes(x = fitted, y = std_resid)
) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Standardized Residuals vs Fitted Values",
       x = "Fitted log(Price)", y = "Standardized Residuals")

## 5.4 Coefficient Table --------------------------------------------------
# Final model coefficients with confidence intervals for reporting.

coef_table <- tidy(model_log_step, conf.int = TRUE) %>%
  mutate(
    estimate  = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 3),
    p.value   = round(p.value, 4),
    conf.low  = round(conf.low, 4),
    conf.high = round(conf.high, 4)
  )

coef_table
