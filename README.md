# Car Price Analysis — Multiple Linear Regression in R

Statistical analysis of vehicle pricing using multiple linear regression to identify which attributes (horsepower, engine displacement, torque, top speed, acceleration, fuel type) most influence car prices.

## Dataset

**Source:** Cars Datasets 2025 — product-level data with specifications and prices for vehicles across multiple fuel types (Petrol, Diesel, Electric, Hybrid, CNG).

The raw data contains text-based ranges (e.g., "$12,000–$15,000", "70–85 hp") which are parsed into numeric midpoints for analysis.

## Analysis Overview

1. **Data Cleaning** — custom parser to extract numeric values from text ranges, fuel type grouping, log-transformation of price
2. **Exploratory Data Analysis** — summary statistics, distributions, skewness, correlation matrix, scatterplots, boxplots by fuel type
3. **ANOVA** — one-way ANOVA + Tukey HSD to test whether mean prices differ significantly across fuel types
4. **Regression Modeling** — three models compared:
   - Raw price model (baseline, violated assumptions)
   - Log-price full model (all predictors)
   - Log-price stepwise model (AIC-selected, final)
5. **Diagnostics** — residual normality, homoscedasticity, VIF for multicollinearity, standardized residuals vs. fitted values

## Methods & Tools

| Category | Details |
|---|---|
| **Language** | R |
| **EDA & Visualization** | ggplot2, GGally, psych |
| **Statistical Modeling** | lm(), aov(), TukeyHSD(), stepAIC() (MASS) |
| **Diagnostics** | car (VIF), broom (tidy model output) |
| **Data Wrangling** | tidyverse, janitor, stringr |

## Key Findings

- **Price is heavily right-skewed** — log-transformation was essential to satisfy regression assumptions and produced near-normal residuals.
- **HP, Torque, and Engine CC** are the strongest positive predictors of price, while **0–100 km/h time** is negatively associated (faster acceleration = higher price).
- **Fuel type matters** — Electric and Hybrid vehicles have significantly higher median prices than Petrol and Diesel (confirmed by ANOVA + Tukey HSD).
- **Stepwise AIC selection** produced a more parsimonious model by dropping non-significant predictors while maintaining comparable R² and lower AIC.
- **VIF analysis** flagged moderate multicollinearity between HP and Torque, which is expected given their mechanical relationship.

## Project Structure

```
├── car_price_analysis.R     # Complete analysis script
├── README.md                # Project documentation
└── .gitignore               # Excludes data files
```

## How to Run

1. Clone this repository
2. Install R packages:
   ```r
   install.packages(c("tidyverse", "janitor", "stringr", "GGally",
                       "psych", "car", "MASS", "broom", "scales"))
   ```
3. Place `Cars Datasets 2025.csv` in the project root
4. Run `car_price_analysis.R` in RStudio or from the terminal:
   ```bash
   Rscript car_price_analysis.R
   ```

## Author

Sebastian — M.S. Data Science & AI, Florida International University
