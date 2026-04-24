# Determinants of Global Life Expectancy (2000-2023) 🌍📊
### Advanced Econometric Analysis & Predictive Modeling

## 📌 Project Overview
The objective of this project is to identify and analyze the key determinants of life expectancy worldwide between 2000 and 2023. Using **World Bank Group** data, I developed a multiple regression model that explains over **80% of the variance** in life expectancy, specifically accounting for structural differences between developed and developing nations.

## 🛠 Tech Stack
* **Language:** R
* **Libraries:**
    * `tidyverse` & `dplyr` – data manipulation & cleaning
    * `car` – VIF calculation & collinearity diagnostics
    * `lmtest` & `sandwich` – robust standard errors (HC3) & heteroskedasticity testing
    * `tseries` & `randtests` – normality (Jarque-Bera) & randomness tests
    * `mice` – missing data pattern analysis
    * `ggplot2`, `patchwork` & `corrplot` – advanced data visualization
* **Methods:** OLS Regression, Hellwig’s Feature Selection, Chow Test, Ramsey RESET, 10-fold Cross-Validation.

## 📈 Key Findings
* **The "Rich" Interaction:** The Chow Test revealed that the impact of GDP and health expenditure on life expectancy differs drastically depending on a country's wealth level (structural break).
* **Fertility Impact:** Each additional birth per woman statistically shortens average life expectancy by approximately **1.8 years** (acting as a proxy for maternal healthcare access).
* **Energy Infrastructure:** Access to electricity serves as a fundamental pillar for modern medicine; every 1% increase in access correlates with a **+0.15 year** increase in life expectancy.
* **Model Accuracy:** The Mean Absolute Percentage Error (MAPE) is only **4.05%**, confirming the model's high predictive power.

## 🔬 Econometric Rigor (Model Diagnostics)
This project follows a high-standard analytical pipeline to ensure model reliability:

1. **Data Preprocessing & EDA:** Initial data restructuring, including handling missing patterns (MICE) and log-transforming skewed variables (GDP, CO2) to stabilize variance and linearize relationships.
2. **Feature Selection:** Optimal predictor selection using Hellwig’s Information Capacity Method, AIC criteria, and ANOVA testing to balance model complexity and explanatory power.
3. **Functional Form Optimization:** Transitioned from a purely linear model to a **non-linear specification** by testing interactions (e.g., Urbanization * GDP) and quadratic terms. The final model was chosen from multiple iterations based on minimizing the **Ramsey RESET** statistic (improving specification from "grossly misstated" to "statistically acceptable").
4. **Structural Stability:** Used the **Chow Test** to identify a structural break between economies. Addressed this by introducing a `rich` dummy variable and interaction terms, reducing the F-statistic by 1500% and ensuring the model holds for both developed and developing nations.
5. **Robustness:** Implemented **HC3 Robust Standard Errors** to maintain valid statistical inference in the presence of confirmed heteroskedasticity.
6. **Initial Error Evaluation:** Before final validation, performed a comprehensive **ex-post error analysis**, calculating **RMSE, MAE, and MAPE** (4.05%). Conducted a **temporal error analysis** (MAE over time) to verify the model's consistency across the 2000-2023 period.
7. **Final Validation:** Conducted a **10-fold Cross-Validation** to prove the model's stability and its ability to generalize to unseen data without overfitting.

## 📊 Visualizations
*The analysis includes logarithmic GDP distributions, correlation matrices, and ex-post error analysis.*

## 📂 Project Structure
* `data_life_exp_wrld.csv` – raw dataset (World Bank Source).
* `life_expectancy_analysis.R` – full R script with detailed comments.
* `README.md` – project documentation.

---
⭐ **Note:** I encourage you to explore the full `.R` script. It is extensively commented to ensure the analytical steps and logic are transparent and easy to follow.
