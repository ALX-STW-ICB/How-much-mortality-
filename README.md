# How-much-mortality

This repository provides a single R script for analysing NHS Emergency Department (ED) data to examine crude mortality across organisations.

The script is saved as `NHSD AE dataWrangleML pipe.R` but is documented internally as **ed_mortality_analysis.R**. It performs extensive data cleaning, modelling and visualisation using the tidymodels ecosystem.

## Overview
The following notes provide a structured, scientific-style summary of **ed_mortality_analysis.R** along with suggestions for validating intermediate and final results.

---

## Overview and Objectives

This script performs a comprehensive analysis of emergency department (ED) mortality across NHS trusts for the 2023/24 year.  Its goals are to:

1. **Import and clean** two data sources
   * Workforce staffing levels (Excel)
   * ED performance (ECDS CSV)
2. **Merge** these to compute a **crude mortality rate** per 100 000 attendances
3. **Exclude outliers** and very low-rate trusts
4. **Define peer groups** for benchmarking
5. Conduct **exploratory plots** (bar charts, scatterplots)
6. Apply **principal component analysis** (PCA) and an **ordinary least squares** (OLS) model on PCs to explore drivers of raw death counts
7. Build and **tune a random forest** regression to predict death counts
8. Use **variable-importance** (VIP) and **LIME** explanations for model interpretability
9. Produce **predicted versus observed** death plots, **funnel plots**, and **interactive dashboards**

This pipeline combines tidy-data principles (Wickham et al., 2019) with modern machine-learning workflows (Kuhn & Johnson, 2020).

---

## 1. Package Loading

```r
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(janitor)
  library(ggplot2); library(plotly)
  library(rsample); library(recipes); library(parsnip)
  library(workflows); library(tune); library(yardstick)
  library(ranger); library(lime); library(vip)
  library(tidymodels); library(dials); library(readxl); library(rlang)
})
```

* Loads **data-wrangling** (tidyverse) and **modeling** (tidymodels) packages.
* Suppresses startup messages for readability.
* **Reference**: Wickham & Grolemund (2019), *R for Data Science*.

---

## 2. Import Workforce Data (Excel)

1. **Download** NHS workforce Excel via `download.file()`.
2. **Inspect** with `excel_sheets()`, `head()`, `str()`, row/column counts.
3. **Read** four sheets (`Table_1a`, `1b`, `1c`, `2`), skipping header rows.
4. **Rename** key columns (e.g. `Doctors_FTE`, `Stability_Index`).
5. **Filter** out summary rows, convert stability to numeric.
6. **Merge** into one “wide” table `dfW_final`.

*Validation:*

* Check `file.exists(destfileW)` and dimensions.
* After cleaning, `stopifnot(ncol(dfW_final) == 5)` and no NAs in code columns.

---

## 3. Import ED Performance Data (CSV)

1. **Download** ECDS CSV and read via `read_csv()`, treating `""`, `"NA"`, `"*"` as missing.
2. **Clean** names (`clean_names()`) and drop rows missing `org_description`, `measure`, or `measure_type`.
3. **Aggregate duplicates** by mean if any (`group_by()` + `summarise()`).
4. **Create** composite header `measure_full = "type — measure"` and `pivot_wider()`.
5. **Compute** crude mortality per 100 000:

   $$
     \text{mortality}_{100k} = 10^5 \times \frac{\text{Died}}{\text{Total Attendances}}
   $$
6. **Filter** out infinite/non-finite rates.

*Validation:*

* Print summary:

  ```r
  cat(sprintf("%d rows, %d missing values\n", nrow(df), sum(is.na(df$measure_value))))
  ```
* Ensure `all(df_wide$crude_mortality_per_100k > 0)`.

---

## 4. Merge Workforce and ED Data

* **Normalize** organisation names (uppercase, strip “THE”, punctuation).
* **Join** on `org_match`, confirm match rate:

  ```r
  sum(!is.na(df_merged$Doctors_FTE)) / nrow(df_merged)
  ```
* **Clean up** duplicate columns and restore `org_description`.

*Validation:*

* Expect ≥ 90% match; investigate any unjoined trusts manually.

---

## 5. Outlier Removal

* Compute **IQR**, define fences at 1.5 × IQR.
* Flag & remove outliers, drop trusts with mortality < 10 per 100 000.
* Print pre-/post-filter statistics.

*Validation:*

* Compare `nrow(df_merged)` vs. `nrow(df_no_outliers)` and ensure removals match printed counts.

---

## 6. Peer-Group Assignment

* Define two peer-group lists (`CHKS`, `NHSE`) plus a special “SaTH” catch for **Shrewsbury & Telford**; all others → `Other`.
* Adds a factor column `peer_group`.

*Validation:*

* `table(df_no_outliers$peer_group)` yields counts for each group.

---

## 7. Exploratory Visualisations

1. **Bar chart** of crude mortality ± 95% CI by trust (coloured by peer group).
2. **Facetted bar chart** by peer group.
3. **Top/Bottom 10** trusts by rate (error bars).
4. **Scatter**: mortality vs. Doctors_FTE (with linear fit).
5. **Scatter**: mortality vs. Consultants_FTE, highlighting SaTH.

*Validation:*

* Visually inspect that plots run without errors and axes are sensible.

---

## 8. PCA & OLS on Principal Components

1. Filter non-SaTH, select numeric predictors (no zero-variance).
2. Build a **recipe**: median-impute → normalize → PCA (10 components).
3. **Prep** and **bake** to extract PCs.
4. Fit `lm(deaths ~ PC1 + … + PC10)`.
5. Summarise, plot diagnostic panels (residuals, QQ).
6. Compute absolute t-values for each PC → “component importance.”
7. Extract and display top-contributing variables for selected PCs.
8. Plot cumulative variance explained.

*References:*

* Jolliffe & Cadima (2016), tutorial on PCA.
* James et al. (2013), *Introduction to Statistical Learning*.

*Validation:*

* Check that cumulative variance curve reaches ≥ 80% by PC10.
* Ensure residual plots show no gross violations of assumptions.

---

## 9. Random Forest Regression

1. **Split** data 80/20 (stratified by death count).
2. Recipe: remove zero/near-zero variance, median-impute, normalize.
3. **Specify** `rand_forest(mtry, trees=500, min_n)` using `ranger`.
4. **Workflow** + 5-fold cross-validation.
5. **Grid-tune** `mtry` and `min_n` via Latin hypercube (10 points).
6. **Select** best by RMSE, **finalize** workflow, and **fit** on full training set.
7. **Evaluate** on test set: RMSE, R².

*Reference:*

* Liaw & Wiener (2002), `randomForest` methodology.

*Validation:*

* Compare train vs. test RMSE to check for overfitting.
* Confirm `best_params` are sensible (e.g. `mtry` around √p).

---

## 10. Model Interpretation (VIP & LIME)

* **VIP**: extract impurity-based importance and plot top features.
* **LIME**: generate local explanations for first 3 cases, plot feature contributions.

*Reference:*

* Molnar (2020), *Interpretable Machine Learning*.

*Validation:*

* Ensure VIP plot runs and LIME explanations show non-zero contributions.

---

## 11. Predictions & Funnel Plots

1. Predict death counts for all trusts, combine with observed values.
2. **Observed vs. Predicted** scatter with peer-group colours and 1:1 line.
3. Compute **O/E ratio**, standard error under Poisson:

   $$
     \text{SE}(\mathrm{O/E}) = \frac{1}{\sqrt{\text{expected}}}
   $$
4. **Funnel plot**: O/E vs. expected with ± 1.96 limits.
5. **Interactive** Plotly versions of funnel and a > 12-hour wait vs. O/E scatter.

*Validation:*

* Confirm that most trusts lie within the funnel limits as expected under Poisson variation.
* Hover tooltips should display correct labels.

---

## 12. Reproducibility and Sanity-Check Checklist

1. **Set seed** at start (e.g. `set.seed(2025)`) to fix cross-validation splits.
2. **Dimension checks** after each major join/transform:

   ```r
   stopifnot(nrow(df_wide) == length(unique(df$org_description)))
   stopifnot(ncol(dfW_final) == 5)
   ```
3. **Missingness reports** for each key column:

   ```r
   summarise_all(df_merged, ~ sum(is.na(.)))
   ```
4. **Summary statistics**:

   ```r
   summary(df_merged$crude_mortality_per_100k)
   ```
5. **Cross-validation diagnostics**: examine `tune_res` object for any failed folds.
6. **Model performance**: ensure `rmse(test)` is within 10–20% of training RMSE.
7. **Visual inspections**: all ggplot and plotly commands run without error.

---

By following this document and running the suggested checks, you can both understand the purpose of each section and verify that **ed_mortality_analysis.R** is producing correct, reproducible results.

## Requirements
* **R** 4.0 or higher
* R packages: `readr`, `dplyr`, `tidyr`, `janitor`, `ggplot2`, `plotly`, `rsample`, `recipes`, `parsnip`, `workflows`, `tune`, `yardstick`, `ranger`, `lime`, `vip`, and the `tidymodels` suite.

Install missing packages with:

```R
install.packages(c("tidymodels", "plotly", "janitor", "ranger", "lime", "vip"))
```

## Usage
1. Edit the `file_path` variable near the top of `NHSD AE dataWrangleML pipe.R` so that it points to your local CSV file.
2. Run the script within R or from the command line:

```bash
Rscript "NHSD AE dataWrangleML pipe.R"
```

The script will print summaries and open plots in your R session.

## License
Distributed under the terms of the [GNU General Public License v3](LICENSE).
