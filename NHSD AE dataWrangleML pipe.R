# ed_mortality_analysis.R
# Comprehensive ED mortality analysis script, ready to run

# 1. Load required packages --------------------------------------------------
suppressPackageStartupMessages({
  library(readr)      # fast CSV import
  library(dplyr)      # data manipulation
  library(tidyr)      # data reshaping
  library(janitor)    # clean column names
  library(ggplot2)    # plotting
  library(plotly)     # interactive plots
  library(rsample)    # data splitting
  library(recipes)    # preprocessing
  library(parsnip)    # model specification
  library(workflows)  # modeling workflows
  library(tune)       # hyperparameter tuning
  library(yardstick)  # performance metrics
  library(ranger)     # random forest engine
  library(lime)       # local model explanations
  library(vip)        # variable importance
  library(tidymodels)
  library(dials)
  library(readxl)
})
library(rlang)       # after updating, ensures new rlang is attached
library(dials)       # gives you finalize()
library(parsnip)     # for rand_forest(), parameters()
library(workflows)
library(tune)

# 1. Update & load
#install.packages(c("rlang","dials","tidymodels"))
# Restart R
library(tidymodels)



# 2. Define file paths and import data ----------------------------------------
#Workforce Data
# Define the URL of the Excel file
urlw <- "https://files.digital.nhs.uk/07/CD51BD/AE_2324%20acci-emer-workforce-data.xlsx"

# Define the local destination file path
destfileW <- "AE_2324_acci-emer-workforce-data.xlsx"

# Download the file (use mode = "wb" for binary)
download.file(urlw, destfileW, mode = "wb")


# List sheet names
sheets <- excel_sheets(destfileW)
print(sheets)

# Read the first sheet (or specify by name/index as needed)
dfw <- read_excel(destfileW, sheet = sheets[1])

# Preview and diagnostics
print(head(dfw))
print(str(dfw))

# Check existence of file and basic row count
cat("File exists?: ", file.exists(destfileW), "\n")
cat("Row count: ", nrow(dfw), "  Column count: ", ncol(dfw), "\n")


# Path to the downloaded Excel file
file_pathw <- destfileW
# === 1. READ TABLES ==========================================================
# Skip header rows to get to structured data (based on inspection)
df_1a <- read_excel(file_pathw, sheet = "Table_1a", skip = 19)
df_1b <- read_excel(file_pathw, sheet = "Table_1b", skip = 19)
df_1c <- read_excel(file_pathw, sheet = "Table_1c", skip = 19)
df_2  <- read_excel(file_pathw, sheet = "Table_2", skip = 22)

# === 2. CLEAN COLUMN NAMES ===================================================
names(df_1a)[c(1,2,7)] <- c("Code", "Provider_Description", "Doctors_FTE")
names(df_1b)[c(1,2,7)] <- c("Code", "Provider_Description", "Consultants_FTE")
names(df_1c)[c(1,2,7)] <- c("Code", "Provider_Description", "Nurses_FTE")
names(df_2)[1:6] <- c("Code", "Provider_Description", "Stability_Index", "Doctors", "Nurses", "Care_providers")

# === 3. SELECT & FILTER ======================================================
df_1a_clean <- df_1a %>% select(Provider_Description, Doctors_FTE)
df_1b_clean <- df_1b %>% select(Provider_Description, Consultants_FTE)
df_1c_clean <- df_1c %>% select(Provider_Description, Nurses_FTE)

df_2_clean <- df_2 %>%
  filter(!is.na(Stability_Index), Stability_Index != "All clinical staff 1,2") %>%
  select(Provider_Description, Stability_Index) %>%
  mutate(Stability_Index = as.numeric(Stability_Index))

# === 4. MERGE INTO WIDE FORMAT ===============================================
dfW_final <- df_1a_clean %>%
  left_join(df_1b_clean, by = "Provider_Description") %>%
  left_join(df_1c_clean, by = "Provider_Description") %>%
  left_join(df_2_clean, by = "Provider_Description")

rm(list = setdiff(ls(), "dfW_final"))


# Define the URL of the CSV file
url <- "https://files.digital.nhs.uk/A3/89C26F/AE_2324_ECDS_pla_output.csv"

# Define the local file path where you want to save the CSV
destfile <- "AE_2324_ECDS_pla_output.csv"

# Download the file
download.file(url, destfile, mode = "wb")  # use mode = "wb" to ensure binary write on Windows


raw_df <- read_csv(
  destfile,
  na = c("", "NA", "*"),
  col_types = cols(.default = col_guess())
)

# 3. Clean names & drop rows missing key identifiers -------------------------
df <- raw_df %>%
  janitor::clean_names() %>%
  filter(
    !is.na(org_description),
    !is.na(measure),
    !is.na(measure_type)
  )

# 4. Aggregate duplicates by mean, if present --------------------------------
dup_counts <- df %>% count(org_description, measure_type, measure) %>% filter(n > 1)
if (nrow(dup_counts) > 0) {
  message("Found duplicates; aggregating values by mean.")
  df <- df %>%
    group_by(org_description, measure_type, measure) %>%
    summarise(
      measure_value = mean(measure_value, na.rm = TRUE),
      .groups = "drop"
    )
}

# 5. Create composite header and pivot to wide format ------------------------
df <- df %>%
  mutate(measure_full = paste(measure_type, measure, sep = " — "))

df_wide <- df %>%
  pivot_wider(
    id_cols     = org_description,
    names_from  = measure_full,
    values_from = measure_value,
    values_fill = NA
  )

# 6. Report overall missingness ----------------------------------------------
total_rows    <- nrow(df)
total_missing <- sum(is.na(df$measure_value))
cat(sprintf(
  "Total rows: %d\nMissing values: %d (%.2f%%)\n",
  total_rows,
  total_missing,
  100 * total_missing / total_rows
))

# 7. Compute crude mortality per 100k and remove non-finite ------------------
df_wide <- df_wide %>%
  mutate(
    crude_mortality_per_100k = 1e5 *
      `Discharge Destination — Died` /
      `Waiting Times — TOTAL_ATTENDANCES`
  ) %>%
  filter(is.finite(crude_mortality_per_100k))

#Add in the Workforce data=========================

# Load required libraries
library(dplyr)
library(stringr)

# Redefine normalization function
normalize_org <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("^THE\\s+", "") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_squish()
}

# Re-rename Provider_Description if needed and normalize
dfW_final <- dfW_final %>%
  rename(org_description = Provider_Description) %>%
  mutate(org_match = normalize_org(org_description))

# Normalize df_wide too
df_wide <- df_wide %>%
  mutate(org_match = normalize_org(org_description))

# Join
df_merged <- df_wide %>%
  left_join(dfW_final, by = "org_match")

# Confirm match rate
cat("Matched orgs with Doctors_FTE: ", sum(!is.na(df_merged$Doctors_FTE)), "\n")

# After joining
df_merged <- df_merged %>%
  rename(org_description = org_description.y) %>%
  select(-ends_with(".x"))  # optional cleanup
df_merged <- df_merged %>%
  select(org_description, everything(), -org_match)


#==============================================================================
#Analysis
#==============================================================================


# Outlier exclusion via 1.5 * IQR --------------------------------------------
library(dplyr)

# 1. Compute Q1 & Q3 on the full merged data
stats <- df_merged %>%
  summarise(
    Q1 = quantile(crude_mortality_per_100k, 0.25, na.rm = TRUE),
    Q3 = quantile(crude_mortality_per_100k, 0.75, na.rm = TRUE)
  )

# 2. Define the 1.5×IQR fences
IQR_val <- stats$Q3 - stats$Q1
lower   <- stats$Q1 - 1.5 * IQR_val
upper   <- stats$Q3 + 1.5 * IQR_val

# 3. Build df_no_outliers in one pipeline:
df_no_outliers <- df_merged %>%
  # a) drop missing names
  filter(!is.na(org_description)) %>%
  # b) flag 1.5×IQR outliers
  mutate(is_outlier = crude_mortality_per_100k < lower |
           crude_mortality_per_100k > upper) %>%
  # c) keep non-outliers only
  filter(!is_outlier) %>%
  # d) drop the flag column
  select(-is_outlier) %>%
  # e) drop any trust with mortality < 10 per 100 000
  filter(crude_mortality_per_100k >= 10)

# 4. Quick summaries & checks
df_no_outliers %>% summarise(
  min_rate   = min(crude_mortality_per_100k),
  max_rate   = max(crude_mortality_per_100k),
  n_trusts   = n()
) %>% print()

removed_total <- nrow(df_merged) - nrow(df_no_outliers)
cat("Removed", removed_total, "rows total (NAs + outliers + <10)\n")

orig_stats <- df_merged %>%
  summarise(
    min_orig = min(crude_mortality_per_100k, na.rm = TRUE),
    max_orig = max(crude_mortality_per_100k, na.rm = TRUE)
  )
print(orig_stats)


# 8. Define peer groups -----------------------------------------------------
chks_peers <- c(
  "BEDFORDSHIRE HOSPITALS NHS FOUNDATION TRUST",
  "COUNTY DURHAM AND DARLINGTON NHS FOUNDATION TRUST",
  "EAST AND NORTH HERTFORDSHIRE NHS TRUST",
  "EAST LANCASHIRE HOSPITALS NHS TRUST",
  "GLOUCESTERSHIRE HOSPITALS NHS FOUNDATION TRUST",
  "NORTHERN CARE ALLIANCE NHS FOUNDATION TRUST",
  "ROYAL CORNWALL HOSPITALS NHS TRUST",
  "UNITED LINCOLNSHIRE HOSPITALS NHS TRUST",
  "WIRRAL UNIVERSITY TEACHING HOSPITAL NHS FOUNDATION TRUST",
  "WORCESTERSHIRE ACUTE HOSPITALS NHS TRUST",
  "THE SHREWSBURY AND TELFORD HOSPITAL NHS TRUST"
)
nhse_peers <- c(
  "AIREDALE NHS FOUNDATION TRUST",
  "CHESTERFIELD ROYAL HOSPITAL NHS FOUNDATION TRUST",
  "COUNTESS OF CHESTER HOSPITAL NHS FOUNDATION TRUST",
  "ISLE OF WIGHT NHS TRUST",
  "SOMERSET NHS FOUNDATION TRUST",
  "TORBAY AND SOUTH DEVON NHS FOUNDATION TRUST",
  "UNITED LINCOLNSHIRE HOSPITALS NHS TRUST",
  "UNIVERSITY HOSPITALS PLYMOUTH NHS TRUST",
  "WARRINGTON AND HALTON HOSPITALS NHS FOUNDATION TRUST",
  "WORCESTERSHIRE ACUTE HOSPITALS NHS TRUST",
  "THE SHREWSBURY AND TELFORD HOSPITAL NHS TRUST"
)

df_no_outliers <- df_no_outliers %>%
  mutate(
    peer_group = case_when(
      str_to_upper(org_description) %in% str_to_upper("Shrewsbury and Telford Hospital NHS Trust") ~ "SaTH",
      str_to_upper(org_description) %in% chks_peers ~ "CHKS",
      str_to_upper(org_description) %in% nhse_peers ~ "NHSE",
      TRUE ~ "Other"
    )
  )



# 9. Plot crude mortality ----------------------------------------------------
# 0. Load packages
library(dplyr)
library(ggplot2)
library(viridis)

# 1. Compute rates & 95% CIs, dropping zero/missing attendances
df_rates <- df_no_outliers %>%
  filter(
    !is.na(`Waiting Times — TOTAL_ATTENDANCES`) &
      `Waiting Times — TOTAL_ATTENDANCES` > 0
  ) %>%
  mutate(
    # deaths per attendance
    rate_per_attend = crude_mortality_per_100k / 100000,
    # SE of that rate
    se_per_attend   = sqrt(rate_per_attend / `Waiting Times — TOTAL_ATTENDANCES`),
    # back to per-100k scale
    rate_per_100k   = rate_per_attend * 100000,
    lower_95_CI     = pmax((rate_per_attend - 1.96 * se_per_attend) * 100000, 0),
    upper_95_CI     =       (rate_per_attend + 1.96 * se_per_attend) * 100000
  )

# 2. Sanity checks
#    a) Column names
stopifnot(all(c("rate_per_100k","lower_95_CI","upper_95_CI") %in% names(df_rates)))


# 3. (Optional) spot-check first row
print(df_rates[1, c("org_description",
                    "Discharge Destination — Died",
                    "Waiting Times — TOTAL_ATTENDANCES",
                    "crude_mortality_per_100k",
                    "rate_per_100k","lower_95_CI","upper_95_CI")])

# 4. Now you can plot:
ggplot(df_rates, aes(
  x    = reorder(org_description, rate_per_100k),
  y    = rate_per_100k,
  fill = peer_group
)) +
  geom_col() +
  geom_errorbar(aes(ymin = lower_95_CI, ymax = upper_95_CI), width = 0.3) +
  coord_flip() +
  labs(
    title = "Crude Mortality per 100 000 Attendances (±95% CI)",
    x     = NULL,
    y     = "Deaths per 100 000",
    fill  = "Peer Group"
  ) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y         = element_text(size = 8)
  )

#=============
library(dplyr)
library(ggplot2)
library(viridis)

# Re‐use df_rates from before, which has rate_per_100k, lower_95_CI, upper_95_CI, peer_group, org_description

ggplot(df_rates, aes(
  x    = reorder(org_description, rate_per_100k),
  y    = rate_per_100k,
  fill = peer_group
)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ peer_group, scales = "free_y", ncol = 2) +
  coord_flip() +
  labs(
    title = "Crude Mortality per 100 000 Attendances by Peer Group",
    x     = NULL, 
    y     = "Deaths per 100 000"
  ) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text        = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )



#===================================================
top_bottom <- df_rates %>%
  arrange(rate_per_100k) %>%
  slice(c(1:10, (n()-9):n())) %>%
  mutate(label = if_else(row_number() <= 10, "Lowest", "Highest"))

ggplot(top_bottom, aes(
  x = rate_per_100k, 
  y = reorder(org_description, rate_per_100k),
  color = label
)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower_95_CI, xmax = upper_95_CI), height = 0.2) +
  labs(
    title = "Top 10 & Bottom 10 Trusts by Crude Mortality",
    x     = "Deaths per 100 000",
    y     = NULL,
    color = NULL
  ) +
  scale_color_manual(values = c("Lowest" = "forestgreen", "Highest" = "firebrick")) +
  theme_minimal(base_size = 12)


#===============================================================

ggplot(df_no_outliers, aes(x = Doctors_FTE, y = crude_mortality_per_100k)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Crude Mortality vs. Doctors FTE",
    x = "Doctors FTE",
    y = "Crude Mortality per 100k"
  )



# Identify SaTH trust name (title case)
sath_org <- df_no_outliers %>%
  filter(str_detect(str_to_upper(org_description), "SHREWSBURY|TELFORD")) %>%
  pull(org_description) %>%
  unique()

# Plot
ggplot(df_no_outliers, aes(x = Consultants_FTE, y = crude_mortality_per_100k)) +
  geom_point(color = "grey70") +
  geom_point(
    data = df_no_outliers %>% filter(org_description %in% sath_org),
    aes(x = Consultants_FTE, y = crude_mortality_per_100k),
    color = "red", size = 3
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Crude Mortality vs. Consultants FTE (2023/24)",
    x = "Consultants (FTE)",
    y = "Crude Mortality per 100,000",
    subtitle = paste("SaTH highlighted:", sath_org)
  ) +
  theme_minimal()



# 10. Correlation & OLS diagnostics -----------------------------------------
# Step 1: Select relevant numeric variables (excluding mortality)
# Step 1: Filter and select numeric variables first (same rows)
df_filtered <- df_no_outliers %>%
  filter(peer_group != "SaTH")

numeric_filtered <- df_filtered %>%
  select(where(is.numeric)) %>%
  select_if(~ sum(!is.na(.)) > 10) %>%
  select_if(~ sd(., na.rm = TRUE) > 0) %>%
  select(-`Discharge Destination — Died`)

# Step 2: Combine with deaths
df_pca_input <- df_filtered %>%
  select(deaths = `Discharge Destination — Died`) %>%
  bind_cols(numeric_filtered)


numeric_filtered <- df_no_outliers %>%
  select(where(is.numeric)) %>%
  select_if(~ sum(!is.na(.)) > 10) %>%
  select_if(~ sd(., na.rm = TRUE) > 0) %>%
  select(-`Discharge Destination — Died`)  # avoid outcome leakage

# Step 2: Combine with outcome and filter non-SaTH
df_pca_input <- df_no_outliers %>%
  filter(peer_group != "SaTH") %>%
  select(deaths = `Discharge Destination — Died`) %>%
  bind_cols(numeric_filtered)

# Step 3: Build recipe: impute → normalize → PCA
rec <- recipe(deaths ~ ., data = df_pca_input) %>%
  step_impute_median(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 10)  # you can increase if needed

# Step 4: Prep and bake the data
rec_prep <- prep(rec)
df_pca <- bake(rec_prep, new_data = NULL)

# Step 5: Fit OLS model on principal components
model_pca <- lm(deaths ~ ., data = df_pca)

# Step 6: Output summary
summary(model_pca)

# Step 7: Plot diagnostics
par(mfrow = c(2,2))
plot(model_pca)


# Extract model summary
model_summary <- summary(model_pca)

# Extract t-values for each PC (excluding intercept)
component_importance <- broom::tidy(model_pca) %>%
  filter(term != "(Intercept)") %>%
  mutate(component = term,
         importance = abs(statistic))  # absolute t-value

# Plot
library(ggplot2)
ggplot(component_importance, aes(x = reorder(component, importance), y = importance)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Component Importance (|t-stat| from OLS Regression)",
    x = "Principal Component",
    y = "Importance (|t| value)"
  ) +
  theme_minimal()

# Extract PCA loadings from the prepped recipe (step 3 = PCA)
loadings <- tidy(rec_prep, number = 3)


# Inspect PCA output from the recipe step
tidied_pca <- tidy(rec_prep, number = 3)
print(head(tidied_pca, 20))


# Top 10 contributing variables to PC2
top_PC2 <- tidied_pca %>%
  filter(component == "PC2") %>%
  mutate(abs_value = abs(value)) %>%
  arrange(desc(abs_value)) %>%
  slice_head(n = 10)
top_PC4 <- tidied_pca %>%
  filter(component == "PC4") %>%
  mutate(abs_value = abs(value)) %>%
  arrange(desc(abs_value)) %>%
  slice_head(n = 10)
# Display
print("Top variables contributing to PC1:")
print(top_PC1)

print("Top variables contributing to PC2:")
print(top_PC2)

print("Top variables contributing to PC4:")
print(top_PC4)


library(scales)  # for percent_format()

# Extract the PCA step (adjust if not step 3)
pca_step <- rec_prep$steps[[which(sapply(rec_prep$steps, function(x) inherits(x, "step_pca")))]]

# Get variance explained
explained_var <- pca_step$res$sdev^2
prop_var <- explained_var / sum(explained_var)
cumulative_var <- cumsum(prop_var)

# Build dataframe for plotting
pca_var_df <- tibble(
  component = seq_along(prop_var),
  variance = prop_var,
  cumulative = cumulative_var
)

# Plot
ggplot(pca_var_df, aes(x = component, y = cumulative)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Cumulative Variance Explained by Principal Components",
    x = "Principal Component",
    y = "Cumulative Variance Explained"
  ) +
  theme_minimal()




# 11. Random forest modeling -----------------------------------------------
# Prepare for ML: select features and target
# Rename death count for modeling
# (Assumes df_no_outliers already defined)
df_ml <- df_no_outliers %>%
  # Exclude non-feature columns: identifiers and derived metrics
  select(-org_description, -peer_group, -crude_mortality_per_100k) %>%
  # Rename outcome for modeling
  rename(deaths = `Discharge Destination — Died`)


# Split data (80/20) with stratification to preserve distribution
split <- initial_split(df_ml, prop = 0.8, strata = deaths)
train <- training(split)
test  <- testing(split)

# Preprocessing recipe: remove zero/near-zero variance, impute, normalize
rec <- recipe(deaths ~ ., data = train) %>%
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())

# Random forest specification with tuning
rf_spec <- rand_forest(
  mtry   = tune(),
  trees  = 500,
  min_n  = tune()
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# Workflow
wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_spec)

# Create 5-fold cross-validation splits
dcv <- vfold_cv(train, v = 5)

# Define tuning parameter ranges and grid
rf_params <- parameters(
  list(
    mtry = mtry(range = c(2, floor(sqrt(ncol(train) - 1)))),
    min_n = min_unique(range = c(2, 10))
  )
)

# Use a Latin hypercube grid for diversity
grid_rf <- grid_max_entropy(rf_params, size = 10)

# Tune model using the explicit grid
# Note: warnings about predictor counts per fold are expected when filter steps vary features
# They do not affect tuning validity but indicate dynamic recipe behavior

tune_res <- tune_grid(
  wf,
  resamples = dcv,
  grid      = grid_rf,
  metrics   = metric_set(rmse, rsq)
)

# Select best tuning parameters by RMSE (named argument required)
best_params <- select_best(tune_res, metric = "rmse")

# Finalize and fit the model
wf_final <- finalize_workflow(wf, best_params)
fit_rf   <- fit(wf_final, train)

# Evaluate on test set
pred <- predict(fit_rf, test) %>% bind_cols(test)
print(pred %>% metrics(truth = deaths, estimate = .pred))

# 12. Interpretation: VIP & LIME -------------------------------------------- Interpretation: VIP & LIME --------------------------------------------
vip(extract_fit_parsnip(fit_rf)$fit)
train_baked <- bake(prep(rec), new_data = train)
explainer <- lime(train_baked %>% select(-deaths), extract_fit_parsnip(fit_rf)$fit)
explanation <- explain(train_baked[1:3,] %>% select(-deaths), explainer, n_features = 5)
plot_features(explanation)

# 13. Predict for all organisations & plot Expected vs Observed -------------
# Prepare full dataset for prediction (retain organisation names and peer groups)
df_pred_input <- df_no_outliers %>%
  select(org_description, peer_group, everything()) %>%
  rename(observed_deaths = `Discharge Destination — Died`)

# Check column names
#print("Columns in df_pred_input:")
#print(names(df_pred_input))

# Extract feature-only dataframe matching training structure
features <- df_pred_input %>%
  select(-org_description, -peer_group, -observed_deaths)

# Generate predictions: returns tibble with .pred
pred_tbl <- predict(fit_rf, new_data = features)
print("Columns in predictions tibble:")
print(names(pred_tbl))

# Combine predictions with org and peer info
df_preds <- bind_cols(
  df_pred_input %>% select(org_description, peer_group, observed_deaths),
  pred_tbl
) %>%
  rename(predicted_deaths = .pred)

# Verify df_preds structure
print("Columns in df_preds:")
print(names(df_preds))
head(df_preds)

# Plot observed vs predicted, highlighting peer groups
ggplot(df_preds, aes(
  x     = observed_deaths,
  y     = predicted_deaths,
  color = peer_group,
  label = org_description
)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "SaTH"  = "orange",
      "CHKS"  = "firebrick",
      "NHSE"  = "deepskyblue",
      "Other" = "grey70"
    ),
    name = "Peer Group"
  ) +
  labs(
    title = "Expected vs Observed ED Deaths by Organisation",
    x     = "Observed Deaths",
    y     = "Predicted Deaths"
  ) +
  theme_minimal() +
  theme(
    legend.position   = "bottom",
    legend.title      = element_text(size = 10),
    legend.text       = element_text(size = 8)
  )

# 14. Funnel plot: Observed vs Expected Deaths Ratio ------------------------
# Compute observed-to-expected ratio (O/E) and control limits assuming Poisson variation
funnel_df <- df_preds %>%
  mutate(
    expected = predicted_deaths,
    observed = observed_deaths,
    oe_ratio = observed / expected,
    se_ratio = 1 / sqrt(expected),                  # SE of O/E under Poisson assumption
    lcl = pmax(0, 1 - 1.96 * se_ratio),              # lower 95% limit
    ucl = 1 + 1.96 * se_ratio                        # upper 95% limit
  )

# Plot funnel: ratio vs expected deaths
ggplot(funnel_df, aes(x = expected, y = oe_ratio, color = peer_group, label = org_description)) +
  geom_point(size = 3, alpha = 0.8) +
  # Add control limit curves
  ggplot2::geom_line(data = funnel_df %>% distinct(expected, lcl),
                     aes(x = expected, y = lcl),
                     inherit.aes = FALSE, linetype = "dashed") +
  ggplot2::geom_line(data = funnel_df %>% distinct(expected, ucl),
                     aes(x = expected, y = ucl),
                     inherit.aes = FALSE, linetype = "dashed") +
  # Reference line at ratio = 1
  ggplot2::geom_hline(yintercept = 1, color = "black", linetype = "solid") +
  scale_color_manual(
    values = c(
      "SaTH"  = "orange",
      "CHKS"  = "firebrick",
      "NHSE"  = "deepskyblue",
      "Other" = "grey70"
    ),
    name = "Peer Group"
  ) +
  labs(
    title = "Funnel Plot of Observed-to-Expected ED Deaths",
    x = "Expected Deaths",
    y = "Observed / Expected Ratio (O/E)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 8)
  )

# 15. Interactive funnel plot with plotly ------------------------------------
library(plotly)

funnel_plotly <- plot_ly(
  data = funnel_df,
  x = ~expected,
  y = ~oe_ratio,
  color = ~peer_group,
  colors = c("#F59E0B", "#B91C1C", "#0EA5E9", "#A1A1AA"),
  text = ~paste(
    "Trust: ", org_description,
    "<br>O/E Ratio: ", round(oe_ratio, 2),
    "<br>Expected: ", round(expected, 1)
  ),
  hoverinfo = "text",
  mode = "markers",
  type = "scatter"
) %>%
  add_lines(
    data = funnel_df %>% distinct(expected, ucl),
    x = ~expected,
    y = ~ucl,
    inherit = FALSE,
    line = list(dash = 'dash')
  ) %>%
  add_lines(
    data = funnel_df %>% distinct(expected, lcl),
    x = ~expected,
    y = ~lcl,
    inherit = FALSE,
    line = list(dash = 'dash')
  ) %>%
  layout(
    title = "Interactive Funnel Plot: Observed vs Expected ED Deaths",
    xaxis = list(title = "Expected Deaths"),
    yaxis = list(title = "Observed / Expected (O/E) Ratio"),
    legend = list(orientation = 'h', x = 0.1, y = -0.2)
  )

# 16. Scatter of Trusts with O/E > 1 vs >12h Waits --------------------------
long_waits_df <- df_no_outliers %>%
  select(org_description,
         long_waits = `Waiting Times — MORE_THAN_12_HOURS_FROM_ARRIVAL_TO_DEPARTURE`)

high_oe <- funnel_df %>% filter(oe_ratio > 1) %>%
  left_join(long_waits_df, by = "org_description")

wait_plotly <- plot_ly(
  data = high_oe,
  x = ~long_waits,
  y = ~oe_ratio,
  color = ~peer_group,
  colors = c("#F59E0B", "#B91C1C", "#0EA5E9", "#A1A1AA"),
  text = ~paste(
    "Trust: ", org_description,
    "<br>O/E Ratio: ", round(oe_ratio, 2),
    "<br>>12h Waits: ", long_waits
  ),
  hoverinfo = "text",
  mode = "markers",
  type = "scatter"
) %>%
  layout(
    title = "Trusts with O/E > 1 vs >12h Waits",
    xaxis = list(title = ">12h ED Visits"),
    yaxis = list(title = "O/E Death Ratio"),
    legend = list(orientation = 'h', x = 0.1, y = -0.2)
  )

print(funnel_plotly)
print(wait_plotly)
# End of script



