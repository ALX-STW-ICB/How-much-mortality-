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



# 2. Define file path and import data ----------------------------------------
file_path <- "C:/Users/alexander.neale/OneDrive - NHS/Analytics STW data lead/Anaylsis/Mortality/ED data/AE_2324_ECDS_pla_output.csv"
raw_df <- read_csv(
  file_path,
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

# Outlier exclusion via 1.5 * IQR --------------------------------------------
stats <- df_wide %>% summarise(Q1 = quantile(crude_mortality_per_100k, 0.25),
                               Q3 = quantile(crude_mortality_per_100k, 0.75))
IQR_val <- stats$Q3 - stats$Q1
lower  <- stats$Q1 - 1.5 * IQR_val
upper  <- stats$Q3 + 1.5 * IQR_val

df_no_outliers <- df_wide %>%
  mutate(is_outlier = crude_mortality_per_100k < lower |
           crude_mortality_per_100k > upper) %>%
  filter(!is_outlier)

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
      org_description == "THE SHREWSBURY AND TELFORD HOSPITAL NHS TRUST" ~ "SaTH",
      org_description %in% chks_peers ~ "CHKS",
      org_description %in% nhse_peers ~ "NHSE",
      TRUE ~ "Other"
    )
  )

# 9. Plot crude mortality ----------------------------------------------------
ggplot(df_no_outliers, aes(
  x = reorder(org_description, crude_mortality_per_100k),
  y = crude_mortality_per_100k,
  fill = peer_group
)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Crude Mortality per 100,000 Attendances by Trust",
    x = "Trust",
    y = "Mortality per 100k",
    fill = "Peer Group"
  ) +
  theme_minimal()

# 10. Correlation & OLS diagnostics -----------------------------------------
df_corr <- df_no_outliers %>%
  select(deaths = `Discharge Destination — Died`, where(is.numeric), -crude_mortality_per_100k) %>%
  cor(use = "pairwise.complete.obs")
top_features <- sort(df_corr["deaths", ], decreasing = TRUE)[1:5]

# Fit linear model on peers (excl. SaTH)
df_model <- df_no_outliers %>%
  filter(peer_group != "SaTH") %>%
  transmute(deaths = `Discharge Destination — Died`, !!!as.list(top_features))
model_ols <- lm(deaths ~ ., data = df_model)
par(mfrow = c(2,2)); plot(model_ols)

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
rf_params <- parameters(rf_spec) %>%
  update(
    mtry  = mtry(range = c(2, floor(sqrt(ncol(train) - 1)))), 
    min_n = min_n(range = c(2, 10))
  ) %>%
  finalize(train)

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
print("Columns in df_pred_input:")
print(names(df_pred_input))

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


