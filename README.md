# How-much-mortality

This repository provides a single R script that analyses NHS Emergency Department (ED) data to examine crude mortality across organisations. The script performs data cleaning, modelling and visualisation using the tidymodels ecosystem.

## Overview
The file `NHSD AE dataWrangleML pipe.R` imports a CSV dataset of ED metrics, aggregates duplicate rows, removes outliers and groups trusts by peer organisation. It then fits a random forest model to predict deaths and generates several plots, including interactive funnel plots of observed versus expected mortality.

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
