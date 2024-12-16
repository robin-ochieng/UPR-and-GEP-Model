# UPR and GEP-Model

**Author:** Robin Ochieng Otieno    
**Version:** 1.4.1

## Overview

The **Unearned Premium Reserves and Gross Earned Premium Model** is an actuarial tool designed to model, calculate, and visualize premium reserves and earned premiums. Built using the R Shiny framework, this interactive tool facilitates data-driven decision-making for insurance analytics:

1. **UPR Calculation:** Estimating unearned premium reserves for insurance contracts.
2. **GEP Calculation:** Summarizing gross earned premiums by insurance class.
3. **Data Exploration:** Providing data upload, validation, and interactive visualization features.


## Key Features

- **Interactive Interface:** Built with bs4Dash and custom themes for a modern look and user experience.
- **Dynamic Data Loading:** Upload premium data in Excel or CSV format and view the data in real-time.
- **Scenario Testing:** Adjust parameters like valuation date and policy start year thresholds.
- **UPR Calculation:** Calculate and visualize class-wise gross UPR and DAC (Deferred Acquisition Costs).
- **GEP Calculation:** Summarize and download earned premiums by IRA class over selected time periods.
- **Downloadable Outputs:** Export tables and visualizations as CSV files.

## Directory Structure

- **modules/**  
  Contains Shiny modules encapsulating key functionalities like UPR and GEP calculations.

- **www/**  
  Holds static resources (CSS, images) for the application's front-end.

- **app.R**  
  The primary Shiny application file that launches the user interface, server logic, and helper functions.

- **app copy.R / app1.R**  
  Additional or legacy versions of the application code. These might be experimental, backup, or reference versions.

- **README.md**  
  This file, providing an overview and instructions for usage and setup.

- **.Rhistory**  
  Contains command history for development convenience (not essential for deployment).

## Getting Started

### Prerequisites

- **R (version 4.0+ recommended)**
- **R Packages:**  
  - `shiny`  
  - `ggplot2`  
  - `dplyr`, `tidyr`  
  - Other packages as required by the modules.

Install any missing packages by running in R:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr"))


