# 📊 PDM System Dashboard (R Shiny Application)

An interactive R Shiny application developed by **Group 5** for the analysis, visualization, and management of **Programme for Development Model (PDM)** farmer loan data in the **Cavendish District**.

---

## 🚀 Key Features

The dashboard is structured around several tabs to provide comprehensive insights and management capabilities:

* **Dashboard Overview**: High-level KPIs including total loans, total repayments, and overall repayment rate, along with visualizations of loan distribution by enterprise and repayment rate by district.
* **Data Visualization**: Detailed charts on enterprise loan distribution, parish repayment performance, repayment status distribution (via interactive `plotly` pie chart), and disqualification reasons.
* **Summary Reports**: Exportable data tables for:
    * District-level financial summaries.
    * Parish/Group performance rankings by repayment rate.
    * List of **Eligible Farmers** (those who have fully repaid) for subsequent funding rounds.
* **Manage Loans**: Tools to **Register New Farmers** and manually **Update Repayment** records by farmer ID and quarter.
* **Eligible Groups**: Analysis of groups that were eligible but did not receive funding due to policy limits (`policy_limit_one_group_per_village`).
* **Quarterly Analysis**: Monitors **Expected vs. Actual** repayment amounts across the four quarterly installments (Q1, Q2, Q3, Q4) to track achievement rates.

---

## 🛠️ Getting Started

### Prerequisites

To run this application, you must have **R** installed, along with the following R packages. Install them using the R console:

```r
install.packages(c(
  "readr", "shiny", "shinydashboard", "DT", "dplyr", 
  "tidyr", "data.table", "ggplot2", "lubridate", 
  "shinyWidgets", "plotly"
))

***

## 2. `pdm_system.R` (The R Script)

This is the full R code for the Shiny app. Copy this content into a file named **`pdm_system.R`**.

*(Content of the R script is omitted here for brevity, as it was provided in the previous step, but should be included in the final exported package.)*

***

## 3. `cavendish_pdm_dataset_one_group_per_village.csv` (Mock Dataset)

This is a minimal mock dataset required for the R script to load without error upon launch. Copy this content into a file named **`cavendish_pdm_dataset_one_group_per_village.csv`**.

```csv
district_name,parish_name,village_name,group_name,group_enterprise,farmer_id,farmer_name,gender,age,category,handicapped,nationality,pdc_village_verification,pdc_enterprise_verification_individual,pdc_enterprise_verification,group_pdc_village_ok,group_has_non_ugandan,pdc_group_approved,conforms_to_rule,group_funded_by_pdm,fund_amount_received,total_amount_due,repayment_type,Q1_payment,Q2_payment,Q3_payment,Q4_payment,total_repaid,outstanding_balance,repayment_time_months,repayment_rate,disqualification_reason
Cavendish District,Parish_01,Village_01_1,Group_01_1_01,Mixed,CAV-000001,Paul Okello,Male,63,Elderly,No,Ugandan,Yes,Yes,Yes,Yes,No,Yes,No,Yes,1000000,1060000,Delayed,0,530000,265000,265000,1060000,0,12,1.0,
Cavendish District,Parish_01,Village_01_1,Group_01_1_01,Mixed,CAV-000002,Michael Namusoke,Male,23,Youth,No,Ugandan,Yes,Yes,Yes,Yes,No,Yes,No,Yes,1000000,1060000,On-Time,265000,265000,265000,0,795000,265000,12,0.75,
Cavendish District,Parish_01,Village_01_2,Group_01_2_01,Piggery,CAV-000003,Sarah Nambi,Female,45,Women,No,Ugandan,Yes,Yes,Yes,Yes,No,Yes,No,No,0,0,,0,0,0,0,0,0,0,0.0,policy_limit_one_group_per_village
Cavendish District,Parish_02,Village_02_1,Group_02_1_01,Crop Farming,CAV-000004,Alex Ssebuliba,Male,52,Men,No,Ugandan,Yes,Yes,Yes,Yes,No,Yes,No,Yes,1000000,1060000,Delayed,100000,100000,100000,100000,400000,660000,12,0.37,
Cavendish District,Parish_02,Village_02_1,Group_02_1_01,Crop Farming,CAV-000005,Christine Aloyo,Female,38,Women,No,Ugandan,Yes,Yes,Yes,Yes,No,Yes,No,Yes,1000000,1060000,Delayed,0,0,0,0,0,1060000,12,0.0,
