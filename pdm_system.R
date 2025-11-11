library(readr)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(shinyWidgets)
library(plotly)

# Load the actual dataset
pdm_dataset <- read_csv("./cavendish_pdm_dataset_one_group_per_village.csv")

# Todo: Data preprocessing
preprocess_pdm_data <- function(df) {
  # Standardize column names to lowercase
  df <- df |> rename_with(tolower)
  
  # Map to our expected column names
  df <- df |> rename(
    district = district_name,
    parish = parish_name,
    group = group_name,
    farmerid = farmer_id,
    name = farmer_name,
    group_enterprise = group_enterprise,
    loanamount = fund_amount_received,
    repayment1 = q1_payment,
    repayment2 = q2_payment,
    repayment3 = q3_payment,
    repayment4 = q4_payment,
    disqualification_reason = disqualification_reason
  )
  
  # Ensure all required columns exist with proper defaults
  required_cols <- c("district", "parish", "group", "farmerid", "name", "age", 
                     "gender", "group_enterprise", "loanamount", "repayment1", 
                     "repayment2", "repayment3", "repayment4", "disqualification_reason")
  
  for (col in required_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  
  # Convert and clean data with 4 repayments
  df <- df |> mutate(
    repayment1 = replace_na(as.numeric(repayment1), 0),
    repayment2 = replace_na(as.numeric(repayment2), 0),
    repayment3 = replace_na(as.numeric(repayment3), 0),
    repayment4 = replace_na(as.numeric(repayment4), 0),
    loanamount = as.numeric(loanamount),
    total_repaid = repayment1 + repayment2 + repayment3 + repayment4,
    # Each farmer receives 1,000,000 and must pay back 1,060,000
    total_amount_due = 1060000,
    outstanding = total_amount_due - total_repaid,
    # Calculate repayment rate for individual farmers
    individual_repayment_rate = ifelse(total_amount_due == 0, 0, round(100 * total_repaid / total_amount_due, 2)),
    # Determine if group is eligible based on disqualification reason
    group_eligible = ifelse(disqualification_reason == "policy_limit_one_group_per_village", "Eligible",
                          ifelse(is.na(disqualification_reason) | disqualification_reason == "", "Funded", "Not Eligible"))
  )
  
  return(df)
}

# Todo: Data management functions (unchanged)
register_farmer_df <- function(df, district, parish, group, farmerid, name, age, gender, group_enterprise, loanamount) {
  new_row <- tibble::tibble(
    district = district,
    parish = parish,
    group = group,
    farmerid = as.character(farmerid),
    name = name,
    age = as.integer(age),
    gender = gender,
    group_enterprise = group_enterprise,
    loanamount = as.numeric(loanamount),
    repayment1 = 0,
    repayment2 = 0,
    repayment3 = 0,
    repayment4 = 0,
    total_repaid = 0,
    total_amount_due = 1060000,
    outstanding = 1060000,
    individual_repayment_rate = 0,
    disqualification_reason = NA,
    group_eligible = "Funded"
  )
  bind_rows(df, new_row)
}

update_repayment_df <- function(df, farmerid, instalment = 1, amount) {
  if (!farmerid %in% df$farmerid) stop("FarmerID not found: ", farmerid)
  
  col <- case_when(
    instalment == 1 ~ "repayment1",
    instalment == 2 ~ "repayment2", 
    instalment == 3 ~ "repayment3",
    instalment == 4 ~ "repayment4"
  )
  
  df[[col]][df$farmerid == farmerid] <- df[[col]][df$farmerid == farmerid] + as.numeric(amount)
  
  df$total_repaid[df$farmerid == farmerid] <- df$repayment1[df$farmerid == farmerid] + 
    df$repayment2[df$farmerid == farmerid] + 
    df$repayment3[df$farmerid == farmerid] + 
    df$repayment4[df$farmerid == farmerid]
  
  df$outstanding[df$farmerid == farmerid] <- df$total_amount_due[df$farmerid == farmerid] - df$total_repaid[df$farmerid == farmerid]
  
  df$individual_repayment_rate[df$farmerid == farmerid] <- ifelse(
    df$total_amount_due[df$farmerid == farmerid] == 0, 0,
    round(100 * df$total_repaid[df$farmerid == farmerid] / df$total_amount_due[df$farmerid == farmerid], 2)
  )
  
  df
}

# Todo: Reporting Functions

# 1. District-level summary report
generate_district_summary <- function(df) {
  df |> 
    group_by(district) |>
    summarise(
      total_farmers = n(),
      total_loans_disbursed = sum(loanamount, na.rm = TRUE),
      total_amount_due = sum(total_amount_due, na.rm = TRUE),
      total_repaid = sum(total_repaid, na.rm = TRUE),
      overall_repayment_rate = ifelse(total_amount_due == 0, 0, round(100 * total_repaid / total_amount_due, 2)),
      avg_individual_repayment_rate = mean(individual_repayment_rate, na.rm = TRUE),
      fully_repaid_farmers = sum(outstanding <= 0, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    arrange(desc(overall_repayment_rate))
}

# 2. Parish-level performance table ranking farmer groups by repayment rate
generate_parish_performance <- function(df) {
  df |> 
    group_by(district, parish, group, group_enterprise) |>
    summarise(
      total_farmers = n(),
      total_loan_amount = sum(loanamount, na.rm = TRUE),
      total_amount_due = sum(total_amount_due, na.rm = TRUE),
      total_repaid = sum(total_repaid, na.rm = TRUE),
      repayment_rate = ifelse(total_amount_due == 0, 0, round(100 * total_repaid / total_amount_due, 2)),
      fully_repaid_farmers = sum(outstanding <= 0, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    arrange(desc(repayment_rate)) |>
    mutate(rank = row_number())
}

# 3. List of farmers eligible for larger loans (those who fully repaid)
get_eligible_farmers <- function(df) {
  df |> 
    filter(outstanding <= 0) |>
    select(district, parish, group, group_enterprise, farmerid, name, gender, age, 
           loanamount, total_repaid, outstanding, individual_repayment_rate) |>
    arrange(district, parish, group, desc(individual_repayment_rate))
}

# 4. Loan distribution by group_enterprise
get_loan_distribution_enterprise <- function(df) {
  df |>
    group_by(group_enterprise) |>
    summarise(
      total_farmers = n(),
      total_loans = sum(loanamount, na.rm = TRUE),
      avg_loan_per_farmer = mean(loanamount, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    arrange(desc(total_loans))
}

# 5. Repayment performance by parish
get_repayment_by_parish <- function(df) {
  df |>
    group_by(district, parish) |>
    summarise(
      total_farmers = n(),
      total_loans = sum(loanamount, na.rm = TRUE),
      total_repaid = sum(total_repaid, na.rm = TRUE),
      total_due = sum(total_amount_due, na.rm = TRUE),
      repayment_rate = ifelse(total_due == 0, 0, round(100 * total_repaid / total_due, 2)),
      .groups = 'drop'
    ) |>
    arrange(desc(repayment_rate))
}

# 6. Repayment performance by district
get_repayment_by_district <- function(df) {
  df |>
    group_by(district) |>
    summarise(
      total_farmers = n(),
      total_loans = sum(loanamount, na.rm = TRUE),
      total_repaid = sum(total_repaid, na.rm = TRUE),
      total_due = sum(total_amount_due, na.rm = TRUE),
      repayment_rate = ifelse(total_due == 0, 0, round(100 * total_repaid / total_due, 2)),
      .groups = 'drop'
    ) |>
    arrange(desc(repayment_rate))
}

# NEW: Groups eligible for loans (policy_limit_one_group_per_village)
get_eligible_groups <- function(df) {
  df |>
    filter(disqualification_reason == "policy_limit_one_group_per_village") |>
    group_by(district, parish, group, group_enterprise, disqualification_reason) |>
    summarise(
      total_farmers = n(),
      total_requested_loan = sum(loanamount, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    arrange(district, parish, group)
}

# NEW: Disqualification reasons summary
get_disqualification_summary <- function(df) {
  df |>
    filter(!is.na(disqualification_reason) & disqualification_reason != "") |>
    group_by(disqualification_reason) |>
    summarise(
      total_groups = n_distinct(group),
      total_farmers = n(),
      total_requested_amount = sum(loanamount, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    arrange(desc(total_farmers))
}

# NEW: Quarterly repayment analysis
get_quarterly_repayment_analysis <- function(df) {
  quarterly_summary <- df |>
    filter(loanamount > 0) |>  # Only funded farmers
    summarise(
      q1_expected = n() * 265000,
      q1_actual = sum(repayment1, na.rm = TRUE),
      q2_expected = n() * 265000,
      q2_actual = sum(repayment2, na.rm = TRUE),
      q3_expected = n() * 265000,
      q3_actual = sum(repayment3, na.rm = TRUE),
      q4_expected = n() * 265000,
      q4_actual = sum(repayment4, na.rm = TRUE)
    )
  
  quarterly_rates <- data.frame(
    quarter = c("Q1", "Q2", "Q3", "Q4"),
    expected = c(
      quarterly_summary$q1_expected,
      quarterly_summary$q2_expected,
      quarterly_summary$q3_expected,
      quarterly_summary$q4_expected
    ),
    actual = c(
      quarterly_summary$q1_actual,
      quarterly_summary$q2_actual,
      quarterly_summary$q3_actual,
      quarterly_summary$q4_actual
    )
  ) |>
    mutate(
      achievement_rate = ifelse(expected == 0, 0, round(100 * actual / expected, 2)),
      shortfall = expected - actual
    )
  
  return(quarterly_rates)
}

#Todo: Graphical User Interface (GUI) initialising

ui <- dashboardPage(
  dashboardHeader(title = "Group 5 - PDM Farmer Loans"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Manage Loans", tabName = "manage", icon = icon("hand-holding-usd")),
      menuItem("Summary Reports", tabName = "reports", icon = icon("file-alt")),
      menuItem("Data Visualization", tabName = "visualization", icon = icon("chart-line")),
      menuItem("Eligible Groups", tabName = "eligible_groups", icon = icon("users")),
      menuItem("Quarterly Analysis", tabName = "quarterly", icon = icon("calendar-alt")),
      menuItem("User Manual", tabName = "manual", icon = icon("book"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".small-box {height: 110px}") )),
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Load / Upload Data", status = "primary", solidHeader = TRUE, width = 6,
                    fileInput("file", "Upload CSV file (optional)", accept = c('.csv')),
                    actionButton("load_pdm", "Load PDM Dataset", icon = icon("seedling")),
                    br(), br(),
                    checkboxInput("header", "CSV has header", TRUE)
                ),
                box(title = "Data Snapshot", width = 6, solidHeader = TRUE, status = "primary",
                    DTOutput("data_table")
                )
              )
      ),
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_loan_box", width = 3),
                valueBoxOutput("total_repaid_box", width = 3),
                valueBoxOutput("repayment_rate_box", width = 3),
                valueBoxOutput("eligible_farmers_box", width = 3)
              ),
              fluidRow(
                box(title = "Loan Distribution by Enterprise", status = "info", width = 6, 
                    plotOutput("plot_enterprise_loan", height = 350)),
                box(title = "Repayment Rate by District", status = "info", width = 6, 
                    plotOutput("plot_district_repayment", height = 350))
              ),
              fluidRow(
                box(title = "Top Performing Groups", status = "success", width = 12, 
                    DTOutput("top_groups_table"))
              )
      ),
      tabItem(tabName = "manage",
              fluidRow(
                box(title = "Register Approved Farmer/Group", status = "primary", width = 6,
                    textInput("reg_district", "District", value = "District_1"),
                    textInput("reg_parish", "Parish", value = "Parish_1"),
                    textInput("reg_group", "Group", value = "Group_1"),
                    textInput("reg_enterprise", "Enterprise", value = "Crop Farming"),
                    textInput("reg_farmerid", "Farmer ID", value = "G1_F001"),
                    textInput("reg_name", "Name"),
                    numericInput("reg_age", "Age", value = 30, min = 18, max = 120),
                    selectInput("reg_gender", "Gender", choices = c("M", "F")),
                    numericInput("reg_loan", "Loan Amount", value = 1000000, step = 1000),
                    actionButton("register_btn", "Register", icon = icon("user-plus"))
                ),
                box(title = "Update Repayment", status = "primary", width = 6,
                    selectizeInput("sel_farmer", "Choose Farmer (type to search)", 
                                   choices = NULL, multiple = FALSE),
                    radioButtons("instalment", "Quarter", 
                                 choices = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4), inline = TRUE),
                    numericInput("rep_amount", "Amount", value = 265000, step = 1000),
                    actionButton("repay_btn", "Record Payment", icon = icon("hand-holding-usd")),
                    hr(),
                    actionButton("bulk_payment_demo", "Simulate Bulk Payments (demo)", 
                                 icon = icon("random"))
                )
              ),
              fluidRow(
                box(title = "Farmers", status = "warning", width = 12, 
                    DTOutput("farmers_table"))
              )
      ),
      tabItem(tabName = "reports",
              fluidRow(
                box(title = "District Summary Report", status = "primary", width = 12,
                    DTOutput("district_summary_report"))
              ),
              fluidRow(
                box(title = "Parish Performance - Groups Ranking", status = "info", width = 12,
                    DTOutput("parish_performance_report"))
              ),
              fluidRow(
                box(title = "Farmers Eligible for Larger Loans", status = "success", width = 12,
                    DTOutput("eligible_farmers_report"))
              ),
              fluidRow(
                box(title = "Download Reports", status = "warning", width = 12,
                    downloadButton("download_district_summary", "Download District Summary"),
                    downloadButton("download_parish_performance", "Download Parish Performance"),
                    downloadButton("download_eligible_farmers", "Download Eligible Farmers List")
                )
              )
      ),
      tabItem(tabName = "visualization",
              fluidRow(
                box(title = "Loan Distribution by Enterprise", status = "primary", width = 6,
                    plotOutput("viz_enterprise_loans", height = 400)
                ),
                box(title = "Repayment Performance by Parish", status = "primary", width = 6,
                    plotOutput("viz_parish_repayment", height = 400)
                )
              ),
              fluidRow(
                box(title = "Repayment Status Distribution", status = "info", width = 6,
                    plotlyOutput("viz_repayment_pie", height = 400)
                ),
                box(title = "Disqualification Reasons", status = "info", width = 6,
                    plotOutput("viz_disqualification_reasons", height = 400)
                )
              ),
              fluidRow(
                box(title = "Top 10 Best Performing Groups", status = "info", width = 6,
                    plotOutput("viz_top_groups", height = 400)
                ),
                box(title = "Loan vs Repayment Analysis", status = "success", width = 6,
                    plotOutput("viz_loan_repayment_scatter", height = 400)
                )
              )
      ),
      tabItem(tabName = "eligible_groups",
              fluidRow(
                valueBoxOutput("eligible_groups_box", width = 4),
                valueBoxOutput("eligible_farmers_count_box", width = 4),
                valueBoxOutput("eligible_loan_amount_box", width = 4)
              ),
              fluidRow(
                box(title = "Groups Eligible for Funding (Policy Limit)", status = "success", width = 12,
                    DTOutput("eligible_groups_table"))
              ),
              fluidRow(
                box(title = "All Disqualified Groups Summary", status = "warning", width = 12,
                    DTOutput("disqualification_summary_table"))
              )
      ),
      tabItem(tabName = "quarterly",
              fluidRow(
                valueBoxOutput("q1_achievement_box", width = 3),
                valueBoxOutput("q2_achievement_box", width = 3),
                valueBoxOutput("q3_achievement_box", width = 3),
                valueBoxOutput("q4_achievement_box", width = 3)
              ),
              fluidRow(
                box(title = "Quarterly Repayment Performance", status = "primary", width = 8,
                    plotOutput("quarterly_repayment_plot", height = 400)
                ),
                box(title = "Quarterly Repayment Details", status = "info", width = 4,
                    DTOutput("quarterly_repayment_table")
                )
              ),
              fluidRow(
                box(title = "Quarterly Achievement Rates", status = "success", width = 12,
                    plotlyOutput("quarterly_achievement_plot", height = 300)
                )
              )
      ),
      tabItem(tabName = "manual",
              box(title = "Brief User Manual", status = "info", width = 12, solidHeader = TRUE,
                  h4("Purpose"),
                  p("This Shiny app provides interactive management and reporting for PDM farmer loans. Each farmer receives UGX 1,000,000 and must repay UGX 1,060,000 in 4 quarterly instalments."),
                  h4("PDM Structure"),
                  p("Farmer > Farmer Group > Village > Parish > District. Each parish has 4 villages and receives UGX 100M."),
                  h4("New Features"),
                  p("• Eligible Groups: Groups that didn't receive funding due to policy limits"),
                  p("• Quarterly Analysis: Expected vs actual repayment performance by quarter"),
                  p("• Enhanced Visualization: Pie charts and better repayment distribution views"),
                  h4("Summary Reports"),
                  p("• District Summary: Total loans vs repayments by district"),
                  p("• Parish Performance: Groups ranked by repayment rate"),
                  p("• Eligible Farmers: Those who fully repaid for larger loans"),
                  h4("Data Visualization"),
                  p("• Loan distribution by enterprise type"),
                  p("• Repayment performance by parish and district"),
                  p("• Performance analytics and rankings"),
                  h4("How to register a farmer"),
                  p("Go to Manage Loans -> Register Farmer. Fill form and click Register."),
                  h4("How to update repayments"),
                  p("Choose a farmer in Manage Loans -> Update Repayment, select quarter and amount then click Record Payment."),
                  h4("Development Team Credits"),
                  p("This application was developed by Group 5 with the following contributions:"),
                  DTOutput("credits_table")
              
                  
              )
      )
    )
  )
)

#Todo: Server Initialisation 

server <- function(input, output, session) {
  # Reactive dataset stored in memory
  rv <- reactiveValues(df = NULL)
  
  # Helper function for toast notifications
  showToast <- function(type, message) {
    switch(type,
           success = shinyWidgets::show_toast("Success", message, type = "success"),
           warning = shinyWidgets::show_toast("Warning", message, type = "warning"),
           error = shinyWidgets::show_toast("Error", message, type = "error"),
           info = shinyWidgets::show_toast("Info", message, type = "info")
    )
  }
  
  # Load PDM dataset
  observeEvent(input$load_pdm, {
    rv$df <- preprocess_pdm_data(pdm_dataset)
    showToast("success", "PDM dataset loaded successfully")
  })
  
  # Handle uploaded CSV files
  observeEvent(input$file, {
    req(input$file)
    df_in <- fread(input$file$datapath)
    rv$df <- preprocess_pdm_data(df_in)
    showToast("success", "CSV file uploaded and processed")
  })
  
  # Initialize with PDM data when app starts
  observe({
    req(!is.null(pdm_dataset))
    if (is.null(rv$df) && exists("pdm_dataset") && nrow(pdm_dataset) > 0) {
      rv$df <- preprocess_pdm_data(pdm_dataset)
      showToast("success", "PDM dataset loaded successfully")
    } else if (is.null(rv$df)) {
      # Fallback: create empty dataset with correct structure
      rv$df <- tibble::tibble(
        district = character(),
        parish = character(),
        group = character(),
        group_enterprise = character(),
        farmerid = character(),
        name = character(),
        age = integer(),
        gender = character(),
        loanamount = numeric(),
        repayment1 = numeric(),
        repayment2 = numeric(),
        repayment3 = numeric(),
        repayment4 = numeric(),
        total_repaid = numeric(),
        total_amount_due = numeric(),
        outstanding = numeric(),
        individual_repayment_rate = numeric(),
        disqualification_reason = character(),
        group_eligible = character()
      )
      showToast("warning", "No dataset available. Using empty structure.")
    }
  })
  
  # Data table snapshot
  output$data_table <- renderDT({
    req(rv$df)
    datatable(
      rv$df |> select(district, parish, group, group_enterprise, farmerid, name, age, gender, 
                      loanamount, repayment1, repayment2, repayment3, repayment4, 
                      total_repaid, total_amount_due, outstanding, individual_repayment_rate,
                      disqualification_reason, group_eligible),
      options = list(pageLength = 6, scrollX = TRUE)
    )
  })
  
  output$total_loan_box <- renderValueBox({
    req(rv$df)
    valueBox(
      paste0("UGX ", format(round(sum(rv$df$loanamount, na.rm = TRUE)), scientific = FALSE, big.mark = ",")), 
      "Total Loans Distributed", 
      icon = icon("coins"), 
      color = "blue"
    )
  })
    
  output$total_repaid_box <- renderValueBox({
    req(rv$df)
    valueBox(
      paste0("UGX ", format(sum(rv$df$total_repaid, na.rm = TRUE), big.mark = ",")), 
      "Total Repaid", 
      icon = icon("hand-holding"), 
      color = "green"
    )
  })
  
    output$repayment_rate_box <- renderValueBox({
    req(rv$df)
    
    #Todo: Filter only farmers who were actually given loans (loanamount > 0)
    funded_farmers <- rv$df |> filter(loanamount > 0)
    
    total_due <- sum(funded_farmers$total_amount_due, na.rm = TRUE)
    total_repaid <- sum(funded_farmers$total_repaid, na.rm = TRUE)
    rate <- ifelse(total_due == 0, 0, round(100 * total_repaid / total_due, 2))
    
    valueBox(
      paste0(rate, "%"), 
      "Overall Repayment Rate", 
      icon = icon("percentage"), 
      color = ifelse(rate > 70, "green", ifelse(rate > 40, "yellow", "red"))
    )
  })
  
  output$eligible_farmers_box <- renderValueBox({
    req(rv$df)
    # count the farmers who got the funding from PDM
    
    eligible <- sum(rv$df$outstanding <= 0, na.rm = TRUE)
    total <- nrow(rv$df)
    
    valueBox(
      paste0(eligible, "/", total), 
      "Funded Farmers vs Total Registered", 
      icon = icon("user-check"), 
      color = "purple"
    )
  })
  
  # Dashboard Plots
  output$plot_enterprise_loan <- renderPlot({
    req(rv$df)
    dat <- get_loan_distribution_enterprise(rv$df)
    
    ggplot(dat, aes(x = reorder(group_enterprise, -total_loans), y = total_loans)) + 
      geom_col(fill = "steelblue", alpha = 0.8) +
      labs(title = "Loan Distribution by Enterprise Type", 
           x = "Enterprise Type", 
           y = "Total Loans (UGX)") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$plot_district_repayment <- renderPlot({
    req(rv$df)
    dat <- get_repayment_by_district(rv$df)
    
    ggplot(dat, aes(x = reorder(district, -repayment_rate), y = repayment_rate)) + 
      geom_col(fill = "darkgreen", alpha = 0.8) +
      labs(title = "Repayment Rate by District", 
           x = "District", 
           y = "Repayment Rate (%)") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Top groups table
  output$top_groups_table <- renderDT({
    req(rv$df)
    datatable(
      generate_parish_performance(rv$df) |> 
        head(10) |>
        select(rank, district, parish, group, group_enterprise, total_farmers, 
               total_loan_amount, total_repaid, repayment_rate),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # Manage: populate farmer selectize choices
  observe({
    req(rv$df)
    choices <- paste0(rv$df$farmerid, " - ", rv$df$name)
    names(choices) <- rv$df$farmerid
    updateSelectizeInput(session, "sel_farmer", choices = choices, server = TRUE)
  })
  
  # Register farmer action
  observeEvent(input$register_btn, {
    req(input$reg_farmerid, input$reg_name)
    
    # Prevent duplicate ID
    if (input$reg_farmerid %in% rv$df$farmerid) {
      showToast("warning", "Farmer ID already exists. Choose a unique ID.")
      return()
    }
    
    rv$df <- register_farmer_df(
      rv$df, input$reg_district, input$reg_parish, input$reg_group, 
      input$reg_farmerid, input$reg_name, input$reg_age, input$reg_gender, 
      input$reg_enterprise, input$reg_loan
    )
    showToast("success", paste0("Registered ", input$reg_name))
  })
  
  # Repayment recording
  observeEvent(input$repay_btn, {
    req(input$sel_farmer, input$rep_amount)
    farmerid <- strsplit(input$sel_farmer, " - ")[[1]][1]
    
    rv$df <- update_repayment_df(
      rv$df, farmerid, as.integer(input$instalment), as.numeric(input$rep_amount)
    )
    showToast("success", paste0("Recorded UGX ", format(as.numeric(input$rep_amount), big.mark = ","), 
                               " for ", farmerid))
  })
  
  # Bulk payment demo
  observeEvent(input$bulk_payment_demo, {
    req(rv$df)
    sample_farmers <- sample(rv$df$farmerid, min(10, nrow(rv$df)))
    
    for (fid in sample_farmers) {
      rv$df <- update_repayment_df(
        rv$df, fid, sample(1:4, 1), sample(c(250000, 265000, 300000), 1)
      )
    }
    showToast("info", "bulk payments applied to 10 -12 farmers")
  })
  
  # Farmers table
  output$farmers_table <- renderDT({
    req(rv$df)
    datatable(
      rv$df |> select(district, parish, group, group_enterprise, farmerid, name, age, gender, 
                      loanamount, repayment1, repayment2, repayment3, repayment4, 
                      total_repaid, total_amount_due, outstanding, individual_repayment_rate,
                      disqualification_reason),
      selection = 'single', 
      editable = FALSE, 
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # SUMMARY REPORTS
  output$district_summary_report <- renderDT({
    req(rv$df)
    datatable(
      generate_district_summary(rv$df),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$parish_performance_report <- renderDT({
    req(rv$df)
    datatable(
      generate_parish_performance(rv$df) |> 
        select(rank, district, parish, group, group_enterprise, total_farmers, 
               total_loan_amount, total_repaid, repayment_rate, fully_repaid_farmers),
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  output$eligible_farmers_report <- renderDT({
    req(rv$df)
    datatable(
      get_eligible_farmers(rv$df),
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  # DATA VISUALIZATION
  output$viz_enterprise_loans <- renderPlot({
    req(rv$df)
    dat <- get_loan_distribution_enterprise(rv$df)
    
    ggplot(dat, aes(x = reorder(group_enterprise, total_loans), y = total_loans)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      coord_flip() +
      labs(title = "Loan Distribution by Enterprise Type",
           x = "Enterprise Type",
           y = "Total Loans (UGX)") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$viz_parish_repayment <- renderPlot({
    req(rv$df)
    dat <- get_repayment_by_parish(rv$df) |> head(15)
    
    ggplot(dat, aes(x = reorder(parish, repayment_rate), y = repayment_rate)) +
      geom_col(fill = "darkgreen", alpha = 0.8) +
      coord_flip() +
      labs(title = "Top 15 Parishes by Repayment Rate",
           x = "Parish",
           y = "Repayment Rate (%)") +
      theme_minimal()
  })
  
  # Todo: Add Pie chart for repayment distribution instead of a bar chart referencing only funded farmers
  output$viz_repayment_pie <- renderPlotly({
    req(rv$df)
    
    repayment_status <- rv$df |>
      filter(loanamount > 0) |>  # Only funded farmers
      mutate(
        status = case_when(
          individual_repayment_rate == 100 ~ "Fully Repaid",
          individual_repayment_rate >= 75 ~ "75-99% Repaid",
          individual_repayment_rate >= 50 ~ "50-74% Repaid",
          individual_repayment_rate >= 25 ~ "25-49% Repaid",
          individual_repayment_rate > 0 ~ "1-24% Repaid",
          TRUE ~ "Not Started"
        )
      ) |>
      count(status) |>
      mutate(percentage = round(100 * n / sum(n), 1))
    
    colors <- c("#2E8B57", "#3CB371", "#90EE90", "#FFD700", "#FFA500", "#FF4500")
    
    plot_ly(repayment_status, labels = ~status, values = ~n, type = 'pie',
            textinfo = 'label+percent',
            hoverinfo = 'text+percent',
            text = ~paste(n, "farmers"),
            marker = list(colors = colors)) |>
      layout(title = "Repayment Status Distribution",
             showlegend = TRUE)
  })
  
  # Todo: add the farmer groups  which were disqualified and their reasons
  output$viz_disqualification_reasons <- renderPlot({
    req(rv$df)
    dat <- get_disqualification_summary(rv$df)
    
    ggplot(dat, aes(x = reorder(disqualification_reason, total_farmers), y = total_farmers)) +
      geom_col(fill = "coral", alpha = 0.8) +
      coord_flip() +
      labs(title = "Disqualification Reasons",
           x = "Disqualification Reason",
           y = "Number of Farmers") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$viz_top_groups <- renderPlot({
    req(rv$df)
    dat <- generate_parish_performance(rv$df) |> head(10)
    
    ggplot(dat, aes(x = reorder(group, repayment_rate), y = repayment_rate)) +
      geom_col(fill = "purple", alpha = 0.8) +
      coord_flip() +
      labs(title = "Top 10 Performing Groups by Repayment Rate",
           x = "Group",
           y = "Repayment Rate (%)") +
      theme_minimal()
  })
  
  output$viz_loan_repayment_scatter <- renderPlot({
    req(rv$df)
    ggplot(rv$df, aes(x = loanamount, y = total_repaid, color = individual_repayment_rate)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      scale_color_gradient(low = "red", high = "green", name = "Repayment Rate (%)") +
      labs(title = "Loan Amount vs Total Repaid",
           x = "Loan Amount (UGX)",
           y = "Total Repaid (UGX)") +
      theme_minimal() +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma)
  })
  
  # Todo Action: Add Eligible Groups Tab on the navigation menu
  output$eligible_groups_box <- renderValueBox({
    req(rv$df)
    eligible_groups <- get_eligible_groups(rv$df)
    total_groups <- n_distinct(eligible_groups$group)
    
    valueBox(
      total_groups, 
      "Eligible Groups (Policy Limit)", 
      icon = icon("users"), 
      color = "green"
    )
  })
  
  output$eligible_farmers_count_box <- renderValueBox({
    req(rv$df)
    eligible_groups <- get_eligible_groups(rv$df)
    total_farmers <- sum(eligible_groups$total_farmers)
    
    valueBox(
      total_farmers, 
      "Farmers in Eligible Groups", 
      icon = icon("user-friends"), 
      color = "blue"
    )
  })
  
  output$eligible_loan_amount_box <- renderValueBox({
    req(rv$df)
    eligible_groups <- get_eligible_groups(rv$df)
    total_amount <- sum(eligible_groups$total_requested_loan)
    
    valueBox(
      paste0("UGX ", format(total_amount, big.mark = ",")), 
      "Total Requested Amount", 
      icon = icon("money-bill-wave"), 
      color = "orange"
    )
  })
  
  output$eligible_groups_table <- renderDT({
    req(rv$df)
    datatable(
      get_eligible_groups(rv$df),
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  output$disqualification_summary_table <- renderDT({
    req(rv$df)
    datatable(
      get_disqualification_summary(rv$df),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # Todo Actiont: Add Quarterly Analysis tab
  output$q1_achievement_box <- renderValueBox({
    req(rv$df)
    quarterly <- get_quarterly_repayment_analysis(rv$df)
    q1_rate <- quarterly$achievement_rate[1]
    
    valueBox(
      paste0(q1_rate, "%"), 
      "Q1 Achievement Rate", 
      icon = icon("chart-line"), 
      color = ifelse(q1_rate > 80, "green", ifelse(q1_rate > 50, "yellow", "red"))
    )
  })
  
  output$q2_achievement_box <- renderValueBox({
    req(rv$df)
    quarterly <- get_quarterly_repayment_analysis(rv$df)
    q2_rate <- quarterly$achievement_rate[2]
    
    valueBox(
      paste0(q2_rate, "%"), 
      "Q2 Achievement Rate", 
      icon = icon("chart-line"), 
      color = ifelse(q2_rate > 80, "green", ifelse(q2_rate > 50, "yellow", "red"))
    )
  })
  
  output$q3_achievement_box <- renderValueBox({
    req(rv$df)
    quarterly <- get_quarterly_repayment_analysis(rv$df)
    q3_rate <- quarterly$achievement_rate[3]
    
    valueBox(
      paste0(q3_rate, "%"), 
      "Q3 Achievement Rate", 
      icon = icon("chart-line"), 
      color = ifelse(q3_rate > 80, "green", ifelse(q3_rate > 50, "yellow", "red"))
    )
  })
  
  output$q4_achievement_box <- renderValueBox({
    req(rv$df)
    quarterly <- get_quarterly_repayment_analysis(rv$df)
    q4_rate <- quarterly$achievement_rate[4]
    
    valueBox(
      paste0(q4_rate, "%"), 
      "Q4 Achievement Rate", 
      icon = icon("chart-line"), 
      color = ifelse(q4_rate > 80, "green", ifelse(q4_rate > 50, "yellow", "red"))
    )
  })
  
  output$quarterly_repayment_plot <- renderPlot({
    req(rv$df)
    quarterly <- get_quarterly_repayment_analysis(rv$df)
    
    quarterly_long <- quarterly |>
      select(quarter, expected, actual) |>
      pivot_longer(cols = c(expected, actual), names_to = "type", values_to = "amount")
    
    ggplot(quarterly_long, aes(x = quarter, y = amount, fill = type)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("expected" = "lightblue", "actual" = "steelblue")) +
      labs(title = "Quarterly Repayment: Expected vs Actual",
           x = "Quarter",
           y = "Amount (UGX)",
           fill = "Type") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$quarterly_repayment_table <- renderDT({
    req(rv$df)
    datatable(
      get_quarterly_repayment_analysis(rv$df) |>
        mutate(
          expected = format(expected, big.mark = ","),
          actual = format(actual, big.mark = ","),
          shortfall = format(shortfall, big.mark = ",")
        ),
      options = list(pageLength = 4, scrollX = TRUE, dom = 't')
    )
  })
  
    output$quarterly_achievement_plot <- renderPlotly({
    req(rv$df)
    quarterly <- get_quarterly_repayment_analysis(rv$df)
    
    # Todo Action: Create a line plot comparing expected vs actual repayments
    quarterly_long <- quarterly |>
      select(quarter, expected, actual) |>
      pivot_longer(cols = c(expected, actual), names_to = "type", values_to = "amount")
    
    plot_ly(quarterly_long, x = ~quarter, y = ~amount, color = ~type, type = 'scatter', mode = 'lines+markers',
            line = list(width = 3),
            marker = list(size = 8),
            text = ~paste("Quarter:", quarter, "<br>Type:", type, "<br>Amount: UGX", format(amount, big.mark = ",")),
            hoverinfo = 'text') |>
      layout(title = "Quarterly Repayment: Expected vs Actual",
            xaxis = list(title = "Quarter"),
            yaxis = list(title = "Amount (UGX)", tickformat = ",.0f"))
  })

  # Credits table output
output$credits_table <- renderDT({
  credits_data <- data.frame(
    ID = c("250262", "212154", "255181", "241633", "247343", "244369"),
    Name = c("Giibwa Joyce Vivian", "Nakanwagi Jalia", "Ekyatuhaire Josephine", "Kisige Erick", "Kibendo Edrin", "Ssozi Paul"),
    Role = c("Documentation / Researcher", "Researcher", "UI Developer", "Backend Developer", "Data Acquisition/QA Tester", "Documentation"),
    Description = c(
      "Overall project coordination and design",
      "Data processing and analysis functions", 
      "User interface design and implementation",
      "Server logic and reactive programming",
      "Testing and validation",
      "Documentation and user manual"
    )
  )
  
  datatable(
    credits_data,
    options = list(
      pageLength = 6,
      dom = 't',
      scrollX = TRUE
    ),
    rownames = FALSE
  )
})
  
  # Download handlers for reports
  output$download_district_summary <- downloadHandler(
    filename = function() { paste0('district_summary_', Sys.Date(), '.csv') },
    content = function(file) { write.csv(generate_district_summary(rv$df), file, row.names = FALSE) }
  )
  
  output$download_parish_performance <- downloadHandler(
    filename = function() { paste0('parish_performance_', Sys.Date(), '.csv') },
    content = function(file) { write.csv(generate_parish_performance(rv$df), file, row.names = FALSE) }
  )
  
  output$download_eligible_farmers <- downloadHandler(
    filename = function() { paste0('eligible_farmers_', Sys.Date(), '.csv') },
    content = function(file) { write.csv(get_eligible_farmers(rv$df), file, row.names = FALSE) }
  )
}

# Todo: Run the application on port 9000 // run the application on localhost:9000 or 127.0.0.1:9000
shinyApp(ui, server, options = list(port = 9000, host = "0.0.0.0"))

