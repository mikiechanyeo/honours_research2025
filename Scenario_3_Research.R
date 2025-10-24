library(tidyverse)
library(quantmod)  # For stock price data
library(rvest)     # For web scraping
library(httr)      # For API requests
library(readxl)    # For reading Excel files
library(lubridate) # For date handling
library(zoo)
library(tidyverse)

################################################################################################
#SQ Group stock price - C6L.SI
################################################################################################


# Original code to get SIA stock data
getSymbols("C6L.SI", src = "yahoo", 
           from = "2010-01-01", 
           to = Sys.Date(),
           auto.assign = TRUE)

# Get SGD/USD exchange rate data for the same period
getSymbols("SGD=X", src = "yahoo", 
           from = "2010-01-01", 
           to = Sys.Date(),
           auto.assign = TRUE)

# Convert SIA data to dataframe with daily data
sia_stock_daily <- data.frame(Date = index(C6L.SI), coredata(C6L.SI))
names(sia_stock_daily) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

# Convert exchange rate data to dataframe
exchange_daily <- data.frame(Date = index(`SGD=X`), coredata(`SGD=X`))
names(exchange_daily) <- c("Date", "Open.FX", "High.FX", "Low.FX", "Close.FX", "Adjusted.FX")

# Merge the datasets by date
merged_daily <- merge(sia_stock_daily, exchange_daily, by = "Date", all.x = TRUE)

# Convert prices to USD
# Note: SGD=X gives us how many USD we get for 1 SGD
merged_daily$Open.USD <- merged_daily$Open * merged_daily$Close.FX
merged_daily$High.USD <- merged_daily$High * merged_daily$Close.FX
merged_daily$Low.USD <- merged_daily$Low * merged_daily$Close.FX
merged_daily$Close.USD <- merged_daily$Close * merged_daily$Close.FX
merged_daily$Adjusted.USD <- merged_daily$Adjusted / merged_daily$Close.FX

# Handle the monthly data conversion
# Convert to monthly data (last trading day of each month)
sia_stock_monthly <- to.monthly(C6L.SI, indexAt = "lastof", OHLC = TRUE)
sia_stock_monthly <- data.frame(Date = index(sia_stock_monthly), coredata(sia_stock_monthly))
names(sia_stock_monthly) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

# Convert exchange rate to monthly (also last trading day)
exchange_monthly <- to.monthly(`SGD=X`, indexAt = "lastof", OHLC = TRUE)
exchange_monthly <- data.frame(Date = index(exchange_monthly), coredata(exchange_monthly))
names(exchange_monthly) <- c("Date", "Open.FX", "High.FX", "Low.FX", "Close.FX", "Volume.FX", "Adjusted.FX")

# Merge monthly datasets
merged_monthly <- merge(sia_stock_monthly, exchange_monthly, by = "Date", all.x = TRUE)

# Convert monthly prices to USD
merged_monthly$Open.USD <- merged_monthly$Open / merged_monthly$Close.FX
merged_monthly$High.USD <- merged_monthly$High / merged_monthly$Close.FX
merged_monthly$Low.USD <- merged_monthly$Low / merged_monthly$Close.FX
merged_monthly$Close.USD <- merged_monthly$Close / merged_monthly$Close.FX
merged_monthly$Adjusted.USD <- merged_monthly$Adjusted / merged_monthly$Close.FX

# Format date to be first day of month as in your original code
merged_monthly$Date <- as.Date(format(merged_monthly$Date, "%Y-%m-01"))

merged_monthly_simple <- data.frame(
  Date = merged_monthly$Date,
  Adjusted_USD = merged_monthly$Adjusted.USD
)

################################################################################################
#Crude oil prices - Brent, Dubai, WTI
################################################################################################

# Set the file path to downloaded Pink Sheet
pink_sheet_path <- "OS_Master.xlsx" 

oil_data <- read_excel(
  pink_sheet_path,
  sheet = "Oil Prices",     
  range = "A1:G187",      
  col_types = c(
    "text",                     # Date
    "numeric",                  # Month
    "numeric",                  # Year
    "numeric",                  # Crude oil, average
    "numeric",                  # Crude oil, Brent
    "numeric",                  # Crude oil, Dubai
    "numeric"                # Crude oil, WTI
    
  )
)

colnames(oil_data) <- c(
  "Date", "Month", "Year",
  "Crude_Oil_Average", "Crude_Oil_Brent", 
  "Crude_Oil_Dubai", "Crude_Oil_WTI"
)

# Create a proper Date column in YYYY-MM-01 format
oil_data <- oil_data %>%
  mutate(
    # Create a proper date column in YYYY-MM-01 format
    Date = as.Date(paste(Year, Month, "01", sep = "-")),
    # Create a Year-Month column for easier matching with other datasets
    YearMonth = paste(Year, sprintf("%02d", Month), sep = "-")
  )


################################################################################################
#SQ Group Operating Statistics
################################################################################################

sqos_path <- "OS_Master.xlsx" 

sia_ops_stats <- read_excel(
  sqos_path,
  sheet = "SIA Group OS",     # 
  range = "A1:G187",          # 
  col_types = c(
    "text",                   # MMM-YY Month
    "numeric",                # Month number
    "numeric",                # Year
    "numeric",                # Available seat-km (million)
    "numeric",                # Revenue passenger-km (million)
    "numeric",                # Passengers carried (thousand)
    "numeric"                 # Passenger load factor (%)
  )
)

# Clean up the data - standardize column names
colnames(sia_ops_stats) <- c(
  "Month_Code", "Month_Num", "Year", 
  "SQ_ASK_million", "SQ_RPK_million", 
  "SQ_Passengers_thousand", "SQ_PLF_percent"
)

sia_ops_stats <- sia_ops_stats %>%
  mutate(
    Date = as.Date(paste(Year, Month_Num, "01", sep = "-")),
    # Create a Year-Month column for easier matching with other datasets
    YearMonth = paste(Year, sprintf("%02d", Month_Num), sep = "-")
  )

# Reorder columns to have Date at the beginning
sia_ops_stats <- sia_ops_stats %>%
  dplyr::select(Date, YearMonth, everything())

################################################################################################
# OECD+6NME industrial production data
################################################################################################

# Read the WIP data from OS_Master.xlsx
wip_data <- read_excel(
  path = "OS_Master.xlsx",
  sheet = "WIP",
  range = "A1:B187",  # Adjust this range as needed to capture all your data
  col_types = c(
    "text",      # Date in YYYYMDD format
    "numeric"    # OECD+6NME industrial production value
  )
)

# Clean up the data - standardize column names
colnames(wip_data) <- c(
  "Date_Code", 
  "OECD_6NME_industrial_production"
)

# Convert the Date_Code to proper Date format
wip_data <- wip_data %>%
  mutate(
    Year = as.numeric(substr(Date_Code, 1, 4)),
    Month = as.numeric(substr(Date_Code, 6, 7)),
    Date = as.Date(paste(Year, Month, "01", sep = "-")),
    YearMonth = paste(Year, sprintf("%02d", Month), sep = "-")
  ) %>%
  # Reorder columns
  dplyr::select(Date, YearMonth, OECD_6NME_industrial_production, everything())


################################################################################################
#Merge Datasets
################################################################################################
options(download.file.method = "wininet")
getSymbols("CPIAUCSL", src = "FRED", from = "2009-01-01", to = Sys.Date())

# ===== FETCH U.S. CPI DATA =====
# Get U.S. CPI data for all urban consumers (CPIAUCSL)
getSymbols("CPIAUCSL", src = "FRED", 
           from = "2009-01-01",  
           to = Sys.Date())

# Convert to monthly data format that matches our existing data
cpi_data <- data.frame(Date = index(CPIAUCSL), CPI = coredata(CPIAUCSL))

# The CPI data is typically reported for the last day of the month
# Convert to first day of month format to match our existing data
cpi_data$Date <- as.Date(format(cpi_data$Date, "%Y-%m-01"))


# First merge SIA stock with oil data
combined_data <- merged_monthly_simple %>%
  left_join(oil_data, by = "Date")

# Then merge with operational statistics
final_data <- combined_data %>%
  left_join(sia_ops_stats, by = "Date")

final_data <- final_data %>%
  dplyr::select(-Crude_Oil_Dubai, -Crude_Oil_Brent, -Crude_Oil_WTI, -Month, -Year.x, -YearMonth.x, -YearMonth.y, -Month_Code, -Month_Num, -Year.y)

final_data <- left_join(final_data, cpi_data, by = "Date")

final_data <- final_data %>%
  left_join(wip_data %>% dplyr::select(Date, OECD_6NME_industrial_production), by = "Date")



###############################################################################################
# Prep my data for model - remove NAs 
###############################################################################################
library(BVAR)      # For Bayesian VAR modeling
library(vars)      # For VAR diagnostics
library(ggplot2)   # For plotting
library(tidyverse) # For data manipulation
library(forecast)  # For forecasting functions
library(lubridate) # For date handling

na_count <- sapply(final_data, function(y) sum(is.na(y)))
print(na_count)

final_data_clean <- na.omit(final_data)

# Calculate real oil prices based on academic literature
final_data_clean <- final_data_clean %>%
  dplyr::mutate(
    Real_Crude_Oil_Average = Crude_Oil_Average / CPIAUCSL * 100
  )

summary(final_data_clean)

library(dplyr)
library(lubridate)

df <- final_data_clean  %>%
  mutate(Year = year(Date))

# Group by year and summarize
yearly_summary <- df %>%
  group_by(Year) %>%
  summarise(
    across(
      .cols = c(Adjusted_USD, Crude_Oil_Average, SQ_ASK_million, SQ_RPK_million,
                SQ_Passengers_thousand, SQ_PLF_percent, CPIAUCSL, Real_Crude_Oil_Average),
      .fns = list(
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE),
        Min = ~min(.x, na.rm = TRUE),
        Max = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

View(yearly_summary)

################################################################################################
#Estimate BVAR model
################################################################################################
vars_for_model <- dplyr::select(final_data_clean, 
                                Date, Adjusted_USD, Real_Crude_Oil_Average, 
                                SQ_ASK_million, SQ_RPK_million, SQ_PLF_percent, OECD_6NME_industrial_production )

vars_transformed <- vars_for_model %>%
  dplyr::mutate(
    Stockprice = log(Adjusted_USD),
    Oilprice = log(Real_Crude_Oil_Average),
    ASK = log(SQ_ASK_million),
    PLF = SQ_PLF_percent,
    RPK = log(SQ_RPK_million),
    WIP = log(OECD_6NME_industrial_production)
  )


# Define COVID period (typically considered March 2020 to December 2021)
covid_start <- as.Date("2020-03-01")
covid_end <- as.Date("2021-12-31")


# Original dataset (with COVID)
vars_transformed_full <- vars_transformed

# Dataset excluding COVID period
vars_transformed_no_covid <- vars_transformed %>%
  filter(Date < covid_start)

# Create a matrix of the transformed variables for the BVAR model with proper Cholesky ordering

bvar_data_full <- vars_transformed_full %>%
  dplyr::select(Oilprice, WIP, Stockprice, ASK, PLF, RPK) %>%
  as.matrix()

bvar_data_no_covid <- vars_transformed_no_covid %>%
  dplyr::select(Oilprice, WIP, Stockprice, ASK, PLF, RPK) %>%
  as.matrix()

################################################################################
# Create a BVAR model with 6 lags, using the Minnesota prior
################################################################################
# Original model (with COVID)
bvar_model_full <- bvar(bvar_data_full, 
                        lags = 13,
                        priors = bv_priors(hyper = "auto"),
                        n_draw = 10000,
                        n_burn = 5000,
                        n_thin = 1,
                        verbose = TRUE)

# Model without COVID period
bvar_model_no_covid <- bvar(bvar_data_no_covid, 
                            lags = 6,
                            priors = bv_priors(hyper = "auto"),
                            n_draw = 10000,
                            n_burn = 5000,
                            n_thin = 1,
                            verbose = TRUE)


################################################################################
# CONDITIONAL FORECASTING: Oil Price Shock Impact on Airline Stock for 12 months
################################################################################
library(BVAR)

# Forecast horizon
H <- 36


Y_data <- bvar_data_no_covid
n_vars <- ncol(Y_data)
n_obs <- nrow(Y_data)
n_lags <- 6  # Assuming VAR(6)

# Variable names in order: Oil, WIP, Stock, ASK, PLF, RPK
var_names <- c("Oilprice", "WIP", "Stockprice", "ASK", "PLF", "RPK")

cat("Model Info:\n")
cat("Variables:", n_vars, "\n")
cat("Observations:", n_obs, "\n")
cat("Lags:", n_lags, "\n")
cat("Variable order:", paste(var_names, collapse = ", "), "\n\n")

#get posterior draws
beta_draws <- bvar_model_no_covid$beta
sigma_draws <- bvar_model_no_covid$sigma

#posterior mean of coefficients and covariance
beta_mean <- apply(beta_draws, c(2,3), mean)

sigma_mean <- apply(sigma_draws, c(2,3), mean)

#Check beta matrix structure is as expected
cat("Beta matrix structure:\n")
for (i in 1:min(15, nrow(beta_mean))) {
  if (i == 1) cat("Row", i, ": CONSTANTS\n")
  else if (i <= 7) cat("Row", i, ": LAG 1, Variable", i-1, "\n") 
  else if (i <= 13) cat("Row", i, ": LAG 2, Variable", i-7, "\n")
  else cat("Row", i, ": LAG 3, Variable", i-13, "\n")
}


#Check beta matrix structure is as expected
cat("Beta matrix structure:\n")
for (i in 1:min(15, nrow(beta_mean))) {
  if (i == 1) cat("Row", i, ": CONSTANTS\n")
  else if (i <= 7) cat("Row", i, ": LAG 1, Variable", i-1, "\n") 
  else if (i <= 13) cat("Row", i, ": LAG 2, Variable", i-7, "\n")
  else cat("Row", i, ": LAG 3, Variable", i-13, "\n")
}

cat("Step 1: Posterior means extracted\n")
cat("Beta matrix dimensions:", dim(beta_mean), "\n")
cat("Sigma matrix dimensions:", dim(sigma_mean), "\n\n")

beta_dims <- dim(bvar_model_no_covid$beta)
sigma_dims <- dim(bvar_model_no_covid$sigma)

# Print sigma matrix
cat("Covariance Matrix Σ:\n")
rownames(sigma_mean) <- var_names
colnames(sigma_mean) <- var_names
print(round(sigma_mean, 6))


# Extract constant (first row)
const <- beta_mean[1, ]
names(const) <- var_names

# Extract lag coefficients
B_matrices <- list()
for (lag in 1:n_lags) {
  start_row <- 1 + (lag - 1) * n_vars + 1
  end_row <- start_row + n_vars - 1
  B_matrices[[lag]] <- beta_mean[start_row:end_row, ]
  rownames(B_matrices[[lag]]) <- var_names
  colnames(B_matrices[[lag]]) <- var_names
}

cat("\n\nStep 2: Coefficients extracted\n")
cat("Constants:\n")
print(round(const, 4))

cat("\nB1 matrix (first lag):\n")
print(round(B_matrices[[1]], 4))

# Cholesky decomposition: A = chol(Σ)'
A <- t(chol(sigma_mean))
A_inv <- solve(A)

cat("\n\nStep 3: Structural A matrix (Cholesky identification)\n")
rownames(A) <- var_names
colnames(A) <- var_names
cat("A matrix:\n")
print(round(A, 6))


# Verification that A A' = Σ
verification <- A %*% t(A)
max_diff <- max(abs(verification - sigma_mean))
cat("\nVerification |A A' - Σ|:", max_diff, "\n")
if (max_diff < 1e-10) cat("✓ Perfect decomposition!\n")

cat("\nMax difference from Σ:", max(abs(verification - sigma_mean)), "\n")

# Get last 6 observations from FULL sample for initial conditions
n_obs_full <- nrow(bvar_data_full)
initial_conditions <- list()

for (i in 0:(n_lags-1)) {
  initial_conditions[[i+1]] <- bvar_data_full[n_obs_full - i, ]
  names(initial_conditions[[i+1]]) <- var_names
}

cat("\n\nStep 4: Initial conditions from FULL sample\n")
cat("Full sample size:", n_obs_full, "observations\n")
cat("Using observations", n_obs_full-5, "to", n_obs_full, "as initial conditions\n")
cat("Latest observation Y_T (", n_obs_full, "):\n")
print(round(initial_conditions[[1]], 4))
cat("Previous observation Y_{T-1} (", n_obs_full-1, "):\n")
print(round(initial_conditions[[2]], 4))

################################################################################
# CONDITIONAL FORECASTING: Prolonged Oil Shock (+25% for 24 months), then release
################################################################################

# Forecast horizon: extend so we can see the release period
horizon <- 36            # <- was 24
n_vars  <- ncol(bvar_data_full)
n_lags  <- 6

# Storage
baseline_forecast    <- matrix(NA, nrow = horizon, ncol = n_vars)
conditional_forecast <- matrix(NA, nrow = horizon, ncol = n_vars)

# 3) baseline loop 
for (h in 1:horizon) {
  X_baseline <- c(1)
  for (lag in 1:n_lags) {
    if (h > lag) {
      # use previously forecasted values (lag 1 pulls h-1, lag 2 pulls h-2, ...)
      X_baseline <- c(X_baseline, baseline_forecast[h - lag, ])
    } else {
      # use initial conditions; [[1]] is lag1 (most recent), [[2]] lag2, ...
      X_baseline <- c(X_baseline, initial_conditions[[lag - h + 1]])
    }
  }
  # deterministic reduced-form forecast (zero innovations)
  y_pred <- as.vector(X_baseline %*% beta_mean)
  stopifnot(length(y_pred) == n_vars)
  baseline_forecast[h, ] <- y_pred
}

# =========================
# 0) SCENARIO + HORIZON
# =========================
horizon <- 36  # extend forecast horizon

# Oil constrained at +25% (relative to last observed level) for 24 months, then released
oil_mult_25x24 <- rep(1.25, 24)   # month-by-month multipliers for h = 1..24

last_oil_log <- tail(bvar_data_full[, "Oilprice"], 1)
oil_target_path_25x24 <- last_oil_log + log(oil_mult_25x24)

# =========================
# 1) CONDITIONAL FORECAST UNDER 25x24 PATH
# =========================
n_vars  <- length(var_names)
oil_idx <- which(var_names == "Oilprice")

conditional_forecast <- matrix(NA_real_, nrow = horizon, ncol = n_vars)
colnames(conditional_forecast) <- var_names

lags_c <- initial_conditions  # list of length n_lags, [[1]] most recent

for (h in 1:horizon) {
  
  # Build X (intercept + lag1..lagp) exactly like baseline
  X_uncond <- c(1)
  for (lag in 1:n_lags) {
    if (h > lag) {
      X_uncond <- c(X_uncond, conditional_forecast[h - lag, ])
    } else {
      X_uncond <- c(X_uncond, lags_c[[lag - h + 1]])
    }
  }
  
  y_uncond <- as.vector(X_uncond %*% beta_mean)
  
  # Constrain oil for months 1..24, then release
  if (h <= length(oil_target_path_25x24)) {
    oil_target_h <- oil_target_path_25x24[h]
    denom <- A[oil_idx, oil_idx]
    if (abs(denom) < 1e-10) stop("A[oil,oil] is ~0; check identification.")
    s_h <- (oil_target_h - y_uncond[oil_idx]) / denom
    u_h <- as.numeric(s_h) * A[, oil_idx]
    y_pred <- y_uncond + u_h
  } else {
    y_pred <- y_uncond
  }
  
  conditional_forecast[h, ] <- y_pred
  
  # Roll lags (keep [[1]] = most recent)
  if (n_lags > 1) {
    lags_c[2:n_lags] <- lapply(1:(n_lags - 1), function(i) lags_c[[i]])
  }
  lags_c[[1]] <- y_pred
}

# =========================
# 2) DATES & HISTORICAL SLICES
# =========================
library(lubridate); library(dplyr); library(ggplot2); library(scales)

forecast_start <- max(vars_transformed_full$Date)
cutoff_date    <- forecast_start %m-% months(6)

historical_dates <- vars_transformed_full$Date
historical_stock <- exp(bvar_data_full[, "Stockprice"])
historical_oil   <- exp(bvar_data_full[, "Oilprice"])

historical_stock_df <- data.frame(
  Date = historical_dates, Value = historical_stock, Scenario = "Historical", Series = "Stock Price"
) %>% filter(Date >= cutoff_date)

historical_oil_df <- data.frame(
  Date = historical_dates, Value = historical_oil, Scenario = "Historical", Series = "Oil Price"
) %>% filter(Date >= cutoff_date)

# Forecast dates (36 months)
forecast_dates <- seq(forecast_start %m+% months(1), by = "month", length.out = horizon)

# Release marker is the 24th forecast month
release_date <- forecast_dates[24]

# =========================
# 3) SERIES IN LEVELS (BASELINE & CONDITIONAL)
# =========================

baseline_stock    <- exp(baseline_forecast[, which(var_names == "Stockprice")])
conditional_stock <- exp(conditional_forecast[, which(var_names == "Stockprice")])

baseline_oil      <- exp(baseline_forecast[, oil_idx])
conditional_oil   <- exp(conditional_forecast[, oil_idx])

baseline_stock_df <- data.frame(Date = forecast_dates, Value = baseline_stock,
                                Scenario = "Baseline Forecast", Series = "Stock Price")
conditional_stock_df <- data.frame(Date = forecast_dates, Value = conditional_stock,
                                   Scenario = "25% for 24m Scenario", Series = "Stock Price")

baseline_oil_df <- data.frame(Date = forecast_dates, Value = baseline_oil,
                              Scenario = "Baseline Forecast", Series = "Oil Price")
conditional_oil_df <- data.frame(Date = forecast_dates, Value = conditional_oil,
                                 Scenario = "25% for 24m Scenario", Series = "Oil Price")

# =========================
# 4) ANCHOR FORECASTS WITH LAST HISTORICAL POINT (column alignment fix)
# =========================
# STOCK
last_hist_stock <- tail(historical_stock_df, 1)
baseline_stock_df <- baseline_stock_df %>% mutate(Series = "Stock Price") 
conditional_stock_df <- conditional_stock_df %>% mutate(Series = "Stock Price")

baseline_stock_df    <- rbind(last_hist_stock, baseline_stock_df) %>%
  mutate(Scenario = "Baseline Forecast")
conditional_stock_df <- rbind(last_hist_stock, conditional_stock_df) %>%
  mutate(Scenario = "25% for 24m Scenario")

# OIL
last_hist_oil <- tail(historical_oil_df, 1) 
baseline_oil_df <- baseline_oil_df %>% mutate(Series = "Oil Price") 
conditional_oil_df <- conditional_oil_df %>% mutate(Series = "Oil Price")

baseline_oil_df    <- rbind(last_hist_oil, baseline_oil_df) %>%
  mutate(Scenario = "Baseline Forecast")
conditional_oil_df <- rbind(last_hist_oil, conditional_oil_df) %>%
  mutate(Scenario = "25% for 24m Scenario")

stock_plot_data <- bind_rows(historical_stock_df, baseline_stock_df, conditional_stock_df)
oil_plot_data   <- bind_rows(historical_oil_df,   baseline_oil_df,   conditional_oil_df)

# =========================
# 5) PLOTS
# =========================
p_stock <- ggplot(stock_plot_data, aes(x = Date, y = Value, color = Scenario, linetype = Scenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 1, alpha = 0.7) +
  geom_vline(xintercept = forecast_start %m+% months(1),
             linetype = "dashed", alpha = 0.6, color = "gray50") +
  geom_vline(xintercept = release_date,
             linetype = "dotted", alpha = 0.8, color = "orange") +
  scale_color_manual(values = c(
    "Historical"            = "gray60",
    "Baseline Forecast"     = "steelblue",
    "25% for 24m Scenario"  = "firebrick"
  )) +
  scale_linetype_manual(values = c(
    "Historical"            = "solid",
    "Baseline Forecast"     = "solid",
    "25% for 24m Scenario"  = "solid"
  )) +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title    = "Singapore Airlines Stock Price: Conditional Forecast",
    subtitle = "VAR(6) | Oil fixed at +25% for months 1–24, then released | Pre-COVID parameters",
    x = "Date", y = "Stock Price (USD)",
    caption = "Dashed: forecast start | Dotted: month 24 (oil released)",
    color = "Scenario", linetype = "Scenario"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    legend.title    = element_blank(),
    legend.text = element_text(size = 15),
    legend.key.size = unit(1.2, "lines"),
    legend.spacing.x = unit(0.5, "cm"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(p_stock)

p_oil <- ggplot(oil_plot_data, aes(x = Date, y = Value, color = Scenario, linetype = Scenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 1, alpha = 0.7) +
  geom_vline(xintercept = forecast_start %m+% months(1),
             linetype = "dashed", alpha = 0.6, color = "gray50") +
  geom_vline(xintercept = release_date,
             linetype = "dotted", alpha = 0.8, color = "orange") +
  scale_color_manual(values = c(
    "Historical"            = "gray60",
    "Baseline Forecast"     = "steelblue",
    "25% for 24m Scenario"  = "firebrick"
  )) +
  scale_linetype_manual(values = c(
    "Historical"            = "solid",
    "Baseline Forecast"     = "solid",
    "25% for 24m Scenario"  = "solid"
  )) +
  scale_y_continuous(labels = dollar_format(prefix = "$", largest_with_cents = 100)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title    = "Oil Price Paths: Baseline vs 25% for 24 Months",
    subtitle = "Months 1–24 fixed at +25% relative to last observed level; thereafter evolves endogenously",
    x = "Date", y = "Oil Price (level)",
    caption = "Dashed: forecast start | Dotted: month 24 (oil released)",
    color = "Scenario", linetype = "Scenario"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    legend.title    = element_blank(),
    legend.text = element_text(size = 15),
    legend.key.size = unit(1.2, "lines"),
    legend.spacing.x = unit(0.5, "cm"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(p_oil)

# =========================
# 6) RESULTS TABLE / HEADLINE GAPS (24m window emphasized; horizon=36)
# =========================
results_table <- data.frame(
  Date = forecast_dates,
  Baseline_Stock    = baseline_stock,
  Conditional_Stock = conditional_stock
) %>%
  mutate(pct_gap = 100 * (Conditional_Stock - Baseline_Stock) / Baseline_Stock)

# headline over constrained window (first 24 months)
head_24m <- results_table %>%
  slice(1:24) %>%
  summarise(
    avg_gap_pct   = mean(pct_gap),
    min_gap_pct   = min(pct_gap),
    max_gap_pct   = max(pct_gap),
    end24_gap_pct = last(pct_gap)
  )
print(head_24m)

# optional: full-horizon headline (36m)
head_36m <- results_table %>%
  summarise(
    avg_gap_pct = mean(pct_gap),
    min_gap_pct = min(pct_gap),
    max_gap_pct = max(pct_gap)
  )
print(head_36m)

# ==== Predictive bands for Scenario 3: +25% for 24 months (then release) ====
set.seed(42)
library(mvtnorm)
library(dplyr); library(tibble)
library(ggplot2); library(scales); library(lubridate)

# Basics
H        <- 36L                     # horizon
p        <- n_lags
k        <- length(var_names)
ix_oil   <- which(var_names == "Oilprice")
ix_stock <- which(var_names == "Stockprice")

# Posterior draws
beta_arr  <- bvar_model_no_covid$beta    # draws x (1+k*p) x k
sigma_arr <- bvar_model_no_covid$sigma   # draws x k x k

# Thin for speed if desired
draw_ids <- seq(1, dim(beta_arr)[1], by = 5L)
n_eff    <- length(draw_ids)

# Build X vector (intercept + lag blocks) exactly like your loop
make_X <- function(h, path, lags) {
  x <- c(1)
  for (L in 1:p) {
    if (h > L) x <- c(x, path[h - L, ]) else x <- c(x, lags[[L - h + 1]])
  }
  x
}


# ONE-DRAW predictive simulation (adds shocks; enforces oil for h<=24)
simulate_one_draw_predictive <- function(beta_draw, sigma_draw, oil_target_path_25x24) {
  colnames(beta_draw) <- var_names
  rownames(sigma_draw) <- colnames(sigma_draw) <- var_names
  
  A <- t(chol(sigma_draw))               # Σ = A A'
  Aii <- A[ix_oil, ix_oil]; if (abs(Aii) < 1e-12) stop("A[ii]≈0 for this draw.")
  
  base_path <- matrix(NA_real_, H, k); colnames(base_path) <- var_names
  cond_path <- matrix(NA_real_, H, k); colnames(cond_path) <- var_names
  
  # Baseline predictive: eps_t ~ N(0, Σ) each step
  for (h in 1:H) {
    Xb <- make_X(h, base_path, initial_conditions)
    y0 <- as.vector(Xb %*% beta_draw)
    eps <- mvtnorm::rmvnorm(1, sigma = sigma_draw)
    base_path[h, ] <- y0 + eps
  }
  
  # Conditional predictive: Oil fixed for months 1..24 via structural shocks
  lags_c <- initial_conditions
  for (h in 1:H) {
    Xc <- make_X(h, cond_path, lags_c)
    y0 <- as.vector(Xc %*% beta_draw)
    
    if (h <= length(oil_target_path_25x24)) {
      # Draw structural shocks s ~ N(0, I) for all, then set oil to hit target
      s <- rnorm(k)
      s[ix_oil] <- (oil_target_path_25x24[h] - y0[ix_oil]) / Aii
      u <- as.vector(A %*% s)             # reduced-form innovation
      yc <- y0 + u
    } else {
      # After release: standard reduced-form shocks
      eps <- mvtnorm::rmvnorm(1, sigma = sigma_draw)
      yc  <- y0 + eps
    }
    
    cond_path[h, ] <- yc
    if (p > 1) lags_c[2:p] <- lapply(1:(p-1), function(i) lags_c[[i]])
    lags_c[[1]] <- yc
  }
  list(base = base_path, cond = cond_path)
}

# Run across draws & collect Stockprice (log) paths
base_stock_draws <- array(NA_real_, dim = c(H, n_eff))
cond_stock_draws <- array(NA_real_, dim = c(H, n_eff))

pb <- utils::txtProgressBar(min = 0, max = n_eff, style = 3)
for (j in seq_along(draw_ids)) {
  d <- draw_ids[j]
  sim <- simulate_one_draw_predictive(beta_arr[d, , ], sigma_arr[d, , ], oil_target_path_25x24)
  base_stock_draws[, j] <- sim$base[, ix_stock]
  cond_stock_draws[, j] <- sim$cond[, ix_stock]
  utils::setTxtProgressBar(pb, j)
}
close(pb)

# Quantiles (in levels)
probs <- c(0.10, 0.50, 0.90)  # 80% bands; use (0.05,0.50,0.95) for 90%
qmat <- function(Mlog) apply(exp(Mlog), 1, stats::quantile, probs = probs, na.rm = TRUE)

Q_base <- qmat(base_stock_draws)  # 3 x H
Q_cond <- qmat(cond_stock_draws)

# Bands dataframe
forecast_start <- max(vars_transformed_full$Date)
forecast_dates <- seq(forecast_start %m+% months(1), by = "month", length.out = H)

as_df <- function(Q, scen) tibble(
  Date = forecast_dates, lo = Q[1,], med = Q[2,], hi = Q[3,], Scenario = scen
)
bands_df <- dplyr::bind_rows(
  as_df(Q_base, "Baseline Predictive"),
  as_df(Q_cond, "25% for 24m Predictive")
)

# Historical tail (last 6 months), in levels
cutoff_date <- forecast_start %m-% months(6)
historical_df <- tibble(
  Date = vars_transformed_full$Date,
  Stock_Price = exp(bvar_data_full[, "Stockprice"])
) %>% dplyr::filter(Date >= cutoff_date)

# Connect median lines to last observed point (avoid gap)
last_hist <- historical_df %>% dplyr::slice_tail(n = 1)
median_lines <- bands_df %>%
  dplyr::select(Date, med, Scenario) %>%
  dplyr::bind_rows(
    tibble(Date = last_hist$Date, med = last_hist$Stock_Price, Scenario = "Baseline Predictive"),
    tibble(Date = last_hist$Date, med = last_hist$Stock_Price, Scenario = "25% for 24m Predictive")
  ) %>%
  dplyr::arrange(Scenario, Date)

# Markers
release_date <- forecast_dates[24]  # month 24 (oil still fixed); first free month is 25

# Plot
ggplot() +
  geom_ribbon(data = bands_df %>% dplyr::filter(Scenario == "Baseline Predictive"),
              aes(x = Date, ymin = lo, ymax = hi, fill = Scenario), alpha = 0.20) +
  geom_ribbon(data = bands_df %>% dplyr::filter(Scenario == "25% for 24m Predictive"),
              aes(x = Date, ymin = lo, ymax = hi, fill = Scenario), alpha = 0.20) +
  geom_line(data = median_lines, aes(x = Date, y = med, color = Scenario), linewidth = 1.1) +
  geom_line(data = historical_df, aes(x = Date, y = Stock_Price, color = "Historical"), linewidth = 1.1) +
  geom_point(data = historical_df, aes(x = Date, y = Stock_Price, color = "Historical"), size = 1, alpha = 0.7) +
  geom_vline(xintercept = forecast_start %m+% months(1), linetype = "dashed", alpha = 0.6, color = "gray50") +
  geom_vline(xintercept = release_date, linetype = "dotted", alpha = 0.8, color = "orange") +
  scale_fill_manual(values = c("Baseline Predictive" = "steelblue",
                               "25% for 24m Predictive" = "firebrick")) +
  scale_color_manual(values = c("Historical" = "gray60",
                                "Baseline Predictive" = "steelblue",
                                "25% for 24m Predictive" = "firebrick")) +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title = "Singapore Airlines Stock Price — Predictive Credible Bands",
    subtitle = "BVAR VAR(6): Parameter + shock uncertainty; Oil fixed at +25% for 24 months, then released",
    x = "Date", y = "Stock Price (USD)",
    caption = "Dashed: forecast start | Dotted: month 24 (oil released)",
    color = "Scenario", fill = "Scenario"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    legend.key.size = unit(1.2, "lines"),
    legend.spacing.x = unit(0.5, "cm"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )


