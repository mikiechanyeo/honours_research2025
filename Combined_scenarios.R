library(tidyverse)
library(quantmod)  # For stock price data
library(rvest)     # For web scraping
library(httr)      # For API requests
library(readxl)    # For reading Excel files
library(lubridate) # For date handling
library(zoo)
library(tidyverse)

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
suppressPackageStartupMessages({
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    message("Tip: install.packages('ggrepel') for nicer end labels; falling back to geom_text.")
    use_repel <- FALSE
  } else {
    library(ggrepel)
    use_repel <- TRUE
  }
})

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
           from = "2009-01-01",  # Start a bit earlier to ensure we have data for our period
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

# ---- Choose which VAR variable to plot (in var_names): "RPK" | "Stockprice" | "ASK" | "PLF" ----
var_to_plot <- "Stockprice"

k          <- length(var_names)
ix_oil     <- which(var_names == "Oilprice")
ix_var     <- which(var_names == var_to_plot)
is_logged  <- !(var_to_plot %in% c("PLF"))  # your PLF isn't logged; others are
horizon    <- 36L                           # long enough to see 24m shocks + release
forecast_start <- max(vars_transformed_full$Date)
forecast_dates <- seq(forecast_start %m+% months(1), by = "month", length.out = horizon)
last_oil_log   <- tail(bvar_data_full[, "Oilprice"], 1)

# =========================
# Helpers
# =========================
library(dplyr)
library(tibble)
library(ggplot2)
library(scales)
library(lubridate)
library(ggrepel)

# Build X vector (intercept + lag blocks) exactly like your loops
make_X_det <- function(h, path, lags, n_lags) {
  x <- c(1)
  for (L in 1:n_lags) {
    if (h > L) x <- c(x, path[h - L, ]) else x <- c(x, lags[[L - h + 1]])
  }
  x
}

# Deterministic (zero-innovation) baseline path
deterministic_baseline <- function(H, k, var_names, n_lags, beta_mean, initial_conditions) {
  out <- matrix(NA_real_, H, k); colnames(out) <- var_names
  for (h in 1:H) {
    X <- make_X_det(h, out, initial_conditions, n_lags)
    out[h, ] <- as.vector(X %*% beta_mean)
  }
  out
}

# Deterministic conditional forecast (hard constraint on Oilprice for 1..K_release)
deterministic_conditional <- function(H, k, var_names, n_lags, beta_mean, A, initial_conditions,
                                      oil_idx, oil_target_path, K_release) {
  out <- matrix(NA_real_, H, k); colnames(out) <- var_names
  lags_c <- initial_conditions
  denom  <- A[oil_idx, oil_idx]; if (abs(denom) < 1e-10) stop("A[oil,oil]≈0; check identification.")
  for (h in 1:H) {
    X  <- make_X_det(h, out, lags_c, n_lags)
    y0 <- as.vector(X %*% beta_mean)
    if (h <= K_release) {
      s_h <- (oil_target_path[h] - y0[oil_idx]) / denom
      u_h <- as.numeric(s_h) * A[, oil_idx]
      y1  <- y0 + u_h
    } else {
      y1 <- y0
    }
    out[h, ] <- y1
    if (n_lags > 1) lags_c[2:n_lags] <- lapply(1:(n_lags - 1), function(i) lags_c[[i]])
    lags_c[[1]] <- y1
  }
  out
}

# Scenario path builders (log targets)
last_oil_log <- tail(bvar_data_full[, "Oilprice"], 1)

# S1: +20% for 12m, release at 12
oil_path_S1 <- function(last_log, K_release = 12L) last_log + log(rep(1.20, K_release))

# S2: “Severe” (30,25,25,20,20,20, then 15%×6) → 12m, release at 12
oil_path_S2 <- function(last_log) {
  mult <- c(1.30, 1.25, 1.25, 1.20, 1.20, 1.20, rep(1.15, 6))
  last_log + log(mult)
}

# S3: +25% for 24m, release at 24
oil_path_S3 <- function(last_log, K_release = 24L) last_log + log(rep(1.25, K_release))

# S4 (NEW IRAN): +110% in 6m → hold 12m → glide to +60% by m24; release at 24
# S4: +110% in 6 months, then hold for 12 months, released at month 20
oil_path_Iran <- function(last_log,
                          K_release = 20L,
                          T_peak = 6L,
                          T_plateau = 12L,
                          S_peak = 1.10) {
  # s_t is the proportional increase (e.g., 1.10 means +110%)
  s <- numeric(K_release)
  
  # Ramp up linearly from 0 to +110% over T_peak months
  if (T_peak > 0) {
    for (h in 1:T_peak) {
      s[h] <- (h / T_peak) * S_peak
    }
  }
  
  # Plateau at +110% for T_plateau months
  end_plateau <- min(K_release, T_peak + T_plateau)
  if (T_plateau > 0 && T_peak + 1 <= end_plateau) {
    s[(T_peak + 1):end_plateau] <- S_peak
  }
  
  # If K_release is longer than ramp+plateau (e.g., 20 > 6+12),
  # keep the plateau level until release.
  if (end_plateau < K_release) {
    s[(end_plateau + 1):K_release] <- S_peak
  }
  
  # Convert proportional targets to log targets for the VAR state
  # log(1 + s_t), since s_t is in "proportion" terms (1.10 = +110%)
  last_log + log1p(s)
}


# S5: Gulf War style: spike +120% in 2m → 1m plateau → glide to +5% by m9; release at 9
oil_path_Gulf <- function(last_log, S_peak = 1.20, T_peak = 2L, T_plateau = 1L, S_LR = 0.05, K_release = 9L) {
  s <- numeric(K_release)
  if (T_peak > 0) for (h in 1:T_peak) s[h] <- (h / T_peak) * S_peak
  end_plateau <- min(K_release, T_peak + T_plateau)
  if (T_plateau > 0 && T_peak + 1 <= end_plateau) s[(T_peak + 1):end_plateau] <- S_peak
  if (end_plateau < K_release) {
    n_rev <- K_release - end_plateau
    for (i in 0:(n_rev - 1)) {
      frac <- (i + 1) / n_rev
      s[end_plateau + i + 1] <- S_peak + frac * (S_LR - S_peak)
    }
  }
  last_log + log1p(s)
}

# =========================
# Build all scenarios
# =========================
H        <- 36L
k        <- length(var_names)
oil_idx  <- which(var_names == "Oilprice")
stk_idx  <- which(var_names == "Stockprice")

# 1) Baseline deterministic (zero-innovation)
baseline_fc <- deterministic_baseline(H, k, var_names, n_lags, beta_mean, initial_conditions)

# 2) Conditional paths
S1_path <- oil_path_S1(last_oil_log, K_release = 12L);    S1_rel <- 12L
S2_path <- oil_path_S2(last_oil_log);                     S2_rel <- 12L
S3_path <- oil_path_S3(last_oil_log, K_release = 24L);    S3_rel <- 24L
S4_path <- oil_path_Iran(last_oil_log, K_release = 20L, T_peak = 6L, T_plateau = 12L, S_peak = 1.10); S4_rel  <- 20L
S5_path <- oil_path_Gulf(last_oil_log, S_peak = 1.20, T_peak = 2L, T_plateau = 1L, S_LR = 0.05, K_release = 9L);   S5_rel <- 9L

S1_fc <- deterministic_conditional(H, k, var_names, n_lags, beta_mean, A, initial_conditions, oil_idx, S1_path, S1_rel)
S2_fc <- deterministic_conditional(H, k, var_names, n_lags, beta_mean, A, initial_conditions, oil_idx, S2_path, S2_rel)
S3_fc <- deterministic_conditional(H, k, var_names, n_lags, beta_mean, A, initial_conditions, oil_idx, S3_path, S3_rel)
S4_fc <- deterministic_conditional(H, k, var_names, n_lags, beta_mean, A, initial_conditions, oil_idx, S4_path, S4_rel)  # NEW IRAN
S5_fc <- deterministic_conditional(H, k, var_names, n_lags, beta_mean, A, initial_conditions, oil_idx, S5_path, S5_rel)

# =========================
# To levels + plotting data
# =========================
forecast_start <- max(vars_transformed_full$Date)                 # last observed (April)
forecast_dates <- seq(forecast_start %m+% months(1), by = "month", length.out = H)  # begins in May

hist_tail_m  <- 12L
historical_df <- tibble(
  Date = vars_transformed_full$Date,
  Stock = exp(bvar_data_full[, "Stockprice"])
) %>% filter(Date >= forecast_start %m-% months(hist_tail_m))

last_hist <- slice_tail(historical_df, n = 1)

make_series <- function(fc_mat, col_idx, scen_name) {
  tibble(Date = forecast_dates, Value = exp(fc_mat[, col_idx]), Scenario = scen_name) %>%
    # prepend last historical point to remove the gap
    bind_rows(tibble(Date = last_hist$Date, Value = last_hist$Stock, Scenario = scen_name), .) %>%
    arrange(Date)
}

plot_df <- bind_rows(
  make_series(baseline_fc, stk_idx, "Baseline"),
  make_series(S1_fc,       stk_idx, "S1: Fixed oil price"),
  make_series(S2_fc,       stk_idx, "S2: Prolonged declining oil price"),
  make_series(S3_fc,       stk_idx, "S3: 24-month oil price spike"),
  make_series(S4_fc,       stk_idx, "S4: Iran-like scenario"),
  make_series(S5_fc,       stk_idx, "S5: Gulf War-like scenario")
)

# Last points for end labels
end_df <- plot_df %>%
  group_by(Scenario) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(label = Scenario)

# Extend x-axis a bit to make room for labels
x_max <- max(plot_df$Date) %m+% months(3)

# Colors
cols <- c(
  "Historical"                    = "gray60",
  "Baseline"                      = "#1f77b4",
  "S1: Fixed oil price"              = "#ff7f0e",
  "S2: Prolonged declining oil price"      = "#17becf",
  "S3: 24-month oil price spike"              = "#9467bd",
  "S4: Iran-like scenario" = "#d62728",
  "S5: Gulf War-like scenario"    = "#2ca02c"
)

# =========================
# Plot — Baseline + 5 scenarios (no bands)
# =========================
# Compute nice Y breaks (≈ every $0.50)
y_rng    <- range(c(historical_df$Stock, plot_df$Value), na.rm = TRUE)
y_breaks <- seq(floor(y_rng[1] / 0.5) * 0.5, ceiling(y_rng[2] / 0.5) * 0.5, by = 0.5)

ggplot() +
  geom_line(data = historical_df, aes(Date, Stock, color = "Historical"), linewidth = 1, alpha = 0.9) +
  geom_point(data = historical_df, aes(Date, Stock, color = "Historical"), size = 0.9, alpha = 0.7) +
  geom_line(data = plot_df, aes(Date, Value, color = Scenario), linewidth = 1.3) +
  geom_vline(xintercept = forecast_start %m+% months(1), linetype = "dashed", color = "gray50", alpha = 0.7) +
  scale_color_manual(values = cols) +
  scale_y_continuous(
    labels = scales::label_dollar(accuracy = 0.01),
    breaks = y_breaks,
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    limits = c(min(historical_df$Date), x_max),
    expand = expansion(mult = c(0.01, 0.07))  # room on the right for labels
  ) +
  geom_label_repel(
    data = end_df,
    aes(Date, Value, label = label, color = Scenario),
    nudge_x = 40,
    direction = "y",
    hjust = 0,
    seed = 123,
    segment.alpha = 0.6,
    box.padding = unit(0.35, "lines"),
    label.padding = unit(0.22, "lines"),
    label.size = 0.25,
    fill = scales::alpha("white", 0.95),
    size = 4.8,
    max.overlaps = Inf
  ) +
  labs(
    title = "Stockprice — Baseline vs Oil-Shock Scenarios",
    subtitle = "Forecasts begin in May (dashed). Hard conditioning during indicated windows; then released.",
    x = "Date", y = "Stockprice",
    caption = "Labels placed at end of each line; last historical point prepended to remove gap."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position   = "none",
    axis.line         = element_line(color = "grey30"),
    axis.ticks        = element_line(color = "grey50"),
    axis.ticks.length = unit(3, "pt"),
    axis.text.x       = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y       = element_text(size = 14),
    axis.title        = element_text(size = 20),
    plot.title        = element_text(size = 20, face = "bold", margin = margin(b = 2)),
    plot.subtitle     = element_text(size = 12, margin = margin(b = 8)),
    plot.caption      = element_text(size = 9, hjust = 1),
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.25),
    panel.grid.major.y = element_line(linewidth = 0.35, colour = "grey88"),
    plot.margin       = margin(12, 52, 28, 12)  # extra right margin for labels
  ) +
  coord_cartesian(clip = "off")
