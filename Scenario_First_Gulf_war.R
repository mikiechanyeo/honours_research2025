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
# Create a BVAR model with 13 lags, using the Minnesota prior
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


# ---- helpers / indices already defined earlier ----
oil_idx   <- which(var_names == "Oilprice")
stock_idx <- which(var_names == "Stockprice")

# --------- Iran 1979 (fast spike, prolonged high) ----------
H           <- 36L           # forecast horizon (show long tail)
S_peak      <- 1.10          # +110% peak
T_peak      <- 6L            # reach peak in 6 months
T_plateau   <- 12L           # hold peak for 12 months (m7–m18)
S_LR        <- 0.60          # glide to +60% by month 24
K_release   <- 24L           # enforce through month 24; release after

# Build an enforced path (length = K_release) in logs
make_oil_target_path <- function(H_path, last_oil_log, S_peak, T_peak, T_plateau, S_LR) {
  s <- numeric(H_path)
  # ramp to peak over T_peak
  if (T_peak > 0) for (h in 1:T_peak) s[h] <- (h / T_peak) * S_peak
  # plateau at peak for T_plateau months
  end_plateau <- min(H_path, T_peak + T_plateau)
  if (T_plateau > 0 && T_peak + 1 <= end_plateau) s[(T_peak + 1):end_plateau] <- S_peak
  # glide toward S_LR over remaining enforced months
  if (end_plateau < H_path) {
    n_rev <- H_path - end_plateau
    for (i in 0:(n_rev - 1)) {
      frac <- (i + 1) / n_rev
      s[end_plateau + i + 1] <- S_peak + frac * (S_LR - S_peak)
    }
  }
  last_oil_log + log1p(s)
}

last_oil_log   <- tail(bvar_data_full[, "Oilprice"], 1)
oil_target_path <- make_oil_target_path(
  H_path = K_release, last_oil_log = last_oil_log,
  S_peak = S_peak, T_peak = T_peak, T_plateau = T_plateau, S_LR = S_LR
)

# ---- Baseline deterministic (zero-innovation) ----
n_vars <- ncol(bvar_data_full)
baseline_forecast <- matrix(NA_real_, nrow = H, ncol = n_vars)
colnames(baseline_forecast) <- var_names

for (h in 1:H) {
  X <- c(1)
  for (lag in 1:n_lags) {
    if (h > lag) X <- c(X, baseline_forecast[h - lag, ]) else X <- c(X, initial_conditions[[lag - h + 1]])
  }
  baseline_forecast[h, ] <- as.vector(X %*% beta_mean)
}

# ---- Conditional deterministic (hard until K_release) ----
conditional_forecast <- matrix(NA_real_, nrow = H, ncol = n_vars)
colnames(conditional_forecast) <- var_names

lags_c <- initial_conditions
denom  <- A[oil_idx, oil_idx]; if (abs(denom) < 1e-10) stop("A[oil,oil]≈0")

for (h in 1:H) {
  X <- c(1)
  for (lag in 1:n_lags) {
    if (h > lag) X <- c(X, conditional_forecast[h - lag, ]) else X <- c(X, lags_c[[lag - h + 1]])
  }
  y0 <- as.vector(X %*% beta_mean)
  
  if (h <= K_release) {
    s_h <- (oil_target_path[h] - y0[oil_idx]) / denom
    u_h <- as.numeric(s_h) * A[, oil_idx]
    y1  <- y0 + u_h
  } else {
    y1 <- y0
  }
  
  conditional_forecast[h, ] <- y1
  if (n_lags > 1) lags_c[2:n_lags] <- lapply(1:(n_lags - 1), function(i) lags_c[[i]])
  lags_c[[1]] <- y1
}

# ---- to levels + dates ----
forecast_start <- max(vars_transformed_full$Date)
forecast_dates <- seq(forecast_start %m+% months(1), by = "month", length.out = H)
release_date   <- forecast_dates[K_release]

baseline_oil      <- exp(baseline_forecast[, oil_idx])
conditional_oil   <- exp(conditional_forecast[, oil_idx])
baseline_stock    <- exp(baseline_forecast[, stock_idx])
conditional_stock <- exp(conditional_forecast[, stock_idx])

hist_cut   <- forecast_start %m-% months(12)
historical_df <- tibble(
  Date = vars_transformed_full$Date,
  Stock_Price = exp(bvar_data_full[, "Stockprice"]),
  Oil_Price   = exp(bvar_data_full[, "Oilprice"])
) %>% filter(Date >= hist_cut)

last_hist_stock <- historical_df %>% slice_tail(n = 1) %>% transmute(Date, Value = Stock_Price)
last_hist_oil   <- historical_df %>% slice_tail(n = 1) %>% transmute(Date, Value = Oil_Price)

stock_df <- bind_rows(
  last_hist_stock %>% mutate(Scenario = "Baseline Forecast"),
  tibble(Date = forecast_dates, Value = baseline_stock,    Scenario = "Baseline Forecast"),
  last_hist_stock %>% mutate(Scenario = "Iran 1979 Scenario"),
  tibble(Date = forecast_dates, Value = conditional_stock, Scenario = "Iran 1979 Scenario")
)

oil_df <- bind_rows(
  last_hist_oil %>% mutate(Scenario = "Baseline Forecast"),
  tibble(Date = forecast_dates, Value = baseline_oil,    Scenario = "Baseline Forecast"),
  last_hist_oil %>% mutate(Scenario = "Iran 1979 Scenario"),
  tibble(Date = forecast_dates, Value = conditional_oil, Scenario = "Iran 1979 Scenario")
)

# ---- Plots (titles updated) ----
p_stock <- ggplot() +
  geom_line(data = historical_df, aes(Date, Stock_Price, color = "Historical"), linewidth = 1) +
  geom_point(data = historical_df, aes(Date, Stock_Price, color = "Historical"), size = 0.9, alpha = 0.7) +
  geom_line(data = stock_df, aes(Date, Value, color = Scenario), linewidth = 1.2) +
  geom_vline(xintercept = forecast_start %m+% months(1), linetype = "dashed", color = "gray50", alpha = .7) +
  geom_vline(xintercept = release_date, linetype = "dotted", color = "orange", alpha = .9) +
  scale_color_manual(values = c("Historical" = "gray60","Baseline Forecast" = "steelblue","Iran 1979 Scenario" = "firebrick")) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title = "Stock Price — Baseline vs Iranian Revolution Shock",
    subtitle = "Oil: +110% in 6m → hold 12m → +60% by m24; hard-conditioned to m24, then released",
    x = "Date", y = "Level (USD)",
    caption = "Dashed: forecast start | Dotted: month 24 (release)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

p_oil <- ggplot() +
  geom_line(data = historical_df, aes(Date, Oil_Price, color = "Historical"), linewidth = 1) +
  geom_point(data = historical_df, aes(Date, Oil_Price, color = "Historical"), size = 0.9, alpha = 0.7) +
  geom_line(data = oil_df, aes(Date, Value, color = Scenario), linewidth = 1.2) +
  geom_vline(xintercept = forecast_start %m+% months(1), linetype = "dashed", color = "gray50", alpha = .7) +
  geom_vline(xintercept = release_date, linetype = "dotted", color = "orange", alpha = .9) +
  scale_color_manual(values = c("Historical" = "gray60","Baseline Forecast" = "steelblue","Iran 1979 Scenario" = "firebrick")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title = "Oil Price — Imposed Iran-Style Path vs Baseline",
    subtitle = "Ramp +110% in 6m → 12m plateau → +60% by m24 | Release after m24",
    x = "Date", y = "Level (USD)",
    caption = "Dashed: forecast start | Dotted: month 24 (release)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

print(p_stock); print(p_oil)

# ---- Headline over enforced window ----
results_table <- tibble(
  Date = forecast_dates,
  Baseline_Stock    = baseline_stock,
  Conditional_Stock = conditional_stock
) %>% mutate(pct_gap = 100 * (Conditional_Stock - Baseline_Stock) / Baseline_Stock)

head_24m <- results_table %>%
  slice(1:K_release) %>%
  summarise(avg_gap_pct = mean(pct_gap), min_gap_pct = min(pct_gap), max_gap_pct = max(pct_gap), end_gap_pct = last(pct_gap))
print(head_24m)
