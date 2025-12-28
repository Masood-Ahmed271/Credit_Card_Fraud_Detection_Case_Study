############################################
# Author: Masood Ahmed
############################################

## Libraries
library(tidyverse)      # For data manipulation and visualization
library(DataExplorer)   # For quick EDA reports
library(Amelia)         # For missing values visualization
library(corrplot)       # For correlation plots
library(ggplot2)        # For plotting
library(caret)          # For pre-processing
library(dplyr)          # Data wrangling
library(scales)         # Pretty number formatting
library(reshape2)       # Reshape for correlation heat-map
library(e1071)          # for skewness detection
library(GGally)         # for including pairwise plot matrix
library(corrplot)       # to display correlation matrix

# Reading the CSV file from the current working directory. Please note that the R script and 
# csv file are in the same directory

# Header is true as first row of the CSV file contains column names
# stringsAsFactors is false so that it doesn't convert strings as factors
dataFrame <- read.csv("A1_data.csv", header = TRUE, stringsAsFactors = FALSE)

# to just get an overview of the data set
str(dataFrame)
dimensions <- dim(dataFrame) # columns = 101 and rows 100,000
columnNames <- names(dataFrame) # to get column names
overviewOfInitialData <- head(dataFrame, 10) #to see the first 10 rows

#─────────────────────────────
# Trying to do initial data exploration to know about the dataset
#─────────────────────────────

## To see the initial isFraud distribution
dataFrame %>%
  count(isFraud) %>%
  mutate(Percentage = n / sum(n)) %>%
  ggplot(aes(x = factor(isFraud), y = n, fill = factor(isFraud))) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(n, " (", percent(Percentage, accuracy = 0.1), ")")),
            vjust = -0.3, size = 5) +
  scale_x_discrete(labels = c("0" = "Not Fraud", "1" = "Fraud")) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  labs(
    title = "Fraud Class Distribution",
    x = "Fraud Flag",
    y = "Number of Transactions"
  ) +
  theme_minimal(base_size = 14)
# Some conclusion:11.3% fraud so sparsed dataset, heavly skewed


## Note: we are using 0 as the start index as we don't exactly know the starting dates from the data set
## Looking at transactions by relative day
txn_daily <- dataFrame %>%
  mutate(
    TxnDay = floor(as.numeric(TxnDT) / (24 * 3600))  # convert seconds → day index
  ) %>%
  group_by(TxnDay) %>%
  summarise(
    FraudSum  = sum(isFraud == 1, na.rm = TRUE),
    TotalTxn  = n(),
    FraudRate = mean(isFraud == 1, na.rm = TRUE),
    .groups   = "drop"
  )

par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))

plot(txn_daily$TxnDay, txn_daily$TotalTxn, type = "l", col = "steelblue",
     main = "Transactions per relative day", xlab = "Day Index (from Start)", ylab = "Transaction Count", lwd = 2)

plot(txn_daily$TxnDay, txn_daily$FraudRate, type = "l", col = "tomato",
     main = "Fraud Rate per Relative Day", xlab = "Day Index (from Start)", ylab = "Fraud Rate", lwd = 2)


## Looking at the transactions and fraud distribution by hour of day distribution
hour_summary <- dataFrame %>%
  mutate(
    Hour = floor((as.numeric(TxnDT) %% (24 * 3600)) / 3600)  # seconds → hour of day [0–23]
  ) %>%
  group_by(Hour) %>%
  summarise(
    total_txn  = n(),
    fraud_rate = mean(isFraud == 1, na.rm = TRUE),
    .groups    = "drop"
  )

ggplot(hour_summary, aes(x = Hour)) +
  geom_col(aes(y = total_txn), fill = "steelblue", alpha = 0.6) +
  geom_line(aes(y = fraud_rate * max(total_txn), group = 1),
            color = "tomato", linewidth = 1.1) +
  scale_y_continuous(
    name = "Transactions",
    sec.axis = sec_axis(~ . / max(hour_summary$total_txn),
                        name = "Fraud Rate")
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Transactions and Fraud Rate by Hour (Relative Time)",
    x = "Hour (0–23)"
  )


## Looking at the transactions and fraud distribution by week distribution
txn_data <- dataFrame %>%
  mutate(
    TxnDayIndex = floor(as.numeric(TxnDT) / (24 * 3600)),
    Weekday = factor(
      TxnDayIndex %% 7,
      labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    )
  )

weekday_summary <- txn_data %>%
  group_by(Weekday) %>%
  summarise(
    total_txn  = n(),
    fraud_rate = mean(isFraud == 1, na.rm = TRUE),
    .groups    = "drop"
  )

ggplot(weekday_summary, aes(x = Weekday)) +
  geom_col(aes(y = total_txn), fill = "steelblue", alpha = 0.6) +
  geom_line(aes(y = fraud_rate * max(total_txn), group = 1),
            color = "tomato", linewidth = 1.1) +
  scale_y_continuous(
    name = "Transactions",
    sec.axis = sec_axis(~ . / max(weekday_summary$total_txn),
                        name = "Fraud Rate")
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Transactions and Fraud Rate by Pseudo‑Weekday (Relative Time)",
    x = "Cyclical Day of Week"
  )


## Looking at the transactions and fraud distribution by month distribution
month_summary <- dataFrame %>%
  mutate(
    MonthIndex = floor(as.numeric(TxnDT) / (30 * 24 * 3600)) + 1
  ) %>%
  group_by(MonthIndex) %>%
  summarise(
    total_txn  = n(),
    fraud_rate = mean(isFraud == 1, na.rm = TRUE),
    .groups    = "drop"
  )

ggplot(month_summary, aes(x = MonthIndex)) +
  geom_col(aes(y = total_txn), fill = "steelblue", alpha = 0.6) +
  geom_line(aes(y = fraud_rate * max(total_txn), group = 1),
            color = "tomato", linewidth = 1.1) +
  scale_y_continuous(
    name = "Transactions",
    sec.axis = sec_axis(~ . / max(month_summary$total_txn),
                        name = "Fraud Rate")
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Transactions and Fraud Rate by Pseudo‑Month (Relative Time)",
    x = "Month Index (30‑Day Intervals)"
  )

## Looking at how isFraud cases are spread over device types
plot_df <- dataFrame %>%
  mutate(
    DevType = ifelse(DevType == "" | is.na(DevType), "unknown", tolower(DevType))
  ) %>%
  group_by(DevType, isFraud) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(plot_df, aes(x = DevType, y = Count, fill = factor(isFraud))) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.6),
    vjust = -0.3, size = 4
  ) +
  scale_fill_manual(
    name = "isFraud",
    values = c("0" = "steelblue", "1" = "tomato"),
    labels = c("0 = Not Fraud", "1 = Fraud")
  ) +
  labs(
    title = "Fraud vs Non‑Fraud by Device Type",
    x = "Device Type",
    y = "Transaction Count"
  ) +
  theme_minimal(base_size = 13)

## fraud and device type relation in terms of percentages
plot_df_pct <- dataFrame %>%
  mutate(
    DevType = ifelse(DevType == "" | is.na(DevType), "unknown", tolower(DevType))
  ) %>%
  group_by(DevType, isFraud) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(DevType) %>%
  mutate(Percent = Count / sum(Count))

ggplot(plot_df_pct, aes(x = DevType, y = Percent, fill = factor(isFraud))) +
  geom_col(position = "fill", width = 0.6) +
  scale_fill_manual(
    name = "isFraud",
    values = c("0" = "steelblue", "1" = "tomato"),
    labels = c("Not Fraud", "Fraud")
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Fraud by Device Type",
    x = "Device Type",
    y = "Percentage of Transactions"
  ) +
  theme_minimal(base_size = 13)
# some insights: Looks evenly spread between mobile and desktop but after looking at the percentages, fraud done by mobile is high

## looking at fraud relation with productcd - type
product_df <- dataFrame %>%
  mutate(ProductCD = ifelse(is.na(ProductCD) | ProductCD == "", "unknown", toupper(ProductCD))) %>%
  group_by(ProductCD, isFraud) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(product_df, aes(x = ProductCD, y = Count, fill = factor(isFraud))) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.6),
    vjust = -0.3, size = 4
  ) +
  scale_fill_manual(
    name = "isFraud",
    values = c("0" = "steelblue", "1" = "tomato"),
    labels = c("0 = Not Fraud", "1 = Fraud")
  ) +
  labs(
    title = "Fraud vs Non‑Fraud Transactions by Product Code",
    x = "Product Code",
    y = "Transaction Count"
  ) +
  theme_minimal(base_size = 13)
# some insight: LY seems highly associated with fraud

## Looking at purchaser email domains and understanding the spread
email_df <- dataFrame %>%
  mutate(
    P_emaildomain = ifelse(is.na(P_emaildomain) | P_emaildomain == "",
                           "unknown",
                           tolower(P_emaildomain))
  ) %>%
  group_by(P_emaildomain) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count)) %>%
  arrange(desc(Percent))

ggplot(email_df, aes(
  x = reorder(P_emaildomain, Percent),
  y = Percent
)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_text(
    aes(label = percent(Percent, accuracy = 0.1)),
    hjust = -0.1, size = 3
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(), 
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Percentage Usage of Purcahser Email Domains",
    x = "Email Domain",
    y = "Share of Transactions"
  ) +
  theme_minimal(base_size = 13)

## Looking at receiver email domains and understanding the spread
r_email_df <- dataFrame %>%
  mutate(
    R_emaildomain = ifelse(is.na(R_emaildomain) | R_emaildomain == "",
                           "unknown",
                           tolower(R_emaildomain))
  ) %>%
  group_by(R_emaildomain) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = Count / sum(Count)) %>%
  arrange(desc(Percent))

ggplot(r_email_df, aes(
  x = reorder(R_emaildomain, Percent),
  y = Percent
)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_text(
    aes(label = percent(Percent, accuracy = 0.1)),
    hjust = -0.1, size = 3
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(), 
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Percentage Usage of Receiver Email Domains",
    x = "Email Domain",
    y = "Share of Transactions"
  ) +
  theme_minimal(base_size = 13)

## Doing some fraud comparison on purchaser and receiver emails
email_fraud_df <- dataFrame %>%
  mutate(
    P_emaildomain = ifelse(is.na(P_emaildomain) | P_emaildomain == "",
                           "unknown",
                           tolower(P_emaildomain))
  ) %>%
  group_by(P_emaildomain, isFraud) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(P_emaildomain) %>%
  mutate(
    Percent = Count / sum(Count)
  ) %>%
  ungroup()

ggplot(email_fraud_df, aes(
  x = reorder(P_emaildomain, -Count),
  y = Count,
  fill = factor(isFraud)
)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(
    name = "isFraud",
    values = c("0" = "steelblue", "1" = "tomato"),
    labels = c("Not Fraud", "Fraud")
  ) +
  labs(
    title = "Fraud vs Non‑Fraud Transactions by Email Domain",
    x = "Email Domain",
    y = "Transaction Count"
  ) +
  coord_flip() +
  theme_minimal(base_size = 13)


r_email_fraud_df <- dataFrame %>%
  mutate(
    R_emaildomain = ifelse(is.na(R_emaildomain) | R_emaildomain == "",
                           "unknown",
                           tolower(R_emaildomain))
  ) %>%
  group_by(R_emaildomain, isFraud) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(R_emaildomain) %>%
  mutate(
    Percent = Count / sum(Count)
  ) %>%
  ungroup()

ggplot(r_email_fraud_df, aes(
  x = reorder(R_emaildomain, -Count),
  y = Count,
  fill = factor(isFraud)
)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(
    name = "isFraud",
    values = c("0" = "steelblue", "1" = "tomato"),
    labels = c("Not Fraud", "Fraud")
  ) +
  labs(
    title = "Fraud vs Non‑Fraud Transactions by Email Domain",
    x = "Email Domain",
    y = "Transaction Count"
  ) +
  coord_flip() +
  theme_minimal(base_size = 13)
# some insights: Seems like Google domain is used a lot for fraud cases

## Looking at the correlation between V features
vars <- c("V310", "V311", "V312", "V313", "V314")
corr_matrix <- cor(dataFrame[, vars], use = "complete.obs")

corrplot(corr_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         col = colorRampPalette(c("tomato", "white", "steelblue"))(200))
# insights: High correlations between V features, seems not all could be useful as they 
# might provide the same information

summary_v312 <- dataFrame %>%
  group_by(isFraud) %>%
  summarise(
    Mean  = mean(V312, na.rm = TRUE),
    Median = median(V312, na.rm = TRUE),
    SD = sd(V312, na.rm = TRUE),
    Count = n()
  )

# seems not useful, can remove
ggplot(dataFrame, aes(x = factor(isFraud), y = V312, fill = factor(isFraud))) +
  geom_boxplot(alpha = 0.6, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("steelblue", "tomato"),
                    labels = c("Not Fraud", "Fraud")) +
  labs(title = "V312 Distribution by Fraud Status",
       x = "isFraud (0 = No, 1 = Yes)", y = "V312 Value") +
  theme_minimal(base_size = 13)

ggplot(dataFrame, aes(x = factor(isFraud), y = V313, fill = factor(isFraud))) +
  geom_boxplot(alpha = 0.6, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("steelblue", "tomato"),
                    labels = c("Not Fraud", "Fraud")) +
  labs(title = "V313 Distribution by Fraud Status",
       x = "isFraud (0 = No, 1 = Yes)", y = "V313 Value") +
  theme_minimal(base_size = 13)
# some insights: Both fraud and non‑fraud groups have very similar distributions — 
# effectively the same dense cluster near 0, with outliers mainly in the non‑fraud group.
# so can remove it

# Looking at how many unique addresses we have
length(unique(na.omit(dataFrame$addr1[dataFrame$addr1 != ""]))) # gave 221 - so probably regions
length(unique(na.omit(dataFrame$addr2[dataFrame$addr2 != ""]))) # gave 68 so most probably country

dataFrame %>%
  filter(!is.na(addr1), addr1 != "") %>%
  count(addr1, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(addr1, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.2) +
  labs(
    title = "Top 10 addr1 (Regions)",
    x = "addr1 Region",
    y = "Transaction Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))
## High frequency for 281

dataFrame %>%
  filter(!is.na(addr2), addr2 != "") %>%
  count(addr2, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(addr2, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.2) +
  labs(
    title = "Top 10 addr2 (Country)",
    x = "addr2 Region",
    y = "Transaction Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))
## Frequency tells 73 is the most common country

addr1_fraud_stats <- dataFrame %>%
  group_by(addr1) %>%
  summarise(
    total_txn  = n(),
    fraud_txn  = sum(isFraud == 1, na.rm = TRUE),
    fraud_rate = mean(isFraud == 1, na.rm = TRUE)
  ) %>%
  arrange(desc(total_txn)) %>%
  slice_head(n = 10)

ggplot(addr1_fraud_stats, aes(x = reorder(addr1, total_txn))) +
  geom_col(aes(y = total_txn), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = fraud_rate * max(total_txn)), color = "tomato", linewidth = 1.2, group = 1) +
  geom_point(aes(y = fraud_rate * max(total_txn)), color = "tomato", size = 2) +
  scale_y_continuous(
    name = "Transaction Count",
    sec.axis = sec_axis(~ . / max(addr1_fraud_stats$total_txn),
                        name = "Fraud Rate (scaled)")
  ) +
  labs(
    title = "Top 10 addr1 Regions – Transaction Count and Fraud Rate",
    x = "addr1 Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

addr2_fraud_stats <- dataFrame %>%
  group_by(addr2) %>%
  summarise(
    total_txn  = n(),
    fraud_txn  = sum(isFraud == 1, na.rm = TRUE),
    fraud_rate = mean(isFraud == 1, na.rm = TRUE)
  ) %>%
  arrange(desc(total_txn)) %>%
  slice_head(n = 10)

ggplot(addr2_fraud_stats, aes(x = reorder(addr2, total_txn))) +
  geom_col(aes(y = total_txn), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = fraud_rate * max(total_txn)), color = "tomato", linewidth = 1.2, group = 1) +
  geom_point(aes(y = fraud_rate * max(total_txn)), color = "tomato", size = 2) +
  scale_y_continuous(
    name = "Transaction Count",
    sec.axis = sec_axis(~ . / max(addr2_fraud_stats$total_txn),
                        name = "Fraud Rate (scaled)")
  ) +
  labs(
    title = "Top 10 addr2 Countries – Transaction Count and Fraud Rate",
    x = "addr2 Countriers"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

## to just get an idea how C1 and C2  columns affect
c1_fraud_stats <- dataFrame %>%
  group_by(C1) %>%
  summarise(
    total_txn  = n(),
    fraud_txn  = sum(isFraud == 1, na.rm = TRUE),
    fraud_rate = mean(isFraud == 1, na.rm = TRUE)
  ) %>%
  arrange(desc(total_txn)) %>%
  slice_head(n = 10)

ggplot(c1_fraud_stats, aes(x = reorder(C1, total_txn))) +
  geom_col(aes(y = total_txn), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = fraud_rate * max(total_txn)), color = "tomato", linewidth = 1.2, group = 1) +
  geom_point(aes(y = fraud_rate * max(total_txn)), color = "tomato", size = 2) +
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(~ . / max(c1_fraud_stats$total_txn),
                        name = "Fraud Rate (scaled)")
  ) +
  labs(
    title = "Top 10 C1 – Count and Fraud Rate",
    x = "C1 Values"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))


c2_fraud_stats <- dataFrame %>%
  group_by(C2) %>%
  summarise(
    total_txn  = n(),
    fraud_txn  = sum(isFraud == 1, na.rm = TRUE),
    fraud_rate = mean(isFraud == 1, na.rm = TRUE)
  ) %>%
  arrange(desc(total_txn)) %>%
  slice_head(n = 10)

ggplot(c2_fraud_stats, aes(x = reorder(C2, total_txn))) +
  geom_col(aes(y = total_txn), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = fraud_rate * max(total_txn)), color = "tomato", linewidth = 1.2, group = 1) +
  geom_point(aes(y = fraud_rate * max(total_txn)), color = "tomato", size = 2) +
  scale_y_continuous(
    name = "Count",
    sec.axis = sec_axis(~ . / max(c2_fraud_stats$total_txn),
                        name = "Fraud Rate (scaled)")
  ) +
  labs(
    title = "Top 10 C2 – Count and Fraud Rate",
    x = "C2 Values"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

#─────────────────────────────
# Handling Missing Values
#─────────────────────────────

## to check number of null/empty values in each column so that we drop not needed columns
missing_vals <- sapply(dataFrame, function(x) sum(is.na(x) | x == ""))
missing_pct <- missing_vals / nrow(dataFrame) * 100
missing_report <- data.frame(Variable = names(missing_vals),
                             Missing = missing_vals,
                             MissingPct = round(missing_pct, 2))
missing_report %>%
  arrange(desc(MissingPct))

# Visualize missing values
missmap(dataFrame, main = "Missing Values Map", col = c("red", "grey"))

#initial checking tells that:
#           1. dist1,M1,M2,M3,M5,M6... has all values null
# so we will remove columns with 90% or more empty or null values
dataFrame <- dataFrame[, colSums(is.na(dataFrame) | (sapply(dataFrame, is.character) & dataFrame == "") | dataFrame == "") < 90000]

# further drop those columns which have identical values
drop_cols <- sapply(dataFrame, function(x) {
  all(is.na(x)) || length(unique(na.omit(x))) <= 1
})

if (any(drop_cols)) {
  cat("Dropping", sum(drop_cols), "constant or all-NA columns:\n")
  print(names(dataFrame)[drop_cols])
  dataFrame <- dataFrame[, !drop_cols]
}

# Looking at some columns to see if they are useful or not and drop if not useful
id_11_investigate <- head(sort(prop.table(table(dataFrame$id_11)) * 100, decreasing = TRUE)) # 95% are values as 100 -> not useful
id_04_investigate <- head(sort(prop.table(table(dataFrame$id_04)) * 100, decreasing = TRUE)) # 99% are values as 0 -> not useful
id_03_investigate <- head(sort(prop.table(table(dataFrame$id_03)) * 100, decreasing = TRUE)) # 96% are values as 0 -> not useful
id_09_investigate <- head(sort(prop.table(table(dataFrame$id_09)) * 100, decreasing = TRUE)) # 93% are values as 0 -> not useful
id_10_investigate <- head(sort(prop.table(table(dataFrame$id_10)) * 100, decreasing = TRUE)) # 97% are values as 0 -> not useful

# further dropping some columns which don't provide much useful information
# which is related to devices, card address etc
cols_to_drop <- c("DevInfo", "id_30", "id_31", "id_33", "dist2", "card1", "card2", "card3", "card5", "M4", "id_11")
dataFrame <- dataFrame %>% select(-all_of(cols_to_drop))

# Dropping some other columns which I think are not useful for my analysis
cols_to_drop <- c("V310", "V311", "V312", "V313", "V314", "id_04", "id_03", "id_05", "id_06", "id_09", "id_10", "TxnID")
dataFrame <- dataFrame %>% select(-all_of(cols_to_drop))

# Also checking if any rows have more than 90% empty values
threshold <- 0.9
row_na_fraction <- apply(dataFrame, 1, function(x) {
  sum(is.na(x) | x == "" | trimws(x) == "") / length(x)
})
rows_high_na <- which(row_na_fraction >= threshold)
cat("Number of rows with ≥ 90% missing values:", length(rows_high_na), "\n")

dimensions <- dim(dataFrame) # columns = 58 and rows 100,000
glimpse(dataFrame)
summary <- summary(dataFrame)
sapply(dataFrame, class)
plot_intro(dataFrame)

## trying to handle the missing values
## Thought process:
  # For numerical values: impute with mean/median depending on skewness of the data
  # For categorical values: impute with mode or create "Unknwon" labels

# to create vectors of with names of the columns
num_vars <- names(dataFrame)[sapply(dataFrame, is.numeric)]
cat_vars <- names(dataFrame)[!sapply(dataFrame, is.numeric)]

# Numeric: median/mean imputation based on skewness
for (col in num_vars) {
  # Computing skewness (ignoring NAs)
  sk <- e1071::skewness(dataFrame[[col]], na.rm = TRUE)
  if (abs(sk) < 1) {
    # roughly symmetric → use mean
    imp_value <- mean(dataFrame[[col]], na.rm = TRUE)
    method <- "mean"
  } else {
    # skewed → use median
    imp_value <- median(dataFrame[[col]], na.rm = TRUE)
    method <- "median"
  }
  
  if (any(is.na(dataFrame[[col]]))) {
    dataFrame[[col]][is.na(dataFrame[[col]])] <- imp_value
  }
  
  cat(sprintf("Imputed column %-20s using %-6s (skewness = %.3f)\n", 
              col, method, sk))
}

# Categorical: replace NA or blank with "Unknown"
for (col in cat_vars) {
  dataFrame[[col]][is.na(dataFrame[[col]]) | dataFrame[[col]] == ""] <- "Unknown"
}

# Sanity check of missing values post-imputation
colSums(is.na(dataFrame))

# rounding down addr so that they are whole numbers after filling NA Values
dataFrame$addr1 <- floor(dataFrame$addr1)
dataFrame$addr2 <- floor(dataFrame$addr2)

#─────────────────────────────
# Outlier Treatment
#─────────────────────────────
num_vars <- names(dataFrame)[sapply(dataFrame, is.numeric)]

outlier_summary <- data.frame(Column = character(),
                              OutlierCount = integer(),
                              OutlierPct = numeric(),
                              stringsAsFactors = FALSE)

for (col in num_vars) {
  Q1 <- quantile(dataFrame[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(dataFrame[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  # Identifying outliers by IQR rule
  outliers <- which(dataFrame[[col]] < (Q1 - 1.5 * IQR_val) | dataFrame[[col]] > (Q3 + 1.5 * IQR_val))
  outlier_pct <- round(length(outliers) / nrow(dataFrame) * 100, 2)
  
  outlier_summary <- rbind(outlier_summary, 
                           data.frame(Column = col, 
                                      OutlierCount = length(outliers),
                                      OutlierPct = outlier_pct))
}

head(outlier_summary[order(-outlier_summary$OutlierPct), ], 10)

#─────────────────────────────
# Doing some analysis after data clean up
#─────────────────────────────
# Transaction Amount distribution
ggplot(dataFrame, aes(x = TxnAmt)) +
  geom_histogram(bins = 40, fill = "#69b3a2") +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Transaction Amount Distribution (Before Log Transform)",
       x = "Transaction Amount",
       y = "Count")

# Log-transform for analysis scale
dataFrame$logTxnAmt <- log1p(dataFrame$TxnAmt)

# Visualize the log-transformed distribution
ggplot(dataFrame, aes(x = logTxnAmt)) +
  geom_histogram(bins = 40, fill = "#3b8bc2") +
  theme_minimal() +
  labs(title = "Transaction Amount Distribution (After Log Transform)",
       x = "log1p(Transaction Amount)",
       y = "Count")

# Analysing outliers in the transactions
Q1 <- quantile(dataFrame$TxnAmt, 0.25, na.rm = TRUE)
Q3 <- quantile(dataFrame$TxnAmt, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
Outliers <- which(dataFrame$TxnAmt < (Q1 - 1.5 * IQR_val) | dataFrame$TxnAmt > (Q3 + 1.5 * IQR_val))
outlier_pct <- round(length(Outliers) / nrow(dataFrame) * 100, 2)
cat("Detected outliers for TxnAmt: ", outlier_pct, "%\n")

# Keeping outliers (important signal for fraud) but use log scale for modeling
ggplot(dataFrame, aes(x = factor(isFraud), y = logTxnAmt, fill = factor(isFraud))) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8, outlier.colour = "grey40") +
  scale_fill_manual(values = c("steelblue", "tomato"),
                    name = "Fraud Status",
                    labels = c("Not Fraud", "Fraud")) +
  labs(title = "Transaction Amount (Log Scale) by Fraud Status",
       x = "isFraud", y = "log(TxnAmt)") +
  theme_minimal(base_size = 12)

#─────────────────────────────
# Doing further Uni variate Analysis after data clean up
#─────────────────────────────

### Define feature groups based on column types

# Transaction timing & amount
num_cols <- c(
  "TxnDTHour", "TxnAmt", "dist1",
  paste0("C", 1:14),
  paste0("D", 1:15),
  paste0("id_", 1:11)
)
# to only have existing cols
num_cols <- intersect(num_cols, names(dataFrame))

# Categorical, binary, or encoded numeric features
cat_cols <- c(
  "ProductCD", "addr1", "addr2",
  paste0("card", 1:6),
  "P_emaildomain", "R_emaildomain",
  paste0("M", 1:9),
  paste0("id_", 12:38),
  "DevType", "DevInfo"
)
# to only have existing cols
cat_cols <- intersect(cat_cols, names(dataFrame))

# Target variable
target_col <- "isFraud"

# Quick summaries
num_summary <- dataFrame %>%
  select(any_of(num_cols)) %>%
  summary()
print(num_summary)

# to look at numeric value columns distributrion
num_to_plot <- num_cols
for (col in num_to_plot) {
  p <- ggplot(dataFrame, aes_string(x = col)) +
    geom_histogram(bins = 40, fill = "#007acc", color = "black") +
    theme_minimal() +
    labs(
      title = paste("Distribution of", col),
      x = col,
      y = "Frequency"
    )
  print(p)
}


cat_to_plot <- cat_cols
for (col in cat_to_plot) {
  p <- ggplot(dataFrame, aes_string(x = col, fill = col)) +
    geom_bar(show.legend = FALSE) +
    theme_minimal() +
    labs(
      title = paste("Frequency of", col),
      x = col,
      y = "Count"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

#─────────────────────────────
#  Bivariate / Multivariate Analysis after data cleaning
#─────────────────────────────

dataFrame %>%
  group_by(isFraud) %>%
  summarise(
    Count = n(),
    MeanAmt = mean(TxnAmt, na.rm = TRUE),
    MedianAmt = median(TxnAmt, na.rm = TRUE),
    SD = sd(TxnAmt, na.rm = TRUE),
    MinAmt = min(TxnAmt, na.rm = TRUE),
    MaxAmt = max(TxnAmt, na.rm = TRUE)
  )

ggplot(dataFrame, aes(x = TxnAmt, fill = factor(isFraud))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 80) +
  scale_fill_manual(values = c("steelblue", "tomato"),
                    labels = c("Not Fraud", "Fraud"),
                    name = "isFraud") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "TxnAmt Distribution by Fraud Status",
    x = "Transaction Amount",
    y = "Count"
  ) +
  theme_minimal(base_size = 13)

ggplot(dataFrame, aes(x = log1p(TxnAmt), fill = factor(isFraud))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("steelblue", "tomato"),
                    labels = c("Not Fraud", "Fraud")) +
  labs(title = "Log(TxnAmt + 1) Densityby Fraud Status",
       x = "log(TxnAmt+ 1)", y = "Density") +
  theme_minimal(base_size = 13)

# For large data, using a 5–10% random sample for scatter-based plots
set.seed(123)
plot_sample <- if (nrow(dataFrame) > 10000) dataFrame[sample(1:nrow(dataFrame), 10000), ] else dataFrame

## Correlation Matrix for Key Numeric Variables
if (length(num_cols) > 1) {
  data_num <- dataFrame[, num_cols]
  data_num <- data_num[, sapply(data_num, function(x) var(x, na.rm = TRUE) > 0)]
  var_order <- order(sapply(data_num, sd, na.rm = TRUE), decreasing = TRUE)
  top_vars <- names(data_num)[var_order[1:min(15, length(var_order))]]
  
  corr_matrix <- cor(data_num[, top_vars], use = "pairwise.complete.obs")
  corrplot(corr_matrix, method = "color", type = "lower",
           tl.cex = 0.7, tl.srt = 45,
           title = "Correlation Matrix (Top Numeric Variables)",
           mar = c(0, 0, 2, 0))
}

# Hour-of-Day vs Fraud
if (all(c("TxnDTHour", "isFraud") %in% names(dataFrame))) {
  ggplot(dataFrame, aes(x = TxnDTHour, fill = factor(isFraud))) +
    geom_histogram(bins = 24, position = "dodge") +
    scale_fill_manual(values = c("steelblue", "tomato"),
                      labels = c("Not Fraud", "Fraud")) +
    labs(title = "Transaction Hour by Fraud Status",
         x = "Hour of Day", y = "Transaction Count") +
    theme_minimal(base_size = 12)
}

# Fraud Rate by Address Region
address_cols <- intersect(c("addr1", "addr2"), names(dataFrame))
for (addr_col in address_cols) {
  dataFrame[[addr_col]] <- as.factor(dataFrame[[addr_col]])
  ggplot(dataFrame, aes_string(x = addr_col, fill = "factor(isFraud)")) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("steelblue", "tomato"),
                      labels = c("Not Fraud", "Fraud")) +
    labs(title = paste("Fraud Rate by", addr_col),
         x = addr_col, y = "Proportion") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

for (addr_col in address_cols) {
  dataFrame[[addr_col]] <- as.factor(dataFrame[[addr_col]])
  
  p <- ggplot(dataFrame, aes_string(x = addr_col, fill = "factor(isFraud)")) +
    geom_bar(position = "fill") +
    scale_fill_manual(
      values = c("steelblue", "tomato"),
      labels = c("Not Fraud", "Fraud"),
      name = "isFraud"
    ) +
    labs(
      title = paste("Fraud Rate by", addr_col),
      x = addr_col, y = "Proportion"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}


# Scatter Plots (Sampled for Speed)
scatter_candidates <- intersect(c("D1", "D2", "C1", "C5", "C13", "C14"), names(dataFrame))

for (col in scatter_candidates) {
  p <- ggplot(plot_sample, aes_string(x = col, y = "logTxnAmt", color = "factor(isFraud)")) +
    geom_point(alpha = 0.4, size = 0.6) +
    scale_color_manual(values = c("steelblue", "tomato")) +
    theme_minimal(base_size = 11) +
    labs(title = paste("Transaction Amount vs", col, "(Sampled, n=10K)"),
         x = col, y = "log(TxnAmt)") +
    theme(legend.position = "bottom")
  print(p)
}

# Pairwise Relationships (Subset + Sampling)
pair_subset <- intersect(c("logTxnAmt", "C1", "C2", "D1", "D2", "id_01"), names(dataFrame))

if (length(pair_subset) > 2) {
  sample_pairs <- plot_sample[, pair_subset]
  sample_pairs$isFraud <- factor(plot_sample$isFraud)
  GGally::ggpairs(
    data = sample_pairs,
    mapping = ggplot2::aes(color = isFraud, alpha = 0.7),
    upper = list(continuous = GGally::wrap("cor", size = 2.5)),
    title = "Pairwise Relationships (Sample of 10K Rows)"
  )
}

#─────────────────────────────
# Feature Engineering 
#─────────────────────────────

# Amount Transformation
if ("TxnAmt" %in% names(dataFrame)) {
  dataFrame <- dataFrame %>%
    mutate(
      TxnAmt_z   = as.numeric(scale(TxnAmt)),
      TxnAmt_log = log1p(TxnAmt)
    )
}


# Email Flags
if (all(c("P_emaildomain", "R_emaildomain") %in% names(dataFrame))) {
  dataFrame <- dataFrame %>%
    mutate(
      SameEmailFlag = ifelse(P_emaildomain == R_emaildomain, 1, 0),
      KnownDomainP  = ifelse(grepl("Google|Yahoo|Apple|Microsoft|AOL", P_emaildomain, ignore.case = TRUE), 1, 0),
      KnownDomainR  = ifelse(grepl("Google|Yahoo|Apple|Microsoft|AOL", R_emaildomain, ignore.case = TRUE), 1, 0)
    )
}

# Generating more time features
dataFrame <- dataFrame %>%
  mutate(
    # TxnDT is in seconds since dataset start -> Convert to relative time units
    TxnDay  = floor(as.numeric(TxnDT) / (24 * 3600)),            # relative day index
    TxnHour = floor((as.numeric(TxnDT) %% (24 * 3600)) / 3600),  # 0–23 hour of day
    
    # weekday: repeating 7‑day pattern
    DayOfWeek = factor(
      TxnDay %% 7,
      labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    ),
    
    # weekend flag (Saturday, Sunday in the cycle)
    IsWeekend = ifelse(DayOfWeek %in% c("Sat", "Sun"), 1L, 0L),
    
    # time‑of‑day buckets
    HourBucket = cut(
      TxnHour,
      breaks = c(0, 6, 12, 18, 24),
      labels = c("Night", "Morning", "Afternoon", "Evening"),
      include.lowest = TRUE,
      right = FALSE
    ),
    
    # binary: transaction between 00:00 – 05:59
    IsNightTxn = ifelse(TxnHour >= 0 & TxnHour < 6, 1L, 0L)
  )

# Drop redundant TxnDTHour column if it exists
if ("TxnDTHour" %in% names(dataFrame)) {
  dataFrame <- select(dataFrame, -TxnDTHour)
}

## doing PCA on C columns
c_cols <- paste0("C", 1:14)
c_cols <- intersect(c_cols, names(dataFrame))
na_counts <- colSums(is.na(dataFrame[, c_cols]))
df_pca <- dataFrame %>%
  select(all_of(c_cols)) %>%
  mutate_all(~ ifelse(is.infinite(.), NA, .)) %>%
  na.omit()
C_scaled <- scale(df_pca, center = TRUE, scale = TRUE)
pca_res <- prcomp(C_scaled, center = TRUE, scale. = TRUE)
var_exp <- pca_res$sdev^2 / sum(pca_res$sdev^2)
cum_var <- cumsum(var_exp)
var_df <- data.frame(
  PC = paste0("PC", seq_along(var_exp)),
  Variance = var_exp,
  Cumulative = cum_var
)
ggplot(var_df, aes(x = PC, y = Variance)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_line(aes(y = Cumulative), group = 1, color = "tomato", linewidth = 1.2) +
  geom_point(aes(y = Cumulative), color = "tomato", size = 1.8) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Variance Explained by PCA Components (C1–C14)",
    y = "Proportion of Variance",
    x = "Principal Component"
  )
C_PCA1 <- as.data.frame(pca_res$x[, 1])
colnames(C_PCA1) <- "C_PCA1"
dataFrame <- cbind(dataFrame, C_PCA1)
dataFrame <- dataFrame %>% select(-all_of(c_cols))

# dropped duplicated column
dataFrame <- dataFrame %>% select(-all_of("logTxnAmt"))


#─────────────────────────────
# Modeling and Machine learning
#─────────────────────────────

library(xgboost)
library(lightgbm)
library(MLmetrics)
library(PRROC)        # For PR-AUC
library(parallel)     # For parallel processing
library(ROSE)     # For ROSE synthetic sampling
library(ranger)   # Fast random forest implementation
library(pROC)     # ROC / AUC calculations
library(smotefamily)  # provides ADAS (ADASYN) implementation
library(mltools)
library(Ckmeans.1d.dp)
library(data.table)

set.seed(2025)

# Initial approach 
## Copying the the data frame so that we can save the original data and can reuse if needed
firstDataFrameForModeling <- copy(dataFrame)

# Ensure target is a factor before splitting so both partitions share the same levels
firstDataFrameForModeling$isFraud <- factor(firstDataFrameForModeling$isFraud, levels = c(0, 1), labels = c("No", "Yes"))

# Dropping unnecessaery columns
columns_to_drop <- c("addr1", "addr2")
firstDataFrameForModeling <- firstDataFrameForModeling[, !(names(firstDataFrameForModeling) %in% columns_to_drop)]

# Convert character predictors to factors (glm/ranger cannot ingest raw character columns)
char_cols <- names(firstDataFrameForModeling)[sapply(firstDataFrameForModeling, is.character)]
firstDataFrameForModeling[char_cols] <- lapply(firstDataFrameForModeling[char_cols], as.factor)

#─────────────────────────────
# Train / Test Split (80 / 20)
#─────────────────────────────
train_idx  <- createDataPartition(firstDataFrameForModeling$isFraud, p = 0.80, list = FALSE)
train_data <- firstDataFrameForModeling[train_idx, ]
test_data  <- firstDataFrameForModeling[-train_idx, ]

cat("\nTrain/Test Split Verification\n")
cat("-----------------------------\n")
cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")
cat("\nTraining class distribution:\n")
print(prop.table(table(train_data$isFraud)))
cat("\nTest class distribution:\n")
print(prop.table(table(test_data$isFraud)))

train_df <- data.frame(Dataset = "Training", Class = train_data$isFraud)
test_df <- data.frame(Dataset = "Test", Class = test_data$isFraud)
combined_df <- rbind(train_df, test_df)

# Create the bar chart
ggplot2::ggplot(combined_df, ggplot2::aes(x = Dataset, fill = Class)) +
  ggplot2::geom_bar(position = "dodge", width = 0.6) +
  ggplot2::geom_text(
    stat = "count",
    ggplot2::aes(label = after_stat(count)),
    position = ggplot2::position_dodge(width = 0.6),
    vjust = -0.3, 
    size = 4
  ) +
  ggplot2::scale_fill_manual(values = c("No" = "steelblue", "Yes" = "tomato")) +
  ggplot2::labs(
    title = "Train-Test Split: Class Distribution",
    x = "Dataset", y = "Number of Records"
  ) +
  ggplot2::theme_minimal(base_size = 13)

#─────────────────────────────
# Remove near-zero-variance predictors
#─────────────────────────────
predictor_names <- setdiff(names(train_data), "isFraud")
nzv_idx <- nearZeroVar(train_data[, predictor_names], saveMetrics = FALSE)

if (length(nzv_idx) > 0) {
  nzv_cols <- predictor_names[nzv_idx]
  
  cat("\nRemoving near-zero-variance predictors\n")
  cat("---------------------------------------\n")
  cat("Dropping", length(nzv_cols), "columns:", paste(nzv_cols, collapse = ", "), "\n")
  
  train_data <- train_data %>% select(-all_of(nzv_cols))
  test_data  <- test_data  %>% select(-all_of(nzv_cols))
}
cat("\nFinal dataset dimensions after NZV removal\n")
cat("------------------------------------------\n")
cat("Train rows:", nrow(train_data), "| Train columns:", ncol(train_data), "\n")
cat("Test rows:", nrow(test_data), "| Test columns:", ncol(test_data), "\n")

#─────────────────────────────
# Handle Class Imbalance with ROSE
#─────────────────────────────

train_bal <- ROSE(isFraud ~ ., data = train_data, seed = 2025)$data
cat("Post-ROSE class distribution:\n"); print(prop.table(table(train_bal$isFraud)))

#─────────────────────────────
# Helper Method: model evaluation metrics
#─────────────────────────────
evaluate_model <- function(actual, predicted, probs) {
  cm <- caret::confusionMatrix(predicted, actual, positive = "Yes")
  precision <- cm$byClass["Precision"]
  recall    <- cm$byClass["Recall"]
  f1        <- ifelse((precision + recall) == 0, NA_real_,
                      2 * ((precision * recall) / (precision + recall)))
  roc_obj <- pROC::roc(
    response  = actual,
    predictor = probs,
    levels    = c("No", "Yes"),
    direction = "<"
  )
  tibble(
    Accuracy  = unname(cm$overall["Accuracy"]),
    Precision = unname(precision),
    Recall    = unname(recall),
    F1        = unname(f1),
    AUC       = unname(as.numeric(roc_obj$auc))
  )
}

#─────────────────────────────
# Model 1: Logistic Regression (GLM)
#─────────────────────────────
logit_model <- glm(isFraud ~ ., data = train_bal, family = binomial())
logit_probs <- predict(logit_model, newdata = test_data, type = "response")
logit_pred  <- factor(ifelse(logit_probs >= 0.5, "Yes", "No"), levels = c("No", "Yes"))
logit_metrics <- evaluate_model(test_data$isFraud, logit_pred, logit_probs)

#─────────────────────────────
# Model 2: Random Forest (ranger)
#─────────────────────────────
rf_model <- ranger(
  formula       = isFraud ~ .,
  data          = train_bal,
  probability   = TRUE,
  num.trees     = 400,
  mtry          = floor(sqrt(ncol(train_bal) - 1)),
  min.node.size = 5,
  seed          = 2025,
  importance    = "impurity"
)

rf_probs <- predict(rf_model, data = test_data)$predictions[, "Yes"]
rf_pred  <- factor(ifelse(rf_probs >= 0.5, "Yes", "No"), levels = c("No", "Yes"))
rf_metrics <- evaluate_model(test_data$isFraud, rf_pred, rf_probs)

#─────────────────────────────
# Consolidated Performance Summary
#─────────────────────────────
model_metrics <- bind_rows(
  Logistic_Regression = logit_metrics,
  Random_Forest       = rf_metrics,
  .id = "Model"
)

print(model_metrics)

# Examine confusion matrices
cm_logit <- caret::confusionMatrix(logit_pred, test_data$isFraud, positive = "Yes")
cm_rf    <- caret::confusionMatrix(rf_pred, test_data$isFraud, positive = "Yes")

cm_logit
cm_rf

# Top-20 variable importance from the random forest
rf_importance <- rf_model$variable.importance %>%
  sort(decreasing = TRUE) %>%
  head(20)

print(rf_importance)

#─────────────────────────────
# Trying some other methods and models to improve the models
#─────────────────────────────

secondDataFrameForModeling <- copy(dataFrame)

# Convert target to factor
secondDataFrameForModeling$isFraud <- factor(secondDataFrameForModeling$isFraud, levels = c(0, 1), labels = c("No", "Yes"))

# Drop high-cardinality low-information columns
columns_to_drop <- c("addr1", "addr2")
secondDataFrameForModeling <- secondDataFrameForModeling[, !(names(secondDataFrameForModeling) %in% columns_to_drop)]

# Convert character predictors to factors
char_cols <- names(secondDataFrameForModeling)[sapply(secondDataFrameForModeling, is.character)]
secondDataFrameForModeling[char_cols] <- lapply(secondDataFrameForModeling[char_cols], as.factor)

cat("Dataset dimensions:", nrow(secondDataFrameForModeling), "rows x", ncol(secondDataFrameForModeling), "columns\n")
cat("Class distribution:\n")
print(prop.table(table(secondDataFrameForModeling$isFraud)))

#─────────────────────────────
# Feature Engineering some more features
#─────────────────────────────

cat("\n=== Creating Advanced Features ===\n")

secondDataFrameForModeling <- secondDataFrameForModeling %>%
  mutate(
    # Transaction velocity features
    TxnAmt_per_Hour = TxnAmt / (TxnHour + 1),
    
    # Email domain interactions
    Email_Mismatch = ifelse(P_emaildomain != R_emaildomain, 1, 0),
    Both_Known_Domains = KnownDomainP * KnownDomainR,
    
    # Card type interactions
    Card_Email_Match = paste0(card4, "_", P_emaildomain),
    
    # Device-Product interaction
    Device_Product = paste0(DevType, "_", ProductCD),
    
    # Time-based risk
    Weekend_Night = IsWeekend * IsNightTxn,
    
    # Amount buckets
    Amt_Bucket = cut(TxnAmt, 
                     breaks = c(0, 50, 100, 200, 500, Inf),
                     labels = c("Very_Low", "Low", "Medium", "High", "Very_High")),
    
    # D-feature aggregations (time deltas)
    D_Sum = D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D10 + D12 + D13 + D14 + D15,
    D_Mean = D_Sum / 13,
    D_Zero_Count = (D1 == 0) + (D2 == 0) + (D3 == 0) + (D4 == 0) + 
      (D5 == 0) + (D6 == 0) + (D7 == 0)
  )

# Convert new character features to factors
new_char_cols <- c("Card_Email_Match", "Device_Product")
secondDataFrameForModeling[new_char_cols] <- lapply(secondDataFrameForModeling[new_char_cols], as.factor)

cat("New features created. Updated dimensions:", ncol(secondDataFrameForModeling), "columns\n")

#─────────────────────────────
# Train-Test split (time-aware)
#─────────────────────────────

cat("\n=== Creating Train-Test Split ===\n")

# For fraud detection, time-based split is more realistic
# Sort by TxnDT to maintain temporal order
secondDataFrameForModeling <- secondDataFrameForModeling %>% arrange(TxnDT)

# 80-20 split maintaining temporal order
split_point <- floor(0.80 * nrow(secondDataFrameForModeling))
train_data <- secondDataFrameForModeling[1:split_point, ]
test_data <- secondDataFrameForModeling[(split_point + 1):nrow(secondDataFrameForModeling), ]

cat("Training set:", nrow(train_data), "rows\n")
cat("Test set:", nrow(test_data), "rows\n")
cat("\nTrain class distribution:\n")
print(prop.table(table(train_data$isFraud)))
cat("\nTest class distribution:\n")
print(prop.table(table(test_data$isFraud)))

#─────────────────────────────
# Handle class imbalance with SMOTE
#─────────────────────────────

cat("\n=== Applying SMOTE for Class Balancing ===\n")

# Prepare data for SMOTE (needs numeric target)
train_smote_prep <- train_data %>%
  mutate(isFraud_numeric = as.numeric(isFraud) - 1)

# Separate features and target
X_train <- train_smote_prep %>% select(-isFraud, -isFraud_numeric)
y_train <- train_smote_prep$isFraud_numeric

# Convert factors to numeric for SMOTE
X_train_numeric <- X_train %>%
  mutate(across(where(is.factor), ~ as.numeric(.)))

# Apply SMOTE
smote_result <- SMOTE(
  X = X_train_numeric,
  target = y_train,
  K = 5,
  dup_size = 3  # Oversample minority class by 300%
)

# Reconstruct balanced training data
train_balanced <- smote_result$data %>%
  rename(isFraud_numeric = class) %>%
  mutate(isFraud = factor(isFraud_numeric, levels = c(0, 1), labels = c("No", "Yes"))) %>%
  select(-isFraud_numeric)

cat("Post-SMOTE class distribution:\n")
print(prop.table(table(train_balanced$isFraud)))
cat("Balanced training set size:", nrow(train_balanced), "rows\n")

#─────────────────────────────
# Preparing data for XGBOOST & LIGHTGBM
#─────────────────────────────

cat("\n=== Preparing Data Matrices ===\n")

# Function to prepare data for tree-based models
prepare_matrix <- function(data, target_col = "isFraud") {
  # Separate target
  if (target_col %in% names(data)) {
    y <- as.numeric(data[[target_col]]) - 1  # Convert to 0/1
    X <- data %>% select(-all_of(target_col))
  } else {
    y <- NULL
    X <- data
  }
  
  # Convert factors to numeric (label encoding)
  X_processed <- X %>%
    mutate(across(where(is.factor), ~ as.numeric(.)))
  
  # Handle any remaining non-numeric columns
  X_processed <- X_processed %>%
    mutate(across(where(~ !is.numeric(.)), ~ as.numeric(as.factor(.))))
  
  list(X = as.matrix(X_processed), y = y)
}

# Prepare matrices
train_prep <- prepare_matrix(train_balanced)
test_prep <- prepare_matrix(test_data)

dtrain <- xgb.DMatrix(data = train_prep$X, label = train_prep$y)
dtest <- xgb.DMatrix(data = test_prep$X, label = test_prep$y)

cat("Training matrix:", nrow(train_prep$X), "x", ncol(train_prep$X), "\n")
cat("Test matrix:", nrow(test_prep$X), "x", ncol(test_prep$X), "\n")

#─────────────────────────────
# MODEL 3 - XGBOOST
#─────────────────────────────

cat("\n=== Training XGBoost Model ===\n")

# Calculate scale_pos_weight for imbalance
fraud_ratio <- sum(test_prep$y == 0) / sum(test_prep$y == 1)

xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.05,                    # Learning rate
  max_depth = 7,                 # Tree depth
  min_child_weight = 5,          # Minimum sum of instance weight
  subsample = 0.8,               # Row sampling
  colsample_bytree = 0.8,        # Column sampling
  scale_pos_weight = fraud_ratio, # Handle imbalance
  gamma = 0.1,                   # Minimum loss reduction
  lambda = 1,                    # L2 regularization
  alpha = 0.1                    # L1 regularization
)

# Train with early stopping
xgb_model <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 50,
  verbose = 1,
  print_every_n = 50
)

cat("Best iteration:", xgb_model$best_iteration, "\n")

# Predictions
xgb_probs <- predict(xgb_model, dtest)
xgb_pred_05 <- factor(ifelse(xgb_probs >= 0.5, "Yes", "No"), levels = c("No", "Yes"))
xgb_probs_train <- predict(xgb_model, dtrain)

#─────────────────────────────
# MODEL 4 - LIGHTGBM
#─────────────────────────────

cat("\n=== Training LightGBM Model ===\n")

# Prepare LightGBM datasets
lgb_train <- lgb.Dataset(
  data = train_prep$X,
  label = train_prep$y
)

lgb_test <- lgb.Dataset.create.valid(
  lgb_train,
  data = test_prep$X,
  label = test_prep$y
)

lgb_params <- list(
  objective = "binary",
  metric = "auc",
  learning_rate = 0.05,
  num_leaves = 63,
  max_depth = 7,
  min_data_in_leaf = 50,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 5,
  scale_pos_weight = fraud_ratio,
  lambda_l1 = 0.1,
  lambda_l2 = 1,
  min_gain_to_split = 0.1,
  verbose = -1
)

lgb_model <- lgb.train(
  params = lgb_params,
  data = lgb_train,
  nrounds = 500,
  valids = list(test = lgb_test),
  early_stopping_rounds = 50,
  verbose = 1,
  eval_freq = 50
)

cat("Best iteration:", lgb_model$best_iter, "\n")

# Predictions
lgb_probs <- predict(lgb_model, test_prep$X)
lgb_pred_05 <- factor(ifelse(lgb_probs >= 0.5, "Yes", "No"), levels = c("No", "Yes"))
lgb_probs_train <- predict(lgb_model, train_prep$X)

#─────────────────────────────
# Doing Threshold Optimization
#─────────────────────────────

cat("\n=== Optimizing Classification Threshold ===\n")

optimize_threshold <- function(actual, probs) {
  thresholds <- seq(0.1, 0.9, by = 0.05)
  results <- tibble()
  
  # Add a progress bar
  pb <- txtProgressBar(min = 0, max = length(thresholds), style = 3)
  
  for (i in seq_along(thresholds)) {
    thresh <- thresholds[i]
    pred <- factor(ifelse(probs >= thresh, "Yes", "No"), levels = c("No", "Yes"))
    cm <- confusionMatrix(pred, actual, positive = "Yes")
    
    precision <- cm$byClass["Precision"]
    recall <- cm$byClass["Recall"]
    f1 <- ifelse((precision + recall) == 0, 0,
                 2 * ((precision * recall) / (precision + recall)))
    
    results <- bind_rows(results, tibble(
      Threshold = thresh,
      Precision = precision,
      Recall = recall,
      F1 = f1
    ))
    
    # Update the progress bar
    setTxtProgressBar(pb, i)
  }
  
  # Close the progress bar
  close(pb)
  
  best_thresh <- results$Threshold[which.max(results$F1)]
  return(list(results = results, best_threshold = best_thresh))
}

# Optimize for using train dataset
# Optimize for XGBoost
xgb_opt <- optimize_threshold(train_balanced$isFraud, xgb_probs_train)
cat("XGBoost optimal threshold:", xgb_opt$best_threshold, "\n")

# Optimize for LightGBM
lgb_opt <- optimize_threshold(train_balanced$isFraud, lgb_probs_train)
cat("LightGBM optimal threshold (train):", lgb_opt$best_threshold, "\n")

# Generate predictions with optimal thresholds
xgb_pred_opt <- factor(
  ifelse(xgb_probs >= xgb_opt$best_threshold, "Yes", "No"),
  levels = c("No", "Yes")
)

lgb_pred_opt <- factor(
  ifelse(lgb_probs >= lgb_opt$best_threshold, "Yes", "No"),
  levels = c("No", "Yes")
)

#─────────────────────────────
# Model Evaluation
#─────────────────────────────

cat("\n=== Model Evaluation ===\n")

evaluate_model_enhanced <- function(actual, predicted, probs, model_name) {
  cm <- confusionMatrix(predicted, actual, positive = "Yes")
  
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1 <- ifelse((precision + recall) == 0, NA_real_,
               2 * ((precision * recall) / (precision + recall)))
  
  # ROC-AUC
  roc_obj <- roc(
    response = actual,
    predictor = probs,
    levels = c("No", "Yes"),
    direction = "<"
  )
  
  # PR-AUC (more informative for imbalanced data)
  pr_obj <- pr.curve(
    scores.class0 = probs[actual == "Yes"],
    scores.class1 = probs[actual == "No"],
    curve = TRUE
  )
  
  # Matthews Correlation Coefficient
  mcc <- mcc(actual, predicted)
  
  tibble(
    Model = model_name,
    Accuracy = unname(cm$overall["Accuracy"]),
    Precision = unname(precision),
    Recall = unname(recall),
    F1 = unname(f1),
    AUC_ROC = unname(as.numeric(roc_obj$auc)),
    AUC_PR = unname(pr_obj$auc.integral),
    MCC = unname(mcc),
    Balanced_Accuracy = unname(cm$byClass["Balanced Accuracy"])
  )
}

# Evaluate all models
model_results <- bind_rows(
  evaluate_model_enhanced(test_data$isFraud, xgb_pred_05, xgb_probs, "XGBoost (thresh=0.5)"),
  evaluate_model_enhanced(test_data$isFraud, xgb_pred_opt, xgb_probs, 
                          sprintf("XGBoost (thresh=%.2f)", xgb_opt$best_threshold)),
  evaluate_model_enhanced(test_data$isFraud, lgb_pred_05, lgb_probs, "LightGBM (thresh=0.5)"),
  evaluate_model_enhanced(test_data$isFraud, lgb_pred_opt, lgb_probs,
                          sprintf("LightGBM (thresh=%.2f)", lgb_opt$best_threshold))
)

print(model_results)

#─────────────────────────────
# Confusion Matrices
#─────────────────────────────

cat("\n=== Confusion Matrices ===\n\n")

cat("XGBoost (Optimized Threshold):\n")
cm_xgb_opt <- confusionMatrix(xgb_pred_opt, test_data$isFraud, positive = "Yes")
print(cm_xgb_opt)

cat("\n\nLightGBM (Optimized Threshold):\n")
cm_lgb_opt <- confusionMatrix(lgb_pred_opt, test_data$isFraud, positive = "Yes")
print(cm_lgb_opt)

#─────────────────────────────
# Feature Importance Analysis
#─────────────────────────────

cat("\n=== Feature Importance Analysis ===\n")

# XGBoost importance
xgb_importance <- xgb.importance(
  feature_names = colnames(train_prep$X),
  model = xgb_model
)

cat("\nTop 20 Features (XGBoost):\n")
print(head(xgb_importance, 20))

# LightGBM importance
lgb_importance <- lgb.importance(lgb_model, percentage = TRUE)

cat("\nTop 20 Features (LightGBM):\n")
print(head(lgb_importance, 20))

# Visualize XGBoost importance
xgb.ggplot.importance(xgb_importance, top_n = 20) +
  ggtitle("Top 20 Features - XGBoost") +
  theme_minimal(base_size = 12)

#─────────────────────────────
# Curves visualisation
#─────────────────────────────

cat("\n=== Generating ROC and PR Curves ===\n")

# ROC Curves
roc_xgb <- roc(test_data$isFraud, xgb_probs, levels = c("No", "Yes"), direction = "<")
roc_lgb <- roc(test_data$isFraud, lgb_probs, levels = c("No", "Yes"), direction = "<")

# Plot ROC
plot(roc_xgb, col = "blue", lwd = 2, main = "ROC Curves Comparison")
plot(roc_lgb, col = "red", lwd = 2, add = TRUE)
legend("bottomright", 
       legend = c(paste0("XGBoost (AUC = ", round(auc(roc_xgb), 3), ")"),
                  paste0("LightGBM (AUC = ", round(auc(roc_lgb), 3), ")")),
       col = c("blue", "red"), lwd = 2)

# PR Curves
pr_xgb <- pr.curve(
  scores.class0 = xgb_probs[test_data$isFraud == "Yes"],
  scores.class1 = xgb_probs[test_data$isFraud == "No"],
  curve = TRUE
)

pr_lgb <- pr.curve(
  scores.class0 = lgb_probs[test_data$isFraud == "Yes"],
  scores.class1 = lgb_probs[test_data$isFraud == "No"],
  curve = TRUE
)

plot(pr_xgb, col = "blue", lwd = 2, main = "Precision-Recall Curves")
plot(pr_lgb, col = "red", lwd = 2, add = TRUE)
legend("bottomleft",
       legend = c(paste0("XGBoost (AUC = ", round(pr_xgb$auc.integral, 3), ")"),
                  paste0("LightGBM (AUC = ", round(pr_lgb$auc.integral, 3), ")")),
       col = c("blue", "red"), lwd = 2)

#─────────────────────────────
# Threshold Optimization Visualization
#─────────────────────────────

# Plot threshold optimization for XGBoost
ggplot(xgb_opt$results, aes(x = Threshold)) +
  geom_line(aes(y = Precision, color = "Precision"), size = 1.2) +
  geom_line(aes(y = Recall, color = "Recall"), size = 1.2) +
  geom_line(aes(y = F1, color = "F1-Score"), size = 1.2) +
  geom_vline(xintercept = xgb_opt$best_threshold, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Precision" = "blue", "Recall" = "green", "F1-Score" = "purple")) +
  labs(
    title = "XGBoost: Threshold Optimization",
    x = "Classification Threshold",
    y = "Score",
    color = "Metric"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

#─────────────────────────────
# Model Comparision Visualization
#─────────────────────────────

# Reshape for plotting
model_results_long <- model_results %>%
  select(Model, Accuracy, Precision, Recall, F1, AUC_ROC, AUC_PR) %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Score")

ggplot(model_results_long, aes(x = Metric, y = Score, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Score, 3)), 
            position = position_dodge(width = 0.7),
            vjust = -0.3, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Model Performance Comparison",
    x = "Evaluation Metric",
    y = "Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#─────────────────────────────
# Final Model Performance and Summary
#─────────────────────────────

cat("\n" , rep("=", 60), "\n", sep = "")
cat("FINAL MODEL PERFORMANCE SUMMARY\n")
cat(rep("=", 60), "\n\n", sep = "")

# Select best model based on F1-score
best_model_idx <- which.max(model_results$F1)
best_model <- model_results[best_model_idx, ]

cat("BEST PERFORMING MODEL:", best_model$Model, "\n")
cat(rep("-", 60), "\n", sep = "")
cat(sprintf("Accuracy:          %.4f\n", best_model$Accuracy))
cat(sprintf("Precision:         %.4f\n", best_model$Precision))
cat(sprintf("Recall:            %.4f\n", best_model$Recall))
cat(sprintf("F1-Score:          %.4f\n", best_model$F1))
cat(sprintf("AUC-ROC:           %.4f\n", best_model$AUC_ROC))
cat(sprintf("AUC-PR:            %.4f\n", best_model$AUC_PR))
cat(sprintf("MCC:               %.4f\n", best_model$MCC))
cat(sprintf("Balanced Accuracy: %.4f\n", best_model$Balanced_Accuracy))
cat(rep("=", 60), "\n\n", sep = "")

# Comparison with your previous models
cat("IMPROVEMENT OVER PREVIOUS MODELS:\n")
cat(rep("-", 60), "\n", sep = "")
cat("Previous Logistic Regression - Accuracy: 0.715, Recall: 0.749, F1: 0.373\n")
cat("Previous Random Forest       - Accuracy: 0.791, Recall: 0.821, F1: 0.471\n")
cat(sprintf("Current %s - Accuracy: %.3f, Recall: %.3f, F1: %.3f\n",
            best_model$Model, best_model$Accuracy, best_model$Recall, best_model$F1))
cat(rep("=", 60), "\n", sep = "")
