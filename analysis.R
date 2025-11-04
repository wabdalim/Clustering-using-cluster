
# ================================================
#        CREDIT DATA CLEANING & EXPLORATION
# ================================================

# 1️⃣ Load the data
# -----------------
# Read data from text file (pipe-separated, with headers)
setwd("C:/Users/wabda/Desktop/K Navitas")
data <- read.table("credit.txt", 
                   header = TRUE,   
                   sep = "|", 
                   stringsAsFactors = FALSE)

# Quick check of structure
str(data)

# Attach data for easier column access (optional)
attach(data)

# ================================================
# 2️⃣ Identify and Explore Categorical Variables
# ================================================

# Drop ID columns for categorical analysis
data_no_ids <- data[, !(names(data) %in% c("Loan.ID", "Customer.ID"))]

# Identify categorical columns (character or factor)
categorical_vars <- sapply(data_no_ids, is.character)

# View unique values for each categorical column
lapply(data_no_ids[, categorical_vars], unique)

# Optional: frequency tables for deeper inspection
lapply(data_no_ids[, categorical_vars], table)

# ================================================
# 3️⃣ Clean Categorical Variables
# ================================================

# Categorical columns that may contain empty strings
cat_vars <- c("Term", "Years.in.current.job", "Home.Ownership", "Purpose")

# Replace empty strings ("") with NA
for (col in cat_vars) {
  data[[col]][data[[col]] == ""] <- NA
}

# Combine n/a and "NA"
data$Years.in.current.job[data$Years.in.current.job %in% c("n/a", "N/A")] <- NA

# Normalize text
data$Purpose <- trimws(tolower(data$Purpose))
data$Purpose <- gsub("_", " ", data$Purpose)

# Map related categories
data$Purpose[data$Purpose %in% c("other", "others")] <- "other"
data$Purpose[data$Purpose %in% c("take a trip", "vacation", "wedding", "moving")] <- "personal"
data$Purpose[data$Purpose %in% c("small business", "business loan")] <- "business"
data$Purpose[data$Purpose %in% c("educational expenses")] <- "education"
data$Purpose[data$Purpose %in% c("home improvements", "renewable energy")] <- "home improvement"
data$Purpose[data$Purpose %in% c("medical bills")] <- "medical"
data$Purpose[data$Purpose %in% c("buy house", "buy a car", "major purchase")] <- "asset purchase"

# Standardise similar categories for Home Ownership
data$Home.Ownership <- trimws(tolower(data$Home.Ownership))

data$Home.Ownership[data$Home.Ownership %in% c("havemortgage", "home mortgage")] <- "mortgage"
data$Home.Ownership[data$Home.Ownership %in% c("own home")] <- "own"
data$Home.Ownership[data$Home.Ownership %in% c("rent")] <- "rent"

# Title case for readability
data$Home.Ownership <- tools::toTitleCase(data$Home.Ownership)


# Check unique values again after cleaning
lapply(data[cat_vars], unique)

# Optional: frequency tables including NAs
lapply(data[cat_vars], function(x) table(x, useNA = "ifany"))



# ================================================
# 4️⃣ Explore Missing Data in Numeric Columns
# ================================================

# Summary of columns with known missingness
summary(Current.Loan.Amount)
summary(Monthly.Debt)
summary(Years.of.Credit.History)
summary(Number.of.Open.Accounts)
summary(Number.of.Credit.Problems)
summary(Current.Credit.Balance)
summary(Maximum.Open.Credit)

summary(Credit.Score)
summary(Annual.Income)
summary(Months.since.last.delinquent)
summary(Bankruptcies)
summary(Tax.Liens)

# Compute proportion of missing values per column
colMeans(is.na(data)) * 100

# ================================================
# 5️⃣ Remove ID Columns & Create Clean Working Dataset
# ================================================

cols_to_drop <- c("Loan.ID", "Customer.ID")
data <- data[, !(names(data) %in% cols_to_drop)]

# Confirm structure
str(data)

# ================================================
# 6️⃣ Clean Suspicious Numeric Values / Outliers
# ================================================

# 6.1 Current.Loan.Amount: placeholder 99,999,999 → NA
data$Current.Loan.Amount[data$Current.Loan.Amount == 99999999] <- NA

# 6.2 Credit.Score: fix 4-digit scores ending with 0
# Any Credit.Score > 1000 that ends with 0 → divide by 10
# Only consider non-NA values
high_scores <- !is.na(data$Credit.Score) & 
  data$Credit.Score > 1000 & 
  data$Credit.Score %% 10 == 0

# Remove the trailing zero
data$Credit.Score[high_scores] <- data$Credit.Score[high_scores] / 10

# 6.3 Maximum.Open.Credit: extreme outliers (>10 million) → NA
data$Maximum.Open.Credit[data$Maximum.Open.Credit > 10000000] <- NA

# 6.4 Current.Credit.Balance: extreme outliers (>10 million) → NA
data$Current.Credit.Balance[data$Current.Credit.Balance > 10000000] <- NA

# 6.5 Annual.Income: extremely high values (>5 million) → NA
data$Annual.Income[data$Annual.Income > 5000000] <- NA


# ================================================
# 7️⃣ Verify Cleaning
# ================================================
summary(data$Current.Loan.Amount)
summary(data$Credit.Score)
summary(data$Maximum.Open.Credit)
summary(data$Current.Credit.Balance)
summary(data$Annual.Income)

# =============================================================
# 📘 Section 8: Feature Engineering & Exploratory Data Analysis
# =============================================================

# --- 8.1 Derived Financial Features ------------------------------------------

data$credit_utilization <- data$Current.Credit.Balance / data$Maximum.Open.Credit
data$debt_to_income     <- (data$Monthly.Debt * 12) / data$Annual.Income
data$loan_to_income     <- data$Current.Loan.Amount / data$Annual.Income
data$has_delinquency    <- !is.na(data$Months.since.last.delinquent)

# Handle division by zero or infinity (e.g., if denominator is 0 or NA)
data$credit_utilization[!is.finite(data$credit_utilization)] <- NA
data$debt_to_income[!is.finite(data$debt_to_income)] <- NA
data$loan_to_income[!is.finite(data$loan_to_income)] <- NA

# --- 8.2 Identify Numeric and Categorical Variables --------------------------

numeric_vars <- sapply(data, is.numeric)
cat_vars     <- sapply(data, is.character)

# --- 8.3 Univariate Plots -----------------------------------------------------

# Numeric variables: histograms
par(mfrow = c(3, 3))
for (col in names(data)[numeric_vars]) {
  hist(data[[col]],
       main = col,
       col = "skyblue",
       xlab = "",
       border = "white")
}
par(mfrow = c(1, 1))  # reset layout

# Categorical variables: bar plots
for (col in names(data)[cat_vars]) {
  cat("Frequency table for:", col, "\n")
  print(table(data[[col]], useNA = "ifany"))
  
  barplot(table(data[[col]], useNA = "ifany"),
          main = col,
          col = "tan",
          las = 2)
}

# --- 8.4 Relationships Between Numeric Variables -----------------------------

# pairs(~ Credit.Score + Annual.Income + Current.Loan.Amount + Monthly.Debt +
#         Current.Credit.Balance + Maximum.Open.Credit,
#       data = data,
#       main = "Scatterplot Matrix: Numeric Relationships")

# --- 8.5 Correlation Matrix ---------------------------------------------------

library(corrplot)

cor_matrix <- cor(data[, numeric_vars], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

# --- 8.6 Boxplots: Numeric vs Categorical ------------------------------------

library(ggplot2)

ggplot(data, aes(x = Term, y = Credit.Score, fill = Term)) +
  geom_boxplot() +
  labs(title = "Credit Score by Loan Term") +
  theme_minimal()

ggplot(data, aes(x = Home.Ownership, y = Annual.Income, fill = Home.Ownership)) +
  geom_boxplot() +
  labs(title = "Annual Income by Home Ownership") +
  theme_minimal()

ggplot(data, aes(x = Purpose, y = Credit.Score, fill = Purpose)) +
  geom_boxplot() +
  labs(title = "Credit Score by Loan Purpose") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 8.7 Financial Ratios: Distributions -------------------------------------

hist(data$credit_utilization,
     main = "Credit Utilization",
     col = "lightblue",
     xlab = "Current Balance / Max Credit")

hist(data$debt_to_income,
     main = "Debt-to-Income Ratio",
     col = "lightgreen",
     xlab = "(Monthly Debt * 12) / Annual Income")

hist(data$loan_to_income,
     main = "Loan-to-Income Ratio",
     col = "lightgoldenrod",
     xlab = "Loan Amount / Annual Income")

# --- 8.8 Delinquency Behavior ------------------------------------------------

hist(data$Months.since.last.delinquent,
     main = "Months Since Last Delinquency",
     col = "lightcoral",
     xlab = "Months",
     breaks = 20)

cat("Borrowers with no delinquency (NA values):\n")
print(table(is.na(data$Months.since.last.delinquent)))




# ================================================
# 🧹 Preprocessing for Clustering
# ================================================

# 1️⃣ Identify numeric and categorical variables
numeric_vars <- sapply(data, is.numeric)
cat_vars     <- sapply(data, is.character)

# 2️⃣ Impute missing values

# Numeric: median imputation
for (col in names(data)[numeric_vars]) {
  data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
}

# Categorical: mode imputation
get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

for (col in names(data)[cat_vars]) {
  mode_val <- get_mode(data[[col]])
  data[[col]][is.na(data[[col]])] <- mode_val
}

# 3️⃣ Scale numeric variables
scaled_numeric <- scale(data[, numeric_vars])

# 4️⃣ One-hot encode categorical variables
cat_encoded <- model.matrix(~ . -1, data = data[, cat_vars])

# 5️⃣ Combine numeric and categorical for clustering
cluster_data <- cbind(scaled_numeric, cat_encoded)

# 6️⃣ Verify final dataset
dim(cluster_data)
head(cluster_data)

# ================================================
# ✅ Ready for clustering (e.g., kmeans)
# ================================================

# ================================================
# 🔍 Risk-Based Clustering with Optimal k Selection
# ================================================

library(dplyr)
library(ggplot2)
library(cluster)     # silhouette
library(factoextra)  # optional, nicer plots

# 1️⃣ Select numeric risk variables
risk_vars <- c("Credit.Score", "credit_utilization", "debt_to_income", "loan_to_income")
risk_data <- data[, risk_vars]

# 2️⃣ Scale numeric variables
risk_data_scaled <- scale(risk_data)

# 3️⃣ Determine optimal k using Elbow & Silhouette
max_k <- 10
wss <- numeric(max_k)
sil_width <- numeric(max_k)

for (k in 2:max_k) {
  set.seed(42)
  km <- kmeans(risk_data_scaled, centers = k)
  
  # WSS (Elbow)
  wss[k] <- km$tot.withinss
  
  # Silhouette
  ss <- silhouette(km$cluster, dist(risk_data_scaled))
  sil_width[k] <- mean(ss[, 3])
}

# Plot Elbow
plot(1:max_k, wss, type = "b", pch = 19, col = "blue",
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for Optimal k")

# Plot Silhouette
plot(1:max_k, sil_width, type = "b", pch = 19, col = "red",
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Width",
     main = "Silhouette Method for Optimal k")

# Suggested k
optimal_k_elbow <- which(diff(diff(wss)) > 0)[1] + 1
optimal_k_sil <- which.max(sil_width)
cat("Suggested k based on Elbow method:", optimal_k_elbow, "\n")
cat("Suggested k based on Silhouette method:", optimal_k_sil, "\n")

# 4️⃣ Choose k (can use one of the suggestions)
k <- 3  # for example

# 5️⃣ Run k-means clustering
set.seed(42)
km <- kmeans(risk_data_scaled, centers = k)

# 6️⃣ Assign clusters to original data
data$Risk.Cluster <- factor(km$cluster)

# Optional: rename clusters based on domain knowledge
data$Risk.Cluster <- recode(data$Risk.Cluster,
                            `1` = "Low Risk",
                            `2` = "Medium Risk",
                            `3` = "High Risk")

# 7️⃣ Cluster profiling (summary & visualization)

# Numeric summary per cluster
cluster_summary <- data %>%
  group_by(Risk.Cluster) %>%
  summarise(across(all_of(risk_vars),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        median = ~median(.x, na.rm = TRUE),
                        min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE))))
print(cluster_summary)

# Visualization: Credit Score vs Credit Utilization
ggplot(data, aes(x = credit_utilization, y = Credit.Score, color = Risk.Cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Risk Segmentation by Credit Behavior",
       x = "Credit Utilization",
       y = "Credit Score") +
  theme_minimal()

# # Visualization: Credit Score vs Debt-to-Income
# ggplot(data, aes(x = debt_to_income, y = Credit.Score, color = Risk.Cluster)) +
#   geom_point(alpha = 0.6) +
#   labs(title = "Risk Segmentation by Debt/Income",
#        x = "Debt-to-Income",
#        y = "Credit Score") +
#   theme_minimal()
# 
# # Visualization: Credit Score vs Loan-to-Income
# ggplot(data, aes(x = loan_to_income, y = Credit.Score, color = Risk.Cluster)) +
#   geom_point(alpha = 0.6) +
#   labs(title = "Risk Segmentation by Loan/Income",
#        x = "Loan-to-income",
#        y = "Credit Score") +
#   theme_minimal()


library(dplyr)
library(ggplot2)
library(tidyr)


# -----------------------------
# 1️⃣ Numeric Profiling
# -----------------------------
numeric_vars <- c("Annual.Income", "Current.Loan.Amount", "Monthly.Debt", "Number.of.Open.Accounts")
for (col in numeric_vars) {
  p <- ggplot(data, aes_string(x = "Risk.Cluster", y = col, fill = "Risk.Cluster")) +
    geom_boxplot() +
    labs(title = paste(col, "Distribution by Risk Cluster"),
         x = "Risk Cluster",
         y = col) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p)  # <-- Must explicitly print inside loop
}

# -----------------------------
# 2️⃣ Categorical Profiling(Histogram)
# -----------------------------
cat_vars <- c("Purpose", "Home.Ownership", "Term", "Years.in.current.job", "has_delinquency")
for (col in cat_vars) {
  cat_summary <- data %>%
    group_by(Risk.Cluster, !!sym(col)) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(percent = count / sum(count) * 100)
  
  print(cat_summary)
  
  p <- ggplot(cat_summary, aes_string(x = col, y = "percent", fill = "Risk.Cluster")) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste(col, "Distribution by Risk Cluster"),
         x = col,
         y = "Percentage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)  # <-- Must explicitly print inside loop
}

library(ggplot2)
library(dplyr)

vars <- c("Number.of.Credit.Problems", "Bankruptcies", 
          "Months.since.last.delinquent", "Tax.Liens")

for (col in vars) {
  # Replace NA with "Missing" for plotting
  data[[col]] <- as.character(data[[col]])
  data[[col]][is.na(data[[col]])] <- "Missing"
  
  # Prepare summary for positioning labels
  plot_data <- data %>%
    group_by(Risk.Cluster, !!sym(col)) %>%
    summarise(count = n(), .groups = "drop")
  
  p <- ggplot(plot_data, aes_string(x = col, y = "count", fill = "Risk.Cluster")) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(aes(label = count), 
              position = position_dodge(width = 0.9), 
              vjust = -0.3, size = 3) +
    labs(title = paste("Counts of", col, "by Risk Cluster"),
         x = col,
         y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}




# binary_vars <- c("Number.of.Credit.Problems", "Bankruptcies", 
#                  "Months.since.last.delinquent", "Tax.Liens")
# 
# for (col in binary_vars) {
#   data[[paste0(col, "_binary")]] <- ifelse(data[[col]] > 0, "Yes", "No")
# }
# 
# library(ggplot2)
# library(dplyr)
# 
# for (col in binary_vars) {
#   bin_col <- paste0(col, "_binary")
#   
#   plot_data <- data %>%
#     group_by(Risk.Cluster, !!sym(bin_col)) %>%
#     summarise(count = n(), .groups = "drop")
#   
#   p <- ggplot(plot_data, aes_string(x = bin_col, y = "count", fill = "Risk.Cluster")) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
#     geom_text(aes(label = count), 
#               position = position_dodge(width = 0.8), 
#               vjust = -0.3, size = 3) +
#     labs(title = paste("Binary Counts of", col, "by Risk Cluster"),
#          x = col,
#          y = "Count") +
#     theme_minimal()
#   
#   print(p)
# }

library(ggplot2)
library(dplyr)

# Variables to check history
history_vars <- c("Number.of.Credit.Problems","Bankruptcies", "Months.since.last.delinquent", "Tax.Liens")

for (col in history_vars) {
  
  # Filter only rows where variable > 0
  history_data <- data %>%
    filter(!!sym(col) > 0)
  
  # Create boxplot
  p <- ggplot(history_data, aes(x = Risk.Cluster, y = credit_utilization, fill = Risk.Cluster)) +
    geom_boxplot(alpha = 0.6, outlier.color = "red") +
    labs(title = paste("Credit Utilisation for People with", col, "by Risk Cluster"),
         x = "Risk Cluster",
         y = "Credit Utilisation") +
    theme_minimal()
  
  print(p)
}



