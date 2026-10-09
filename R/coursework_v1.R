# Clear environment and load required libraries
rm(list = ls())

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tm", dependencies = TRUE)
install.packages("naivebayes")
install.packages("gmodels")
install.packages("SnowballC")
install.packages("cols4all")
install.packages("lubridate")
install.packages("patchwork")
install.packages("GGally")

library(tidyverse)
library(class)    # For KNN classification
library(gmodels)# For cross tabulation
library(caret)
library(C50)
library(lubridate)
library(patchwork)
library(GGally)
library(pROC)
library(dplyr) 
library(scales)

df <- read.csv("LUBS5990M_courseworkData_202526.csv", encoding = 'UTF-8')

#standar review of data and first checkings
str(df)
summary(df)
str(df_c)

df_c %>%
  filter(is.na(price_usd)) %>%
  group_by(goal) %>%
  summarise(
    count = n(),
    percentage = (n() / sum(n())) * 100
  )

#percentage of goal or not
round(prop.table(table(df$goal)) * 100, digits = 1)

max(df_c$price_usd)
summary(df_c$distributed_in_ico)
quantile(df$price_usd, 0.99, na.rm = TRUE)

# Total number of complete rows
sum(complete.cases(df_c))
mean(complete.cases(df_c)) * 100# Percentage of the dataset that havo no missing values


#------------------------------------------------------------------------------
#                               DATA CLEANING
#------------------------------------------------------------------------------

# List the columns you want to inspect
target_cols <- c("kyc", "bonus", "mvp", "ERC20" , "whitelist")
lapply(df[target_cols], function(x) table(x, useNA = "always"))# Print a frequency table for each


#convert the whitelist caracters
df_c <- df %>%
  mutate(whitelist = case_match(tolower(whitelist),
                                c("yes", "y") ~ "yes",
                                c("no", "n")  ~ "no",
                                .default = "no"  # Handles unexpected values or existing NAs
  )) %>%
  mutate(whitelist = as.factor(whitelist)) %>%

    #clean the kyc column
  mutate(kyc = case_match(kyc,
                  c("0.0","0", "false", "False", "N", "No") ~ "no",
                  c("1", "1.0", "1", "true","True", "Y","Yes") ~ "yes"  ,
                  .default = NA_character_)
         ) %>%
  mutate(kyc = as.factor(kyc)) %>%

#clean the bonus variable
  mutate(bonus = case_match(bonus,
                          c("0.0","0", "false", "False", "N", "No") ~ "no",
                          c("1", "1.0", "1", "true","True", "Y","Yes") ~ "yes"  ,
                          .default = NA_character_)
  ) %>%
  mutate(bonus = as.factor(bonus)) %>%
  
#clean the mvp variable
  mutate(mvp = case_match(mvp,
                            c("available", "Available", "AVAILABLE", "Available ","Present", "1", "1.0", "1", "true","True", "Y","Yes") ~ "yes"  ,
                            .default = "no")
  ) %>%
  mutate(mvp = as.factor(mvp))%>%

#clean the ERC20 varible
  mutate(ERC20 = case_match(ERC20,
                            c("0.0","0", "false", "False", "N", "No") ~ "no",
                            c("1", "1.0", "1", "true","True", "Y","Yes") ~ "yes"  ,
                            .default = "no")
  ) %>%
  mutate(ERC20 = as.factor(ERC20))


#understand how much distributed ICO are in a wrong format
df_c %>%
  mutate(format_type = case_when(
    distributed_in_ico <= 1 ~ "Proportion (0-1)",
    distributed_in_ico > 1 & distributed_in_ico <= 100 ~ "Percentage (1-100)",
    distributed_in_ico > 100 ~ "Extreme Outlier (>100)",
    TRUE ~ "Missing (NA)"
  )) %>%
  group_by(format_type) %>%
  summarise(
    count = n(),
    percentage = round((n() / nrow(df_c)) * 100, 2),
    mean_val = mean(distributed_in_ico, na.rm = TRUE)
  )

#transform the wrong format of distributed_in_ico variable depending on the stimated error
df_c <- df_c %>%
  mutate(distributed_in_ico = case_when(
    distributed_in_ico > 100 ~ distributed_in_ico / 1000, # Fixes 995 -> 0.995
    distributed_in_ico > 1   ~ distributed_in_ico / 100,  # Fixes 56 -> 0.56
    TRUE                     ~ distributed_in_ico         # Keeps 0.56 as 0.56
  ))


# Convert using the correct 'dmy' order
df_c <- df_c %>%
  mutate(
    ico_start_dt = dmy(ico_start),
    ico_end_dt   = dmy(ico_end),
    pre_start_dt = dmy(pre_ico_start),
    pre_end_dt   = dmy(pre_ico_end)
  ) %>%
  # Create the 'duration' feature as suggested in the brief 
  mutate(
    ico_duration = as.numeric(ico_end_dt - ico_start_dt)
  )

#change the non logical results in dates
df_c <- df_c %>%
  mutate(ico_duration = if_else(ico_duration < 0, NA_real_, ico_duration))
sum(df_c$ico_duration < 0, na.rm = TRUE)

# binary encoding the links variables as factors
df_c <- df_c %>%
  mutate(
    has_linkedin   = if_else(!is.na(linkedin_link) & linkedin_link != "", 1, 0),
    has_github     = if_else(!is.na(github_link) & github_link != "", 1, 0),
    has_whitepaper = if_else(!is.na(link_white_paper) & link_white_paper != "", 1, 0),
    has_website   = if_else(!is.na(website) & website != "", 1, 0),
  )
df_c <- df_c %>%
  mutate(across(c(has_linkedin, has_github, has_whitepaper, has_website), 
                ~ factor(.x, levels = c(0, 1), labels = c("No", "Yes"))))

# Adding the value_ico variable to df_c for total value asked (apply log function to handle extreme values)
df_c <- df_c %>%
  mutate(
    value_ico = price_usd * token_for_sale,
    log_value_ico = log10(value_ico + 1)
  )

colnames(df_c)
    
# Extracting the Year and month to capture the 'Market Era'
df_c <- df_c %>%
  mutate(
    ico_year = as.factor(year(ico_start_dt)),
    ico_month = as.factor(month(ico_start_dt))
  )

#-------------------------------------------------------------------------------
#                                       EDA
#-------------------------------------------------------------------------------
goal_summary <- df_c %>%
  group_by(goal) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count),
         Goal_Label = ifelse(goal == 1, "Success", "Fail"))

ggplot(goal_summary, aes(y = Goal_Label, x = Count, fill = Goal_Label)) +
  geom_col(alpha = 0.8, color = "black", width = 0.6) +
  # Changed vjust to hjust to push the text to the right of the bars
  geom_text(aes(label = percent(Percentage, accuracy = 0.1)), hjust = -0.2, size = 5) +
  scale_fill_manual(values = c("Fail" = "#f59e98", "Success" = "#6ae7eb")) +
  theme_minimal() +
  labs(title = "Baseline Distribution of ICO Outcomes",
       subtitle = "Proportion of Failed vs. Successful Projects",
       y = "Project Outcome (Goal)", # Swapped axis labels
       x = "Number of Projects") +
  # Added a slight expansion to the x-axis so the percentage text doesn't get cut off
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))



#-------------------------------UNIVARIABLE-------------------------------------

# 1. Histograms for all Numeric Variables
numeric_cols <- c("log_value_ico", "distributed_in_ico", "rating", 
                  "teamsize", "ico_duration")

p_hist <- df_c %>%
  select(all_of(numeric_cols)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "Univariate Histograms of Numerical Signals",
       subtitle = "Visualizing distribution and potential skewness",
       x = "Value", y = "Frequency")

# Price USD
p_box_price <- ggplot(df_c, aes(x = price_usd, y = "")) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red") +
  scale_x_log10() + 
  theme_minimal() +
  labs(title = "Price USD (Log Scale)", x = "USD", y = "")

# Pre-ICO Price
p_box_preprice <- ggplot(df_c, aes(x = pre_ico_price_usd, y = "")) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  scale_x_log10() +
  theme_minimal() +
  labs(title = "Pre-ICO Price (Log Scale)", x = "USD", y = "")

# Tokens for Sale (Log scale added due to likely massive outliers)
p_box_token <- ggplot(df_c, aes(x = token_for_sale, y = "")) +
  geom_boxplot(fill = "orange", outlier.color = "red") +
  scale_x_log10() +
  theme_minimal() +
  labs(title = "Tokens for Sale (Log Scale)", x = "Token Count", y = "")

# Combine for display
(p_box_price / p_box_preprice / p_box_token)
p_hist


#-------------------------------BIVARIABLE-------------------------------------

#plott numerical variables
df_c %>%
  select(goal, rating, teamsize, ico_duration, log_value_ico, distributed_in_ico) %>%
  mutate(goal = as.factor(goal)) %>% 
  ggpairs(
    aes(color = goal, alpha = 0.4),
    lower = list(continuous = wrap("points", size = 0.3)),
    upper = list(continuous = wrap("cor", size = 3)),
    legend = c(2, 1)
  ) +
  labs(title = "Pairwise Relationships of Numerical Variables by ICO Outcome") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )






#plot categorical variables

features <- c("mvp", "whitelist", "kyc", "ERC20", "bonus", "has_whitepaper", "has_github")

# Prepare data: Calculate success rate for each category within each feature
plot_data <- df_c %>%
  select(goal, all_of(features)) %>%
  # Convert everything to character for unified pivoting
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = -goal, names_to = "Feature", values_to = "Status") %>%
  # Filter out NAs so we only see 'yes/no' or '0/1'
  filter(!is.na(Status), Status != "") %>%
  group_by(Feature, Status) %>%
  summarise(
    Success_Rate = mean(as.numeric(goal)),
    n = n(),
    .groups = "drop"
  )

# Create the visualization
ggplot(plot_data, aes(x = Status, y = Success_Rate, fill = Status)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  geom_text(aes(label = scales::percent(Success_Rate, accuracy = 1)), v_as = -0.5, size = 3) +
  facet_wrap(~Feature, scales = "free_x") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_manual(values = c("no" = "#fbb4af", "yes" = "#73dcdf", 
                               "0" = "#fbb4af", "1" = "#73dcdf",
                               "No" = "#fbb4af", "Yes" = "#73dcdf")) +
  theme_minimal() +
  labs(
    title = "Signaling Power: How Binary Features Impact Success Rate",
    subtitle = "Comparing the probability of success (Goal=1) based on project attributes",
    x = "Feature Status",
    y = "Success Rate (%)"
  )

table(df_c$goal)

df_c %>%
  filter(!is.na(rating)) %>%
  mutate(rating_bin = cut(rating, breaks=c(0, 3, 4, 4.5, 5))) %>%
  group_by(rating_bin, has_github) %>%
  summarise(success_rate = mean(as.numeric(goal)), .groups = 'drop') %>%
  ggplot(aes(x = rating_bin, y = has_github, fill = success_rate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(title = "Interaction: Rating vs. GitHub Presence",
       subtitle = "Does technical transparency boost the impact of expert ratings?",
       x = "Expert Rating Category", y = "Has GitHub?")


country_stats <- df_c %>%
  group_by(country) %>%
  summarise(
    total_projects = n(),
    success_rate = mean(as.numeric(goal), na.rm = TRUE),
    avg_rating = mean(rating, na.rm = TRUE)
  ) %>%
  filter(total_projects > 30) %>% # Focus on countries with enough data
  arrange(desc(total_projects))

# Plotting Success Rate vs. Volume
ggplot(country_stats %>% filter(total_projects >= 50), aes(x = reorder(country, success_rate), y = success_rate)) +
  geom_segment(aes(xend = country, yend = 0), color = "grey") +
  geom_point(aes(size = total_projects, color = avg_rating)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_color_gradient(low = "red", high = "green") +
  theme_minimal() +
  labs(
    title = "Regional Success Hubs: Top 10 ICO Countries",
    subtitle = "Size = Volume of Projects | Color = Average Expert Rating",
    x = "Country",
    y = "Success Rate (%)"
  )


#-------------------------------------------------------------------------------
#                           SPLITTING AND PREPARATION
#-------------------------------------------------------------------------------

# 1. Function to find the Mode (R doesn't have a built-in one for some reason)
calc_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

df_c <- df_c %>%
  mutate(
    # NUMERICAL: Median Imputation + Shadow Variable
    across(c(rating, teamsize, log_value_ico, distributed_in_ico), 
           ~ if_else(is.na(.x), 1, 0), .names = "flag_{.col}"),
    across(c(rating, teamsize, log_value_ico, distributed_in_ico), 
           ~ if_else(is.na(.x), median(.x, na.rm = TRUE), .x)),
    
    # CATEGORICAL: New Category Imputation
    # We replace NA in factors with "Unknown"
    across(c(country, kyc, whitelist, bonus), 
           ~ as.character(.x)), # Temporarily convert to char
    across(c(country, kyc, whitelist, bonus), 
           ~ if_else(is.na(.x) | .x == "", "Unknown", .x)),
    across(c(country, kyc, whitelist, bonus), 
           ~ as.factor(.x)) # Convert back to factor
  )


#variables selection to have the cleanest dataframe to work with
data <- df_c %>%
  select(
    goal,
    rating, teamsize, ico_duration, log_value_ico, distributed_in_ico,
    country, kyc, whitelist, bonus, mvp, ERC20, 
    has_whitepaper, has_github, has_linkedin,
    ico_year, ico_month,
    starts_with("flag_")
  )

data <- data %>%
  mutate(
    # Fill remaining numerical NA
    ico_duration = if_else(is.na(ico_duration), median(ico_duration, na.rm = TRUE), ico_duration),
    
    # Fill remaining factor NAs with "Unknown"
    ico_year = addNA(ico_year),
    ico_month = addNA(ico_month)
  )

# Check the current distribution of top countries
table(df_c$country) %>% sort(decreasing = TRUE) %>% head(10)

# Lump all but the top 5 countries into an "Other" category
data <- data %>%
  mutate(country = as.factor(country)) %>%
  mutate(country_grouped = fct_lump_n(country, n = 10, other_level = "Other")) %>%
  select(-country)

# Check the new distribution
table(df_c$country_grouped)

# Verify: Should be 0
sum(is.na(data))
str(data)
summary(data)

data$goal <- factor(data$goal, 
                    levels = c(0, 1), 
                    labels = c("Fail", "Success"))

#data splitting--------------------------------
training_ratio = 0.8
set.seed(99)
train.index = createDataPartition(data$goal, p = training_ratio, list = F)

train_data <- data[train.index, ]
test_data  <- data[-train.index, ]

prop.table(table(train_data$goal))
prop.table(table(test_data$goal))

ctrl <- trainControl(method = "cv", 
                     number = 10, 
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary)


#-------------------------------------------------------------------------------
#                           LOGISTIC REGRESSION
#-------------------------------------------------------------------------------

set.seed(99)
model_logit <- train(goal ~ .- kyc - ERC20 - has_whitepaper - has_github, 
                     data = train_data, 
                     method = "glm", 
                     family = "binomial",
                     trControl = ctrl, 
                     preProcess = c("center", "scale"),
                     metric = "ROC")

print(model_logit)
summary(model_logit)


#-------------------------------------------------------------------------------
#                                     KNN
#-------------------------------------------------------------------------------

knn_grid <- expand.grid(k = seq(5, 85, by = 4))

set.seed(99)
model_knn <- train(goal ~ ., 
                   data = train_data, 
                   method = "knn",
                   trControl = ctrl, 
                   metric = "ROC",
                   preProcess = c("center", "scale"),
                   tuneGrid = knn_grid)

print(model_knn)
summary(model_knn)
plot(model_knn)






#-------------------------------------------------------------------------------
#                                 Random forest
#-------------------------------------------------------------------------------

# Testing mtry from very restricted (2) to very broad (15)
rf_grid <- expand.grid(mtry = c(2, 4, 6, 8, 10, 12, 15))

set.seed(99)
model_rf <- train(goal ~ ., 
                  data = train_data, 
                  method = "rf",
                  trControl = ctrl, 
                  metric = "ROC",
                  tuneGrid = rf_grid,     
                  importance = TRUE) 

print(model_rf)
plot(model_rf)









#-------------------------------------------------------------------------------
#                                     SVM
#-------------------------------------------------------------------------------




svm_grid <- expand.grid(
  sigma = c(0.01, 0.05, 0.1),   
  C = c(0.1, 1, 10)             
)

set.seed(99)
model_svm <- train(goal ~ ., 
                   data = train_data, 
                   method = "svmRadial", 
                   trControl = ctrl, 
                   metric = "ROC", 
                   preProcess = c("center", "scale"), 
                   tuneGrid = svm_grid)               

print(model_svm)
plot(model_svm)


# 1. We lock in the optimal parameters the grid search found earlier
svm_optimal <- data.frame(sigma = 0.01, C = 1)

# 2. We train the SVM using the CLEAN formula (dropping the problematic variables)
set.seed(99)
model_svm2 <- train(goal ~ . - kyc - ERC20 - has_whitepaper - has_github, 
                   data = train_data, 
                   method = "svmRadial",
                   trControl = ctrl, 
                   metric = "ROC",
                   preProcess = c("center", "scale"),
                   tuneGrid = svm_optimal)

# 3. Test to see if probabilities work now!
cat("Testing SVM Probabilities...\n")
test_probs <- predict(model_svm2, newdata = test_data, type = "prob")

# 4. Check if we have numbers or NAs
head(test_probs)



#-------------------------------------------------------------------------------
#                   MODEL COMPARISON 
#-------------------------------------------------------------------------------

model_results <- resamples(list(
  Logistic = model_logit, 
  KNN = model_knn, 
  RandomForest = model_rf, 
  SVM = model_svm
))

summary(model_results)

bwplot(model_results, metric = "ROC", main = "Cross-Validated ROC Comparison")



my_models <- list(
  Logistic = model_logit,
  KNN = model_knn,
  RandomForest = model_rf,
  SVM = model_svm
)

# Create an empty data frame to store our business metrics
results_table <- data.frame(
  Model = character(),
  AUC = numeric(),
  Precision = numeric(),
  F1_Score = numeric(),
  stringsAsFactors = FALSE
)

# 3. Setup colors for our ROC plot
plot_colors <- c("midnightblue", "firebrick", "forestgreen", "darkorange")
plot_initialized <- FALSE

# 4. Loop through each model to calculate metrics and build the plot
for(i in seq_along(my_models)) {
  model_name <- names(my_models)[i]
  current_model <- my_models[[i]]
  
  # A. Predict Classes and Probabilities on UNSEEN test data
  preds_class <- predict(current_model, newdata = test_data)
  preds_prob  <- predict(current_model, newdata = test_data, type = "prob")
  
  # B. Generate Confusion Matrix (using mode = "prec_recall" to get F1 and Precision)
  cm <- confusionMatrix(data = preds_class, 
                        reference = test_data$goal, 
                        positive = "Success",
                        mode = "prec_recall")
  
  # C. Calculate the actual ROC Object and AUC score
  # We track the probability of "Success"
  roc_obj <- roc(response = test_data$goal, 
                 predictor = preds_prob$Success, 
                 levels = c("Fail", "Success"))
  model_auc <- as.numeric(auc(roc_obj))
  
  # D. Extract our required metrics
  model_precision <- cm$byClass["Precision"]
  model_f1        <- cm$byClass["F1"]
  
  # E. Add to our results table
  results_table <- rbind(results_table, data.frame(
    Model = model_name,
    AUC = round(model_auc, 3),
    Precision = round(model_precision, 3),
    F1_Score = round(model_f1, 3)
  ))
  
  # F. Add the curve to our Master Plot
  if(!plot_initialized) {
    # Draw the first curve and set up the plot window
    plot(roc_obj, col = plot_colors[i], 
         main = "Master ROC Curve Comparison (Unseen Test Data)", 
         lwd = 2, legacy.axes = TRUE) # legacy.axes puts False Positive Rate on the X-axis
    plot_initialized <- TRUE
  } else {
    # Add subsequent curves to the existing plot
    plot(roc_obj, col = plot_colors[i], add = TRUE, lwd = 2)
  }
}

# 5. Add a legend to the plot so we know which color is which model
legend("bottomright", legend = names(my_models), col = plot_colors, lwd = 3)

# 6. Print the final formatted table, sorting by highest Precision
print("FINAL TEST SET PERFORMANCE (Ranked by Precision):")
print(results_table %>% arrange(desc(Precision)))





#-------------------------------------------------------------------------------
#                            Evaluation
#-------------------------------------------------------------------------------


# Evaluation models:

# 1. Group the 3 stable, probabilistic models
my_models <- list(
  Logistic = model_logit,
  KNN = model_knn,
  RandomForest = model_rf
)

# 2. Create an empty data frame for our metrics
results_table <- data.frame(
  Model = character(),
  AUC = numeric(),
  Precision = numeric(),
  F1_Score = numeric(),
  stringsAsFactors = FALSE
)

# 3. Setup colors for the 3 models on the plot
plot_colors <- c("midnightblue", "firebrick", "forestgreen")
plot_initialized <- FALSE

# 4. Loop through the models to build the table and the plot
for(i in seq_along(my_models)) {
  model_name <- names(my_models)[i]
  current_model <- my_models[[i]]
  
  # Predict Classes and Probabilities on unseen data
  preds_class <- predict(current_model, newdata = test_data)
  preds_prob  <- predict(current_model, newdata = test_data, type = "prob")
  
  # Generate Confusion Matrix (mode = "prec_recall" gives us Precision/F1)
  cm <- confusionMatrix(data = preds_class, 
                        reference = test_data$goal, 
                        positive = "Success",
                        mode = "prec_recall")
  
  # Calculate the ROC curve and AUC
  roc_obj <- roc(response = test_data$goal, 
                 predictor = preds_prob$Success, 
                 levels = c("Fail", "Success"))
  model_auc <- as.numeric(auc(roc_obj))
  
  # Extract metrics safely
  model_precision <- as.numeric(cm$byClass["Precision"])
  model_f1        <- as.numeric(cm$byClass["F1"])
  
  # Add the results to our table
  results_table <- rbind(results_table, data.frame(
    Model = model_name,
    AUC = round(model_auc, 3),
    Precision = round(model_precision, 3),
    F1_Score = round(model_f1, 3)
  ))
  
  # Draw the curves on the Master Plot
  if(!plot_initialized) {
    plot(roc_obj, col = plot_colors[i], 
         main = "Master ROC Curve Comparison (Unseen Test Data)", 
         lwd = 2, legacy.axes = TRUE) 
    plot_initialized <- TRUE
  } else {
    plot(roc_obj, col = plot_colors[i], add = TRUE, lwd = 2)
  }
}

# 5. Add Legend to the plot
legend("bottomright", legend = names(my_models), col = plot_colors, lwd = 3)

# 6. Print the formatted table, sorted by highest Precision
cat("\n======================================================\n")
cat("PART 1: TOP 3 MODELS (Ranked by Precision)\n")
cat("======================================================\n")
print(results_table %>% arrange(desc(Precision)))



#Evaluation SVM----

preds_class_svm <- predict(model_svm, newdata = test_data)

cm_svm <- confusionMatrix(data = preds_class_svm, 
                          reference = test_data$goal, 
                          positive = "Success",
                          mode = "prec_recall")


cat("Precision:  ", round(as.numeric(cm_svm$byClass["Precision"]), 3), "\n")
cat("F1-Score:   ", round(as.numeric(cm_svm$byClass["F1"]), 3), "\n")
cat("Sensitivity:", round(as.numeric(cm_svm$byClass["Sensitivity"]), 3), "\n")
cat("Specificity:", round(as.numeric(cm_svm$byClass["Specificity"]), 3), "\n")


# Extract the importance matrix from the winning Random Forest model
rf_importance <- varImp(model_rf, scale = TRUE)

# Print the exact numbers to the console so you can see them
print(rf_importance)

# Plot the Top 20 most important variables
plot(rf_importance, top = 20, 
     main = "Random Forest: Top 20 Drivers of ICO Success",
     col = "midnightblue")


#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.----------
#TESTS OF CODES

test1 <- df_c$price_usd * df_c$token_for_sale
p_raw <- ggplot(df_c, aes(x = test1)) +
  geom_histogram(fill = "midnightblue", bins = 50) +
  theme_minimal() +
  labs(title = "Raw Distribution of ICO Value",
       x = "Value (USD)", y = "Count")
p_raw
p_log <- ggplot(df_c, aes(x = test1)) +
  geom_histogram(fill = "firebrick", bins = 50) +
  scale_x_log10(labels = scales::comma) + 
  theme_minimal() +
  labs(title = "Log-Scaled Distribution of ICO Value",
       x = "Value (USD - Log10 Scale)", y = "Count")

# Display them together
p_raw / p_log
summary(test1)

write.csv(data, "ICO_Final_Modeling_Data.csv", row.names = FALSE)
