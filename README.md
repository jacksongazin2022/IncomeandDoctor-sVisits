---
title: "Project_2_code"
author: "Jackson Gazin"
date: "2023-11-14"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(AER)
data("DoctorVisits", package = "AER")
library(ggplot2)
library(dplyr)
library(xtable)
```

## Data Cleaning 
```{r}
DoctorVisits$age <- DoctorVisits$age*100
DoctorVisits$income <- DoctorVisits$income * 10000

summary(DoctorVisits$income)

levels(DoctorVisits$gender)
```
```{r}
str(DoctorVisits)

## Update Age so it is not a decimal


calculate_percentage_counts <- function(data, column_name) {
  # Check if the column exists in the data
  if (!(column_name %in% colnames(data))) {
    stop("Column not found in the dataset.")
  }
  
  # Calculate counts for each level
  counts <- table(data[[column_name]])
  
  # Calculate percentages
  percentages <- prop.table(counts) * 100
  
  # Combine counts and percentages into a data frame
  result <- data.frame(Level = names(counts), Count = as.numeric(counts), Percentage = percentages)
  
  return(result)
}


calculate_percentage_counts(DoctorVisits, "gender")

calculate_percentage_counts(DoctorVisits, "freepoor")
calculate_percentage_counts(DoctorVisits, "freerepat")
calculate_percentage_counts(DoctorVisits, "nchronic")
calculate_percentage_counts(DoctorVisits, "lchronic")
```


```{r}
generate_summary_table <- function(df, variables) {
  info_for_summary <- list()

  for (variable in variables) {
    var_data <- df[[variable]]

    if (is.numeric(var_data)) {
      info_for_summary[[variable]] <- paste("Mean:", round(mean(var_data), 2), 
                                            "Median:", round(median(var_data), 2), 
                                            "IQR:", round(IQR(var_data), 2))
    } else {
      var_percent <- prop.table(table(var_data)) * 100
      info_for_summary[[variable]] <- paste(names(var_percent), ": ", 
                                            round(var_percent, 2), "%", 
                                            collapse = " | ")
    }
  }

  summary_df <- data.frame(Variable = variables, 
                           Summary_Info = unlist(info_for_summary), 
                           stringsAsFactors = FALSE)
  return(summary_df)
}

# Variables to analyze
variables_to_analyze <- c("visits", "gender", "age", "income", "illness", 
                          "reduced", "health", "private", "freepoor", 
                          "freerepat", "nchronic", "lchronic")

# Assuming your data frame is named df
summary_table <- generate_summary_table(DoctorVisits, variables_to_analyze)

xtable_summary <- xtable(summary_table, caption = "Summary Table for Australian Health Survey Dataset")

# Print the xtable in LaTeX format
print(xtable_summary, comment = FALSE)
```



## EDA
```{r}
summary(DoctorVisits$age)
summary(DoctorVisits$income)
summary(DoctorVisits$illness)
summary(DoctorVisits$reduced)
summary(DoctorVisits$he)
```

Age seems ok

```{r}
library(ggplot2)
library(ggplot2)

quantitative_vars <- sapply(DoctorVisits, is.numeric)
quantitative_vars <- names(quantitative_vars[quantitative_vars])

create_pretty_histograms <- function(data, quantitative_vars) {
  plots_list <- list()
  
  # Generate a histogram for each quantitative variable
  for (var in quantitative_vars) {
    # Sort unique values and find the minimum gap between consecutive values
    sorted_values <- sort(unique(data[[var]]))
    min_gap <- min(diff(sorted_values))
    
    # Set binwidth to the minimum gap
    binwidth <- max(min_gap, 1e-7)  # Adding a small number to avoid division by zero

    plot <- ggplot(data, aes(x = !!sym(var))) +
      geom_histogram(fill = "skyblue", color = "white", binwidth = binwidth, position = "identity") +
      labs(title = paste("Histogram of", var),
           x = var,
           y = "Frequency") +
      theme_minimal()
    
    plots_list[[var]] <- plot
  }
  
  return(plots_list)
}

# Example usage:
# Assuming 'your_data' is your data frame and 'quantitative_vars' is a vector of column names
histograms_list <- create_pretty_histograms(DoctorVisits, quantitative_vars)

# Plot each histogram
for (var in names(histograms_list)) {
  print(histograms_list[[var]])
}

unique(DoctorVisits$health)

```
```{r}
hist(log(DoctorVisits$health+1))
```

```{r}
library(dplyr)
```


```{r}
str(DoctorVisits)
```

```{r}
library(dplyr)
library(ggplot2)

logmean_plot <- function(data, bin_method, x, y, grouping = NULL, reg_formula = y ~ x) {
  num_bins <- ceiling(1 + log2(nrow(data)))

  if (is.null(grouping)) {
    dat <- data.frame(x = data[, x], 
                      y = data[, y],
                      group = 1)
  } else {
    dat <- data.frame(x = data[, x], 
                      y = data[, y],
                      group = data[, grouping])
  }
  
  if (bin_method == "equal_size") {
    log_table <- dat %>%
      na.omit() %>%
      arrange(group, x) %>%
      group_by(group) %>%
      mutate(obs = y,
             bin = rep(1:num_bins,
                       each = ceiling(n() / num_bins))[1:n()]) %>%
      group_by(bin, group) %>%
      summarize(mean_x = mean(x),
                mean_y = mean(obs),
                num_obs = n()) %>%
      ungroup() %>%
      mutate(log_mean = log(mean_y))
  } else {
    log_table <- dat %>%
      na.omit() %>%
      group_by(group) %>%
      mutate(obs = y,
             bin = cut(x, 
                       breaks = num_bins,
                       labels = FALSE)) %>%
      group_by(bin, group) %>%
      summarize(mean_x = mean(x),
                mean_y = mean(obs),
                num_obs = n()) %>%
      ungroup() %>%
      mutate(log_mean = log(mean_y))
  }
  
  if (is.null(grouping)) {
    log_table %>%
      ggplot(aes(x = mean_x,
                 y = log_mean)) +
      geom_point(size = 2.5) +
      geom_smooth(se = FALSE, method = "lm", formula = reg_formula) +
      theme_bw() +
      labs(x = x,
           y = "") +
      theme(text = element_text(size = 25))
  } else {
    log_table %>%
      ggplot(aes(x = mean_x,
                 y = log_mean,
                 color = group,
                 shape = group)) +
      geom_point(size = 2.5) +
      geom_smooth(se = FALSE, method = "lm", formula = reg_formula) +
      theme_bw() +
      labs(x = x,
           y = "Empirical log mean count",
           color = grouping,
           shape = grouping) +
      theme(text = element_text(size = 25))
  }
}

# Assuming DoctorVisits is your data frame
logmean_plot(DoctorVisits, "equal_size", "age", "visits")
logmean_plot(DoctorVisits, "equal_size", "income", "visits")
logmean_plot(DoctorVisits, "equal_size", "illness", "visits")
logmean_plot(DoctorVisits, "equal_size", "reduced", "visits")
logmean_plot(DoctorVisits, "equal_size", "health", "visits")
```

```{r}
library(ggplot2)
library(gridExtra)

# Set a base theme for all plots


# Create the plots with adjusted theme settings
p1 <- logmean_plot(DoctorVisits, "equal_size", "illness", "visits") +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
p2 <- logmean_plot(DoctorVisits, "equal_size", "reduced", "visits") +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
p3 <- logmean_plot(DoctorVisits, "equal_size", "health", "visits") +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())
p4 <- logmean_plot(DoctorVisits, "equal_size", "illness", "visits", reg_formula = y ~ log(x+1)) +
  theme(legend.position = "none")
p5 <- logmean_plot(DoctorVisits, "equal_size", "reduced", "visits", reg_formula = y ~ log(x+1)) +
  theme(legend.position = "none")
p6 <- logmean_plot(DoctorVisits, "equal_size", "health", "visits", reg_formula = y ~ log(x+1)) +
  theme(legend.position = "none")

# Arrange the plots into a grid with adjusted margins
combined_plot <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2, 
                              top = "Log-Mean Plots of Illness, Reduced, and Health",
                              bottom = "Number of Visits")

# Save the plot to a file with adjusted dimensions
ggsave("combined_plot_adjusted.png", combined_plot, width = 20, height = 15)

```

```{r}
z_scores <- scale(DoctorVisits$visits)
outliers <- abs(z_scores) > 2

DoctorVisits[abs(scale(DoctorVisits$visits))>10,]

str(DoctorVisits)

create_boxplots_for_binary_vars <- function(data, binary_vars, income_var) {
  for (var in binary_vars) {
    if (var %in% names(data)) {
      if (is.factor(data[[var]]) && length(levels(data[[var]])) == 2) {
        # Check if the variable is binary
        p <- ggplot(data, aes(x = !!sym(var), y = !!sym(income_var))) +
          geom_boxplot() +
          labs(title = paste("Boxplot of", income_var, "by", var),
               x = var, y = income_var)
        print(p)
      }
    } else {
      cat("Variable", var, "not found in the dataset.\n")
    }
  }
}

# Example usage:
binary_vars <- c("gender", "private", "freepoor", "freerepat", "nchronic", "lchronic")
create_boxplots_for_binary_vars(DoctorVisits, binary_vars, "income")
```

```{r}
help("DoctorVisits")
(summary(DoctorVisits$reduced))
```

## Fitting the model

```{r}
library(MASS)
str(DoctorVisits)
neg_binom_model_full <- glm.nb(visits ~ gender + age + income + illness + reduced + health +
                            private + freepoor + freerepat + nchronic + lchronic,
                          data = DoctorVisits)

coefficients(neg_binom_model_full)[4]*10000

neg_binom_model_reduced <- glm.nb(visits ~ gender + age + illness + reduced + health +
                            private + freepoor + freerepat + nchronic + lchronic,
                          data = DoctorVisits)

likelihood_ratio_statistic <- neg_binom_model_full$twologlik - neg_binom_model_reduced$twologlik
p_value <- pchisq(likelihood_ratio_statistic, df = 1, lower.tail = FALSE)

# Create a data frame for the table
table_data <- data.frame(
  "Likelihood Ratio Statistic" = likelihood_ratio_statistic,
  "P Value" = p_value
)


latex_table <- xtable(table_data)

# Print the LaTeX-formatted table
print(latex_table, include.rownames = FALSE)
```
```{r}
library(ggplot2)
library(gridExtra)

# Assuming 'data' is your data frame with 'reduced', 'illness', and 'health' as columns
# Function to calculate the binwidth based on the smallest non-zero gap between sorted values
calculate_binwidth <- function(data, var) {
  sorted_values <- sort(unique(data[[var]]))
  min_gap <- min(diff(sorted_values))
  max(min_gap, .Machine$double.eps)  # Ensuring the binwidth is not smaller than the smallest representable positive number
}

# Create histogram for 'reduced'
binwidth_reduced <- calculate_binwidth(DoctorVisits, "reduced")
p1 <- ggplot(DoctorVisits, aes(x = reduced)) + 
  geom_histogram(fill = "skyblue", color = "black", binwidth = binwidth_reduced) +
  theme_minimal() +
  labs(title = "Reduced")

# Create histogram for 'illness'
binwidth_illness <- calculate_binwidth(DoctorVisits, "illness")
p2 <- ggplot(DoctorVisits, aes(x = illness)) + 
  geom_histogram(fill = "coral", color = "black", binwidth = binwidth_illness) +
  theme_minimal() +
  labs(title = "Illness")

# Create histogram for 'health'
binwidth_health <- calculate_binwidth(DoctorVisits, "health")
p3 <- ggplot(DoctorVisits, aes(x = health)) + 
  geom_histogram(fill = "lightgreen", color = "black", binwidth = binwidth_health) +
  theme_minimal() +
  labs(title = "Health")

# Combine the plots
combined_plots <- grid.arrange(p1, p2, p3, ncol = 3, 
                               top = "Distribution of Reduced, Illness, and Health Variables")

# Add a caption below the plots using grid.text (from the grid package)
caption <- textGrob("Reduced, illness, and health show right skewness likely due to their discrete nature.", 
                    gp = gpar(fontface = "italic", fontsize = 10), 
                    hjust = 0.5, 
                    vjust = 0)

# Arrange the plots and the caption in one grid
grid.arrange(combined_plots, caption, nrow = 2, heights = c(10, 1))

```


## Diagnosistics: Part 1 Zero Inflation
```{r}
library(countreg)

# Adjust the outer margins (bottom, left, top, right)

# Plot the rootogram with the title inside the function
rootogram(neg_binom_model, main="Rootogram of Our Negative Binomial Model", x = "Doctor's Visits in the Past Two Weeks", y = "Square Root of Absolute Residuals")
```

## Diagnostics: Part 2 
```{r}

library(MASS)
library(statmod)
library(tidyverse)



p1 <- data.frame(x = DoctorVisits$age, resids = qresid(neg_binom_model)) %>%
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  labs(x = "age", y = "Quantile residuals",
       title = "Quantile Residual Plot of Age") +
  theme_bw()
library(grid)

p2 <- data.frame(x = DoctorVisits$income, resids = qresid(neg_binom_model)) %>%
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  labs(x = "income", y = "Quantile residuals",
       title = "Quantile Residual Plot of income") +
  theme_bw()

p3 <- data.frame(x = DoctorVisits$illness, resids = qresid(neg_binom_model)) %>%
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  labs(x = "illness", y = "Quantile residuals",
       title = "Quantile Residual Plot of illness") +
  theme_bw()

p4 <- data.frame(x = DoctorVisits$reduced, resids = qresid(neg_binom_model)) %>%
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  labs(x = "reduced", y = "Quantile residuals",
       title = "Quantile Residual Plot of reduced") +
  theme_bw()

p5 <- data.frame(x = DoctorVisits$health, resids = qresid(neg_binom_model)) %>%
  ggplot(aes(x = x, y = resids)) +
  geom_point()  +
  geom_smooth() +
  labs(x = "health", y = "Quantile residuals",
       title = "Quantile Residual Plot of health") +
  theme_bw()


grid.arrange(arrangeGrob(p1, p2, p3, p4, p5, ncol = 2, 
                         top = textGrob("Quantile Residual Plots", gp = gpar(fontsize = 20))))
```


No evidence of Zero Inflation or Over Dispersion

```{r}
library(car)
vif_values <- vif(neg_binom_model)

# Convert VIF values to a data frame
vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values)

# Use xtable to create a table
vif_table <- xtable(vif_df)


max(cooks.distance(neg_binom_model))

```

