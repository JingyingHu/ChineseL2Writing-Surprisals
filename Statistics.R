#load necessary library
library(readr)
library(tidyr)
library(readxl)
library(dplyr)
library(psych)
library(car)
library(rstatix)
library(purrr)

#----read the data----
df<- read.csv("ChineseL2Writing.csv")
str(df)
colnames(df)

#set as factor
df$cefl_level <- factor(df$cefl_level, levels = c("A2", "B1", "B2", "C1"))
str(df$cefl_level)


#----descriptive stat----
numeric_df <- df[sapply(df, is.numeric)] 

stat_res <- lapply(names(numeric_df), function(col) {
  df %>%
    group_by(cefl_level) %>%
    get_summary_stats(all_of(col), type = "common") %>%
    mutate(variable = col)  
})

summary_stats <- bind_rows(results_list)
summary_stats

write.csv(summary_stats, "descriptives_results_bygroup.csv", row.names = FALSE)


#----table 1 effect size----
# 1) Normality: Shapiro-Wilk test
# If p > 0.05: Fail to reject the null hypothesis, suggesting the residuals are normally distributed.
numeric_df <- df[sapply(df, is.numeric)] 

shapiro_results <- sapply(numeric_df, function(column) {shapiro.test(column)$p.value})

shapiro_results

# 2) Homogeneity of variance: leveneTest
#If p > 0.05, this indicates that the variances across groups are homogeneous, and the assumption of homogeneity is met.
levene_results <- lapply(names(numeric_df), function(col) {
  formula <- as.formula(paste(col, "~ cefl_level"))
  leveneTest(formula, data = df)})

levene_results

# 3) Apply Kruskal-Wallis test and calculate effect size to all variables
KW_results <- map(numeric_df, ~ {
  var <- .
  kruskal_res <- kruskal_test(as.formula(paste(var, "~ cefl_level")), data = df)
  effsize_res <- kruskal_effsize(as.formula(paste(var, "~ cefl_level")), data = df)
  list(variable = var, kruskal = kruskal_res, effect_size = effsize_res)})

KW_results

# Extract relevant information and combine into a data frame
results_df <- map_dfr(KW_results, function(res) {
  tibble(
    variable = res$variable,                             
    statistic = round(res$kruskal$statistic,3),          
    p_value = round(res$kruskal$p, 3),                   
    effect_size = round(res$effect_size$effsize,3),      
    magnitude = res$effect_size$magnitude                
  )
})

print(results_df)

#save the results
write.csv(results_df, "kruskal_results.csv", row.names = FALSE)


