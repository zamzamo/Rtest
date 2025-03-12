#Author: Azzam


library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(skimr)
library(Hmisc)
library(readxl)
library(broom)
library(ranger)
library(matchit)
library(ggplot2)
library(ggthemes)

#Set working directory
getwd()
setwd("C:/Users/azmuhamm/OneDrive - UGent/Project Batubara/Data")

#Logical operators in R

# ==
# !=
# >
# <
# >=
# <=
# &
# ||
# !

#Import csv file Wolfgang
df <- read_csv("Dealscan Wolfgang CSV/dealscan_nfc_listed.csv",
               col_types = cols(
                 "eligible_property_value_percent" = col_double(),
                 "sp_commercial_paper_rating_current" = col_character()
               )
)

# Convert column 164 to logical (treating "-" as FALSE)
df <- df %>%
  mutate(across("sp_commercial_paper_rating_current", ~na_if(.x, "-")))

#df <- read_csv("Dealscan Wolfgang CSV/dealscan_nfc_listed.csv")

#Inspect import problems
issues <- problems(df)
issues_column <- issues %>% distinct(col)
col_indexes <- issues_column[[1]]  # Extracts column index values
col_names <- colnames(df)

#tabulate the problematic columns
df %>% distinct(df[["sp_commercial_paper_rating_current"]])
df %>% distinct(df[["eligible_property_value_percent"]])
df %>% distinct(df[["total_debt_to_EBITDA"]])

#tabulate
table(df[["total_debt_to_EBITDA"]], useNA = "always")
table(df$deal_currency[df$EU28 == TRUE])

#Drop if
table(df$organization_type)
df <- df %>%
  filter(organization_type != "Government-Sovereign")
table(df$organization_type)

#Check the datatype and name of problematic columns. Use name hereafter to subset. 
typeof(df[[164]])
colnames(df)[164] #"sp_commercial_paper_rating_current"

typeof(df[[281]])
colnames(df)[281] #"eligible_property_value_percent"

#Sort the df
df <- df %>% arrange(LPC_deal_id)

#Generate summary statistics. Skim also works for datatables
skim(df)

#Group_by
df_group_bank_year <- df %>%
  group_by(lender_parent_id, tranche_active_year) %>%
  summarise(
    average_deal_size = mean(tranche_amount_m, na.rm = TRUE),
    borrower_name = first(borrower_name),
    .groups = "drop"
  )

#long to wide and vice-versa
setwd("C:/Users/azmuhamm/Downloads")
npl_panel <- read_csv('20250311 Copy panel.csv')
npl_panel_wide <- npl_panel %>%
  pivot_wider(names_from = year, values_from = NPL_FSI)

#subset
df_2010_2020 <- df %>%
  filter(tranche_active_year >= 2010 & tranche_active_year <= 2020)

#Gen new variable
df <- df %>%
  mutate(aisd_100 = aisd *100)

#Gen indicator based on if
df <- df %>%
  mutate(refin_buyout_ind = ifelse(deal_purpose %in% c("Leveraged Buyout", "Management Buyout"), 1, 0))
table(df$deal_purpose) #check if equal to 2824
table(df$refin_buyout_ind) #check if equal to 282

#label and rename the variables
label(df$all_in_spread_drawn_bps) <- "All-In-Spread-Drawn"
df <- df %>% rename(aisd = all_in_spread_drawn_bps)
label(df$aisd)

#lapply, sapply, tapply, vapply function

#Lapply applies a function to each element of a list or vector. Always returns a list.
#lapply(X, FUN, ...)
my_list <- list(a = c(1, 2, 3), b = c(4, 5, 6))
lapply(my_list, mean)  # Computes mean for each element

#Sapply works like lapply() but tries to simplify the output. Vapply you control the output, coerce to numeric. 
#returns a vector or matrix instead of a list if possible
sapply(my_list, mean)
sapply(my_list, function(x) sum(x^2))  # Sum of squares custom function

#Tapply used to apply a function to subsets of a vector, grouped by a factor.
#commonly used in data aggregation.
#tapply(X, INDEX, FUN, ...)

df_tapply <- data.frame(
  category = c("A", "A", "B", "B", "C"),
  value = c(10, 20, 30, 40, 50)
)

tapply(df_tapply$value, df_tapply$category, mean)

#Write a function. Think of econometrics class (this is X var, this is Y var, this is standard error spec, perform matrix operations)
my_function <- function(arg1, arg2) {
  result <- arg1 + arg2  # Perform an operation
  return(result)  # Return the output
}

#Left-join ETS database
mefp <- read_excel("20230713 MEFP Panel_test.xlsx")

panel_npl_mefp <- mefp %>% left_join(npl_panel, by = c("approval_year" = "year", "ccode" = "imfcode")) %>%
  mutate(
    merge_status = case_when(
      !is.na(NPL_FSI) ~ "match",
    )
  )

#left join data tables
dt_left_join <- dt2[dt1, on = c("ID", "Year")]

#remove duplicates df1 for left join. Do we want the duplicates? Depends on dataset. 
df1 <- df1 %>%
  distinct(id, year, .keep_all = TRUE)

#evaluate join. Not possible with inner join. 
skim(panel_npl_mefp) # 498 - 236 = 262
skim(npl_panel) #2372 - 619 = 1752

#fit a regression
model <- lm(aisd ~ borr_ghg_scope123_lvl + deal_amount_m, data = df)
tidy(model)

#fit a random forest
rf_ranger <- ranger(Treatment ~ ., data = npl_panel, num.trees = 500, mtry = 2, importance = "impurity")
df$propensity_score <- predict(rf_ps, type = "prob")[, 2]

#Uses Caret for control CV
ridge_model <- train(
  x = X, y = Y,
  method = "glmnet",           # Ridge regression via glmnet
  trControl = control,         # Cross-validation setup
  tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 1, length = 100))  # Alpha = 0 for Ridge
)

predictions <- predict(ridge_model, newdata = as.matrix(mtcars[, -1]))
head(predictions)

#Generate line chart
npl_panel_year <- npl_panel %>%
  filter(!is.na(year)) %>%  # Remove NA years
  group_by(year) %>%
  summarise(mean_npl = mean(NPL_FSI, na.rm = TRUE), .groups = "drop")

ggplot(npl_panel_year %>% filter(year >= 2005 & year <= 2020), aes(x = year, y = mean_npl)) +
  geom_line(color = "blue", size = 1) +  # Line plot
  labs(title = "NPL Over Time", x = "Year", y = "NPL (percentage)") +
  theme_minimal()

ggplot(npl_panel_year %>% filter(year >= 2005 & year <= 2020), 
       aes(x = year, y = mean_npl)) +
  geom_line(color = "#2E86C1", size = 1.2) +  # Modern blue line
  labs(title = "NPL Over Time", x = "Year", y = "NPL (percentage)") +
  theme_light(base_size = 14) +  # Modern light theme
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))

#Generate bar chart
ggplot(npl_panel_year %>% filter(year >= 2005 & year <= 2020), 
       aes(x = factor(year), y = mean_npl)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +  # Bar chart
  labs(title = "NPL Over Time", x = "Year", y = "NPL (percentage)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate year labels

ggplot(npl_panel_year %>% filter(year >= 2005 & year <= 2020), 
       aes(x = factor(year), y = mean_npl)) +
  geom_bar(stat = "identity", fill = "#1ABC9C", color = "black") +  # Teal bars
  labs(title = "NPL Over Time", x = "Year", y = "NPL (percentage)") +
  theme_fivethirtyeight() +  # Stylish FiveThirtyEight theme
  theme(axis.title = element_text())  # Add axis titles (theme_fivethirtyeight() removes them)

#export to file
fwrite(npl_panel, "large_data.csv", sep = ",")

#Clear all
rm(list = ls())
