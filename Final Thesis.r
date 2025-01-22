

# Load the required libraries

library(tidyverse)
library(vdemdata)
library(dplyr)

# Step 1: Define the list of selected countries and variables
selected_countries <- c("India", "Vietnam", "Philippines", 
                        "Bangladesh", "Pakistan", "Nepal", "Sri Lanka", 
                        "Indonesia", "Myanmar")
indicators <- c("v2elvotbuy", "v2psprlnks", "v2psprbrch", 
                "v2dlencmps", "v2cscnsult", "v2x_pubcorr")

# Step 2: Filter the V-Dem dataset for the selected countries and indicators
vdem_data_filtered <- vdem %>%
  filter(country_name %in% selected_countries & 
         year >= 1990 & 
         year <= 2023 & 
         indicator %in% indicators)

# Step 3: Reshape the data (wide format: one row per country-year, indicators as columns)
vdem_data_wide <- vdem_data_filtered %>%
  select(country_name, year, indicator, value) %>%
  spread(key = indicator, value = value)

# View the first few rows of the reshaped data
head(vdem_data_wide)

# Step 4: Standardize the data (excluding country_name and year columns)
vdem_data_standardized <- vdem_data_wide %>%
  select(-country_name, -year) %>%
  scale()  # Standardizing the data


# Step 5: Perform PCA
pca_result <- prcomp(vdem_data_standardized, center = TRUE, scale. = TRUE)

# View the summary of the PCA result
summary(pca_result)

# View the loadings (contribution of each indicator to the principal components)
pca_result$rotation

# View the scores (principal component scores for each country and year)
pca_result$x

# Step 6: Visualize the Results
# Biplot (showing both loadings and scores)
biplot(pca_result, main = "PCA Biplot")

# Create a data frame for PCA scores
pca_scores <- data.frame(pca_result$x)
# Add country names to the PCA scores
pca_scores$country_name <- vdem_data_wide$country_name
# Plot the first two principal components (PC1 vs. PC2)
ggplot(pca_scores, aes(x = PC1, y = PC2, color = country_name)) +
  geom_point() +
  labs(title = "PCA: Countries in Principal Component Space (PC1 vs PC2)") +
  theme_minimal()
# From this point we want to isolate PC1 and PC2 for further analysis. 
# Add country names and years to the PCA scores
pca_scores$country_name <- vdem_data_wide$country_name
pca_scores$year <- vdem_data_wide$year

# Select only PC1 and PC2 columns, and create panel data
pca_data <- pca_scores %>%
  select(country_name, year, PC1, PC2)

# I have my panel data for net_migration, ed_exp, gdp_per_cap, gdp_per_cap_oecd and population in an excel file named final thesis.
library(readxl)
file_path <- "final thesis.xlsx"
panel_data <- read_excel(file_path)
View(panel_data) # Now you can View the panel data 

# Now to prperly arange it to a panel data that is accessible to R we have to pivot the coloumn s

library(tidyr)
# Convert to long format using pivot_longer()
panel_data_long <- panel_data %>%
  pivot_longer(
    cols = c(net_migration, ed_exp, gdp_per_cap, gdp_per_cap_oecd, population), 
    names_to = "indicator",  
    values_to = "value"      
  )
 
# Reshape the data to wide format using pivot_wider()
panel_data_wide <- panel_data_long %>%
  pivot_wider(
    names_from = indicator,  # The new columns will be created based on the 'indicator' values
    values_from = value      # The values in the new columns will come from the 'value' column
  )

#We now merge the pca_data and panel_data_long
# Merge PCA data (PC1, PC2) with the panel data (long format)
merged_data <- panel_data_wide %>%
  left_join(pca_scores %>% select(country_name, year, PC1, PC2), 
            by = c("country_name", "year"))

# we now have the data for analysis. Before all that we need to produce a new coloumn for differntial

# Calculate the 'differential' column using mutate
merged_data <- merged_data %>%
  mutate(differential = (gdp_per_cap - gdp_per_cap_oecd) / gdp_per_cap_oecd)

#So now we have our dependent variable net_migration, independent variables PC1 and PC2 and control variables differntial, ed_exp, population 
# To reduce the complication we now scale our variables 
variables_to_scale <- c("differential", "ed_exp", "population", "net_migration")

# Apply scaling using scale() and create new scaled columns
panel_data_wide_scaled <- panel_data_wide %>%
  mutate(across(all_of(variables_to_scale), ~ scale(.)))

#Now we check for stationarity among the variables
# Load plm package
library(plm)

# Convert your data to panel data format
panel_data_p <- pdata.frame(panel_data_wide_scaled, index = c("country_name", "year"))

# Perform the LLC test for each variable
llc_results <- lapply(panel_data_p[, c("differential", "ed_exp", "population", "net_migration","PC1", "PC2")], function(x) {
  purtest(x, test = "levinlin")  
})

# View detailed results of LLC test for each variable
lapply(llc_results, summary) #we use lapply because we have listed our variables within the brackets

library(dynlm)

# Fit the ARDL model (assuming net_migration as dependent variable and lagging the independent variables)
model_ardl <- dynlm(net_migration ~ L(PC1, 1) + L(PC2, 1) + L(differential, 1) + 
                    L(ed_exp, 1) + L(population, 1), 
                    data = panel_data_p)

# Summary of the ARDL model
summary(model_ardl)
library(urca)
bounds_test <- ur.df(panel_data_p$net_migration, type = "trend", lags = 1, selectlags = "AIC")
summary(bounds_test)

# Fit the ECM model (if cointegration exists)
ecm_model <- dynlm(diff(net_migration) ~ L(PC1, 1) + L(PC2, 1) + L(differential, 1) + 
                   L(ed_exp, 1) + L(population, 1) + 
                   coint_eq, # the cointegrating equation (error correction term)
                   data = panel_data_p)

# Summary of the ECM model
summary(ecm_model)






