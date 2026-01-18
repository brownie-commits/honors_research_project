# ======================================================================
# Data Analysis
# Author: Austin J. Brown
# ======================================================================


# Plotting Outcome Variables ----------------------------------------------

# For anes_cleaned
anes_cleaned |> 
  select(rpoltrst, rstrglead, pplrule) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "value") |> 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histogram of ANES Outcome Variables", 
       x = "Variable", 
       y = "Count")


# For ess_cleaned
ess_cleaned |> 
  select(trstplt, ipstrgv, psppsgva) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "value") |> 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histogram of ESS Outcome Variables", 
       x = "Variable", 
       y = "Count")
  
# Correlation Matrix ------------------------------------------------------

# ANES

anes_cor_matrix <- cor(anes_cleaned, 
                    use = "pairwise.complete.obs")
corrplot(anes_cor_matrix, 
         method = "color",
         type = "upper", 
         tl.cex = 0.6,
         tl.col = "black",
         title = "Correlation Matrix of ANES Variables",
         mar = c(0, 0, 2, 0))
  
# ESS 

ess_cor_matrix <- cor(ess_cleaned, 
                  use = "pairwise.complete.obs")
corrplot(ess_cor_matrix, 
         method = "color",
         type = "upper", 
         tl.cex = 0.6,
         tl.col = "black",
         title = "Correlation Matrix of ESS Variables",
         mar = c(0, 0, 2, 0))

# Identify highly correlated variables with outcome variables

# ANES

cor_with_rpoltrst <- cor(anes_cleaned, 
                      anes_cleaned$rpoltrst,
                      use = "pairwise.complete.obs")

cor_with_rpoltrst_df <- data.frame(
  variable = names(anes_cleaned),           
  correlation = cor_with_rpoltrst[, 1]) |> 
  arrange(desc(abs(correlation))) |> 
  head(10)

print(cor_with_rpoltrst_df)

cor_with_rstrglead <- cor(anes_cleaned, 
                         anes_cleaned$rstrglead,
                         use = "pairwise.complete.obs")

cor_with_rstrglead_df <- data.frame(
  variable = names(anes_cleaned),           
  correlation = cor_with_rstrglead[, 1]) |> 
  arrange(desc(abs(correlation))) |> 
  head(10)

print(cor_with_rstrglead_df)


cor_with_pplrule <- cor(anes_cleaned, 
                         anes_cleaned$pplrule,
                         use = "pairwise.complete.obs")

cor_with_pplrule_df <- data.frame(
  variable = names(anes_cleaned),           
  correlation = cor_with_pplrule[, 1]) |> 
  arrange(desc(abs(correlation))) |> 
  head(10)

print(cor_with_pplrule_df)

cor_with_pplrule_df |> 
  ggplot(aes(x = reorder(variable, correlation), y = correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Correlated with pplrule",
       x = "Variable",
       y = "Correlation")

cor_with_rpoltrst_df |> 
  ggplot(aes(x = reorder(variable, correlation), y = correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Correlated with rpoltrst",
       x = "Variable",
       y = "Correlation")

cor_with_rstrglead_df |> 
  ggplot(aes(x = reorder(variable, correlation), y = correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Correlated with rstrglead",
       x = "Variable",
       y = "Correlation")


# Combine all correlations into one dataframe
combined_cors <- map2_dfr(
  target_vars, 
  anes_cors,
  ~mutate(.y, target = .x)
)

# Single faceted plot
combined_cors |> 
  ggplot(aes(x = reorder(variable, correlation), y = correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  facet_wrap(~ target, scales = "free_y") +
  labs(
    title = "Top 10 Correlations for Each Outcome Variable",
    x = "Variable",
    y = "Correlation"
  )


# Function to calculate top correlations
get_top_correlations <- function(data, target_var, n = 10) {
  cor_matrix <- cor(data, data[[target_var]], use = "pairwise.complete.obs")
  
  data.frame(
    variable = names(data),
    correlation = cor_matrix[, 1]
  ) |> 
    arrange(desc(abs(correlation))) |> 
    head(n)
}

# Function to plot correlations
plot_correlations <- function(cor_df, target_var) {
  cor_df |> 
    ggplot(aes(x = reorder(variable, correlation), y = correlation)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = paste("Top 10 Variables Correlated with", target_var),
      x = "Variable",
      y = "Correlation"
    )
}

# Use for ANES
target_vars <- c("rpoltrst", "rstrglead", "pplrule")

# Calculate correlations
anes_cors <- map(target_vars, ~get_top_correlations(anes_cleaned, .x))
names(anes_cors) <- target_vars

# Print results
walk2(anes_cors, target_vars, ~{
  cat("\nCorrelations with", .y, ":\n")
  print(.x)
})

# Create plots
anes_plots <- map2(anes_cors, target_vars, ~plot_correlations(.x, .y))

# View plots
anes_plots

# Combine all correlations into one dataframe
combined_cors <- map2_dfr(
  target_vars, 
  anes_cors,
  ~mutate(.y, target = .x)
)

# Single faceted plot
combined_cors |> 
  ggplot(aes(x = reorder(variable, correlation), y = correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  facet_wrap(~ target, scales = "free_y") +
  labs(
    title = "Top 10 Correlations for Each Outcome Variable",
    x = "Variable",
    y = "Correlation"
  )

# Cross Tabs --------------------------------------------------------------

trstplt_uemp3m_xtab <- cross_tab_diagnostic(ess_cleaned, trstplt, uemp3m)
trstplt_stfeco_xtab <- cross_tab_diagnostic(ess_cleaned, trstplt, stfeco)
trstplt_hincfel_xtab <- cross_tab_diagnostic(ess_cleaned, trstplt, hincfel)
trstplt_imbgeco_xtab <- cross_tab_diagnostic(ess_cleaned, trstplt, imbgeco)
trstplt_imueclt_xtab <- cross_tab_diagnostic(ess_cleaned, trstplt, imueclt)
trstplt_imwbcnt_xtab <- cross_tab_diagnostic(ess_cleaned, trstplt, imwbcnt)
trstplt_prtvtbitm_xtab <- cross_tab_diagnostic(ess_cleaned, trstplt, prtvtbit)
trstplt_prtvtbit_xtab <- cross_tab_diagnostic(ess_cleaned, trstplt, prtvtbit)
trstplt_prtvtfnl_xtab <- cross_tab_diagnostic(ess_cleaned, trstplt, prtvtfnl)

ipstrgv_uemp3m_xtab <- cross_tab_diagnostic(ess_cleaned, ipstrgv, uemp3m)
ipstrgv_stfeco_xtab <- cross_tab_diagnostic(ess_cleaned, ipstrgv, stfeco)
ipstrgv_hincfel_xtab <- cross_tab_diagnostic(ess_cleaned, ipstrgv, hincfel)
ipstrgv_imbgeco_xtab <- cross_tab_diagnostic(ess_cleaned, ipstrgv, imbgeco)
ipstrgv_imueclt_xtab <- cross_tab_diagnostic(ess_cleaned, ipstrgv, imueclt)
ipstrgv_imwbcnt_xtab <- cross_tab_diagnostic(ess_cleaned, ipstrgv, imwbcnt)
ipstrgv_prtvtbit_xtab <- cross_tab_diagnostic(ess_cleaned, ipstrgv, prtvtbit)
ipstrgv_prtvtfnl_xtab <- cross_tab_diagnostic(ess_cleaned, ipstrgv, prtvtfnl)

psppsgva_uemp3m_xtab <- cross_tab_diagnostic(ess_cleaned, psppsgva, uemp3m)
psppsgva_stfeco_xtab <- cross_tab_diagnostic(ess_cleaned, psppsgva, stfeco)
psppsgva_hincfel_xtab <- cross_tab_diagnostic(ess_cleaned, psppsgva, hincfel)
psppsgva_imbgeco_xtab <- cross_tab_diagnostic(ess_cleaned, psppsgva, imbgeco)
psppsgva_imueclt_xtab <- cross_tab_diagnostic(ess_cleaned, psppsgva, imueclt)
psppsgva_imwbcnt_xtab <- cross_tab_diagnostic(ess_cleaned, psppsgva, imwbcnt)
psppsgva_prtvtbit_xtab <- cross_tab_diagnostic(ess_cleaned, psppsgva, prtvtbit)
psppsgva_prtvtfnl_xtab <- cross_tab_diagnostic(ess_cleaned, psppsgva, prtvtfnl)











