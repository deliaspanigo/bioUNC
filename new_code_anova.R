
# # # Inputs de la futura funcion de anova
database <- mtcars
name_var_vr <- "mpg"
name_var_factor <- "cyl"
alpha_value <- 0.05

#################################################################################


# # # Information on vectors
vector_all_var_names <- colnames(database)
vector_name_selected_vars <- c(name_var_vr, name_var_factor)
vector_rol_vars <- c("VR", "FACTOR")


#################################################################################



# # # Selected vars info - dataframe
df_selected_vars <- data.frame(
  "order" = 1:length(vector_name_selected_vars),
  "var_name" = vector_name_selected_vars,
  "var_number" = match(vector_name_selected_vars, vector_all_var_names),
  "var_letter" = openxlsx::int2col(match(vector_name_selected_vars, vector_all_var_names)),
  "var_role" = vector_rol_vars,
  "doble_reference" = paste0(vector_rol_vars, "(", vector_name_selected_vars, ")")
)



# # # Anova minibase
# Only selected vars. Only completed rows. Factor columns as factor object in R.
minibase <- na.omit(database[vector_name_selected_vars])
colnames(minibase) <- vector_rol_vars
minibase[,2] <- as.factor(minibase[,2])



# # # Anova control
# 'VR' must be numeric and 'FACTOR must be factor.
df_control_minibase <- data.frame(
  "order" = 1:nrow(df_selected_vars),
  "var_name" = df_selected_vars$var_name,
  "var_role" = df_selected_vars$var_role,
  "control" = c("is.numeric()", "is.factor()"),
  "verify" = c(is.numeric(minibase[,1]), is.factor(minibase[,2]))
)


# # # database and minibase reps
# Our 'n' is from minibase
df_show_n <- data.frame(
  "object" = c("database", "minibase"),
  "n_col" = c(ncol(database), ncol(minibase)),
  "n_row" = c(nrow(database), nrow(minibase))
)


# # # Factor info
# Default order for levels its alphabetic order.
df_factor_info <- data.frame(
  "order" = 1:nlevels(minibase[,2]),
  "level" = levels(minibase[,2]),
  "n" = as.vector(table(minibase[,2])),
  "mean" = tapply(minibase[,1], minibase[,2], mean),
  "color" = rainbow(nlevels(minibase[,2]))
)

# # # Unbalanced reps for levels?
# Important information for Tukey.
# If reps its equal or not equal in all levels must be detailled
# on Tukey.
check_unbalanced_reps <- all(df_factor_info$n == df_factor_info$n[1])




# # # Anova Test
lm_anova <- lm(VR ~ FACTOR, data = minibase)            # Linear model
aov_anova <- aov(lm_anova)                              # R results for anova
table_anova <- as.data.frame(summary(aov_anova)[[1]])   # Common anova table
table_anova





# # # Creation:  minibase_mod
# # Detect rows on database there are on minibase
dt_rows_database_ok <- rowSums(!is.na(database[vector_name_selected_vars])) == ncol(minibase)

# # Object minibase_mod and new cols
minibase_mod <- minibase
minibase_mod$"lvl_order_number" <- as.numeric(minibase_mod[,2])
minibase_mod$"lvl_color" <- df_factor_info$color[minibase_mod$"lvl_order_number"]
minibase_mod$"fitted.values" <- df_factor_info$"mean"[minibase_mod$"lvl_order_number"]
minibase_mod$"residuals" <- lm_anova$residuals
minibase_mod$"id_database" <- c(1:nrow(database))[dt_rows_database_ok]
minibase_mod$"id_minibase" <- 1:nrow(minibase)






# # # Residuals requeriments
# # Normality test (Shapiro-Wilk)
test_normality_residuals <- shapiro.test(minibase_mod$residuals)
test_normality_residuals

# # Homogeinidy test (Bartlett)
test_homogeneity_residuals <- bartlett.test(residuals ~ FACTOR, data = minibase_mod)
test_homogeneity_residuals


# Medidas de dispersion particionadas  (residuals)
df_residuals_variance_levels <- data.frame(
  "order" = 1:nlevels(minibase_mod[,2]),
  "level" = levels(minibase_mod[,2]),
  "variance" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
  "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
)

# # Sum for residuals
sum_residuals <- sum(minibase_mod$residuals)
sum_residuals

# # Mean for residuals
mean_residuals <- mean(minibase_mod$residuals)
mean_residuals




# # # Tukey test
# # Tukey with groups - Full version
tukey01_full_groups <- agricolae::HSD.test(y = lm_anova,
                                           trt = colnames(minibase)[2],
                                           alpha = alpha_value,
                                           group = TRUE,
                                           console = FALSE,
                                           unbalanced = check_unbalanced_reps)

# # Tukey pairs comparation - Full version
tukey02_full_pairs <- agricolae::HSD.test(y = lm_anova,
                                          trt = colnames(minibase)[2],
                                          alpha = alpha_value,
                                          group = FALSE,
                                          console = FALSE,
                                          unbalanced = check_unbalanced_reps)


df_tukey_orginal_table <- tukey01_full_groups$groups
tukey01_full_groups$groups


df_tukey_new_table <- data.frame(
  "level" = rownames(tukey01_full_groups$groups),
  "mean" = tukey01_full_groups$groups[,1],
  "group" = tukey01_full_groups$groups[,2]
)

df_tukey_new_table



#####################################################################################

# Partitioned Measures of Position (VR)
df_vr_position_levels <- data.frame(
  "order" = 1:nlevels(minibase[,2]),
  "level" = levels(minibase[,2]),
  "min" = tapply(minibase[,1], minibase[,2], min),
  "mean" = tapply(minibase[,1], minibase[,2], mean),
  "median" = tapply(minibase[,1], minibase[,2], median),
  "max" = tapply(minibase[,1], minibase[,2], max),
  "n" = tapply(minibase[,1], minibase[,2], length)
)



# Partitioned Measures of Dispersion (VR)
df_vr_dispersion_levels <- data.frame(
  "order" = 1:nlevels(minibase[,2]),
  "level" = levels(minibase[,2]),
  "range" = tapply(minibase[,1], minibase[,2], function(x){max(x) - min(x)}),
  "variance" = tapply(minibase[,1], minibase[,2], var),
  "standard_deviation" = tapply(minibase[,1], minibase[,2], sd),
  "standard_error" = tapply(minibase[,1], minibase[,2], function(x){sd(x)/sqrt(length(x))}),
  "n" = tapply(minibase[,1], minibase[,2], length)
)


# General Measures of Position (VR)
df_vr_position_general <- data.frame(
  "min" = min(minibase[,1]),
  "mean" = mean(minibase[,1]),
  "median" = median(minibase[,1]),
  "max" = max(minibase[,1]),
  "n" = length(minibase[,1])
)



# General Measures of Dispersion (VR)
df_vr_dispersion_general <- data.frame(
  "range" = max(minibase[,1]) - min(minibase[,1]),
  "variance" = var(minibase[,1]),
  "standard_deviation" = sd(minibase[,1]),
  "standard_error" = sd(minibase[,1])/(sqrt(length(minibase[,1]))),
  "n" = length(minibase[,1])
)


#########################################################################################


# Partitioned Measures of Position (residuals)
df_residuals_position_levels <- data.frame(
  "order" = 1:nlevels(minibase_mod[,2]),
  "level" = levels(minibase_mod[,2]),
  "min" = tapply(minibase_mod$residuals, minibase_mod[,2], min),
  "mean" = tapply(minibase_mod$residuals, minibase_mod[,2], mean),
  "median" = tapply(minibase_mod$residuals, minibase_mod[,2], median),
  "max" = tapply(minibase_mod$residuals, minibase_mod[,2], max),
  "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
)



# Partitioned Measures of Dispersion (residuals)
df_residual_dispersion_levels <- data.frame(
  "order" = 1:nlevels(minibase_mod[,2]),
  "level" = levels(minibase_mod[,2]),
  "range" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){max(x) - min(x)}),
  "variance" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
  "standard_deviation" = tapply(minibase_mod$residuals, minibase_mod[,2], sd),
  "standard_error" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){sd(x)/sqrt(length(x))}),
  "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
)


# General Measures of Position (residuals)
df_residuals_position_general <- data.frame(
  "min" = min(minibase_mod$residuals),
  "mean" = mean(minibase_mod$residuals),
  "median" = median(minibase_mod$residuals),
  "max" = max(minibase_mod$residuals),
  "n" = length(minibase_mod$residuals)
)



# General Measures of Dispersion (residuals)
df_residuals_dispersion_general <- data.frame(
  "range" = max(minibase_mod$residuals) - min(minibase_mod$residuals),
  "variance" = var(minibase_mod$residuals),
  "standard_deviation" = sd(minibase_mod$residuals),
  "standard_error" = sd(minibase_mod$residuals)/(sqrt(length(minibase_mod$residuals))),
  "n" = length(minibase_mod$residuals)
)



######################################################################################

# # # Model
est_mu <- mean(df_vr_position_general$mean)
vector_est_mu_i <- df_vr_position_levels$mean
vector_est_mu <- rep(est_mu, nrow(df_vr_position_levels))
vector_est_tau_i <- vector_est_mu_i - vector_est_mu

sum_est_tau_i <- sum(vector_est_tau_i)

df_anova_model_long <- data.frame(
  "order" = df_factor_info$order,
  "level" = df_factor_info$level,
  "n" = df_factor_info$n,
  "est_mu" = vector_est_mu,
  "est_tau_i" = vector_est_tau_i
)


df_anova_model_short <- data.frame(
  "order" = df_factor_info$order,
  "level" = df_factor_info$level,
  "n" = df_factor_info$n,
  "est_mu_i" = vector_est_mu_i
)



#############

# hide_: all objects in the internal envirorment
hide_all_objets <- ls()
seq_along(hide_all_objets)
