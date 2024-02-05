
database <- mtcars
var_vr <- 1
var_factor <- 2
selected_pos_vars <- c(var_vr, var_factor)
original_colnames <- colnames(database)
selected_colname_vars <- original_colnames[selected_pos_vars]


minibase <- database[selected_pos_vars]
colnames(minibase) <- c("VR", "FACTOR")
colname_var_vr <- "VR"
colname_var_factor <- "FACTOR"
minibase[, colname_var_factor] <- as.factor(minibase[, colname_var_factor])

PreControl_Anova_All <- function(database, minibase, selected_colname_vars){

# PreControl_Anova_database(database, var_vr, var_factor)
TableControl01_database <- PreControl_Anova_database(database = database,
                                            selected_colname_vars = selected_colname_vars)

TableControl02_vars <- PreControl_Anova_vars(database = database,
                                      selected_colname_vars = selected_colname_vars)

TableControl03_minibase <- PreControl_Anova_minibase(minibase = minibase)

dt_database <- TableControl01_database$"Verify"[nrow(TableControl01_database)]
dt_vars <- TableControl02_vars$"Verify"[nrow(TableControl02_vars)]
dt_minibase <- TableControl03_minibase$"Verify"[nrow(TableControl03_minibase)]


TableControl04_anova <- PreControl_Anova_test(minibase, dt_database, dt_vars, dt_minibase)
dt_anova <- TableControl04_anova$"Verify"[nrow(TableControl04_anova)]


new_cols <- c("Herramientas", "Disponible")
new_rows <-   c("Tablas", "Graficos", "Test de Anova")
TableControl05_details <- as.data.frame(matrix(NA, length(new_rows), length(new_cols)))
colnames(TableControl05_details) <- new_cols
TableControl05_details$Herramientas <- new_rows
TableControl05_details$Disponible <- c(dt_minibase, dt_minibase, dt_anova)




output_list <-Hmisc::llist(TableControl01_database,
                    TableControl02_vars,
                    TableControl03_minibase,
                    TableControl04_anova,
                    TableControl05_details)

return(output_list)
}


PreControl_Anova_All(database, minibase, selected_colname_vars)
