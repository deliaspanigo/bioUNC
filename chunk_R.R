

# # # Seccion 01 - Librerias --------------------------------------------------------
library(stats)     # Funciones estadisticas y graficas basicas
library(openxlsx)  # Importar archivos xlsx
library(agricolae) # Test de Tukey
library(plotly)    # Funciones graficas avanzadas




# # # Seccion 02 - Especificaciones del operador ------------------------------------
# Valor alfa
alpha_value <- alpha_value

# Path completo del archivo excel
xlsx_path <- selected_path
xlsx_file_name <- tail(strsplit(xlsx_path, "/")[[1]], n = -1)
xlsx_file_name




# # # Seccion 03 - Importar la base de datos Excel ----------------------------------
# Importar base
database <- openxlsx::read.xlsx(xlsxFile =  xlsx_file_name, sheet = 1)


# Corregir posibles errores de nombre de columna
colnames(database) <- make.names(colnames(database))


# Visualizamos las primeras 10 filas de la base de datos
head(x = database, n = 10)


# Visualizamos el nombre de las columnas del objeto database
list_base <- list()
list_base$"file_name" <- xlsx_file_name
list_base$"ncol" <- ncol(database)
list_base$"nrow" <- nrow(database)
table01_base <- do.call(cbind.data.frame, list_base)
table01_base




# # # Seccion 04 - Selección y rol de las variables ---------------------------------
# El usuario debe detallar el numero de columna de cada variable.
# Recomendamos agregar comentarios especificos
pos_var_vr <- 4     # "Peso"        - Columna D
pos_var_factor <- 2 # "Dieta"       - Columna B


# Creamos algunos objetos con informacion especifica
pos_vars <- c(pos_var_vr, pos_var_factor)
amount_vars <- length(pos_vars)
role_vars <- c("VR", "FACTOR")
colname_vars <- colnames(database)[pos_vars]


# Armamos una lista con todo el contenido
list_selected_vars <-list()
list_selected_vars$"orden" <- 1:amount_vars
list_selected_vars$"colname_vars" <- colname_vars
list_selected_vars$"pos_vars" <- pos_vars
list_selected_vars$"letter_vars" <- openxlsx::int2col(pos_vars)
list_selected_vars$"role_vars" <- role_vars
list_selected_vars$"doble_reference_vars" <- paste0(role_vars, "(", colname_vars, ")")


# Convertimos la lista en una tabla
table02_selected_vars <- do.call(cbind.data.frame, list_selected_vars)
table02_selected_vars



# # # Seccion 05 - minibase ---------------------------------------------------------
# Orden de las columnas: VR - FACTOR
minibase <- na.omit(database[table02_selected_vars$colname_vars])
colnames(minibase) <- table02_selected_vars$role_vars
minibase[,"FACTOR"] <- as.factor(minibase[,"FACTOR"])
head(minibase)




# Repeticiones de database y minibase
list_data_details <- list()
list_data_details$"r_object" <- c("database", "minibase")
list_data_details$"nrow" <- c(nrow(database), nrow(minibase))
list_data_details$"ncol" <- c(ncol(database), ncol(minibase))
list_data_details$"deleted_rows" <- c(0, (nrow(database) - nrow(minibase)))


# Tabla del "n" de database y minibase
table03_data_details <- do.call(cbind.data.frame, list_data_details)
table03_data_details

# # # Seccion 06 - Verificacion de formato -------------------
vector_verify01 <- c(is.numeric(minibase[,"VR"]), is.factor(minibase[,"FACTOR"]))
dt01_format <- sum(vector_verify01) == 2
phrase01_A_format <- "Se cumple con el formato de datos para realizar Anova en R."
phrase01_B_format <- "No se cumple con el formato de datos para realizar Anova en R."
if(dt01_format) cat(phrase01_A_format) else cat(phrase01_A_format)


# # # Section07 - Analisis del Factor
vector_order <- 1:length(levels(minibase[,"FACTOR"]))
table04_factor <- cbind.data.frame(vector_order , as.data.frame(table(minibase[, "FACTOR"])))
colnames(table04_factor) <- c("orden", "level_name", "level_freq")
table04_factor$"level_color" <- rainbow(nrow(table04_factor))
table04_factor

# Desbalanceo en repeticiones
dt02_balanced_rep <- length(unique(table04_factor$level_freq)) == 1
phrase02_A_balanced_rep <- "El modelo es balanceado en repeticiones."
phrase02_B_balanced_rep <- "El modelo es desbalanceado en repeticiones."
if(dt02_balanced_rep) cat(phrase02_A_balanced_rep) else cat(phrase02_B_balanced_rep)


# # # Section 07 - Medidas Resumen
sapply(levels(minibase[,"FACTOR"]), function(x){

  dt_selection <- minibase[, "FACTOR"] == x
  selected_data <- minibase[dt_selection, "VR"]
  output_vector <-   c("min" = min(selected_data),
                       "mean" = mean(selected_data),
                       "median" = median(selected_data),
                       "max" = max(selected_data))
  output_vector
})


# # # Seccion 07 - Modelo de Ancova con interacción en R ----------------------------
# Resultados de Ancova con interaccion
general_lineal_model_full <- lm(formula = VR ~ FACTOR, data = minibase)
general_lineal_model_full


# # # Seccion 08 - Modelo de Ancova con interacción en R ----------------------------
# New columns on minibase
minibase$"fitted.values" <- general_lineal_model_full$fitted.values
minibase$"residuals" <- general_lineal_model_full$residuals
minibase$"id_minibase" <- 1:nrow(minibase)
minibase$"lvl_order_number" <- as.numeric(minibase[,"FACTOR"])
minibase$"lvl_color" <- table04_factor$level_color[minibase$"lvl_order_number"]


# # # Seccion 09 - Requisitos del modelo de Ancova con interacción ------------------
# Test de Normalidad de Shapiro-Wilk
test_normality_residuals <- shapiro.test(minibase$residuals)
test_normality_residuals

dt03_normality_residuals <- test_normality_residuals$p.value > alpha_value
phrase03_A_normality_residuals <- "El modelo cumple con la normalidad de los residuos."
phrase03_B_normality_residuals <- "El modelo no cumple con la normalidad de los residuos."
if(dt03_normality_residuals) cat(phrase03_A_normality_residuals) else cat(phrase03_A_normality_residuals)


# Test de Homogeneidad de varianzas de Bartlett
test_homogeneity_residuals <- bartlett.test(formula = residuals ~ FACTOR, data = minibase)
test_homogeneity_residuals

dt04_homogeneity_residuals <- test_homogeneity_residuals$p.value > alpha_value
phrase04_A_homogeneity_residuals <- "El modelo cumple con la homogeneidad de los residuos."
phrase04_B_homogeneity_residuals <- "El modelo no cumple con la homogeneidad de los residuos."
if(dt04_homogeneity_residuals) cat(phrase04_A_homogeneity_residuals) else cat(phrase04_B_homogeneity_residuals)

# Media de los residuos
mean(minibase$residuals)



dt05_requirements <- sum(c(dt04_homogeneity_residuals, dt03_normality_residuals)) == 2
phrase05_A_requirements <- "El modelo cumple con los requisitos del modelo."
phrase05_B_requirements <- "El modelo NO cumple con los requisitos del modelo. \n No es válido sacar conclusiones del test estadístico."
if(dt05_requirements) cat(phrase05_A_requirements) else cat(phrase05_B_requirements)



# # # Seccion 09 - Analisis de Ancova con interaccion -------------------------------

# Formato para Ancova
anova_full <- aov(general_lineal_model_full)
anova_full


# # Sección 09-1) Tabla Ancova -------------------------------------------------
table04_anova <- as.data.frame(summary(anova_full)[[1]])
table04_anova


# # Sección 09-2) Tabla Tukey --------------------------------------------------
# Tukey completo
tukey01_full_groups <- agricolae::HSD.test(y = general_lineal_model_full,
                                           trt = "FACTOR",
                                           alpha = alpha_value,
                                           group = TRUE,
                                           console = FALSE,
                                           unbalanced = !dt02_balanced_rep)

tukey02_full_pairs <- agricolae::HSD.test(y = general_lineal_model_full,
                                          trt = "FACTOR",
                                          alpha = alpha_value,
                                          group = FALSE,
                                          console = FALSE,
                                          unbalanced = !dt02_balanced_rep)


########################################################


# Medias de los niveles del factor
vector_means_vr <- tukey01_full_groups$means[,1]
names(vector_means_vr) <- rownames(tukey01_full_groups$means)

# Media de las medias (mdlm)
mdlm_vr <- mean(vector_means_vr)
vector_mdlm_vr <- rep(mdlm_vr, length(vector_means_vr))
names(vector_mdlm_vr) <- names(vector_means_vr)

# Efectos tau
vector_tau_vr <- vector_means_vr - vector_mdlm_vr
names(vector_tau_vr) <- names(vector_means_vr)

# Lista con las estimaciones
list03 <- list()
list03$"orden" <- 1:length(levels(minibase[,"FACTOR"]))
list03$"level" <- levels(minibase[,"FACTOR"])
list03$"mu" <- vector_mdlm_vr
list03$"tau" <- vector_tau_vr
list03$"mu_i" <- vector_means_vr
list03$"groups" <- tukey01_full_groups$groups$groups


# Tabla de Tukey
table05_tukey <- do.call(cbind.data.frame, list03)
table05_tukey


# Suma de los efectos tau
sum(table05_tukey$tau)



# # # Common mistake: think about mean of VR is always equal than mean o means.
cat("Mean of VR:", mean(minibase[,"VR"]))
cat("Mean of means:", mdlm_vr)
cat("Absolute diference:", abs(mean(minibase[,"VR"]) - mdlm_vr))
cat("On Anova 1 Factor model, mu its mean of means.")
cat("On Anova 1 Factor model, mu its mean of means.")
cat("Solo cuando todos los niveles del factor tienen el mismo 'n' ocurre que la media de VR es igual a la media de las medias.")


# # Sección 09-5) Tabla con la media, minimo y máximo de la covariable para
# #               cada nivel del factor ----------------------------------------
# Estimacion de la interaccion en las combinaciones COV:FACTOR


# Reunimos las medidas de posicion en una lista
selected_var <- "VR"
selected_factor <- "FACTOR"
list06 <- list()
list06$"selected_var" <- rep(selected_var, length(levels(minibase[,selected_factor])))
list06$"selected_factor" <- rep(selected_factor, length(levels(minibase[,selected_factor])))
list06$"orden" <- 1:length(levels(minibase[,selected_factor]))
list06$"levels" <- levels(minibase[,selected_factor])
list06$"min" <- tapply(minibase[,selected_var], minibase[,selected_factor], min)
list06$"mean" <- tapply(minibase[,selected_var], minibase[,selected_factor], mean)
list06$"median" <- tapply(minibase[,selected_var], minibase[,selected_factor], median)
list06$"max" <- tapply(minibase[,selected_var], minibase[,selected_factor], max)
list06$"n" <- tapply(minibase[,selected_var], minibase[,selected_factor], length)

# Conformamos una tabla
table06_position_vr <- do.call(cbind.data.frame, list06)
rownames(table06_position_vr) <- 1:nrow(table06_position_vr)
table06_position_vr



# Reunimos las medidas de dispersion en una lista
selected_var <- "VR"
selected_factor <- "FACTOR"
list07 <- list()
list07$"selected_var" <- rep(selected_var, length(levels(minibase[,selected_factor])))
list07$"selected_factor" <- rep(selected_factor, length(levels(minibase[,selected_factor])))
list07$"orden" <- 1:length(levels(minibase[,selected_factor]))
list07$"levels" <- levels(minibase[,selected_factor])
list07$"rango" <- table06_position_vr$max - table06_position_vr$min
list07$"variance" <- tapply(minibase[,selected_var], minibase[,selected_factor], var)
list07$"standard_deviation" <- tapply(minibase[,selected_var], minibase[,selected_factor], sd)
list07$"n" <- tapply(minibase[,selected_var], minibase[,selected_factor], length)
list07$"standard_error" <- list07$"standard_deviation"/sqrt(list07$"n")

# Conformamos una tabla
table07_dispertion_vr <- do.call(cbind.data.frame, list07)
rownames(table07_dispertion_vr) <- 1:nrow(table07_dispertion_vr)
table07_dispertion_vr

# Intervalos para la media
list08 <- list()


# # # # Configuracion grafica general
set_labels <- "doble_reference_vars"




# # Grafico 1) Gráfico XY - sencillo
#--------------- INICIO GRAFICO 01 ---------------------------------------------

# Definimos un titulo para el grafico
set_main01 <- paste0("Anova 1 Factor", "\n",
                     "Gráfico 01 - Puntos por nivel", "\n",
                     xlsx_file_name)

# Generamos el grafico 01
plot(x = minibase$lvl_order_number,
     y = minibase$VR,
     main = set_main01,
     col = minibase$lvl_color,
     xlab = table02_selected_vars[[set_labels]][2],
     ylab = table02_selected_vars[[set_labels]][1],
     xaxt = "n")

# Agregamos las especificaciones al eje X con el nombre de los niveles
axis(1, at = table04_factor$orden, labels = as.character(table04_factor$level_name), las = 1)

#--------------- FIN GRAFICO 01 ------------------------------------------------




# # Grafico 2) Gráfico XY - sencillo
#--------------- INICIO GRAFICO 02 ---------------------------------------------

# Definimos un titulo para el grafico
set_main02 <- paste0("Anova 1 Factor", "\n",
                     "Gráfico 02 - Boxplot por nivel", "\n",
                     xlsx_file_name)

# Generamos el grafico 01
boxplot(minibase$VR ~ minibase$FACTOR,
        main = set_main02,
        col = table04_factor$level_color,
        xlab = table02_selected_vars[[set_labels]][2],
        ylab = table02_selected_vars[[set_labels]][1])


#--------------- FIN GRAFICO 02 ------------------------------------------------



# # Grafico 3) Media y desvio
#--------------- INICIO GRAFICO 03 ---------------------------------------------

# Definimos un titulo para el grafico
set_main03 <- paste0("Anova 1 Factor", "\n",
                     "Gráfico 03 - Media y un desvio estandard", "\n",
                     xlsx_file_name)

# Generamos el grafico 01
plot(x = minibase$lvl_order_number,
     y = minibase$VR,
     main = set_main01,
     col = "white",
     xlab = table02_selected_vars[[set_labels]][2],
     ylab = table02_selected_vars[[set_labels]][1],
     xaxt = "n")

# Agregamos las especificaciones al eje X con el nombre de los niveles
axis(1, at = table04_factor$orden, labels = table04_factor$level_name, las = 1)


# Generamos el grafico 01
points(x = table06_position_vr$orden,
       y = table06_position_vr$mean,
       col = table04_factor$level_color)


# Agregamos los intervalos
arrows(x0 = vector_orden,
       y0 = vector_inf,
       x1 = vector_orden,
       y1 = vector_sup,
       angle = 90, code = 3, length = 0.1)


#--------------- FIN GRAFICO 03 ------------------------------------------------

# Generamos el grafico 01
plot(x = table06_position_vr$orden,
     y = table06_position_vr$mean,
     main = set_main03,
     col = "white",
     xlab = table02_selected_vars[[set_labels]][2],
     ylab = table02_selected_vars[[set_labels]][1],
     xaxt = "n")

# points(plot(x = COV, y = VR,
#             col = COLORES,
#             xlab = tabla01_variables$referencia[2],
#             ylab = tabla01_variables$referencia[1],
#             main = set_main04, type = "p", pch = 19)

       # Agregamos los segmentos de cada recta
       segments(x0 = tabla09_plot$min_x_cov_i,
                y0 = tabla09_plot$est_min_x_cov_i,
                x1 = tabla09_plot$max_x_cov_i,
                y1 = tabla09_plot$est_max_x_cov_i,
                col = tabla09_plot$colores,
                lwd = 5)


       #####################################################################################

       # # Grafico 02) Puntos con color
       #--------------- INICIO GRAFICO 02 ---------------------------------------------


       # Definimos un titulo para el grafico 02
       set_main02 <- paste0("Ancova con interaccion", "\n",
                            "Gráfico 02 - Boxplot por nivel", "\n",
                            xlsx_file_name)

       # Generamos un espacio grafico doble para poder colocar las referencias.
       par(mfrow = c(1, 2))

       # En la porcion 1 se plotea el grafico 02
       plot(x = COV, y = VR,
            col = COLORES,
            xlab = tabla01_variables$referencia[2],
            ylab = tabla01_variables$referencia[1],
            main = set_main02, type = "p", pch = 19)

       # En la porcion 2 se grafican las referencias del grafico 02
       # Para ello sobre la segunda porcion se genera un gŕafico totalmente blanco.
       plot(x = 0:10, y = 0:10, type = "n",
            xaxt = "n", yaxt = "n",
            xlab = "", ylab = "",
            xlim = c(0, 10), ylim = c(0, 10),
            col = "white", bty = "n")

       # Se agrega la leyenda
       legend("center", inset = c(-0.4, 0.3),
              legend = tabla03_tukey$niveles,
              col = vector_colores, pch = 16,
              title = tabla01_variables$referencia[3])

       # Se vuelven los parametros graficos a los valores originales
       par(mfrow = c(1, 1))

       #--------------- FIN GRAFICO 02 ------------------------------------------------


       # # Grafico 03) Rectas con color
       #--------------- INICIO GRAFICO 03 ---------------------------------------------


       # Definimos un titulo para el grafico 03
       set_main03 <- paste0("Ancova con interaccion", "\n",
                            "Gráfico 03 - Rectas con color", "\n",
                            xlsx_file_name)

       # Generamos un espacio grafico doble para poder colocar las referencias.
       par(mfrow = c(1, 2))

       # En la primera porción gráfica, colocamos los puntos con color blanco
       # sobre un fondo blanco, de tal manera que no se noten, pero se mantengan
       # todas las caracteristicas del formato que teniamos en el grafico 02.
       plot(x = COV, y = VR,
            col = "white",
            xlab = tabla01_variables$referencia[2],
            ylab = tabla01_variables$referencia[1],
            main = set_main03, type = "p", pch = 19)

       # Agregamos los trazos de rectas para cada nivel como segmentos
       segments(x0 = tabla09_plot$min_x_cov_i,
                y0 = tabla09_plot$est_min_x_cov_i,
                x1 = tabla09_plot$max_x_cov_i,
                y1 = tabla09_plot$est_max_x_cov_i,
                col = tabla09_plot$colores,
                lwd = 5)


       # En la porcion 2 se grafican las referencias del grafico 03
       # Para ello sobre la segunda porcion se genera un gŕafico totalmente blanco.
       plot(x = 0:10, y = 0:10, type = "n",
            xaxt = "n", yaxt = "n",
            xlab = "", ylab = "",
            xlim = c(0, 10), ylim = c(0, 10),
            col = "white", bty = "n")

       # Se agrega la leyenda
       legend("center", inset = c(-0.4, 0.3),
              legend = tabla09_plot$niveles,
              col = tabla09_plot$colores, pch = 16,
              title = tabla01_variables$referencia[3])

       # Se vuelven los parametros graficos a los valores originales
       par(mfrow = c(1, 1))

       #--------------- FIN GRAFICO 03 ------------------------------------------------



       # # Grafico 04) Puntos y Rectas con color
       #--------------- INICIO GRAFICO 04 ---------------------------------------------


       # Definimos un titulo para el grafico 04
       set_main04 <- paste0("Ancova con interaccion", "\n",
                            "Gráfico 04 - Puntos y Rectas con color", "\n",
                            xlsx_file_name)

       # Generamos un espacio grafico doble para poder colocar las referencias.
       par(mfrow = c(1, 2))

       # En la primera porcion, graficamos los puntos con color
       plot(x = COV, y = VR,
            col = COLORES,
            xlab = tabla01_variables$referencia[2],
            ylab = tabla01_variables$referencia[1],
            main = set_main04, type = "p", pch = 19)

       # Agregamos los segmentos de cada recta
       segments(x0 = tabla09_plot$min_x_cov_i,
                y0 = tabla09_plot$est_min_x_cov_i,
                x1 = tabla09_plot$max_x_cov_i,
                y1 = tabla09_plot$est_max_x_cov_i,
                col = tabla09_plot$colores,
                lwd = 5)

       # Ploteamos la 2da parte del grafico para agregar las referencias
       plot(x = 0:10, y = 0:10, type = "n",
            xaxt = "n", yaxt = "n",
            xlab = "", ylab = "",
            xlim = c(0, 10), ylim = c(0, 10),
            col = "white", bty = "n")

       # Agregamos la leyenda
       legend("center", inset = c(-0.4, 0.3),
              legend = tabla09_plot$niveles,
              col = tabla09_plot$colores, pch = 16,
              title = tabla01_variables$referencia[3])

       # Se vuelven los parametros graficos a los valores originales
       par(mfrow = c(1, 1))

       #--------------- FIN GRAFICO 04 ------------------------------------------------
       # # Grafico 05) Medias y Error Estandard
       #--------------- INICIO GRAFICO 05 ---------------------------------------------

       # # Hacemos algunas estimaciones para realizar el grafico

       # Estimacions de VR
       vector_medias_vr <- tapply(VR, FACTOR, mean)
       vector_sd_vr <- tapply(VR, FACTOR, sd)
       vector_length_vr <- tapply(VR, FACTOR, length)
       vector_se_vr <- vector_sd_vr/sqrt(vector_length_vr)

       # Estimaciones de COV
       vector_medias_cov <- tapply(COV, FACTOR, mean)
       vector_sd_cov <- tapply(VR, FACTOR, sd)
       vector_length_cov <- tapply(VR, FACTOR, length)
       vector_se_cov <- vector_sd_cov/sqrt(vector_length_cov)

       # Intervalo para el error stanrdar (Estos no son intervalos de confianza!)
       cantidad_ee <- 1
       vector_sup <- vector_medias_vr + cantidad_ee*vector_se_vr
       vector_inf <- vector_medias_vr - cantidad_ee*vector_se_vr

       # Definimos un titulo para el grafico 05
       set_main05 <- paste0("Ancova con interaccion", "\n",
                            "Gráfico 05 - Media y un Error Standard", "\n",
                            xlsx_file_name)

       # Generamos un espacio grafico doble para poder colocar las referencias.
       par(mfrow = c(1, 2))

       # Ploteamos las medias de los niveles del factor
       plot(x = vector_orden, y = vector_medias_vr,
            xaxt = "n",
            main = set_main05, type = "p", pch = 19,
            xlim = c(min(vector_orden), max(vector_orden)),
            ylim = c(min(VR), max(VR)),
            col = vector_colores,
            ylab = tabla01_variables$referencia[1],
            xlab = tabla01_variables$referencia[3])

       # Agregamos las especificaciones al eje X con el nombre de los niveles
       axis(1, at = vector_orden, labels = tabla09_plot$niveles, las = 1)

       # Agregamos los intervalos
       arrows(x0 = vector_orden,
              y0 = vector_inf,
              x1 = vector_orden,
              y1 = vector_sup,
              angle = 90, code = 3, length = 0.1)



       # Generamos la 2da porcion para agregar las referencias
       plot(x = 0:10, y = 0:10, type = "n",
            xaxt = "n", yaxt = "n",
            xlab = "", ylab = "",
            xlim = c(0, 10), ylim = c(0, 10),
            col = "white", bty = "n")

       # Agregamos las referencias
       legend("center", inset = c(-0.4, 0.3),
              legend = tabla09_plot$niveles,
              col = tabla09_plot$colores, pch = 16,
              title = tabla01_variables$referencia[3])

       # Se vuelven los parametros graficos a los valores originales
       par(mfrow = c(1, 1))

       #--------------- FIN GRAFICO 05 ---------------------------------------------


       # # Grafico 06) Boxplot
       #--------------- INICIO GRAFICO 06 ---------------------------------------------

       # Definimos un titulo para el grafico
       set_main06 <- paste0("Ancova con interaccion", "\n",
                            "Gráfico 06 - Boxplot", "\n",
                            xlsx_file_name)

       # Generamos un espacio grafico doble para poder colocar las referencias.
       par(mfrow = c(1, 2))

       # En la primera porcion, generamos el grafico de boxplot
       boxplot(VR ~ FACTOR, col = vector_colores,
               main = set_main06, type = "p", pch = 19,
               xlab = tabla01_variables$referencia[3],
               ylab = tabla01_variables$referencia[1])



       # Generamos la 2da porcion para agregar las referencias
       plot(x = 0:10, y = 0:10, type = "n",
            xaxt = "n", yaxt = "n",
            xlab = "", ylab = "",
            xlim = c(0, 10), ylim = c(0, 10),
            col = "white", bty = "n")

       # Agregamos las referencias
       legend("center", inset = c(-0.4, 0.3),
              legend = tabla09_plot$niveles,
              col = tabla09_plot$color, pch = 16,
              title = tabla01_variables$referencia[3])

       # Se vuelven los parametros graficos a los valores originales
       par(mfrow = c(1, 1))

       #--------------- FIN GRAFICO 06 ---------------------------------------------


       # # Grafico 07) Grafico XY con puntos de color
       #--------------- INICIO GRAFICO 07 ---------------------------------------------

       # Definimos un titulo para el grafico 07
       set_main07 <- paste0("Ancova con interaccion", "\n",
                            "Gráfico 07 - Puntos con color - Plotly", "\n",
                            xlsx_file_name)

       # Generamos el grafico 07
       # Por defecto, incluira las refrencias.
       fig07 <- plot_ly(x = COV,
                        y = VR,
                        color = FACTOR,
                        colors = vector_colores,
                        type = "scatter",
                        mode = "markers")

       # Agregamos titulo y detalles a los ejes
       fig07 <- fig07 %>%
         layout(
           title = set_main07,
           xaxis = list(title = tabla01_variables$referencia[2]),
           yaxis = list(title = tabla01_variables$referencia[1])
         )

       # Salida del grafico07
       fig07

       #--------------- FIN GRAFICO 07 ---------------------------------------------


       # # Grafico 08) Grafico XY solo rectas con color
       #--------------- INICIO GRAFICO 08 ---------------------------------------------

       # Definimos un titulo para el grafico 08
       set_main08 <- paste0("Ancova con interaccion", "\n",
                            "Gráfico 08 - Solo rectas - Plotly", "\n",
                            xlsx_file_name)

       # # Para realizar solo los segmentos de recta, la generacion del grafico
       # es ligeramente diferente.

       # Generamos un grafico vacio
       fig08 <- plot_ly()

       # Agregamos los segmentos de cada nivel del factor.
       fig08 <- fig08 %>%
         add_segments(data = tabla09_plot,
                      x = ~min_x_cov_i, y = ~est_min_x_cov_i,
                      xend = ~max_x_cov_i, yend = ~est_max_x_cov_i,
                      color = ~niveles,
                      colors = ~colores
         )

       # Agregamos a este grafico vacio detalles de los ejes y el titulo
       fig08 <- fig08 %>%
         layout(
           title = set_main08,
           xaxis = list(title = tabla01_variables$referencia[2]),
           yaxis = list(title = tabla01_variables$referencia[1])
         )

       # Salida del grafico08
       fig08

       #--------------- FIN GRAFICO 08 ---------------------------------------------



       # # Grafico 09) Grafico XY con puntos y rectas con color
       #--------------- INICIO GRAFICO 09 ---------------------------------------------

       # Definimos un titulo para el grafico 09
       set_main09 <- paste0("Ancova con interaccion", "\n",
                            "Gráfico 09 - Puntos y rectas con color - Plotly", "\n",
                            xlsx_file_name)

       # Generamos el grafico con los puntos
       fig09 <- plot_ly(x = COV,
                        y = VR,
                        color = as.character(FACTOR),
                        colors = vector_colores,
                        type = "scatter",
                        mode = "markers")

       # Agregamos especificaciones a los ejes y el titulo
       fig09 <- fig09 %>%
         layout(
           title = set_main09,
           xaxis = list(title = tabla01_variables$referencia[2]),
           yaxis = list(title = tabla01_variables$referencia[1])
         )

       # Agregamos los segmentos
       fig09 <- fig09 %>%
         add_segments(data = tabla09_plot,
                      x = ~min_x_cov_i, y = ~est_min_x_cov_i,
                      xend = ~max_x_cov_i, yend = ~est_max_x_cov_i,
                      color = ~niveles,
                      colors = ~colores)

       # Salida del grafico 09
       fig09

       #--------------- FIN GRAFICO 09 ---------------------------------------------
