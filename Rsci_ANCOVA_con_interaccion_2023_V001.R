

alpha_value <- 0.05

# Path completo del archivo excel
xlsx_path <- "./mi_base.xlsx"
xlsx_file_name <- tail(strsplit(xlsx_path, "/")[[1]], n = -1)
xlsx_file_name

sheeet_pos <- 1

database <- mtcars

pos_var_vr <- 1
pos_var_factor <- 2
pos_var_cov <- 3


#################################################################################

# Interno de R-science
selected_pos_vars <- c(pos_var_vr, pos_var_factor, pos_var_cov)


# # # Seccion 05 - minibase ---------------------------------------------------------
# Orden de las columnas: VR - FACTOR - COV
minibase <- na.omit(database[selected_pos_vars])
minibase[,2] <- as.factor(minibase[,2])
minibase

#####################################################################################

vector_niveles <- levels(minibase[,2])
cantidad_niveles <- length(vector_niveles)
vector_orden_niveles <- 1:cantidad_niveles

#####################################################################################

df_factor <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "freq" = table(minibase[,2]),
  "color" = rainbow(cantidad_niveles)
)

#####################################################################################

# Medidas de posicion particionadas (VR)
df_position_vr <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "min" = tapply(minibase[,1], minibase[,2], min),
  "mean" = tapply(minibase[,1], minibase[,2], mean),
  "median" = tapply(minibase[,1], minibase[,2], median),
  "max" = tapply(minibase[,1], minibase[,2], max),
  "n" = tapply(minibase[,1], minibase[,2], length)
)



# Medidas de dispersion particionadas (VR)
df_dispersion_vr <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "rango" = tapply(minibase[,1], minibase[,2], function(x){max(x) - min(x)}),
  "varianza" = tapply(minibase[,1], minibase[,2], var),
  "desvio_estandard" = tapply(minibase[,1], minibase[,2], sd),
  "error_estandard" = tapply(minibase[,1], minibase[,2], function(x){sd(x)/sqrt(length(x))}),
  "n" = tapply(minibase[,1], minibase[,2], length)
)
#########################################################################################

# Medidas de posicion particionadas (COV)
df_position_cov <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "min" = tapply(minibase[,3], minibase[,2], min),
  "mean" = tapply(minibase[,3], minibase[,2], mean),
  "median" = tapply(minibase[,3], minibase[,2], median),
  "max" = tapply(minibase[,3], minibase[,2], max),
  "n" = tapply(minibase[,3], minibase[,2], length)
)



# Medidas de dispersion particionadas  (COV)
df_dispersion_cov <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "rango" = tapply(minibase[,3], minibase[,2], function(x){max(x) - min(x)}),
  "varianza" = tapply(minibase[,3], minibase[,2], var),
  "desvio_estandard" = tapply(minibase[,3], minibase[,2], sd),
  "error_estandard" = tapply(minibase[,3], minibase[,2], function(x){sd(x)/sqrt(length(x))}),
  "n" = tapply(minibase[,3], minibase[,2], length)
)



################################################################################

# Analisis
set_var <- colnames(minibase)
names(set_var) <- c("VR", "FACTOR", "COV")

sentencia_modelo_ancova_con <- "lm(VR ~ COV + FACTOR + FACTOR:COV, data = minibase)"
sentencia_ejecucion_ancova_con <- stringr::str_replace_all(string = sentencia_modelo_ancova_con,
                                                           set_var)


lm_ancova_con <- eval(parse(text = sentencia_ejecucion_ancova_con))
ancova_full_con <- aov(lm_ancova_con)
info_rectas_ancova_con <- coefficients(ancova_full_con)
tabla_ancova_con <- summary(ancova_full_con)[[1]]




######################################################################################


minibase_mod <- minibase
minibase_mod$"fitted.values" <- lm_ancova_con$fitted.values
minibase_mod$"residuals" <- lm_ancova_con$residuals
minibase_mod$"id_minibase" <- 1:nrow(minibase)
minibase_mod$"lvl_order_number" <- as.numeric(minibase[,2])
minibase_mod$"lvl_color" <- df_factor$color[minibase_mod$"lvl_order_number"]



######################################################################################

# Medidas de posicion particionadas (residuals)
df_position_residuals <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "min" = tapply(minibase_mod$residuals, minibase_mod[,2], min),
  "mean" = tapply(minibase_mod$residuals, minibase_mod[,2], mean),
  "median" = tapply(minibase_mod$residuals, minibase_mod[,2], median),
  "max" = tapply(minibase_mod$residuals, minibase_mod[,2], max),
  "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
)



# Medidas de dispersion particionadas  (residuals)
df_dispersion_residuals <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "rango" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){max(x) - min(x)}),
  "varianza" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
  "desvio_estandard" = tapply(minibase_mod$residuals, minibase_mod[,2], sd),
  "error_estandard" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){sd(x)/sqrt(length(x))}),
  "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
)



# # # Seccion 09 - Requisitos del modelo de Ancova con interacción ------------------
# Test de Normalidad de Shapiro-Wilk
test_normality_residuals <- shapiro.test(minibase_mod$residuals)
test_normality_residuals


test_homogeneity_residuals <- bartlett.test(x = minibase_mod$residuals, g = minibase_mod[,2])
test_homogeneity_residuals


suma_residuos <- sum(minibase_mod$residuals)
suma_residuos

# # Sección 09-2) Tabla Tukey --------------------------------------------------
# Tukey completo
tukey01_full_groups <- agricolae::HSD.test(y = lm_ancova_con,
                                           trt = colnames(minibase)[2],
                                           alpha = alpha_value,
                                           group = TRUE,
                                           console = FALSE,
                                           unbalanced = T)

tukey02_full_pairs <- agricolae::HSD.test(y = lm_ancova_con,
                                          trt = colnames(minibase)[2],
                                          alpha = alpha_value,
                                          group = FALSE,
                                          console = FALSE,
                                          unbalanced = T)


########################################################

vector_medias_vr <- tukey01_full_groups$means[,1]
names(vector_medias_vr) <- rownames(tukey01_full_groups$means)

mdlm_vr <- mean(vector_medias_vr)
vector_mdlm_vr <- rep(mdlm_vr, cantidad_niveles)

df_tukey <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "mu" = vector_mdlm_vr,
  "tau_i" = vector_medias_vr - vector_mdlm_vr,
  "mu_i" = vector_medias_vr,
  "groups" = tukey01_full_groups$groups$groups
)


# Suma de los efectos tau
suma_tau <- sum(df_tukey$tau_i)
suma_tau






# # Sección 09-3) Tabla con las pendientes y ordenadas de cada factor ----------
# Ecuaciones de cada recta
# Ordenadas de los niveles del factor
pos_ordenadas <- c((1:(cantidad_niveles-1)) + 2)
vector_ordenadas <- c(info_rectas_ancova_con[1], info_rectas_ancova_con[pos_ordenadas] + info_rectas_ancova_con[1])
names(vector_ordenadas) <-


# Pendientes de los niveles dle factor
pos_pendientes <- c((cantidad_niveles + 2):length(info_rectas_ancova_con))
vector_pendientes <- c(info_rectas_ancova_con[2], info_rectas_ancova_con[pos_pendientes] + info_rectas_ancova_con[2])
names(vector_pendientes) <- vector_niveles


df_rectas <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "pendientes" = vector_pendientes,
  "ordenadas" = vector_ordenadas
)




# # Sección 09-4) Tabla con las estimaciones del modelo ------------------------
# Estimacion de la interaccion en las combinaciones COV:FACTOR
pendiente_general <- mean(vector_pendientes)
vector_pendiente_general <- rep(pendiente_general, length(vector_pendientes))


# Efectos de interaccion
vector_interaccion <- vector_pendientes - vector_pendiente_general
names(vector_interaccion) <- vector_niveles

df_interaccion <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "beta" = vector_pendiente_general,
  "gamma_i" = vector_interaccion,
  "beta_i" = vector_pendientes
)


# Suma de las interacciones
suma_interaccion <- sum(vector_interaccion)
suma_interaccion






# # Sección 09-6) Tabla con estimaciones del modelo de Ancova (modelo largo) ---

df_modelo_ancova_con_largo <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "mu" = df_tukey$mu,
  "tau_i" = df_tukey$tau_i,
  "beta" = df_interaccion$beta,
  "gamma_i" = df_interaccion$gamma_i,
  "mean_cov_i" = df_position_cov$mean,
  "min_cov_i" = df_position_cov$min,
  "max_cov_i" = df_position_cov$max
)



df_modelo_ancova_con_short <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "mu_i" = df_tukey$mu_i,
  "beta_i" = df_interaccion$beta_i,
  "mean_cov_i" = df_position_cov$mean,
  "min_cov_i" = df_position_cov$min,
  "max_cov_i" = df_position_cov$max
)



vector_initial_point_y <- vector_medias_vr + (vector_pendientes*(df_position_cov$min - df_position_cov$mean))
vector_end_point_y <- vector_medias_vr + (vector_pendientes*(df_position_cov$max - df_position_cov$mean))


df_segments <- data.frame(
  "orden" = vector_orden_niveles,
  "level" = vector_niveles,
  "min_cov_i_x" = df_position_cov$min,
  "max_cov_i_x" = df_position_cov$max,
  "initial_point_i_y" = vector_initial_point_y,
  "end_point_i_x" = vector_end_point_y
)


# Obtener todos los nombres de objetos en el entorno actual
objetos <- ls()

# Filtrar los nombres que comienzan con "df_"
objetos_df <- objetos[grep("^df_", objetos)]

# Crear una lista con los objetos que cumplen con el criterio utilizando sapply
lista_objetos <- sapply(objetos_df, function(nombre) {
  get(nombre)  # Obtener el objeto por su nombre
}, simplify = FALSE)

#
#
# aver <- boxplot(mtcars, range = 0)
#
#
#
#
#
# # # Grafico 1) Gráfico XY - sencillo
# #--------------- INICIO GRAFICO 01 ---------------------------------------------
#
# # Definimos un titulo para el grafico
# set_main01 <- paste0("Ancova con interaccion", "\n",
#                      "Gráfico 01 - Puntos básico", "\n",
#                      xlsx_file_name)
#
# # Generamos el grafico 01
# plot(x = COV, y = VR,
#      #col = COLORES,
#      xlab = tabla01_variables$referencia[2],
#      ylab = tabla01_variables$referencia[1],
#      main = set_main01)
#
# #--------------- FIN GRAFICO 01 ------------------------------------------------
#
#
#
#
# # # Grafico 02) Puntos con color
# #--------------- INICIO GRAFICO 02 ---------------------------------------------
#
# # Definimos un titulo para el grafico 02
# set_main02 <- paste0("Ancova con interaccion", "\n",
#                      "Gráfico 02 - Puntos con color", "\n",
#                      xlsx_file_name)
#
# # Generamos un espacio grafico doble para poder colocar las referencias.
# par(mfrow = c(1, 2))
#
# # En la porcion 1 se plotea el grafico 02
# plot(x = COV, y = VR,
#      col = COLORES,
#      xlab = tabla01_variables$referencia[2],
#      ylab = tabla01_variables$referencia[1],
#      main = set_main02, type = "p", pch = 19)
#
# # En la porcion 2 se grafican las referencias del grafico 02
# # Para ello sobre la segunda porcion se genera un gŕafico totalmente blanco.
# plot(x = 0:10, y = 0:10, type = "n",
#      xaxt = "n", yaxt = "n",
#      xlab = "", ylab = "",
#      xlim = c(0, 10), ylim = c(0, 10),
#      col = "white", bty = "n")
#
# # Se agrega la leyenda
# legend("center", inset = c(-0.4, 0.3),
#        legend = tabla03_tukey$niveles,
#        col = vector_colores, pch = 16,
#        title = tabla01_variables$referencia[3])
#
# # Se vuelven los parametros graficos a los valores originales
# par(mfrow = c(1, 1))
#
# #--------------- FIN GRAFICO 02 ------------------------------------------------
#
#
# # # Grafico 03) Rectas con color
# #--------------- INICIO GRAFICO 03 ---------------------------------------------
#
#
# # Definimos un titulo para el grafico 03
# set_main03 <- paste0("Ancova con interaccion", "\n",
#                      "Gráfico 03 - Rectas con color", "\n",
#                      xlsx_file_name)
#
# # Generamos un espacio grafico doble para poder colocar las referencias.
# par(mfrow = c(1, 2))
#
# # En la primera porción gráfica, colocamos los puntos con color blanco
# # sobre un fondo blanco, de tal manera que no se noten, pero se mantengan
# # todas las caracteristicas del formato que teniamos en el grafico 02.
# plot(x = COV, y = VR,
#      col = "white",
#      xlab = tabla01_variables$referencia[2],
#      ylab = tabla01_variables$referencia[1],
#      main = set_main03, type = "p", pch = 19)
#
# # Agregamos los trazos de rectas para cada nivel como segmentos
# segments(x0 = tabla09_plot$min_x_cov_i,
#          y0 = tabla09_plot$est_min_x_cov_i,
#          x1 = tabla09_plot$max_x_cov_i,
#          y1 = tabla09_plot$est_max_x_cov_i,
#          col = tabla09_plot$colores,
#          lwd = 5)
#
#
# # En la porcion 2 se grafican las referencias del grafico 03
# # Para ello sobre la segunda porcion se genera un gŕafico totalmente blanco.
# plot(x = 0:10, y = 0:10, type = "n",
#      xaxt = "n", yaxt = "n",
#      xlab = "", ylab = "",
#      xlim = c(0, 10), ylim = c(0, 10),
#      col = "white", bty = "n")
#
# # Se agrega la leyenda
# legend("center", inset = c(-0.4, 0.3),
#        legend = tabla09_plot$niveles,
#        col = tabla09_plot$colores, pch = 16,
#        title = tabla01_variables$referencia[3])
#
# # Se vuelven los parametros graficos a los valores originales
# par(mfrow = c(1, 1))
#
# #--------------- FIN GRAFICO 03 ------------------------------------------------
#
#
#
# # # Grafico 04) Puntos y Rectas con color
# #--------------- INICIO GRAFICO 04 ---------------------------------------------
#
#
# # Definimos un titulo para el grafico 04
# set_main04 <- paste0("Ancova con interaccion", "\n",
#                      "Gráfico 04 - Puntos y Rectas con color", "\n",
#                      xlsx_file_name)
#
# # Generamos un espacio grafico doble para poder colocar las referencias.
# par(mfrow = c(1, 2))
#
# # En la primera porcion, graficamos los puntos con color
# plot(x = COV, y = VR,
#      col = COLORES,
#      xlab = tabla01_variables$referencia[2],
#      ylab = tabla01_variables$referencia[1],
#      main = set_main04, type = "p", pch = 19)
#
# # Agregamos los segmentos de cada recta
# segments(x0 = tabla09_plot$min_x_cov_i,
#          y0 = tabla09_plot$est_min_x_cov_i,
#          x1 = tabla09_plot$max_x_cov_i,
#          y1 = tabla09_plot$est_max_x_cov_i,
#          col = tabla09_plot$colores,
#          lwd = 5)
#
# # Ploteamos la 2da parte del grafico para agregar las referencias
# plot(x = 0:10, y = 0:10, type = "n",
#      xaxt = "n", yaxt = "n",
#      xlab = "", ylab = "",
#      xlim = c(0, 10), ylim = c(0, 10),
#      col = "white", bty = "n")
#
# # Agregamos la leyenda
# legend("center", inset = c(-0.4, 0.3),
#        legend = tabla09_plot$niveles,
#        col = tabla09_plot$colores, pch = 16,
#        title = tabla01_variables$referencia[3])
#
# # Se vuelven los parametros graficos a los valores originales
# par(mfrow = c(1, 1))
#
# #--------------- FIN GRAFICO 04 ------------------------------------------------
# # # Grafico 05) Medias y Error Estandard
# #--------------- INICIO GRAFICO 05 ---------------------------------------------
#
# # # Hacemos algunas estimaciones para realizar el grafico
#
# # Estimacions de VR
# vector_medias_vr <- tapply(VR, FACTOR, mean)
# vector_sd_vr <- tapply(VR, FACTOR, sd)
# vector_length_vr <- tapply(VR, FACTOR, length)
# vector_se_vr <- vector_sd_vr/sqrt(vector_length_vr)
#
# # Estimaciones de COV
# vector_medias_cov <- tapply(COV, FACTOR, mean)
# vector_sd_cov <- tapply(VR, FACTOR, sd)
# vector_length_cov <- tapply(VR, FACTOR, length)
# vector_se_cov <- vector_sd_cov/sqrt(vector_length_cov)
#
# # Intervalo para el error stanrdar (Estos no son intervalos de confianza!)
# cantidad_ee <- 1
# vector_sup <- vector_medias_vr + cantidad_ee*vector_se_vr
# vector_inf <- vector_medias_vr - cantidad_ee*vector_se_vr
#
# # Definimos un titulo para el grafico 05
# set_main05 <- paste0("Ancova con interaccion", "\n",
#                      "Gráfico 05 - Media y un Error Standard", "\n",
#                      xlsx_file_name)
#
# # Generamos un espacio grafico doble para poder colocar las referencias.
# par(mfrow = c(1, 2))
#
# # Ploteamos las medias de los niveles del factor
# plot(x = vector_orden, y = vector_medias_vr,
#      xaxt = "n",
#      main = set_main05, type = "p", pch = 19,
#      xlim = c(min(vector_orden), max(vector_orden)),
#      ylim = c(min(VR), max(VR)),
#      col = vector_colores,
#      ylab = tabla01_variables$referencia[1],
#      xlab = tabla01_variables$referencia[3])
#
# # Agregamos las especificaciones al eje X con el nombre de los niveles
# axis(1, at = vector_orden, labels = tabla09_plot$niveles, las = 1)
#
# # Agregamos los intervalos
# arrows(x0 = vector_orden,
#        y0 = vector_inf,
#        x1 = vector_orden,
#        y1 = vector_sup,
#        angle = 90, code = 3, length = 0.1)
#
#
#
# # Generamos la 2da porcion para agregar las referencias
# plot(x = 0:10, y = 0:10, type = "n",
#      xaxt = "n", yaxt = "n",
#      xlab = "", ylab = "",
#      xlim = c(0, 10), ylim = c(0, 10),
#      col = "white", bty = "n")
#
# # Agregamos las referencias
# legend("center", inset = c(-0.4, 0.3),
#        legend = tabla09_plot$niveles,
#        col = tabla09_plot$colores, pch = 16,
#        title = tabla01_variables$referencia[3])
#
# # Se vuelven los parametros graficos a los valores originales
# par(mfrow = c(1, 1))
#
# #--------------- FIN GRAFICO 05 ---------------------------------------------
#
#
# # # Grafico 06) Boxplot
# #--------------- INICIO GRAFICO 06 ---------------------------------------------
#
# # Definimos un titulo para el grafico
# set_main06 <- paste0("Ancova con interaccion", "\n",
#                      "Gráfico 06 - Boxplot", "\n",
#                      xlsx_file_name)
#
# # Generamos un espacio grafico doble para poder colocar las referencias.
# par(mfrow = c(1, 2))
#
# # En la primera porcion, generamos el grafico de boxplot
# boxplot(VR ~ FACTOR, col = vector_colores,
#         main = set_main06, type = "p", pch = 19,
#         xlab = tabla01_variables$referencia[3],
#         ylab = tabla01_variables$referencia[1])
#
#
#
# # Generamos la 2da porcion para agregar las referencias
# plot(x = 0:10, y = 0:10, type = "n",
#      xaxt = "n", yaxt = "n",
#      xlab = "", ylab = "",
#      xlim = c(0, 10), ylim = c(0, 10),
#      col = "white", bty = "n")
#
# # Agregamos las referencias
# legend("center", inset = c(-0.4, 0.3),
#        legend = tabla09_plot$niveles,
#        col = tabla09_plot$color, pch = 16,
#        title = tabla01_variables$referencia[3])
#
# # Se vuelven los parametros graficos a los valores originales
# par(mfrow = c(1, 1))
#
# #--------------- FIN GRAFICO 06 ---------------------------------------------
#
#
# # # Grafico 07) Grafico XY con puntos de color
# #--------------- INICIO GRAFICO 07 ---------------------------------------------
#
# # Definimos un titulo para el grafico 07
# set_main07 <- paste0("Ancova con interaccion", "\n",
#                      "Gráfico 07 - Puntos con color - Plotly", "\n",
#                      xlsx_file_name)
#
# # Generamos el grafico 07
# # Por defecto, incluira las refrencias.
# fig07 <- plot_ly(x = COV,
#                  y = VR,
#                  color = FACTOR,
#                  colors = vector_colores,
#                  type = "scatter",
#                  mode = "markers")
#
# # Agregamos titulo y detalles a los ejes
# fig07 <- fig07 %>%
#   layout(
#     title = set_main07,
#     xaxis = list(title = tabla01_variables$referencia[2]),
#     yaxis = list(title = tabla01_variables$referencia[1])
#   )
#
# # Salida del grafico07
# fig07
#
# #--------------- FIN GRAFICO 07 ---------------------------------------------
#
#
# # # Grafico 08) Grafico XY solo rectas con color
# #--------------- INICIO GRAFICO 08 ---------------------------------------------
#
# # Definimos un titulo para el grafico 08
# set_main08 <- paste0("Ancova con interaccion", "\n",
#                      "Gráfico 08 - Solo rectas - Plotly", "\n",
#                      xlsx_file_name)
#
# # # Para realizar solo los segmentos de recta, la generacion del grafico
# # es ligeramente diferente.
#
# # Generamos un grafico vacio
# fig08 <- plot_ly()
#
# # Agregamos los segmentos de cada nivel del factor.
# fig08 <- fig08 %>%
#   add_segments(data = tabla09_plot,
#                x = ~min_x_cov_i, y = ~est_min_x_cov_i,
#                xend = ~max_x_cov_i, yend = ~est_max_x_cov_i,
#                color = ~niveles,
#                colors = ~colores
#   )
#
# # Agregamos a este grafico vacio detalles de los ejes y el titulo
# fig08 <- fig08 %>%
#   layout(
#     title = set_main08,
#     xaxis = list(title = tabla01_variables$referencia[2]),
#     yaxis = list(title = tabla01_variables$referencia[1])
#   )
#
# # Salida del grafico08
# fig08
#
# #--------------- FIN GRAFICO 08 ---------------------------------------------
#
#
#
# # # Grafico 09) Grafico XY con puntos y rectas con color
# #--------------- INICIO GRAFICO 09 ---------------------------------------------
#
# # Definimos un titulo para el grafico 09
# set_main09 <- paste0("Ancova con interaccion", "\n",
#                      "Gráfico 09 - Puntos y rectas con color - Plotly", "\n",
#                      xlsx_file_name)
#
# # Generamos el grafico con los puntos
# fig09 <- plot_ly(x = COV,
#                  y = VR,
#                  color = as.character(FACTOR),
#                  colors = vector_colores,
#                  type = "scatter",
#                  mode = "markers")
#
# # Agregamos especificaciones a los ejes y el titulo
# fig09 <- fig09 %>%
#   layout(
#     title = set_main09,
#     xaxis = list(title = tabla01_variables$referencia[2]),
#     yaxis = list(title = tabla01_variables$referencia[1])
#   )
#
# # Agregamos los segmentos
# fig09 <- fig09 %>%
#   add_segments(data = tabla09_plot,
#                x = ~min_x_cov_i, y = ~est_min_x_cov_i,
#                xend = ~max_x_cov_i, yend = ~est_max_x_cov_i,
#                color = ~niveles,
#                colors = ~colores)
#
# # Salida del grafico 09
# fig09
#
# #--------------- FIN GRAFICO 09 ---------------------------------------------
