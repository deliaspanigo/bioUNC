# Space slr
vector_show_vars_slr <- c("VR" = T, "X" = T, "id_database" = T, "id_minibase" = T)
vector01_slr <- c("Seleccionar fuente" = "","R-science examples" = "example", "xlsx" = "xlsx")
vector02_slr <- c("Seleccionar base" = "", "mtcars_mod", "iris_mod", "mtcars", "iris")
space_dummy <- "space_dummy"
module_dummy_rscience_ui <- function(id){
  ns <- shiny::NS(id)

  div(
    h2("Initial user election - Dummy"),
    fluidRow(
      column(4,
             selectInput(inputId = ns("file_source"), label = "Fuente de datos",
                         choices = vector01_slr,
                         selected = vector01_slr[1]),
             conditionalPanel(condition = 'input.file_source == "xlsx"',ns = ns,
                              fileInput(inputId = ns("info_xlsx"),
                                        label = "Cargar archivo Excel"),
                              uiOutput(ns("sheet_selection"))
             ),
             conditionalPanel(condition = 'input.file_source == "example"', ns = ns,
                              selectInput(inputId = ns("file_example"),
                                          label = "Example",
                                          choices = vector02_slr,
                                          selected = vector02_slr[1]))
      ),
      column(4, uiOutput(ns("vars_selection"))),
      column(4,
             selectInput(inputId = ns("alpha_value"), label = "Alpha",
                         choices = c(0.10, 0.05, 0.01),
                         selected = 0.05),

             selectInput(inputId = ns("n_digits"), label = "Digits",
                         choices = 0:10, selected = 4)
      )
    ), # End fluidRow
    uiOutput(ns("slr_full_show_initial")),
    uiOutput(ns("slr_full_tabsetpanel"))

  ) # End div
}
module_dummy_rscience_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {


      # # # # UiOutput for inputs and var selection
      # Sheet selection for xlsx
      output$sheet_selection <- renderUI({

        ns <- shiny::NS(id)

        req(input$info_xlsx, input$info_xlsx$name, input$info_xlsx$datapath)

        ext <- tools::file_ext(input$info_xlsx$name)
        validate(need(ext == "xlsx", "Please upload a xlsx file"))

        name_sheets <- getSheetNames(input$info_xlsx$datapath)
        set_options <- super_sheet_info(name_sheets = name_sheets)

        set_options <- c("Seleccione una... " = "", set_options)
        pos_selected <- 1

        selectInput(inputId = ns("sheet_xlsx"),
                    label = "Seleccionar hoja:", choices = set_options,
                    selected = set_options[pos_selected])

      })

      # var selection
      output$vars_selection <- renderUI({

        ns <- shiny::NS(id)

        req(control01_database())



        set_options <- super_var_info(database = database())
        set_options <- c("Seleccione una..." = "", set_options)


        div(
          fluidRow(
            selectInput(inputId = ns("var_vr"), label = "Variable Respuesta",
                        choices = set_options ,
                        selected = set_options[1]),
            selectInput(inputId = ns("var_reg"), label = "Variable Regresora (X)",
                        choices = set_options,
                        selected = set_options[1])
          )
        )
      })

      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # Very important input objects
      # database reactive
      database <- reactive({

        req(input$file_source)
        req(input$file_source != "")

        if(input$file_source == "xlsx"){

          req(input$info_xlsx, input$info_xlsx$name, input$info_xlsx$datapath, input$sheet_xlsx)

          ext <- tools::file_ext(input$info_xlsx$name)
          #validate(need(ext == "xlsx", "Please upload a csv file"))


          if (grepl(".xlsx$", input$info_xlsx$name)) {
            # Para archivos XLSX
            output_df <- openxlsx::read.xlsx(xlsxFile = input$info_xlsx$datapath, sheet = input$"sheet_xlsx")

            #colnames(output_df) <- make.names(colnames(output_df))
            return(output_df)
          } else {
            return(NULL)
          }
        }

        else if (input$file_source == "example"){


          #armado <- "data_example <- input$file_example"
          data_example <- eval(parse(text = input$file_example))

          if(!is.null(data_example)){
            rownames(data_example) <- c(1:nrow(data_example))
          }

        }

        return(data_example)
      })

      all_colnames <- reactive({
        req(database())
        colnames(database())
      })

      # alpha_value reactive
      alpha_value <- reactive({

        as.numeric(input$alpha_value)

      })

      # digits_value reactive
      digits_value <- reactive({

        as.numeric(input$digits_value)

      })

      # Reactive - selected colname vars
      selected_pos_vars <- reactive({

        req(control02_vars_selection())
        #    req(input$var_vr, input$var_factor, input$var_cov)
        #    req(input$var_vr != "", input$var_factor != "", input$var_cov != "")

        vector_pos <- c(input$var_vr, input$var_reg)
        vector_pos <- as.integer(as.numeric(vector_pos))
        names(vector_pos) <- c("VR", "X")
        vector_pos
      })


      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # RESULTS
      # Results - Ancova with
      results_all_slr <- reactive({

        req(control02_vars_selection())
        the_output <- slr_full_gen02(database = database(),
                                     pos_var_vr = selected_pos_vars()[1],
                                     pos_var_reg = selected_pos_vars()[2],
                                     alpha_value = alpha_value())


        #the_output <- do.call(c, the_output)
        #the_output <- unlist(the_output)

        the_output

      })




      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # RESET OPTIONS
      observeEvent(input$file_source, {
        ns <- shiny::NS(id)

        if(input$file_source == "xlsx"){

          updateSelectInput(session, inputId = ns("file_example"), label = "Example",
                            choices = vector02_slr,
                            selected = vector02_slr[1])

        }

        if(input$file_source == "example"){

          updateSelectInput(session, inputId = ns("file_example"), label = "Example",
                            choices = vector02_slr,
                            selected = vector02_slr[1])

        }

      })


      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


      # # # # INTERNAL CONTROLS
      # Control 01 - database
      control01_database <- reactive({

        dt_database <- is.null(database())


        if(dt_database) return(FALSE) else return(TRUE)


      })

      # Control 02 - seleceted vars
      control02_vars_selection <- reactive({

        req(control01_database())

        if(is.null(input$var_vr)) return(FALSE) else
          if(is.null(input$var_reg)) return(FALSE) else
            if(input$var_vr == "") return(FALSE) else
              if(input$var_reg == "") return(FALSE) else return(TRUE)

      })





      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # INITIAL SHOW - ANOVA
      # Initial show complete
      output$slr_full_show_initial <- renderUI({
        ns <- shiny::NS(id)
        div(tableOutput(ns("show_initial_database"))
        )
      })

      # Initial show for database
      output$show_initial_database <- renderTable({

        #cat(control02_selected_vars())
        #req(!control02_selected_vars())
        req(database())
        req(!control02_vars_selection())
        database()

      }, rownames = TRUE, align = "c")



      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # TABSETPANEL FULL - ANOVA
      output$slr_full_tabsetpanel <- renderUI({
        ns <- shiny::NS(id)

        req(control01_database(), control02_vars_selection())


        shiny::tabsetPanel(
          shiny::tabPanel("database", # 01
                          tableOutput(ns("database"))
          ),
          tabPanel("Requeriments", # 04
                   fluidRow(
                     column(6,
                            h2("Simple Linear Regression"),
                            verbatimTextOutput(ns("requeriments_slr"))
                     ),
                     column(6,
                            h2("Residuals vs Fitted (Homogeneity)"),
                            plotOutput(ns("requeriments_homogeinity_slr")))




                   )
          ),




          tabPanel("Analysis",  # 05
                   fluidRow(
                     column(12,
                            h2("Anova 1 way"),
                            verbatimTextOutput(ns("analysis_slr")))
                   )
          ),
          tabPanel("graphs", #07
                   fluidRow(
                     column(12,
                            h2("Simple Linear Regression"),
                            plotOutput(ns("graph01_slr")))
                   )
          ),
          tabPanel("R code",  #08
                   fluidRow(
                     column(12,
                            h2("Simple Linear Regression"),
                            verbatimTextOutput(ns("code_slr")))
                   )

          ),
          tabPanel("Full R Results", # 02
                   fluidRow(
                     column(12,
                            h2("Anova 1 way"),
                            verbatimTextOutput(ns("results_all_slr")))
                   )
          ),
          tabPanel("minibase", # 03
                   h2("Selected vars"),
                   fluidRow(tableOutput(ns("df_selected_vars"))), br(),
                   h2("R enviroments - Control on minibase"),
                   fluidRow(tableOutput(ns("df_control_minibase"))), br(),
                   h2("database & minibase"),
                   fluidRow(tableOutput(ns("df_show_n"))),br(),br(),



                   fluidRow(
                     column(4,
                            h2("Selected vars - database"),
                            tableOutput(ns("database_selected_vars"))),
                     column(4,
                            h2("Selected vars - minibase"),
                            tableOutput(ns("minibase_selected_vars"))),
                     column(4, uiOutput(ns("ref_selection")))
                   ),

                   br(), br(), br(), br(), br(), br()

          ),
          tabPanel("minibase_mod", #06
                   fluidRow(
                     column(12,
                            h2("Anova 1 way"),
                            verbatimTextOutput(ns("minibase_mod_slr")))

                   )

          ),

          tabPanel("R-science code"),
          tabPanel("Download"),
          tabPanel("Control"),
          tabPanel("User info")
        )

      })


      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # Tab 01 - database
      output$database <- renderTable({
        req(database())

        database()
        #mtcars
      }, rownames = TRUE, align = "c")


      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # Tab 02 - Full R Results...
      # Ancova with...
      output$results_all_slr <- renderPrint({

        results_all_slr()
      })

      output$results_all_anova <- renderPrint({

        results_all_slr()
      })



      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # Tab 03 - minibase...
      df_selected_vars <- reactive({

        req(results_all_slr())
        results_all_slr()$df_selected_vars

      })


      output$df_selected_vars <- renderTable({
        req(df_selected_vars())
        df_selected_vars()

      }, rownames = FALSE, align = "c")


      output$df_control_minibase <- renderTable({

        req(results_all_slr())
        results_all_slr()$df_control_minibase


      }, rownames = FALSE, align = "c")


      output$df_show_n <- renderTable({

        req(results_all_slr())
        results_all_slr()$df_show_n


      }, rownames = FALSE, align = "c")




      output$ref_selection <- renderUI({
        ns <- shiny::NS(id)


        div(
          radioButtons(inputId = ns("show_reference"),
                       label = "Selected Reference",
                       choices = colnames(df_selected_vars ())[c(5,6)]),

          radioButtons(inputId = ns("n_rows"),
                       label = "Show rows",
                       choices = c("10" = 10, "20" = 20, "All" = "All")),
          checkboxGroupInput(inputId = ns("show_vars"),
                             label = "Visualización",
                             choices = names(vector_show_vars_slr),
                             selected = names(vector_show_vars_slr))
        )
      })


      dt_show_vars <- reactive({



        names(vector_show_vars_slr) %in% unlist(input$show_vars)
      })

      #observe(print(dt_show_vars()))

      #
      # Output - database only selected cols
      output$database_selected_vars <- renderTable({

        # # Req
        req(database(), selected_pos_vars(), df_selected_vars())
        req(input$n_rows, dt_show_vars, input$show_reference)
        req(input$show_reference != 0)



        last_row <- input$n_rows

        gen_data <- database()[selected_pos_vars()]
        gen_data["id_database"] <- 1:nrow(database())

        dt_row_without_na <- rowSums(is.na(gen_data)) == 0
        acum_sum <- cumsum(dt_row_without_na)
        acum_sum[!dt_row_without_na] <- NA
        gen_data["id_minibase"] <- acum_sum
        # gen_data <- visual_id_data_ancova(database = database(),
        #                                   selected_vars = selected_pos_vars(),
        #                                   selected_case = "database")

        #gen_data

        #
        #gen_data <- gen_data[dt_show_vars()]
        #
        if(input$n_rows == "All") last_row <- nrow(gen_data)
        selected_rows <- 1:last_row

        gen_data <- gen_data[selected_rows, ]
        #
        gen_data <- gen_data[dt_show_vars()]

        if(ncol(gen_data) > 0) return(gen_data) else return(NULL)

      }, rownames = TRUE, align = "c")
      #
      # Output - database only selected cols
      output$minibase_selected_vars <- renderTable({

        req(database(), selected_pos_vars(), df_selected_vars())
        req(input$n_rows, dt_show_vars, input$show_reference)
        req(input$show_reference != 0)

        last_row <- input$n_rows


        var_show_selection <- c("VR", "X", "id_database", "id_minibase")

        new_names <- df_selected_vars()[[input$show_reference]] # var_role	doble_reference
        #print(aver)
        gen_data <- results_all_slr()$minibase_mod[var_show_selection]

        colnames(gen_data)[1:length(new_names)] <- new_names

        # gen_data <- visual_id_data_ancova(database = database(),
        #                                   selected_vars = selected_pos_vars(),
        #                                   selected_case = "minibase")
        #
        # colnames(gen_data)[c(3,4,5)] <- aver
        #
        # gen_data <- gen_data[dt_show_vars()]

        if(input$n_rows == "All") last_row <- nrow(gen_data)
        selected_rows <- 1:last_row

        gen_data <- gen_data[selected_rows, ]
        gen_data <- gen_data[dt_show_vars()]

        if(ncol(gen_data) > 0) return(gen_data) else return(NULL)
      }, rownames = TRUE, align = "c")


      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


      # # # # Tab 04 - Requeriments...
      output$requeriments_slr <- renderPrint({

        mi_lista <- results_all_slr()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("test_normality_residuals",
                           "df_position_residuals_general", "df_dispersion_residuals_general",
                           "sum_residuals", "mean_residuals")

        # Usar lapply para mostrar los elementos deseados
        elementos_a_ver <- lapply(nombres_a_ver, function(nombre) mi_lista[[nombre]])
        names(elementos_a_ver) <- nombres_a_ver
        elementos_a_ver

      })

      output$requeriments_homogeinity_slr <- renderPlot({

        minibase_mod <- results_all_slr()$minibase_mod
        df_selected_vars <- results_all_slr()$df_selected_vars
        #df_factor <- results_all_slr()$df_factor


        set_main04 <- paste0("Simple Linear Regression", "\n",
                             "Gráfico 01 - Studentized residuals vs. Fitted values", "\n")

        min_abs_y <- 3
        general_max_y <- ceiling(max(abs(minibase_mod$rstandard))) + 1
        if(general_max_y < min_abs_y) general_max_y <- min_abs_y
        vector_lim_y <- c(-general_max_y, general_max_y)

        limit_lines_y <- c(-1.96, 0, 1.96)

        # X axis
        min_x <- min(minibase_mod$fitted.values)
        max_x <- max(minibase_mod$fitted.values)

        # Generamos un espacio grafico doble para poder colocar las referencias.
        par(mfrow = c(1, 2))

        # En la primera porcion, graficamos los puntos con color
        plot(x = minibase_mod$fitted.values, y = minibase_mod$rstandard,
             col = "red",
             ylim = vector_lim_y,
             xlab = "Fitted values",
             ylab = "Studentized residuals",
             main = set_main04, type = "p", pch = 19, axes = FALSE)

        axis(2, at = seq(-general_max_y, general_max_y, by = 1))

        abline(h = limit_lines_y, col = "red", lty = 2)

        # Ploteamos la 2da parte del grafico para agregar las referencias
        plot(x = 0:10, y = 0:10, type = "n",
             xaxt = "n", yaxt = "n",
             xlab = "", ylab = "",
             xlim = c(0, 10), ylim = c(0, 10),
             col = "white", bty = "n")

        text(5, 5, "Algo le agregamos")
        # # Agregamos la leyenda
        # legend("center", inset = c(-0.4, 0.3),
        #        legend = "Algo le agregamos",
        #        col = df_factor$color, pch = 16,
        #        title = df_selected_vars$doble_reference[2])

        # Se vuelven los parametros graficos a los valores originales
        par(mfrow = c(1, 1))

      })

      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


      # # # # Tab 05 - Analysis...
      output$analysis_slr <- renderPrint({

        mi_lista <- results_all_slr()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("table_slr", "df_determination_coef", "df_line")
        # nombres_a_ver <- c("df_selected_vars", "df_factor", "dt_unbalanced_reps", "lm_ancova_with",
        #                    "test_normality_residuals", "test_homogeneity_residuals",
        #                    "sum_residuos", "table_ancova_with",
        #                    "df_resumen_ancova_with_large", "df_resumen_ancova_with_short",
        #                    "df_tukey", "tukey01_full_groups", "tukey02_full_pairs")

        # Usar lapply para mostrar los elementos deseados
        elementos_a_ver <- lapply(nombres_a_ver, function(nombre) mi_lista[[nombre]])
        names(elementos_a_ver) <- nombres_a_ver
        elementos_a_ver

      })


      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # Tab 06 - minibase_mod...


      output$minibase_mod_slr <- renderPrint({

        results_all_slr()$minibase_mod
      })



      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


      # # # # Tab 07 - graphs...




      output$graph01_slr <- renderPlot({

        minibase_mod <- results_all_slr()$minibase_mod
        df_selected_vars <- results_all_slr()$df_selected_vars
        df_graph01_segment <-  results_all_slr()$df_graph01_segment

        #df_factor <- results_all_slr()$df_factor


        set_main04 <- paste0("Simple Linear Regression", "\n",
                             "Gráfico 01 - Puntos", "\n")

        # Generamos un espacio grafico doble para poder colocar las referencias.
        par(mfrow = c(1, 2))

        # En la primera porcion, graficamos los puntos con color
        plot(x = minibase_mod$X, y = minibase_mod$VR,
             col = "red",
             xlab = df_selected_vars$doble_reference[2],
             ylab = df_selected_vars$doble_reference[1],
             main = set_main04, type = "p", pch = 19)


        segments(x0 = df_graph01_segment$x[1],
                 y0 = df_graph01_segment$y[1],
                 x1 = df_graph01_segment$x[2],
                 y1 = df_graph01_segment$y[2])


        # Ploteamos la 2da parte del grafico para agregar las referencias
        plot(x = 0:10, y = 0:10, type = "n",
             xaxt = "n", yaxt = "n",
             xlab = "", ylab = "",
             xlim = c(0, 10), ylim = c(0, 10),
             col = "white", bty = "n")

        text(5, 5, "Algo le agregamos")
        # # Agregamos la leyenda
        # legend("center", inset = c(-0.4, 0.3),
        #        legend = "Algo le agregamos",
        #        col = df_factor$color, pch = 16,
        #        title = df_selected_vars$doble_reference[2])

        # Se vuelven los parametros graficos a los valores originales
        par(mfrow = c(1, 1))

      })




      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


      # # # # Tab 08 - code...
      code_slr <- reactive({
        req(input$file_source)

        #
        list_code <- list()

        list_code[[1]] <- slr_general_section01_to_03(file_source = input$file_source,
                                                      alpha_value = "0.05", selected_path = NULL,
                                                      selected_pos_vars = selected_pos_vars(),
                                                      name_database = input$file_example,
                                                      all_colnames = all_colnames())


        codigo_fuente <- capture.output(slr_full_gen01)
        codigo_fuente <- codigo_fuente[-1]
        codigo_fuente <- codigo_fuente[-length(codigo_fuente)]
        codigo_fuente <- grep("hide_", codigo_fuente, value = TRUE, invert = TRUE)
        codigo_fuente <- paste0(codigo_fuente , collapse = "\n")

        list_code[[2]] <-codigo_fuente

        the_code <- unlist(list_code)
        the_code <- paste0(the_code, collapse = "\n\n\n")
        the_code
      })

      output$code_slr <- renderText({


        code_slr()


      })




      #########################################################



    }
  )
}

