# Space ANOVA
vector_show_vars_anova <- c("VR" = T, "FACTOR" = T, "id_database" = T, "id_minibase" = T)
vector01_anova <- c("Seleccionar fuente" = "","R-science examples" = "example", "xlsx" = "xlsx")
vector02_anova <- c("Seleccionar base" = "", "mtcars_mod", "iris_mod", "mtcars", "iris")

module02_anova_s01_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("vars_selection2"))
    )
}




module02_anova_s01_varselection_server <- function(id, input_general){
  moduleServer(
    id,
    function(input, output, session) {

      # # # Very importan objects from input_general
      # # Vector var names from database
      vector_var_names_database <- reactive({
        req(input_general())

        input_general()$vector_var_names_database
      })

      # Info
      intro_source_database <- reactive({
        req(input_general())

        input_general()$intro_source_database
      })



      # Initial values
      initial_color <- "#F4A020"
      color_control_var <- reactiveVal(initial_color)
      action_show_vars <- reactiveVal(FALSE)




  output$action_buttons <- renderUI({

        ns <- shiny::NS(id)

#F4A020
#    AAAF50
        vector_details <- "color: white; background-color: _color_;width: 150px; height: 50px; font-size: 20px;"
        the_color <- color_control_var()
        #if(!is.null(color_control_previous())) the_color <- color_control_previous()

        vector_details <- gsub(pattern = "_color_", replacement = the_color, x = vector_details)
        div(shinyjs::useShinyjs(), id = ns("input-action_buttons"),
          fluidRow(
            column(2, actionButton(ns("do"), label = "LOAD", style = vector_details)),
            column(8),
            column(2, actionButton(ns("reset_all"), "RESET ALL", style = "color: white; background-color: #F4A020;width: 150px; height: 50px; font-size: 20px;"))
          )
        )
      })



  output$intro_source_database <- renderText({

    #req(intro_source_database())


    contenido_texto <- paste(intro_source_database(), collapse = "<br>")
    contenido_texto


  })




  output$vars_selection2 <- renderUI({

    ns <- shiny::NS(id)

    if(is.null(vector_var_names_database())) return(NULL)

    set_options <- setup_var_info(all_var_names = vector_var_names_database())
    set_options <- c("Seleccione una..." = "", set_options)


    div(shinyjs::useShinyjs(), id = ns("input-var-selection"),
        h1("ANOVA 1 Way"),
        fluidRow( column(12,
                         h2("Database details"),
                         tags$div(
                           style = "font-size: 30px;",
                           htmlOutput(ns("intro_source_database"))))),
        br(), br(), br(),

      fluidRow(
        column(2,
        selectInput(inputId = ns("var_vr"), label = "Variable Respuesta",
                    choices = set_options ,
                    selected = set_options[1]),
        selectInput(inputId = ns("var_factor"), label = "Factor",
                    choices = set_options,
                    selected = set_options[1])
        ),
        column(2,  selectInput(inputId = ns("alpha_value"), label = "Alpha",
                               choices = c(0.10, 0.05, 0.01),
                               selected = 0.05)
               ),
        column(6, uiOutput(ns("action_buttons"))
               )

    )
    )
  })


  control_var_selection <- reactive({
    if(is.null(input$var_vr)) return(FALSE) else
      if(is.null(input$var_factor)) return(FALSE) else
        if(input$var_vr == "") return(FALSE) else
          if(input$var_factor == "") return(FALSE) else
            if(input$var_vr == input$var_factor) return(FALSE) else return(TRUE)
  })


  # Activate load and show -------------------------------------------------
  observeEvent(input$do, {

    #req(control01_previous_database())

    if(control_var_selection()) action_show_vars(TRUE) else action_show_vars(FALSE)


  })


  observeEvent(input$do, {

    if(is.null(control_var_selection())) color_control_var(initial_color) else
      if(control_var_selection()) color_control_var("green") else
        if(!control_var_selection()) color_control_var("red")


  })


  # Reset All --------------------------------------------------------------
  observeEvent(input$reset_all, {

    shinyjs::reset("input-var-selection")
    color_control_var(initial_color)
    action_show_vars(FALSE)
    #database(NULL)

    #digits_value(NULL)
    #control_xlsx(FALSE)
    #control_example(FALSE)


  })


  # Reset if change file source or any options
  # from input-panel -------------------------------------------------------
  observeEvent(input$var_vr, {

    # Not show yet
    color_control_var(initial_color)
    action_show_vars(FALSE)

})

  observeEvent(input$var_factor, {

    # Not show yet
    color_control_var(initial_color)
    action_show_vars(FALSE)

  })


  var_name_vr <- reactive({
     output_value <- as.numeric(as.character(input$var_vr))
    return(output_value)
  })

  var_name_factor <- reactive({


    output_value <- as.numeric(as.character(input$var_factor))
    return(output_value)
  })


  alpha_value <- reactive({ as.numeric(as.character(input$alpha_value))})

  output_list <- reactive({

    req(action_show_vars())

    if(!action_show_vars()) return(NULL)

    the_list <- list(var_name_vr(), var_name_factor(), alpha_value())
    names(the_list) <- c("var_name_vr", "var_name_factor", "alpha_value")
    the_list
  })


  return(output_list)
    })
}


module02_anova_02_rscience_ui <- function(id){
  ns <- shiny::NS(id)

  new_name <- "Requeriments"

  armado <- "
    .tabbable > .nav > li > a                  {background-color: aqua;  color:black}
    .tabbable > .nav > li > a[data-value='_new_name_'] {background-color: red;   color:white}

      }
  "

  armado <- gsub(pattern = "_new_name_", replacement = new_name, x = armado)

  div(
    #tags$head(
    #tags$style(HTML(armado))),
    h2("Rscience - ANOVA 1 Way"),
    uiOutput(ns("anova_full_tabsetpanel"))

  ) # End div
}


module02_anova_02_rscience_server <- function(id, input_general, input_01_anova){
  moduleServer(
    id,
    function(input, output, session) {

  # Input general
      database <- reactive(input_general()$database)
      all_var_names <- reactive(input_general()$vector_var_names_database)

  # Input 01 - Anova
    var_name_vr <- reactive(input_01_anova()$var_name_vr)
    var_name_factor <- reactive(input_01_anova()$var_name_factor)
    alpha_value <- reactive(input_01_anova()$alpha_value)



      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # RESULTS
      # Results - Ancova with
      results_all_anova <- reactive({

        req(control02_vars_selection())
        the_output <- anova_full_gen01(database = database(),
                                       name_var_vr = name_var_vr(),
                                       name_var_factor = name_var_factor,
                                       alpha_value = alpha_value())




        the_output

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

        req(control01_database(), var_name_vr(), var_name_factor())

        if(is.null(var_name_vr())) return(FALSE) else
          if(is.null(var_name_factor())) return(FALSE) else
            if(var_name_vr() == "") return(FALSE) else
              if(var_name_factor() == "") return(FALSE) else return(TRUE)

      })





      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


      # # # # TABSETPANEL FULL - ANOVA
      output$anova_full_tabsetpanel <- renderUI({
        ns <- shiny::NS(id)

        req(control01_database(), control02_vars_selection())

        div(
        radioButtons(inputId = ns("show_results"), label = "Show results",
                     choices = c("Short" = "short", "Large" = "large"), selected = "large"),
        shiny::tabsetPanel(id = ns("super_tabset_panel"),
           tabPanel("Analysis",  # 05
                   fluidRow(
                     column(12,
                            h2("Anova 1 way"),
                            verbatimTextOutput(ns("analysis_anova")))
                   )

          ),
          tabPanel("Requeriments", # 04
                   fluidRow(
                     column(12,
                            h2("Anova 1 way"),
                            verbatimTextOutput(ns("requeriments_anova")),
                     )
                   )
          ),
          tabPanel("graphs", #07
                   fluidRow(
                     column(12,
                            h2("Anova 1 way"),
                            plotOutput(ns("graph01_anova")))
                   )
          ),
          tabPanel("Modelo",  # 05
                   fluidRow(
                     column(12,
                            h2("Anova 1 way"),
                            verbatimTextOutput(ns("modelo_anova")))
                   )
          ),
          tabPanel("Download")








        )

      )

      })


      observeEvent(input$show_results, {

        ns <- shiny::NS(id)

        #ns <- shiny::NS(id)
        if(input$show_results == "large"){
           insertTab(inputId = "super_tabset_panel",
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

                     ), target = "Download")

          insertTab(inputId = "super_tabset_panel",
                    tabPanel("R code",  #08
                             fluidRow(
                               column(12,
                                      h2("Anova 1 way"),
                                      verbatimTextOutput(ns("code_anova")))
                             )

                    ), target = "Download")

          insertTab(inputId = "super_tabset_panel",
                    tabPanel("Full R Results", # 02
                             fluidRow(
                               column(12,
                                      h2("Anova 1 way"),
                                      verbatimTextOutput(ns("results_all_anova")))
                             )
                    ),
                    target = "R code")

          insertTab(inputId = "super_tabset_panel",
                    tabPanel("minibase_mod", #06
                             fluidRow(
                               column(12,
                                      h2("Anova 1 way"),
                                      verbatimTextOutput(ns("minibase_mod_anova")))

                             )

                    ),
                    target = "Full R Results")

          insertTab(inputId = "super_tabset_panel",
                    tabPanel("R-science code"), target = "Full R Results")


          insertTab(inputId = "super_tabset_panel",
                    tabPanel("Control"), target = "R-science code")

          insertTab(inputId = "super_tabset_panel",
                    tabPanel("User info"), target = "Control")


        } else
          if(input$show_results == "short"){
            removeTab(inputId = "super_tabset_panel",target = "minibase")
            removeTab(inputId = "super_tabset_panel",target = "R code")
            removeTab(inputId = "super_tabset_panel",target = "Full R Results")
            removeTab(inputId = "super_tabset_panel",target = "minibase_mod")
            removeTab(inputId = "super_tabset_panel",target = "R-science code")
            removeTab(inputId = "super_tabset_panel",target = "Control")
            removeTab(inputId = "super_tabset_panel",target = "User info")

          }
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
      output$results_all_anova <- renderPrint({

        results_all_anova()
      })

      output$results_all_anovaout <- renderPrint({

        results_all_anovaout()
      })



      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # Tab 03 - minibase...
      df_selected_vars <- reactive({

        req(results_all_anova())
        results_all_anova()$df_selected_vars

      })


      output$df_selected_vars <- renderTable({
        req(df_selected_vars())
        df_selected_vars()

      }, rownames = FALSE, align = "c")


      output$df_control_minibase <- renderTable({

        req(results_all_anova())
        results_all_anova()$df_control_minibase


      }, rownames = FALSE, align = "c")


      output$df_show_n <- renderTable({

        req(results_all_anova())
        results_all_anova()$df_show_n


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
                             choices = names(vector_show_vars_anova),
                             selected = names(vector_show_vars_anova))
        )
      })


      dt_show_vars <- reactive({



        names(vector_show_vars_anova) %in% unlist(input$show_vars)
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


        var_show_selection <- c("VR", "FACTOR", "id_database", "id_minibase")

        new_names <- df_selected_vars()[[input$show_reference]] # var_role	doble_reference
        #print(aver)
        gen_data <- results_all_anova()$minibase_mod[var_show_selection]

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
      output$requeriments_anova <- renderPrint({

        mi_lista <- results_all_anova()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("test_residuals_normality",
                           "test_residuals_homogeneity",
                           "df_residuals_variance_levels")

        # Usar lapply para mostrar los elementos deseados
        elementos_a_ver <- lapply(nombres_a_ver, function(nombre) mi_lista[[nombre]])
        names(elementos_a_ver) <- nombres_a_ver
        elementos_a_ver

      })



      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


      # # # # Tab 05 - Analysis...
      output$analysis_anova <- renderPrint({

        mi_lista <- results_all_anova()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("df_selected_vars",
                           "table_anova", "df_factor", "dt_unbalanced_reps",
                           "df_tukey_means")
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



      # # # # Tab 05 - Analysis...
      output$modelo_anova <- renderPrint({

        mi_lista <- results_all_anova()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("df_resumen_anova_large",
                           "df_resumen_anova_short")
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


      output$minibase_mod_anova <- renderPrint({

        results_all_anova()$minibase_mod
      })



      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


      # # # # Tab 07 - graphs...



      output$graph01_anova <- renderPlot({

        # # Grafico 04) Puntos y Rectas con color
        #--------------- INICIO GRAFICO 04 ---------------------------------------------

        minibase_mod <- results_all_anova()$minibase_mod
        df_selected_vars <- results_all_anova()$df_selected_vars
        df_factor <- results_all_anova()$df_factor


        set_main04 <- paste0("Anova 1 way", "\n",
                             "Gráfico 04 - Puntos y Rectas con color", "\n")

        # Generamos un espacio grafico doble para poder colocar las referencias.
        par(mfrow = c(1, 2))

        # En la primera porcion, graficamos los puntos con color
        plot(x = minibase_mod$lvl_order_number, y = minibase_mod$VR,
             col = minibase_mod$lvl_color,
             xlab = df_selected_vars$doble_reference[3],
             ylab = df_selected_vars$doble_reference[1],
             main = set_main04, type = "p", pch = 19)



        # Ploteamos la 2da parte del grafico para agregar las referencias
        plot(x = 0:10, y = 0:10, type = "n",
             xaxt = "n", yaxt = "n",
             xlab = "", ylab = "",
             xlim = c(0, 10), ylim = c(0, 10),
             col = "white", bty = "n")

        # Agregamos la leyenda
        legend("center", inset = c(-0.4, 0.3),
               legend = df_factor$level,
               col = df_factor$color, pch = 16,
               title = df_selected_vars$doble_reference[2])

        # Se vuelven los parametros graficos a los valores originales
        par(mfrow = c(1, 1))

        #--------------- FIN GRAFICO 04 ------------------------------------------------


      })






      # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


      # # # # Tab 08 - code...
      code_anova <- reactive({
        #req(input$file_source)

        #"HOAL"
        #
        list_code <- list()

        list_code[[1]] <- anova_general_section01_to_03(file_source = "asdasdasdas.xlsx", #input$file_source,
                                                        alpha_value = "0.05", selected_path = NULL,
                                                        selected_pos_vars = selected_pos_vars(),
                                                        name_database = input$file_example,
                                                        all_colnames = vector_var_names_database())


        codigo_fuente <- capture.output(anova_full_gen01)
        codigo_fuente <- codigo_fuente[-1]
        codigo_fuente <- codigo_fuente[-length(codigo_fuente)]
        codigo_fuente <- grep("hide_", codigo_fuente, value = TRUE, invert = TRUE)
        codigo_fuente <- paste0(codigo_fuente , collapse = "\n")

        list_code[[2]] <-codigo_fuente

        the_code <- unlist(list_code)
        the_code <- paste0(the_code, collapse = "\n\n\n")
        the_code


      })

      output$code_anova <- renderText({


        code_anova()

        #  "HOLA 2"
      })




      #########################################################



    }
  )
}

space_anova <- "space_anova"
