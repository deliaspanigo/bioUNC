
# # # Module 01 - ANOVA
# UI and SERVER for all modules respect to ANOVA



module02_anova_s01_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("vars_selection"))
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


      # # Info about source database
      intro_source_database <- reactive({
        req(input_general())

        input_general()$intro_source_database
      })


      # # # Control user 01
      control_user_01 <- reactive({


        validate(
          need(!is.null(input_general), "Error 01: Module anova s01 - input_general can not be NULL."),
          need(!is.null(vector_var_names_database()), "Error 10: Module anova s01 - vector_var_names_database() can not be NULL."),
          need(!is.null(intro_source_database()), "Error 11: Module anova s01 - intro_source_database() can not be NULL.")
        )


        return(TRUE)
      })



      # # # Initial values
      action_button_load <- reactiveVal(FALSE)
      action_button_show <- reactiveVal(FALSE)

      # # Colours for actions buttons
      hardcorded_initial_color <- "orange" #"#F4A020"
      color_button_load <- reactiveVal(hardcorded_initial_color)
      color_button_show <- reactiveVal(hardcorded_initial_color)



      # # # Action buttons
      output$action_buttons <- renderUI({

        ns <- shiny::NS(id)


        # # # Style for load button
        standard_style_button_load <- "color: white; background-color: _color_;width: 150px; height: 50px; font-size: 20px;"
        output_style_button_load <- gsub(pattern = "_color_",
                                         replacement = color_button_load(),
                                         x = standard_style_button_load)

        # # # Style for reset button
        output_style_button_reset <- "color: white; background-color: #F4A020;width: 150px; height: 50px; font-size: 20px;"

        # # # UI content
        div(
          fluidRow(
            actionButton(ns("action_load"), label = "LOAD", style = output_style_button_load),
            actionButton(ns("action_reset_all"), "RESET ALL", style = output_style_button_reset)
          )
        )
      })



      # # # Intro source database
      output$intro_source_database <- renderTable({



        df_output <- as.data.frame(intro_source_database())
        colnames(df_output) <- names(intro_source_database())
        df_output



      })



      # # # Var selection for anova 1 way
      output$vars_selection <- renderUI({

        ns <- shiny::NS(id)



        set_options <- setup_var_info(all_var_names = vector_var_names_database())
        set_options <- c("Var selection..." = "", set_options)




        div(shinyjs::useShinyjs(), id = ns("input-var-selection"),
            fluidRow(
              column(2, h1("ANOVA 1 Way"))
              ),
            fluidRow(
              column(6,
                    tableOutput(ns("intro_source_database")))
              ),
            br(), br(), br(),


            fluidRow(
              column(2,
                selectInput(inputId = ns("vr_var_name"), label = "Response Variable",
                            choices = set_options ,
                            selected = set_options[1])
            ),
              column(2,
                selectInput(inputId = ns("factor_var_name"), label = "Factor",
                            choices = set_options,
                            selected = set_options[1])
            ),

            column(2,
                selectInput(inputId = ns("alpha_value"), label = "Alpha value",
                            choices = c(0.10, 0.05, 0.01),
                            selected = 0.05)
                   ),
            column(4, br(), br(), uiOutput(ns("action_buttons"))
                   )
            ),
          br(),
          textOutput(ns("calling_help"))
          )



      })



      # # # Control user 02
      control_user_02 <- reactive({

        req(control_user_01())

      validate(
        need(!is.null(input$vr_var_name), "Error 09: Module anova s01 - input$vr_var_name can not be NULL."),
        need(!is.null(input$factor_var_name), "Error 10: Module anova s01 - input$factor_var_name can not be NULL."),
        need(!is.null(input$alpha_value), "Error 11: Module anova s01 - input$alpha_value can not be NULL.")
      )

      validate(
        need(is.vector(input$vr_var_name), "Error 12: Module anova s01 - input$vr_var_name must be a vector."),
        need(is.vector(input$factor_var_name), "Error 13: Module anova s01 - input$vr_var_name must be a vector."),
        need(is.vector(input$alpha_value), "Error 14: Module anova s01 - input$alpha_value must be a vector.")
      )

      validate(
        need(length(input$vr_var_name) == 1, "Error 15: Module anova s01 - input$vr_var_name has length 1."),
        need(length(input$factor_var_name) == 1, "Error 16: Module anova s01 - input$factor_var_name has length 1."),
        need(length(input$alpha_value) == 1, "Error 17: Module anova s01 - input$alpha_value has length 1.")
      )


      validate(
        need(input$vr_var_name != "", "Select a response variable."),
        need(input$factor_var_name != "", "Select a factor."),
      )

      validate(
        need(input$vr_var_name != input$factor_var_name, "Selected variables can not be equal.")
      )

      return(TRUE)
    })


       # # # General control user
      control_user_99 <- reactive({

        req(control_user_02())
        control_user_02()

      })


# #
  # Reset All --------------------------------------------------------------
  observeEvent(input$action_reset_all, {

    shinyjs::reset("input-var-selection")
    color_button_load(hardcorded_initial_color)
    color_button_show(hardcorded_initial_color)
    action_button_load(FALSE)
    action_button_show(FALSE)


  })


  # # # Reset if change file source or any options
  # # VR selection
  observeEvent(input$vr_var_name, {

    # Not show yet
    color_button_load(hardcorded_initial_color)
    color_button_show(hardcorded_initial_color)
    action_button_load(FALSE)
    action_button_show(FALSE)

  })


  # # Factor selection
  observeEvent(input$factor_var_name, {

    # Not show yet
    color_button_load(hardcorded_initial_color)
    color_button_show(hardcorded_initial_color)
    action_button_load(FALSE)
    action_button_show(FALSE)

  })



  observeEvent(input$action_load, {


    req(control_user_99())
    action_button_load(TRUE)
    color_button_load("green")
  })



  observeEvent(input$action_load, {

    req(!action_button_load())

    color_button_load("red")
    action_button_load(FALSE)

  })




  observeEvent(input$action_load, {

    req(action_button_load(), control_user_99())
    action_button_show(TRUE)

  })



  # # # Final objects
  vr_var_name <- reactive({

    req(action_button_show())

     output_value <- input$vr_var_name
    return(output_value)
  })


  factor_var_name <- reactive({
    req(action_button_show())

    output_value <- input$factor_var_name
    return(output_value)
  })


  alpha_value <- reactive({
    req(action_button_show())
    output_value <- as.numeric(as.character(input$alpha_value))
    output_value
    })


  output$calling_help <- renderText({

    req(control_user_99())
    ""

  })


  # # # Output list
  output_list <- reactive({

    req(action_button_show())


    the_list <- list(vr_var_name(), factor_var_name(), alpha_value())
    names(the_list) <- c("vr_var_name", "factor_var_name", "alpha_value")
    the_list
  })


  return(output_list)
    })
}






module02_anova_s02_rscience_ui <- function(id){

  ns <- shiny::NS(id)

  div(
  textOutput(ns("calling_help")),

  shiny::tabsetPanel(id = ns("super_tabset_panel"),
                     tabPanel("Analysis",  # 05
                              fluidRow(
                                column(12,
                                       h2("Anova 1 way"),
                                       verbatimTextOutput(ns("tab03_analysis_anova"))
                                )
                              )
                     ),
                     tabPanel("Requeriments",  # 05
                              fluidRow(
                                column(12,
                                       h2("Anova 1 way"),
                                       verbatimTextOutput(ns("tab02_requeriments"))
                                )
                              )
                     ),
                     tabPanel("Plots",  # 05
                              fluidRow(
                                column(6,
                                       h2("Anova 1 way"),
                                       #plotOutput(ns("tab04_plots")),
                                       br(),
                                       uiOutput(ns("tab_04_all_plotly")),
                                )
                              )
                     ),
                     tabPanel("Full Results",  # 05
                              fluidRow(
                                column(12,
                                       h2("Anova 1 way"),
                                       verbatimTextOutput(ns("tab01_all_anova_results"))
                                )
                              )
                     ),



                     tabPanel("R code",  # 05
                              fluidRow(
                                column(12,
                                       h2("Anova 1 way"),
                                       verbatimTextOutput(ns("tab05_code"))
                                )
                              )
                     )

  )
  )

}




module02_anova_s02_rscience_server <- function(id, input_general, input_01_anova){
  moduleServer(
    id,
    function(input, output, session) {


      # # # Control for initial inputs
      control_user_01 <- reactive({

        validate(
          need(!is.null(input_general()), "Error 01: Module anova s02 - input_general can not be NULL."),
          need(!is.null(input_01_anova()), "Error 02: Module anova s02 - iinput_01_anova can not be NULL.")
        )

        return(TRUE)
      })


      # # # All anova results
      results_01_anova <- reactive({

        req(control_user_01())

        the_output <- anova_full_gen01(database = input_general()$database,
                                       name_var_vr = input_01_anova()$vr_var_name,
                                       name_var_factor = input_01_anova()$factor_var_name,
                                       alpha_value = input_01_anova()$alpha_value)




        the_output

      })


      # # # Control for anova results object
      control_user_02 <- reactive({

        req(control_user_01())

        validate(
          need(!is.null(results_01_anova()), "Error 03: Module anova s02 - results_01_anova() can not be NULL.")        )

        return(TRUE)

      })


      # # # Tab01 - Anova Results
      output$tab01_all_anova_results <- renderPrint({

        req(control_user_02())

        results_01_anova()
      })

      ##########################################################################

      # # #Tab02 - Residuals Requeriments
      output$tab02_requeriments <- renderPrint({

        req(control_user_02())


        mi_lista <- results_01_anova()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("test_residuals_normality",
                           "test_residuals_homogeneity",
                           "df_residuals_variance_levels")

        # Usar lapply para mostrar los elementos deseados
        elementos_a_ver <- lapply(nombres_a_ver, function(nombre) mi_lista[[nombre]])
        names(elementos_a_ver) <- nombres_a_ver
        elementos_a_ver

      })

      ##########################################################################

      # # # Tab 05 - Analysis resume...
      output$tab03_analysis_anova <- renderPrint({

        req(control_user_02())

        mi_lista <- results_01_anova()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("df_selected_vars",
                           "df_table_anova", "df_factor_info", "check_unbalanced_reps",
                           "df_tukey_original_table",
                           "df_tukey_new_table")
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

      ##########################################################################

      # # # # Tab04 - Graphics
      # output$tab04_plots <- renderPlot({
      #
      #   req(control_user_02())
      #
      #   boxplot(results_01_anova()$minibase[,1] ~ results_01_anova()$minibase[,2],
      #           col = "red")
      # })



      # Boxplot
      output$plot001 <- renderPlotly({

        req(control_user_02())
        #plot_ly(x = 1:10, y = 1:10, type = "scatter", mode = "markers")

        minibase_mod <- results_01_anova()$minibase_mod
        df_factor_info <- results_01_anova()$df_factor_info


        # Crear puntos para cada nivel del factor
        puntos <- plot_ly(data = minibase_mod,
                          x = ~FACTOR, y = ~VR, color = ~FACTOR,
                          type = "scatter", mode = "markers",
                          colors = df_factor_info$color,
                          marker = list(size = 15, opacity = 0.7))


          puntos <- puntos %>%

        layout(title = list(
                  text = "Grafico 01 - Puntos (Datos originales)",
                  size = 20),
              font = list(
              size = 20  # Ajusta el tamaño de letra deseado
          ),
          margin = list(t = 100))


        # Mostrar el gráfico de puntos

        puntos <- puntos %>%
          layout(yaxis = list(zeroline = FALSE))

        puntos

      })


      # Boxplot
      output$plot002 <- renderPlotly({

        req(control_user_02())
        #plot_ly(x = 1:10, y = 1:10, type = "scatter", mode = "markers")

        #minibase_mod <- results_01_anova()$minibase_mod
        #df_factor_info <- results_01_anova()$df_factor_info
        df_table_graph01 <- results_01_anova()$df_table_graph01

        base_mod <- cbind.data.frame(rep(df_table_graph01$level, nrow(df_table_graph01)),
                                     c(df_table_graph01$mean, df_table_graph01$inferior_limit,
                                       df_table_graph01$superior_limit))
        colnames(base_mod) <- c("FACTOR", "VR")


        # Crear puntos para cada nivel del factor
        puntos <- plot_ly(data = base_mod,
                          x = ~FACTOR, y = ~VR, color = ~FACTOR,
                          type = "scatter", mode = "markers",
                          colors = df_factor_info$color,
                          marker = list(size = 15, opacity = 0.7))%>%

          layout(title = list(
                    text = "Grafico 02 - Media y error standard armado con la varianza
                 residual del modelo.",
                    size = 20),
                 font = list(
                   size = 20  # Ajusta el tamaño de letra deseado
                 ),
                 margin = list(t = 100))


        # Mostrar el gráfico de puntos

        puntos <- puntos %>%
          layout(yaxis = list(zeroline = FALSE))

        puntos

      })





      # Boxplot
      output$plot003 <- renderPlotly({

        req(control_user_02())
        #plot_ly(x = 1:10, y = 1:10, type = "scatter", mode = "markers")

        minibase_mod <- results_01_anova()$minibase_mod
        df_factor_info <- results_01_anova()$df_factor_info


        boxplot_plot <- plot_ly(data = minibase_mod,
                                x = ~FACTOR, y = ~VR, color = ~FACTOR,
                                type = "box", boxpoints = FALSE, boxmean = FALSE,
                                colors = df_factor_info$color)


          boxplot_plot  <- boxplot_plot %>%
          layout(title = "Grafico 03 - Boxplot",
                 font = list(
                   size = 20  # Ajusta el tamaño de letra deseado
                 ),
                 margin = list(t = 100))


        boxplot_plot <- boxplot_plot %>%
          layout(yaxis = list(zeroline = FALSE))

        # Mostrar el boxplot
        boxplot_plot




      })



      # Boxplot con medias
      output$plot004 <- renderPlotly({

        req(control_user_02())
        #plot_ly(x = 1:10, y = 1:10, type = "scatter", mode = "markers")

        minibase_mod <- results_01_anova()$minibase_mod
        df_factor_info <- results_01_anova()$df_factor_info


        boxplot_plot <- plot_ly(data = minibase_mod,
                                x = ~FACTOR, y = ~VR, color = ~FACTOR,
                                type = "box", boxpoints = FALSE, boxmean = TRUE,
                                colors = df_factor_info$color)


        boxplot_plot  <- boxplot_plot %>%
          layout(title = "Grafico 04 - Boxplot con medias",
                 font = list(
                   size = 20  # Ajusta el tamaño de letra deseado
                 ),
                 margin = list(t = 100))

        boxplot_plot <- boxplot_plot %>%
          layout(yaxis = list(zeroline = FALSE))

        # Mostrar el boxplot
        boxplot_plot




      })


      # Boxplot
      output$plot005 <- renderPlotly({

        req(control_user_02())

        minibase_mod <- results_01_anova()$minibase_mod
        df_factor_info <- results_01_anova()$df_factor_info


        fig <- plot_ly(data = minibase_mod,
                       x = ~FACTOR, y = ~VR, color = ~FACTOR,
                       type = "violin",
                       points = "all",
                       box = list(visible = T),
                       meanline = list(visible = T)
        )

        fig  <- fig %>%
          layout(title = "Grafico 05 - Violinplot",
                 font = list(
                   size = 20  # Ajusta el tamaño de letra deseado
                 ),
                 margin = list(t = 100))



        fig <- fig %>%
          layout(
            yaxis = list(title = "", zeroline = F)
          )

        fig





      })

      output$tab_04_all_plotly <- renderUI({

        ns <- shiny::NS(id)

        div(
          plotlyOutput(ns("plot001")),
          br(),
          br(),
          plotlyOutput(ns("plot002")),
          br(),
          br(),
          plotlyOutput(ns("plot003")),
          br(),
          br(),
          plotlyOutput(ns("plot004")),
          br(),
          br(),
          plotlyOutput(ns("plot005"))
        )

      })
      ##########################################################################

      # # # Tab 05 - Analysis...
      output$tab05_code <- renderText({

        req(control_user_02())


        the_code <- list()

        the_code[[1]] <- "
        # # # Librerias
        library('openxlsx')



        "
        the_code[[2]] <- showme_your_code(selected_fn = anova_full_gen01)

        the_code <- unlist(the_code)
        the_code
      })

      ##########################################################################


      # # # Calling help
      output$calling_help <- renderText({

        req(control_user_01(), control_user_02())
        ""

      })

    }
  )
}

