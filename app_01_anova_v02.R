

rm(list = ls())
source("global.R")


module02_anova_s02_rscience_ui <- function(id){

  ns <- shiny::NS(id)



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
                                column(12,
                                       h2("Anova 1 way"),
                                       plotOutput(ns("tab04_plots"))
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

}


module02_anova_s02_rscience_server <- function(id, input_general, input_01_anova){
  moduleServer(
    id,
    function(input, output, session) {

      # Tab 01 - All anova results
      results_01_anova <- reactive({


        the_output <- anova_full_gen01(database = input_general()$database, #,
                                       name_var_vr = input_01_anova()$var_name_vr, #,
                                       name_var_factor = input_01_anova()$var_name_factor, #,
                                       alpha_value = input_01_anova()$alpha_value) #)




        the_output

      })


      output$tab01_all_anova_results <- renderPrint({

        results_01_anova()
      })

      ################################################################


      # Tab02 - Residuals Requeriments

      output$tab02_requeriments <- renderPrint({

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

      ###############################################################

      # # # # Tab 05 - Analysis...
      output$tab03_analysis_anova <- renderPrint({

        mi_lista <- results_01_anova()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("df_selected_vars",
                           "table_anova", "df_factor_info", "check_unbalanced_reps",
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

      ###############################################################

      # Tab04 - Graficos

      output$tab04_plots <- renderPlot({

        boxplot(results_01_anova()$minibase[,1] ~ results_01_anova()$minibase[,2],
                col = "red")
      })

      ###############################################################


      # # # # Tab 05 - Analysis...
      output$tab05_code <- renderText({

        the_code <- list()

        the_code[[1]] <- "
        # # # Librerias
        library('openxlsx')



        "
        the_code[[2]] <- showme_your_code(selected_fn = anova_full_gen01)

        the_code <- unlist(the_code)
        the_code
      })

    }
  )
}



ui <- dashboardPage(

  # # # Dashboard title
  dashboardHeader(title = "R-Science"),

  # # # Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "database", tabName = "tab01_database", icon = icon("th")),
      menuItem(text = "Anova 1 way", tabName = "tab02_anova", icon = icon("th"))
    )
  ),

  # # # Body content
  dashboardBody(

    # # # Tabpanel style
    tags$style(
      "li a {
        font-size: 15px;
        font-weight: bold;
      }
    "
    ),


    # # # verbatimTextOutput style
    tags$style(
      "pre {
        color:black;
        background: white;
        font-size: 20px;
        font-weight: bold;
      }
    "
    ),


    # # # Tab items
    tabItems(
      # 1) Data base selection
      tabItem(tabName = "tab01_database",
              module01_database_s01_rscience_ui(id = space_database)),

      # 2) ANOVA
      tabItem(tabName = "tab02_anova",
              module02_anova_s01_varselection_ui(id = "anova01"),
              module02_anova_s02_rscience_ui(id = "anova02")
      )
    )
  )

)







server <- function(input, output, session) {

  # # # Initial inputs
  # - all_var_names()
  # - database
  input_general <- module01_database_s01_rscience_server(id = space_database)


  # # # Vars selection for anova
  # - "var_name_vr", "var_name_factor", "alpha_value"
  input_01_anova <- module02_anova_s01_varselection_server(id = "anova01",
                                     all_var_names = reactive(input_general()$all_var_names))



  module02_anova_s02_rscience_server(id = "anova02",
                                     input_general = input_general,
                                     input_01_anova = input_01_anova)




  # module_ancova_rscience_server(id = space_ancova)
  #
  # module_dummy_rscience_server(id = space_dummy)
  #
  # module_slr_rscience_server(id = space_slr)

}


shinyApp(ui, server)
