## app.R ##
#
rm(list = ls())
source("global.R")










ui <- dashboardPage(
  dashboardHeader(title = "R-Science"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      # menuItem("Bienvenida", tabName = "welcome", icon = icon("dashboard")),
      menuItem(text = "database", tabName = "database_rscience", icon = icon("th")),
      menuItem(text = "Anova 1 way", tabName = "tab_anova_1_way_rscience", icon = icon("th")),
      menuItem(text = "Ancova", tabName = "tab_ancova_rscience", icon = icon("th")),
      menuItem(text = "Dummy", tabName = "tab_dummy_rscience", icon = icon("th")),
      menuItem(text = "Simple Linear Regression", tabName = "tab_slr_rscience", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    # Formato para todo lo que sean listas, como los tabPanel
    tags$style(
      "li a {
        font-size: 15px;
        font-weight: bold;
      }
    "
    ),
#     tags$head(tags$style(paste0("#", ns("requeriments_anova"), "{color:red; font-size:50px; font-style:italic;
# overflow-y:scroll; background: ghostwhite;}")
    # Formato para los verbatimTextOutput!
    tags$style(
      "pre {
        color:black;
        background: white;
        font-size: 20px;
        font-weight: bold;
      }
    "
    ),


    tabItems(
      # 1) Data base selection
      tabItem(tabName = "database_rscience",
              module_database_rscience_ui(id = space_database)),

      # 2) ANOVA
      tabItem(tabName = "tab_anova_1_way_rscience",
              module_anova_01_varselection_ui(id = "anova01"),
              module_anova_02_rscience_ui(id = "anova02")
      ),

      tabItem(tabName = "tab_anova_2_way_rscience", "Anova bifactorial aquí!"),
      tabItem(tabName = "tab_anova_1f_1b_rscience", "Anova a 1 factor con bloque aquí!"),
      tabItem(tabName = "tab_ancova_rscience",
              module_ancova_rscience_ui(id = space_ancova)
      ),
      tabItem(tabName = "tab_dummy_rscience",
              module_dummy_rscience_ui(id = space_dummy)
      ),

      tabItem(tabName = "tab_slr_rscience",
              module_slr_rscience_ui(id = space_slr)
      ),
      tabItem(tabName = "tab_anova_mlr_rscience", "Regresión Lineal Múltiple!")


    )
  )

)




server <- function(input, output, session) {

  # # # Initial inputs
  # - all_var_names()
  # - database
  initial_inputs <- module_database_rscience_server(id = space_database)


  # # # Vars selection for anova
  # - "var_name_vr", "var_name_factor", "alpha_value"
  selected_vars_anova <- module_anova_01_varselection_server(id = "anova01",
                              all_var_names = reactive(initial_inputs()$all_var_names))



  module_anova_02_rscience_server(id = "anova02", database = reactive(initial_inputs()$database),
                                selected_vars_anova = selected_vars_anova,
                                all_var_names = reactive(initial_inputs()$all_var_names))




  module_ancova_rscience_server(id = space_ancova)

  module_dummy_rscience_server(id = space_dummy)

  module_slr_rscience_server(id = space_slr)

}


shinyApp(ui, server)
