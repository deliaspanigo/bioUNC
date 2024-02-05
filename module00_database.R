# Space database

space_database <- "space_database"
vector01_anova <- c("Source..." = "","R-science examples" = "example", "xlsx" = "xlsx")
vector02_anova <- c("Select..." = "", "mtcars_mod", "iris_mod", "mtcars", "iris")
module_database_rscience_ui <- function(id){
  ns <- shiny::NS(id)


  div(shinyjs::useShinyjs(), id = ns("input-panel"),
    h2("Initial user election - database"),
    fluidRow(
      column(2,
             selectInput(inputId = ns("file_source"), label = "Source for database...",
                         choices = vector01_anova,
                         selected = vector01_anova[1]),

             div(shinyjs::useShinyjs(), id = ns("input-xlsx"),
             conditionalPanel(condition = 'input.file_source == "xlsx"',ns = ns,
                              fileInput(inputId = ns("info_xlsx"),
                                        label = "Cargar archivo Excel"),
                              uiOutput(ns("sheet_selection"))
             )),

             div(shinyjs::useShinyjs(), id = ns("input-example"),
             conditionalPanel(condition = 'input.file_source == "example"', ns = ns,
                              selectInput(inputId = ns("file_example"),
                                          label = "Example",
                                          choices = vector02_anova,
                                          selected = vector02_anova[1])
                              ))
      ),
      column(6, br(), uiOutput(ns("action_buttons")))

    ), # End fluidRow
    fluidRow(

      column(12, tableOutput(ns("df_database")))

      )

  ) # End div
}
module_database_rscience_server <- function(id){
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

        name_sheets <- openxlsx::getSheetNames(input$info_xlsx$datapath)
        set_options <- super_sheet_info(name_sheets = name_sheets)

        set_options <- c("Seleccione una... " = "", set_options)
        pos_selected <- 1

        selectInput(inputId = ns("sheet_xlsx"),
                    label = "Seleccionar hoja:", choices = set_options,
                    selected = set_options[pos_selected])

      })



      output$action_buttons <- renderUI({

        ns <- shiny::NS(id)

        #F4A020
        #AAAF50

        vector_details <- "color: white; background-color: _color_;width: 150px; height: 50px; font-size: 20px;"
        the_color <- color_control_previous()
        #if(!is.null(color_control_previous())) the_color <- color_control_previous()

        vector_details <- gsub(pattern = "_color_", replacement = the_color, x = vector_details)
        div(
          fluidRow(
            column(2, actionButton(ns("do"), label = "LOAD", style = vector_details)),
            column(8),
            column(2, actionButton(ns("reset_all"), "RESET ALL", style = "color: white; background-color: #F4A020;width: 150px; height: 50px; font-size: 20px;"))
          )
        )
      })


      # Initial default values -------------------------------------------------
      database <- reactiveVal(NULL)
      digits_value <- reactiveVal(NULL)

      control_xlsx <- reactiveVal(FALSE)
      control_example <- reactiveVal(FALSE)

      initial_color <- "#F4A020"
      color_control_previous <- reactiveVal(initial_color)

      action_load_database <- reactiveVal(FALSE)
      action_show_database <- reactiveVal(FALSE)


      # Reset All --------------------------------------------------------------
      observeEvent(input$reset_all, {

        shinyjs::reset("input-panel")
        action_load_database(FALSE)
        action_show_database(FALSE)
        color_control_previous(initial_color)
        #database(NULL)

        #digits_value(NULL)
        #control_xlsx(FALSE)
        #control_example(FALSE)


      })



      # Reset if change file source or any options
      # from input-panel -------------------------------------------------------
      observeEvent(input$file_source, {

        # Not show yet
        action_load_database(FALSE)
        action_show_database(FALSE)
        color_control_previous(initial_color)

        # If its selected "xlsx"
        if(input$file_source == "xlsx"){

          # Reset for others inputs
          shinyjs::reset("input-example")

          # If change input-file, reset
          observeEvent(input$info_xlsx, {
            shinyjs::reset("sheet_xlsx")
            action_load_database(FALSE)
            action_show_database(FALSE)
            color_control_previous(initial_color)
            })


          # If change sheet, reset show_database
          observeEvent(input$sheet_xlsx, {
            action_load_database(FALSE)
            action_show_database(FALSE)
            color_control_previous(initial_color)
            })



        } else

        if(input$file_source == "example"){
            shinyjs::reset("input-xlsx")
          shinyjs::reset("sheet_xlsx")
          color_control_previous(initial_color)

          # if user change example...
          observeEvent(input$file_example,{
            action_load_database(FALSE)
            action_show_database(FALSE)
            color_control_previous(initial_color)
          })

        }


      })


      # Previous control -------------------------------------------------------
      control_xlsx <- reactive({

        #req(input$file_source, input$file_example)
        dt_control <- FALSE
        if(is.null(input$file_source)) return(dt_control)
        if(is.null(input$info_xlsx)) return(dt_control)
        if(is.null(input$sheet_xlsx)) return(dt_control)

        vector_items <- c(input$file_source, input$info_xlsx, input$sheet_xlsx)
        dt_vector <- vector_items != ""
        dt_ok <- sum(dt_vector) == length(dt_vector)

        if(dt_ok) dt_control <- TRUE


        return(dt_control)

      })


      control_xlsx <- reactive({

        #req(input$file_source, input$file_example)
        dt_control <- FALSE
        if(is.null(input$file_source)) return(dt_control)
        if(is.null(input$info_xlsx)) return(dt_control)
        if(is.null(input$sheet_xlsx)) return(dt_control)

        vector_items <- c(input$file_source, input$info_xlsx, input$sheet_xlsx)
        dt_vector <- vector_items != ""
        dt_ok <- sum(dt_vector) == length(dt_vector)

        if(dt_ok) dt_control <- TRUE


        return(dt_control)

      })

      control_example <- reactive({

        #req(input$file_source, input$file_example)
        dt_control <- FALSE
        if(is.null(input$file_source)) return(dt_control)
        if(is.null(input$file_example)) return(dt_control)

        vector_items <- c(input$file_source, input$file_example)
        dt_vector <- vector_items != ""
        dt_ok <- sum(dt_vector) == length(dt_vector)

        if(dt_ok) dt_control <- TRUE

        return(dt_control)

      })

      control01_previous_database <- reactive({

        #req(input$file_source)
        if(is.null(input$file_source)) return(NULL)
        if(input$file_source == "xlsx") control_xlsx() else
          if(input$file_source == "example") control_example() else FALSE

      })





      # Activate load and show -------------------------------------------------
      observeEvent(input$do, {

        req(control01_previous_database())

        if(control01_previous_database()){
              action_load_database(TRUE)
              action_show_database(TRUE)
        }

      })


      observeEvent(input$do, {

        if(is.null(control01_previous_database())) color_control_previous(initial_color) else
          if(control01_previous_database()) color_control_previous("green") else
            if(!control01_previous_database()) color_control_previous("red")


      })

      # Import database --------------------------------------------------------
      observeEvent(action_load_database(),{

            if(action_load_database()){
            imported_database <- reactive({

              req(input$file_source)
              req(input$file_source != "")

              if(input$file_source == "xlsx"){

                # req(control_xlsx(), action_load_database())
                req(control_xlsx())
                req(input$info_xlsx, input$info_xlsx$name, input$info_xlsx$datapath, input$sheet_xlsx)

                ext <- tools::file_ext(input$info_xlsx$name)
                #validate(need(ext == "xlsx", "Please upload a csv file"))


                if (grepl(".xlsx$", input$info_xlsx$name)) {
                  # Para archivos XLSX

                  # Internal control to stop no changes in some input
                  ######################################################################
                  name_sheets <- openxlsx::getSheetNames(input$info_xlsx$datapath)
                  selected_sheet <- input$"sheet_xlsx"
                  dt_inside <- sum(name_sheets == selected_sheet) == 1
                  if(!dt_inside) return(NULL)
                  ######################################################################

                  output_df <- openxlsx::read.xlsx(xlsxFile = input$info_xlsx$datapath, sheet = input$"sheet_xlsx")

                  #colnames(output_df) <- make.names(colnames(output_df))
                  return(output_df)
                } else {
                  return(NULL)
                }
              }

              else if (input$file_source == "example"){

                # req(control_example(), action_load_database())
                req(control_example())
                #armado <- "data_example <- input$file_example"
                data_example <- eval(parse(text = input$file_example))

                if(!is.null(data_example)){
                  rownames(data_example) <- c(1:nrow(data_example))
                }

              }

              return(data_example)
            })

            database(imported_database())

            } else database(NULL)
})


      output$df_database <- renderTable({

        req(action_show_database(), database())

        database()
        #mtcars
      }, rownames = TRUE, align = "c")






      # # # # INTERNAL CONTROLS
      # Control 01 - database
      control01_database <- reactive({

        dt_database <- is.null(database())


        if(dt_database) return(FALSE) else return(TRUE)


      })









     # digits_value reactive
     digits_value <- reactive({

       as.numeric(as.character(input$digits_value))

     })


     all_var_names <- reactive({
       #req(database())
       colnames(database())
     })

     output_list <- reactive({

       req(action_show_database())

       if(!action_show_database()) return(NULL)


       the_list <- list(all_var_names(), database())

       names(the_list) <- c("all_var_names", "database")

       return(the_list)
       })



     # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # Tab 01 - database







    return(output_list)



    }
  )
}

