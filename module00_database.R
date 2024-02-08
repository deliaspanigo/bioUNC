# Space database

space_database <- "space_database"


# # # 01) UI - Selection for 'database'
module01_database_s01_rscience_ui <- function(id){
  ns <- shiny::NS(id)


  div(shinyjs::useShinyjs(), id = ns("input-panel"),

      h2("Initial user election - database"),
      fluidRow(
        column(2,

             # # # Select source of information
             selectInput(inputId = ns("file_source"), label = "File source...",
                         choices = c("Select a file source..." = "",
                                     "R-science examples" = "example",
                                     "xlsx" = "xlsx")),

             # # # For xlsx files...
             div(shinyjs::useShinyjs(), id = ns("input-xlsx"),
             conditionalPanel(condition = 'input.file_source == "xlsx"',ns = ns,
                              fileInput(inputId = ns("info_xlsx"),
                                        label = "File '.xlsx' selection..."),
                              uiOutput(ns("sheet_selection"))
             )),

             # # # For example files
             div(shinyjs::useShinyjs(), id = ns("input-example"),
             conditionalPanel(condition = 'input.file_source == "example"', ns = ns,
                              selectInput(inputId = ns("file_example"),
                                          label = "Example",
                                          choices = c("Select..." = "",
                                                      "mtcars_mod",
                                                      "iris_mod",
                                                      "mtcars",
                                                      "iris")
                              ))
        )
        ),
        # # # Action buttons
        column(6, br(), uiOutput(ns("action_buttons")))

        ), # End fluidRow

    # # # Visualization for database
    fluidRow(

      column(12, tableOutput(ns("df_database")))

      )

  ) # End div
}

# # # 01) SERVER - Selection for 'database'
module01_database_s01_rscience_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {


      # # # Initial objects and default values -------------------------------------------------
      # # Action buttons - default values
      action_button_load <- reactiveVal(FALSE)
      action_button_show <- reactiveVal(FALSE)

      # # Colours for actions buttons
      hardcorded_initial_color <- "#F4A020"
      color_button_load <- reactiveVal(hardcorded_initial_color)

      # # Default - 'database' object
      selected_info_xlsx <- reactiveVal(NULL)
      sheets_xlsx <- reactiveVal(NULL)

      # # Default - Control for source of information
      control_xlsx <- reactiveVal(FALSE)
      control_example <- reactiveVal(FALSE)

      database <- reactiveVal(NULL)


      # # # renderUI for action buttons ----------------------------------------
      output$action_buttons <- renderUI({

        ns <- shiny::NS(id)
        req(color_button_load())

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
            column(2, actionButton(ns("action_load"), label = "LOAD", style = output_style_button_load)),
            column(8),
            column(2, actionButton(ns("action_reset_all"), "RESET ALL", style = output_style_button_reset))
          )
        )
      })





      # # # File source --------------------------------------------------------
      # # 1) Control
      control_file_source_99 <- reactive({

        validate(
          # # # File name control
          need(!is.null(input$file_source), 'Error 05: input$file_source problems!.'),
          need(input$file_source != "", 'Select a file source.')
        )

        return(TRUE)

      })

      # # 2) Observ Event for input$file_source
      observeEvent(input$file_source, {

        req(control_file_source_99())

        # # # Reset for action buttons
        action_button_load(FALSE)
        action_button_show(FALSE)
        color_button_load(hardcorded_initial_color)

        selected_info_xlsx(NULL)
        shinyjs::reset("input-xlsx")
        shinyjs::reset("sheet_xlsx")
        # # # Reset database
        database(NULL)

      })




      # # # Excel --------------------------------------------------------------
      # # # Excel control 01 of 04
      # input$file_source is "xlsx"
      control_xlsx_01 <- reactive({

        req(control_file_source_99())

        validate(
          need(input$file_source == 'xlsx', 'Error 03: File source error!.'),
        )

        return(TRUE)

      })



      # # # Excel control 02 of 04
      # input$info_xlsx control
      control_xlsx_02 <- reactive({

        req(control_xlsx_01())

        # # # Initial text
        validate(
          need(!is.null(input$info_xlsx), "Select a '.xlsx' file.")
        )

        # # # File name control
        validate(
          need(!is.null(input$info_xlsx$name), 'Error 05: File name problems. Change the file name.'),
          need(input$info_xlsx$name != '', 'Error 06: File name problems. Change the file name.')
        )

        # # # File path control
        validate(
          need(!is.null(input$info_xlsx$datapath), 'Error 08: problems regarding the path of the xlsx file. Change the file name or folder name.'),
          need(input$info_xlsx$datapath != '', 'Error 09: File path problems. Change the file name or folder names.')
        )

        # # # Specific control for file extension
        validate(
          need(grepl(".xlsx$", input$info_xlsx$name), "Error 07: Wrong file! You must select a '.xlsx'' file."),
        )

        return(TRUE)

      })

      observeEvent(input$info_xlsx, {

        req(control_xlsx_02())
        # # # Reset for action buttons
        action_button_load(FALSE)
        action_button_show(FALSE)
        color_button_load(hardcorded_initial_color)

        selected_info_xlsx(input$info_xlsx)
        shinyjs::reset("sheet_xlsx")
        # # # Reset database
        database(NULL)

      })



      # # # Excel control 03 of 04
      # input$info_xlsx control
      control_xlsx_03 <- reactive({

        req(control_xlsx_02())

        # # # Initial text
        validate(
          need(!is.null(selected_info_xlsx()), "Select a '.xlsx' file.")
        )

        # # # File name control
        validate(
          need(!is.null(selected_info_xlsx()$name), 'Error 05: File name problems. Change the file name.'),
          need(selected_info_xlsx()$name != '', 'Error 06: File name problems. Change the file name.')
        )

        # # # File path control
        validate(
          need(!is.null(selected_info_xlsx()$datapath), 'Error 08: problems regarding the path of the xlsx file. Change the file name or folder name.'),
          need(selected_info_xlsx()$datapath != '', 'Error 09: File path problems. Change the file name or folder names.')
        )

        # # # Specific control for file extension
        validate(
          need(grepl(".xlsx$", selected_info_xlsx()$name), "Error 07: Wrong file! You must select a '.xlsx'' file."),
        )

        return(TRUE)

      })

      observeEvent(selected_info_xlsx(), {

        req(control_xlsx_03())
        database(NULL)
        sheets_xlsx(NULL)

        # # # # Reset for action buttons
        # action_button_load(FALSE)
        # action_button_show(FALSE)
        # color_button_load(hardcorded_initial_color)
        #
        # shinyjs::reset("input-xlsx")
        # shinyjs::reset("sheet_xlsx")
        # # # # Reset database

        name_sheets <- openxlsx::getSheetNames(selected_info_xlsx()$datapath)
        set_options <- super_sheet_info(name_sheets = name_sheets)
        set_options <- c("Select one... " = "", set_options)
        sheets_xlsx(set_options)
      })



      # # # Sheet selection for xlsx file
      output$sheet_selection <- renderUI({

        ns <- shiny::NS(id)

        # Previuos controls
        req(control_xlsx_03())

        pos_selected <- 1

        # # # SelectInput for sheet selection
        selectInput(inputId = ns("sheet_xlsx"),
                    label = "Sheet selector:",
                    choices = sheets_xlsx(),
                    selected = sheets_xlsx()[pos_selected])

      })


      # # # Excel control 04 of 04
      control_xlsx_04 <- reactive({

        req(control_xlsx_03())

        validate(


          need(!is.null(input$sheet_xlsx), 'Error 06: Sheet problems. Change the sheet names.'),
          need(input$sheet_xlsx != '', "Select a sheet from the 'xlsx' file."),



        )

        return(TRUE)

      })

      observeEvent(input$sheet_xlsx, {

        req(control_xlsx_04())



        # # # # Reset for action buttons
        action_button_load(FALSE)
        action_button_show(FALSE)
        color_button_load(hardcorded_initial_color)
        database(NULL)


      })





      # # # Reset actions
      # There are differents reset actions.
      #
      # # 01) Reset All with reset button---------------------------------------
      observeEvent(input$action_reset_all, {

        # # # Reset for div()
        shinyjs::reset("input-panel")
        shinyjs::reset("input-xlsx")
        shinyjs::reset("info_xlsx")


        # # # Reset for action buttons
        action_button_load(FALSE)
        action_button_show(FALSE)
        color_button_load(hardcorded_initial_color)

        # # # Reset internal info_xlsx (path and file name)
        selected_info_xlsx(NULL)
        sheets_xlsx(NULL)
        # # # Reset database
        database(NULL)
      })







      # # # Previous control for database --------------------------------------
      # For each source of information there is a specific control.
      # Each control return only a TRUE or FALSE.
      # Depending on the user's choice, the control_file_source_99 object pipes the corresponding control.
      control_xlsx <- reactive({

        req(control_xlsx_04())

        dt_control <- control_xlsx_04()

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

      #


      # # # Activate 'load' ---------------------------------------------
      # 1) If the load button is pressed...
      # If all the previous control for database are OK we change the state of
      # the action "load" to TRUE.
      # For xlsx files we take the info_xlsx information
      # internally to try import the selected xlsx file.
      observeEvent(input$action_load, {


        req(input$action_load, control_file_source_99())


        if(control_file_source_99()){
             action_button_load(TRUE)


              if(input$file_source == "xlsx"){
                #selected_info_xlsx(input$info_xlsx)
              }
        }

      })


      # Import database --------------------------------------------------------
      observeEvent(action_button_load(),{

        if(action_button_load()){


          if(input$file_source == "xlsx"){


            req(control_xlsx())


            # # Internal control to stop no changes in some input
            # ######################################################################
            # name_sheets <- openxlsx::getSheetNames(input$info_xlsx$datapath)
            # selected_sheet <- input$"sheet_xlsx"
            # dt_inside <- sum(name_sheets == selected_sheet) == 1
            # if(!dt_inside) return(NULL)
            # ######################################################################

            database(
              read.xlsx(xlsxFile = selected_info_xlsx()$datapath,
                        sheet = input$"sheet_xlsx")
            )


          } else



            if (input$file_source == "example"){


              req(control_example())

              database(
                eval(parse(text = input$file_example))
              )



            }


        }
      })


      # # # database post control
      control02_post_database <- reactive({

        req(action_button_load(), control_file_source_99())

        validate(
          need(!is.null(database()), "Error Database 01: 'database' is a NULL object."),
          need(is.data.frame(database()), "Error Database 02: The 'database' object must be a dataframe."),
          need(ncol(database()) > 0, "Error Database 03: 'database' must have at least one column."),
          need(ncol(database()) > 0, "Error Database 04: 'database' must have at least one row."),


        )


        return(TRUE)

      })


      # # # Activate 'show'
      # If database has beed imported succesfully, and all the controls
      # post upload are OK, we put the status for 'show' as TRUE.
      observeEvent(control02_post_database(), {

        req(control02_post_database())
        action_button_show(TRUE)

      })

      # 2) We also activate a color change on the buttons.
      # If the previous controls are met, it will be GREEN.
      # # If the prior control is not met, it will be red.
      # observeEvent(control_file_source_99(), {
      #
      #
      #
      #   if(is.null(control_file_source_99())){
      #
      #     if(!action_button_load()) color_button_load(hardcorded_initial_color) else
      #     if(action_button_load()) color_button_load("red")
      #
      #   } else
      #
      #   if(control_file_source_99()){
      #     if(action_button_load()) color_button_load("green")
      #   }
      #
      # })

      observeEvent(control02_post_database(), {

        if(is.null(control02_post_database())) color_button_load(hardcorded_initial_color) else
          if(control02_post_database()) color_button_load("green") else
            if(!control02_post_database()) color_button_load("red")


      })




      output$df_database <- renderTable({


        req(control02_post_database(), action_button_show(), database())

        database()
        #mtcars
      }, rownames = TRUE, align = "c")






      # # # # INTERNAL CONTROLS
      # Control 01 - database
      control01_database <- reactive({

        dt_database <- is.null(database())


        if(dt_database) return(FALSE) else return(TRUE)


      })










     all_var_names <- reactive({
       #req(database())
       colnames(database())
     })

     output_list <- reactive({

       req(action_button_show())

       if(!action_button_show()) return(NULL)


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

