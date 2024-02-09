# Space database


# # # 01) UI - Selection for 'database'
module01_database_s01_excel_ui <- function(id){
  ns <- shiny::NS(id)


  div(shinyjs::useShinyjs(), id = ns("input-panel"),

      h2("Initial user election - database"),
      fluidRow(
        column(2,

             # # # For xlsx files...
             div(shinyjs::useShinyjs(), id = ns("input-xlsx"),
                              fileInput(inputId = ns("info_xlsx"),
                                        label = "File '.xlsx' selection..."),
                              uiOutput(ns("sheet_selection"))
             )
        ),


        # # # Action buttons
        column(6, br(), uiOutput(ns("action_buttons")))
      ),  br(), br(),

    # End fluidRow

    # # # Visualization for database
    fluidRow(

      column(12, tableOutput(ns("df_database")))

      )

  ) # End div
}



# # # 01) SERVER - Selection for 'database'
module01_database_s01_excel_server <- function(id){
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
      control_user_99 <- reactiveVal(FALSE)
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







      # # # Excel --------------------------------------------------------------
      # # # Excel control 01 of 04
      # input$file_source is "xlsx"


      # # # Excel control 02 of 04
      # input$info_xlsx control
      control_user_01 <- reactive({

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

        req(control_user_01())
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
      control_user_02 <- reactive({

        req(control_user_01())

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

        req(control_user_02())
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
        req(control_user_02())

        pos_selected <- 1

        # # # SelectInput for sheet selection
        selectInput(inputId = ns("sheet_xlsx"),
                    label = "Sheet selector:",
                    choices = sheets_xlsx(),
                    selected = sheets_xlsx()[pos_selected])

      })


      # # # Excel control 04 of 04
      control_user_03 <- reactive({

        req(control_user_02())

        validate(


          need(!is.null(input$sheet_xlsx), 'Error 06: Sheet problems. Change the sheet names.'),
          need(input$sheet_xlsx != '', "Select a sheet from the 'xlsx' file."),



        )

        return(TRUE)

      })


      observeEvent(input$sheet_xlsx, {

        req(control_user_03())



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
      control_user_99 <- reactive({

        req(control_user_03())

        dt_control <- control_user_03()

        return(dt_control)

      })




      # # # Activate 'load' ---------------------------------------------
      # 1) If the load button is pressed...
      # If all the previous control for database are OK we change the state of
      # the action "load" to TRUE.
      # For xlsx files we take the info_xlsx information
      # internally to try import the selected xlsx file.
      observeEvent(input$action_load, {


        req(input$action_load, control_user_99())
        action_button_load(TRUE)
        database(
          read.xlsx(xlsxFile = selected_info_xlsx()$datapath,
                    sheet = input$"sheet_xlsx")
        )
      })





      # Import database --------------------------------------------------------
      # observeEvent(action_button_load(),{
      #
      #
      #       req(control_user_99(), action_button_load())
      #
      #
      #       database(
      #         read.xlsx(xlsxFile = selected_info_xlsx()$datapath,
      #                   sheet = input$"sheet_xlsx")
      #       )
      #
      # })


      # # # database post control
      control02_post_database <- reactive({

        req(control_user_99(), action_button_load())

        validate(
          need(!is.null(database()), "Error Database 01: 'database' is a NULL object."),
          need(is.data.frame(database()), "Error Database 02: The 'database' object must be a dataframe."),
          need(ncol(database()) > 0, "Error Database 03: 'database' must have at least one column."),
          need(ncol(database()) > 0, "Error Database 04: 'database' must have at least one row.")


        )


        return(TRUE)

      })


      observeEvent(action_button_load(),{


        req(control_user_99(), action_button_load(), control02_post_database())

        action_button_show(TRUE)
      })



      # # # Activate 'show'
      # If database has beed imported succesfully, and all the controls
      # post upload are OK, we put the status for 'show' as TRUE.
      observeEvent(input$action_load, {
        #req(control02_post_database())
        if(!action_button_show() && is.null(database())){

                  color_button_load("red")
        } else color_button_load("green")



      })




      # observeEvent(action_button_load(), {
      #
      #   #req(action_button_show())
      #   print("AAA")
      #   print(is.null(control02_post_database()))
      #   print(control02_post_database())
      #   print("BBB")
      #
      #
      #     if(action_button_show()) color_button_load("green") else
      #       if(!control02_post_database() && !action_button_show()) color_button_load("red") else
      #         if(is.null(control02_post_database()) && !action_button_show()) color_button_load("red")
      #
      #
      # })




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

       req(database(), action_button_show())
       colnames(database())
     })



     intro_source_data <- reactive({

       req(database(), action_button_show())

       out_detail <- paste0("Excel file - ", selected_info_xlsx()$name)
       out_detail
     })

     xlsx_file_name <- reactive({

       req(database(), action_button_show())
       selected_info_xlsx()$name

     })

     output_list <- reactive({

       req(database(),
           action_button_show(),
           all_var_names(), intro_source_data(), xlsx_file_name())



       the_list <- list(all_var_names(), database(), intro_source_data(), xlsx_file_name())

       names(the_list) <- c("all_var_names", "database", "info_source_data", "xlsx_file_name")

       return(the_list)
       })



     # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      # # # # Tab 01 - database







    return(output_list)



    }
  )
}

