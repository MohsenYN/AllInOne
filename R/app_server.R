#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#'
#' @noRd
app_server <- function(input, output, session) {

  rv <- shiny::reactiveValues(
    data = NULL, dataC = NULL, dataT = NULL, VarPYSL = NULL, flags = 1,
    SelectedTraits = NULL, Vec_LASTV = NULL, dependent_variables = NULL,
    independent_variables = NULL, dep_col = NULL, buffer = NULL, spat_buffer = NULL,
    indep_col = NULL, outliers = NULL, outliers_row = NULL,
    active_opt_ = 'db', slider.k = 1, filter.k = base::rep(base::list(base::list(search = "")), 500),
    selected.col = NULL, cor_temp = NULL, slider.str = '', filter_flag = 0,
    pdf_address = NULL, png_address = NULL, csv_address = NULL, txt_address = NULL,
    csv_value = NULL, csv_value2 = NULL, review_flag = TRUE, refresh_flag = NULL,
    blup_buffer = NULL, blup_temp = NULL, Maximum_Level_For_Group_By = 20,
    Ignore_Reserved_Letters = T, Replace_Reserved_Letters = F, User_Config_notif_delay = 8, User_Config_notif_size = 4
    , Path_For_Saving_Results = '', Show_Errors = T,
  )

  allinone_initialize <- function(rv) {
    shiny::updateSelectInput(inputId = 'res_blue_str', selected = 'None')
    rv$outliers_row = NULL
    rv$selected.col = NULL
    rv$review_flag = TRUE
    if (dir.exists(app_sys('app/Results')))
      unlink(app_sys('app/Results'), recursive = T)
    dir.create(paste0(app_sys('app'), '/Results'))
    cat("\014")
  }

  if (dir.exists(app_sys('app/Results')))
    unlink(app_sys('app/Results'), recursive = T)
  dir.create(paste0(app_sys('app'), '/Results'))

  forbidden_characters <- base::c('/', ':', '\\', '<', '>', '|', '*', '?', '"',
                                  ' ', '!', ';', ',', '|', '!', '@', '#', '$',
                                  '%', '^', '&', '*', '(', ')', '+', '-')

  # shinyjs::hideElement('Rep_Res_Wrd')

  shiny::observe({
    rv$User_Config_notif_delay = input$notif_delay
    rv$User_Config_notif_size = input$notif_size
    rv$Maximum_Level_For_Group_By = input$Max_levels_GB
    rv$Ignore_Reserved_Letters = input$Ign_Res_Wrd
    # rv$Replace_Reserved_Letters = input$Rep_Res_Wrd
  })

  # shiny::observeEvent(input$save_results, {
  #   if (!input$save_results) {
  #     shinyjs::disable('results_folder')
  #     shinyjs::disable('results_folder_path')
  #   }
  #
  #   else {
  #     shinyjs::enable('results_folder')
  #     shinyjs::enable('results_folder_path')
  #   }
  #
  # })

  # observeEvent(input$results_folder, {
  #   base::tryCatch({
  #     shiny::updateTextInput(inputId = 'results_folder_path', value = utils::choose.dir())
  #   }, error = function(e) {
  #     shiny_showNotification(rv, e$message)
  #     shiny_showNotification(rv, 'As some thing went wrong in selecting the path, please copy it in the folowing inputbox')
  #   })
  # })

  # observeEvent(input$results_folder_path, {
  #   rv$Path_For_Saving_Results = input$results_folder_path
  # })

  output$o_res_blue_k <- renderUI({
    if (!is.null(input$res_blue_str)) {
      str = input$res_blue_str
      if (base::dir.exists(app_sys(paste0("app/Results/"), str))) {
        imgs <-
          base::list.files(app_sys(base::paste0("app/Results/", str)),
                           pattern = stringr::regex("*.(png|csv|txt)"))
        if (length(imgs) > 0) {
          imgs = put_csv_last(imgs)
          shiny::selectInput(
            'res_blue_k',
            'Results file',
            imgs,
            width = '100%'
          )
        }
      }else
        if (str != 'None')
          shiny::h4(paste0('No output history in ', str, ' Menu'))
    }
  })

  output$header_notification_deactive <- shinydashboard::renderMenu({
    shinydashboard::dropdownMenu(
      type = "messages",
      shinydashboard::messageItem(
        from = 'Admin',
        message = "Welcome to the AllInOne Shinyapp!"
      )
    )
  })

  output$o_results_btns <- renderUI({
    if ((input$res_blue_str != 'None') & (!is.null(rv$png_address)))
      shiny::tagList(
        if (base::file.exists(rv$pdf_address))
          shiny::downloadButton('download_pdf_res', 'Download as PDF'),
        if (base::file.exists(rv$png_address))
          shiny::downloadButton('download_png_res', 'Download full size image'),
        if (base::file.exists(rv$csv_address))
          shiny::downloadButton('download_csv_res', 'Download the table'),
        # if (base::file.exists(rv$csv_address))
        #   shiny::actionButton('csvs_use', 'Set the table as main dataset'),
      )
  })

  observeEvent(input$res_blue_str, {
    rv$pdf_address = NULL
    rv$png_address = NULL
    rv$csv_address = NULL
    rv$csv_value = NULL
  })

  output$o_results <- renderUI({
    tryCatch({
      if (!is.null(input$res_blue_str) & !is.null(input$res_blue_k)) {
        str = input$res_blue_str
        imgs <-
          base::list.files(app_sys(base::paste0("app/Results/", str)),
                           pattern = stringr::regex("*.(png|csv|txt)"))
        if (length(imgs) > 0) {

          imgs = put_csv_last(imgs)

          k = which(imgs == input$res_blue_k)

          if (length(k) != 0) {
            file_address = app_sys('app/Results/', str, '/', imgs[k])

            rv$png_address = base::paste0(base::substring(file_address, 1, base::nchar(file_address) - 4), '.png')
            rv$pdf_address = base::paste0(base::substring(file_address, 1, base::nchar(file_address) - 4), '.pdf')
            rv$csv_address = base::paste0(base::substring(file_address, 1, base::nchar(file_address) - 4), '.csv')
            rv$txt_address = base::paste0(base::substring(file_address, 1, base::nchar(file_address) - 4), '.txt')

            file_address = base::paste0('Results/', str, '/', imgs[k], '?', runif(1, 1, 2))

            extention = base::substring(imgs[k], base::nchar(imgs[k]) - 2, base::nchar(imgs[k]))

            if (extention == 'png') {
              shiny::img(src = file_address, style = 'width : 100%')
            }
            else if (extention == 'csv') {
              rv$csv_value = utils::read.csv(
                file = rv$csv_address, header = TRUE,
                sep = ",", fileEncoding = "UTF-8-BOM")

              DT::renderDataTable(
                rv$csv_value,
                options = base::list(
                  scrollX = TRUE,
                  columnDefs = list(list(className = 'dt-center', targets = '_all')),
                  scrollCollapse = TRUE, dom = 'ltip')
              )
            }
            else if (extention == 'txt') {
              sum = ''
              for (i in readLines(rv$txt_address)) {
                sum = paste0(sum, '<br/>', i)
              }
              helpText(HTML(sum))
            }
          }
        }
      }
    }, error = function(e) {
      shiny_showNotification(rv, e$message)
    })
  })

  show_slider <- function(str, k = 1) {
    tryCatch({
      rv$slider.str = str
      rv$slider.k = k

      imgs <-
        base::list.files(app_sys(base::paste0("app/Results/", str)),
                         pattern = stringr::regex("*.(png|csv|txt)"))
      imgs = put_csv_last(imgs)

      if (base::is.null(rv$slider.k))
        k = 1
      if (k <= base::length(imgs) & k > 0) {
        file_address = app_sys('app/Results/', str, '/', imgs[k])

        rv$png_address = base::paste0(base::substring(file_address, 1, base::nchar(file_address) - 4), '.png')
        rv$pdf_address = base::paste0(base::substring(file_address, 1, base::nchar(file_address) - 4), '.pdf')
        rv$csv_address = base::paste0(base::substring(file_address, 1, base::nchar(file_address) - 4), '.csv')
        rv$txt_address = base::paste0(base::substring(file_address, 1, base::nchar(file_address) - 4), '.txt')

        file_address = base::paste0('Results/', str, '/', imgs[k], '?', runif(1, 1, 2))

        extention = base::substring(imgs[k], base::nchar(imgs[k]) - 2, base::nchar(imgs[k]))
        shiny::showModal(shiny::modalDialog(
          title = shiny::tags$b(base::substring(imgs[k], 1, base::nchar(imgs[k]) - 4)),
          if (extention == 'png') {
            shiny::HTML(
              paste0(
                '<img
            src="', file_address, '" ,
          style="width:100%;height:auto;object-fit:contain",
          alt = " This plot is not availabe based on your data or the used arguments!",
          >'
              )
            )
          }else if (extention == 'csv') {
            rv$csv_value = utils::read.csv(
              file = rv$csv_address, header = TRUE,
              sep = ",", fileEncoding = "UTF-8-BOM")

            DT::renderDataTable(
              rv$csv_value,
              options = base::list(
                scrollX = TRUE,
                scrollCollapse = TRUE, dom = 'ltip')
            )
          }else if (extention == 'txt') {
            sum = ''
            for (i in readLines(rv$txt_address)) {
              sum = paste0(sum, '<br/>', i)
            }
            helpText(HTML(sum))
          },
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::actionButton('slider_back', 'Previous'),
            if (base::file.exists(rv$pdf_address))
              shiny::downloadButton('download_pdf', 'Download as PDF'),
            if (base::file.exists(rv$png_address))
              shiny::downloadButton('download_png', 'Download full size image'),
            if (base::file.exists(rv$csv_address))
              shiny::downloadButton('download_csv', 'Download the table'),
            if (base::file.exists(rv$csv_address))
              shiny::actionButton('csvs_use', 'Set the table as main dataset'),

            shiny::actionButton('slider_next', base::paste0(k, '/', base::length(imgs), '  Next'))
          )
        ))
      }
    }, error = function(e) {
      shiny_showNotification(rv, e$message)
    })
  }

  db.edit <- function(temp_r, temp_c, temp_v, col_type = 'number') {

    if (col_type != 'subset_dataset') {
      temp_r <- base::as.numeric(temp_r)

      if (col_type == 'number') {
        temp_c <- base::as.numeric(temp_c)
      }

      if (base::is.numeric(rv$data[[temp_c]])) {
        rv$data[[temp_c]][temp_r] = base::as.numeric(temp_v)
      }
      else
        rv$data[[temp_c]][temp_r] = temp_v

      if (col_type == 'number') {
        shiny::updateTextInput(inputId = base::paste0('OTL_', base::colnames(rv$data))[temp_c], value = temp_v)
      } else {
        shiny::updateTextInput(inputId = base::paste0('OTL_', temp_c, value = temp_v))
      }
    }else {
      rv$outliers = NULL
      rv$outliers_row = NULL
      res = NULL
      for (i in input$subset_levels) {
        buffer = base::subset(rv$data, base::get(input$subset_indep) == i)
        res = rbind(res, buffer)
      }
      rv$data <- res
      base::rownames(rv$data) = 1:base::nrow(rv$data)
    }
    rv$VarPYSL <-
      rv$data %>% dplyr::select(dplyr::all_of(input$main_db_indep_val))

    # include Dependent variables
    rv$SelectedTraits <-
      rv$data %>% dplyr::select(dplyr::all_of(input$main_db_dep_val))

    # include Independent variables TOO
    rv$independent_variables <-
      rv$data %>% dplyr::select(input$main_db_indep_val)

    # include Dependent variables TOO
    rv$dependent_variables <-
      rv$data %>% dplyr::select(input$main_db_dep_val)
  }

  output$information <- shiny::renderUI({
    information_ui
  })

  output$o_opt_list <- shiny::renderUI({
    shiny::selectInput(
      inputId = 'active_opt',
      label = shiny::h6('Select Operator'),
      choices = base::list(
        'Dataset' = 'db',
        'Select variables' = 'ind_var',
        'Interaction' = 'interaction',
        'Subset the dataset' = 'subset'
      ), selected = rv$active_opt_
    )
  })

  output$mice_input <- shiny::renderUI({
    if (input$impute_method != 'rm') {
      shiny::tagList(
        shiny::helpText(mice_help[[input$impute_method]]),
        shiny::textInput('mice_input_m',
                         'Specify the number of imputation',
                         value = 5),
        shiny::textInput('mice_input_maxit',
                         'Specify the number of iteration',
                         value = 5, placeholder = 'A scalar giving the number of iterations'),
        shiny::textInput('mice_input_seed',
                         'Specify the seed number',
                         value = 500),
        shiny::selectInput('mice_input_collinear',
                           'Specify if the collinearity effect should be removed',
                           choices = base::c('Remove' = T, 'Don\'t remove' = F), selected = 'Remove')
      )
    }
  })

  shiny::observeEvent(input$file$data, {
    tryCatch({
      db_flag = T
      address = input$file$data
      postfix = base::substring(
        address,
        base::nchar(address) - 3,
        base::nchar(address)
      )
      if (postfix == "xlsx") {
        rv$dataC <- readxl::read_xlsx(address, sheet = 1)
        output$dataC_sheet <- shiny::renderUI({
          if (!input$use_sampledb)
            shiny::selectInput('sheet_name', 'Sheet', choices = readxl::excel_sheets(address), selected = 1)
        })
      }
      else if (postfix == ".csv") {
        rv$dataC <-
          utils::read.csv(
            address,
            header = TRUE,
            sep = ",",
            fileEncoding = "UTF-8-BOM"
          )
        output$dataC_sheet <- shiny::renderUI({
          if (!input$use_sampledb) {
            shiny::tagList(
              shiny::selectInput(
                inputId = 'dataC_delimiter',
                label = 'Delimiter',
                choices = list(
                  Tab = "\t", Comma = ",", Semicolon = ";", Space = " ")
                , selected = ','),
              shiny::checkboxInput(
                inputId = 'dataC_header',
                label = 'Header',
                value = T
              )
            ) }
        })
      }
      else if (postfix == ".txt") {
        rv$dataC <-
          utils::read.delim(
            address,
            header = TRUE,
            fileEncoding = "UTF-8-BOM")
        output$dataC_sheet <- shiny::renderUI({
          if (!input$use_sampledb) {
            shiny::tagList(
              shiny::selectInput(
                inputId = 'dataC_delimiter',
                label = 'Delimiter',
                choices = list(
                  Empty = '', Tab = "\t", Comma = ",", Semicolon = ";", Space = " "),
                selected = '\t'),
              shiny::checkboxInput(
                inputId = 'dataC_header',
                label = 'Header',
                value = T
              )
            )
          }
        })
      }
      else {
        db_flag = FALSE
        session$sendCustomMessage(type = 'testmessage',
                                  message = "Please select a valid dataset !")
      }
      if (db_flag) {
        output$columns_name <- shiny::renderUI({
          if (!input$use_sampledb)
            shiny::selectInput('columns_name_list', 'Columns', base::colnames(rv$dataC))
        })
        output$column_new_name <- shiny::renderUI({
          if (!input$use_sampledb)
            shiny::textInput('new_col_name', 'New Name:', value = input$columns_name_list)
        })
        output$columns_name_btn <- shiny::renderUI({
          if (!input$use_sampledb)
            shiny::actionButton('new_col_name_btn', 'Apply new name')
        })
      }
    }, error = function(e) {
      shiny_showNotification(rv, e$message)
    })
  })

  shiny::observeEvent(input$setting_file$data, {
    # tryCatch({
    address = input$setting_file$data
    postfix = base::substring(
      address,
      base::nchar(address) - 3,
      base::nchar(address)
    )
    if (postfix == ".csv") {
      setting_dat <- as.data.frame(
        utils::read.csv(
          address,
          fileEncoding = "UTF-8-BOM"
        )
      )
      notif_delay = as.numeric(setting_dat[['General']][1])
      notif_size = as.numeric(setting_dat[['General']][2])
      Ign_Res_Wrd = as.logical(setting_dat[['General']][3])

      shiny::updateNumericInput(inputId = 'notif_delay', value = notif_delay)
      shiny::updateSelectInput(inputId = 'notif_size', selected = notif_size)
      shiny::updateCheckboxInput(inputId = 'Ign_Res_Wrd', value = Ign_Res_Wrd)
      ######################################################################################
      Max_levels_GB = as.numeric(setting_dat[['Plots']][1])

      shiny::updateNumericInput(inputId = 'Max_levels_GB', value = Max_levels_GB)
    }
    else {
      shiny_showNotification(rv, 'Failed to import setting file')
    }
    # }, error = function(e) {
    #   shiny_showNotification(rv, e$message)
    # })
  })

  observeEvent(input$save_setting, {
    notif_delay = input$notif_delay
    notif_size = input$notif_size
    Ign_Res_Wrd = input$Ign_Res_Wrd

    ######################################################################################
    Max_levels_GB = input$Max_levels_GB

    l = list()

    l[['General']][1] = notif_delay
    l[['General']][2] = notif_size
    l[['General']][3] = Ign_Res_Wrd
    l[['Plots']][1] = Max_levels_GB

    write.csv(l, 'My_Setting.csv', row.names = F)
  })

  observeEvent(
    ignoreInit = TRUE, c(
      input$dataC_delimiter,
      input$dataC_header), {
    tryCatch({
      address = input$file$data
      postfix = base::substring(
        address,
        base::nchar(address) - 3,
        base::nchar(address)
      )
      if (postfix == ".csv") {
        rv$dataC <-
          utils::read.csv(
            address,
            header = input$dataC_header,
            sep = input$dataC_delimiter,
            fileEncoding = "UTF-8-BOM"
          )
      }else if (postfix == ".txt") {
        rv$dataC <-
          utils::read.delim(
            address,
            header = input$dataC_header,
            sep = input$dataC_delimiter,
            fileEncoding = "UTF-8-BOM"
          )
      }else {
        session$sendCustomMessage(type = 'testmessage',
                                  message = "Please select a valid dataset !")
      }
    }, error = function(e) {
      shiny_showNotification(rv, e$message)
    })
  })

  observeEvent(input$sheet_name, {
    tryCatch({
      address = input$file$data
      postfix = base::substring(
        address,
        base::nchar(address) - 3,
        base::nchar(address)
      )
      if (postfix == "xlsx") {
        buf = readxl::excel_sheets(address)
        rv$dataC <- readxl::read_xlsx(address, sheet = which(buf == input$sheet_name))
      }else {
        session$sendCustomMessage(type = 'testmessage',
                                  message = "Please select a valid dataset !")
      }
    }, error = function(e) {
      shiny_showNotification(rv, e$message)
    })
  })

  check_name <- function(name, force = F) {
    if (name == '')
      base::return('')

    if (rv$Replace_Reserved_Letters | force) {
      str = base::strsplit(name, '')[[1]]
      for (c in 1:base::length(str)) {
        if (str[c] %in% forbidden_characters) {
          str[c] = '_'
        }
      }
      buf = ''
      for (j in str)
        buf = base::paste0(buf, j)
      base::return(buf)
    }

    if (rv$Ignore_Reserved_Letters) {
      base::return(name)
    }

    str = base::strsplit(name, '')[[1]]
    for (s in str) {
      if (s %in% forbidden_characters) {
        base::return('')
      }
    }
    base::return(name)
  }

  shiny::observeEvent(input$new_col_name_btn, {
    if (check_name(input$new_col_name) != '') {
      base::colnames(rv$dataC)[base::which(base::names(rv$dataC) == input$columns_name_list)] <- check_name(input$new_col_name)
    }else {
      shiny_showNotification(rv, 'Column names can not include " \ | ? * : < > () and space')
    }
  })

  shiny::observeEvent(input$subset_btn, {
    if (!is.null(input$subset_levels) & !is.null(input$subset_indep)) {
      shiny::removeModal()
      db.edit(1, 1, 1, 'subset_dataset')
    }
  })

  shiny::observeEvent(input$subset_indep, {
    subset_ch = base::levels(base::as.factor(rv$data[[input$subset_indep]]))
    output$subset_levels <- shiny::renderUI({
      shiny::selectInput(
        'subset_levels',
        'Select Level(s)',
        choices = subset_ch,
        multiple = T)
    })
  })

  shiny::observeEvent(input$run_normalize, {
    if (!base::is.null(input$nbin) &
      !base::is.na(base::as.numeric(input$nbin)) &
      input$nbin > 0) {
      shiny::removeModal()
      if (base::dir.exists(app_sys("app/Results/Normalization")))
        base::unlink(app_sys("app/Results/Normalization"), recursive = TRUE)
      waiter$show()
      base::tryCatch({
        NormaLiZaTIoN(input, rv)
      }, error = function(e) {
        if (rv$Show_Errors)
          shiny_showNotification(rv, e$message)
        else
          shiny_showNotification(rv, 'Incorrect arguments, please review your data!')
        # base::setwd("../../")
      })
      waiter$hide()
      show_slider('Normalization')
    }
  })

  shiny::observeEvent(input$slider_next, {
    imgs <-
      base::list.files(app_sys(base::paste0("app/Results/", rv$slider.str)),
                       pattern = stringr::regex("*.(png|csv|txt)"))
    imgs = put_csv_last(imgs)
    if (rv$slider.k >= base::length(imgs)) {
      show_slider(rv$slider.str, 1)
    }else
      show_slider(rv$slider.str, rv$slider.k + 1)
  })

  shiny::observeEvent(input$slider_back, {

    if (rv$slider.k <= 1) {
      imgs <-
        base::list.files(app_sys(base::paste0("app/Results/", rv$slider.str)),
                         pattern = stringr::regex("*.(png|csv|txt)"))
      imgs = put_csv_last(imgs)
      show_slider(rv$slider.str, base::length(imgs))
    }else
      show_slider(rv$slider.str, rv$slider.k - 1)
  })

  shiny::observeEvent(input$missing_handler_btn, {
    if (input$missing_handler_opt == 'missing') {
      if (base::dir.exists(app_sys("app/Results/Missing Values")))
        base::unlink(app_sys("app/Results/Missing Values"), recursive = TRUE)
      waiter$show()
      base::tryCatch({
        CheckMissing(input, rv)
      }, error = function(e) {
        if (rv$Show_Errors)
          shiny_showNotification(rv, e$message)
        else
          shiny_showNotification(rv, 'Incorrect arguments, please review your data!')
        # base::setwd("../../")
      })
      waiter$hide()
      show_slider("Missing Values")
    }
    else if (input$missing_handler_opt == 'impute') {
      shiny::removeModal()
      if (base::dir.exists(app_sys("app/Results/Missing Imputation")))
        base::unlink(app_sys("app/Results/Missing Imputation"), recursive = TRUE)
      waiter$show()
      base::tryCatch({
        ImputeMissing(input, rv, session)
      }, error = function(e) {
        if (rv$Show_Errors)
          shiny_showNotification(rv, e$message)
        else
          shiny_showNotification(rv, 'Incorrect arguments, please review the data!')
        # base::setwd("../../")
      })
      waiter$hide()
      show_slider('Missing Imputation')
    }
  })

  output$scatter_vars_ui <- shiny::renderUI({
    if ('scatterplot' %in% input$plots_name) {
      shiny::selectInput(
        'scatter_vars',
        'Select two dependant/response variables for scatterplot',
        choices = input$main_db_dep_val,
        selected = input$scatter_vars,
        multiple = T
      )
    }
  })

  output$boxplot_vars_ui <- shiny::renderUI({
    buf = base::c(
      'Boxplot' = 'boxplot',
      'Densityplot' = 'densityplot',
      'Violinplot' = 'violinplot'
    )
    if ('TRUE' %in% (buf %in% input$plots_name)) {
      #Remove variables with too much levels
      indep_cols = input$main_db_indep_val
      for (i in indep_cols) {
        if (base::length(base::unique(rv$data[[i]])) > rv$Maximum_Level_For_Group_By) {
          indep_cols = base::subset(indep_cols, indep_cols != i)
        }
      }

      shiny::selectInput(
        'boxplot_vars',
        'Select variables for plot(s)',
        choices = indep_cols,
        selected = input$boxplot_vars,
        multiple = T
      )
    }
  })

  shiny::observeEvent(input$opt_list_btn_2, {

    #to make sure that all directories are deletable
    temp = rv$buffer
    for (c in 1:100) {
      rv$buffer = FALSE
      base::tryCatch(
        base::invisible(grDevices::dev.off()),
        error = function(e) {
          rv$buffer = TRUE
        })
      if (rv$buffer) {
        rv$buffer = temp
        break
      }
    }
    if (!rv$review_flag)
    {
      if (input$active_opt_2 == 'missing_handler') {
        shiny::showModal(
          shiny::modalDialog(
            shiny::radioButtons(
              inputId = 'missing_handler_opt',
              label = 'Would you like to',
              choices = base::c('Check Missing Pattern' = 'missing',
                                'Impute Missing Value' = 'impute')),
            uiOutput('mice_input2'),
            easyClose = FALSE,
            footer = shiny::tagList(
              shiny::actionButton('missing_handler_btn', 'Done'),
              shiny::modalButton('Dismiss')
            )
          )
        )
        output$mice_input2 <- renderUI({
          if (input$missing_handler_opt == 'impute') {
            shiny::tagList(
              shiny::selectInput(
                'impute_method',
                'Select the imputation method',
                base::c(
                  'Remove Missing Point' = 'rm',
                  # 'Random Forest' = 'rand',
                  # 'MI' = 'mi',
                  'Predictive mean matching' = 'pmm',
                  'Weighted predictive mean matching' = 'midastouch',
                  'Random sample from observed values' = 'sample',
                  'Classification and regression trees' = 'cart',
                  'Random forest imputations' = 'rf',
                  'Unconditional mean imputation' = 'mean',
                  'Bayesian linear regression' = 'norm',
                  'Linear regression ignoring model error' = 'norm.nob',
                  'Linear regression using bootstrap' = 'norm.boot',
                  'Linear regression predicted values' = 'norm.predict',
                  'Lasso linear regression' = 'lasso.norm',
                  'Lasso select and linear regression' = 'lasso.select.norm'
                ),
                selected = 'rm'
              ),
              shiny::uiOutput('mice_input')
            )
          }
        })
      }

      else if (input$active_opt_2 == 'outlier') {
        output$normal_outlier_ui <- shiny::renderUI({
          if (input$outlier_method == 'A')
            shiny::tagList(
              shiny::radioButtons(
                inputId = 'indep_outlier_2',
                label = 'Select the independent variable',
                choices = input$main_db_indep_val),

              shiny::textInput(
                'minp',
                "Lower (Q1) quartile percentile",
                value = 0.25
              ),

              shiny::textInput(
                'maxp',
                "Upper quartile (Q3) percentile",
                value = 0.75
              ))
        })

        output$cooksdistance_ui <- shiny::renderUI({
          if (input$outlier_method == 'B')
            shiny::tagList(
              shiny::selectInput('outlier_resp',
                                 'Dependant/response variable',
                                 choices = input$main_db_dep_val
              ),
              shiny::checkboxGroupInput('outlier_rand',
                                        'Select the independent variable(s)',
                                        choices = input$main_db_indep_val)
            )
        })

        shiny::showModal(shiny::modalDialog(
          shiny::radioButtons(
            inputId = 'outlier_method',
            label = 'Outlier method',
            choices = base::c('Quantile' = 'A', "Cook\'s Distance" = 'B')),
          shiny::uiOutput('cooksdistance_ui'),
          shiny::uiOutput('normal_outlier_ui'),
          footer = shiny::tagList(
            shiny::actionButton('run_outlier', 'Run'),
            shiny::modalButton('Dismiss')),
          easyClose = FALSE
        ))
      }

      else if (input$active_opt_2 == 'plots') {
        indep_cols = input$main_db_indep_val
        for (i in indep_cols) {
          if (base::length(base::unique(rv$data[[i]])) > rv$Maximum_Level_For_Group_By) {
            indep_cols = base::subset(indep_cols, indep_cols != i)
            shiny_showNotification(rv,
                                   base::paste0(
                                     'The variable {', i,
                                     '} is removed from independent variables list as it has more than ',
                                     rv$Maximum_Level_For_Group_By, ' levels!'
                                   )
            )
          }
        }

        str = NULL
        for (n in base::colnames(rv$dependent_variables)) {
          if (base::is.character(rv$data[[n]]))
            if (base::is.null(str))
              str = base::paste0(str, n)
            else
              str = base::paste0(str, ' and ', n)
        }
        if (!base::is.null(str))
          shiny_showNotification(rv, base::paste0('Only continuous variables, it is not possible to draw plots for ', str))
        shiny::showModal(
          shiny::modalDialog(
            title = 'Data Visualization',
            shiny::checkboxGroupInput(
              'plots_name',
              'Select the plot(s) type',
              choices = base::c(
                'Boxplot' = 'boxplot',
                'Densityplot' = 'densityplot',
                'Violinplot' = 'violinplot',
                'Scatterplot' = 'scatterplot'
              )
            ),

            shiny::uiOutput('boxplot_vars_ui'),
            shiny::uiOutput('scatter_vars_ui'),
            easyClose = T,
            footer = shiny::actionButton('boxplot_modal_btn', 'OK')
          )
        )
      }

      else if (input$active_opt_2 == 'correlation') {
        dep_cols = input$main_db_dep_val
        indep_cols = input$main_db_indep_val
        for (i in indep_cols) {
          if (base::length(base::unique(rv$data[[i]])) < 2) {
            indep_cols = base::subset(indep_cols, indep_cols != i)
            shiny_showNotification(rv,
                                   base::paste0('The variable {', i,
                                                '} is removed from independent variables list as it has less then two levels!'
                                   )
            )
          } else if (base::length(base::unique(rv$data[[i]])) > rv$Maximum_Level_For_Group_By) {
            indep_cols = base::subset(indep_cols, indep_cols != i)
            shiny_showNotification(rv,
                                   base::paste0(
                                     'The variable {', i,
                                     '} is removed from independent variables list as it has more than ',
                                     rv$Maximum_Level_For_Group_By, ' levels!'
                                   )
            )
          }
        }
        output$cor_intera_indep <- shiny::renderUI({
          if (input$cor_opt == 'Inter correlation') {
            shiny::radioButtons(
              inputId = 'indep_cor',
              label = 'Select one independent variable',
              choices = indep_cols)
          }else if (input$cor_opt == 'Intra correlation') {
            shiny::checkboxGroupInput(
              inputId = 'indep_cor',
              label = 'Select independent variable(s)',
              choices = indep_cols)
          }
        })
        output$cor_intera_dep <- shiny::renderUI({
          if (input$cor_opt == 'Inter correlation') {
            shiny::radioButtons(
              inputId = 'dep_cor',
              label = 'Select one dependent/response variable',
              choices = dep_cols)
          }
          else if (input$cor_opt == 'Intra correlation') {
            shiny::checkboxGroupInput(
              inputId = 'dep_cor',
              label = 'Select two dependent/response variables',
              choices = dep_cols)
          }
        })

        shiny::showModal(shiny::modalDialog(
          shiny::radioButtons(
            inputId = 'cor_opt',
            label = 'Correlation methods',
            choices = base::c('Non-independent-based correlation',
                              'Inter correlation',
                              'Intra correlation')),
          shiny::uiOutput('cor_intera_indep'),
          shiny::uiOutput('cor_intera_dep'),
          footer = shiny::tagList(shiny::actionButton('indep_cor_btn', 'OK'),
                                  shiny::modalButton('Dismiss'))
        ))
      }

      else if (input$active_opt_2 == 'normalize') {
        shiny::showModal(shiny::modalDialog(
          shiny::textInput('nbin', "Set the number of bins"),
          footer = shiny::tagList(
            shiny::actionButton('run_normalize', 'Run'),
            shiny::modalButton('Dismiss')),
          easyClose = FALSE
        ))
      }

      else if (input$active_opt_2 == 'Heritability') {
        shiny::showModal(shiny::modalDialog(
          shiny::selectInput(
            'her_action', 'Action to do:',
            choices = base::c(
              'Spatial Analysis' = 'spatial',
              'Mixed Analysis' = 'mixed_analysis',
              'Heritability' = 'heritability'
            ), selected = 'spatial', multiple = F),

          if (!base::is.null(rv$spat_buffer)) {
            shiny::checkboxInput('use_spat', 'Use recent spatial analysis as dataset')
          },
          shiny::uiOutput('use_spat_checkbox_ui'),

          footer = shiny::tagList(shiny::actionButton('her_btn', 'OK'),
                                  shiny::modalButton('Dismiss'))
        ))
      }
    }
    else {
      session$sendCustomMessage(type = 'testmessage',
                                message = "Please select dependent and independent variables!")
    }
  })

  output$use_spat_checkbox_ui <- shiny::renderUI({
    if (!base::is.null(rv$spat_buffer)) {
      if (input$use_spat) {
        DT::renderDataTable(
          rv$spat_buffer,
          options = base::list(
            scrollX = TRUE,
            scrollCollapse = TRUE,
            dom = 'ltip'
          )
        )
      }
    }
  })
  shiny::observeEvent(input$her_btn, {
    shiny::removeModal()
    dep_cols = input$main_db_dep_val
    indep_cols = input$main_db_indep_val

    for (i in indep_cols) {
      if (base::length(base::unique(rv$data[[i]])) < 2) {
        indep_cols = base::subset(indep_cols, indep_cols != i)
        shiny_showNotification(rv,
                               base::paste0(i,
                                            ' is removed from independent variables list as it has less then two levels!'
                               )
        )
      }
    }
    if (base::length(indep_cols)) {

      if (input$her_action == 'spatial') {
        shiny::showModal(shiny::modalDialog(
          shiny::selectInput('spat_resp',
                             'Dependent/response variable',
                             choices = dep_cols
          ),
          shiny::selectInput(
            inputId = 'spat_gen',
            label = 'Genotype variable',
            choices = indep_cols),
          shiny::selectInput(
            inputId = 'spat_row',
            label = 'Row variable',
            choices = indep_cols,
            selected = 'Row'),

          shiny::selectInput(
            inputId = 'spat_col',
            label = 'Column variable',
            choices = indep_cols,
            selected = 'Col'),
          shiny::checkboxGroupInput('spat_fix',
                                    'Fixed effects',
                                    choices = indep_cols),

          shiny::selectInput('spat_fix_interact',
                             'If you have interaction for fixed effects; please select interacted columns two by two',
                             choices = indep_cols,
                             multiple = T),
          shiny::uiOutput('help_fix'),

          shiny::checkboxGroupInput('spat_rand',
                                    'Random effects',
                                    choices = indep_cols),

          shiny::selectInput('spat_rand_interact',
                             'If you have interaction for random effects; please select interacted columns two by two',
                             choices = indep_cols,
                             multiple = T),
          shiny::uiOutput('help_rand'),

          footer = shiny::tagList(shiny::actionButton('indep_spat_btn', 'OK'),
                                  shiny::modalButton('Dismiss'))
        ))
      }

      else if (input$her_action == 'blue') {
        shiny::showModal(shiny::modalDialog(
          shiny::selectInput('blue_resp',
                             'Dependent/response variable',
                             choices = dep_cols
          ),
          if (base::length(dep_cols) > 1)
            shiny::selectInput('blue_cof',
                               'Cofactor variable (if there is any)',
                               choices = dep_cols, multiple = TRUE
            ),
          shiny::checkboxGroupInput('blue_fix',
                                    'Fixed variable(s)',
                                    choices = indep_cols),

          shiny::selectInput('blue_fix_interact',
                             'If you have interaction for fixed effects; please select interacted columns two by two',
                             choices = indep_cols,
                             multiple = T),
          shiny::uiOutput('help_fix_blue'),

          shiny::checkboxGroupInput('blue_rand',
                                    'Random variable(s)',
                                    choices = indep_cols),

          shiny::selectInput('blue_rand_interact',
                             'If you have interaction for random effects; please select interacted columns two by two',
                             choices = indep_cols,
                             multiple = T),
          shiny::uiOutput('help_rand_blue'),
          footer = shiny::tagList(shiny::actionButton('indep_blue_btn', 'OK'),
                                  shiny::modalButton('Dismiss'))
        ))
      }

      else if (input$her_action == 'heritability') {
        shiny::showModal(shiny::modalDialog(
          shiny::checkboxGroupInput('her_rand',
                                    'Independent variable(s)',
                                    choices = indep_cols),
          shiny::selectInput('her_rand_interact',
                             'Interaction variable(s)',
                             choices = indep_cols, multiple = TRUE),
          shiny::radioButtons(
            inputId = 'indep_her',
            label = 'Genotype column',
            choices = indep_cols),
          footer = shiny::tagList(shiny::actionButton('indep_her_btn', 'OK'),
                                  shiny::modalButton('Dismiss')),
          shiny::uiOutput('help_her')
        ))
      }

      else if (input$her_action == 'blup') {
        shiny::showModal(shiny::modalDialog(
          shiny::selectInput('blup_resp',
                             'Please select dependant/response variable',
                             choices = dep_cols
          ),
          shiny::selectInput('blup_indep',
                             'Please select independent variable for estimating BLUP',
                             choices = indep_cols
          ),
          if (base::length(dep_cols) > 1)
            shiny::selectInput('blup_cof',
                               'Please select cofactor variable (if there is any)',
                               choices = dep_cols, multiple = TRUE
            ),
          shiny::checkboxGroupInput('her_rand',
                                    'Please select random variable(s)',
                                    choices = indep_cols),
          shiny::selectInput('her_rand_interact',
                             'If you have interaction for random effects; please select interacted columns two by two',
                             choices = indep_cols, multiple = TRUE),

          footer = shiny::tagList(shiny::actionButton('indep_blup_btn', 'OK'),
                                  shiny::modalButton('Dismiss')),
          shiny::uiOutput('help_her')
        ))
      }

      else if (input$her_action == 'mixed_analysis') {

        shiny::showModal(shiny::modalDialog(
          shiny::selectInput('blue_resp',
                             'Dependent/response variable',
                             choices = dep_cols
          ),

          shiny::numericInput(
            'mix_intercept',
            label = tags$span(
              'Intercept',
              tags$i(
                class = "glyphicon glyphicon-info-sign",
                style = "color: var(--Just-color);",
                title = 'Can be 1 (with intercept) or 0 (without intercept).
               For estimating BLUE, intercept should be 0.
               For estimating BLUP, intercept should be 1'
              )),
            0,
            -1,
            1,
            '100%'
          ),

          if (base::length(dep_cols) > 1)
            shiny::selectInput('blue_cof',
                               'Cofactor variable (if there is any)',
                               choices = dep_cols,
                               multiple = TRUE
            ),

          shiny::checkboxGroupInput('blue_fix',
                                    'Fixed variable(s)',
                                    choices = indep_cols),

          shiny::selectInput('blue_fix_interact',
                             'If you have interaction for fixed effects, please select interacted columns two by two',
                             choices = indep_cols,
                             multiple = T),

          shiny::uiOutput('help_fix_blue'),

          shiny::checkboxGroupInput('blue_rand',
                                    'Random variable(s)',
                                    choices = indep_cols),

          shiny::selectInput('blue_rand_interact',
                             'If you have interaction for random effects, please select interacted columns two by two',
                             choices = indep_cols,
                             multiple = T),

          shiny::uiOutput('help_rand_blue'),

          footer = shiny::tagList(shiny::actionButton('indep_mixed_btn', 'OK'),
                                  shiny::modalButton('Dismiss'))
        ))
      }
    }else {
      session$sendCustomMessage(
        type = 'testmessage',
        message = 'There should be at least one independent variable with more than one level')
    }

  })

  shiny::observeEvent(input$indep_spat_btn, {
    shiny::removeModal()
    if (base::dir.exists(app_sys("app/Results/Spatial Analysis")))
      base::unlink(app_sys("app/Results/Spatial Analysis"), recursive = TRUE)
    waiter$show()

    base::tryCatch({
      ExSPATS(input, rv)
    }, error = function(e) {
      waiter$hide()
      if (rv$Show_Errors)
        shiny_showNotification(rv, e$message)
      else
        shiny_showNotification(rv, 'Something is wrong! Would you like to check everything again? ')
      # base::setwd("../../")
    })

    waiter$hide()
    show_slider('Spatial Analysis')
  })

  shiny::observeEvent(input$spat_show, {
    shiny::removeModal()
    show_slider('Spatial Analysis')
  })

  shiny::observeEvent(input$use_spat_buffer, {
    rv$spat_buffer[['ResidualValue']] = NULL
    rv$data = rv$spat_buffer
    #   rv$spat_buffer should be NULL again but if you NULL it now it will be disappeared
  })

  shiny::observeEvent(input$indep_blue_btn, {
    if (base::length(input$blue_cof) > 2)
      shiny_showNotification(rv, 'You can only select one cofactor!')
    else if (
      input$rv_blue_rand == '' |
        input$rv_blue_fix == '' |
        base::length(input$blue_fix_interact) %% 2 != 0 |
        base::length(input$blue_rand_interact) %% 2 != 0 |
        base::length(input$blue_fix) + base::length(input$blue_fix_interact) == 0 |
        base::length(input$blue_rand) + base::length(input$blue_rand_interact) == 0
    )
      shiny_showNotification(rv, 'Error in formula')
    else {
      shiny::removeModal()
      if (base::dir.exists(app_sys("app/Results/Blue")))
        base::unlink(app_sys("app/Results/Blue"), recursive = TRUE)
      waiter$show()
      base::tryCatch({

        ExBLUE(input = input, rv = rv)

      }, error = function(e) {

        if (rv$Show_Errors)
          shiny_showNotification(rv, e$message)
        else
          shiny_showNotification(rv, 'Something is wrong! Would you like to check the dataset? ')
        # base::setwd("../../")
      })
      waiter$hide()
      show_slider('Blue')
    }
  })

  shiny::observeEvent(input$indep_mixed_btn, {


    shiny::removeModal()
    if (base::dir.exists(app_sys("app/Results/Mixed Analysis")))
      base::unlink(app_sys("app/Results/Mixed Analysis"), recursive = TRUE)
    waiter$show()
    base::tryCatch({

      Mixed_Analysis(input, rv)

    }, error = function(e) {

      if (rv$Show_Errors)
        shiny_showNotification(rv, e$message)
      else
        shiny_showNotification(rv, 'Something is wrong! Would you like to check the dataset? ')
      # base::setwd("../../")
    })
    waiter$hide()
    show_slider('Mixed Analysis')
  })

  output$help_fix <- shiny::renderUI({
    a = input$spat_fix
    f = ''
    flag = T
    for (s in a)
    {
      if (flag) {
        f = base::paste0('~', s)
        flag = F
      }else {
        f = base::paste0(f, ' + ', s)
      }
    }
    res = f
    f = ''
    a = input$spat_fix_interact
    if (base::length(a) %% 2 == 0 && base::length(a) != 0) {
      for (i in base::seq(from = 1, to = base::length(a), by = 2))
      {
        s = base::paste0(a[i], ':', a[i + 1])

        if (flag) {
          f = base::paste0('~', s)
          flag = F
        }else {
          f = base::paste0(f, ' + ', s)
        }
      }
      res = base::paste0(res, f)
    }

    shiny::textInput('rv_spat_fix', value = res, label = 'Fixed equation')
  })

  output$help_rand <- shiny::renderUI({
    a = input$spat_rand
    f = ''
    flag = T
    for (s in a)
    {
      if (flag) {
        f = base::paste0('~', s, ' ')
        flag = F
      }else {
        f = base::paste0(f, '+ ', s, ' ')
      }
    }
    res = f
    f = ''
    a = input$spat_rand_interact
    if (base::length(a) %% 2 == 0 && base::length(a) != 0) {
      for (i in base::seq(from = 1, to = base::length(a), by = 2))
      {
        s = base::paste0(a[i], ':', a[i + 1])
        if (flag) {
          f = base::paste0('~', s, ' ')
          flag = F
        }else {
          f = base::paste0(f, '+ ', s, ' ')
        }
      }
      res = base::paste0(res, f)
    }
    shiny::textInput('rv_spat_rand', value = res, label = 'Random equation')
  })

  output$help_fix_blue <- shiny::renderUI({
    a = input$blue_fix
    f = ''
    flag = T
    for (s in a)
    {
      if (flag) {
        f = s
        flag = F
      }else {
        f = base::paste0(f, ' + ', s)
      }
    }
    res = f
    f = ''
    a = input$blue_fix_interact
    if (base::length(a) %% 2 == 0 && base::length(a) != 0) {
      for (i in base::seq(from = 1, to = base::length(a), by = 2))
      {
        s = base::paste0(a[i], ':', a[i + 1])

        if (flag) {
          f = s
          flag = F
        }else {
          f = base::paste0(f, ' + ', s)
        }
      }
      res = base::paste0(res, f)
    }

    shiny::textInput('rv_blue_fix', value = res, label = 'Fixed equation')
  })

  output$help_rand_blue <- shiny::renderUI({
    a = input$blue_rand
    f = ''
    flag = T
    for (s in a)
    {
      if (flag) {
        f = base::paste0('(1|', s, ')')
        flag = F
      }else {
        f = base::paste0(f, ' + (1|', s, ')')
      }
    }
    res = f
    f = ''
    a = input$blue_rand_interact
    if (base::length(a) %% 2 == 0 && base::length(a) != 0) {
      flag = T
      for (i in base::seq(from = 1, to = base::length(a), by = 2))
      {
        s = base::paste0('(1|', a[i], ':', a[i + 1], ')')

        if (flag) {
          f = s
          flag = F
        }else {
          f = base::paste0(f, ' + ', s)
        }
      }

      if (res == '')
        res = f
      else
        res = base::paste0(res, ' + ', f)
    }
    shiny::textInput('rv_blue_rand', value = res, label = 'Random equation')
  })

  output$help_fix_outlier <- shiny::renderUI({
    a = input$outlier_fix
    f = ''
    flag = T
    for (s in a)
    {
      if (flag) {
        f = s
        flag = F
      }else {
        f = base::paste0(f, ' + ', s)
      }
    }
    res = f
    f = ''
    a = input$outlier_fix_interact
    if (base::length(a) %% 2 == 0 && base::length(a) != 0) {
      for (i in base::seq(from = 1, to = base::length(a), by = 2))
      {
        s = base::paste0(a[i], ':', a[i + 1])

        if (flag) {
          f = s
          flag = F
        }else {
          f = base::paste0(f, ' + ', s)
        }
      }
      res = base::paste0(res, f)
    }

    shiny::textInput('rv_outlier_fix', value = res, label = 'Fixed equation')
  })

  output$help_rand_outlier <- shiny::renderUI({
    a = input$outlier_rand
    f = ''
    flag = T
    for (s in a)
    {
      if (flag) {
        f = base::paste0('(1|', s, ')')
        flag = F
      }else {
        f = base::paste0(f, ' + (1|', s, ')')
      }
    }
    res = f
    f = ''
    a = input$outlier_rand_interact
    if (base::length(a) %% 2 == 0 && base::length(a) != 0) {
      for (i in base::seq(from = 1, to = base::length(a), by = 2))
      {
        s = base::paste0('(1|', a[i], ':', a[i + 1], ')')
        if (flag) {
          f = base::paste0('~', s)
          flag = F
        }else {
          f = base::paste0(f, ' + ', s)
        }
      }
      res = base::paste0(res, f)
    }
    shiny::textInput('rv_outlier_rand', value = res, label = 'Random equation')
  })

  output$help_her <- shiny::renderUI({
    a = input$her_rand
    f = ''
    flag = T
    for (s in a)
    {
      if (flag) {
        f = base::paste0('(1|', s, ')')
        flag = F
      }else {
        f = base::paste0(f, ' + (1|', s, ')')
      }
    }

    a = input$her_rand_interact
    if (base::length(a) %% 2 == 0 && base::length(a) != 0) {
      for (i in base::seq(from = 1, to = base::length(a), by = 2))
      {
        s = base::paste0('(1|', a[i], ':', a[i + 1], ')')
        if (f == '')
          f = s
        else
          f = base::paste0(f, ' + ', s)
      }
    }
    shiny::textInput('rv_her', value = f, label = 'Formula')
  })

  shiny::observeEvent(input$indep_her_btn, {
    if (input$rv_her != '') {
      shiny::removeModal()
      if (base::dir.exists(app_sys("app/Results/Heritability")))
        base::unlink(app_sys("app/Results/Heritability"), recursive = TRUE)
      waiter$show()
      base::tryCatch({
        Heritability(input, rv)
      }, error = function(e) {
        if (rv$Show_Errors)
          shiny_showNotification(rv, e$message)
        else
          shiny_showNotification(rv, 'Incorrect arguments, please review the data!')
        # base::setwd("../../")
      })
      waiter$hide()
      show_slider('Heritability')
    }
  })

  shiny::observeEvent(input$indep_blup_btn, {
    if (input$rv_her == '')
      shiny_showNotification(rv, 'Please select variable(s) first!')
    else if (base::length(input$blup_cof) > 1)
      shiny_showNotification(rv, 'You can only select one co-factor!')
    else {
      shiny::removeModal()
      if (base::dir.exists(app_sys("app/Results/blup")))
        base::unlink(app_sys("app/Results/blup"), recursive = TRUE)
      waiter$show()
      base::tryCatch({
        BLUP(input, rv)
      }, error = function(e) {
        if (rv$Show_Errors)
          shiny_showNotification(rv, e$message)
        else
          shiny_showNotification(rv, 'Incorrect arguments, please review the data!')
        # base::setwd("../../")
      })
      waiter$hide()
      show_slider('Blup')
    }
  })


  shiny::observeEvent(input$indep_cor_btn, {
    if ((input$cor_opt == 'Intra correlation' && base::length(input$indep_cor) == 0) |
      (input$cor_opt == 'Intra correlation' && base::length(input$dep_cor) != 2)) {
      shiny_showNotification(rv, 'Please selecet two dependent variables and at least one independent variable')
    }else {
      if (base::dir.exists(app_sys("app/Results/Correlation")))
        base::unlink(app_sys("app/Results/Correlation"), recursive = TRUE)
      shiny::removeModal()
      waiter$show()
      base::tryCatch({
        CoReLaTiOnSS(input, rv)
      }, error = function(e) {
        if (rv$Show_Errors)
          shiny_showNotification(rv, e$message)
        else
          shiny_showNotification(rv, 'Incorrect arguments, please review the data!')
        # base::setwd("../../")
      })
      waiter$hide()
      show_slider("Correlation")
    }
  })

  shiny::observeEvent(input$density_modal_btn, {
    if (!base::is.null(input$density_vars))
    {
      shiny::removeModal()
      if (base::dir.exists(app_sys("app/Results/Density Plots")))
        base::unlink(app_sys("app/Results/Density Plots"), recursive = TRUE)
      waiter$show()
      base::tryCatch({
        DensityPlot(input, rv)
      }, error = function(e) {
        if (rv$Show_Errors)
          shiny_showNotification(rv, e$message)
        else
          shiny_showNotification(rv, 'Incorrect arguments, please review the data!')
        # base::setwd("../../")
      })
      waiter$hide()
      show_slider("Density Plots")
    }else {
      session$sendCustomMessage(
        type = 'testmessage',
        message = 'At least one variable need to be selected')
    }
  })

  shiny::observeEvent(input$boxplot_modal_btn, {
    if (base::dir.exists(app_sys("app/Results/Data Visualization")))
      base::unlink(app_sys("app/Results/Data Visualization"), recursive = TRUE)
    if ('boxplot' %in% input$plots_name) {
      if (!base::is.null(input$boxplot_vars)) {
        shiny::removeModal()
        waiter$show()
        base::tryCatch({
          CheckBOXVIO(input, rv)
        }, error = function(e) {
          if (rv$Show_Errors)
            shiny_showNotification(rv, e$message)
          else
            shiny_showNotification(rv, 'Incorrect arguments, please review the data!')
          # base::setwd("../../")
        })
        waiter$hide()
      }else
        shiny_showNotification(rv, 'At least one independent variable need to be selected')
    }
    if ('densityplot' %in% input$plots_name) {
      if (!base::is.null(input$boxplot_vars))
      {
        shiny::removeModal()
        waiter$show()
        base::tryCatch({
          DensityPlot(input, rv)
        }, error = function(e) {
          if (rv$Show_Errors)
            shiny_showNotification(rv, e$message)
          else
            shiny_showNotification(rv, 'Incorrect arguments, please review the data!')
          # base::setwd("../../")
        })
        waiter$hide()
      }else
        shiny_showNotification(rv, 'At least one independent variable need to be selected')
    }
    if ('violinplot' %in% input$plots_name) {
      if (!base::is.null(input$boxplot_vars)) {
        shiny::removeModal()
        waiter$show()
        base::tryCatch({
          CheckVIO(input, rv)
        }, error = function(e) {
          if (rv$Show_Errors)
            shiny_showNotification(rv, e$message)
          else
            shiny_showNotification(rv, 'Incorrect arguments, please review the data!')
          # base::setwd("../../")
        })
        waiter$hide()
      }else
        shiny_showNotification(rv, 'At least one independent variable need to be selected')
    }
    if ('scatterplot' %in% input$plots_name) {
      if (base::length(input$scatter_vars) == 2) {
        shiny::removeModal()
        waiter$show()
        base::tryCatch({
          CheckSatterplot(input, rv)
        }, error = function(e) {
          if (rv$Show_Errors)
            shiny_showNotification(rv, e$message)
          else
            shiny_showNotification(rv, 'Incorrect arguments, please review the data!')
          # base::setwd("../../")
          waiter$hide()
        })
        waiter$hide()
      }else
        shiny_showNotification(rv, 'Two dependent/response variables are required for creating Scatterplot')
    }
    if (base::is.null(input$plots_name)) {
      shiny_showNotification(rv, 'Please select at least one option!')
    }
    show_slider("Data Visualization")
  })

  shiny::observeEvent(input$run_outlier, {
    flag = T
    if (input$outlier_method == 'B') {
      if (is.null(input$outlier_rand)) {
        flag = F
      }
    }
    if (flag) {
      shiny::removeModal()
      outliers = NULL
      outliers_row = NULL
      if (base::dir.exists(app_sys("app/Results/Outlier")))
        base::unlink(app_sys("app/Results/Outlier"), recursive = TRUE)
      waiter$show()
      base::tryCatch({
        PoSiBlEoUtLieR(input, rv)
        if (base::length(rv$outliers_row) > 0) {
          session$sendCustomMessage(
            type = 'testmessage',
            message = base::paste0(base::length(rv$outliers_row),
                                   ' Outlier(s) are founded!'))

          outl_rows = base::unique(base::sort(rv$outliers_row))
          output$content_3 <- shiny::renderUI({
            if (input$active_opt_2 == 'outlier')
              if (base::length(base::unique(base::sort(rv$outliers_row)))) {
                shiny::tagList(
                  shiny::actionButton('show_pic_outlier', "See Plots"),

                  shiny::actionButton('ref_outlier', "Auto Refine Outliers"),

                  shiny::selectInput(
                    'outl_select_input',
                    label = shiny::h5("Select Outlier"),
                    choices = base::unique(base::sort(rv$outliers_row))
                  ),
                  shiny::selectInput(
                    inputId = 'indep_outlier',
                    label = 'Select the independent variable',
                    choices = input$main_db_indep_val
                  ),
                  shiny::actionButton('filter_outlier', "Filter The Dataset")
                )
              }
          })
        }
        else {
          session$sendCustomMessage(type = 'testmessage',
                                    message = 'Congratulations! There is no outlier')
        }
      }, error = function(e) {
        if (rv$Show_Errors)
          shiny_showNotification(rv, e$message)
        else
          shiny_showNotification(rv, 'Incorrect arguments, please review the data!')
        # base::setwd("../../")
      })
      waiter$hide()
    }
  })

  shiny::observeEvent(input$show_pic_outlier, {
    show_slider("Outlier")
  })

  shiny::observeEvent(input$interacted_name, {
    shiny::updateTextInput(
      inputId = 'interacted_name',
      label = 'New column name',
      value = check_name(input$interacted_name, T))
  })

  shiny::observeEvent(input$new_col_name, {
    shiny::updateTextInput(
      inputId = 'new_col_name',
      label = 'New Name:',
      value = check_name(input$new_col_name, T))
  })

  shiny::observeEvent(input$project_name, {
    shiny::updateTextInput(
      inputId = 'project_name',
      label = "Project Name",
      value = check_name(input$project_name, T))
  })

  shiny::observeEvent(input$create_db, {
    tryCatch({
      if (input$use_sampledb) {
        shiny::removeModal()
        # dat <- rio::import("https://github.com/MohsenYN/AllInOne/blob/main/inst/app/SampleDB/SampleDB.xlsx?raw=true")
        dat <- readxl::read_xlsx(app_sys('app/SampleDB/SampleDB.xlsx'), sheet = 1)
        rv$data <- base::as.data.frame(dat)
        allinone_initialize(rv)
      }
      else if (!is.null(rv$dataC)) {
        flag = T
        for (col in base::c(base::colnames(rv$dataC), input$project_name)) {
          if (check_name(col) == '') {
            flag = F
            break
          }
        }
        if (flag) {
          shiny::removeModal()
          rv$data <- base::as.data.frame(rv$dataC)
          allinone_initialize(rv)

        }else {
          shiny_showNotification(rv, 'Column/Project name can not include " \ | ? * : < > () and space')
        }
        base::rm(flag)
      }
    }, error = function(e) {
      shiny_showNotification(rv, e$message)
    })
  })

  output$summary <- renderUI({
    if (!is.null(rv$data)) {
      s = base::summary(rv$data)
      write.csv(s, 'summary.csv', row.names = F)
      s = read.csv('summary.csv',
                   header = T
      )
      unlink('summary.csv')
      colnames(s) = colnames(rv$data)
      DT::renderDataTable(
        s,
        rownames = F,
        options = base::list(
          scrollX = TRUE,
          scrollCollapse = TRUE,
          selection = 'none',
          dom = 'lti'
        )
      )
    }
  })

  get_col_type <- function(col) {
    if (!is.null(col)) {
      if (is.character(rv$data[[col]]))
        return(1)
      else if (is.factor(rv$data[[col]]))
        return(3)
      else if (is.numeric(rv$data[[col]]))
        return(2)
      else
        return(4)
    }
  }

  observeEvent(input$structure_change_type, {
    if (input$str_column_type == 1)
      rv$data[[input$str_column_name]] = as.character(rv$data[[input$str_column_name]])
    else if (input$str_column_type == 2)
      rv$data[[input$str_column_name]] = as.numeric(rv$data[[input$str_column_name]])
    else if (input$str_column_type == 3)
      rv$data[[input$str_column_name]] = as.factor(rv$data[[input$str_column_name]])
  })

  output$structure <- renderUI({
    if (!is.null(rv$data)) {
      shiny::tagList(
        shiny::HTML(paste0('<br/>', c(' ', ' ', ' ', ' ', capture.output(str(rv$data)))))
      )
    }
  })

  output$o_structure_col_name <- renderUI({
    if (!is.null(rv$data))
      shiny::column(
        width = 5,
        shiny::selectInput(
          inputId = 'str_column_name',
          label = 'Column name',
          choices = base::colnames(rv$data)
        )
      )
  })

  output$o_structure_col_type <- renderUI({
    if (!is.null(rv$data))
      shiny::column(
        width = 5,
        shiny::selectInput(
          inputId = 'str_column_type',
          label = 'Set structure',
          choices = list(
            'Character' = 1,
            'Numeric' = 2,
            'Factor' = 3,
            'Undefined' = 4
          ),
          selected = get_col_type(input$str_column_name))
      )

  })

  output$o_structure_col_btn <- renderUI({
    if (!is.null(rv$data))
      shiny::column(
        width = 2,
        class = "structure_change_type_col",
        shiny::actionButton('structure_change_type', 'Apply')
      )
  })

  shiny::observeEvent(input$data_structure, {
    shiny::showModal(shiny::modalDialog(
      shiny::HTML((paste0(capture.output(str(rv$data)), '<br/><br/>'))),
      easyClose = T, title = 'Structure of your data'
    ))
  })

  shiny::observeEvent(input$interaction_btn_apply, {
    #check interaction column
    ath_flag = T
    interacted_name = input$interacted_name
    str = base::strsplit(interacted_name, '')[[1]]
    for (s in str) {
      if (s %in% forbidden_characters) {
        session$sendCustomMessage(
          type = 'testmessage',
          message = 'Column names can not include " \ | ? * : < > () and space')
        ath_flag = F
        break
      }
    }

    if (base::length(input$main_db_interaction_col) > 1 & ath_flag) {
      shiny::removeModal()
      waiter$show()
      if (interacted_name == '')
        interacted_name = base::paste0('Interacted_Column', base::length(base::colnames(rv$data)) + 1)

      temp = rv$data %>% dplyr::select(dplyr::all_of(input$main_db_interaction_col))

      temp = rv$data %>% tibble::add_column(add_Interacted_Column_Allinone = 1:base::length(temp[, 1]))

      for (i in base::seq(1:base::length(temp[, 1]))) {
        temp[i, 'add_Interacted_Column_Allinone'] = ""
        counter = 0
        for (k in input$main_db_interaction_col) {
          counter = counter + 1
          j = base::which(base::names(rv$data) == dplyr::all_of(k))
          temp[i, 'add_Interacted_Column_Allinone'] = base::paste0(temp[i, 'add_Interacted_Column_Allinone'], base::ifelse(counter == 1, '', '-'), rv$data[i, j])
        }
      }
      base::colnames(temp)[base::which(base::names(temp) == dplyr::all_of('add_Interacted_Column_Allinone'))] <-
        interacted_name
      rv$data = temp
      new_main_db_indep_val <- input$main_db_indep_val
      new_main_db_indep_val <- base::append(new_main_db_indep_val, interacted_name)
      waiter$hide()
      session$sendCustomMessage(
        type = 'testmessage',
        message = 'Interaction is done! Please review independent variables.')

      shiny::showModal(shiny::modalDialog(
        shiny::checkboxGroupInput(
          inputId = 'main_db_indep_val',
          label = 'Selected Independent/Factor Variables',
          choices = base::names(dplyr::select(rv$data, dplyr::all_of(base::c(input$main_db_indep_val, interacted_name)))),
          selected = new_main_db_indep_val
        ), easyClose = FALSE,
        footer = shiny::actionButton('apply_db', "Apply"))
      )
    } else if (ath_flag) {
      session$sendCustomMessage(
        type = 'testmessage',
        message = 'Please select at least two independent variables to interact!')
    }

  })

  shiny::observeEvent(input$apply_db, {
    if (base::length(base::c(input$main_db_indep_val, input$main_db_dep_val)) != base::length(base::unique(base::c(input$main_db_indep_val, input$main_db_dep_val)))) {
      shiny_showNotification(
        rv,
        'It is not possible to select a variable as dependent and independent, simultaneously'
      )
    }else if (base::length(input$main_db_indep_val) == 0) {
      shiny_showNotification(
        rv,
        'At least one independent variable should be selected'
      )
    }else if (base::length(input$main_db_dep_val) == 0) {
      shiny_showNotification(
        rv,
        'At least one dependent/response variable should be selected'
      )
    }else {
      shiny::removeModal()
      # include Independent variables
      rv$VarPYSL <-
        rv$data %>% dplyr::select(dplyr::all_of(input$main_db_indep_val))

      # include Dependent variables
      rv$SelectedTraits <-
        rv$data %>% dplyr::select(dplyr::all_of(input$main_db_dep_val))

      # include Independent variables TOO
      rv$independent_variables <-
        rv$data %>% dplyr::select(input$main_db_indep_val)

      # include Dependent variables TOO
      rv$dependent_variables <-
        rv$data %>% dplyr::select(input$main_db_dep_val)
    }
  })

  shiny::observeEvent(input$show_res_outlier, {
    if (!base::is.null(rv$outliers_row)) {
      outl_rows = base::unique(base::sort(rv$outliers_row))
    }
    else {
      shiny::showModal(shiny::modalDialog(
        title = "Results",
        footer = shiny::modalButton("Return"),
        shiny::h1("There is no outlier!"),
        easyClose = FALSE))
    }
  })

  shiny::observeEvent(input$filter_outlier_reset, {
    rv$selected.col = NULL
    for (i in 1:base::length(rv$filter.k)) {
      rv$filter.k[[i]]$search = 'k'
      rv$filter.k[[i]]$search = ''
    }
  })

  shiny::observeEvent(input$filter_outlier, {
    for (i in 1:base::length(rv$filter.k)) {
      rv$filter.k[[i]]$search <- ''
    }
    rv$selected.col <- input$indep_outlier
    row_num = input$outl_select_input
    val_filter = rv$data[input$outl_select_input, input$indep_outlier]
    col_num = base::which(base::colnames(rv$data) == input$indep_outlier)
    if (base::is.numeric(rv$data[, input$indep_outlier])) {
      val_filter = base::paste0(val_filter, ' ... ', val_filter)
    } else {
      val_filter = base::paste0('^', val_filter, '$')
    }
    rv$filter.k[[col_num + 1]]$search = ''
    rv$filter.k[[col_num + 1]]$search = val_filter
  })

  shiny::observeEvent(input$ref_outlier, {
    if (!base::is.null(rv$outliers_row)) {
      waiter$show()
      o_col <- NULL
      o_row <- NULL
      len = base::length(rv$outliers)

      for (i in rv$outliers_row) {
        for (x in base::seq(2, len, 3)) {
          if (rv$outliers[x] == i) {
            c = rv$outliers[x - 1]
            r = rv$outliers[x]
            db.edit(r, c, '', 'string')
          }
        }
      }
      rv$outliers_row = rv$outliers = NULL

      shiny_showNotification(rv, ' Outliers are refined!')

      if (base::dir.exists(app_sys("app/Results/Refine")))
        base::unlink(app_sys("app/Results/Refine"), recursive = TRUE)

      base::tryCatch({
        Refine(input, rv)
      }, error = function(e) {
        if (rv$Show_Errors)
          shiny_showNotification(rv, e$message)
        else
          shiny_showNotification(rv, 'Incorrect arguments, please review the dataset!')
        # base::setwd("../../")
      })
      waiter$hide()
      show_slider("Refine")
    }
    else {
      shiny::showModal(shiny::modalDialog(
        title = "Results",
        footer = shiny::modalButton("Return"),
        shiny::h1("There is no outlier!"),
        easyClose = FALSE))
    }
  })

  observeEvent(input$active_opt_db, {
    base::options(shiny.maxRequestSize = 50 * 1024^2)
    shiny::showModal(shiny::modalDialog(
      shiny::fileInput('file', 'Upload Dataset File :'),
      shiny::checkboxInput('use_sampledb', "Sample Dataset", value = F),
      shiny::textInput('project_name', "Project Name", value = "Untitled"),
      # shiny::textInput('Results_dir', "Insert a directory for outputs", placeholder = "C:/User/Desktop/Results"),
      shiny::uiOutput('dataC_sheet'),
      shiny::uiOutput('columns_name'),
      shiny::uiOutput('column_new_name'),
      # shiny::uiOutput('column_type'),
      shiny::uiOutput('columns_name_btn'),
      footer = shiny::tagList(shiny::actionButton('create_db', 'Apply Information'),
                              shiny::modalButton('Dismiss')),
      easyClose = FALSE
    ))
  })

  observeEvent(input$active_opt_ind_var, {
    if (base::is.null(rv$data)) {
      session$sendCustomMessage(type = 'testmessage',
                                message = "Please select the main detaset first !")
    } else {
      temp_indep = input$main_db_indep_val
      if (rv$Show_Errors)if (base::is.null(temp_indep)) {
        temp_indep = base::c(
          'T',
          'Treatment',
          'Column',
          'Rows',
          'Genotype',
          'Province',
          'Site',
          'Year',
          'Line',
          'MultiYear',
          'Location',
          'Rep',
          'Row',
          'Col',
          'Entry',
          'Plot')
      }
      temp_dep = input$main_db_dep_val
      if (rv$Show_Errors)if (base::is.null(temp_dep)) {
        temp_dep = base::c(
          'Yield',
          'Oil',
          'Maturity',
          'Seedweight',
          'Height',
          'Protein',
          'Disease')
      }
      coln = base::colnames(rv$data)
      cls = ''
      if (base::length(coln) > 10)
        cls = 'multicol'
      shiny::showModal(
        shiny::modalDialog(
          shiny::tags$div(align = 'left',
                          class = cls,
                          shiny::checkboxGroupInput(
                            inputId = 'main_db_indep_val',
                            label = 'Select independent/factor variables',
                            choices = coln,
                            selected = temp_indep
                          )),
          shiny::tags$div(align = 'left',
                          class = cls,
                          shiny::checkboxGroupInput(
                            inputId = 'main_db_dep_val',
                            label = 'Select dependent/response variables',
                            choices = coln,
                            selected = temp_dep
                          )),
          easyClose = FALSE,
          footer = shiny::actionButton('apply_db', 'OK')
        )
      )
      rv$review_flag = FALSE
    }
  })

  observeEvent(input$active_opt_interaction, {
    if (base::is.null(rv$data)) {

      session$sendCustomMessage(type = 'testmessage',
                                message = "Please select the main detaset first !")

    } else if (rv$review_flag) {
      session$sendCustomMessage(type = 'testmessage',
                                message = "Please select variables !")
    }else {
      defult_name = base::paste0('Interacted_Column_', base::sample(1:99, 1))
      # Check if this name already exists
      shiny::showModal(
        shiny::modalDialog(
          shiny::checkboxGroupInput(
            inputId = 'main_db_interaction_col',
            label = 'Select variables to interact',
            choices = base::names(rv$data),
          ),
          shiny::textInput(
            'interacted_name',
            'New column name',
            defult_name
          ),
          footer =
            shiny::tagList(shiny::actionButton('interaction_btn_apply', 'interact'),
                           shiny::modalButton('Dismiss')),
          easyClose = FALSE
        )
      )
    }
  })

  observeEvent(input$active_opt_subset, {
    if (base::is.null(rv$data)) {

      session$sendCustomMessage(type = 'testmessage',
                                message = "Please select the main detaset first !")

    } else if (rv$review_flag) {
      session$sendCustomMessage(type = 'testmessage',
                                message = "Please select variables !")
    } else {
      shiny::showModal(
        shiny::modalDialog(
          shiny::selectInput(
            inputId = 'subset_indep',
            label = 'Select an independent variable to subset',
            choices = input$main_db_indep_val,
          ),
          shiny::uiOutput('subset_levels'),
          easyClose = FALSE,
          footer = {
            shiny::tagList(
              shiny::actionButton('subset_btn', 'Subset'),
              shiny::modalButton('Dismiss'))
          }
        )
      )
      rv$active_opt_ = 'db'
    }
  })

  output$content_save_db <- shiny::renderUI({
    flag = F
    for (i in 1:base::length(rv$filter.k)) {
      if (rv$filter.k[[i]]$search != '')
        flag = T
    }
    shiny::tagList(
      column(width = 3, shiny::actionButton('active_opt_db', 'Upload Dataset')),
      column(width = 3, shiny::actionButton('active_opt_ind_var', 'Select Variables')),
      column(width = 3, shiny::actionButton('active_opt_interaction', 'Create Interactions')),
      column(width = 3, shiny::actionButton('active_opt_subset', 'Subset Dataset')),
      column(width = 3, shiny::downloadButton('download_db', 'Save As')),
      if (flag)
        column(width = 3, shiny::actionButton('filter_outlier_reset', "Revert filters!!!",
                                              style = 'color : black; background-color : var(--secondary-color)'))
      # ,column(width = 3,shiny::actionButton('debug', "debug"))
    )
  })

  output$download_db <- shiny::downloadHandler(
    filename = function() {
      base::paste0(input$project_name, "__", base::Sys.Date(), ".csv")
    },
    contentType = "text/csv",
    content = function(path) {
      utils::write.csv(rv$data, path, row.names = F)
    }
  )

  output$save_setting <- shiny::downloadHandler(
    filename = function() {
      'My_setting.csv'
    },
    contentType = "text/csv",
    content = function(path) {

      notif_delay = input$notif_delay
      notif_size = input$notif_size
      Ign_Res_Wrd = input$Ign_Res_Wrd

      #####################################

      Max_levels_GB = input$Max_levels_GB

      l = list()

      l[['General']][1] = notif_delay
      l[['General']][2] = notif_size
      l[['General']][3] = Ign_Res_Wrd
      l[['Plots']][1] = Max_levels_GB

      utils::write.csv(l, path, row.names = F)
    }
  )

  output$download_pdf <- shiny::downloadHandler(

    filename = function() {
      base::basename(rv$pdf_address)
    },
    contentType = "pdf",
    content = function(path) {
      base::file.copy(rv$pdf_address, path, overwrite = TRUE)
    }
  )

  output$download_pdf_res <- shiny::downloadHandler(

    filename = function() {
      base::basename(rv$pdf_address)
    },
    contentType = "pdf",
    content = function(path) {
      base::file.copy(rv$pdf_address, path, overwrite = TRUE)
    }
  )

  output$download_png <- shiny::downloadHandler(

    filename = function() {
      base::basename(rv$png_address)
    },
    contentType = "png",
    content = function(path) {
      base::file.copy(rv$png_address, path, overwrite = TRUE)
    }
  )

  output$download_png_res <- shiny::downloadHandler(

    filename = function() {
      base::basename(rv$png_address)
    },
    contentType = "png",
    content = function(path) {
      base::file.copy(rv$png_address, path, overwrite = TRUE)
    }
  )

  output$download_csv <- shiny::downloadHandler(

    filename = function() {
      base::basename(rv$csv_address)
    },
    contentType = "csv",
    content = function(path) {
      base::file.copy(rv$csv_address, path, overwrite = TRUE)
    }
  )

  output$download_csv_res <- shiny::downloadHandler(

    filename = function() {
      base::basename(rv$csv_address)
    },
    contentType = "csv",
    content = function(path) {
      base::file.copy(rv$csv_address, path, overwrite = TRUE)
    }
  )

  shiny::observeEvent(input$table1_cell_edit, {

    temp_r = input$table1_cell_edit$row

    temp_c = input$table1_cell_edit$col

    temp_v = input$table1_cell_edit$value

    db.edit(temp_r, temp_c, temp_v)

  })

  output$save_as <- shiny::renderUI({
    if (!base::is.null(rv$data))
      shiny::actionButton("save_db", "Save as the database")
  })

  output$table1 <- DT::renderDT({
    tryCatch({
      k = rv$selected.col
      k = base::which(base::colnames(rv$data) == k)
      DT::datatable(
        rv$data,
        editable = TRUE,
        selection = base::list(
          mode = "multiple",
          selected = base::list(rows = rv$outliers_row, cols = base::as.numeric(k)),
          target = 'row+column'
        ),
        filter = base::list(position = 'top', plain = TRUE),
        options = base::list(
          scrollX = TRUE,
          search = base::list(regex = TRUE),
          lengthChange = T,
          dom = 'ltip',
          searchCols = rv$filter.k
        )
      )
    }, error = function(e) {
      shiny_showNotification(rv, e$message)
    })
  })

  shiny::observeEvent(input$OTL_apply_changes, {

    rv$outliers_row <-
      base::subset(
        rv$outliers_row,
        rv$outliers_row != input$outl_select_input
      )

    COLN = base::colnames(rv$data)
    for (i in COLN) {

      if (!base::is.null(input[[base::paste0("OTL_", i)]])) {
        db.edit(input$outl_select_input, i, input[[base::paste0("OTL_", i)]], col_type = 'name')
      }
    }
  })

  shiny::observeEvent(input$outl_select_input, {

    if (input$outlier_method == 'A') {
      k = input$outl_select_input
      COLN = base::colnames(rv$data)
      o_cln <- NULL
      len = base::length(rv$outliers)
      for (x in base::seq(2, len, 3)) {
        if (base::as.numeric(rv$outliers[x]) == k) {
          o_cln <- base::append(o_cln, rv$outliers[x - 1])
        }
      }

      args1 = ''
      for (i in COLN) {
        if (i %in% o_cln) {
          str_temp = base::paste0('<b><h4>', i, ' (', input$indep_outlier_2, ')</h4></b>')
          args1 = base::append(args1, base::list(shiny::textInput(
            base::paste0("OTL_", i),
            label = shiny::HTML(str_temp),
            value = rv$data[k, base::paste0(i)]
          )))
        }else {
          if (!is.null(input[[base::paste0("OTL_", i)]])) {
            str_temp = base::paste0('<h4>', i, ' (', input$indep_outlier_2, ')</h4>')
            args1 = base::append(args1, base::list(shiny::textInput(
              base::paste0("OTL_", i),
              label = shiny::HTML(str_temp),
              value = rv$data[k, base::paste0(i)]
            )))
          }
        }
      }
      args1 = base::append(args1, base::list(shiny::actionButton("OTL_apply_changes", "Apply Change(s) / Ignore")))
      output$content_7 <- shiny::renderUI({
        if (input$active_opt_2 == 'outlier')
          if (base::length(base::unique(base::sort(rv$outliers_row))))
            shiny::tagList(args1)
      })
    }else {
      k = base::as.numeric(input$outl_select_input)

      output$content_7 <- shiny::renderUI({
        if (input$active_opt_2 == 'outlier')
          if (base::length(base::unique(base::sort(rv$outliers_row))))
            shiny::tagList(
              shiny::textInput(base::paste0("OTL_", input$outlier_resp),
                               label = input$outlier_resp,
                               value = rv$data[[input$outlier_resp]][k]
              ),
              shiny::actionButton("OTL_apply_changes", "Apply Change(s) / Ignore")
            )
      })
    }
  })

  shiny::observeEvent(input$outl_select_input, {
    k = input$outl_select_input
    COLN = input$main_db_dep_val
    o_cln <- NULL
    len = base::length(rv$outliers)
    for (x in base::seq(2, len, 3)) {
      if (base::as.numeric(rv$outliers[x]) == k) {
        o_cln <- base::append(o_cln, rv$outliers[x - 1])
      }
    }

    for (i in COLN)
      if (!(i %in% o_cln))
        shiny::removeUI(selector = paste0("div:has(> #", base::paste0("OTL_", i), ")"))
  })

  shiny::observeEvent(input$csvs_use, {

    allinone_initialize(rv)
    # Delete residualValue from data
    # Note that the first column of a dataset should not be ResidualValue
    if (base::colnames(rv$csv_value)[1] == 'ResidualValue')
      rv$csv_value[[1]] = NULL

    rv$data = rv$csv_value

    temp_indep = input$main_db_indep_val
    temp_dep = input$main_db_dep_val

    shiny::showModal(
      shiny::modalDialog(
        shiny::checkboxGroupInput(
          inputId = 'main_db_indep_val',
          label = 'Select independent/factor variables',
          choices = base::names(rv$data),
          selected = temp_indep
        ),
        shiny::checkboxGroupInput(
          inputId = 'main_db_dep_val',
          label = 'Select dependent/response variables',
          choices = base::names(rv$data),
          selected = temp_dep
        ),
        easyClose = FALSE,
        footer = shiny::actionButton('apply_db', 'OK')
      )
    )
    rv$review_flag = FALSE
  })
}
