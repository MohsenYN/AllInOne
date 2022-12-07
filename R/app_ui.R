Results_subfolders <- c('Outlier', 'Missing Values', 'Missing Imputation', 'Data Visualization', 'Correlation',
                        'Normalization', 'Spatial Analysis', 'Mixed Analysis', 'Heritability')

#______________UI__________________
interface <- shiny::fluidPage(
  shinydisconnect::disconnectMessage(
    text = "Hmmm ... Seems your session has timed out. No worries you can always try again :)",
    refresh = "Reload",
    background = "#FFC72A",
    colour = "#000000",
    overlayColour = "grey",
    overlayOpacity = 0.5,
    refreshColour = "brown",
    size = 22,
    width = "full",
    top = "center",
    css = "padding: 0 !important; border: 3px solid #000000;"
  ),
  waiter::use_waiter(),
  shinyjs::useShinyjs(),
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css?v16"),
    shiny::tags$link(rel = "stylesheet", type = "text/css",
                     href = "https://cdn.jsdelivr.net/npm/pretty-checkbox@3.0/dist/pretty-checkbox.min.css")
  ),
{ shiny::tags$head(
  shiny::tags$script(shiny::HTML('Shiny.addCustomMessageHandler("testmessage",
  function(message) {
    alert(message);
  }
);')),
  shiny::tags$style(shiny::HTML("
        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
         h1 {
        font-family: 'Trattatello', fantasy;
        font-weight: 500;
        line-height: 1.1;
       color: #D2691E;
      align = 'center'
      }
      .multicol {
          -webkit-column-count: 2; /* Chrome, Safari, Opera */
          -moz-column-count: 2;    /* Firefox */
          column-count: 2;
          -moz-column-fill: auto;
          -column-fill: auto;
      }
    "))
) }

)

menu_setting <- shiny::tagList(
  shiny::column(
    width = 8,
    shiny::numericInput(
      'notif_delay',
      label = tags$span(
        'Duration (seconds) to display the notifications:',
        tags$i(
          class = "glyphicon glyphicon-info-sign",
          style = "color: var(--Just-color);",
          title = 'How long notifications will appear on the screen?'
        )),
      value = 6,
      min = 1,
      max = 60,
      step = '100%',
      width = '100%'
    )),
  shiny::column(
    width = 8,
    shiny::selectInput(
      inputId = 'notif_size',
      label = tags$span(
        'Notification size',
        tags$i(
          class = "glyphicon glyphicon-info-sign",
          style = "color: var(--Just-color);",
          title = 'Size of the notifications which appears on the right-bottom of the screen in case of facing an error'
        )),
      choices = base::list(
        'Big' = '4',
        'Medium' = '5',
        'Small' = '6'
      ),
      selected = '5',
      width = '100%'
    )),
  shiny::column(
    width = 12,
    shinyWidgets::switchInput(
      inputId = "Ign_Res_Wrd",
      label = shiny::tags$span(
        'Special characters',
        shiny::tags$i(
          class = "glyphicon glyphicon-info-sign",
          style = "color: var(--Just-color);",
          title = 'Some characters(" \ | ? * : < > space, etc) are not allowed in the column names.
       By checking the box, AllInOne ignores the limitation. however, it is highly recommended not to check the box!'
        )),
      value = F,
      width = '100%',
      onLabel = "Allowed",
      offLabel = "Restricted",
    )
  )
)

menu_plots <- shiny::tagList(
  shiny::column(
    width = 8,
    shiny::numericInput(
      'Max_levels_GB',
      label = tags$span(
        'Maximum levels in a factor for creating graph:',
        tags$i(
          class = "glyphicon glyphicon-info-sign",
          style = "color: var(--Just-color);",
          title = 'The independant variable with a level number more than the maximum set will be ignored for creating plots'
        )),
      value = 20,
      min = 5,
      max = 100,
      step = '100%',
      width = '100%'
    )),
  shiny::column(
    width = 8,
    shiny::column(
      width = 8,
      # colourpicker::colourInput(inputId = 'setting_color_picker', label = 'Add color', closeOnClick = T)
      shiny::HTML('
        <div class="form-group shiny-input-container">
          <label class="control-label" id="idd-label" for="setting_color_picker">Add color</label>
          <input id="setting_color_picker" type="color" class="form-control" value="#FFFFFF"/>
        </div>
      ')
    ),
    shiny::column(
      width = 4,
      class = "structure_change_type_col",
      shiny::actionButton('setting_add_color', 'Add this color to the colors list')
    )
  ),
  shiny::column(
    width = 12,
    shiny::uiOutput('o_setting_colors_list')
  )
)

menu_setting_cor <- shiny::tagList(
  shiny::column(
    width = 8,
    shinyWidgets::pickerInput(
      width = '100%',
      inputId = 'setting_cor_plot',
      label = tags$span(
        'Select correlation plot',
        tags$i(
          class = "glyphicon glyphicon-info-sign",
          style = "color: var(--Just-color);",
          title = 'Select what kind of correlation plots do you want to have?')),
      choices = c(
        'Circle' = 'circle',
        'Color' = 'color',
        'Full' = 'full',
        'Hculst' = 'hclust',
        'Lower' = 'lower',
        'Number' = 'number',
        'pie' = 'pie',
        'Upper' = 'upper',
        'upper and hclust (AXIS)' = 'axis',
        'upper and hclust (BR)' = 'br',
        'upper and hclust (BW)' = 'bw',
        'upper and hclust (COLA)' = 'cola',
        'upper and hclust (COLB)' = 'colb',
        'upper and hclust (SIG)' = 'sig',
        'upper and hclust (SIGBLANK)' = 'sigblank'
      ),
      selected = c('circle', 'color', 'full', 'hclust', 'lower', 'number', 'pie', 'upper', 'axis', 'br', 'bw', 'cola', 'colb', 'sig', 'sigblank'),
      multiple = T,
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      )
    )
  )
)

menu_db <- shinydashboard::menuItem(
  "Dataset",
  icon = shiny::icon("download"),
  tabName = "Main_Dataset",
  shiny::selectInput(
    inputId = 'active_opt',
    label = shiny::h6('Would you like to ...'),
    choices = base::list(
      'Upload Dataset' = 'db',
      'Select Variables' = 'ind_var',
      'Create Interactions' = 'interaction',
      'Subset Dataset' = 'subset'
    )
  ),
  shiny::actionButton('opt_list_btn', 'Start')
)

menu_operators <- shinydashboard::menuItem(
  "Data Pre-Processing",
  icon = shiny::icon("pencil"),
  tabName = "Operators",
  shiny::selectInput(
    inputId = 'active_opt_2',
    label = shiny::h6('Pre-Processing Steps'),
    choices = base::list(
      'Outlier Detection' = 'outlier',

      'Missing data handler' = 'missing_handler',

      'Data Visualization' = 'plots',
      'Correlation' = 'correlation',
      'Normalization' = 'normalize',

      'Variance Analysis' = 'Heritability'
    )
  )
  , shiny::actionButton('opt_list_btn_2', "Let's Go")
)
sidebar <- shinydashboard::dashboardSidebar({
  shinydashboard::sidebarMenu(
    # menu_setting,
    # menu_db,
    menu_operators,
    shiny::uiOutput('content_3'), #Outliers content
    shiny::uiOutput('content_7'), #apply changes outliers
    shiny::div(class = "needhelp-spacer"),
    shiny::a(href = "https://github.com/MohsenYN/AllInOne/wiki", shiny::img(src = "www/Picture3.png", class = "MYNHelp"), class = "MYNHelpL")
  )
})

body <- shinydashboard::dashboardBody(
  shiny::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Home",
      shiny::uiOutput('information')
    ),
    shiny::tabPanel(
      title = "Dataset",
      shiny::uiOutput("content_save_db"),
      DT::DTOutput("table1"),
      interface
    ),
    shiny::tabPanel(
      title = "Results",
      shiny::column(width = 4, shiny::selectInput(
        'res_blue_str', 'Results folder',
        c('None', Results_subfolders)
      )),
      shiny::column(width = 8, shiny::uiOutput('o_res_blue_k')),
      shiny::uiOutput('o_results_btns'),
      shiny::uiOutput('o_results')
    ),
    shiny::tabPanel(
      title = shiny::img(src = "www/AllInOneGlance.png", class = "title-image-tab"),
      shiny::tabsetPanel(
        type = "pills",
        shiny::tabPanel(
          title = "Structure",
          shiny::uiOutput("o_structure_col_name"),
          shiny::uiOutput("o_structure_col_type"),
          shiny::uiOutput("o_structure_col_btn"),
          shiny::uiOutput("structure")
        ),
        shiny::tabPanel(
          title = "Summary",
          shiny::uiOutput("summary")
        ),
        shiny::tabPanel(
          title = "Missing",
          shiny::uiOutput("o_sum_missing")
        ),
        shiny::tabPanel(
          title = "Correlation",
          shiny::uiOutput("o_sum_correlation")
        ),
        shiny::tabPanel(
          title = "Outlier",
          shiny::uiOutput("o_sum_outlier")
        ),
        shiny::tabPanel(
          title = "BoxPlot",
          shiny::uiOutput("o_sum_boxplot")
        ),
        shiny::tabPanel(
          title = "Density",
          shiny::uiOutput("o_sum_density")
        ),
        shiny::tabPanel(
          title = "Violin",
          shiny::uiOutput("o_sum_violin")
        )
      )
    ),
    shiny::tabPanel(
      title = 'Setting',
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          title = "General",
          shiny::tabsetPanel(
            type = 'pill',
            tabPanel(
              title = 'Options',
              menu_setting
            ),
            tabPanel(
              title = 'Correlation',
              menu_setting_cor
            )
          )
        ),
        shiny::tabPanel(
          title = "Plots",
          menu_plots
        ),
        shiny::tabPanel(
          title = "Import/Export",
          shiny::column(
            width = 12,
            shiny::fileInput(
              'setting_file',
              'Upload Setting File :'
            )
          ),
          shiny::column(
            width = 12,
            shiny::downloadButton(
              'save_setting',
              'Export Setting')
          )
        )
      )
    )

  ),
  shiny::a(href = "https://www.uoguelph.ca/oac/", shiny::img(src = "www/OACL.png", class = "MYNGuelph"))
)

Header <- shinydashboard::dashboardHeader(

  shiny::tags$li(shiny::a(href = "https://github.com/MohsenYN/AllInOne",
                          shiny::img(src = "www/PictureRM2.png", class = "title-image")), class = 'dropdown title-logo'),
  shinydashboard::dropdownMenuOutput('header_notification')
)


#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      Header,
      sidebar,
      body,
      skin = "red", # Don't Remove this!
      title = "AllInOne"
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  add_resource_path(
    "Results",
    app_sys("app/Results")
  )
  add_resource_path(
    "SampleDB",
    app_sys("app/SampleDB")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AllInOne"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
