#______________UI__________________
interface <- shiny::fluidPage(
  shinydisconnect::disconnectMessage(
    text = "Your session timed out.",
    refresh = "Reload",
    background = "#00b050",
    colour = "white",
    overlayColour = "grey",
    overlayOpacity = 0.3,
    refreshColour = "brown"
  ),
  waiter::use_waiter(),
  shinyjs::useShinyjs(),
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css?v15")
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
  ) },
  shiny::uiOutput('information')
)

menu_setting <- shinydashboard::menuItem(
  "Setting",
  icon = shiny::icon("wrench", lib = 'glyphicon'),
  tabName = "Setting",

  shiny::numericInput(
    'Max_levels_GB',
    label = tags$span(
      'The maximum level number for groupby:',
      tags$i(
       class = "glyphicon glyphicon-info-sign",
       style = "color: var(--Just-color);",
       title = 'The columns with level number more than this will be ignored for groupby'
      )),
    20,
    5,
    100,
    '100%'
  ),
  shiny::numericInput(
    'notif_delay',
    label = tags$span(
      'Number of seconds to display the alert message:',
      tags$i(
       class = "glyphicon glyphicon-info-sign",
       style = "color: var(--Just-color);",
       title = 'How long the alert message will be appears on the screen'
      )),
    6,
    1,
    60,
    '100%'
  ),
  shiny::selectInput(
    'notif_size',
    label = tags$span(
      'Size of the alert message',
      tags$i(
       class = "glyphicon glyphicon-info-sign",
       style = "color: var(--Just-color);",
       title = 'Size of the alert message which appears on the right-bottom of screen in case of facing error'
      )),
    choices = base::list(
      'Big' = '4',
      'Normal Interactions' = '5',
      'Small' = '6'
    ),
    selected = '5'
  ),
  shiny::checkboxInput(
    'Ign_Res_Wrd',
    label = shiny::tags$span(
      'Ignore reserved characters in column names',
      shiny::tags$i(
       class = "glyphicon glyphicon-info-sign",
       style = "color: var(--Just-color);",
       title = 'Some characters(" \ | ? * : < > and space) are not allowed in the column names.
       By checking this option, we ignore this limitation. however, we highly recommend you not to do this!'
      )),
    T
  )
  ,
  shiny::checkboxInput(
    'Rep_Res_Wrd',
    label = shiny::tags$span(
      'Replace reserved characters in column names',
      shiny::tags$i(
       class = "glyphicon glyphicon-info-sign",
       style = "color: var(--Just-color);",
       title = 'Some characters(" \ | ? * : < > and space) are not allowed in the column names.
       By checking this option, we automatically replace reserved letters by the hyphen.'
      )),
    F
  ),

  shiny::checkboxInput(
    'save_results',
    label = shiny::tags$span(
      'Save all outputs',
      shiny::tags$i(
       class = "glyphicon glyphicon-info-sign",
       style = "color: var(--Just-color);",
       title = 'Some users tend to save all the output.
Therefore we provide them with this option.
Thereby, they can have all the output in a spesific folder.'
      )),
    F
  )
  ,
  shiny::actionButton(
    'results_folder',
    'Select a folder'
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
    menu_setting,
    menu_db,
    menu_operators,
    shiny::uiOutput('content_3'), #Outliers content
    shiny::uiOutput('content_7'), #apply changes outliers
    shiny::a(href="mailto:myoosefz@uoguelph.ca",shiny::img(src="www/Picture3.png", class="MYNHelp"))
  )
})

body <- shinydashboard::dashboardBody(
  shiny::uiOutput("content_save_db"),
  DT::DTOutput("table1"),
  shiny::a(href="https://www.uoguelph.ca/oac/",shiny::img(src="www/OACL.png", class="MYNGuelph")),
  interface
)

Header <- shinydashboard::dashboardHeader(

  title = shiny::a(href="https://github.com/Mohsen1080",
                   shiny::img(src="www/ALL.png", class="title-image"))
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
      skin = "red",# Don't Remove this!
      title="AllInOne"
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
