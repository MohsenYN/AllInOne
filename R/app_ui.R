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

      .shiny-notification {
             top: calc(100%);
             left: calc(100%);
      }

    "))
  ) },

  {
    shiny::tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }")
  },
  shiny::uiOutput('information')
)

menu_setting <- shinydashboard::menuItem(
  "Setting",
  icon = shiny::icon("wrench", lib = 'glyphicon'),
  tabName = "Setting",
  numericInput(
    'Max_levels_GB',
    'The maximum level number for groupby:',
    20,
    5,
    100,
    '100%'
  ),
  numericInput(
    'notif_delay',
    'Number of seconds to display the alert message',
    8,
    1,
    60,
    '100%'
  ),
  selectInput(
    'notif_size',
    'Size of the alert message',
    choices = base::list(
      'Big' = '4',
      'Normal Interactions' = '5',
      'Small' = '6'
    ),
    selected = '5'
  ),
  checkboxInput(
    'Ign_Res_Wrd',
    'Ignore reserved words in names?',
    T
  )
  ,
  checkboxInput(
    'Rep_Res_Wrd',
    'Replace reserved words in names?',
    F
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
    menu_db,
    menu_operators,
    shiny::uiOutput('content_3'), #Outliers content
    shiny::uiOutput('content_7'), #apply changes outliers
    menu_setting,
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
      skin = "red",
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
