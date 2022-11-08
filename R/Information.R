#' The User Interface of home page
#'
#' @noRd
#'
information_ui <- shiny::column(
  12,
  shiny::tags$body(
    shiny::h2(shiny::strong('Welcome to the AllInOne')),
    shiny::h3(shiny::strong('An Open-Source Breeder-Friendly Analytical Package for Pre-processing Phenotypic Data')),
    shiny::h4(shiny::strong('Version 1.0.2')),
    shiny::hr(),
    shiny::tags$blockquote(
      shiny::img(src = 'www/Picture.png', align = "center", width = "100%", height = "100%")
    ),
    shiny::hr(),
    #br(),
    #h3(strong('Data Visualization in interactive way')),
    #br(),
    shiny::tags$blockquote(
      shiny::tags$div(shiny::tags$ul(
        shiny::tags$li("Interactive Environment for Pre-Processing Breeding Datasets."),
        shiny::tags$li("Detecting Missing Pattern Based on Independant Variables."),
        shiny::tags$li("Imputing Missing Data Using the Power of MICE Package."),
        shiny::tags$li("Phenotypic Data Visualization: Interactive Histograms, Density Plots, Box Plots, and Scatter Plots."),
        shiny::tags$li("Independant variable- Based Outlier Detection Using Quantile and Cook's Distance Methods."),
        shiny::tags$li("Independant variable- Based Correlation Analysis."),
        shiny::tags$li("Inter Correlation of a Dependant Variable based on an Independant Variable."),
        shiny::tags$li("Intra Correlation based on Two Dependant Variables and Several Independant Variables."),
        shiny::tags$li("Normalizing Data Using the Power of bestNormalize Package."),
        shiny::tags$li("Spatial Analysis Using Two-Dimensional Penalized Spline Models (SpATS package)."),
        shiny::tags$li("Calculating of Best Linear Unbiased Estimators (BLUE)."),
        shiny::tags$li("Calculating of Best Linear Unbiased Prediction (BLUP)."),
        shiny::tags$li("Calculating Heritability and Variance Proportion."),
        shiny::tags$li("Interactive Environment for Creating and Subsetting Variables at Different Levels."))),
      style = "font-size: 18px"
    ),
    shiny::hr(),
    shiny::h3(shiny::strong('Information')),
    shiny::tags$blockquote(

      shiny::tags$ol(
        shiny::tags$li("AllInOne is an open-Source, R-Shiny user interface package designed in the Plant Agriculture department at the University of Guelph to provide a broad range of pre-processing analysis features for phenotypic breeding datasets. This app uses different R packages, such as mice, VIM, lme4, bestNormalize, etc., to enable plant breeders to use all the mentioned packages simultaneously in an interactive environment. Furthermore, it allows plant breeders to edit, organize, subset, and sort datasets in a live mode."),
        shiny::br(),
        shiny::tags$li("A wide range of datasets can be easily uploaded in this app. This app now works well with the three most common file formats such as .txt (comma separated), .csv (comma separated), and .xlsx, so there is no need to change the dataset format before using it."),
        shiny::br(),
        shiny::tags$li("Visualization is the most important part of AllInOne. Some of the expected graphs are shown below:"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Missing Pattern:"))),
        shiny::img(src = 'www/MPP.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Missing data imputation:"))),
        shiny::img(src = 'www/MII1.png', align = "center", width = "60%", height = "60%"),
        shiny::img(src = 'www/MII2.png', align = "center", width = "60%", height = "60%"),
        shiny::img(src = 'www/MII3.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Density plot:"))),
        shiny::img(src = 'www/DPP.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Box plot:"))),
        shiny::img(src = 'www/BPP.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Violin plot:"))),
        shiny::img(src = 'www/VPP.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Scatter Plot:"))),
        shiny::img(src = 'www/SCAT.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Detecting Outlier (Quantile):"))),
        shiny::img(src = 'www/DOUQ.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Detecting Outlier (Cook's Distance):"))),
        shiny::img(src = 'www/COQ.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Normalization:"))),
        shiny::img(src = 'www/NO.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("General Correaltion:"))),
        shiny::img(src = 'www/GCORR.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Inter Correaltion:"))),
        shiny::img(src = 'www/INTERCORR.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Intra Correaltion:"))),
        shiny::img(src = 'www/INTRACORR.png', align = "center", width = "60%", height = "60%"),
        shiny::img(src = 'www/INTRACORR2.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Variance Proportion:"))),
        shiny::img(src = 'www/VPOR.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("BLUP/BLUE:"))),
        shiny::img(src = 'www/BLUPE.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Spatial Analysis:"))),
        shiny::img(src = 'www/SPAA.png', align = "center", width = "60%", height = "60%"),
        shiny::hr(),
        shiny::tags$ul(
          shiny::h3(shiny::strong("Heritability:"))),
        shiny::img(src = 'www/HER.png', align = "center", width = "60%", height = "60%"),
        shiny::hr()), style = "font-size: 16px; line-height: 1.7;"),
    shiny::hr(),

    shiny::h3(shiny::strong('Important Notes:')),
    shiny::tags$ol(
      shiny::tags$li('Please make sure that you should leave all the missing data points empty (not filled with "." or "NA", etc.).'),
      shiny::tags$li('Spaces in column names are not allowed (e.g., "Yield (KG)" should be "Yield(KG))".'),
      shiny::br()),
    style = "font-size: 18px"),
  shiny::hr(),
  shiny::h3(shiny::strong('Contact Information and Help:')),
  shiny::tags$blockquote(
    shiny::tags$p(shiny::strong('Main Contact:', style = "font-size: 20px")),
    shiny::tags$p(shiny::strong('Mohsen Yoosefzadeh Najafabadi', style = "font-size: 16px")),
    shiny::tags$p('Research Assocaite', style = "font-size: 14px"),
    shiny::tags$p('Soybean Breeding & Computational Biology - Department of Plant Agriculture', style = "font-size: 14px"),
    shiny::tags$p('University of Guelph', style = "font-size: 14px"),
    shiny::tags$p('Email Address : myoosefz@uoguelph.ca', style = "font-size: 14px"),
    shiny::tags$p(' ', style = "font-size: 14px"),
    shiny::tags$p(shiny::strong('Alternative Contact:', style = "font-size: 20px")),
    shiny::tags$p(shiny::strong('Ali Heidari', style = "font-size: 16px")),
    shiny::tags$p('Master of Science in Bioinformatics', style = "font-size: 14px"),
    shiny::tags$p('University of Science and Culture', style = "font-size: 14px"),
    shiny::tags$p('Email Address : alihdr@stu.usc.ac.ir', style = "font-size: 14px")),
  shiny::hr(),
  shiny::h3(shiny::strong('Let us know:')),
  shiny::tags$blockquote(
    shiny::tags$p("What do you think about AllInOne?", style = "font-size: 16px"),
    shiny::tags$p("Is there anything you particularly like or don't?", style = "font-size: 16px"),
    shiny::tags$p("Any feedback you can provide would be greatly appreciated.", style = "font-size: 16px"),
    style = "font-size: 18px"),
  shiny::hr(),
  shiny::h3(shiny::strong('ENJOY!')),
  shiny::hr()
)