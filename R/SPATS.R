#' Spatial Analysis
#'
#' @description Adjusting a given dataset using  two-dimensional Penalized spline models ( based on SpATS package)
#'
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @noRd
ExSPATS <- function(input, rv) {
  if (!require(SpATS))
    utils::install.packages("SpATS")
  if (!require(viridis))
    utils::install.packages("viridis")
  pkgs = base::rownames(utils::installed.packages())
  if(!('SpATS' %in% pkgs) | !('viridis' %in% pkgs)){
    shiny_showNotification(rv ,'Installing SpATS and/or viridis package(s) failed!')
    return(F)
  }
  set_wd('Spatial Analysis')

  response = input$spat_resp
  NameG = input$spat_gen
  spat_row = input$spat_row
  spat_col = input$spat_col
  Fixedv = input$rv_spat_fix
  Randv = input$rv_spat_rand

  if (!base::is.null(rv$spat_buffer)) {
    if (input$use_spat) {
      data <- rv$spat_buffer %>%
        dplyr::mutate_at(dplyr::vars(input$main_db_indep_val), as.factor)
    }else {
      data <- rv$data %>% dplyr::mutate_at(dplyr::vars(input$main_db_indep_val), as.factor)
    }
  }
  else
    data <- rv$data %>% dplyr::mutate_at(dplyr::vars(input$main_db_indep_val), as.factor)

  ._fixV <- stats::as.formula(base::paste(Fixedv))
  ._RanV <- stats::as.formula(base::paste(Randv))

  ._data <- base::as.data.frame(data)
  ._data[['RowF']] <- base::as.numeric(._data[, input$spat_row])
  ._data[['ColF']] <- base::as.numeric(._data[, input$spat_col])

  Lrow <- base::nlevels(base::as.factor(._data[, input$spat_row]))
  Lcol <- base::nlevels(base::as.factor(._data[, input$spat_col]))
  spat_nseg = base::c(Lcol, Lrow)

  Model <- SpATS::SpATS(response = response,
                 spatial = ~SpATS::PSANOVA(ColF, RowF, nseg = spat_nseg),
                 genotype = NameG,
                 fixed = ._fixV,
                 random = ._RanV,
                 data = ._data)

  ._Dataset <- Model$data

  Data <- base::data.frame(ResidualValue = stats::residuals(Model))

  Data[[NameG]] <- ._Dataset[, Model$model$geno$genotype]
  Data[[input$spat_col]] <- ._Dataset[, Model$terms$spatial$terms.formula$x.coord]
  Data[[input$spat_row]] <- ._Dataset[, Model$terms$spatial$terms.formula$y.coord]
  Data[['FittedValue']] <- stats::fitted.values(Model)
  Data[['RawValue']] <- ._Dataset[, Model$model$response]

  k = base::c(input$main_db_indep_val,
               input$main_db_dep_val)

  for (i in k) {
    Data[[i]] = ._Dataset[[i]]
  }
  rv$spat_buffer = Data
  rv$spat_buffer[[input$spat_resp]] = NULL
  rv$spat_buffer[['RawValue']] = NULL
  rv$spat_buffer[['Genotype']] = NULL

  base::colnames(rv$spat_buffer)[base::which(base::names(rv$spat_buffer) == 'FittedValue')] = input$spat_resp

  cols_b = base::colnames(rv$spat_buffer)
  for (i in base::colnames(rv$data)) {
    if(!(i %in% cols_b))
      rv$spat_buffer[[i]] = ._Dataset[[i]]
  }

  utils::write.csv(rv$spat_buffer,
            file = base::paste0(
              input$project_name,
              " -- Spatial analysis of ", response, ".csv"), row.names = FALSE)


  ._Parameter <- function(M) {

    ._responseV <- M$data[, M$model$response]
    ._responseV
    ._mresponse <- base::mean(._responseV, na.rm = TRUE)
    ._fitted <- M$fitted
    MSE <- base::mean((._responseV - ._fitted)^2, na.rm = TRUE)
    base::names(MSE) <- "MSE"
    RMSE <- sqrt(MSE)
    base::names(RMSE) <- "RMSE"
    NRMSE <- RMSE / ._mresponse
    base::names(NRMSE) <- "NRMSE"
    CV <- NRMSE * 100
    base::names(CV) <- "CV(%)"
    A <- base::cbind(MSE, RMSE, NRMSE, CV)
    base::rownames(A) <- base::c("Parameter")
    base::print(A)

  }

  P <- ._Parameter(Model)

  utils::write.csv(P,
            file = base::paste0(input$project_name, " -- Spatial analysis parameter for ", response, ".csv"), row.names = FALSE)

  p <- ggplot2::ggplot(Data, ggplot2::aes(get(input$spat_col), get(input$spat_row), fill = RawValue)) +
    ggplot2::geom_tile() +
    viridis::scale_fill_viridis(discrete = F) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = "Raw Value", fill = 'Response Variable')

  ggplot2::ggsave(
    p,
    file = base::paste0(input$project_name, " -- Spatial analysis -- Raw Value ( ", response, " ).png"),
    width = 32,
    height = 15,
    units = "cm"
  )
  ggplot2::ggsave(
    p,
    file = base::paste0(input$project_name, " -- Spatial analysis -- Raw Value ( ", response, " ).pdf"),
    width = 32,
    height = 15,
    units = "cm"
  )

  p <- ggplot2::ggplot(Data, ggplot2::aes(base::get(input$spat_col), base::get(input$spat_row), fill = FittedValue)) +
    ggplot2::geom_tile() +
    viridis::scale_fill_viridis(discrete = FALSE) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = "Fitted Value", fill = 'Response Variable')

  ggplot2::ggsave(
    p,
    file = base::paste0(input$project_name, " -- Spatial analysis -- Fitted Value ( ", response, " ).png"),
    width = 32,
    height = 15,
    units = "cm"
  )
  ggplot2::ggsave(
    p,
    file = base::paste0(input$project_name, " -- Spatial analysis -- Fitted Value ( ", response, " ).pdf"),
    width = 32,
    height = 15,
    units = "cm"
  )

  p <- ggplot2::ggplot(Data, ggplot2::aes(get(input$spat_col), get(input$spat_row), fill = ResidualValue)) +
    ggplot2::geom_tile() +
    viridis::scale_fill_viridis(discrete = FALSE) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = "Residual Value", fill = 'Response Variable')

  ggplot2::ggsave(
    p,
    file = base::paste0(input$project_name, " -- Spatial analysis -- Residual Value ( ", response, " ).png"),
    width = 32,
    height = 15,
    units = "cm"
  )
  ggplot2::ggsave(
    p,
    file = base::paste0(input$project_name, " -- Spatial analysis -- Residual Value ( ", response, " ).pdf"),
    width = 32,
    height = 15,
    units = "cm"
  )
  set_wd('Spatial Analysis', rv, input$save_results)
}