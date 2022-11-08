#' Waiter object
#'
#' @noRd

if (!require(waiter))
   utils::install.packages("waiter")

waiter <- waiter::Waiter$new(
  color = 'rgba(100,100,100,0.5)',
  html = waiter::spin_folding_cube()
)
