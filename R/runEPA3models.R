#' @export
aq3 <- function() {

  list.of.packages <- c("shiny","ggplot2", "dplyr","data.table","DT","readxl", "shinythemes","splus2R",
                        "plotly","Hmisc","tinytex","shinyFiles","utils","webshot")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)

  appDir <- system.file("shiny_apps", "myapp", package = "epa3aq")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `epa3aq`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
#' @import plotly dplyr
