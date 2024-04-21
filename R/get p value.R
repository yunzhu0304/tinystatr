#' @title For P Value (T Test)
#'
#' @param data A data frame containing the data.
#' @param variable A character string specifying the name of the variable in \code{data} for which the t-test will be conducted.
#' @param id A value to filter the \code{variable} by.
#' @param formula An optional formula specifying the structure of the statistical model to be tested.
#' @param var.equal Logical indicating whether to assume equal variances in the two groups. Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to \code{\link[rstatix]{t_test}}.
#'
#' @return A data frame containing 'group1', 'group2', 'p', 'p.adj', and
#'     'variable'.
#'
#' @importFrom dplyr filter %>% mutate sym select all_of
#' @importFrom stats median sd
#'
#' @export
#' @examples
#' \donttest{
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth%>%
#' filter(dose %in% c("0.5","1"))
#'
#' tf_p(data = df,variable = "supp",id="VC",formula = len ~ dose)
#' }

tf_p <- function(id,...){
  tpd <- tf(id,...)
  tpd$variable <- id
  tpd$method <- "t test"
  return(tpd)
}



#' @title For P Value (Welch's t-test)
#'
#' @param data A data frame containing the data.
#' @param variable A character string specifying the name of the variable in \code{data} for which the t-test will be conducted.
#' @param id A value to filter the \code{variable} by.
#' @param formula An optional formula specifying the structure of the statistical model to be tested.
#' @param var.equal Logical indicating whether to assume equal variances in the two groups. Default is \code{FALSE}.
#' @param ... Additional arguments to be passed to \code{\link[rstatix]{t_test}}.
#'
#' @return A data frame containing 'group1', 'group2', 'p', 'p.adj', and
#'     'variable'.
#'
#' @importFrom dplyr filter %>% mutate sym select all_of
#' @importFrom stats median sd
#'
#' @export
#' @examples
#' \donttest{
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth%>%
#' filter(dose %in% c("0.5","1"))
#'
#' weltf_p(data = df,variable = "supp",id="VC",formula = len ~ dose)
#' }

weltf_p <- function(id,...){
  tpd <- welch_tf(id,...)
  tpd$variable <- id
  tpd$method <- "Welch's t-test"
  return(tpd)
}



#' @title For P Value (Wilcoxon test)
#'
#' @param data A data frame containing the data.
#' @param variable A character string specifying the name of the variable in \code{data} for which the wilcox_test will be conducted.
#' @param id A value to filter the \code{variable} by.
#' @param formula An optional formula specifying the structure of the statistical model to be tested.
#' @param ... Additional arguments to be passed to \code{\link[rstatix]{wilcox_test}}.
#'
#' @return A data frame containing 'group1', 'group2', 'p', 'p.adj', and
#'     'variable'.
#'
#' @importFrom dplyr filter %>% mutate sym select all_of
#' @importFrom stats median sd
#'
#' @export
#' @examples
#' \donttest{
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#' df <- ToothGrowth%>%
#' filter(dose %in% c("0.5","1"))
#'
#' wf_p(data = df,variable = "supp",id="VC",formula = len ~ dose)
#' }

wf_p <- function(id,...){
  tpd <- wf(id,...)
  tpd$variable <- id
  tpd$method <- "Wilcoxon test"
  return(tpd)
}
