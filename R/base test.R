#' @title Two Sample T-test
#' @description
#'     Conducts a two-sample t-test on a specified variable in the provided
#'     data.
#' @details This function filters the data based on the provided \code{variable}
#'     and \code{id}, and then conducts a two-sample t-test on the filtered
#'     subset of data. If a formula is provided, the t-test is conducted based
#'     on the specified model structure.
#'
#' @param data A data frame containing the data.
#' @param variable A character string specifying the name of the variable in \code{data} for which the t-test will be conducted.
#' @param id A value to filter the \code{variable} by.
#' @param formula An optional formula specifying the structure of the statistical model to be tested.
#' @param var.equal Logical indicating whether to assume equal variances in the two groups. Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to \code{\link[rstatix]{t_test}}.
#'
#' @return A t-test result.
#'
#' @details This function filters the data based on the provided \code{variable} and \code{id}, and then conducts a t-test on the filtered subset of data. If a formula is provided, the t-test is conducted based on the specified model structure.
#'
#' @seealso \code{\link[rstatix]{t_test}} for more advanced t-test functionality.
#'
#'
#' @importFrom rstatix t_test
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
#' tf(data = df,variable = "supp",id="VC",formula = len ~ dose)
#' }

tf <- function(data, variable, id,formula,var.equal = TRUE,...) {
  data %>%
    filter(.data[[variable]] == id) %>%
    t_test(formula,...)
}




#' @title Welch's T-test
#' @description
#'     Conducts a Welch's t-test on a specified variable in the provided
#'     data.
#' @details This function filters the data based on the provided \code{variable}
#'     and \code{id}, and then conducts a welch's t-test on the filtered
#'     subset of data. If a formula is provided, the t-test is conducted based
#'     on the specified model structure.
#'
#' @param data A data frame containing the data.
#' @param variable A character string specifying the name of the variable in \code{data} for which the t-test will be conducted.
#' @param id A value to filter the \code{variable} by.
#' @param formula An optional formula specifying the structure of the statistical model to be tested.
#' @param var.equal Logical indicating whether to assume equal variances in the two groups. Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to \code{\link[rstatix]{t_test}}.
#'
#' @return A t-test result.
#'
#' @details This function filters the data based on the provided \code{variable} and \code{id}, and then conducts a t-test on the filtered subset of data. If a formula is provided, the t-test is conducted based on the specified model structure.
#'
#' @seealso \code{\link[rstatix]{t_test}} for more advanced t-test functionality.
#'
#'
#' @importFrom rstatix t_test
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
#' welch_tf(data = df,variable = "supp",id="VC",formula = len ~ dose)
#' }

welch_tf <- function(data, variable, id,formula,var.equal = FALSE,...) {
  data %>%
    filter(.data[[variable]] == id) %>%
    t_test(formula,...)
}



#' @title Wilcoxon Tests
#' @description
#'     Conducts a Wilcoxon test on a specified variable in the provided
#'     data.
#' @details This function filters the data based on the provided \code{variable}
#'     and \code{id}, and then conducts a Wilcoxon test on the filtered
#'     subset of data. If a formula is provided, the Wilcoxon is conducted based
#'     on the specified model structure.
#'
#' @param data A data frame containing the data.
#' @param variable A character string specifying the name of the variable in \code{data} for which the wilcox test will be conducted.
#' @param id A value to filter the \code{variable} by.
#' @param formula An optional formula specifying the structure of the statistical model to be tested.
#' @param var.equal Logical indicating whether to assume equal variances in the two groups. Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to \code{\link[rstatix]{wilcox_test}}.
#'
#' @return A wilcoxon test result.
#'
#' @details This function filters the data based on the provided \code{variable} and \code{id}, and then conducts a wilcox test on the filtered subset of data. If a formula is provided, the wilcox test is conducted based on the specified model structure.
#'
#' @seealso \code{\link[rstatix]{wilcox_test}} for more advanced wilcoxon test functionality.
#'
#'
#' @importFrom rstatix wilcox_test
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
#' wf(data = df,variable = "supp",id="VC",formula = len ~ dose)
#' }

wf <- function(data, variable, id,formula,var.equal = FALSE,...) {
  data %>%
    filter(.data[[variable]] == id) %>%
    wilcox_test(formula,...)
}
