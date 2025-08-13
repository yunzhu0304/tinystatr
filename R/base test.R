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

tf <- function(data,
               variable,
               id,
               formula,
               var.equal = TRUE,...) {
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

welch_tf <- function(data,
                     variable,
                     id,
                     formula,
                     var.equal = FALSE,...) {
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

wf <- function(data,
               variable,
               id,
               formula,
<<<<<<< HEAD
               var.equal = FALSE,
               ...) {

  df <- data %>%
    dplyr::filter(.data[[variable]] == id)

  vars <- all.vars(formula)
  resp <- vars[1]
  grp  <- vars[2]

  df <- df %>%
    dplyr::filter(!is.na(.data[[resp]]), !is.na(.data[[grp]]))


  glevels <- levels(factor(df[[grp]]))
  n1 <- sum(df[[grp]] == glevels[1], na.rm = TRUE)
  n2 <- sum(df[[grp]] == glevels[2], na.rm = TRUE)


  dots <- list(...)
  res <- do.call(stats::wilcox.test,
                 c(list(formula = formula, data = df), dots))


  tibble::tibble(
    group1 = as.character(glevels[1]),
    group2 = as.character(glevels[2]),
    n1     = n1,
    n2     = n2,
    p      = format_p(unname(res$p.value), digits = 4) %>% as.numeric()

  )
}

#' Format P-values for display
#'
#' This function formats numeric p-values for reporting.
#' Values smaller than `small_thresh` are displayed in scientific notation
#' with 2 significant digits. Other values are rounded to four decimal places.
#'
#' @param p A numeric p-value.
#' @param small_thresh Threshold below which the p-value will be displayed
#' in scientific notation. Defaults to `1e-4`.
#'
#' @return A character string representing the formatted p-value.
#' @examples
#' format_p(0.123456)
#' format_p(0.0000002)
#' format_p(NA)
#' @export
format_p <- function(p, small_thresh = 1e-4, digits = 4) {
  if (is.na(p)) return(NA_character_)
  if (p < small_thresh) {
    # 科学计数法
    formatC(p, format = "e", digits = digits - 1)
  } else {
    # 固定小数位
    sprintf(paste0("%.", digits, "f"), round(p, digits))
  }
}

=======
               var.equal = FALSE,...) {
  data %>%
    filter(.data[[variable]] == id) %>%
    wilcox_test(formula,...)
}
>>>>>>> origin/main
