#' @title Perform Analysis of Variance with Filtering
#' @description
#' This function conducts an analysis of variance (ANOVA) after filtering data based on a specified variable and its corresponding identifier.
#'
#'
#' @param data A data frame containing the dataset.
#' @param variable A character string specifying the variable in the dataset used for filtering.
#' @param id A character string specifying the identifier value to filter the data.
#' @param formula A formula specifying the model for the analysis of variance.
#' @param ... Additional arguments to be passed to the \code{\link[stats]{aov}} function.
#'
#' @return An ANOVA object.
#'
#' @seealso \code{\link[stats]{aov}} for more advanced t-test functionality.
#'
#' @importFrom dplyr filter %>% mutate sym select all_of
#' @importFrom stats median sd aov
#' @export
#'
#' @examples
#' \donttest{
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' data("ToothGrowth")
#'
#' df <- ToothGrowth
#' anof(data = df, variable = "supp", id = "VC", formula = len ~ dose)
#' }
#'
anof <- function(data,
                 variable,
                 id,
                 formula, ...){
  data %>%
    filter(.data[[variable]] == id) %>%
    aov(data=., formula,...)
}




#' @title Perform Kruskal-Wallis Test with Filtering
#' @description
#' This function conducts a Kruskal-Wallis test after filtering data based on a specified variable and its corresponding identifier.
#'
#'
#' @title Perform Kruskal-Wallis Test with Filtering
#' @description
#' This function conducts a Kruskal-Wallis test after filtering data based on a specified variable and its corresponding identifier.
#'
#' @param data A data frame containing the dataset.
#' @param variable A character string specifying the variable in the dataset used for filtering.
#' @param id A character string specifying the identifier value to filter the data.
#' @param formula A formula specifying the model for the Kruskal-Wallis test.
#' @param ... Additional arguments to be passed to the \code{\link[rstatix]{kruskal_test}} function.
#'
#' @return A Kruskal-Wallis test object.
#'
#' @seealso \code{\link[rstatix]{kruskal_test}} for more information on Kruskal-Wallis test functionality.
#'
#' @importFrom dplyr filter %>%
#' @importFrom rstatix kruskal_test
#' @export
#'
#' @examples
#' \donttest{
#' # Load data
#' data("ToothGrowth")
#'
#' df <- ToothGrowth
#' kwf(data = df, variable = "supp", id = "VC", formula = len ~ dose)
#' }


kwf <- function(data,
                variable,
                id,
                formula, ...){
  data %>%
    filter(.data[[variable]] == id) %>%
    kruskal_test(data=., formula,...)
}

