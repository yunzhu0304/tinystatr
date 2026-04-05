#' @title Perform Post-hoc Test for Tukey's HSD
#' @description
#'     This function conducts a post-hoc test using Tukey's Honestly Significant
#'     Difference (HSD) method after conducting an ANOVA and filtering data
#'     based on a specified variable and its corresponding identifier.
#'
#' @param data A data object coercible to a data.frame containing the dataset.
#' @param group A character string specifying the grouping variable in the dataset.
#' @param variable A character string specifying the variable in the dataset used for filtering.
#' @param id A character string specifying the identifier value to filter the data.
#' @param formula A formula specifying the model for the analysis of variance.
#' @param method One of \code{"hsd"}, \code{"bonf"}, \code{"lsd"}, \code{"scheffe"}, \code{"newmankeuls"}. Default is \code{"hsd"}.
#' @param ... Additional arguments to be passed.
#'
#' @return A data frame containing the adjusted p-values and additional information for each comparison.
#'
#' @seealso \code{\link[DescTools]{PostHocTest}} for more information on post-hoc tests.
#'
#' @importFrom dplyr mutate
#' @importFrom DescTools PostHocTest
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr separate
#' @export
#'
#' @examples
#' \donttest{
#' data("ToothGrowth")
#' df <- ToothGrowth
#' hsd_p(data = df, group = "dose", variable = "supp", id = "VC", formula = len ~ dose)
#' }
hsd_p <- function(data, group, variable, id, formula, method = "hsd", ...) {
  data <- .as_df(data)
  data[[group]] <- as.character(data[[group]])

  fit <- anof(data = data, variable = variable, id = id, formula = formula, ...)

  tpd <- as.matrix(
    DescTools::PostHocTest(fit, method = method, ...)[[group]][, 4]
  ) %>%
    `colnames<-`("p.adj") %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "groups") %>%
    dplyr::mutate(posthoc = method, variable = id) %>%
    tidyr::separate(groups, into = c("group2", "group1"), sep = "-", remove = TRUE)

  tpd$p1 <- summary(fit)[[1]][1, 5]
  tpd$P1method <- "ANOVA"
  return(tpd)
}

#' @title Perform Post-hoc Test using Dunn's Test
#' @description
#'     This function conducts a post-hoc test using Dunn's test with a
#'     specified adjustment method after filtering data based on a specified
#'     variable and its corresponding identifier.
#'
#' @param data A data object coercible to a data.frame containing the dataset.
#' @param group A character string specifying the grouping variable in the dataset.
#' @param variable A character string specifying the variable in the dataset used for filtering.
#' @param id A character string specifying the identifier value to filter the data.
#' @param formula A formula specifying the model for the post-hoc test. For example, \code{TP53 ~ cancer_group}.
#' @param method Allowed values include \code{"holm"}, \code{"hochberg"}, \code{"hommel"}, \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"}, \code{"none"}. Default is \code{"bonferroni"}.
#' @param ... Additional arguments to be passed.
#'
#' @return A data frame containing the p-values and additional information for each comparison.
#'
#' @seealso \code{\link[rstatix]{dunn_test}} for more information on Dunn's test.
#'
#' @importFrom dplyr filter mutate select
#' @importFrom rstatix dunn_test
#' @export
#'
#' @examples
#' \donttest{
#' data("ToothGrowth")
#' df <- ToothGrowth
#' dunn_p(data = df, group = "dose", variable = "supp", id = "VC", formula = len ~ dose)
#' }
dunn_p <- function(data, group, variable, id, formula, method = "bonferroni", ...) {
  data <- .as_df(data)

  tpd <- data %>%
    dplyr::filter(.data[[variable]] == id) %>%
    rstatix::dunn_test(formula = formula, p.adjust.method = method, ...) %>%
    dplyr::mutate(posthoc = method, variable = id) %>%
    dplyr::select(group1, group2, p.adj, posthoc, variable)

  tpd$p1 <- kwf(data = data, variable = variable, id = id, formula = formula, ...)$p
  tpd$P1method <- "K_W"
  return(tpd)
}
