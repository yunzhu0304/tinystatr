
#' @title Perform Post-hoc Test for Tukey's HSD
#' @description
#'     This function conducts a post-hoc test using Tukey's Honestly Significant
#'     Difference (HSD) method after conducting an ANOVA and filtering data
#'     based on a specified variable and its corresponding identifier.
#'
#' @param data A data frame containing the dataset.
#' @param group A character string specifying the grouping variable in the dataset.
#' @param variable A character string specifying the variable in the dataset used for filtering.
#' @param id A character string specifying the identifier value to filter the data.
#' @param formula A formula specifying the model for the analysis of variance.
#' @param method one of "hsd", "bonf", "lsd", "scheffe", "newmankeuls", default is "hsd".
#' @param ... Additional arguments to be passed.
#'
#' @return A data frame containing the adjusted p-values and additional information for each comparison.
#'
#' @see \code{\link[DescTools]{PostHocTest}} for more information on post-hoc tests.
#'
#' @importFrom dplyr mutate
#' @importFrom DescTools PostHocTest
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr separate
#' @export
#'
#' @examples
#' \donttest{
#' # Load data
#' data("ToothGrowth")
#'
#' df <- ToothGrowth
#' hsd_p(data = df, group = "dose", variable = "supp", id = "VC", formula = len ~ dose)
#' }
#'
hsd_p <- function(data, group, variable, id,formula,method = "hsd",...){
  data[[group]] <- as.character(data[[group]])
  tpd <- as.matrix(DescTools::PostHocTest(anof(data, variable, id,formula), method = method)[[group]][,4]) %>%
    `colnames<-`("p.adj") %>%
    as.data.frame() %>%
    rownames_to_column(var = "groups")%>%
    mutate(.,posthoc = method,variable = id) %>%
    separate(groups, into = c("group2", "group1"), sep = "-", remove = TRUE)
  tpd$p1 <- summary(anof(data, variable, id,formula))[[1]][1,5]
  tpd$P1method <- "ANOVA"
  return(tpd)
}




#' @title Perform Post-hoc Test using Dunn's Test
#' @description
#'     This function conducts a post-hoc test using Dunn's test with a
#'     specified adjustment method after filtering data based on a specified
#'     variable and its corresponding identifier.
#'
#' @param data A data frame containing the dataset.
#' @param group A character string specifying the grouping variable in the dataset.
#' @param variable A character string specifying the variable in the dataset used for filtering.
#' @param id A character string specifying the identifier value to filter the data.
#' @param formula A formula specifying the model for the post-hoc test. For example, formula = TP53 ~ cancer_group.
#' @param method  Allowed values include "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". Default is "bonferroni".
#' @param ... Additional arguments to be passed.
#'
#' @return A data frame containing the p-values and additional information for each comparison.
#'
#' @seealso \code{\link[rstatix]{dunn_test}} for more information on Dunn's test.
#'
#' @importFrom dplyr filter mutate
#' @importFrom rstatix dunn_test
#' @export
#'
#' @examples
#' \donttest{
#' # Load data
#' data("ToothGrowth")
#'
#' df <- ToothGrowth
#' dunn_p(data = df, group = "dose", variable = "supp", id = "VC", formula = len ~ dose)
#' }
#'
dunn_p <- function(data, group, variable, id,formula,method = "bonferroni",...){
  tpd <- data %>%
    filter(.data[[variable]] == id) %>%
    dunn_test(formula, p.adjust.method = method)%>%
    mutate(.,posthoc = method,variable = id) %>%
    select(group1,group2,p.adj,posthoc,variable)
  tpd$p1 <- kwf(data, variable, id,formula)$p
  tpd$P1method <- "K_W"
  return(tpd)
}

