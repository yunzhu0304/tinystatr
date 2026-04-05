#' Perform Statistical Analysis with Post-hoc Tests and Visualization Positions
#'
#' This function conducts statistical tests (ANOVA or Kruskal-Wallis) to compare group means,
#' performs post-hoc tests when necessary, and provides positions for p-value visualization.
#' The results are returned as an S4 object containing statistical outcomes, normality tests,
#' and recommended p-value positions for plotting.
#'
#' @param data A data frame containing the dataset to be analyzed.
#' @param group A character string specifying the grouping variable (e.g., treatment groups).
#' @param value A character string specifying the response variable to be tested.
#' @param formula A formula object specifying the statistical model (e.g., `value ~ group`).
#' @param variable A logical value or character string indicating whether the dataset contains
#'   a 'variable' column. If \code{FALSE}, a default "id" variable will be added. Default is
#'   \code{FALSE}.
#' @param id A character string specifying the identifier for a particular variable to be
#'   analyzed. Default is \code{"id"}.
#' @param parametric This argument allows users to skip the automatic decision procedure.
#'    Logical or \code{NULL}. Default is \code{NULL}.
#'    If \code{NULL}, the function automatically selects between parametric and non-parametric
#'    procedures based on data characteristics.
#'    If \code{TRUE}, a parametric procedure is forced: for more than two groups, ANOVA followed
#'    by post-hoc testing; for two groups, the argument is passed to \code{stat2()}.
#'    If \code{FALSE}, a non-parametric procedure is forced: for more than two groups,
#'    Kruskal-Wallis followed by Dunn's test; for two groups, the argument is passed to
#'   \code{stat2()}.
#' @param ... Additional arguments passed to internal functions (\code{stat2},
#'   \code{hsd_p}, \code{dunn_p}).
#'
#' @return An S4 object of class \code{statresult} with the following slots:
#' \item{stat}{A data frame containing pairwise comparisons with adjusted p-values and test
#'   methods.}
#' \item{normal}{A data frame summarizing normality test results, mean values, and standard
#'   deviations.}
#' \item{p_position}{A data frame providing recommended y-axis positions for p-value
#'   annotations in plots.}
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Checks if there are at least three samples per group; groups with fewer samples are
#'     excluded.
#'   \item Evaluates normality using the Shapiro-Wilk test for each group.
#'   \item If data are normally distributed, Bartlett's test for homogeneity of variance is
#'     conducted.
#'   \item Depending on variance test results and the \code{parametric} argument, an ANOVA or
#'     Kruskal-Wallis test is performed.
#'  \item Post-hoc tests (Tukey HSD for ANOVA or Dunn's test for Kruskal-Wallis)
#'     are used to generate pairwise comparisons.
#'   \item Calculates appropriate y-axis positions for visualizing p-values on plots.
#' }
#'
#' @importFrom dplyr mutate filter select ungroup count group_by pull
#' @importFrom rlang sym
#' @importFrom rstatix add_significance
#' @importFrom stats bartlett.test shapiro.test
#' @importFrom methods new
#'
#' @examples
#' \donttest{
#' data("ToothGrowth")
#' df <- ToothGrowth
#'
#' # Automatic selection
#' result_auto <- stat3(
#'   data = df,
#'   group = "dose",
#'   value = "len",
#'   variable = "supp",
#'   id = "OJ",
#'   formula = len ~ dose,
#'   parametric = NULL
#' )
#' result_auto@stat
#' result_auto@normal
#' result_auto@p_position
#'
#' # Force parametric procedure
#' result_param <- stat3(
#'   data = df,
#'   group = "dose",
#'   value = "len",
#'   variable = "supp",
#'   id = "OJ",
#'   formula = len ~ dose,
#'   parametric = TRUE
#' )
#' result_param@stat
#'
#' # Force non-parametric procedure
#' result_nonparam <- stat3(
#'   data = df,
#'   group = "dose",
#'   value = "len",
#'   variable = "supp",
#'   id = "OJ",
#'   formula = len ~ dose,
#'   parametric = FALSE
#' )
#' result_nonparam@stat
#'
#' # Example without a variable column
#' data("HairEyeColor")
#' df2 <- as.data.frame(HairEyeColor)[, c(2, 4)]
#'
#' result2 <- stat3(
#'   data = df2,
#'   group = "Eye",
#'   value = "Freq",
#'   formula = Freq ~ Eye,
#'   parametric = NULL
#' )
#' result2@stat
#' result2@normal
#' result2@p_position
#' }
#' @export
stat3 <- function(data,
                  group,
                  value,
                  formula,
                  variable = FALSE,
                  id = "id",
                  parametric = NULL,
                  ...) {

  data <- .as_df(data)

  if (!is.null(parametric)) {
    if (!is.logical(parametric) || length(parametric) != 1) {
      stop("`parametric` must be one of NULL, TRUE, or FALSE.", call. = FALSE)
    }
  }

  if (isFALSE(variable)) {
    data     <- mutate(data, variable = "id")
    variable <- "variable"
  }

  vari <- rlang::sym(variable)
  gro  <- rlang::sym(group)

  nordis_data <- data.frame(
    group     = character(),
    variable  = character(),
    normal    = logical(),
    meanvalue = numeric(),
    sd        = numeric()
  )

  data_id <- data %>%
    filter(!!vari == id)

  small_groups <- data_id %>%
    count(!!gro) %>%
    filter(n < 3) %>%
    pull(!!gro)

  if (length(small_groups) > 0) {
    message(
      "The following groups have less than 3 samples and are not included ",
      "in the analysis: ", paste(small_groups, collapse = ", ")
    )
    data_id <- data_id %>%
      group_by(!!gro) %>%
      filter(n() >= 3) %>%
      ungroup()
  } else {
    message("All groups have 3 or more samples.")
  }

  n_groups <- length(unique(data_id[[group]]))

  if (n_groups < 3) {
    if (n_groups == 2) {
      stat2result <- stat2(
        data      = data,
        group     = group,
        variable  = variable,
        id        = id,
        value     = value,
        formula   = formula,
        parametric = parametric,
        ...
      )

      stat3result <- stat2result@stat %>%
        select(group1, group2, p, method, variable) %>%
        mutate(p1 = NA, P1method = NA) %>%
        `colnames<-`(c("group1", "group2", "p.adj", "posthoc",
                       "variable", "p1", "P1method")) %>%
        add_significance("p.adj")

      statresult <- new(
        "statresult",
        stat       = as.data.frame(stat3result),
        normal     = stat2result@normal,
        p_position = stat2result@p_position
      )

      return(statresult)

    } else {
      warning("Unable to perform statistics as there are fewer than 2 groups.")
      return(invisible(NULL))
    }
  }


  group_levels <- unique(data_id[[group]])

  for (i in seq_along(group_levels)) {
    test_data <- data_id %>%
      filter(!!gro == group_levels[i]) %>%
      select(all_of(value)) %>%
      unlist(use.names = FALSE) %>%
      as.numeric()

    test_data <- test_data[!is.na(test_data)]

    ntest <- if (length(test_data) < 3 || length(unique(test_data)) == 1) {
      FALSE
    } else {
      shapiro.test(test_data)$p.value > 0.05
    }

    new_row <- data.frame(
      group     = group_levels[i],
      variable  = id,
      normal    = ntest,
      meanvalue = mean(test_data, na.rm = TRUE),
      sd        = sd(test_data,   na.rm = TRUE)
    )

    nordis_data <- rbind(nordis_data, new_row)
  }

  if (exists("nordata_save")) {
    nordata_save <- rbind(nordata_save, nordis_data)
  } else {
    nordata_save <- nordis_data
  }


  if (isTRUE(parametric)) {
    message("User-specified: parametric test")
    message("ANOVA")

    stat3result <- hsd_p(
      data     = data,
      group    = group,
      variable = variable,
      id       = id,
      formula  = formula,
      ...
    ) %>%
      add_significance("p.adj")

  } else if (isFALSE(parametric)) {
    message("User-specified: non-parametric test")
    message("Kruskal-Wallis")

    stat3result <- dunn_p(
      data     = data,
      group    = group,
      variable = variable,
      id       = id,
      formula  = formula,
      ...
    ) %>%
      add_significance("p.adj")

  } else {
    if (sum(nordis_data$normal) == n_groups) {
      message("Normally distributed")

     vtest <- bartlett.test(formula = formula, data = data_id)$p.value > 0.05

      if (vtest) {
        message("Variance equal")
        message("ANOVA")

        stat3result <- hsd_p(
          data     = data,
          group    = group,
          variable = variable,
          id       = id,
          formula  = formula,
          ...
        ) %>%
          add_significance("p.adj")

      } else {
        message("Variance unequal")
        message("Kruskal-Wallis")

        stat3result <- dunn_p(
          data     = data,
          group    = group,
          variable = variable,
          id       = id,
          formula  = formula,
          ...
        ) %>%
          add_significance("p.adj")
      }

    } else {
      message("Non-normally distributed")
      message("Kruskal-Wallis")

      stat3result <- dunn_p(
        data     = data,
        group    = group,
        variable = variable,
        id       = id,
        formula  = formula,
        ...
      ) %>%
        add_significance("p.adj")
    }
  }

  max_value <- max(as.numeric(data_id[[value]]), na.rm = TRUE)

  p_position <- stat3result[, 1:2]
  p_position$y.position <- NA

  if (max_value > 0) {
    p_position$y.position[1] <- max_value * 1.12
  } else if (abs(max_value) >= 1) {
    p_position$y.position[1] <- 1.12
  } else {
    p_position$y.position[1] <- max(abs(max_value) * 1.12, 0.01)
  }

  if (nrow(p_position) > 1) {
    for (i in 2:nrow(p_position)) {
      p_position$y.position[i] <- p_position$y.position[i - 1] * 1.08
    }
  }

  statresult <- new(
    "statresult",
    stat       = as.data.frame(stat3result),
    normal     = as.data.frame(nordata_save),
    p_position = as.data.frame(p_position)
  )

  return(statresult)
}
