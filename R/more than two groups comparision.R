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
#' @param formula A formula object specifying the statistical model (e.g., \code{value ~ group}).
#' @param variable A logical value indicating whether the dataset contains a 'variable' column.
#'   If \code{FALSE}, a default "id" variable will be added. Default is \code{FALSE}.
#' @param id A character string specifying the identifier for a particular variable to be analyzed.
#'   Default is \code{"id"}.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return An S4 object of class \code{statresult} with the following slots:
#' \item{stat}{A data frame containing pairwise comparisons with adjusted p-values and test methods.}
#' \item{normal}{A data frame summarizing normality test results, mean values, and standard deviations.}
#' \item{p_position}{A data frame providing recommended y-axis positions for p-value annotations in plots.}
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Checks if there are at least three samples per group; groups with fewer samples are excluded.
#'   \item Evaluates normality using the Shapiro-Wilk test for each group.
#'   \item If data are normally distributed, Bartlett's test for homogeneity of variance is conducted.
#'   \item Depending on variance test results, an ANOVA or Kruskal-Wallis test is performed.
#'   \item Post-hoc tests (Tukey HSD for ANOVA or Dunn's test for Kruskal-Wallis) are applied when p < 0.05.
#'   \item Calculates appropriate positions for visualizing p-values on plots.
#' }
#'
#' @importFrom dplyr mutate filter select ungroup count group_by
#' @importFrom rlang .data sym
#' @importFrom rstatix add_significance
#' @importFrom stats bartlett.test shapiro.test
#' @importFrom methods new
#'
#' @examples
#' \donttest{
#' # Example 1: data frame with multiple columns (filter by variable)
#' data("ToothGrowth")
#' df <- ToothGrowth
#' result <- stat3(
#'   data = df, group = "dose", value = "len",
#'   variable = "supp", id = "OJ", formula = len ~ dose
#' )
#' result@stat
#' result@normal
#' result@p_position
#'
#' # Example 2: data frame with only two columns (group, value)
#' data("HairEyeColor")
#' df <- as.data.frame(HairEyeColor)[, c(2, 4)]
#' result <- stat3(
#'   data = df, group = "Eye", value = "Freq",
#'   formula = Freq ~ Eye
#' )
#' result@stat
#' result@normal
#' result@p_position
#' }
#' @export
stat3 <- function(data,
                  group,
                  value,
                  formula,
                  variable = FALSE,
                  id = "id", ...){

  if (variable == FALSE) {
    data <- mutate(data, variable = "id")
    variable <- "variable"
  }

  vari <- rlang::sym(variable)
  gro  <- rlang::sym(group)
  valu <- rlang::sym(value)

  nordis_data <- data.frame(
    group = character(),
    variable = character(),
    normal = logical(),
    meanvalue = numeric(),
    sd = numeric()
  )

  small_groups <- data %>%
    dplyr::count(!!gro) %>%
    dplyr::filter(n < 3) %>%
    dplyr::pull(!!gro)

  if (length(small_groups) > 0) {
    message(
      "The following groups have less than 3 samples and are not included in the analysis: ",
      paste(small_groups, collapse = ", ")
    )
    # remove groups with < 3 samples
    data <- data %>%
      dplyr::group_by(!!gro) %>%
      dplyr::filter(dplyr::n() >= 3) %>%
      dplyr::ungroup()
  } else {
    cat("All groups have 3 or more samples.\n")
  }

  if (length(unique(data[[group]])) < 3) {
    if (length(unique(data[[group]])) == 2) {
      stat2result <- stat2(
        data = data, group = group, variable = variable,
        id = id, value = value, formula = formula
      )

      stat3result <- stat2result@stat %>%
        dplyr::select(group1, group2, p, method, variable) %>%
        dplyr::mutate(p1 = NA, P1method = NA) %>%
        `colnames<-`(c("group1", "group2", "p.adj", "posthoc", "variable", "p1", "P1method")) %>%
        rstatix::add_significance("p.adj")

      statresult <- methods::new(
        "statresult",
        stat       = as.data.frame(stat3result),
        normal     = stat2result@normal,
        p_position = stat2result@p_position
      )
      return(statresult)
    } else {
      warning("Unable to perform statistics as there are fewer than 2 groups.")
    }
  } else {
    for (i in seq_along(unique(data[[group]]))) {
      test_data <- data %>%
        dplyr::filter(!!gro == unique(!!gro)[i]) %>%
        dplyr::filter(!!vari == id) %>%
        dplyr::select(!!valu) %>%    # 修复：避免 all_of(valu) 带来 tidyselect 依赖/报错
        unlist() %>%
        as.numeric()

      Ntest <- if (length(unique(test_data)) == 1) {
        FALSE
      } else {
        stats::shapiro.test(test_data)$p.value > 0.05
      }

      new_row <- data.frame(
        group     = unique(data[[group]])[i],
        variable  = id,
        normal    = Ntest,
        meanvalue = mean(test_data),
        sd        = sd(test_data)
      )
      nordis_data <- rbind(nordis_data, new_row)
    }

    if (exists("nordata_save")) {
      nordata_save <- rbind(nordata_save, nordis_data)
    } else {
      nordata_save <- nordis_data
    }

    if (sum(nordis_data$normal) == length(unique(data[[group]]))) {
      cat("Normally distributed\n")
      Vtest <- data %>%
        dplyr::filter(!!vari == id) %>%
        stats::bartlett.test(data = ., formula) %>%
        .$p.value > 0.05

      if (Vtest) {
        cat("Variance equal\n")
        cat("Anova\n")
        stat3result <- hsd_p(
          data = data, group = group, variable = variable, id = id, formula = formula
        ) %>% rstatix::add_significance("p.adj")
      } else {
        cat("Variance unequal\n")
        cat("Kruskal-Wallis\n")
        stat3result <- dunn_p(
          data = data, group = group, variable = variable, id = id, formula = formula
        ) %>% rstatix::add_significance("p.adj")
      }
    } else {
      cat("Non-normally distributed\n")
      cat("Variance unequal\n")
      cat("Kruskal-Wallis\n")
      stat3result <- dunn_p(
        data = data, group = group, variable = variable, id = id, formula = formula
      ) %>% rstatix::add_significance("p.adj")
    }

    max_value <- max(as.numeric(data[, value]))
    p_position <- stat3result[, 1:2]
    p_position$y.position <- NA_real_
    if (max_value > 0) {
      p_position$y.position[1] <- max_value * 1.12
      for (i in 2:nrow(p_position)) {
        p_position$y.position[i] <- p_position$y.position[i - 1] * 1.08
      }
    } else {
      if (abs(max_value) >= 1) {
        p_position$y.position[1] <- 1.12
        for (i in 2:nrow(p_position)) {
          p_position$y.position[i] <- p_position$y.position[i - 1] * 1.08
        }
      } else {
        p_position$y.position[1] <- abs(max_value) * 1.12
        for (i in 2:nrow(p_position)) {
          p_position$y.position[i] <- p_position$y.position[i - 1] * 1.08
        }
      }
    }

    statresult <- methods::new(
      "statresult",
      stat       = as.data.frame(stat3result),
      normal     = as.data.frame(nordata_save),
      p_position = as.data.frame(p_position)
    )
    return(statresult)
  }
}
