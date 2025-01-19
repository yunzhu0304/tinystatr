#' @title Perform statistical tests for two-group comparison
#' @description
#'     Depending on the characteristics of the data, it automatically determines whether to use parametric tests (t-test, Welch's t-test) or non-parametric tests (Wilcoxon test).
#'
#' @param data A data frame containing the variables of interest.
#' @param variable A character string specifying the name of the variable in \code{data} for which the test will be conducted. Defaults to FALSE if not available.
#' @param id The identifier for each observation. Defaults to \code{"id"}.
#' @param group The grouping variable for comparing groups.
#' @param value The variable representing the values to be analyzed.
#' @param formula A formula specifying the relationship between variables.
#' @param sap.size The sample size threshold for determining whether to use parametric tests. Default is 30.
#' @param ... Additional arguments to be passed to the statistical tests.
#'
#' @return An S4 object of class \code{statresult}, which includes:
#' \item{stat}{A data frame containing the statistical test results.}
#' \item{normal}{A data frame with normality test results for each group.}
#' \item{p_position}{A data frame indicating the position of p-values for visualization purposes.}
#'
#' @importFrom rstatix levene_test
#' @importFrom stats bartlett.test shapiro.test median sd
#' @importFrom dplyr filter %>% mutate sym select all_of
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Load data
#' data("ToothGrowth")
#'
#' df <- ToothGrowth %>%
#' filter(dose %in% c("0.5", "1"))
#'
#' result <- stat2(data = df, variable = "supp",
#' id = "VC", group = "dose", value = "len",
#' formula = len ~ dose)
#'
#' # Inspect results
#' result@stat
#' result@normal
#' result@p_position
#' }

stat2 <- function(data,
                  group,
                  value,
                  formula,
                  variable = FALSE,
                  id = "id",
                  sap.size = 30, ...){

  if (variable == FALSE) {
    data <- mutate(data, variable= "id")
    variable  <- "variable"
  }

  vari  <- sym(variable)
  gro <- sym(group)
  valu  <- sym(value)

  nordis_data <- data.frame(group = character(),
                            variable = character(),
                            normal = logical(),
                            meanvalue = numeric(),
                            sd = numeric())

  for (i in 1:length(unique(data[[group]]))) {
    test_data <- data %>%
      filter(!!gro == unique(!!gro)[i]) %>%
      filter(!!vari == id) %>%
      select(all_of(valu)) %>%
      unlist() %>%
      as.numeric()



    Ntest <- if (length(unique(test_data)) == 1) FALSE else shapiro.test(test_data)$p.value > 0.05


    new_row <- data.frame(group = unique(data[[group]])[i],
                          variable = id,
                          normal =  Ntest,
                          meanvalue = mean(test_data),
                          sd = sd(test_data))

    nordis_data <- rbind(nordis_data, new_row)
  }


  if (exists("nordata_save")) {


    nordata_save <- rbind(nordata_save, nordis_data)
  } else {

    nordata_save <- nordis_data
  }

  if (sum(nordis_data$normal) > 0) {
    if (sum(nordis_data$normal) == 2) {
      cat("Normally distributed  \n")

      Vtest <- data %>%
        filter(!!vari == id) %>%
        bartlett.test(data=., formula) %>%
        .$p.value > 0.05
      if (Vtest) {
        cat("Variance equal  \n")
        cat(" t-test\n")
        stat2result <- tf_p(id = id,data = data,variable = variable,formula = formula)
      }else{
        cat("Variance unequal  \n")
        cat(" welch's t-test\n")
        stat2result <-weltf_p(id = id,data = data,variable = variable,formula = formula)
      }
    }
    if (sum(nordis_data$normal) == 1) {
      cat("Not all normally distributed  \n")
      truesap_size <- min(as.vector(table(data %>%
                                            filter(!!vari == id) %>%
                                            .[[group]])))
      if (truesap_size >= sap.size) {
        cat(paste0("Sample Size: ",truesap_size,"\n"))

        data[[group]] <- as.character(data[[group]])
        Vtest <- data %>%
          filter(!!vari == id) %>%
          rstatix::levene_test(. , formula = formula, center = median)  %>%
          .$p > 0.05
        if (Vtest) {
          cat("Variance equal  \n")
          cat(" welch's t-test\n")
          stat2result <- weltf_p(id = id,data = data,variable = variable,formula = formula)
        }else{
          cat("Variance unequal  \n")
          cat(" wilcoxon test\n")
          stat2result <- wf_p(id = id,data = data,variable = variable,formula = formula)
        }
      }else{
        cat(paste0("Sample size: ",truesap_size,"\n"))
        cat(" wilcoxon test\n")
        stat2result <- wf_p(id = id,data = data,variable = variable,formula = formula)
      }
    }
  }else{
    cat("Non-normally distributed  \n")
    cat(" wilcoxon test\n")
    stat2result <- wf_p(id = id,data = data,variable = variable,formula = formula)
  }
  max_value <- max(as.numeric(data[,value]))
  p_position <- stat2result[,1:2] %>%
    mutate(y.position = ifelse(max_value >0,max_value*1.12,
                               ifelse (abs(max_value) >= 1,1.12,
                                       abs(max_value)*1.12 )))
  statresult <- new("statresult",stat=as.data.frame(stat2result),
                    normal = as.data.frame(nordata_save),
                    p_position = as.data.frame(p_position))

  cat(paste0("p = ",stat2result$p))
  return(statresult)
}
