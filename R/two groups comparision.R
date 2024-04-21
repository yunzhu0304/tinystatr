#' @title Perform statistical tests for two-group comparison
#' @description
#'     Depending on the characteristics of the data, it automatically determines whether to use parametric tests (t-test, Welch's t-test) or non-parametric tests (Wilcoxon test).
#'
#' @param data A data frame containing the variables of interest.
#' @param variable A character string specifying the name of the variable in \code{data} for which the test will be conducted. Ignore if not available.
#' @param id The identifier for each observation.A value to filter the \code{variable} by. Ignore if not available.
#' @param group The grouping variable for comparing groups.
#' @param value The variable representing the values to be analyzed.
#' @param formula A formula specifying the relationship between variables.An optional formula specifying the structure of the statistical model to be tested.
#' @param sap.size The sample size threshold for determining whether to use parametric tests. Default is 30.
#' @param ... Additional arguments to be passed to the statistical tests.
#'
#' @return  A list containing the results of the statistical tests.
#'
#' @importFrom rstatix levene_test
#' @importFrom stats bartlett.test shapiro.test median sd
#'
#' @importFrom dplyr filter %>% mutate sym select all_of
#'
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Load data
#' #:::::::::::::::::::::::::::::::::::::::
#' # Dataframe with multiple columns, need to filter variable
#' data("ToothGrowth")
#'
#' df <- ToothGrowth%>%
#' filter(dose %in% c("0.5","1"))
#'
#' stat2(data = df,variable = "supp",id="VC",group = "dose",value = "len",
#' formula = len ~ dose)
#'
#' #:::::::::::::::::::::::::::::::::::::::
#' # Dataframe with only two columns (group,value)
#' data("HairEyeColor")
#' df <- as.data.frame(HairEyeColor)[,c(3,4)]
#'
#' stat2(data = df,group = "Sex",value = "Freq", formula = Freq ~ Sex) # Ignoring variable and id
#' }

stat2 <- function(data, group, value, formula,variable = FALSE, id = "id",
                  sap.size = 30, ...){

  if (variable == FALSE) {
    data <- mutate(data, variable= "id")
    variable  <- "variable"
  }

  vari  <- sym(variable)
  gro <- sym(group)
  valu  <- sym(value)

  stat2result <<- list()

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
        stat2result[["stat"]] <<- tf_p(id = id,data = data,variable = variable,formula = formula)
      }else{
        cat("Variance unequal  \n")
        cat(" welch's t-test\n")
        stat2result[["stat"]] <<-weltf_p(id = id,data = data,variable = variable,formula = formula)
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
          stat2result[["stat"]] <<- weltf_p(id = id,data = data,variable = variable,formula = formula)
        }else{
          cat("Variance unequal  \n")
          cat(" wilcoxon test\n")
          stat2result[["stat"]] <<- wf_p(id = id,data = data,variable = variable,formula = formula)
        }
      }else{
        cat(paste0("Sample size: ",truesap_size,"\n"))
        cat(" wilcoxon test\n")
        stat2result[["stat"]] <<- wf_p(id = id,data = data,variable = variable,formula = formula)
      }
    }
  }else{
    cat("Non-normally distributed  \n")
    cat(" wilcoxon test\n")
    stat2result[["stat"]] <<- wf_p(id = id,data = data,variable = variable,formula = formula)
  }

  stat2result[["normal"]] <<- nordata_save
  return(stat2result[["stat"]])
}
