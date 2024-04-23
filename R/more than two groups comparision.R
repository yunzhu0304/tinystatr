#' @title Perform Statistical Test for More Than Two-group Comparison
#' @description
#'     Depending on the characteristics of the data, it automatically determines
#'     whether to use parametric tests (Anova, HSD(post-hoc)) or non-parametric
#'     tests (kruskal, Dunn's(post-hoc)).
#'
#' @param data A data frame containing the variables of interest.
#' @param variable A character string specifying the name of the variable in \code{data} for which the test will be conducted. Ignore if not available.
#' @param id The identifier for each observation.A value to filter the \code{variable} by. Ignore if not available.
#' @param group The grouping variable for comparing groups.
#' @param value The variable representing the values to be analyzed.
#' @param formula A formula specifying the relationship between variables.An optional formula specifying the structure of the statistical model to be tested.
#' @param ... Additional arguments to be passed to the statistical tests.
#'
#' @return A data frame containing the results of the statistical test.
#' @importFrom dplyr mutate select filter bind_rows
#' @importFrom stats shapiro.test bartlett.test
#' @importFrom rstatix add_significance
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Load data
#' # Dataframe with multiple columns, need to filter variable
#' data("ToothGrowth")
#'
#' df <- ToothGrowth
#' stat3(data = df, group = "dose", value = "len", variable = "supp", id = "OJ", formula = len ~ dose)
#'
#' #:::::::::::::::::::::::::::::::::::::::
#' # Dataframe with only two columns (group,value)
#' data("HairEyeColor")
#' df <- as.data.frame(HairEyeColor)[,c(2,4)]
#'
#' stat3(data = df,group = "Eye",value = "Freq", formula = Freq ~ Eye) # Ignoring variable and id
#' }
#'
stat3 <- function(data,
                  group,
                  value,
                  formula,
                  variable = FALSE,
                  id = "id", ...){

  if (variable == FALSE) {
    data <- mutate(data, variable= "id")
    variable  <- "variable"
  }

  vari  <- sym(variable)
  gro <- sym(group)
  valu  <- sym(value)

  stat3result <<- list()

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

  if (sum(nordis_data$normal) == length(unique(data[[group]]))) {
    cat("Normally distributed  \n")

    Vtest <- data %>%
      filter(!!vari == id) %>%
      bartlett.test(data=., formula) %>%
      .$p.value > 0.05
    if (Vtest) {
      cat("Variance equal  \n")
      cat(" Anova\n")
      stat3result[["stat"]] <<- hsd_p(data = data, group = group,
                                      variable = variable, id = id,
                                      formula = formula) %>%
                                      add_significance("p.adj")
    }else{
      cat("Variance unequal  \n")
      cat("Kruskal-Wallis \n")
      stat3result[["stat"]] <<-dunn_p(data = data, group = group,
                                      variable = variable, id = id,
                                      formula = formula)%>%
                                      add_significance("p.adj")
    }
  }else{
    cat("Non-normally distributed  \n")
    cat("Variance unequal  \n")
    cat("Kruskal-Wallis \n")
    stat3result[["stat"]] <<-dunn_p(data = data, group = group,
                                    variable = variable, id = id,
                                    formula = formula)%>%
                                    add_significance("p.adj")
  }

  stat3result[["normal"]] <<- nordata_save
  return(stat3result[["stat"]])
}
