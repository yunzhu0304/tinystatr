% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/two groups comparision.R
\name{stat2}
\alias{stat2}
\title{Perform statistical tests for two-group comparison}
\usage{
stat2(
  data,
  group,
  value,
  formula,
  variable = FALSE,
  id = "id",
  sap.size = 30,
  ...
)
}
\arguments{
\item{data}{A data frame containing the variables of interest.}

\item{group}{The grouping variable for comparing groups.}

\item{value}{The variable representing the values to be analyzed.}

\item{formula}{A formula specifying the relationship between variables.}

\item{variable}{A character string specifying the name of the variable in \code{data} for which the test will be conducted. Defaults to FALSE if not available.}

\item{id}{The identifier for each observation. Defaults to \code{"id"}.}

\item{sap.size}{The sample size threshold for determining whether to use parametric tests. Default is 30.}

\item{...}{Additional arguments to be passed to the statistical tests.}
}
\value{
An S4 object of class \code{statresult}, which includes:
\item{stat}{A data frame containing the statistical test results.}
\item{normal}{A data frame with normality test results for each group.}
\item{p_position}{A data frame indicating the position of p-values for visualization purposes.}
}
\description{
\if{html}{\out{<div class="sourceCode">}}\preformatted{Depending on the characteristics of the data, it automatically determines whether to use parametric tests (t-test, Welch's t-test) or non-parametric tests (Wilcoxon test).
}\if{html}{\out{</div>}}
}
\examples{
\donttest{
# Load data
data("ToothGrowth")

df <- ToothGrowth \%>\%
filter(dose \%in\% c("0.5", "1"))

result <- stat2(data = df, variable = "supp",
id = "VC", group = "dose", value = "len",
formula = len ~ dose)

# Inspect results
result@stat
result@normal
result@p_position
}
}
