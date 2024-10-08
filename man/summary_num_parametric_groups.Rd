% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_num_parametric_groups.R
\name{summary_num_parametric_groups}
\alias{summary_num_parametric_groups}
\title{summary_num_parametric_groups}
\usage{
summary_num_parametric_groups(df, col_num, col_cat, teste_extra = "F")
}
\arguments{
\item{df}{A data frame containing the data to be summarized.}

\item{col_num}{A string representing the name of the numeric variable to be summarized.}

\item{col_cat}{A string representing the name of the categorical variable that defines the groups.}

\item{teste_extra}{A character indicating whether to calculate Cohen's d effect size. Set to "T" to perform this calculation;
otherwise, no effect size is calculated. Default is "F".}
}
\value{
A data frame with the following columns:
\item{Variable}{The name of the numeric variable.}
\item{mean (std)}{The mean and standard deviation of the numeric variable for each group and overall.}
\item{P-value}{The p-value from the statistical test (T-Test or ANOVA).}
\item{Test_Used}{The type of statistical test applied ("T Test" or "Anova").}
\item{teste_extra}{The Cohen's d effect size and confidence interval, if calculated.}
}
\description{
Generates a summary table of mean and standard deviation for a numeric variable across groups, with optional effect size calculation.
}
\details{
This function calculates the mean and standard deviation of a numeric variable within groups defined by a categorical variable.
It can also perform statistical tests (T-Test for two groups or ANOVA for more than two groups) and optionally calculate Cohen's d
effect size for pairwise comparisons.
}
\examples{
# Example of using the summary_num_parametric_groups function
data <- data.frame(value = c(1.2, 2.3, 3.1, 4.7, 5.0, 2.1, 3.2, 4.8, 5.6, 6.7),
                   group = c("A", "B", "A", "B", "A", "A", "B", "B", "A", "B"))
summary_num_parametric_groups(data, 'value', 'group', teste_extra = "T")
# Returns a summary table with mean, standard deviation, p-value, test used, and Cohen's d effect.
}
