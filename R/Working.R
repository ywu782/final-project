### Working Document ###

library(here)
library(gtsummary)
library(tidyverse)
library(ggplot2)

# Create a {gtsummary} table of descriptive statistics about your data (1 pt)
load(here('data','diabetes.rda'))

colnames(diabetes) <- c('pregnancy', 'glucose', 'dbp', 'triceps', 'insulin',
												'bmi', 'pedigree', 'age', 'diabetes_status')

diabetes <- mutate(diabetes,
									 diabetes_status = factor(diabetes_status,
									 												 labels = c('Negative', 'Positive')))

tbl_summary(
	diabetes,
	by = diabetes_status,
	include = c(age, pregnancy, glucose, dbp, triceps,
							insulin, bmi, pedigree))

# Fit a regression and present well-formatted results from the regression (1 pt)
logistic_model <- glm( diabetes_status ~ pregnancy + glucose + bmi + age,
											data = diabetes, family = binomial())
tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		pregnancy ~ "Number of Pregnancy",
		glucose ~ "Plasma Glucose Concentration",
		bmi ~ "BMI",
		age ~ "Age"
	))

# Create a figure (1 pt)
hist(diabetes$bmi)

# Write and use a function that does something with the data (1 pt)
calculate_variance <- function(df, column_name) {
	column_data <- df[[column_name]]

	if (!is.numeric(column_data)) {
		stop("The specified column is not numeric.")
	}

	n <- length(column_data)
	mean_x <- mean(column_data, na.rm = TRUE)
	squared_diff <- (column_data - mean_x)^2
	var_x <- sum(squared_diff, na.rm = TRUE) / (n - 1)

	return(var_x)
}

calculate_variance(diabetes, 'bmi')

# Use the {here} package again (1 pt)
write.csv(diabetes, here("data", "new_diabetes.csv"), row.names = FALSE)





