install.packages("ggplot2")        # Install ggplot2 package
library("ggplot2")                 # Load ggplot2 package

# Read in data
data <- read.csv(file = "bodyfatmen.csv")
fit <- lm(formula = density ~ ., data = data)
data$predicted <- predict(object = fit)
data$residuals <- MASS::studres(fit)
data$r.student <- rstudent(model = fit)

# Normality plot of residuals
library("ggplot2")
CreateQQPlot <- function(data) {
  plt <- ggplot(data, aes(sample = residuals))+
    stat_qq()+
    stat_qq_line() +
    ylab("Studentized residuals") +
    xlab("Theoretical quantiles")
  return(plt)
}
CreateQQPlot(data)

# Fitted values against rstudent residuals
CreateFittedAgainstResidualsPlot <- function(data) {
  plt <- ggplot(data, aes(x = predicted, y = r.student)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    xlab("Fitted values") +
    ylab("R-student residuals")
  return(plt)
}
CreateFittedAgainstResidualsPlot(data)

# Partial regression plots
# hacky solution because we couldn't fit more than 9 on one image.
install.packages("car")  
library(car)  # avPlots
CreateAddedVariablePlots <- function(fit) {
  avPlots(fit, ~ )
}
CreateAddedVariablePlots(fit)

CreateAddedVariablePlots <- function(fit) {
  avPlots(fit, ~ankle + biceps + forearm + wrist )
}
CreateAddedVariablePlots(fit)

# Anova significance test
# Copy output to latex table generator
install.packages("knitr")  
library(knitr)
anova_result = anova(fit)
kable(anova_result, digits = 3)  


#Box cox plot
install.packages('lindia')
library(lindia)
CreateBoxCoxPlot <- function(fit) {
  
  plt <- gg_boxcox(fit)

  return (plt)
}
CreateBoxCoxPlot(fit)

# From above we see that we should use lambda = 0.9
install.packages('MASS')
library(MASS)
CreateTransformedQQPlot <- function(fit, data, lambda = 0.9) {
  iv <- names(data)[!names(data) %in% "density"]
  iv.f <- paste(iv, collapse=" + ")
  tf <- paste0("(", "density", "^lambda - 1)/lambda", "~", iv.f)
  new.fit <- lm(formula = as.formula(tf), data = data)
  res <- data.frame(residuals = studres(new.fit))
  plt <- CreateQQPlot(res) + ggtitle("Q-Q plot")

  return (plt)
}

CreateTransformedQQPlot(fit, data)

# Multicolinerarity correlation matrix
install.packages("corrplot")
library(corrplot)
df_multi = data[ , !(names(data) %in% c("density", "residuals", "r.student", "predicted"))]
corrplot(cor(df_multi))

# Multi col measures (in overleaf table)
names = names(df_multi)
eigen_v = eigen(cor(df_multi))$values
vif_v = vif(fit)

# Add combo variable for the variables that have high coorelation
# Update data and rerun same things.
install.packages('dplyr')
library(dplyr)
data <- read.csv(file = "bodyfatmen.csv")
data = data %>%
  mutate(combo = (hip*thigh*abdomen)/weight) %>%
  select(-c(thigh, abdomen, weight, hip))

#Update Fit with combo variable
fit <- lm(formula = density ~ ., data = data)
data$predicted <- predict(fit)
data$residuals <- MASS::studres(fit)
data$r.student <- rstudent(fit)
# Run same things as before:
CreateQQPlot(data)
CreateFittedAgainstResidualsPlot(data)
anova_result = anova(fit)
kable(anova_result, digits = 3)
vif_v = vif(fit)
df_multi = data[ , !(names(data) %in% c("density", "residuals", "r.student", "predicted"))]
eigen_v = eigen(cor(df_multi))$values

#Handling of outliers
install.packages('olsrr')
library(olsrr)

ols_plot_cooksd_bar(fit)
ols_plot_dffits(fit)
ols_plot_dfbetas(fit)
