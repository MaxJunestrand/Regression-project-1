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
library(car)  # avPlots
CreateAddedVariablePlots <- function(fit) {
  avPlots(fit, ~ )
}
CreateAddedVariablePlots(fit)

CreateAddedVariablePlots <- function(fit) {
  avPlots(fit, ~ankle + biceps + forearm + wrist )
}
CreateAddedVariablePlots(fit)



