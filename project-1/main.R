install.packages("ggplot")        # Install ggplot2 package
library("ggplot")                 # Load ggplot2 package

# Read in data
data <- read.csv(file = "bodyfatmen.csv")
fit <- lm(formula = density ~ ., data = data)
data$predicted <- predict(object = fit)
data$residuals <- MASS::studres(fit)
data$r.student <- rstudent(model = fit)

# Normality plot of residuals
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

