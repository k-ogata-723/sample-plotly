library('plotly')
library('glmnet')
library('ggplot2')

set.seed(1)

x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

n <- length(x)

indices <- sort(sample(1:n, round(0.5 * n)))

training.x <- x[indices]
training.y <- y[indices]

test.x <- x[-indices]
test.y <- y[-indices]

df <- data.frame(X = x, Y = y)
training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

rmse <- function(y, h) {
  return(sqrt(mean((y - h) ^ 2)))
}

glmnet.fit <- with(training.df, glmnet(poly(X, degree = 10), Y))

lambdas <- glmnet.fit$lambda

performance <- data.frame()

for (lambda in lambdas) {
  performance <- rbind(performance,
    data.frame(Lambda = lambda,
                RMSE = rmse(test.y, with(test.df, predict(glmnet.fit, poly(X,
                            degree = 10), s = lambda)))))
}

p <- ggplot(performance, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line()

p <- ggplotly(p)

p
