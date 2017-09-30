library('glmnet')
library('ggplot2')
library('plotly')

# データを学習データとテストデータに分けて、交差検定をおこなう
# ちないみに、データのyieldは出来高で、buy-sellは売買代金

stock_2015 <- read.csv("stocks-sony-2015.csv")
# typeofを使えば、データ型を確認できる
# typeof(stock_2015)
# 日付で昇順にする
stock_2015 <- stock_2015[order(stock_2015[,1]),]

stock_2016 <- read.csv("stocks-sony-2016.csv")
stock_2016 <- stock_2016[order(stock_2016[,1]),]

stock_2017 <- read.csv("stocks-sony-2017.csv")
stock_2017 <- stock_2017[order(stock_2017[,1]),]

stock <- rbind(stock_2015, stock_2016, stock_2017)

# orderをすると、row numberがあべこべになる
# 必要かわからないがrownamesを使って行番号を振り直す
rownames(stock) <- c(1:nrow(stock))

print(stock)

# yieldを使ってendを予測する
x <- stock[, 6]
y <- stock[, 5]


n <- length(x)

###
## 以下、ランダムにトレーニングデータとテストデータを分ける方法
## データをtraingデータとtestデータにランダムな割合に分割するためにindicesを生成
## printしてみるとランダムに抜き出していて、時系列の解析には合わない？？
# indices <- sort(sample(1:n, round(0.5 * n)))
# print(indices)
###

indices <- 420

# 始めからindices個のデータをトレーニングデータとして利用する
training.x <- x[1:indices]
training.y <- y[1:indices]


hoge.df <- data.frame(X = training.x, Y = training.y)
print(hoge.df)

print(training.x)
print(training.y)

test.x <- x[indices:n]
test.y <- y[indices:n]

print(test.x)
print(test.y)

# テストデータとトレーニングデータのデータフレームを作成する
# rでは、data.frameは色々な方が入れられて、matrixは1種類の型しか入れられないらしい
training.df <- data.frame(X = training.x, Y = training.y)
test.df <- data.frame(X = test.x, Y = test.y)

# 以下、RMSE（平均２乗誤差、root mean square error）のコードを作成する
# degree（関数の次元）を増やしていって、テストデータとトレーニングデータの学習曲線を比較
# まず、コストを出す関数を自作
rmse <- function(y, h) {
    return(sqrt(mean((y - h) ^ 2)))
}

performance <- data.frame()

# degreeを1から12まで増やしつつ、計算していく
for (d in 1:12)
{
  poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)

  performance <- rbind(performance,
                      data.frame(Degree = d,
                                  Data = 'Training',
                                  RMSE = rmse(training.y, predict(poly.fit))))

  performance <- rbind(performance,
                      data.frame(Degree = d,
                                  Data = 'Test',
                                  RMSE = rmse(test.y, predict(poly.fit,
                                              newdata = test.df))))
}

p <- ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
  geom_point() +
  geom_line()


p <- ggplotly(p)

p

# pの結果を見る限りだと、degreeは3~9の間が良さそう
# 真ん中の5あたりを使ってみる

p.lm <- lm(Y ~ poly(X, degree = 5), data = training.df)
predict.p <- transform(training.df , Predict.traning.y = predict(p.lm))

plot.predict.p <- ggplot(predict.p, aes(x = X, y = Y)) +
  geom_point() +
  geom_line()

plotly.predict.p <- ggplotly(plot.predict.p)

print(plotly.predict.p)