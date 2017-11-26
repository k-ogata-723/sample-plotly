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

indices <- 300

training.y <- stock[1:indices, 5]
# yieldをxに取り出し
training.x <- stock[1:indices, 6]
# buy-sellをx1に取り出し
training.x1 <- stock[1:indices, 7]

training.df <- data.frame(X = training.x, X1 = training.x1, Y = training.y)

n <- length(stock)

test.y <- stock[indices:n, 5]
# yieldをxに取り出し
test.x <- stock[indices:n, 6]
# buy-sellをx1に取り出し
test.x1 <- stock[indices:n, 7]

test.df <- data.frame(X = test.x, X1 = test.x1, Y = test.y)

rmse <- function(y, h) {
  return(sqrt(mean((y - h) ^ 2)))
}

performance <- data.frame()

# lmで複数の説明変数でpoly的なことをしたければ、polymを使えばいいらしい
# 以下、polymolistic linear regressionをpolymを使って回していく

for (d in 1:12) {
  polym.fit <- lm(Y ~ polym(X, X1, degree = d), data = training.df)

  # print(polym.fit)
  # print(class(polym.fit)) # データ型を確認する
  # str(polym.fit) # str関すでは、オブジェクトの中身の概要を確認できる。詳細を確認する時にはsummary関数を利用する


  performance <- rbind(performance,
                      data.frame(Degree = d,
                                  Data = 'Training',
                                  RMSE = rmse(training.y, predict(polym.fit))))

  performance <- rbind(performance,
                      data.frame(Degree = d,
                                  Data = 'Test',
                                  RMSE = rmse(test.y, predict(polym.fit,
                                              newdata = test.df))))
}

print(performance)

p <- ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
  geom_point() +
  geom_line()

p <- ggplotly(p)

p


# #### 以下、yieldのみを使ってendを予測したモデル
# # yieldを使ってendを予測する
# x <- stock[, 6]
# y <- stock[, 5]
#
# n <- length(x)
#
# ###
# ## 以下、ランダムにトレーニングデータとテストデータを分ける方法
# ## データをtraingデータとtestデータにランダムな割合に分割するためにindicesを生成
# ## printしてみるとランダムに抜き出していて、時系列の解析には合わない？？
# # indices <- sort(sample(1:n, round(0.5 * n)))
# # print(indices)
# ###
#
# indices <- 420
#
# # 始めからindices個のデータをトレーニングデータとして利用する
# training.x <- x[1:indices]
# training.y <- y[1:indices]
#
#
# hoge.df <- data.frame(X = training.x, Y = training.y)
# print(hoge.df)
#
# print(training.x)
# print(training.y)
#
# test.x <- x[indices:n]
# test.y <- y[indices:n]
#
# print(test.x)
# print(test.y)
#
# # テストデータとトレーニングデータのデータフレームを作成する
# # rでは、data.frameは色々な方が入れられて、matrixは1種類の型しか入れられないらしい
# training.df <- data.frame(X = training.x, Y = training.y)
# test.df <- data.frame(X = test.x, Y = test.y)
#
# # # training.dfを単にplotしてみる
# # ttt <- ggplot(training.df, aes(x = X, y = Y)) +
# #   geom_point() +
# #   geom_line()
# #
# # print(ttt)

#
# # 以下、RMSE（平均２乗誤差、root mean square error）のコードを作成する
# # degree（関数の次元）を増やしていって、テストデータとトレーニングデータの学習曲線を比較
# # まず、コストを出す関数を自作
# rmse <- function(y, h) {
#     return(sqrt(mean((y - h) ^ 2)))
# }
#
# performance <- data.frame()
#
# # degreeを1から12まで増やしつつ、計算していく
# for (d in 1:12)
# {
#   poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)
#
#   performance <- rbind(performance,
#                       data.frame(Degree = d,
#                                   Data = 'Training',
#                                   RMSE = rmse(training.y, predict(poly.fit))))
#
#   performance <- rbind(performance,
#                       data.frame(Degree = d,
#                                   Data = 'Test',
#                                   RMSE = rmse(test.y, predict(poly.fit,
#                                               newdata = test.df))))
# }
#
# p <- ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
#   geom_point() +
#   geom_line()
#
#
# p <- ggplotly(p)
#
# p
#
# # pの結果を見る限りだと、degreeは3~9の間が良さそう
# # 真ん中の5あたりを使ってみる
#
# p.lm <- lm(Y ~ poly(X, degree = 5), data = training.df)
#
# # # predict関数を利用すると、p.lmの結果を利用して、test.dfの予測値がだされる
# # # predictに関する使い方は、下記の記事がいい感じ
# # # https://qiita.com/stkdev/items/f79af90db4799370e3aa
# # predict(p.lm, newdata = test.df)
#
# # transform関数でtraining.dfにPredict.training.yを追加する
# # predict(p.lm)は、predict(p.lm, newdata = training.df)となる
# # つまり、predictでデータを指定しなければ、lmで使ったデータについて出力される
# predict.p <- transform(training.df , Predict.training.y = predict(p.lm))
#
# # plotした時に、桁数の大きい数字が指数表示(2e+1とか)にならないように設定する
# options(scipen=100000)
#
# # yieldを使って、endが予測されている
# # ただ、あまり良くない結果っぽい
# plot.predict.p <- ggplot(predict.p, aes(x = X, y = Predict.training.y)) +
#   geom_point() +
#   geom_line()
#
# plotly.predict.p <- ggplotly(plot.predict.p)
#
# print(plotly.predict.p)
#
# #### ここまで、yieldのみを使ってendを予測したモデル
