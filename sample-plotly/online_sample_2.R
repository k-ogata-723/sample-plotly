library("plotly")

Sys.setenv("plotly_username"="koki.ogata")
Sys.setenv("plotly_api_key"="qZ8FtXjmxDahUU6fenXG")

py <- plotly(username = "XXXXXXX", key = "XXXXXXX")
ggiris <- qplot(Petal.Width, Sepal.Length, data = iris, color = Species) #ggplotデータの作成

#plotlyへデータを送信
r <- py$plotly(ggiris)

#プロットをブラウザで表示
browseURL(r$url)
