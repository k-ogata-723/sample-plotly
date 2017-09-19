library(plotly)

Sys.setenv("plotly_username"="koki.ogata")
Sys.setenv("plotly_api_key"="qZ8FtXjmxDahUU6fenXG")

p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
api_create(p, filename = "r-docs/midwest-boxplots")

# 下記のエラーが発生する。
# 有料会員じゃないとオンラインモードは使えない？

# エラー: Client error: (402) Payment Required
#  To create folders, you need to upgrade your subscription. Upgrade your account to save folders, private files, and more: https://plot.ly/settings/subscription
