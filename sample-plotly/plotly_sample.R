library(plotly)

print(midwest)
p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
p
