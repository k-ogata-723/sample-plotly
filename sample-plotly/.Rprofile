# Sample Rprofile.site file

# Things you might want to change
# options(papersize="a4")
# options(editor="notepad")
# options(pager="internal")

# R interactive prompt
# options(prompt="> ")
# options(continue="+ ")

# to prefer Compiled HTML
help options(chmhelp=TRUE)
# to prefer HTML help
# options(htmlhelp=TRUE)

# General options
options(tab.width = 2)
options(width = 130)
options(graphics.record=TRUE)


.First <- function(){
 library(Hmisc)
 library(R2HTML)
 cat("\nWelcome at", date(), "\n")
}

# plotlyのusernameとapi_leyを読み込む
Sys.setenv("plotly_username"="koki.ogata")
Sys.setenv("plotly_api_key"="qZ8FtXjmxDahUU6fenXG")

.Last <- function(){
 cat("\nGoodbye at ", date(), "\n")
}
