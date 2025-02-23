library(FactoMineR)
library(explor)
library(shiny)

ui <- fluidPage(
  explor::explorUI("pca")
)

server <- function(input, output, session) {
  data(iris)
  res.pca <- PCA(iris[, 1:4], graph = FALSE)
  explor::explor("pca", res.pca)
}

shinyApp(ui, server)
