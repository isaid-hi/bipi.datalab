library(tidyverse)
library(networkD3)
library(highcharter)


# Example data
data <- readxl::read_excel("C:/Users/acer/OneDrive/comps wiring check - said.xlsx", sheet = "data")
node <- readxl::read_excel("C:/Users/acer/OneDrive/comps wiring check - said.xlsx", sheet = "node")
data <- na.omit(data)


data <- data.frame(
  from = data$source,
  to = data$target,
  weight = data$value,
  id = paste0(data$source,data$target),
  series = rep(1, length(data$source))
)


label_style <- list(
  fontFamily = "Helvetica",
  color = "#666666",
  fontSize = "10px", # Modify the font size to make it more minimalistic
  backgroundColor = "none",
  textOutline = "none"
)


# Create the Sankey diagram
sankey <- highchart() %>%
  hc_chart(type = "sankey") %>%
  hc_add_series(
    data = data,
    type = "sankey",
    name = "Sankey",
    keys = c("from", "to", "weight", "series"),
    colorByPoint = FALSE,
    dataLabels = list(
      enabled = TRUE,
      style = label_style # Add the label_style to the dataLabels list
    )
  )
  

sankey
