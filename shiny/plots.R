library(tidyr)
library(dplyr)
library(highcharter)

aucDotPlot <- function(aucData,
                       title = "",
                       subtitle = "",
                       prefix = NULL,
                       clicked = NULL)
{
  aucData <- aucData %>%
    arrange(AUC) %>%
    mutate(RowNumber = 0)

  if (!is.null(prefix)) aucData <- aucData %>% mutate(Label = paste(prefix, ": ", ID, sep = ""))

  n <- nrow(aucData)
  if (n > 0) aucData <- aucData %>% mutate(RowNumber = 1:n)

  dt <- aucData %>% transmute(
    x = AUC, y = RowNumber,
    id = ID,
    label = Label,
    auc = AUC
    # auc = round(AUC, digits = 2)
  )
  ds <- dt %>% list_parse

  labels <- data_frame(
    x = c(0.1, 0.9),
    y = c(n + 10, n + 10),
    text = c("Resistant", "Sensitive")
  ) %>% list_parse

  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(
      title = list(text = "AUC"),
      gridLineWidth = 1,
      min = 0.0,
      max = 1.0,
      plotLines = list(list(color = "#FF0000", width = 2, value = 0.2))
    ) %>%
    hc_yAxis(
      title = list(text = NULL),
      lineWidth = 1,
      gridLineWidth = 0,
      labels = list(enabled = FALSE)
    ) %>%
    hc_chart(zoomType = "x") %>%
    hc_add_series(data = ds, type = "scatter", showInLegend = FALSE) %>%
    hc_add_series(
      data = labels, name = "annotations", type = "scatter", color = "transparent",
      showInLegend = FALSE, enableMouseTracking = FALSE,
      dataLabels = list(
        enabled = TRUE,
        y = 10,
        format = "{point.text}",
        style = list(fontSize = "11px", color =  'rgba(0, 0, 0, 0.7)')
      )
    ) %>%
    hc_tooltip(formatter = JS("function() { return (this.point.label + '<br>AUC: ' + this.point.auc) }"))

  if (!is.null(clicked))
  {
    fn <- paste("function() { Shiny.onInputChange('", clicked, "', this.id) }", sep = "")
    hc <- hc %>%
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(
            events = list(
              click = JS(fn)
            )
          )
        )
      )
  }

  hc
}


# function for creating a highchart bar plot
# expects data in the form of a vector of values
barPlot <- function(values,
                    title = "",
                    subtitle = "",
                    categories = NULL,
                    colours = NULL
)
{
  counts <- count(data_frame(Value = values), Value)

  if (is.null(categories)) categories <- counts$Value

  counts <- counts %>%
    right_join(data_frame(Value = categories), by = "Value") %>%
    mutate(n = ifelse(is.na(n), 0, n))

  highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(categories = categories) %>%
    hc_yAxis(title = list(text = "")) %>%
    hc_add_series(data = counts$n, colorByPoint = TRUE, showInLegend = FALSE) %>%
    hc_colors(colours) %>%
    hc_tooltip(formatter = JS("function() { return (this.x + ': ' + this.y) }"))
}


bubblePlot <- function(data,
                       title = "",
                       subtitle = "",
                       xLabel = "x",
                       yLabel = "y",
                       series = NULL,
                       colours = NULL,
                       showInLegend = TRUE,
                       xLine = NULL, xLineColour = "#FF0000", xLineWidth = 2,
                       yLine = NULL, yLineColour = "#FF0000", yLineWidth = 2,
                       minSize = "2%",
                       maxSize = "10%",
                       clicked = NULL)
{
  hc <- highchart() %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle)

  if (is.null(xLine))
    hc <- hc %>% hc_xAxis(title = list(text = xLabel))
  else
    hc <- hc %>% hc_xAxis(
      title = list(text = xLabel),
      plotLines = list(list(color = xLineColour, width = xLineWidth, value = xLine))
    )

  if (is.null(yLine))
    hc <- hc %>% hc_yAxis(title = list(text = yLabel))
  else
    hc <- hc %>% hc_yAxis(
      title = list(text = yLabel),
      plotLines = list(list(color = yLineColour, width = yLineWidth, value = yLine))
    )

  hc <- hc %>% hc_chart(zoomType = "xy")

  if (is.null(series))
    series <- data %>% select(series) %>% distinct %>% arrange(series) %>% unlist %>% as.character

  zIndex <- length(series)
  for (i in series)
  {
    seriesData <- data %>% filter(series == i) %>% select(-series) %>% list_parse
    hc <- hc %>% hc_add_series(data = seriesData, name = i, type = "bubble", showInLegend = showInLegend, zIndex = zIndex)
    zIndex <- zIndex - 1
  }

  if (!is.null(colours)) hc <- hc %>% hc_colors(colours)

  hc <- hc %>% hc_plotOptions(bubble = list(minSize = minSize, maxSize = maxSize, color = ""))

  hc <- hc %>% hc_tooltip(formatter = JS("function() { return (this.point.tooltip) }"))

  if (!is.null(clicked))
  {
    fn <- paste("function() { Shiny.onInputChange('", clicked, "', this.id) }", sep = "")
    hc <- hc %>%
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(
            events = list(
              click = JS(fn)
            )
          )
        )
      )
  }

  hc
}
