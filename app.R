library(ggplot2)

library(shiny)

drawsin <- function(intercpet = 0, rotate_time = 1, x_intercept = 0, wide = 1){
  temp_value <- rep(0, 5000)
  for(i in 1:5000){
    temp_value[i] <- intercpet + wide * sin(0.001 * rotate_time * pi * (i - x_intercept))
  }
  return(temp_value)
}

createplot <- function(i1 = 0, i2 = 0, i3 = 0, i4 = 0, 
                       r1 = 1, r2 = 1, r3 = 1, r4 = 1,
                       x1 = 0, x2 = 0, x3 = 0, x4 = 0,
                       w1 = 1, w2 = 1, w3 = 1, w4 = 1){
  result_vec <- 
    drawsin(intercpet = i1, rotate_time = r1, x_intercept = x1, wide = w1) + 
    drawsin(intercpet = i2, rotate_time = r2, x_intercept = x2, wide = w2) + 
    drawsin(intercpet = i3, rotate_time = r3, x_intercept = x3, wide = w3) + 
    drawsin(intercpet = i4, rotate_time = r4, x_intercept = x4, wide = w4)
  plotdf_temp <- data.frame(c(1:5000), result_vec)
  names(plotdf_temp) <- c("time", "value")
  return(ggplot(plotdf_temp, aes(x = time, y = value)) + geom_line())
}

ui <- fluidPage(
  titlePanel("Shiny Fourier series"),
  sidebarPanel(
    h3("1st Curve"),
    sliderInput("i1", h4("Y Intercept"),
                min = 0, max = 10, value = 0),
    sliderInput("r1", h4("Frequency"),
                min = 1, max = 10, value = 1),
    sliderInput("x1", h4("X Intercept"),
                min = 0, max = 10, value = 0),
    sliderInput("w1", h4("Amplitude"),
                min = 0, max = 10, value = 1),
    h3("2nd Curve"),
    sliderInput("i2", h4("Y Intercept"),
                min = 0, max = 10, value = 0),
    sliderInput("r2", h4("Frequency"),
                min = 1, max = 10, value = 1),
    sliderInput("x2", h4("X Intercept"),
                min = 0, max = 10, value = 0),
    sliderInput("w2", h4("Amplitude"),
                min = 0, max = 10, value = 1),
    h3("3rd Curve"),
    sliderInput("i3", h4("Y Intercept"),
                min = 0, max = 10, value = 0),
    sliderInput("r3", h4("Frequency"),
                min = 1, max = 10, value = 1),
    sliderInput("x3", h4("X Intercept"),
                min = 0, max = 10, value = 0),
    sliderInput("w3", h4("Amplitude"),
                min = 0, max = 10, value = 1),
    h3("4th Curve"),
    sliderInput("i4", h4("Y Intercept"),
                min = 0, max = 10, value = 0),
    sliderInput("r4", h4("Frequency"),
                min = 1, max = 10, value = 1),
    sliderInput("x4", h4("X Intercept"),
                min = 0, max = 10, value = 0),
    sliderInput("w4", h4("Amplitude"),
                min = 0, max = 10, value = 1)
  ),
  mainPanel(
    fluidRow(
      column(width = 5, offset = 7,
             h3("Made by Ruoyan", style = "color:#163883")
      )),
    plotOutput("main_plot")
  )
)

server <- function(input, output){
  # output$main_plot <- renderPlot({createplot(
  #   input$i1, input$i2, input$i3, input$i4,1,2,3,4,1,2,3,4,1,2,3,4
  # )})
  output$main_plot <- renderPlot({createplot(
    input$i1, input$i2, input$i3, input$i4,
    input$r1, input$r2, input$r3, input$r4,
    input$x1, input$x2, input$x3, input$x4,
    input$w1, input$w2, input$w3, input$w4
  )})
}
shinyApp(ui, server)