#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in RStudio.
#


library(shiny)

ui <- fluidPage(
  titlePanel("FBI Hate Crimes Statistics 2013, KNN-Classification model"),
  
 inputPanel(
  sliderInput(inputId = "a",
              label = "Race related crimes",
              value = 0, min = 0, max = 100),
  sliderInput(inputId = "b",
              label = "Religious related crimes",
              value = 0, min = 0, max = 100),
  sliderInput(inputId = "c",
              label = "Ethinicity related crimes",
              value = 0, min = 0, max = 100),
  sliderInput(inputId = "d",
              label = "Disability related crimes",
              value = 0, min = 0, max = 100),
  sliderInput(inputId = "e",
              label = "Gender related crimes",
              value = 0, min = 0, max = 100),
  sliderInput(inputId = "f",
              label = "Sexual orientation related crimes",
              value = 0, min = 0, max = 100),
  sliderInput(inputId = "g",
              label = "Gender_identity related crimes",
              value = 0, min = 0, max = 100)),
  verbatimTextOutput("str"),
  plotOutput("plot")
)

server <- function(input, output) {
  
 line_a <- visualization1 +
    ggtitle("Race") +
    geom_point(aes(y= input$a), colour = 'blue')
  line_b <- visualization2 +
    ggtitle("Religion") +
    geom_point(aes(y= input$b), colour = 'green')
  line_c <- visualization3 +
    ggtitle("Ethnicity") +
    geom_point(aes(y= input$c), colour = 'yellow')
  line_d <- visualization4 +
    ggtitle("Disability")+
    geom_point(aes(y= input$d), colour = 'pink')
   line_e <- visualization5 +
    ggtitle("Gender") +
    geom_point(aes(y= input$e), colour = 'orange')
   line_f <- visualization6 +
     ggtitle("Sexual_orientation") +
     geom_point(aes(y= input$f), colour = 'red')
    line_g <- visualization7 +
     ggtitle("Gender_identity") +
     geom_point(aes(y= input$g), colour = 'purple')
 
  output$plot<- renderPlot(grid.arrange(line_a, line_b, line_c,line_d,line_e, line_f,line_g, ncol = 4))

  output$str <- renderPrint({myprediction(input$a, input$b,input$c,input$d,input$e,
                                                input$f,input$g)})
}

shinyApp(server = server, ui = ui)
