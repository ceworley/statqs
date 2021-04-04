library(shiny)
library(latex2exp)

# Define UI for application that draws a histogram
ui <- fluidPage(
    numericInput("mu","mean",0),
    numericInput("sig","standard deviation",1),
    radioButtons("type","Probability Type:",
                 c("Left" = "l",
                   "Right" = "r",
                   "Center" = "c",
                   "Two-tail" = "t",
                   "Between two zs" = "b")),
    conditionalPanel("input.type == 'l' | input.type == 'r' | input.type == 'c' | input.type == 't'",
                     sliderInput("z","z:",-3,3,1.96,0.01),
                     textInput("zt","",""),
                     actionButton("b","enter")),
    conditionalPanel("input.type == 'b'",
                     sliderInput("z1","z1:",-3,3,1.96,0.01),
                     textInput("z1t","",""),
                     actionButton("b1","enter"),
                     sliderInput("z2","z2:",-3,3,1.96,0.01),
                     textInput("z2t","",""),
                     actionButton("b2","enter")),
    plotOutput("density",width="600px",height="300px"),
    plotOutput("spinner",width="600px",height="600px")
)


server <- function(input, output, session) {
    observeEvent(input$b,{
        z = as.numeric(input$zt)
        if(is.numeric(z)){
            updateSliderInput(session,'z',value=z)
            updateTextInput(session,'zt',value="")
        } else {
            updateSliderInput(session,'z',value=1.96)
            updateTextInput(session,'zt',value="")
        }
    })
    observeEvent(input$b1,{
        z = as.numeric(input$z1t)
        if(is.numeric(z)){
            updateSliderInput(session,'z1',value=z)
            updateTextInput(session,'z1t',value="")
        } else {
            updateSliderInput(session,'z1',value=-1.96)
            updateTextInput(session,'z1t',value="")
        }
    })
    observeEvent(input$b2,{
        z = as.numeric(input$z2t)
        if(is.numeric(z)){
            updateSliderInput(session,'z2',value=z)
            updateTextInput(session,'z2t',value="")
        } else {
            updateSliderInput(session,'z2',value=-1.96)
            updateTextInput(session,'z2t',value="")
        }
    })
    observeEvent(input[["keyPressed"]],{
        z = as.numeric(input$zt)
        z1 = as.numeric(input$z1t)
        z2 = as.numeric(input$z2t)
        if(! is.na(z) & -3<=z & z<=3){
            updateSliderInput(session,"z1",value=z1)
            updateTextInput(session,"z1t",value="")
        } else {
            updateTextInput(session,"z1t",value="")
        }
        if(! is.na(z1) & -3<=z1 & z1<=3){
            updateSliderInput(session,"z1",value=z1)
            updateTextInput(session,"z1t",value="")
        } else {
            updateTextInput(session,"z1t",value="")
        }
        if(! is.na(z2) & -3<=z2 & z2<=3){
            updateSliderInput(session,"z2",value=z2)
            updateTextInput(session,"z2t",value="")
        } else {
            updateTextInput(session,"z2t",value="")
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
