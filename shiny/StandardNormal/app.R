library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Standard Normal"),

    # Sidebar with a slider input for number of bins 
    verticalLayout(
        sidebarPanel(
            sliderInput("z",
                        "z:",
                        min = -3,
                        max = 3,
                        value = 0,
                        step=0.01,
                        width='1000px'),
            numericInput("z2", "", 0, min = -3, max = 3,step=0.01),
            
            radioButtons("dist", "Area:",
                         c("Left" = "l",
                           "Right" = "r",
                           "Center" = "c",
                           "Two-tail" = "t",
                           "Arbitrary" = "a"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

drawit = function(z,type){
    Z = seq(-3,3,0.01)
    retval <- renderPlot({
        if(type == "l"){
            plot(Z,dnorm(Z),type="l",ylab="density",lwd=3,main=TeX(paste("$P(Z<",z,") = ",round(pnorm(z),4),"$",collapse="")))
            polygon(c(Z[Z<=z],z,-3),c(dnorm(Z[Z<=z]),0,0),col=rgb(1,0.5,0.5,0.5))
        }
        if(type == "r"){
            plot(Z,dnorm(Z),type="l",ylab="density",lwd=3,main=TeX(paste("$P(Z>",z,") = ",round(1-round(pnorm(z),4),4),"$",collapse="")))
            polygon(c(Z[Z>=z],3,z),c(dnorm(Z[Z>=z]),0,0),col=rgb(1,0.5,0.5,0.5))
        }
        if(type == "c"){
            plot(Z,dnorm(Z),type="l",ylab="density",lwd=3,main=TeX(paste("$P(|Z|<",z,") = ",round(max(2*round(pnorm(z),4)-1,0),4),"$",collapse="")))
            polygon(c(Z[Z>=-z & Z<=z],z,-z),c(dnorm(Z[Z>=-z & Z<=z]),0,0),col=rgb(1,0.5,0.5,0.5))
        }
        if(type == "t"){
            plot(Z,dnorm(Z),type="l",ylab="density",lwd=3,main=TeX(paste("$P(|Z|>",z,") = ",round(min(2-2*round(pnorm(z),4),1),4),"$",collapse="")))
            polygon(c(Z[Z<=-z],-z,-3),c(dnorm(Z[Z<=-z]),0,0),col=rgb(1,0.5,0.5,0.5))
            polygon(c(Z[Z>=z],3,z),c(dnorm(Z[Z>=z]),0,0),col=rgb(1,0.5,0.5,0.5))
        }
        abline(h=seq(0,0.4,0.05),lwd=0.5)
        abline(v=seq(-3,3,0.2),lwd=0.5)
        abline(h=0,lwd=2)
    })
    return(retval)
}

server <- function(input, output, session) {
    observeEvent(input$z2, {
        if ((as.numeric(input$z2) != input$z) &  is.numeric(input$z2)  & input$z2 != "" &  input$z != ""){
            updateSliderInput(session,'z',value = input$z2)
            output$distPlot = drawit(input$z2,input$dist)
        }
    })
    observeEvent(input$z, {
        if ((as.numeric(input$z2) != input$z) & input$z != "" & input$z2 != "") {
            updateNumericInput(session,'z2',value=input$z)
            output$distPlot = drawit(input$z,input$dist)
            }
        })
    observeEvent(input$dist, {
        output$distPlot = drawit(input$z,input$dist)
        }
        )
 
}

# Run the application 
shinyApp(ui = ui, server = server)
