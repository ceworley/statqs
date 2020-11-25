library(shiny)
library(latex2exp)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$script('$(document).on("keyup", function(e) {
                  if(e.keyCode == 13){
                    Shiny.onInputChange("keyPressed", Math.random());
                  }
                });'),
    titlePanel("Standard Normal Distribution - Left, Right, Center, and Two-tail probabilities"),
    radioButtons("dist", "Area:",
                 c("Left" = "l",
                   "Right" = "r",
                   "Center" = "c",
                   "Two-tail" = "t")),
    sliderInput("z",
                "z-score:",
                min = -3,
                max = 3,
                value = 0,
                step=0.01,
                width='100%'),

    textInput("z3","Enter a specific z-score:"),
    actionButton("butt","Enter"),
    plotOutput("distPlot",width="600px",height="300px"),   
    plotOutput("spinPlot",width = "600px",height="600px"))


drawit = function(z,type){
    Z = seq(-3,3,0.01)
    retval <- renderPlot({
        if(type == "l"){
            plot(Z,dnorm(Z),type="l",ylab="density",lwd=3,main=TeX(paste("$P(Z<",z,") = ",sprintf("%.4f",round(pnorm(z),4)),"$",collapse="")))
            polygon(c(Z[Z<=z],z,-3),c(dnorm(Z[Z<=z]),0,0),col=rgb(1,0.5,0.5,0.5))
        }
        if(type == "r"){
            plot(Z,dnorm(Z),type="l",ylab="density",lwd=3,main=TeX(paste("$P(Z>",z,") = ",sprintf("%.4f",round(1-round(pnorm(z),4),4)),"$",collapse="")))
            polygon(c(Z[Z>=z],3,z),c(dnorm(Z[Z>=z]),0,0),col=rgb(1,0.5,0.5,0.5))
        }
        if(type == "c"){
            plot(Z,dnorm(Z),type="l",ylab="density",lwd=3,main=TeX(paste("$P(|Z|<",z,") = ",sprintf("%.4f",round(max(2*round(pnorm(z),4)-1,0),4)),"$",collapse="")))
            polygon(c(Z[Z>=-z & Z<=z],z,-z),c(dnorm(Z[Z>=-z & Z<=z]),0,0),col=rgb(1,0.5,0.5,0.5))
        }
        if(type == "t"){
            plot(Z,dnorm(Z),type="l",ylab="density",lwd=3,main=TeX(paste("$P(|Z|>",z,") = ",sprintf("%.4f",round(min(2-2*round(pnorm(z),4),1),4)),"$",collapse="")))
            if(z>0){
                polygon(c(Z[Z<=-z],-z,-3),c(dnorm(Z[Z<=-z]),0,0),col=rgb(1,0.5,0.5,0.5))
                polygon(c(Z[Z>=z],3,z),c(dnorm(Z[Z>=z]),0,0),col=rgb(1,0.5,0.5,0.5))
            } else {
                polygon(c(Z,3,-3),c(dnorm(Z),0,0),col=rgb(1,0.5,0.5,0.5))
            }
        }
        abline(h=seq(0,0.4,0.05),lwd=0.5)
        abline(v=seq(-3,3,0.2),lwd=0.5)
        abline(h=0,lwd=2)
    })
    return(retval)
}

sns = function(){
    par(mar=c(0,0,0,0))
    plot(0,0,type="n",xlim=c(-1.2,1.2),ylim=c(-1.2,1.2),ann=F)
    theta = seq(0,2*pi,0.01)
    polygon(cos(theta),sin(theta))
    z1 = round(seq(-3,3,1),2)
    z2 = round(seq(-3,3,0.5),2)
    z3 = round(seq(-3,3,0.1),2)
    z4 = round(seq(-2,2,0.05),2)
    z5 = round(seq(-2,2,0.01),2)
    markz = function(z,L,cex=1){
        p = pnorm(z)
        for(pp in p){
            lines(c((1-L)*cos(2*pi*pp),cos(2*pi*pp)),c((1-L)*sin(2*pi*pp),sin(2*pi*pp)),cex=cex)
        }
    }
    markz(z1,0.3,2)
    markz(z2,0.2)
    markz(z3,0.1)
    markz(z4,0.05)
    markz(z5,0.025)
    for(z in z1){
        p = pnorm(z)
        pc = min(max(p,0.005),0.995)
        text((1-0.33)*cos(2*pi*pc),(1-0.33)*sin(2*pi*pc),round(qnorm(p),2),cex=0.9)
    }
    for(z in z2[!(z2 %in% z1)]){
        p = pnorm(z)
        pc = min(max(p,0.005),0.995)
        text((1-0.26)*cos(2*pi*pc),(1-0.26)*sin(2*pi*pc),round(qnorm(p),2),cex=0.9)
    }
    for(z in z3[!(z3 %in% z2) & abs(z3)<2]){
        p = pnorm(z)
        pc = min(max(p,0.005),0.995)
        text((1-0.16)*cos(2*pi*pc),(1-0.16)*sin(2*pi*pc),round(qnorm(p),2),srt=(90+p*360)%%180-90,cex=0.9)
    }
    for(z in z4[!(z4 %in% z3) & abs(z4)<1.5]){
        p = pnorm(z)
        pc = min(max(p,0.005),0.995)
        text((1-0.1)*cos(2*pi*pc),(1-0.1)*sin(2*pi*pc),round(qnorm(p),2),srt=(90+p*360)%%180-90,cex=0.6)
    }
    p1 = round(seq(0,1,0.1),2)
    p2 = round(seq(0,1,0.05),2)
    p3 = round(seq(0,1,0.01),2)
    lines(c(1,1.2),c(0,0),lwd=3)
    for(p in p1){
        lines(c(cos(p*2*pi),1.15*cos(p*2*pi)),c(sin(p*2*pi),1.15*sin(p*2*pi)))
        pc = max(min(p,0.996),0.004)
        text(1.2*cos(pc*2*pi),1.2*sin(pc*2*pi),round(p,2),cex=1)
    }
    for(p in p2[!(p2 %in% p1)]){
        lines(c(cos(p*2*pi),1.1*cos(p*2*pi)),c(sin(p*2*pi),1.1*sin(p*2*pi)))
        pc = max(min(p,0.996),0.004)
        text(1.16*cos(pc*2*pi),1.16*sin(pc*2*pi),round(p,2),srt=(90+p*360)%%180-90,cex=0.8)
    }
    for(p in p3[!(p3 %in% p2)]){
        lines(c(cos(p*2*pi),1.05*cos(p*2*pi)),c(sin(p*2*pi),1.05*sin(p*2*pi)))
        pc = max(min(p,0.996),0.004)
        text(1.12*cos(pc*2*pi),1.12*sin(pc*2*pi),round(p,2),srt=(90+p*360)%%180-90,cex=0.8)
    }
    text(1,1,"U\nStandard Uniform\nDistribution")
    text(0,0,"Z\nStandard Normal\nDistribution")
}


drawspin = function(z,type){
    P = seq(0,1,0.001)
    p = pnorm(z)
    col=rgb(1,0.5,0.5,0.5)
    retval <- renderPlot({
        if(type == "l"){
            sns()
            polygon(c(0,cos(2*pi*P[P<=p])),c(0,sin(2*pi*P[P<=p])),col=col)
        }
        if(type == "r"){
            sns()
            polygon(c(0,cos(2*pi*P[P>=p])),c(0,sin(2*pi*P[P>=p])),col=col)
        }
        if(type == "c"){
            sns()
            if(z>0){
                p2 = pnorm(-z)
                polygon(c(0,cos(2*pi*P[P>=p2 & P<=p])),c(0,sin(2*pi*P[P>=p2 & P<=p])),col=col)
            }
        }
        if(type == "t"){
            sns()
            if(z>0){
                p2 = pnorm(-z)
                ppp = c(P[P>=p],P[P<=p2])
                polygon(c(0,cos(2*pi*ppp)),c(0,sin(2*pi*ppp)),col=col)
            } else {
                polygon(cos(2*pi*P),sin(2*pi*P),col=col)
            }
        }
    })
    return(retval)
}


server <- function(input, output, session) {
    observeEvent(input$z, {
            output$distPlot = drawit(input$z,input$dist)
            output$spinPlot = drawspin(input$z,input$dist)
        })
    observeEvent(input$butt, {
        z = as.numeric(input$z3)
        if(! is.na(z) & -3<z & z<3){
            updateSliderInput(session,"z",value=z)
            output$distPlot = drawit(z,input$dist)
            output$spinPlot = drawspin(z,input$dist)
            updateTextInput(session,"z3",value="")
        } else {
            updateTextInput(session,"z3",value="")
        }
    })
    observeEvent(input[["keyPressed"]],{
        z = as.numeric(input$z3)
        if(! is.na(z) & -3<=z & z<=3){
            updateSliderInput(session,"z",value=z)
            output$distPlot = drawit(z,input$dist)
            output$spinPlot = drawspin(z,input$dist)
            updateTextInput(session,"z3",value="")
        } else {
            updateTextInput(session,"z3",value="")
        }
    })

    observeEvent(input$dist, {
        output$distPlot = drawit(input$z,input$dist)
        output$spinPlot = drawspin(input$z,input$dist)
        }
        )
 
}

# Run the application 
shinyApp(ui = ui, server = server)
