library(shiny)
library(latex2exp)


drawit = function(z1,z2){
    Z = seq(-3,3,0.01)
    plot(Z,dnorm(Z),type="l",ylab="density",lwd=3,main=TeX(paste("$P(",z1,"<Z<",z2,") = ",sprintf("%.4f",max(round(round(pnorm(z2),4)-round(pnorm(z1),4),4),0)),"$",collapse="")))
    abline(h=seq(0,0.4,0.05),lwd=0.5)
    abline(v=seq(-3,3,0.2),lwd=0.5)
    abline(h=0,lwd=2)
    polygon(c(Z[Z>=z1 & Z<=z2],min(z2,3),max(z1,-3)),c(dnorm(Z[Z>=z1 & Z<=z2]),0,0),col=rgb(1,0.5,0.5,0.5))
}

drawleft = function(z){
    Z = seq(-3,3,0.01)
    plot(Z,dnorm(Z),type="l",ylab="density",lwd=3,main=TeX(paste("$P(Z<",z,") = ",sprintf("%.4f",max(round(pnorm(z),4),0)),"$",collapse="")))
    abline(h=seq(0,0.4,0.05),lwd=0.5)
    abline(v=seq(-3,3,0.2),lwd=0.5)
    abline(h=0,lwd=2)
    polygon(c(Z[Z<=z],min(z,3),max(z,-3)),c(dnorm(Z[Z<=z]),0,0),col=rgb(1,0.5,0.5,0.5))
}

drawit2 = function(z1,z2){
    Z = seq(-3,3,0.01)
    p1 = round(pnorm(z1),4)
    p2 = round(pnorm(z2),4)
    ppp = p2-p1
    if(p2<p1){
        p2 = 0
        p1 = 0
        ppp = 0
        plot(Z,dnorm(Z),type="l",ylab="density",lwd=3,main=TeX(paste("$P(",z1,"<Z<",z2,") = ",sprintf("%.4f",ppp),"$",collapse="")))
    } else {
        plot(Z,dnorm(Z),type="l",ylab="density",lwd=3,main=TeX(paste("$P(",z1,"<Z<",z2,") = ",sprintf("%.4f",p2),"-",sprintf("%.4f",p1)," = ",sprintf("%.4f",ppp),"$",collapse="")))
    }
    abline(h=seq(0,0.4,0.05),lwd=0.5)
    abline(v=seq(-3,3,0.2),lwd=0.5)
    abline(h=0,lwd=2)
    polygon(c(Z[Z>=z1 & Z<=z2],min(z2,3),max(z1,-3)),c(dnorm(Z[Z>=z1 & Z<=z2]),0,0),col=rgb(1,0.5,0.5,0.5))
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


drawspin = function(z1,z2){
    P = seq(0,1,0.001)
    p1 = pnorm(z1)
    p2 = pnorm(z2)
    col=rgb(1,0.5,0.5,0.5)
    sns()
    polygon(c(0,cos(2*pi*P[P>=p1 & P<=p2])),c(0,sin(2*pi*P[P>=p1 & P<=p2])),col=col)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$script('$(document).on("keyup", function(e) {
                  if(e.keyCode == 13){
                    Shiny.onInputChange("keyPressed", Math.random());
                  }
                });'),
    titlePanel("Standard Normal Distribution - Probability that Z lands between two z-scores"),
    titlePanel("P(z1 < Z < z2)"),
    textOutput("ans"),
    splitLayout(
        verticalLayout(
            conditionalPanel(condition = "input.z1i == false",
                sliderInput("z1",
                        "z1:",
                        min = -3,
                        max = 3,
                        value = -1.96,
                        step=0.01,
                        width='100%'),
            textInput("z1t","Enter a specific z1:"),
            actionButton("butt1","Enter")),
            checkboxInput("z1i", "z1 = -infinity", value = FALSE)),
        verticalLayout(
            conditionalPanel(condition = "input.z2i == false",
            sliderInput("z2",
                        "z2:",
                        min = -3,
                        max = 3,
                        value = 1.96,
                        step=0.01,
                        width='100%'),
            textInput("z2t","Enter a specific z2:"),
            actionButton("butt2","Enter")),
            checkboxInput("z2i", "z2 = infinity", value = FALSE))),
    titlePanel("Standard Normal Density Curve, with 1% squares"),
    plotOutput("distPlot",width="600px",height="300px"),   
    titlePanel("Standard Normal Spinner, with uniform tickmarks to calculate probability."),
    textOutput("ans2"),
    plotOutput("spinPlot",width = "600px",height="600px"),
    titlePanel("Visualization of probability as a difference"),
    titlePanel("P(z1 < Z < z2)   =   P(Z<z2) - P(Z<z1)"),
    plotOutput("plotz2",width = "600px",height="300px"),
    plotOutput("plotz1",width = "600px",height="300px"),
    plotOutput("plotdiff",width = "600px",height="300px")
    )



server <- function(input, output, session) {
    observeEvent(input$butt1,{
        z1 = as.numeric(input$z1t)
        if(is.numeric(z1)){
            updateSliderInput(session,'z1',value=z1)
            updateTextInput(session,'z1t',value="")
        } else {
            updateSliderInput(session,'z1',value=-1.96)
            updateTextInput(session,'z1t',value="")
        }
    })
    observeEvent(input$butt2,{
        z2 = as.numeric(input$z2t)
        if(is.numeric(z2)){
            updateSliderInput(session,'z2',value=z2)
            updateTextInput(session,'z2t',value="")
        } else {
            updateSliderInput(session,'z2',value=1.96)
            updateTextInput(session,'z2t',value="")
        }
    })
    observeEvent(input[["keyPressed"]],{
        z1 = as.numeric(input$z1t)
        z2 = as.numeric(input$z2t)
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
    
    output$distPlot = renderPlot({
        z1 = input$z1
        z2 = input$z2
        if(input$z1i) z1=-Inf
        if(input$z2i) z2=Inf
        getzs = c(z1,z2)
        drawit(z1,z2)})
    output$spinPlot = renderPlot({
        z1 = input$z1
        z2 = input$z2
        if(input$z1i) z1=-Inf
        if(input$z2i) z2=Inf
        getzs = c(z1,z2)
        drawspin(z1,z2)})
    output$plotz2 = renderPlot({
        z1 = input$z1
        z2 = input$z2
        if(input$z1i) z1=-Inf
        if(input$z2i) z2=Inf
        getzs = c(z1,z2)
        drawleft(z2)})
    output$plotz1 = renderPlot({
        z1 = input$z1
        z2 = input$z2
        if(input$z1i) z1=-Inf
        if(input$z2i) z2=Inf
        getzs = c(z1,z2)
        drawleft(z1)})
    output$plotdiff = renderPlot({
        z1 = input$z1
        z2 = input$z2
        if(input$z1i) z1=-Inf
        if(input$z2i) z2=Inf
        getzs = c(z1,z2)
        drawit2(z1,z2)})
    output$ans = renderText({
        z1 = input$z1
        z2 = input$z2
        if(input$z1i) z1=-Inf
        if(input$z2i) z2=Inf
        getzs = c(z1,z2)
        paste("P(",z1,"<Z<",z2,") = ",sprintf("%.4f",max(round(round(pnorm(z2),4)-round(pnorm(z1),4),4),0)),collapse="")
        })
    output$ans2 = renderText({
        z1 = input$z1
        z2 = input$z2
        if(input$z1i) z1=-Inf
        if(input$z2i) z2=Inf
        getzs = c(z1,z2)
        p1 = round(pnorm(z1),4)
        p2 = round(pnorm(z2),4)
        if(p2<p1){
            p2 = 0
            p1 = 0
        }
        pp = max(round(p2-p1,4),0)
        paste("P(",z1,"<Z<",z2,")  =  ",sprintf("%.4f",p2),"-",sprintf("%.4f",p1),"  =  ",sprintf("%.4f",pp),collapse="")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
