library(shiny)
library(latex2exp)

discretespinner = function(poss=1:6,prob=rep(1/6,6),
                           cex=apply(matrix(c(prob*40,rep(5,length(prob))),ncol=2),1,min),
                           lwd=1,txd=0.7){
    theta=seq(0,2*pi,2*pi/1000)
    plot(sin(theta),cos(theta),type="l",xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
    cumprob = c(0,cumsum(prob)/sum(prob))
    lows = cumprob[1:(length(cumprob)-1)]*2*pi
    highs = cumprob[2:length(cumprob)]*2*pi
    mids = (lows+highs)/2
    for(theta in lows){
        lines(c(0,sin(theta)),c(0,cos(theta)),lwd=lwd)
    }
    text(sin(mids)*txd,cos(mids)*txd,poss,cex=cex)
    theta = seq(0,1,0.01)*2*pi
    for(th in theta){
        lines(c(1,1.02)*sin(th),c(1,1.02)*cos(th))
    }
    theta = seq(0,1,0.05)*2*pi
    for(th in theta){
        lines(c(1,1.04)*sin(th),c(1,1.04)*cos(th),lwd=1.5)
    }
    theta = seq(0,1,0.1)*2*pi
    for(th in theta){
        lines(c(1,1.1)*sin(th),c(1,1.1)*cos(th),lwd=2)
    }
}


continuousspinner = function(poss=1:6,prob=rep(1/6,6),
                           cex=apply(matrix(c(prob*40,rep(5,length(prob))),ncol=2),1,min),
                           lwd=1,txd=0.7){
    theta=seq(0,2*pi,2*pi/1000)
    plot(sin(theta),cos(theta),type="l",xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
    cumprob = c(0,cumsum(prob)/sum(prob))
    lows = cumprob[1:(length(cumprob)-1)]*2*pi
    highs = cumprob[2:length(cumprob)]*2*pi
    mids = (lows+highs)/2
    for(theta in lows){
        lines(c(0,sin(theta)),c(0,cos(theta)),lwd=lwd)
    }
    text(sin(mids)*txd,cos(mids)*txd,poss,cex=cex)
    theta = seq(0,1,0.01)*2*pi
    for(th in theta){
        lines(c(1,1.02)*sin(th),c(1,1.02)*cos(th))
    }
    theta = seq(0,1,0.05)*2*pi
    for(th in theta){
        lines(c(1,1.04)*sin(th),c(1,1.04)*cos(th),lwd=1.5)
    }
    theta = seq(0,1,0.1)*2*pi
    for(th in theta){
        lines(c(1,1.1)*sin(th),c(1,1.1)*cos(th),lwd=2)
    }
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    withMathJax(),
    titlePanel("Central Limit Theorem Exploration",windowTitle="CLTExplore"),
    numericInput("seed",tags$a(href="https://en.wikipedia.org/wiki/Random_seed",target="_blank", "set seed"),1,1,9999999,1),
    radioButtons("dist",tags$a(href="https://en.wikipedia.org/wiki/Probability_distribution",target="_blank", "choose distribution"),
                 c("Bernoulli",
                   "dice",
                   "uniform, continuous",
                   "uniform, discrete",
                   "geometric",
                   "exponential",
                   "normal",
                   "beta",
                   "poisson",
                   "binomial",
                   "discrete",
                   "continuous",
                   "t dist"
                   )),
    conditionalPanel(
        condition = "input.dist == 'Bernoulli'",
        numericInput("bernp","p",0.5,0,1,0.01),
        plotOutput('bernspin',"600px","600px")
    ),
    conditionalPanel(
        condition = "input.dist == 'dice'",
        numericInput("diceN","number of sides",6,2,10000,1),
        plotOutput('dicespin',"600px","600px")
    ),
    conditionalPanel(
        condition = "input.dist == 'uniform, continuous'",
        numericInput("uca","lower bound",0,step=0.1),
        numericInput("ucb","upper bound",1,step=0.1),
        plotOutput('ucon',"600px","600px")
    )
)

server <- function(input, output, session) {
    output$bernspin = renderPlot({
        par(mar=c(0,0,0,0))
        discretespinner(c(0,1),c(1-input$bernp,input$bernp))
    })
    output$dicespin = renderPlot({
        par(mar=c(0,0,0,0))
        discretespinner(1:input$diceN,rep(1/input$diceN,input$diceN))
    })
    output$ucon = renderPlot({
        par(mar=c(0,0,0,0))
        discretespinner(1:input$diceN,rep(1/input$diceN,input$diceN))
    })
}







# Run the application 
shinyApp(ui = ui, server = server)
