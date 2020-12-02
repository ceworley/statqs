library(shiny)
library(latex2exp)

# Define UI for application that draws a histogram
ui <- fluidPage(
    withMathJax(),
    titlePanel("Central Limit Theorem, normal approximation",windowTitle="CLTprob"),
    helpText("Supply the population's parameters. Let \\(X\\) represent the population. Notice, \\(X\\) does NOT need to be normal,
             but then the probabilities are approximations. Those approximations are better with larger \\(n\\).
             Also, you may need to apply a continuity correction (to the boundary) if your population is discrete."),
    numericInput("mu","\\(\\mu=\\)",0),
    numericInput("sigma","\\(\\sigma=\\)",1),
    helpText("Describe the sampling. Let \\(\\sum X\\) represent a random total. 
             Let \\(\\overline{X}\\) represent a random average."),
    radioButtons("sty","Are you sampling a total or an average?",choices=c("total","average")),
    numericInput("n","What is the sample size? \\(n = \\)",25,step=1),
    conditionalPanel("input.sty == 'total'",
                     uiOutput("statement1"),
                     uiOutput("statement2"),
                     radioButtons("totp","What kind of probability?",
                                  c("Left, \\(P(\\sum X < b)\\)"="L",
                                    "Right, \\(P(\\sum X > b)\\)"="R",
                                    "Center, \\(P(|\\sum X-n\\mu| < d)\\)"="C",
                                    "Two-tail, \\(P(|\\sum X-n\\mu| > d)\\)"="T"))
                     ),
    conditionalPanel("input.sty == 'average'",
                     uiOutput("statement3"),
                     uiOutput("statement4"),
                     radioButtons("avep","What kind of probability?",
                                  c("Left, \\(P(\\overline{X} < b)\\)"="L",
                                    "Right, \\(P(\\overline{X} > b)\\)"="R",
                                    "Center, \\(P(|\\overline{X}-\\mu| < d)\\)"="C",
                                    "Two-tail, \\(P(|\\overline{X}-\\mu| > d)\\)"="T"))
                     ),
    conditionalPanel("input.sty == 'total' & (input.totp=='L' | input.totp=='R')",
                     numericInput("bt","Enter a boundary, \\(b=\\)",0)),
    conditionalPanel("input.sty == 'total' & (input.totp=='C' | input.totp=='T')",
                     numericInput("dt","Enter a distance, \\(d=\\)",1,min=0)),
    conditionalPanel("input.sty == 'average' & (input.avep=='L' | input.avep=='R')",
                     numericInput("ba","Enter a boundary, \\(b=\\)",0)),
    conditionalPanel("input.sty == 'average' & (input.avep=='C' | input.avep=='T')",
                     numericInput("da","Enter a distance, \\(d=\\)",1,min=0)),
    plotOutput("plot",width=800,height=300),
    uiOutput("statement5")
)





server <- function(input, output, session) {
    options(scipen=100)
    output$statement1 = renderUI({withMathJax(sprintf("$$\\text{expected value}=n\\mu= (%d)(%.4g) = %.4g$$",
                                                      input$n,input$mu,input$n*input$mu))})
    output$statement2 = renderUI({withMathJax(sprintf("$$\\text{SE}=\\sigma \\sqrt{n}= (%.4g) \\sqrt{%d} = %.4g$$",
                                                      input$sigma,input$n,input$sigma*sqrt(input$n)))})
    output$statement3 = renderUI({withMathJax(sprintf("$$\\text{expected value}=\\mu= %.4g$$",
                                                      input$mu))})
    output$statement4 = renderUI({withMathJax(sprintf("$$\\text{SE}= \\frac{\\sigma}{\\sqrt{n}}= \\frac{%.4g}{\\sqrt{%d}} = %.4g$$",
                                                      input$sigma,input$n,input$sigma/sqrt(input$n)))})
    
    output$statement5 = renderUI({
        if(input$sty=="total"){
            EV = input$n*input$mu
            SE = input$sigma*sqrt(input$n)
            if(input$totp=="L"){
                b = input$bt
                z = (b-EV)/SE
                P = pnorm(z)
                eq1 = sprintf("$$z=\\frac{%.4g-%.4g}{%.4g}=%.4g$$",b,EV,SE,z)
                eq2 = sprintf("$$P(Z<%.4g)=%.4g$$",z,pnorm(z))
                eq3 = sprintf("$$P\\left(\\sum X < %.4g\\right)=%.4f$$",b,P)
                s = paste0(eq1,eq2,eq3,collapse="\n")
            } else if(input$totp=="R"){
                b = input$bt
                z = (b-EV)/SE
                P = 1-pnorm(z)
                eq1 = sprintf("$$z=\\frac{%.4g-%.4g}{%.4g}=%.4g$$",b,EV,SE,z)
                eq2 = sprintf("$$P(Z<%.4g)=%.4g$$",z,pnorm(z))
                eq3 = sprintf("$$P(Z>%.4g)=1-%.4g=%.4g$$",z,pnorm(z),1-pnorm(z))
                eq4 = sprintf("$$P\\left(\\sum X > %.4g\\right)=%.4f$$",b,P)
                s = paste0(eq1,eq2,eq3,eq4,collapse="\n")
            } else if(input$totp=="C"){
                d = input$dt
                z = d/SE
                if(z>0){
                    P=1-2*(1-pnorm(z))
                } else {
                    P=0
                }
                s = sprintf("$$P\\left(\\left|\\sum X-%.4g \\right|< %.4g\\right)=%.4f$$",EV,d,P)
            } else {
                d = input$dt
                z = d/SE
                if(z>0){
                    P=2*(1-pnorm(z))
                } else {
                    P=1
                }
                s = sprintf("$$P\\left(\\left|\\sum X-%.4g \\right|> %.4g\\right)=%.4f$$",EV,d,P)
            }
        }
        if(input$sty=="average"){
            EV = input$mu
            SE = input$sigma/sqrt(input$n)
            if(input$avep=="L"){
                b = input$ba
                z = (b-EV)/SE
                P = pnorm(z)
                eq1 = sprintf("$$z=\\frac{%.4g-%.4g}{%.4g}=%.4g$$",b,EV,SE,z)
                eq2 = sprintf("$$P(Z<%.4g)=%.4g$$",z,pnorm(z))
                eq3 = sprintf("$$P\\left(\\bar{X} < %.4g\\right)=%.4f$$",b,P)
                s = paste0(eq1,eq2,eq3,collapse="\n")
            } else if(input$avep=="R"){
                b = input$ba
                z = (b-EV)/SE
                P = 1-pnorm(z)
                eq1 = sprintf("$$z=\\frac{%.4g-%.4g}{%.4g}=%.4g$$",b,EV,SE,z)
                eq2 = sprintf("$$P(Z<%.4g)=%.4g$$",z,pnorm(z))
                eq3 = sprintf("$$P(Z>%.4g)=1-%.4g=%.4g$$",z,pnorm(z),1-pnorm(z))
                eq4 = sprintf("$$P\\left(\\bar{X} > %.4g\\right)=%.4f$$",b,P)
                s = paste0(eq1,eq2,eq3,eq4,collapse="\n")
            } else if(input$avep=="C"){
                d = input$da
                z = d/SE
                if(z>0){
                    P=1-2*(1-pnorm(z))
                } else {
                    P=0
                }
                s = sprintf("$$P\\left(\\left|\\bar{X}-%.4g \\right|< %.4g\\right)=%.4f$$",EV,d,P)
            } else {
                d = input$da
                z = d/SE
                if(z>0){
                    P=2*(1-pnorm(z))
                } else {
                    P=1
                }
                s = sprintf("$$P\\left(\\left|\\bar{X}-%.4g \\right|> %.4g\\right)=%.4f$$",EV,d,P)
            }
        }
        withMathJax(s)
    })
    
    
    output$plot = renderPlot({
        n = input$n
        mu = input$mu
        sigma = input$sigma
        zs=seq(-3,3,0.01)
        par(mar=c(5,0,1,0))
        plot(zs,dnorm(zs),type="l",lwd=2,axes=F,ann=F)
        if(input$sty=="total"){
            EV = n*mu
            SE = sigma*sqrt(n)
            if(input$totp=="L" | input$totp=="R"){
                zstar = (input$bt-n*mu)/SE
                ttyy = "LR"
            } else {
                zstar = input$dt/SE
                ttyy = "CT"
            }
            mtext(TeX("$\\sum X$"),1,3)
        } else {
            EV = mu
            SE = sigma/sqrt(n)
            if(input$avep=="L" | input$avep=="R"){
                zstar = (input$ba-mu)/SE
                ttyy = "LR"
            } else {
                zstar = input$da/SE
                ttyy = "CT"
            }
            mtext(TeX("$\\bar{X}$"),1,3)
        }
        zs=seq(-3,3,0.2)
        abline(v=zs,lty=2)
        abline(h=seq(0,0.4,0.05),lty=2)
        abline(h=0)
        axis(1,-3:3,signif(EV+(-3:3)*SE,4))
        if(input$sty=="total"){
            if(input$totp=="L"){
                zzz = seq(-3,zstar,length.out=100)
                polygon(c(-3,zzz,zstar),c(0,dnorm(zzz),0),col=rgb(0.4,0.4,0.8,alpha=0.4))
            } else if(input$totp=="R"){
                zzz = seq(zstar,3,length.out=100)
                polygon(c(zstar,zzz,3),c(0,dnorm(zzz),0),col=rgb(0.4,0.4,0.8,alpha=0.4))
            } else if(input$totp=="C"){
                zzz = seq(-zstar,zstar,length.out=100)
                if(zstar>0){
                    polygon(c(-zstar,zzz,zstar),c(0,dnorm(zzz),0),col=rgb(0.4,0.4,0.8,alpha=0.4))
                }
            } else {
                zzz1 = seq(-3,-zstar,length.out=50)
                zzz2 = seq(zstar,3,length.out=50)
                if(zstar>0){
                    polygon(c(-3,zzz1,-zstar),c(0,dnorm(zzz1),0),col=rgb(0.4,0.4,0.8,alpha=0.4))
                    polygon(c(zstar,zzz2,3),c(0,dnorm(zzz2),0),col=rgb(0.4,0.4,0.8,alpha=0.4))
                } else {
                    zzz = seq(-3,3,length.out=100)
                    polygon(c(-3,zzz,3),c(0,dnorm(zzz),0),col=rgb(0.4,0.4,0.8,alpha=0.4))
                }
            }
        }
        if(input$sty=="average"){
            if(input$avep=="L"){
                zzz = seq(-3,zstar,length.out=100)
                polygon(c(-3,zzz,zstar),c(0,dnorm(zzz),0),col=rgb(0.4,0.4,0.8,alpha=0.4))
            } else if(input$avep=="R"){
                zzz = seq(zstar,3,length.out=100)
                polygon(c(zstar,zzz,3),c(0,dnorm(zzz),0),col=rgb(0.4,0.4,0.8,alpha=0.4))
            } else if(input$avep=="C"){
                zzz = seq(-zstar,zstar,length.out=100)
                if(zstar>0){
                    polygon(c(-zstar,zzz,zstar),c(0,dnorm(zzz),0),col=rgb(0.4,0.4,0.8,alpha=0.4))
                }
            } else {
                zzz1 = seq(-3,-zstar,length.out=50)
                zzz2 = seq(zstar,3,length.out=50)
                if(zstar>0){
                    polygon(c(-3,zzz1,-zstar),c(0,dnorm(zzz1),0),col=rgb(0.4,0.4,0.8,alpha=0.4))
                    polygon(c(zstar,zzz2,3),c(0,dnorm(zzz2),0),col=rgb(0.4,0.4,0.8,alpha=0.4))
                } else {
                    zzz = seq(-3,3,length.out=100)
                    polygon(c(-3,zzz,3),c(0,dnorm(zzz),0),col=rgb(0.4,0.4,0.8,alpha=0.4))
                }
            }
        }
    })
}




# Run the application 
shinyApp(ui = ui, server = server)
