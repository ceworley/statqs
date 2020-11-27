library(shiny)
library(latex2exp)

# Define UI for application that draws a histogram
ui <- fluidPage(
    withMathJax(),
    titlePanel("Student's T Distribution",windowTitle="T Dist"),
    radioButtons("type","What type of probability?",
                 c("left, \\(P(T<t^\\star)\\)" = "l",
                   "right, \\(P(T>t^\\star)\\)" = "r",
                   "center, \\(P(|T|<t^\\star)\\)" = "c",
                   "two-tail, \\(P(|T|>t^\\star)\\)" = "tt")),
    numericInput("df","Degrees of freedom, df",value=30,min=1),
    numericInput("t","\\(t^\\star\\)=",value=0,step=0.01),
    conditionalPanel("input.type == 'l'",
                     numericInput("P1","\\(P(T<t^\\star)\\)=",value=0.5,min=0,max=1,step=0.01)),
    conditionalPanel("input.type == 'r'",
                     numericInput("P2","\\(P(T>t^\\star)\\)=",value=0.5,min=0,max=1,step=0.01)),
    conditionalPanel("input.type == 'c'",
                     numericInput("P3","\\(P(|T|<t^\\star)\\)=",value=0.5,min=0,max=1,step=0.01)),
    conditionalPanel("input.type == 'tt'",
                     numericInput("P4","\\(P(|T|>t^\\star)\\)=",value=0.5,min=0,max=1,step=0.01)),
    
    titlePanel("Density visualization:"),
    plotOutput("density",width="600px",height="300px")
)


fb = function(value,low,high){ #force between
    max(min(value,high),low)
}

server <- function(input, output, session) {
    options(scipen=100)
    observeEvent(input$df,  {
        p = pt(input$t,input$df)
        updateSliderInput(session=session, inputId="P1", value=p)
        updateSliderInput(session=session, inputId="P2", value=1-p)
        updateSliderInput(session=session, inputId="P3", value=fb(1-(1-p)*2,0,1))
        updateSliderInput(session=session, inputId="P4", value=fb((1-p)*2,0,1))
    })
    
    observeEvent(input$t,  {
        p = pt(input$t,input$df)
        updateSliderInput(session=session, inputId="P1", value=p)
        updateSliderInput(session=session, inputId="P2", value=1-p)
        updateSliderInput(session=session, inputId="P3", value=fb(1-(1-p)*2,0,1))
        updateSliderInput(session=session, inputId="P4", value=fb((1-p)*2,0,1))
    })
    observeEvent(input$P1,  {
        p = input$P1
        updateSliderInput(session=session, inputId="t", value=qt(p,input$df))
        updateSliderInput(session=session, inputId="P2", value=1-p)
        updateSliderInput(session=session, inputId="P3", value=fb(1-(1-p)*2,0,1))
        updateSliderInput(session=session, inputId="P4", value=fb((1-p)*2,0,1))
    })
    observeEvent(input$P2,  {
        p = 1-input$P2
        updateSliderInput(session=session, inputId="t", value=qt(p,input$df))
        updateSliderInput(session=session, inputId="P1", value=p)
        updateSliderInput(session=session, inputId="P3", value=fb(1-(1-p)*2,0,1))
        updateSliderInput(session=session, inputId="P4", value=fb((1-p)*2,0,1))
    })
    observeEvent(input$P3,  {
        P = input$P3
        p = fb(P+(1-P)/2,0,1)
        updateSliderInput(session=session, inputId="t", value=qt(p,input$df))
        updateSliderInput(session=session, inputId="P2", value=1-p)
        updateSliderInput(session=session, inputId="P1", value=p)
        updateSliderInput(session=session, inputId="P4", value=fb((1-p)*2,0,1))
    })
    observeEvent(input$P4,  {
        P = input$P4
        p = fb(1-P/2,0,1)
        updateSliderInput(session=session, inputId="t", value=qt(p,input$df))
        updateSliderInput(session=session, inputId="P2", value=1-p)
        updateSliderInput(session=session, inputId="P3", value=fb(1-(1-p)*2,0,1))
        updateSliderInput(session=session, inputId="P1", value=p)
    })
    
    # output$statement = renderUI({withMathJax(paste("$$\\mu =  ",input$mu,"$$\n $$\\sigma = ",input$sig," $$ \n $$X \\sim \\mathcal{N}(",input$mu,", ",input$sig,") $$"))})
    output$density = renderPlot({
        n = input$df+1
        mint = -ceiling(qt(0.98,n-1))
        maxt = ceiling(qt(0.98,n-1))
        t = input$t
        p = pt(t,n-1)
        if(input$type=="l"){
            t1 = mint
            t2 = fb(t,mint,maxt)
        } else if(input$type=="r"){
            t1 = fb(t,mint,maxt)
            t2 = maxt
        } else if(input$type=="c"){
            t1 = -fb(t,mint,maxt)*(t>0)
            t2 = fb(t,mint,maxt)*(t>0)
        } else if(input$type=="tt"){
            t1 = -fb(t,mint,maxt)*(t>0)
            t2 = fb(t,mint,maxt)*(t>0)
        }
        ts = seq(mint,maxt,length.out=200)
        par(mar=c(4,0,2,0))
        plot(ts,dt(ts,n-1),type="l",lwd=3,xlim = c(mint,maxt),ylim=c(0,0.4),ann=F,axes=F)
        lines(ts,dnorm(ts),lty=2,lwd=0.5)
        if(input$type == "l"){
            ts2 = seq(t1,t2,length.out=100)
            polygon(c(t1,ts2,t2),c(0,dt(ts2,n-1),0),col=rgb(1,0.5,0.5,0.5))
            mtext(paste("P(T<",signif(t,4),") = ",round(p,4),collapse=""),3,0,cex=1.5)
        }
        if(input$type == "r"){
            ts2 = seq(t1,t2,length.out=100)
            polygon(c(t1,ts2,t2),c(0,dt(ts2,n-1),0),col=rgb(1,0.5,0.5,0.5))
            mtext(paste("P(T>",signif(t,4),") = ",round(1-p,4),collapse=""),3,0,cex=1.5)
        }
        if(input$type == "c"){
            if(t>0){
                ts2 = seq(t1,t2,length.out=100)
                polygon(c(t1,ts2,t2),c(0,dt(ts2,n-1),0),col=rgb(1,0.5,0.5,0.5))
                mtext(paste("P(|T|<",signif(t,4),") = ",round(1-(1-p)*2,4),collapse=""),3,0,cex=1.5)
            } else {
                mtext(paste("P(|T|<",signif(t,4),") = ",0,collapse=""),3,0,cex=1.5)
            }
        }
        if(input$type == "tt"){
            if(t>0){
                ts2 = seq(mint,t1,length.out=50)
                ts3 = seq(t2,maxt,length.out=50)
                polygon(c(mint,ts2,t1),c(0,dt(ts2,n-1),0),col=rgb(1,0.5,0.5,0.5))
                polygon(c(t2,ts3,maxt),c(0,dt(ts3,n-1),0),col=rgb(1,0.5,0.5,0.5))
                mtext(paste("P(|T|>",signif(t,4),") = ",round(2*(1-p),4),collapse=""),3,0,cex=1.5)
            } else {
                ts2 = seq(mint,maxt,length.out=200)
                polygon(c(mint,ts2,maxt),c(0,dt(ts2,n-1),0),col=rgb(1,0.5,0.5,0.5))
                mtext(paste("P(|T|>",signif(t,4),") = ",1,collapse=""),3,0,cex=1.5)
            }
        }
        axis(1,seq(mint,maxt,1),line=0.5)
        mtext('t',side=1,line=2.2)
        abline(h=0,lwd=2)
    })
}





# Run the application 
shinyApp(ui = ui, server = server)
