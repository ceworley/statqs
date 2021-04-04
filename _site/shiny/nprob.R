library(shiny)
library(latex2exp)

# Define UI for application that draws a histogram
ui <- fluidPage(
    withMathJax(),
    titlePanel("Parameters of  \\(X\\sim \\mathcal{N}(\\mu,\\sigma)\\):"),
    numericInput("mu","The mean of \\(X\\), \\(\\mu\\)",0),
    numericInput("sig","The standard deviation of \\(X\\), \\(\\sigma\\)",1),
    # uiOutput("statement"),
    titlePanel("Boundary parameters:"),
    radioButtons("type","Probability Type:",
                 c("Left" = "l",
                   "Right" = "r",
                   "Center" = "c",
                   "Two-tail" = "t",
                   "Between two x values" = "b")),
    conditionalPanel("input.type == 'l' | input.type == 'r'",
                     numericInput("x","Boundary, x:",value=0)),
    conditionalPanel("input.type == 'c' | input.type == 't'",
                     withMathJax(numericInput("d","Distance between boundary and mean, \\(d=x_2-\\mu\\):",value=1.96,min=0))),
    conditionalPanel("input.type == 'b'",
                     numericInput("x1","Lower Boundary, \\(x_1\\):",value=-1.96),
                     numericInput("x2","Upper Boundary, \\(x_2\\):",value=1.96)),
    titlePanel("Density visualization:"),
    plotOutput("density",width="600px",height="300px"),
    titlePanel("Algebraic statements:"),
    uiOutput("statement2"),
    uiOutput("statement4"),
    uiOutput("statement3"),
    titlePanel("Spinner visualization:"),
    plotOutput("spinner",width="600px",height="600px")
)


sns = function(mu,sig){
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
        text((1-0.33)*cos(2*pi*pc),(1-0.33)*sin(2*pi*pc),signif(mu+sig*qnorm(p),2),cex=0.9)
    }
    for(z in z2[!(z2 %in% z1)]){
        p = pnorm(z)
        pc = min(max(p,0.005),0.995)
        text((1-0.26)*cos(2*pi*pc),(1-0.26)*sin(2*pi*pc),signif(mu+sig*qnorm(p),2),cex=0.9)
    }
    for(z in z3[!(z3 %in% z2) & abs(z3)<2]){
        p = pnorm(z)
        pc = min(max(p,0.005),0.995)
        text((1-0.16)*cos(2*pi*pc),(1-0.16)*sin(2*pi*pc),signif(mu+sig*qnorm(p),2),srt=(90+p*360)%%180-90,cex=0.9)
    }
    for(z in z4[!(z4 %in% z3) & abs(z4)<1.5]){
        p = pnorm(z)
        pc = min(max(p,0.005),0.995)
        text((1-0.1)*cos(2*pi*pc),(1-0.1)*sin(2*pi*pc),signif(mu+sig*qnorm(p),2),srt=(90+p*360)%%180-90,cex=0.6)
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
    text(0,0,"X\nA Normal Distribution")
}


server <- function(input, output, session) {
    options(scipen=100)
    # output$statement = renderUI({withMathJax(paste("$$\\mu =  ",input$mu,"$$\n $$\\sigma = ",input$sig," $$ \n $$X \\sim \\mathcal{N}(",input$mu,", ",input$sig,") $$"))})
    output$density = renderPlot({
        mu = input$mu
        sig = input$sig
        x = input$x
        Z = seq(-3,3,0.01)
        z = (x-mu)/sig
        d = input$d
        xd = mu + input$d
        zd = input$d/sig
        x1 = input$x1
        x2 = input$x2
        z1 = (x1-mu)/sig
        z2 = (x2-mu)/sig
        par(mar=c(7,0,2,0))
        plot(Z,dnorm(Z),type="l",lwd=3,xlim = c(-3,3),ylim=c(0,0.4),ann=F,axes=F)
        if(input$type == "l"){
            polygon(c(Z[Z<=z],z,-3),c(dnorm(Z[Z<=z]),0,0),col=rgb(1,0.5,0.5,0.5))
            mtext(paste("P(X<",signif(x,4),") = P(Z<",signif(z,4),") = ",round(pnorm(z),4),collapse=""),3,0,cex=1.5)
        }
        if(input$type == "r"){
            polygon(c(Z[Z>=z],3,z),c(dnorm(Z[Z>=z]),0,0),col=rgb(1,0.5,0.5,0.5))
            mtext(paste("P(X>",signif(x,4),") = P(Z>",signif(z,4),") = ",round(1-pnorm(z),4),collapse=""),3,0,cex=1.5)
        }
        if(input$type == "c"){
            if(zd>0){
                polygon(c(Z[Z>=-zd & Z<=zd],zd,-zd),c(dnorm(Z[Z>=-zd & Z<=zd]),0,0),col=rgb(1,0.5,0.5,0.5))
                zz = signif(zd,4)
                dd = signif(d,4)
                pp = signif(2*pnorm(zd)-1,4)
                mm = signif(mu,4)
                if(mm>=0){
                    mtext(paste("P(|X -",mm,"|<",dd,") = P(|Z|<",zz,") = ",pp,collapse=""),3,0,cex=1.5)
                } else {
                    mtext(paste("P(|X -(",mm,")|<",dd,") = P(|Z|<",zz,") = ",pp,collapse=""),3,0,cex=1.5)
                }
            } else {
                zz = signif(zd,4)
                dd = signif(d,4)
                pp = 0
                mm = signif(mu,4)
                if(mm>=0){
                    mtext(paste("P(|X -",mm,"|<",dd,") = P(|Z|<",zz,") = ",pp,collapse=""),3,0,cex=1.5)
                } else {
                    mtext(paste("P(|X -(",mm,")|<",dd,") = P(|Z|<",zz,") = ",pp,collapse=""),3,0,cex=1.5)
                }
            }
        }
        if(input$type == "t"){
            if(zd>0){
                polygon(c(Z[Z<=-zd],-zd,-3),c(dnorm(Z[Z<=-zd]),0,0),col=rgb(1,0.5,0.5,0.5))
                polygon(c(Z[Z>=zd],3,zd),c(dnorm(Z[Z>=zd]),0,0),col=rgb(1,0.5,0.5,0.5))
                zz = signif(zd,4)
                dd = signif(d,4)
                pp = signif(2-2*pnorm(zd),4)
                mm = signif(mu,4)
                if(mm>=0){
                    mtext(paste("P(|X -",mm,"|>",dd,") = P(|Z|>",zz,") = ",pp,collapse=""),3,0,cex=1.5)
                } else {
                    mtext(paste("P(|X -(",mm,")|>",dd,") = P(|Z|>",zz,") = ",pp,collapse=""),3,0,cex=1.5)
                }
            } else {
                polygon(c(Z,3,-3),c(dnorm(Z),0,0),col=rgb(1,0.5,0.5,0.5))
                zz = signif(zd,4)
                dd = signif(d,4)
                pp = 1
                mm = signif(mu,4)
                if(mm>=0){
                    mtext(paste("P(|X -",mm,"|>",dd,") = P(|Z|>",zz,") = ",pp,collapse=""),3,0,cex=1.5)
                } else {
                    mtext(paste("P(|X -(",mm,")|>",dd,") = P(|Z|>",zz,") = ",pp,collapse=""),3,0,cex=1.5)
                }
            }
            
        }
        if(input$type == "b"){
            zz1 = signif(z1,4)
            zz2 = signif(z2,4)
            xx1 = signif(x1,4)
            xx2 = signif(x2,4)
            polygon(c(Z[Z>=z1 & Z<=z2],z2,z1),c(dnorm(Z[Z>=z1 & Z<=z2]),0,0),col=rgb(1,0.5,0.5,0.5))
            pp = max(signif(pnorm(z2)-pnorm(z1),4),0)
            mtext(paste("P(",xx1,"<X<",xx2,") = P(",zz1,"<Z<",zz2,") = ",pp,collapse=""),3,0,cex=1.5)
        }
        axis(1,seq(-3,3,1),line=0.5)
        mtext('z',side=1,line=2.2)
        axis(1,seq(-3,3,1),seq(mu-3*sig,mu+3*sig,sig),line=3.7)
        mtext('x',side=1,line=5.4)
        abline(h=seq(0,0.4,0.05),lwd=0.5)
        abline(v=seq(-3,3,0.2),lwd=0.5)
        abline(h=0,lwd=2)
    })
    output$statement2 = renderUI({withMathJax(paste("$$\\mu =  ",input$mu,"$$\n $$\\sigma = ",input$sig," $$ \n $$X \\sim \\mathcal{N}(",input$mu,", ",input$sig,") $$"))})
    output$statement3 = renderUI({
        mu = input$mu
        sig = input$sig
        x = input$x
        Z = seq(-3,3,0.01)
        z = (x-mu)/sig
        d = input$d
        xd = mu + input$d
        zd = input$d/sig
        x1 = input$x1
        x2 = input$x2
        z1 = (x1-mu)/sig
        z2 = (x2-mu)/sig
        if(input$type=="l"){
            s = paste("$$ P(X<",signif(x,4),") ~=~ P(Z<",signif(z,4),") ~=~ ",round(pnorm(z),4)," $$")
        }
        if(input$type == "r"){
           s = paste("$$P(X>",signif(x,4),") ~=~ P(Z>",signif(z,4),") ~=~ ",round(1-pnorm(z),4),"$$",collapse="")
        }
        if(input$type == "c"){
            if(zd>0){
                zz = signif(zd,4)
                dd = signif(d,4)
                pp = signif(2*pnorm(zd)-1,4)
                mm = signif(mu,4)
                if(mm>=0){
                    s = paste("$$P\\left(\\big|X -",mm,"\\big|<",dd,"\\right) ~=~ P\\left(\\big|Z\\big|<",zz,"\\right) ~=~ ",pp,"$$",collapse="")
                } else {
                    s = paste("$$P\\left(\\big|X -(",mm,")\\big|<",dd,"\\right) ~=~ P\\left(\\big|Z\\big|<",zz,"\\right) ~=~ ",pp,"$$",collapse="")
                }
            } else {
                zz = signif(zd,4)
                dd = signif(d,4)
                pp = 0
                mm = signif(mu,4)
                if(mm>=0){
                    s = paste("$$P\\left(\\big|X -",mm,"\\big|<",dd,"\\right) ~=~ P\\left(\\big|Z\\big|<",zz,"\\right) ~=~ ",pp,"$$",collapse="")
                } else {
                    s = paste("$$P\\left(\\big|X -(",mm,")\\big|<",dd,"\\right) ~=~ P\\left(\\big|Z\\big|<",zz,"\\right) ~=~ ",pp,"$$",collapse="")
                }
            }
        }
        if(input$type == "t"){
            if(zd>0){
                zz = signif(zd,4)
                dd = signif(d,4)
                pp = signif(2-2*pnorm(zd),4)
                mm = signif(mu,4)
                if(mm>=0){
                    s = paste("$$P(|X -",mm,"|>",dd,") ~=~ P(|Z|>",zz,") ~=~ ",pp,"$$",collapse="")
                } else {
                    s = paste("$$P(|X -(",mm,")|>",dd,") ~=~ P(|Z|>",zz,") ~=~ ",pp,"$$",collapse="")
                }
            } else {
                zz = signif(zd,4)
                dd = signif(d,4)
                pp = 1
                mm = signif(mu,4)
                if(mm>=0){
                    s = paste("$$P(|X -",mm,"|>",dd,") ~=~ P(|Z|>",zz,") ~=~ ",pp,"$$",collapse="")
                } else {
                    s = paste("$$P(|X -(",mm,")|>",dd,") ~=~ P(|Z|>",zz,") ~=~ ",pp,"$$",collapse="")
                }
            }
            
        }       
        if(input$type == "b"){
            zz1 = signif(z1,4)
            zz2 = signif(z2,4)
            xx1 = signif(x1,4)
            xx2 = signif(x2,4)
            pp = max(signif(pnorm(z2)-pnorm(z1),4),0)
            s = paste("$$P(",xx1,"<X<",xx2,") ~=~ P(",zz1,"<Z<",zz2,") ~=~ ",pp,"$$",collapse="")
        }
        withMathJax(s)
    })
    output$statement4 = renderUI({
        mu = input$mu
        sig = input$sig
        x = input$x
        Z = seq(-3,3,0.01)
        z = (x-mu)/sig
        d = input$d
        xd = mu + input$d
        zd = input$d/sig
        x1 = input$x1
        x2 = input$x2
        z1 = (x1-mu)/sig
        z2 = (x2-mu)/sig
        if(input$type=="l"){
            if(mu>=0){
                s = sprintf("$$x = %.4g$$ \n $$z = \\frac{x-\\mu}{\\sigma} = \\frac{%.4g-%.4g}{%.4g} = %.4g $$",x,x,mu,sig,z)
            } else {
                s = sprintf("$$x = %.4g$$ \n $$z = \\frac{x-\\mu}{\\sigma} = \\frac{%.4g-(%.4g)}{%.4g} = %.4g $$",x,x,mu,sig,z)
            }
        }
        if(input$type == "r"){
            if(mu>=0){
                s = sprintf("$$x = %.4g$$ \n $$z = \\frac{x-\\mu}{\\sigma} = \\frac{%.4g-%.4g}{%.4g} = %.4g $$",x,x,mu,sig,z)
            } else {
                s = sprintf("$$x = %.4g$$ \n $$z = \\frac{x-\\mu}{\\sigma} = \\frac{%.4g-(%.4g)}{%.4g} = %.4g $$",x,x,mu,sig,z)
            }
        }
        if(input$type == "c"){
            s = sprintf("$$d = %.4g$$ \n $$z = \\frac{d}{\\sigma} = \\frac{%.4g}{%.4g} = %.4g $$",d,d,sig,zd)
        }
        if(input$type == "t"){
            s = sprintf("$$d = %.4g$$ \n $$z = \\frac{d}{\\sigma} = \\frac{%.4g}{%.4g} = %.4g $$",d,d,sig,zd)
        }       
        if(input$type == "b"){
            if(mu>=0){
                s = sprintf("$$x_1 = %.4g$$ \n $$x_2=%.4g$$ \n $$z_1 = \\frac{x_1-\\mu}{\\sigma} = \\frac{%.4g-%.4g}{%.4g} = %.4g $$ \n $$z_2 = \\frac{x_2-\\mu}{\\sigma} = \\frac{%.4g-%.4g}{%.4g}  = %.4g $$",x1,x2,x1,mu,sig,z1,x2,mu,sig,z2)
            } else {
                s = sprintf("$$x_1 = %.4g$$ \n $$x_2=%.4g$$ \n $$z_1 = \\frac{x_1-\\mu}{\\sigma} = \\frac{%.4g-( %.4g )}{%.4g} = %.4g $$ \n $$z_2 = \\frac{x_2-\\mu}{\\sigma} = \\frac{%.4g-( %.4g )}{%.4g}  = %.4g $$",x1,x2,x1,mu,sig,z1,x2,mu,sig,z2)
            }
        }
        withMathJax(s)
    })
    
    
    output$spinner = renderPlot({
        mu = input$mu
        sig = input$sig
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
            text((1-0.35)*cos(2*pi*pc),(1-0.35)*sin(2*pi*pc),signif(mu+sig*qnorm(p),4),cex=0.9)
        }
        for(z in z2[!(z2 %in% z1)]){
            p = pnorm(z)
            pc = min(max(p,0.005),0.995)
            text((1-0.26)*cos(2*pi*pc),(1-0.26)*sin(2*pi*pc),signif(mu+sig*qnorm(p),4),cex=0.9)
        }
        for(z in z3[!(z3 %in% z2) & abs(z3)<2]){
            p = pnorm(z)
            pc = min(max(p,0.005),0.995)
            text((1-0.16)*cos(2*pi*pc),(1-0.16)*sin(2*pi*pc),signif(mu+sig*qnorm(p),4),srt=(90+p*360)%%180-90,cex=0.9)
        }
        for(z in z4[!(z4 %in% z3) & abs(z4)<1.5]){
            p = pnorm(z)
            pc = min(max(p,0.005),0.995)
            text((1-0.1)*cos(2*pi*pc),(1-0.1)*sin(2*pi*pc),signif(mu+sig*qnorm(p),4),srt=(90+p*360)%%180-90,cex=0.6)
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
        P = seq(0,1,0.001)
        col = rgb(1,0.5,0.5,0.5)
        if(input$type=="l"){
            pr = pnorm((input$x-mu)/sig)
            polygon(c(0,cos(2*pi*P[P<=pr])),c(0,sin(2*pi*P[P<=pr])),col=col)
        }
        if(input$type == "r"){
            pl = pnorm((input$x-mu)/sig)
            polygon(c(0,cos(2*pi*P[P>=pl])),c(0,sin(2*pi*P[P>=pl])),col=col)
        }
        if(input$type == "c"){
            pl = pnorm(-input$d/sig)
            pr = pnorm(input$d/sig)
            polygon(c(0,cos(2*pi*P[P>=pl & P<=pr])),c(0,sin(2*pi*P[P>=pl & P<=pr])),col=col)
        }
        if(input$type == "t"){
            pl = pnorm(-input$d/sig)
            pr = pnorm(input$d/sig)
            polygon(c(0,cos(2*pi*c(P[P>=pr],P[P<=pl]))),c(0,sin(2*pi*c(P[P>=pr],P[P<=pl]))),col=col)
        }       
        if(input$type == "b"){
            pl = pnorm((input$x1-mu)/sig)
            pr = pnorm((input$x2-mu)/sig)
            polygon(c(0,cos(2*pi*P[P>=pl & P<=pr])),c(0,sin(2*pi*P[P>=pl & P<=pr])),col=col)
        }
        
        text(1,1,"U\nStandard Uniform\nDistribution")
        text(0,0,sprintf("X\nA Normal Distribution\nwith\nmean = %.4g \n and \nstdev = %.4g",input$mu,input$sig))
        })
}







# Run the application 
shinyApp(ui = ui, server = server)
