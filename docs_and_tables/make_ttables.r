library(latex2exp)
options(scipen=100)
gamma = c(0.8,0.9,0.95,0.96,0.98,0.99,0.995,0.996,0.998,0.999)
left = round(gamma+(1-gamma)/2,4)
right = round(1-left,4)
alpha = round(1-gamma,4)
df = 1:40
nrows = length(df)+5
ncols = length(gamma)+1
c1wd = 0.15
cwd = (1-c1wd)/(ncols-1)

pdf("ttable.pdf",8.5,11,title="T table",paper="letter")
par(mar=c(0,0,0,0))
plot(0,0,type="n",axes=F,ann=F,xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
for(i in 0:nrows){
  abline(h=i/nrows)
}
abline(v=0)
abline(v=c1wd)
for(i in 1:ncols){
  abline(v=c1wd+cwd*i)
}
lab = c("P(T<t)","P(T>t)","P(|T|<t)","P(|T|>t)")
polygon(c(0,1,1,0),c(1,1,1-4/nrows,1-4/nrows),col=rgb(0.8,0.5,0.5,0.4),lwd=3)
for(i in 1:4){
  ypos = 1-(i-0.5)/nrows
  text(c1wd/2,ypos,lab[i],adj = c(0.5,0.5),family = "mono")
}
for(j in 1:length(gamma)){
    xpos = c1wd+(j-0.5)*cwd
    ypos = 1-(1:4-0.5)/nrows
    text(xpos,ypos,c(left[j],right[j],gamma[j],alpha[j]),family = "mono")
}
polygon(c(0,0,c1wd,c1wd),c(0,1-4/nrows,1-4/nrows,0),lwd=3,col=rgb(0.2,0.4,1,0.2))
polygon(c(c1wd,c1wd,1,1),c(0,1-4/nrows,1-4/nrows,0),lwd=3,col=rgb(0.8,0.8,0.3,0.2))
for(i in seq(5,nrows,2)){
  ylo = 1-i/nrows
  yhi = 1-(i-1)/nrows
  polygon(c(0,1,1,0),c(ylo,ylo,yhi,yhi),col=rgb(0,0,0,0.05))
}
text(c1wd/2,1-(5-0.5)/nrows,"df = ",family = "mono")
text(c1wd+cwd/2,1-(5-0.5)/nrows,"t = ",family = "mono")
for(i in 1:length(df)){
  ypos = 1-(5-0.5)/nrows-i/nrows
  text(c1wd/2,ypos,df[i],family = "mono")
  for(j in 1:length(left)){
    xpos = c1wd+(j-0.5)*cwd
    text(xpos,ypos,sprintf("%.2f",round(qt(left[j],df[i]),2)),family = "mono")
  }
}


# second page


df = c(41:50,seq(55,100,5),seq(120,500,20),Inf)
nrows = length(df)+5

par(mar=c(0,0,0,0))
plot(0,0,type="n",axes=F,ann=F,xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
for(i in 0:nrows){
  abline(h=i/nrows)
}
abline(v=0)
abline(v=c1wd)
for(i in 1:ncols){
  abline(v=c1wd+cwd*i)
}

polygon(c(0,1,1,0),c(1,1,1-4/nrows,1-4/nrows),col=rgb(0.8,0.5,0.5,0.4),lwd=3)
for(i in 1:4){
  ypos = 1-(i-0.5)/nrows
  text(c1wd/2,ypos,lab[i],adj = c(0.5,0.5),family = "mono")
}
for(j in 1:length(gamma)){
    xpos = c1wd+(j-0.5)*cwd
    ypos = 1-(1:4-0.5)/nrows
    text(xpos,ypos,c(left[j],right[j],gamma[j],alpha[j]),family = "mono")
}
polygon(c(0,0,c1wd,c1wd),c(0,1-4/nrows,1-4/nrows,0),lwd=3,col=rgb(0.2,0.4,1,0.2))
polygon(c(c1wd,c1wd,1,1),c(0,1-4/nrows,1-4/nrows,0),lwd=3,col=rgb(0.8,0.8,0.3,0.2))
for(i in seq(5,nrows,2)){
  ylo = 1-i/nrows
  yhi = 1-(i-1)/nrows
  polygon(c(0,1,1,0),c(ylo,ylo,yhi,yhi),col=rgb(0,0,0,0.05))
}
text(c1wd/2,1-(5-0.5)/nrows,"df = ",family = "mono")
text(c1wd+cwd/2,1-(5-0.5)/nrows,"t = ",family = "mono")
for(i in 1:length(df)){
  ypos = 1-(5-0.5)/nrows-i/nrows
  text(c1wd/2,ypos,gsub("Inf",expression(infinity),df[i]),family = "mono")
  for(j in 1:length(left)){
    xpos = c1wd+(j-0.5)*cwd
    text(xpos,ypos,sprintf("%.2f",round(qt(left[j],df[i]),2)),family = "mono")
  }
}

dev.off()

### print version

library(latex2exp)
options(scipen=100)
gamma = c(0.8,0.9,0.95,0.96,0.98,0.99,0.995,0.996,0.998,0.999)
left = round(gamma+(1-gamma)/2,4)
right = round(1-left,4)
alpha = round(1-gamma,4)
df = 1:40
nrows = length(df)+5
ncols = length(gamma)+1
c1wd = 0.15
cwd = (1-c1wd)/(ncols-1)

pdf("t_table_printable.pdf",8.5,11,title="T table",paper="letter")
par(mar=c(0,0,0,0))
plot(0,0,type="n",axes=F,ann=F,xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
for(i in 0:nrows){
  abline(h=i/nrows)
}
abline(v=0)
abline(v=c1wd)
for(i in 1:ncols){
  abline(v=c1wd+cwd*i)
}
lab = c("P(T<t)","P(T>t)","P(|T|<t)","P(|T|>t)")
polygon(c(0,1,1,0),c(1,1,1-4/nrows,1-4/nrows),lwd=3)
for(i in 1:4){
  ypos = 1-(i-0.5)/nrows
  text(c1wd/2,ypos,lab[i],adj = c(0.5,0.5),family = "mono")
}
for(j in 1:length(gamma)){
    xpos = c1wd+(j-0.5)*cwd
    ypos = 1-(1:4-0.5)/nrows
    text(xpos,ypos,c(left[j],right[j],gamma[j],alpha[j]),family = "mono")
}
polygon(c(0,0,c1wd,c1wd),c(0,1-4/nrows,1-4/nrows,0),lwd=3)
polygon(c(c1wd,c1wd,1,1),c(0,1-4/nrows,1-4/nrows,0),lwd=3)
text(c1wd/2,1-(5-0.5)/nrows,"df = ",family = "mono")
text(c1wd+cwd/2,1-(5-0.5)/nrows,"t = ",family = "mono")
for(i in 1:length(df)){
  ypos = 1-(5-0.5)/nrows-i/nrows
  text(c1wd/2,ypos,df[i],family = "mono")
  for(j in 1:length(left)){
    xpos = c1wd+(j-0.5)*cwd
    text(xpos,ypos,sprintf("%.2f",round(qt(left[j],df[i]),2)),family = "mono")
  }
}


# second page


df = c(41:50,seq(55,100,5),seq(120,500,20),Inf)
nrows = length(df)+5

par(mar=c(0,0,0,0))
plot(0,0,type="n",axes=F,ann=F,xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
for(i in 0:nrows){
  abline(h=i/nrows)
}
abline(v=0)
abline(v=c1wd)
for(i in 1:ncols){
  abline(v=c1wd+cwd*i)
}

polygon(c(0,1,1,0),c(1,1,1-4/nrows,1-4/nrows),lwd=3)
for(i in 1:4){
  ypos = 1-(i-0.5)/nrows
  text(c1wd/2,ypos,lab[i],adj = c(0.5,0.5),family = "mono")
}
for(j in 1:length(gamma)){
    xpos = c1wd+(j-0.5)*cwd
    ypos = 1-(1:4-0.5)/nrows
    text(xpos,ypos,c(left[j],right[j],gamma[j],alpha[j]),family = "mono")
}
polygon(c(0,0,c1wd,c1wd),c(0,1-4/nrows,1-4/nrows,0),lwd=3)
polygon(c(c1wd,c1wd,1,1),c(0,1-4/nrows,1-4/nrows,0),lwd=3)
text(c1wd/2,1-(5-0.5)/nrows,"df = ",family = "mono")
text(c1wd+cwd/2,1-(5-0.5)/nrows,"t = ",family = "mono")
for(i in 1:length(df)){
  ypos = 1-(5-0.5)/nrows-i/nrows
  text(c1wd/2,ypos,gsub("Inf",expression(infinity),df[i]),family = "mono")
  for(j in 1:length(left)){
    xpos = c1wd+(j-0.5)*cwd
    text(xpos,ypos,sprintf("%.2f",round(qt(left[j],df[i]),2)),family = "mono")
  }
}

dev.off()
