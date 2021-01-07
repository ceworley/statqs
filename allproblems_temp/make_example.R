library("exams")
set.seed(100)

htmlintro = readLines("plain8intro.html")
htmlend = readLines("plain8end.html")

name = "allproblems"
s = gsub(".Rmd","",list.files(pattern=".Rmd",recursive = T))
s2 = gsub("/","-",s)
myexam = c(paste(s,".Rmd",sep=""))
qurep = paste0('<p id="',s2,'"><h2>',s2,"</h2></p>")
qu = "REPLACEME"

drs = list.dirs()
drs2 = character()
for(dr in drs){
  lf = list.files(dr,pattern=".Rmd")
  if(length(lf)>0){
    drs2=c(drs2,dr)
  }
}

drs2

aaa = paste0('<p><a href="#',s2,'">',s2,"</a></p>")
newfile = paste0(paste0(htmlintro,collapse=""),paste0(aaa,collapse=""),paste0(htmlend,collapse=""),collapse="")
writeLines(newfile,"template.html")

exams2html(myexam,question=qu,template="./template.html",
             n=1,name=name,
             dir = "out",solution=F
             )


ml = readLines("out/allproblems1.html")
ss = paste0(ml,collapse="\n")
for(i in 1:length(qurep)){
  ss = sub("REPLACEME",qurep[i],ss)
}
writeLines(ss,"out/final.html")

