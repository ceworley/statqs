library("exams")
set.seed(100)

name = "allproblems"
s = gsub(".Rmd","",list.files(pattern=".Rmd",recursive = T))
myexam = c(paste(s,".Rmd",sep=""))

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

exams2pdf(myexam[1],texdir="tex",
             n=1,name=name,
             dir = "out",solution=F
             )

