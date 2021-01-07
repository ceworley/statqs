library("exams")
set.seed(100)

name = "allproblems"
s = gsub(".Rmd","",list.files(pattern=".Rmd",recursive = T))
myexam <- c(paste(s,".Rmd",sep=""))

exams2html(myexam,question=s,
             n=1,name="all_probs",
             dir = "out",solution=F
             )

