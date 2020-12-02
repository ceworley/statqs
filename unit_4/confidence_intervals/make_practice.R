library("exams")
set.seed(8)

name = "p4_confint_practice"
s = gsub(".Rmd","",list.files(pattern=".Rmd",recursive = T))

myexam <- c(paste(s,".Rmd",sep=""))

exams2moodle(myexam, 
             n=50,
             name=name,
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

