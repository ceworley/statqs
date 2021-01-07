library("exams")
set.seed(23456)

# s = c("compare_boxplots",
#       "identify_distributions_boxplots",
#       "read_boxplot.Rmd")

name3 = "standard_score"
s = c("standard_score")#gsub(".Rmd","",list.files(pattern=".Rmd"))

myexam <- c(paste(s,".Rmd",sep=""))

exams2moodle(myexam, n = 100,name=name3,
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

tx  <- readLines(paste0("out/",name3,".xml",collapse=""))
tx2  <- gsub(pattern = "𝐱", replace = "x", x = tx)
tx2  <- gsub(pattern = "𝙰𝙽𝙳", replace = "AND", x = tx2)
tx2  <- gsub(pattern = "𝙾𝚁", replace = "OR", x = tx2)
tx2  <- gsub(pattern = "𝙽𝙾𝚃", replace = "NOT", x = tx2)
writeLines(tx2, con=paste0("out/",name3,"_fixed.xml",collapse=""))


# for(ss in s){
#    tx  <- readLines("template/not_exam.html")
#    tx2  <- gsub(pattern = "##ExampleName##", replace = ss, x = tx)
#    ffnn = paste0("template/not_exam_",ss,".html")
#    writeLines(tx2, con=ffnn)
#    exams2html(paste0(ss,".Rmd"),
#               name=ss,
#               question = "<hr><h3>Question</h3>",
#               solution="<hr><hr><hr><h3>Solution</h3>",
#               template=ffnn,
#               dir = "out",
#               converter="pandoc-mathml"
#    )
#    unlink(ffnn)
#    file.rename(paste0("out/",ss,"1.html"),paste0("out/",ss,".html"))
# }
