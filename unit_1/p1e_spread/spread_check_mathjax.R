library("exams")
set.seed(23456)

s = c("get_SD2")

myexam <- c(paste(s,".Rmd",sep=""))


name3 = "spread"
exams2moodle(myexam, n = 1,name=name3,
             stitle=s,
             dir = "out2")#,
             #converter="pandoc-mathml"
             #)

tx  <- readLines(paste0("out2/",name3,".xml",collapse=""))
tx2  <- gsub(pattern = "𝐱", replace = "x", x = tx)
tx2  <- gsub(pattern = "𝙰𝙽𝙳", replace = "AND", x = tx2)
tx2  <- gsub(pattern = "𝙾𝚁", replace = "OR", x = tx2)
tx2  <- gsub(pattern = "𝙽𝙾𝚃", replace = "NOT", x = tx2)
writeLines(tx2, con=paste0("out2/",name3,"_fixed.xml",collapse=""))

# 
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
#    file.rename(paste0("out2/",ss,"1.html"),paste0("out2/",ss,".html"))
# }
