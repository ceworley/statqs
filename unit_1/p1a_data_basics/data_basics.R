library("exams")

set.seed(12345)

s = c('list_counting_basic',
      'list_counting_between',
      'list_counting_large',
      'make_frequency_distribution',
      'read_frequency_distribution',
      'read_histogram',
      'read_histogram_discrete',
      'read_circle_freq')
      
s = c('list_counting_large')

myexam <- c(paste(s,".Rmd",sep=""))

exams2moodle(myexam, n = 50,name="data_basics",
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

tx  <- readLines("out/data_basics.xml")
tx2  <- gsub(pattern = "𝐱", replace = "x", x = tx)
tx2  <- gsub(pattern = "𝙰𝙽𝙳", replace = "AND", x = tx2)
tx2  <- gsub(pattern = "𝙾𝚁", replace = "OR", x = tx2)
tx2  <- gsub(pattern = "𝙽𝙾𝚃", replace = "NOT", x = tx2)
writeLines(tx2, con="out/data_basics_fixed.xml")


for(ss in s){
   tx  <- readLines("template/not_exam.html")
   tx2  <- gsub(pattern = "##ExampleName##", replace = ss, x = tx)
   ffnn = paste0("template/not_exam_",ss,".html")
   writeLines(tx2, con=ffnn)
   exams2html(paste0(ss,".Rmd"),
              name=ss,
              question = "<hr><h3>Question</h3>",
              solution="<hr><hr><hr><h3>Solution</h3>",
              template=ffnn,
              dir = "out",
              converter="pandoc-mathml"
   )
   unlink(ffnn)
}
