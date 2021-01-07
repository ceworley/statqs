library("exams")

# set.seed(12345)
set.seed(23456)

s = c("read_frequency_distribution_prop",
	"list_proportion_basic",
	"read_histogram_discrete",
	"list_proportion_between",
	"read_histogram",
	"list_proportion_large",
	"make_relfreq_dens",
	"read_circle_prop")


myexam <- c(paste(s,".Rmd",sep=""))

exams2moodle(myexam, n = 40,name="proportion",
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

tx  <- readLines("out/proportion.xml")
tx2  <- gsub(pattern = "𝐱", replace = "x", x = tx)
tx2  <- gsub(pattern = "𝙰𝙽𝙳", replace = "AND", x = tx2)
tx2  <- gsub(pattern = "𝙾𝚁", replace = "OR", x = tx2)
tx2  <- gsub(pattern = "𝙽𝙾𝚃", replace = "NOT", x = tx2)
writeLines(tx2, con="out/proportion_fixed.xml")


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
