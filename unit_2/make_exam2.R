library("exams")
set.seed(54321)

# folders = list.dirs(recursive = F)

name3 = "exam2"

myexam <- list("p2a_overview/q1_cards.Rmd",
               "p2a_overview/q2_probdist.Rmd",
               "p2a_overview/q3_comb_single.Rmd",
               "p2a_overview/q4_perm_single.Rmd",
               "p2a_overview/q5_binomial.Rmd",
               "p2a_overview/q6_bobhappy.Rmd",
               "p2a_overview/q7_winlose.Rmd")

s = c("q1_cards",
      "q2_probdist",
      "q3_comb",
      "q4_perm",
      "q5_binomial",
      "q6_bobhappy",
      "q7_winlose")

exams2moodle(myexam, n = 50,name=name3,
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

tx  <- readLines(paste0("out/",name3,".xml",collapse=""))
tx2  <- gsub(pattern = "𝐱", replace = "x", x = tx)
tx2  <- gsub(pattern = "𝐰", replace = "w", x = tx2)
tx2  <- gsub(pattern = "𝙰𝙽𝙳", replace = "AND", x = tx2)
tx2  <- gsub(pattern = "𝙾𝚁", replace = "OR", x = tx2)
tx2  <- gsub(pattern = "𝙽𝙾𝚃", replace = "NOT", x = tx2)
writeLines(tx2, con=paste0("out/",name3,"_fixed.xml",collapse=""))

