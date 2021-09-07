library("exams")
set.seed(200)

n = 50 #number of versions
outdir = "outmoodle"
fn = "statqs"
s = gsub(".Rmd","",list.files(pattern=".Rmd",recursive = T))

## Find the folders that have at least one Rmd file
## Create an xml in that folder containing "n" copies of each question

fold = character(0)
for(s2 in strsplit(s,"/")){
  if(length(s2)>1){
    s3 = paste(s2[1:(length(s2)-1)],collapse="/")
  } else {
    s3 = ""
  }
  if(!(s3 %in% fold)){
    fold = c(fold,s3)
  }
}

for(fold2 in fold[1:length(fold)]){
  print(fold2)
  files = list.files(fold2,pattern=".Rmd",full.names = T)
  print(files)
  name = paste0("training__",paste0(unlist(strsplit(fold2,"/")),collapse="-"))
  exams2moodle(files,n=n,dir=outdir,converter="pandoc-mathml",
               name=name)
  fn2 = paste0(outdir,"/",name,".xml")
  tx  <- readLines(fn2)
  tx  <- gsub(pattern = "𝐱", replace = "x", x = tx)
  tx  <- gsub(pattern = "𝙰𝙽𝙳", replace = "AND", x = tx)
  tx  <- gsub(pattern = "𝙽𝙾𝚃", replace = "NOT", x = tx)
  tx  <- gsub(pattern = "𝙾𝚁", replace = "OR", x = tx)
  writeLines(tx, con=fn2)
}
