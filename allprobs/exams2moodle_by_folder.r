library("exams")
source("exams2html_mod.r")
set.seed(200)

n = 2 #number of versions
outdir = "outui"
fn = "statqs"
s = gsub(".Rmd","",list.files(pattern=".Rmd",recursive = T))

## Find the folders that have at least one Rmd file
## Create an xml in that folder containing "n" copies of each question

fold = character(0)
for(s2 in str_split(s,"/")){
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
  exams2moodle(files,n=n,dir="xmls",converter="pandoc-mathjax",
               name=paste0(unlist(strsplit(fold2,"/")),collapse="-"))
}
