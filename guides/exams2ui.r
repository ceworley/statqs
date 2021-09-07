library("exams")
source("exams2html_mod.r")
set.seed(100)

nov = 5 #number of versions
outdir = "outui"
fn = "statqs"
s = gsub(".Rmd","",list.files(pattern=".Rmd",recursive = T))

html1 = '<!DOCTYPE html>
<html>
<head>
<title>Stat Qs</title>
<style>
 body { padding-bottom: 30vh; }
/* Remove default bullets */
ul, #myUL {
  list-style-type: none;
}
/* Remove margins and padding from the parent ul */
#myUL {
  margin: 0;
  padding: 0;
}
/* Style the caret/arrow */
.caret {
  cursor: pointer;
  user-select: none; /* Prevent text selection */
}
/* Create the caret/arrow with a unicode, and style it */
.caret::before {
  content: "\\2B24";
  color: #AAAAAA;
  display: inline-block;
}
.erase::before {
  content: none;
}
.sel::before {
  content: "\\2B24";
  color: green;
  display: inline-block;
}
/* Hide the nested list */
.nested {
  display: none;
}
/* Show the nested list when the user clicks on the caret/arrow (with JavaScript) */
.active {
  display: block;
}
#myBtn1 {
  display: block;
  position: fixed;
  bottom: 0px;
  left: 0px;
  width: 10vw;
  height: 5vh;
  z-index: 99;
  font-size: 3vh;
  border: none;
  outline: none;
  background-color: black;
  color: white;
  cursor: pointer;
  border-radius: 1vh;
}
#myBtn2 {
  display: block;
  position: fixed;
  bottom: 0px;
  left: 10vw;
  width: 10vw;
  height: 5vh;
  z-index: 99;
  font-size: 3vh;
  border: none;
  outline: none;
  background-color: blue;
  color: white;
  cursor: pointer;
  border-radius: 1vh;
}
#myBtn3 {
  display: block;
  position: fixed;
  bottom: 0px;
  left: 20vw;
  width: 10vw;
  height: 5vh;
  z-index: 99;
  font-size: 3vh;
  border: none;
  outline: none;
  background-color: red;
  color: white;
  cursor: pointer;
  border-radius: 1vh;
}
#myBtn4 {
  display: block;
  position: fixed;
  bottom: 0px;
  left: 30vw;
  width: 10vw;
  height: 5vh;
  z-index: 99;
  font-size: 3vh;
  border: none;
  outline: none;
  background-color: #555555;
  color: white;
  cursor: pointer;
  border-radius: 1vh;
}
#myBtn5 {
  display: block;
  position: fixed;
  bottom: 0px;
  left: 40vw;
  width: 10vw;
  height: 5vh;
  z-index: 99;
  font-size: 3vh;
  border: none;
  outline: none;
  background-color: #666666;
  color: white;
  cursor: pointer;
  border-radius: 1vh;
}
#myBtn6 {
  display: block;
  position: fixed;
  bottom: 0px;
  left: 50vw;
  width: 10vw;
  height: 5vh;
  z-index: 99;
  font-size: 3vh;
  border: none;
  outline: none;
  background-color: #555555;
  color: white;
  cursor: pointer;
  border-radius: 1vh;
}
#myBtn7 {
  display: block;
  position: fixed;
  bottom: 0px;
  left: 60vw;
  width: 10vw;
  height: 5vh;
  z-index: 99;
  font-size: 3vh;
  border: none;
  outline: none;
  background-color: #666666;
  color: white;
  cursor: pointer;
  border-radius: 1vh;
}
.button {
  background: #AAAAAA;
}
</style>'

html2 = '<script>
var qs = ["%s"];
var as = ["%s"];
var cur = 0;
function setUrl() {
    var queryString = window.location.search;
    var urlParams = new URLSearchParams(queryString);
    var pn = parseInt(urlParams.get("probnum"));
    if(isNaN(pn)){
      pn = 0;
    }
    if(pn<0){
      pn=0;
    }
    if(pn>=qs.length){
      pn=0;
    }
    cur = pn;
    document.getElementsByName("cake")[0].src = qs[pn];
    document.getElementsByName("duck")[0].src = as[pn];
    var n = qs.length;
    document.getElementById("myBtn4").onclick = function() {
      changeUrl((((pn-1) %% n ) + n ) %% n);
      };
    document.getElementById("myBtn5").onclick = function() {
      changeUrl((((pn+1) %% n ) + n ) %% n);
      };
    document.getElementById("myBtn6").onclick = function() {
      changeUrl((((pn-pn%%%d-%d) %% n ) + n ) %% n);
    };
    document.getElementById("myBtn7").onclick = function() {
      changeUrl((((pn-pn%%%d+%d) %% n ) + n ) %% n);
    };
    mybut = document.getElementById("but"+pn);
    mybut.style.background="#AAFFAA";
    mpn = mybut.parentNode.parentNode;
    mpn.parentElement.querySelector(".nested").classList.toggle("active");
    mpn.parentElement.querySelector(".caret").classList.toggle("sel");
    mpn = mpn.parentNode.parentNode;
    mpn.parentElement.querySelector(".nested").classList.toggle("active");
    mpn.parentElement.querySelector(".caret").classList.toggle("sel");
}
function changeUrl(i) {
    var queryString = window.location.search;
    var urlParams = new URLSearchParams(queryString);
    oldi = urlParams.get("probnum");
    urlParams.set("probnum",parseInt(i));
    window.history.replaceState("", "", window.location.pathname+"?probnum="+i);
    document.getElementsByName("cake")[0].src = qs[i];
    document.getElementsByName("duck")[0].src = as[i];
    var n = qs.length;
    document.getElementById("myBtn4").onclick = function() {
      changeUrl((((i-1) %% n ) + n ) %% n);
    };
    document.getElementById("myBtn5").onclick = function() {
      changeUrl(i+1);
    };
    document.getElementById("myBtn6").onclick = function() {
      changeUrl((((i-i%%%d-%d) %% n ) + n ) %% n);
    };
    document.getElementById("myBtn7").onclick = function() {
      changeUrl((((i-i%%%d+%d) %% n ) + n ) %% n);
    };
    var mybut = document.getElementById("but"+cur);
    mybut.style.background="#AAAAAA";
    var mpn = mybut.parentNode.parentNode;
    mpn.parentElement.querySelector(".caret").classList.toggle("sel");
    mpn = mpn.parentNode.parentNode;
    mpn.parentElement.querySelector(".caret").classList.toggle("sel");
    mybut = document.getElementById("but"+i);
    mybut.style.background="#AAFFAA";
    mpn = mybut.parentNode.parentNode;
    mpn.parentElement.querySelector(".caret").classList.toggle("sel");
    mpn = mpn.parentNode.parentNode;
    mpn.parentElement.querySelector(".caret").classList.toggle("sel");
    cur = i;
}
function topFunction() {
  document.getElementById("top").scrollIntoView();
}
function qFunction() {
  document.getElementsByName("cake")[0].scrollIntoView();
}
function aFunction() {
  document.getElementsByName("duck")[0].scrollIntoView();
}
document.addEventListener("keypress", function onEvent(event) {
    if (event.key === "q") {
        document.getElementsByName("cake")[0].scrollIntoView();
    }
    if (event.key === "t") {
        document.getElementById("top").scrollIntoView();
    }
    if (event.key === "a") {
        document.getElementsByName("duck")[0].scrollIntoView();
    }
});
function ScrollToTarget()
{
     document.getElementById("top").scrollIntoView(true);
};
</script>
</head>
<body onload="setUrl();ScrollToTarget();">

<div id="welcome" style="background:#dddddd; font: italic" >
Welcome to Chad Worley\'s statistics questions. Please also check out this
<a href="https://github.com/ceworley/statistics" target="_blank">project\'s github</a>.
These problems were originally used in Moodle, but they
should be easy to export to many other formats (see <a href="http://www.r-exams.org">R/exams</a>)... feel
free to reuse this work in a copyleft fashion.
<br><br>
You may find <a href="http://18.191.167.248:3838" target="_blank">these Shiny apps</a> helpful.
<br><br>
If the math equations are small, try clicking on an equation; you should be able to change your zoom settings.
</div>

<br>

<div id="top">
%s
</div>
<button onclick="topFunction()" id="myBtn1" title="Go to top">Top</button>
<button onclick="qFunction()" id="myBtn2" title="Go to question (shortcut=`q`)">Q</button>
<button onclick="aFunction()" id="myBtn3" title="Go to answer (shortcut=`a`)">A</button>
<button onclick="changeUrl(1)" id="myBtn4" title="Previous version">&#8592</button>
<button onclick="changeUrl(1)" id="myBtn5" title="Next version">&#8594</button>
<button onclick="changeUrl(1)" id="myBtn6" title="Previous question">&#8593</button>
<button onclick="changeUrl(1)" id="myBtn7" title="Next question">&#8595</button>
<center>
<iframe src=qs[1] frameborder=0 name="cake" id="cake" style ="border:2px solid black;height:95vh;background-color:#dadaff;" width="95%%"></></iframe>
</center>
<center>
<iframe src=as[1] frameborder=0 name="duck" id="duck" style ="border:2px solid black;height:95vh;background-color:#ffdada;" width="95%%"></></iframe>
</center>
<script>
var toggler = document.getElementsByClassName("caret");
var jjj;
for (jjj = 0; jjj < toggler.length; jjj++) {
  toggler[jjj].addEventListener("click", function() {
    this.parentElement.querySelector(".nested").classList.toggle("active");
  });
}
</script>
</body>
</html>'

mys = '<ul id="myUL">\n'
folders = character()
lasti = 0
for(kk in 1:length(s)){
  prob = s[kk]
  bits = unlist(strsplit(prob,"/"))
  nextkk = kk%%length(s)+1
  prevkk = (kk-2)%%length(s)+1
  nprob = s[nextkk]
  pprob = s[prevkk]
  for(i in 1:length(bits)){
    fol = paste0(bits[1:i],collapse="")
    fol2 = substr(bits[i],4,1000)
    if(!(fol %in% folders)){
      if(lasti>i){
        reps = lasti-i
        news = paste0(rep("</ul></li>\n",reps),collapse="")
        mys = paste0(mys,news,collapse="")
      }
      if(i<length(bits)){
        news = paste0(paste0(rep(' ',i),collapse=""),'<li><span class="caret">',fol2,'</span>\n<ul class="nested">\n',
                      collapse="")
        mys = paste0(mys,news,collapse="")
        lasti = i
      } else {
        jk = kk*nov-nov
        news = paste0(paste0(paste0(rep(' ',i),collapse=""),'<li><button class="button" id="but',
        jk, '" onclick="changeUrl(',jk,')">',fol2,'</button>'))
        if(nov>1){
          for(ijk in 2:nov){
            news=paste0(news,'<button class="button" id="but',jk+ijk-1,'" onclick="changeUrl(',
            jk+ijk-1,
          ')">V',ijk,'</button>',collapse="")
          }
        }
        news=paste0(news,'</li>\n',collapse="")
        mys = paste0(mys,news,collapse="")
        lasti = i
      }
      folders=c(folders,fol)
    }
  }
}
news = paste0(rep("</ul></li>\n",lasti-1),collapse="")
mys = paste0(mys,news,'</ul>',collapse="")
qs = character(0)
as = character(0)
for(i in 1:length(s)){
  for(j in 1:nov){
    qs = c(qs,paste0(s[i],j,".html",collapse=""))
    as = c(as,paste0(s[i],"_sol",j,".html",collapse=""))
  }
}
page = sprintf(html2,
               paste0(qs,collapse='","'),
               paste0(as,collapse='","'),
               nov,nov,nov,nov,nov,nov,nov,nov,
               mys)
page = paste0(html1,page,collapse="\n")
write.table(page,paste0(outdir,"/",fn,".html",collapse=""),quote=F,col.names=F,qmethod="d",row.names=F)


seed = 0
for(prob in s){
  bits = unlist(strsplit(prob,"/"))
  n = length(bits)
  dir = paste0(bits[1:(n-1)],collapse="/")
  myexam <- c(paste0(prob,".Rmd",sep=""))
  mydir = paste0(outdir,"/",dir,collapse="")
  dir.create(mydir,recursive=T,showWarnings=F)
  qname = paste0(mydir,"/",bits[n],1,".html",collapse="")
  if(!file.exists(qname)){
    exams2html_mod(myexam,n=nov,seed=seed:(seed+nov-1),name=bits[n],template="plain8B.html",mathjax=T,
               dir = mydir,solution=F,converter = "pandoc-mathjax",question=prob
    )
    exams2html_mod(myexam,n=nov,seed=seed:(seed+nov-1),name=paste0(bits[n],"_sol"),template="plain8B.html",mathjax=T,
               dir = mydir,question=F,converter = "pandoc-mathjax",solution=paste0("solution_",prob,collapse="")
    )
    for(i in 1:nov){
      qname = paste0(mydir,"/",bits[n],i,".html",collapse="")
      print(qname)
      myf = paste(readLines(qname),collapse="\n")
      rep1 = sprintf("<ol>\n<li>\n%s/%s",dir,bits[n])
      rep2 = sprintf("<small><i>%s/%s_v%s</i></small>",dir,bits[n],i)
      myf = sub(rep1,rep2,myf)
      rep1 = "</li>\n</ol>\n\n</body>"
      rep2 = "\n\n</body>"
      myf = sub(rep1,rep2,myf)
      write.table(myf,qname,row.names=F,col.names=F,qmethod="d",quote=F)
      qname = paste0(mydir,"/",bits[n],"_sol",i,".html",collapse="")
      myf = paste(readLines(qname),collapse="\n")
      rep1 = sprintf("<ol>\n<li>\nsolution_%s/%s",dir,bits[n])
      rep2 = sprintf("<small><i>solution_%s/%s_v%s</i></small>",dir,bits[n],i)
      myf = sub(rep1,rep2,myf)
      rep1 = "</li>\n</ol>\n\n</body>"
      rep2 = "\n\n</body>"
      myf = sub(rep1,rep2,myf)
      write.table(myf,qname,row.names=F,col.names=F,qmethod="d",quote=F)
      # mystyle = "body{font-size: 5vh;}"
      # write.table(mystyle,paste0(mydir,"/mystyle.css"),row.names=F,col.names=F,qmethod="d",quote=F)
    }
  }
  seed = seed+nov
}

