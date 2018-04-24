findwords <- function(tf) {
  # read in the words from the file, into a vector of mode character
  txt <- scan(tf, " ")
  wl <- list()
  for (i in 1:length(txt)) {
    wrd <- txt[i]
    wl[[wrd]] <- c(wl[[wrd]], i)
  }
  return(wl)
}

wl <- findwords("data/testconcord.txt")

alphawl <- function(wrdlst) {
  nms <- names(wrdlst)
  sn <- sort(nms)
  return(wrdlst[sn])
}

alphawl(wl)

makecorp <- function(corpname) {
  t <- all2006[all2006$Employer_Name == corpname]
  return(t)
}

corplist <- c("MS CORPORATION", "ms", "INTEL CORPORATION", "SUN MICROSYSTEMS, INC.", "sun", "GOOGLE INC.", "google")

for (i in 1:(length(corplist)/2)) {
  corp <- corplist[2*i-1]
  newdt <- paste(corplist[2*i], "2006", sep = " ")
  assign()
}