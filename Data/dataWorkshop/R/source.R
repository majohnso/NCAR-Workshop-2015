URLdataWorkshopDownload<- "http://www.image.ucar.edu/~nychka/Temp/dataDownload/"
dataRemote<- function( dname){
  dname<-c(as.character(substitute(dname)))
  linkAddress<- paste0(URLdataWorkshopDownload,dname,".rda")
  webConnection<- url(linkAddress)
  load(webConnection, envir=sys.frame(which=0))
}
