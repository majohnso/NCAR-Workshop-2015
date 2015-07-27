ggpoly.image<-function(x, y, z, midpoint=FALSE, col=tim.colors(256), legend=NULL, limits=NULL){ 
  require(fields)
  require(ggplot2)
  ## Snipe code from poly.image in fields package (thanks Doug and Steve) ##
  Dx <- dim(x)
  Dy <- dim(y)
  if (any((Dx - Dy) != 0)) {
    stop(" x and y matrices should have same dimensions")
  }
  Dz <- dim(z)
  if (all((Dx - Dz) == 0) & !midpoint) {
    x <- poly.image.regrid(x)
    y <- poly.image.regrid(y)
  }
  
  N <- ncol(x)
  Nm1 <- N - 1
  M <- nrow(x)
  Mm1 <- M - 1
  
  ## Instead of plotting in for loop, need to save to vectors ## 
  # Initialize vectors
  xp<-c()
  yp<-c()
  zp<-c()
  group<-c()
  
  ## poly.image code to make polygons ##
  for (i in (1:Mm1)) {
    x1 <- cbind(x[i, 1:Nm1], x[i + 1, 1:Nm1], x[i + 1, 2:N],
                x[i, 2:N], rep(NA, Nm1)) # Upper Left, Lower left, Lower Right, Upper Right, ?
    y1 <- cbind(y[i, 1:Nm1], y[i + 1, 1:Nm1], y[i + 1, 2:N],
                y[i, 2:N], rep(NA, Nm1))
  ## Each polygon is a "group" for geom_polygon
    group1<-rep(((i-1)*Nm1+1):(i*Nm1),each=5) 
    
    x1 <- c(t(x1))
    y1 <- c(t(y1))
    
    xp<-c(xp,x1)
    yp<-c(yp,y1)
    group<-c(group, group1)
    
    pcol <- c(z[i, 1:Nm1])
    z1<-rep(pcol,each=5)
    zp<-c(zp,z1)    
  }
  
  ## Save to dataframe ##
  df<-data.frame(group,xp,yp,zp)

  ## Basic plotting, return(p) instead of print(p) allows the addition of aesthetics  
  p <- ggplot(df, aes(x=xp, y=yp))+
    scale_color_gradientn(colours=col,name=legend, na.value="white", limits=limits)+
    scale_fill_gradientn(colours=col,name=legend, na.value="white", limits=limits)  +
    geom_polygon(aes(fill=zp,color=zp,group=group))
  return(p)
}

ggpoly.image_NAs<-function(x, y, z, midpoint=FALSE, col=tim.colors(256), legend=NULL, limits=NULL, x_lim=range(x), y_lim=range(y)){ 
  require(fields)
  require(ggplot2)
  ## Snipe code from poly.image in fields package (thanks Doug and Steve) ##
  Dx <- dim(x)
  Dy <- dim(y)
#   if(sum(!is.na(z))==0) {
#     stop(" No non-missing values in z")
#   }
  if (any((Dx - Dy) != 0)) {
    stop(" x and y matrices should have same dimensions")
  }
  Dz <- dim(z)
  if (all((Dx - Dz) == 0) & !midpoint) {
    x <- poly.image.regrid(x)
    y <- poly.image.regrid(y)
  }
  
  N <- ncol(x)
  Nm1 <- N - 1
  M <- nrow(x)
  Mm1 <- M - 1
  
  ## Instead of plotting in for loop, need to save to vectors ## 
  # Initialize vectors
  xp<-c()
  yp<-c()
  zp<-c()
  group<-c()
  
  ## poly.image code to make polygons ##
  for (i in (1:Mm1)) {
    ind<-which(!is.na(z[i,1:Nm1]))
    if(length(ind)!=0){
      x1 <- cbind(x[i, ind], x[i + 1, ind], x[i + 1, ind+1],
                  x[i, ind+1], rep(NA, length(ind))) # Upper Left, Lower left, Lower Right, Upper Right, ?
      y1 <- cbind(y[i, ind], y[i + 1, ind], y[i + 1, ind+1],
                  y[i, ind+1], rep(NA, length(ind)))
                                 ## Each polygon is a "group" for geom_polygon
      group1<-rep(i*ind*Dx[2]+i*1000000,each=5) 
                                 
      x1 <- c(t(x1))
      y1 <- c(t(y1))
                                 
      xp<-c(xp,x1)
      yp<-c(yp,y1)
      group<-c(group, group1)
                                 
      pcol <- c(z[i, ind])
      z1<-rep(pcol,each=5)
      zp<-c(zp,z1)    
    }
  }
  
  ## Save to dataframe ##
  df2<-data.frame(group,xp,yp,zp)
  
  ## Basic plotting, return(p) instead of print(p) allows the addition of aesthetics 
  if(length(df2)==0){
    p<-ggplot() + scale_color_gradientn(colours=col,name=legend, na.value="white", limits=limits) +
      scale_fill_gradientn(colours=col,name=legend, na.value="white", limits=limits)  
  }else{
    p <- ggplot(df2, aes(x=xp, y=yp))+
      geom_polygon(aes(fill=zp, colour=zp, group=group)) + xlim(x_lim) + ylim(y_lim) +
      scale_color_gradientn(colours=col,name=legend, na.value="white", limits=limits) +
      scale_fill_gradientn(colours=col,name=legend, na.value="white", limits=limits)  
  }
  return(p)
}

