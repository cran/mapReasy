plotonMAP <-
function(shapedir= "WorkingDir", shapename,boundary.label,all.label=FALSE,lab.cex=0.8,plot.type="bar",plot.data,plot.col,plot.label.col="blue",pie.radius = 0.8,plot.size=c(0.5,0.5)){
       if(is.character(shapename)==FALSE | length(shapename)>1){
               stop("shapename must be a character input of length 1")
         }
       if(shapedir == "WorkingDir"){
            ogrListLayers(paste(shapename,".shp",sep=""))
            shape <- readOGR(paste(shapename,".shp",sep=""), layer=shapename)
       }else{
            ogrListLayers(shapedir)
            shape <- readOGR(dsn = shapedir, layer=shapename)
        }
            plregion <- match(as.character(plot.data[,1]),as.character(shape[[boundary.label]]))
            tmp.mat <- t(as.matrix(plot.data[,-1]))
            rownames(tmp.mat) <- NULL
            plot(shape)

if(plot.type=="pie"){
   for(i in 1:ncol(tmp.mat)){
       plcoor <- getSpPPolygonsLabptSlots(shape)[plregion[i],]
             subplot(pie(tmp.mat[,i],labels=NA,radius=pie.radius,main=" ",col=plot.col,ann=F),
             plcoor[1],plcoor[2], size=plot.size, vadj=0, hadj=.5, pars=NULL)
}
}else{
    for(i in 1:ncol(tmp.mat)){
       plcoor <- getSpPPolygonsLabptSlots(shape)[plregion[i],]
             subplot(barplot(tmp.mat[,i],main=" ",xlab=" ", ylab=" ",col=plot.col,axes=F,ann=F),
             plcoor[1],plcoor[2], size=plot.size, vadj=0, hadj=.5, pars=NULL)
}
}
invisible(text(getSpPPolygonsLabptSlots(shape)[plregion,],labels=as.character(plot.data[,1]), cex=lab.cex,pos=1,col=plot.label.col))
if(all.label==TRUE){
  invisible(text(getSpPPolygonsLabptSlots(shape)[-plregion,], labels=as.character(shape[[boundary.label]])[-plregion],cex=lab.cex))
       }
}
