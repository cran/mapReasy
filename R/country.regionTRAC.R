country.regionTRAC <-
function(shapedir= "WorkingDir",shapename,boundary.label,spc.level,long.vector=NULL,lat.vector=NULL,cex=0.8,point.col="red",point.pch=19,point.cex=0.5,point.trc=FALSE){
             if(is.character(shapename)==FALSE | length(shapename)>1){
               stop("shapename must be a character input of length 1")
            }
            if(length(long.vector)!=length(lat.vector)) {
             stop("Latitude and longitude must have same length") }        
       if(shapedir == "WorkingDir"){
            ogrListLayers(paste(shapename,".shp",sep=""))
            shape <- readOGR(paste(shapename,".shp",sep=""), layer=shapename)
       }else{
            ogrListLayers(shapedir)
            shape <- readOGR(dsn = shapedir, layer=shapename)
        }
            newshape <- shape[shape[[boundary.label]] %in% spc.level,]
            plot(newshape)
            invisible(text(getSpPPolygonsLabptSlots(newshape), labels=as.character(newshape[[boundary.label]]),cex=cex))
            if(!is.null(long.vector) & !is.null(lat.vector)){            
                    points(long.vector,lat.vector,col=point.col, pch=point.pch, cex=point.cex)  }
            if(!is.null(long.vector) & !is.null(lat.vector) & point.trc==TRUE){                         
             text(long.vector,lat.vector,labels=paste(long.vector,lat.vector,sep=","))
                             }
}

