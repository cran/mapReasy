map.country <-
function(shapedir = "WorkingDir", shapename,boundary.label=NULL,spe.vector=NULL,cex=0.8,col="lightblue"){
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
       if(!is.null(spe.vector)){
               if(is.null(boundary.label)){ stop("boundary.label must specify") }
          if(length(col)==1){
             members <- rep(0,nrow(shape))
             members[match(spe.vector,shape[[boundary.label]])]=1
             mycol <- ifelse(members==1,col,"white")
             plot(shape,col=mycol)
             invisible(text(getSpPPolygonsLabptSlots(shape),labels=as.character(shape[[boundary.label]]), cex=cex))
          }else{
             mycol <- rep("white",nrow(shape))
             mycol[match(spe.vector,shape[[boundary.label]])] <- col
             plot(shape,col=mycol)
             invisible(text(getSpPPolygonsLabptSlots(shape),labels=as.character(shape[[boundary.label]]), cex=cex))
             }
       }else{
            if(length(col)>1) {stop("spe.vector must specify for different color levels") }
            plot(shape)
            if(!is.null(boundary.label)){
            invisible(text(getSpPPolygonsLabptSlots(shape),labels=as.character(shape[[boundary.label]]), cex=cex))
                }
  }}
