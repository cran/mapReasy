info.shape <-
function(shapedir="WorkingDir",shapename){
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
elem <- list(NULL)
for(i in 1:length(names(shape))){
if(class(shape[[i]])=="factor"){
elem[[i]] <- list(shape[[i]])  
names(elem)[i] <- names(shape)[i] 
 }
}
return(elem)
}
