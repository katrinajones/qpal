#' Model 2 dimensional shape from PC scores
#'
#' @param PCscores scores
#' @param dim dimensions
#' @param eigenvectors eigenvectors
#' @param consensus consensus
#'
#' @return shape plot
#' @export
#'
#'
model.space <- function(PCscores,dim, eigenvectors, consensus) {
   model <- matrix(PCscores[1]*eigenvectors[,dim[1]]+PCscores[2]*eigenvectors[,dim[2]]+as.vector(t(consensus)),
                    nrow=9,ncol=2,byrow=T)
  geomorph::plotRefToTarget(consensus,model,sub=paste("PC1=",PC1," PC2=",PC2,""))
}

#'Subset geomorph data frames
#'
#'Allows subsetting of geomorph data frames (and all variables therein) either by specimen or by landmark
#'
#'The specified values may be either kept or removed from the final object
#'
#' @param gm.data.frame geomorph data frame
#' @param dim land or spec
#' @param value vector with which items to remove
#' @param keep T or F - keep or remove values
#'
#' @return subsampled geomorph data frame
#' @export
#'

  subsetgeom<-function(gm.data.frame, dim, value, keep){
    if(keep==T){
      scalefact<-1
    }  else{
      scalefact<--1}
    if(dim=="land"){
      for(i in 1:length(gm.data.frame)){
        item<-gm.data.frame[[i]]
        if((length(dim(item))==3)){
          gm.data.frame[[i]]<-item[scalefact*value,,]
        }
      }
    }
    if(dim=="spec"){
      for(i in 1:length(gm.data.frame)){
        item<-gm.data.frame[[i]]
        if((length(dim(item))==3)){##lanmdmarks
          gm.data.frame[[i]]<-item[,,scalefact*value]
        }
        if((length(dim(item))==2)){##pc scores
          gm.data.frame[[i]]<-item[scalefact*value,]
        }
        if(is.vector(item)==T){#covariate
          gm.data.frame[[i]]<-item[scalefact*value]
        }
        if(is.factor(item)==T){#variable
          gm.data.frame[[i]]<-item[scalefact*value]
          gm.data.frame[[i]]<-droplevels(gm.data.frame[[i]])
        }
      }

    }
    return(gm.data.frame)
  }

#' Create a sliders file
#'
#' Make a sliders file from a character vector indicating which landmarks are curves
#'
#' @param curves Vector indicating curves
#' @param id Curve names
#' @param begin First static landmark number
#' @param end Last static landmark number
#'
#' @return Sliders object
#' @export
#'
#'
  makesliders<-function(curves, id, begin=NULL, end=NULL){

    x=NULL

    for(i in 1:length(id)){

      idi<-id[i]#which curve

      xall<-which(curves==idi)
      xall<-c(begin[i],xall,end[i])
      x1<-xall[1:(length(xall)-2)]
      x2<-xall[2:(length(xall)-1)]
      x3<-xall[3:length(xall)]
      xi<-cbind(x1,x2,x3)
      x<-rbind(x,xi)
    }

    return(x)

  }
