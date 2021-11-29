#Script BeaufEffortByPolyInsxnLFcns.r...Megan C. Ferguson...27 March 2019

  library(sp)
  library(maptools)
  library(rgeos)

  #Compute effort by month-within-year for each survey block and depth stratum
    #Function to clip transect segments to polygons and summarize effort within
    #the polygons
      debugg <- FALSE
      #debugg <- TRUE
      if(debugg == TRUE){
        yrdat <- alldat[which(alldat$Yr == 1982),]
        modat <- yrdat[yrdat$Month == 9,]
        Prj <- ECproj
        ttrax <- xtract.trax.SL(modat, Prj)
        SP.list <- bwasp.SPlist
        #SP.list <- bwasp.z.SPlist
        fnam <- "Test"
      }
      #
      beauf.effort.by.poly <- function(ttrax, tx.type, Prj, SP.list, fnam, debugg=FALSE){
        #Divide transect effort into polygons defined by SP.list
          insxn <- lapply(1:length(SP.list), function(p){
            #p <- 1
            if(tx.type == 3 | tx.type == 2){ #allow tx.type == 2 for "all effort" flightlines
              p.insxn <- lapply(1:length(ttrax[[tx.type]]@lines[[1]]@Lines), function(i, P=p){
                #i <- 2
                p.i <- paste(P, i, sep="_")
                SL.i <- SpatialLines(list(Lines(list(ttrax[[tx.type]]@lines[[1]]@Lines[[i]]), ID=i)), proj=Prj)
                p.i.insxn <- gIntersection(SL.i, SP.list[[P]], id=p.i)
                return(p.i.insxn)
                if(debugg == TRUE){
                  plot(SP.list[[p]])
                  plot(SL.i, add=TRUE)
                  dev.off()
                  #
                  plot(SL.i)
                  plot(SP.list[[p]], col="green", add=TRUE)
                  plot(SL.i, add=TRUE)
                  dev.off()
                }
              })
            } else if (tx.type == 6){
              p.insxn <- lapply(1:length(ttrax[[tx.type]]@lines), function(i, P=p){
                #i <- 2
                p.i <- paste(P, i, sep="_")
                SL.i <- SpatialLines(list(ttrax[[tx.type]]@lines[[i]]), proj=Prj)
                p.i.insxn <- gIntersection(SL.i, SP.list[[P]], id=p.i)
                return(p.i.insxn)
                if(debugg == TRUE){
                  plot(SP.list[[p]])
                  plot(SL.i, add=TRUE)
                  dev.off()
                  #
                  plot(SL.i)
                  plot(SP.list[[p]], col="green", add=TRUE)
                  plot(SL.i, add=TRUE)
                  dev.off()
                }
              })
            }  
            return(p.insxn)
          })    
          save(insxn, file=fnam)
          return(insxn)
        }
      

    #Function to compute length of transect effort for each transect/polygon in
    #eff.smry
      insxn.L <- function(insxn, fnam){
        L <- sapply(1:length(insxn), function(p){
        #L <- sapply(6:6, function(p){
            L.p <- sapply(1:length(insxn[[p]]), function(i, P=p){
            #L.p <- sapply(1493:1493, function(i, P=p){
              if(is.null(insxn[[p]][[i]]) == FALSE){
                if(class(insxn[[p]][[i]]) == "SpatialLines"){
                  L.p.i <- LinesLength(insxn[[p]][[i]]@lines[[1]], longlat=FALSE)/1000
                } else {
                  L.p.i <- 0
                }
              } else {
                L.p.i <- 0
              }
              return(L.p.i)
            })
        })
        save(L, file=fnam)
        return(L)
      }

  save(insxn.L, beauf.effort.by.poly, file="BeaufEffortByPolyInsxnLFcns.Rdata")