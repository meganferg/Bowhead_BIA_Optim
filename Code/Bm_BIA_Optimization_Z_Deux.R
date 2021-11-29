#Script Bm_BIA_Optimization_Z_Deux.r...Megan C. Ferguson...3 August 2020

# Notes
#
#  0. Cluster enumeration algorithm is developed for a test case in script
#     Ferguson_Cluster_Enumeration_Alg_Deux.r.
#  1. Required input files:
#       a. gshhs_i.b
#       b. BWASPblocks_wblk1barriers_ManyPts
#       c. Bm_FeedMill_Rate_ByMo_0019_pooled (created by CamdenBayBehavior.r)
#       d. Bm_Calf_Rate_ByMo_1219_pooled (created by CamdenBayBehavior.r)
#  2. Geographic projection used is:
#    ECBproj6 <- CRS('+proj=eqdc +lat_1=69.9d
#                              +lat_2=71.6d
#                              +lat_0=70.75d
#                              +lon_0=-148.0d
#                              +x_0=0
#                              +y_0=0')
#  3. Hex grid is defined exactly as in CamdenBayBehavior.r, with 25-km resolution
#     and same random start point
#  4. The LP formulation is based on Toth et al. (2009):
#    Tóth, Sandor F., Robert G. Haight, Stephanie A. Snyder, Sonney George, James R. Miller, 
#    Mark S. Gregory, Adam M. Skibbe. 2009. Reserve selection with minimum contiguous area 
#    restrictions: An application to open space protection planning in suburban Chicago.
#    Biological Conservation 142(8): 1617-1627. https://doi.org/10.1016/j.biocon.2009.02.037.
#  5. This script enumerates clusters, writes cplex code, and summarizes cplex output for
#   the following variables:
#     a. min contiguous clusters: {1, 2, 3, 4, 5}
#     b. max proportion of the occupied cells included in BIAs: 0.1 to 1.2 by 0.1
#     c. months: jul, aug, sep, oct
#     d. activity states: feed/mill, calves
#  6. Output_BIA_Z_Deux\:
#     a. cluster enumeration .csv files: ClusterSets
#     b. cplex code, logs, and solutions: CPLEX_LP
#     c. figures showing optimal BIA solutions: Figures
#     d. summaries of results for each combination of variables: OutLists

  library(sp)
  library(rgeos)
  library(maptools)
  library(rgdal)
  library(fields)
  library(stringr)
  
  #Define map projection for Beaufort Sea study area.  Use Equidistant Conic.
    ECBproj6 <- CRS('+proj=eqdc +lat_1=69.9d
                              +lat_2=71.6d
                              +lat_0=70.75d
                              +lon_0=-148.0d
                              +x_0=0
                              +y_0=0')
    
  #Import shoreline as SpatialPolygons object from GSHHS library and re-project
    shor.xlim <- c((360-169),(360-139.25))
    shor.ylim <- c(69.3,73)
    if (!rgeosStatus()) gpclibPermit()
    shor.SP <- Rgshhs("Data//gshhs_i.b",xlim=shor.xlim,ylim=shor.ylim, level=1)
    shor.SP.ec <- spTransform(shor.SP$SP, ECBproj6)
    
  #Recreate hex grid  
    
    #Import the shapefiles BWASPblocks_wblk1barriers_ManyPts (which has a polygon for inside
    #barrier islands in block 1). Re-project survey blocks, and create list object
    #comprised of individual blocks.
      bwasp13.LL.SP <- readOGR(dsn="Shapefiles", layer="BWASPblocks_wblk1barriers_ManyPts")
      proj4string(bwasp13.LL.SP) <- CRS('+proj=longlat') 
      bwasp13.SP <- spTransform(bwasp13.LL.SP, ECBproj6)
      bwasp.SP <- SpatialPolygons(list(Polygons(list(bwasp13.SP@polygons[[1]]@Polygons[[1]]), ID="1"),
                                       Polygons(list(bwasp13.SP@polygons[[2]]@Polygons[[1]]), ID="2"),
                                       Polygons(list(bwasp13.SP@polygons[[3]]@Polygons[[1]]), ID="3"),
                                       Polygons(list(bwasp13.SP@polygons[[4]]@Polygons[[1]]), ID="4"),
                                       Polygons(list(bwasp13.SP@polygons[[5]]@Polygons[[1]]), ID="5"),
                                       Polygons(list(bwasp13.SP@polygons[[6]]@Polygons[[1]]), ID="6"),
                                       Polygons(list(bwasp13.SP@polygons[[7]]@Polygons[[1]]), ID="7"),
                                       Polygons(list(bwasp13.SP@polygons[[8]]@Polygons[[1]]), ID="8"),
                                       Polygons(list(bwasp13.SP@polygons[[9]]@Polygons[[1]]), ID="9"),
                                       Polygons(list(bwasp13.SP@polygons[[10]]@Polygons[[1]]), ID="10"),
                                       Polygons(list(bwasp13.SP@polygons[[11]]@Polygons[[1]]), ID="11"),
                                       Polygons(list(bwasp13.SP@polygons[[12]]@Polygons[[1]]), ID="12"),
                                       Polygons(list(bwasp13.SP@polygons[[14]]@Polygons[[1]]), ID="1b")), 
                                  proj4string=ECBproj6)
  
    #Create hex grid 
      
      #Create aces.hex grid with 5-km cells, using buffered study area polygon  
       buff.SP <- gBuffer(bwasp.SP, width = 25000)
       #Ck
         plot(buff.SP, col="black")
         plot(bwasp.SP, col="cyan", add=TRUE)
         dev.off()
         
         #Create buff.hex grid with 5-km cells
           set.seed(-1865068913) #To get the same grid every time
           buff.hexPts <- spsample(buff.SP, type="hexagonal", cellsize=25000)
           buff.hexPts.LL <- spTransform(buff.hexPts, CRS('+proj=longlat'))
           buff.hexPols <- HexPoints2SpatialPolygons(buff.hexPts)
           
           plot(bwasp.SP, col="cyan")
           plot(buff.hexPols, add=TRUE)
           plot(buff.hexPts, pch=19, cex=0.1, add=TRUE)
           dev.off()
           
           plot(bwasp.SP, col="cyan")
           plot(buff.hexPts, pch=19, cex=0.1, add=TRUE)
           dev.off()
           
           plot(bwasp.SP, col="cyan")
           plot(buff.hexPols, pch=19, cex=0.1, add=TRUE)
           dev.off()
           
        #Create a list 
          hex.SPlist <- lapply(1:length(buff.hexPols), function(i){
            p.i <- SpatialPolygons(list(buff.hexPols@polygons[[i]]), proj4string=ECBproj6)
            return(p.i)
          })
          n.hex <- length(hex.SPlist)
          #CK
            n.hex #284
            class(hex.SPlist[[1]])
            
  #Omit hex cells that will not be included in the analysis because they have no feed/mill
  #or calf sightings for any period and they are not within a tight boundary of occupied
  #cells. 
    
    omit.hex <- sort(c(18, 52, 72, 118, 142, 157:168, 177:194, 205:220, 230:246, 253:271, 
                  272:283, 143:145, 119:120, 95:98, 73:75, 53:54, 33:39, 19:22, 6:13, 
                  0:5) + 1) #FID numbers from the shapefiles, but FID numbering begins 
                           #with 0 whereas the r spatial objects begin indexing with 1
    in.out.df <- data.frame(rep(0,n.hex))
    in.out.df[-omit.hex,1] <- 1
    
    in.out.spdf <- SpatialPolygonsDataFrame(buff.hexPols, data=in.out.df, match.ID=FALSE)
            writeOGR(in.out.spdf, "Output_BIA_Z_Deux//Shapefiles","in_out_spdf",
                               driver="ESRI Shapefile", overwrite_layer = TRUE)
            
    n.hex.in <- sum(in.out.df[,1]) #146
    
    C1 <- data.frame("i.1"=which(in.out.df[,1] == 1))
    hex.in.SPlist <- hex.SPlist[C1$i.1]
    buff.hexPols.in <- buff.hexPols[C1$i.1,]
    #Ck
      n.hex.in + length(omit.hex) #should equal n.hex
      n.hex
      
      plot.now <- FALSE
      #plot.now <- TRUE
      if(plot.now == TRUE){
          plot(buff.hexPols)
          for(i in 1:n.hex){
            if(in.out.df[i,1] == 1) plot(hex.SPlist[[i]], col="purple", add=TRUE)
          }
          
          plot(buff.hexPols)
          for(i in 1:n.hex.in){
            plot(hex.in.SPlist[[i]], col="red", add=TRUE)
          }
      }  

  #Determine which cells in hexgrid share a common boundary, and the length of that CB
    
    #Figure out how gIntersection works        
      plot(buff.hexPols)
      plot(hex.SPlist[[1]], col="purple", add=TRUE)
      plot(hex.SPlist[[2]], col="blue", add=TRUE)
      plot(hex.SPlist[[3]], col="cyan", add=TRUE)
      
      test <- gIntersection(hex.SPlist[[2]], hex.SPlist[[3]])     
      class(test)
      is.null(test)
      test.len <- SpatialLinesLengths(test)
      test.len
      plot(test, col="magenta", lwd=3, add=TRUE)
      
      test <- gIntersection(hex.SPlist[[1]], hex.SPlist[[2]])     
      class(test)
      is.null(test)  
      test.len <- try(SpatialLinesLengths(test), silent=TRUE)
      test.len
      
    #Compute number of pairwise comparisons needed in hex.SPlist
      n.pr <- choose(n.hex.in, 2)
      n.pr #10585
      
      C2 <- cbind.data.frame("i.1"=rep(NA, n.pr),
                                "i.2"=rep(NA, n.pr),
                                "cb"=rep(NA, n.pr))

      idx <- 0
      for(i in 1:n.hex.in){
        for(j in 1:n.hex.in){
          
          if(i != j){
            
            idx <- idx + 1
            
            x <- gIntersection(hex.in.SPlist[[i]], hex.in.SPlist[[j]])     
            if(is.null(x) == FALSE & class(x) == "SpatialLines"){
              x.len <- SpatialLinesLengths(x)
            } else {
              x.len <- 0
            }
            
            C2[idx,1] <- C1$i.1[i]
            C2[idx,2] <- C1$i.1[j]
            C2[idx,3] <- x.len
          }  

        }  
      }
      idx <- which(C2$cb > 0)
      C2 <- C2[idx,]
      write.csv(C2, file="Output_BIA_Z_Deux//ClusterSets//C2_dup.csv") #output neighbors;
                                                                #each neighbor will be represented 2x
      
      #Create a set of 2-cell clusters that represent each pair of neighbors only once. Do this
      #by first sorting rows of C2 in increasing order so that i.1 < i.2, then extract unique
      #rows.
        C2.unq <- sapply(1:nrow(C2), function(r){
          r.sort <- sort(c(C2[r,1], C2[r,2]))
          return(r.sort)
        })
        dim(C2.unq)
        class(C2.unq)
        C2.unq <- as.data.frame(t(C2.unq))
        C2.unq <- unique(C2.unq)
        names(C2.unq) <- c("i.1","i.2")
        write.csv(C2.unq, file="Output_BIA_Z_Deux//ClusterSets//C2.csv") #output unique clusters
      #Ck
        summary(C2)
        nrow(C2) #total number of common boundaries = 662 (2 for each cb)
        
        nrow(C2.unq) #331
        class(C2.unq) #df
        summary(C2.unq)

    #Extract area of each cell. Note that area is constant across all cells:
    #541265877 m^2 = 541.2659 km^2
    
      hex.a <- sapply(1:n.hex.in, function(i){
        a.i <- hex.in.SPlist[[i]]@polygons[[1]]@area/(1000*1000) #km^2
        return(a.i)
      })
      C1 <- cbind.data.frame(C1, "a"=hex.a)
      write.csv(C1, file="Output_BIA_Z_Deux//ClusterSets//C1.csv") 
      #Ck
        dim(C1)
        summary(C1)
    
  #Define sets of clusters for minimum contiguity thresholds of 1:5 cells. See
  #Ferguson_Cluster_Enumeration_Alg_Deux.r.
  #  Notes
  #    1. C1 is the set of single cells. C2 is the set of all 2-cell clusters. 
  #       ...C5 is the set of all 5-cell clusters.
  #    2. Each C# set contains clusters that are unique combinations of cells.
        
    #1-cell clusters and areas are in C1
      
    #2-cell clusters and common boundaries are in C2
        
    #3-cell clusters
      
      #Enumerate all 3-cell clusters
      
        C3 <- cbind.data.frame("i.1"=-1, "i.2"=-1, "i.3"=-1)
        #Ck
          dim(C3)
          class(C3)
          C3
          
        for(r in 1:nrow(C2)){ 

          #Identify all neighbors to i.2[r] 
            
            idx <- which(C2$i.1 == C2$i.2[r])
            i.3 <- C2$i.2[idx] 
            
          #Cycle through each value of i.3 and extract clusters comprised of 3 unique 
          #indices. At the end of each cycle, append new combinations of indices 
          #{i.1, i.2, i.3} (i.e., new clusters) to C3.
            
            for(idx in 1:length(i.3)){
              
              test <- sort(c(C2$i.1[r], C2$i.2[r], i.3[idx]))
              if(length(unique(test)) == 3){ #3 unique indices comprise a valid cluster
                new.C3 <- cbind.data.frame("i.1"=test[1], "i.2"=test[2], "i.3"=test[3])
                C3 <- rbind.data.frame(C3, new.C3)
              }
                    
            }   
  
        }  
        
        C3 <- C3[-1,] #Omit the initial dummy row of -1s 
        nrow(C3) #2532
        write.csv(C3, file="Output_BIA_Z_Deux//ClusterSets//C3_with_dups.csv")
        
        C3 <- unique(C3) 
        nrow(C3) #892
        write.csv(C3, file="Output_BIA_Z_Deux//ClusterSets//C3.csv")

    #4-cell clusters

      #Enumerate all 4-cell clusters, using C3 as the base tree to branch from
      
        C4 <- cbind.data.frame("i.1"=-1, "i.2"=-1, "i.3"=-1, "i.4"=-1)
        #Ck
          dim(C4)
          class(C4)
          summary(C4)
          
        for(r in 1:nrow(C3)){ 

          #Identify all neighbors to C3$i.3[r] 
            
            idx <- which(C2$i.1 == C3$i.3[r])
            i.4 <- C2$i.2[idx] 
            
          #Cycle through each value of i.4 and extract clusters comprised of 4 unique 
          #indices. At the end of each cycle, append new combinations of indices 
          #{i.1, i.2, i.3, i.4} (i.e., new clusters) to C4.
            
            for(idx in 1:length(i.4)){
              
              test <- sort(c(C3$i.1[r], C3$i.2[r], C3$i.3[r], i.4[idx]))
              if(length(unique(test)) == 4){ #4 unique indices comprise a valid cluster
                new.C4 <- cbind.data.frame("i.1"=test[1], "i.2"=test[2], 
                                           "i.3"=test[3], "i.4"=test[4])
                C4 <- rbind.data.frame(C4, new.C4)
              }
                    
            }   
        }
        
        C4 <- C4[-1,] #Omit the initial dummy row of -1s
        nrow(C4) #3205
        write.csv(C4, file="Output_BIA_Z_Deux//ClusterSets//C4_with_dups.csv")
        
        C4 <- unique(C4)
        nrow(C4) #2269
        write.csv(C4, file="Output_BIA_Z_Deux//ClusterSets//C4.csv")
        #CK
          nrow(C4)
          #Check for repeat indices within clusters
            test <- sapply(1:nrow(C4), function(i){
              x <- length(unique(C4[i,]))
            })
            summary(test) #Should all be length 4

    #5-cell clusters

      #Enumerate all 5-cell clusters, using C4 as the base tree to branch from
      
        C5 <- cbind.data.frame("i.1"=-1, "i.2"=-1, "i.3"=-1, "i.4"=-1, "i.5"=-1)
        #Ck
          dim(C5)
          class(C5)
          summary(C5)
          
        for(r in 1:nrow(C4)){ 

          #Identify all remaining neighbors to C4$i.4[r] 
            
            idx <- which(C2$i.1 == C4$i.4[r])
            i.5 <- C2$i.2[idx] 
            
            #Cycle through each value of i.5 and extract clusters comprised of 5 unique 
            #indices. At the end of each cycle, append new combinations of indices 
            #{i.1, i.2, i.3, i.4, i.5} (i.e., new clusters) to C5.
              
              for(idx in 1:length(i.5)){
                
                test <- sort(c(C4$i.1[r], C4$i.2[r], C4$i.3[r], C4$i.4[r], i.5[idx]))
                if(length(unique(test)) == 5){ #5 unique indices comprise a valid cluster
                  new.C5 <- cbind.data.frame("i.1"=test[1], "i.2"=test[2], 
                                             "i.3"=test[3], "i.4"=test[4], "i.5"=test[5])
                  C5 <- rbind.data.frame(C5, new.C5)
                }
                      
              }   
        }
        
        C5 <- C5[-1,] #Omit the initial dummy row of -1s 
        nrow(C5) #7639
        write.csv(C5, file="Output_BIA_Z_Deux//ClusterSets//C5_with_dups.csv")
        
        C5 <- unique(C5)
        nrow(C5) #5308
        write.csv(C5, file="Output_BIA_Z_Deux//ClusterSets//C5.csv")
        #CK
          nrow(C5)
          #Check for repeat indices within clusters
            test <- sapply(1:nrow(C5), function(i){
              x <- length(unique(C5[i,]))
            })
            summary(test) #Should all be length 5

          #Check for repeat clusters within C5
            nrow(unique(C5))
            nrow(C5) #should equal number of unique rows
            
            nrow(C1) #146
            nrow(C2.unq) #331
            nrow(C2) #662
            nrow(C3) #892
            nrow(C4) #2269
            nrow(C5) #5308
            
            #Note that indices in C# sets refer to indices in hex.SPlist object, NOT
            #hex.in.SPlist
            plot(buff.hexPols)
            k <- 243
              plot(hex.SPlist[[C2$i.1[k]]], col="orange", add=TRUE)
              plot(hex.SPlist[[C2$i.2[k]]], col="orange", add=TRUE)
              plot(buff.hexPols.in, lwd=3, add=TRUE)
              
            plot(buff.hexPols)
            k <- 243 #likely a different pair than C2[243,]
              plot(hex.SPlist[[C2.unq$i.1[k]]], col="magenta", add=TRUE)
              plot(hex.SPlist[[C2.unq$i.2[k]]], col="magenta", add=TRUE)
              plot(buff.hexPols.in, lwd=3, add=TRUE)

            plot(buff.hexPols)
            k <- 43
              plot(hex.SPlist[[C3$i.1[k]]], col="purple", add=TRUE)
              plot(hex.SPlist[[C3$i.2[k]]], col="purple", add=TRUE)
              plot(hex.SPlist[[C3$i.3[k]]], col="purple", add=TRUE)
              plot(buff.hexPols.in, lwd=3, add=TRUE)

            plot(buff.hexPols)
            k <- 1200
              plot(hex.SPlist[[C4$i.1[k]]], col="turquoise", add=TRUE)
              plot(hex.SPlist[[C4$i.2[k]]], col="turquoise", add=TRUE)
              plot(hex.SPlist[[C4$i.3[k]]], col="turquoise", add=TRUE)
              plot(hex.SPlist[[C4$i.4[k]]], col="turquoise", add=TRUE)
              plot(buff.hexPols.in, lwd=3, add=TRUE)
                            
            plot(buff.hexPols)
            k <- 5300
              plot(hex.SPlist[[C5$i.1[k]]], col="red", add=TRUE)
              plot(hex.SPlist[[C5$i.2[k]]], col="red", add=TRUE)
              plot(hex.SPlist[[C5$i.3[k]]], col="red", add=TRUE)
              plot(hex.SPlist[[C5$i.4[k]]], col="red", add=TRUE)
              plot(hex.SPlist[[C5$i.5[k]]], col="red", add=TRUE)
              plot(buff.hexPols.in, lwd=3, add=TRUE)
              
            dev.off()  
            
  #Create function to produce CPLEX formulations for LP
            
    #Define function for writing LP to file    
              
    debugg <- FALSE
    #debugg <- TRUE
    if(debugg == TRUE){
      c1 <- C1
      C.set <- C3
      Z <- 0.5
      mo <- 9
      FM.spdf <- readOGR(dsn="Output0019/Shapefiles", 
                           layer="Bm_FeedMill_Rate_ByMo_0019_pooled")
      d.i <- FM.spdf@data$september
      fpath <- "Output_BIA_Z_Deux/CPLEX_LP/"
      FM.calf <- "FM"
    }
    
    write.cpx.LP <- function(c1, C.set, Z, d.i, fpath, FM.calf, mo){
      
      #C.set should be a dataframe whose only columns are the i.# columns that provide
      #cell indices. 
        card.j <- ncol(C.set)
        
      #Create constraint matrix
        n.x.i <- nrow(c1) #number of x.i variables
        n.y.j <- nrow(C.set) #number of y.j variables
        
        nc <- n.x.i + n.y.j #number of variables
        nr <- 1 + n.x.i + 3*n.y.j #number of constraints
      
        c.mtx <- matrix(data=rep(0, nr*nc), nrow=nr)
        dim(c.mtx)
        colnames(c.mtx) <- c(paste("x",c1$i.1,sep="_"), #parcel indices, x.i
                             paste("y",1:nrow(C.set),sep="_")) #clusters, y.j
  
        rownames(c.mtx) <- c("Area",
                             paste("Constraint3",c1$i,sep="_"),
                             paste("Constraint4",1:nrow(C.set),sep="_"),
                             paste("Constraint5",1:nrow(C.set),sep="_"),
                             paste("Constraint6",1:nrow(C.set),sep="_"))
    
      #Coeffs for constraint 2: area constraint
        c.mtx[1,1:n.x.i] <- 1
              
      #Coeffs for constraint 3
        for(i in 1:n.x.i){
          
          c.mtx[(1+i),i] <- -1  #x.i
  
          c.test <- sapply(1:n.y.j, function(j){
            i.in.cluster <- match(c1$i.1[i], C.set[j,])
            return(i.in.cluster)
          })
          c.idx <- which(is.na(c.test)==FALSE)
          c.mtx[(1+i),(n.x.i+c.idx)] <- 1 #y.j
          
        }
        
      #Coeffs for constraint 4
  
        #Card.j, cardinality of each j, equals the number of cells per cluster

        for(j in 1:n.y.j){
          c.mtx[(n.x.i + 1 + j),(n.x.i+j)] <- -card.j   #y.j
          
          col.idx <- sapply(1:card.j, function(k){
            x <- which(c1$i.1 == C.set[j,k])
            return(x)
          })
          
          c.mtx[(n.x.i + 1 + j),col.idx] <- 1        #x.i
        }   
       
      #Coeffs for constraint 5
          
        for(j in 1:n.y.j){
          c.mtx[(n.x.i + 1 + n.y.j + j),(n.x.i + j)] <- -1  #y.j
          
          col.idx <- sapply(1:card.j, function(k){
            x <- which(c1$i.1 == C.set[j,k])
            return(x)
          })
          
          c.mtx[(n.x.i + 1 + n.y.j + j),col.idx] <- 1        #x.i
          
        } 
        
      #Coeffs for constraint 6
        
        #Define parameter delta       
          idx <- which(d.i > 0)
          delta <- min(d.i[idx])

        for(j in 1:n.y.j){
          c.mtx[(n.x.i + 1 + 2*n.y.j + j),(n.x.i + j)] <- -1  #y.j
          
          col.idx <- sapply(1:card.j, function(k){
            x <- which(c1$i.1 == C.set[j,k])
            return(x)
          })
          
          c.mtx[(n.x.i + 1 + 2*n.y.j + j),col.idx] <- (1/delta)*d.i[c1$i.1[col.idx]]  #x.i
          
        } 

      #RHS vector and sense 
        rhs <- rep("0",nrow(c.mtx))
        sense <- rep(NA,nrow(c.mtx))
        
        #constraint 2
          n.occupied <- length(which(d.i>0))
          rhs[1] <- as.character(n.occupied*Z) #proportion of occupied cells included in solution
          sense[1] <- "<="

        #constraint 3
          rhs[2:(1+n.x.i)] <- "0"
          sense[2:(1+n.x.i)] <- ">="
          
        #Constraint 4
          rhs[(n.x.i + 2):(1 + n.x.i + n.y.j)] <- "0"
          sense[(n.x.i + 2):(1 + n.x.i + n.y.j)] <- ">="
          
        #constraint 5
          rhs[(n.x.i + n.y.j + 2):(1 + n.x.i + 2*n.y.j)] <- as.character(card.j-1)
          sense[(n.x.i + n.y.j + 2):(1 + n.x.i + 2*n.y.j)] <- "<="
          
        #constraint 6
          rhs[(n.x.i + 2*n.y.j + 2):nr] <- "0"
          sense[(n.x.i + 2*n.y.j + 2):nr] <- ">="
  
      #objective function coefficients
        obj <- C1$a * d.i[C1$i.1]
        
      #Variable types
        ctype <- rep("B",nc)
    
      #Begin writing to text file
        
        fnam <- paste(fpath,"/Bm_BIA_Optim_C",card.j,"_Z",Z,"_m",mo,"_",FM.calf,"_LP.cpx",sep="")
        
        maxmin <- "MAX"
        write.table(maxmin, file = fnam, append = FALSE, 
                    quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                    col.names = FALSE)
        
        obj.string <- "OBJECTIVE:"
        write.table(obj.string, file = fnam, append = TRUE, 
                    quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                    col.names = FALSE)
    
      #Variable names
        var.nam <- colnames(c.mtx)
        
      #Concatenate objective fcn and output to lp text file
        obj.fcn <- paste("+", obj, var.nam[1:n.x.i], sep=" ", collapse=" ")
        obj.fcn
        
        #Each line in LP file can be only 560 characters long. Trim the lines to be <= 100 characters
        #for readability
        
          num.char <- nchar(obj.fcn)
          num.char
          
          line.len <- 100 #set number of characters per line
          
          if(num.char <= 100){
            write.table(obj.fcn, file = fnam, append = TRUE, 
                        quote = FALSE, sep = " ",
                        eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                        col.names = FALSE)
          }else{
            
            pos <- str_locate_all(obj.fcn,"\\+") #Locate the position of plus signs
            n.substr <- floor(num.char/100)
            start.idx <- 1
            for(idx in 1:n.substr){
              end.idx <- which(pos[[1]][,1] > start.idx & 
                                 pos[[1]][,1] <= idx*line.len)
              end.idx <- end.idx[length(end.idx)]
              substr.out <- substr(obj.fcn, start=start.idx, stop=(pos[[1]][end.idx,1]-1))
                #print(substr.out)
              write.table(substr.out, file = fnam, append = TRUE, 
                          quote = FALSE, sep = " ",
                          eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                          col.names = FALSE)
    
              start.idx <- pos[[1]][end.idx,1] #next row begins at the plus sign
    
            }
            
            #Write last line of obj.fcn
              substr.out <- substr(obj.fcn, start=start.idx, stop=num.char)
              write.table(substr.out, file = fnam, append = TRUE, 
                          quote = FALSE, sep = " ",
                          eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                          col.names = FALSE)
            
          }
    
        write.table("", file = fnam, append = TRUE, 
                    quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                    col.names = FALSE)
      
        write.table("Subject To", file = fnam, append = TRUE, 
                    quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                    col.names = FALSE)
        
        write.table("", file = fnam, append = TRUE, 
                    quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                    col.names = FALSE)
      
      #Constraints
        
        #Name constraints
          constraint.names <- paste(row.names(c.mtx),":",sep="")
        
        #Concatenate constraint rows and output to lp text file
          
          #Only output constraints for rows with non-zero entries
            sums <- rowSums(abs(c.mtx))
            length(sums) #should be 563
            no0 <- as.vector(which(sums > 0))
    
          for(r in no0){
            z.idx <- which(c.mtx[r,] == 0)
            sign.idx <- which(c.mtx[r,] < 0)
            sign <- rep("+",nc)
            sign[sign.idx] <- "-"
            if(length(z.idx) > 0){
              lhs <- paste(paste(sign[-z.idx], abs(c.mtx[r,-z.idx]), var.nam[-z.idx]), collapse = " ")
            } else {
              lhs <- paste(paste(sign, abs(c.mtx[r,]), var.nam), collapse = " ")
            }
            r.string <- paste(constraint.names[r], lhs, sense[r], rhs[r])
            
            num.char <- nchar(r.string)
            #num.char
            
            if(num.char <= 100){
              write.table(r.string, file = fnam, append = TRUE, 
                          quote = FALSE, sep = " ",
                          eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                          col.names = FALSE)
            }else{
              
              pos <- str_locate_all(r.string,"\\+") #Locate the position of plus signs
              neg <- str_locate_all(r.string,"-")
              sign.loc <- rbind.data.frame(pos[[1]],neg[[1]])
              sort.idx <- sort.int(sign.loc[,1], index.return=TRUE)$ix
              sign.loc <- sign.loc[sort.idx,]
              
              n.substr <- floor(num.char/100)
              start.idx <- 1
              for(idx in 1:n.substr){
                end.idx <- which(sign.loc[,1] > start.idx & 
                                   sign.loc[,1] <= idx*line.len)
                end.idx <- end.idx[length(end.idx)]
                substr.out <- substr(r.string, start=start.idx, stop=(sign.loc[end.idx,1]-1))
                  #print(substr.out)
                write.table(substr.out, file = fnam, append = TRUE, 
                            quote = FALSE, sep = " ",
                            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                            col.names = FALSE)
      
                start.idx <- sign.loc[end.idx,1] #next row begins at the plus sign
              }
              
              #Write last line of r.string
                substr.out <- substr(r.string, start=start.idx, stop=num.char)
                write.table(substr.out, file = fnam, append = TRUE, 
                            quote = FALSE, sep = " ",
                            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                            col.names = FALSE)
              
            }
          }
    
      #Binary Variables 
          
        write.table("", file = fnam, append = TRUE, 
                    quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                    col.names = FALSE)
      
        write.table("BINARY", file = fnam, append = TRUE, 
                    quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                    col.names = FALSE)
        
        for(i in 1:nc){
          write.table(var.nam[i], file = fnam, append = TRUE, 
                      quote = FALSE, sep = " ",
                      eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                      col.names = FALSE)
        }
          
      #End statement
        
        write.table("", file = fnam, append = TRUE, 
                    quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                    col.names = FALSE)
      
        write.table("END", file = fnam, append = TRUE, 
                    quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                    col.names = FALSE)
          
    } 
   
  #Input encounter rate shapefiles 
      FM.spdf <- readOGR(dsn="Output0019/Shapefiles", 
                           layer="Bm_FeedMill_Rate_ByMo_0019_pooled")
      
      calf.spdf <- readOGR(dsn="Output1219/Shapefiles", 
                           layer="Bm_Calf_Rate_ByMo_1219_pooled")
      
      #Identify which cells have d.i > 0 for each month
        
        jul.FM.gt0 <- which(FM.spdf@data$july > 0)
        aug.FM.gt0 <- which(FM.spdf@data$august > 0)
        sep.FM.gt0 <- which(FM.spdf@data$september > 0)
        oct.FM.gt0 <- which(FM.spdf@data$october > 0)
        FM.gt0 <- list("jul"=jul.FM.gt0,
                                   "aug"=aug.FM.gt0,
                                   "sep"=sep.FM.gt0,
                                   "oct"=oct.FM.gt0)
        
        jul.calf.gt0 <- which(calf.spdf@data$july > 0)
        aug.calf.gt0 <- which(calf.spdf@data$august > 0)
        sep.calf.gt0 <- which(calf.spdf@data$september > 0)
        oct.calf.gt0 <- which(calf.spdf@data$october > 0)
        calf.gt0 <- list("jul"=jul.calf.gt0,
                                   "aug"=aug.calf.gt0,
                                   "sep"=sep.calf.gt0,
                                   "oct"=oct.calf.gt0)
        
  #Create LP files for different values of Z, C.set, months, and activity states     
      
      #Set variables to cycle through
        #Z.vec <- seq(from=0.05, to=0.5, by=0.05)
        Z.vec <- seq(from=0.1, to=1.2, by=0.1)
        C.list <- list(C1[,1], C2.unq, C3, C4, C5)
        fm.calf <- c("FM", "calf")
        
      #Set filenames, paths, and text that are common to all cycles  
        cplex.folder <- "/Output_BIA_Z_Deux/CPLEX_LP/"
        
        wd <- getwd()
        bat.txt <- rep("X",7)
        bat.txt[1] <- "set timelimit 3600"
        bat.txt[4] <- "mipopt"
        bat.txt[5] <- "change problem fixed"
        bat.txt[6] <- "primopt"
        bat.fnam <- paste(wd,cplex.folder,"batch.txt",sep="")
        #bat.fnam <- paste(wd,cplex.folder,"test.txt",sep="")
        
        mos <- 7:10
        
        write.table("", file = bat.fnam, append = FALSE, 
                    quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                    col.names = FALSE)
      
      debugg <- FALSE
      #debugg <- TRUE
      if(debugg <- TRUE){
        z.idx <- 10
        c.idx <- 2
        m.idx <- 3
      }
      
      for(c.idx in 2:5){
        
        for(z.idx in 1:length(Z.vec)){
          
          for(m.idx in 1:length(mos)){

            #Create LP files
            
              folder <- substr(cplex.folder, start=2, stop=nchar(cplex.folder))
            
              write.cpx.LP(c1=C1, C.set=C.list[[c.idx]], Z=Z.vec[z.idx], 
                           d.i=FM.spdf@data[,m.idx], fpath=folder, FM.calf="FM", mo=mos[m.idx])
              
              write.cpx.LP(c1=C1, C.set=C.list[[c.idx]], Z=Z.vec[z.idx], 
                           d.i=calf.spdf@data[,m.idx], fpath=folder, FM.calf="calf", mo=mos[m.idx])
              
            #Output commands to batch file
              
              for(j in 1:length(fm.calf)){
      
                bat.txt[2] <- paste("set logfile ", wd, cplex.folder, "Bm_BIA_Optim_C", 
                             c.idx, "_Z", Z.vec[z.idx], "_m", mos[m.idx], "_", fm.calf[j], 
                             "_log.log", sep="") 
                
                bat.txt[3] <- paste("read ", wd, cplex.folder, "Bm_BIA_Optim_C", 
                             c.idx, "_Z", Z.vec[z.idx], "_m", mos[m.idx],  "_", fm.calf[j],
                             "_LP.cpx lp", sep="")
                
                bat.txt[7] <- paste("write ", wd, cplex.folder, "Bm_BIA_Optim_C", 
                             c.idx, "_Z", Z.vec[z.idx], "_m", mos[m.idx],  "_", fm.calf[j],
                             "_solution.sol", sep="")
                
                for(b in 1:length(bat.txt)){
                  write.table(bat.txt[b], file = bat.fnam, append = TRUE, 
                            quote = FALSE, sep = " ",
                            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                            col.names = FALSE)
                }  #end b loop
                
              }  #end j loop
              
          } #end m loop    

        } #end Z loop
        
      } #end C loop
        
     
      
      
      
      
      
         
        
        
  #  1. Change all of the "/" in the batch file to "\"  
  #  2. The cplex path for entering into terminal:
  #    C:\Program Files\ibm\ILOG\CPLEX_Studio129\cplex\bin\x64_win64
  #  3. The CPLEX_LP path in the working directory: 
  #    C:\Users\megan.ferguson\Work\FergusonFiles\ArcticMonkeys\Analysis\CamdenBayBehavior\Output_BIA_Z_Deux\CPLEX_LP
  #  4. Run CPLEX from the terminal 
      
      
      
      
      
      
      
      
      
      

  #Input cplex solutions and create objects to store results
      
      Z.vec <- seq(from=0.1, to=1.2, by=0.1)
      
      debugg <- FALSE
      #debugg <- TRUE
      if(debugg == TRUE){  
        c.idx <- 3
        z.idx <- 7
        m.idx <- 3
        j <- 2
      }  
      
      smry.df <- cbind.data.frame("c"=rep(0.0,384),
                                  "z"=rep(0.0,384),
                                  "m"=rep(0.0,384),
                                  "fm.calf"=rep(NA,384),
                                  "objVal"=rep(0.0,384),
                                  "sum.xi"=rep(0.0,384),
                                  "binary.x"=rep(TRUE,384))
      smry.idx <- 0

      for(c.idx in 2:5){
        
        for(z.idx in 1:length(Z.vec)){
          
          for(m.idx in 1:length(mos)){

            #Results for FM & calf
              
              for(j in 1:2){
                
                binary.x <- TRUE
            
                fnam <- paste(cplex.folder,"Bm_BIA_Optim_C",c.idx,"_Z",Z.vec[z.idx],
                              "_m",mos[m.idx],"_",fm.calf[j],"_solution.sol",sep="")
                fnam <- substr(fnam, start=2, stop=nchar(fnam))
                
                lp.sol <- scan(fnam, what=character(), sep="<")
                var.sol <- character()
                for(i in 1:length(lp.sol)){
                  #Find & extract variables 
                    test.var <- grep("variable name=x", lp.sol[i]) 
                    if(length(test.var)>0)
                      var.sol <- c(var.sol,lp.sol[i])
                  #Find & extract value of objective fcn
                    test.obj <- grep("objectiveValue=", lp.sol[i])
                    if(length(test.obj)>0){
                      start.idx <- str_locate(lp.sol[i],"=") + 1  
                      objVal <- as.numeric(substr(lp.sol[i], start=start.idx, stop=nchar(lp.sol[i])))
                    }
                }
            
                x.in.sol <- cbind.data.frame("x.i"=C1$i.1, "value"=0)
                
                for(i in 1:nrow(x.in.sol)){
                  var.test <- grep("value=1", var.sol[i]) 
                  if(length(var.test) == 1){
                    x.in.sol[i,2] <- 1  
                  }
                  
                  #Check for non-binary values. This was an issue in c3/sep/z0.7/calf
                    val.idx <- str_locate(var.sol[i], "value=") 
                    if(substr(var.sol[i], start=(val.idx[1,2]+2), stop=(val.idx[1,2]+2)) ==
                       "."){ #If non-binary
                      x.in.sol[i,2] <- as.numeric(substr(var.sol[i], 
                                                  start=(val.idx[1,2]+1),
                                                  stop=(val.idx[1,2]+3))) 
                      binary.x <- FALSE
                    }  
                }
                #CK
                  debugg <- FALSE
                  #debugg <- TRUE
                  if(debugg == TRUE){
                    print(var.sol)
                    print(class(var.sol))
                    
                    print(length(var.sol))
                    print(nrow(C1))
                    
                    print(summary(x.in.sol))
                    print(sum(x.in.sol[,2]))
                    print(x.in.sol)
                    
                    print(objVal)
                  }  
                
                #Plot    
                  #plot.now <- FALSE
                  plot.now <- TRUE
                  if(plot.now == TRUE){
                    
                    plot.col <- c("turquoise","orchid")
      
                    png(file=paste("Output_BIA_Z_Deux//Figures//C",c.idx,"_Z",Z.vec[z.idx],
                                   "_m",mos[m.idx],"_",fm.calf[j],".png",sep=""),
                                   height=500, width=1000, pointsize=30)
                    
                      par(mai=rep(0.5,4))
                    
                      plot(buff.hexPols)
                      
                      plot.x <- x.in.sol$x.i[which(x.in.sol$value > 0)]
                      for(i in plot.x){
                        plot(hex.SPlist[[i]], col=plot.col[j], add=TRUE)
                      }
                      
                      for(i in 1:n.hex.in){
                        plot(hex.in.SPlist[[i]], lwd=4, add=TRUE)
                      }
                      
                      if(j == 1) plot(buff.hexPts[FM.gt0[[m.idx]],], pch="*", add=TRUE) #* in cells with d.i>0
                      if(j == 2) plot(buff.hexPts[calf.gt0[[m.idx]],], pch="*", add=TRUE) #* in cells with d.i>0
                      
                      plot(shor.SP.ec, col="gray", add=TRUE)
                    
                    dev.off()    
                    
                  }
                  
                #Output x.in.sol and objVal
                  out.list <- list("c"=c.idx,
                                   "z"=Z.vec[z.idx],
                                   "m"=mos[m.idx],
                                   "fm.calf"=fm.calf[j],
                                   "objVal"=objVal,
                                   "x.in.sol"=x.in.sol)
                  save(out.list, file=paste("Output_BIA_Z_Deux//OutLists//C",c.idx,"_Z",Z.vec[z.idx],
                                   "_m",mos[m.idx],"_",fm.calf[j],".Rdata",sep=""))
                  
                  smry.idx <- smry.idx + 1

                  smry.df$c[smry.idx] <- c.idx
                  smry.df$z[smry.idx] <- Z.vec[z.idx]
                  smry.df$m[smry.idx] <- mos[m.idx]
                  smry.df$fm.calf[smry.idx] <- fm.calf[j]
                  smry.df$objVal[smry.idx] <- objVal
                  smry.df$sum.xi[smry.idx] <- sum(x.in.sol[,2])
                  smry.df$binary.x[smry.idx] <- binary.x
                  
              }  
            
          } #end m loop
          
        } #end z loop
        
      } #end c loop  
      
        save(smry.df, FM.gt0, calf.gt0, 
             buff.hexPols, buff.hexPts, hex.SPlist, n.hex.in, hex.in.SPlist, shor.SP.ec, ECBproj6,
             file="Output_BIA_Z_Deux//OutLists//Bm_BIA_Optim.rdata")
        write.csv(smry.df, file="Output_BIA_Z_Deux//OutLists//smry_df.csv")
      
      
          
