#Script CamdenBayBehavior.r...Megan C. Ferguson...25 April 2020

  # 1. This script borrowed code from MedianMig0019.r, ASAMM2019EffortSightingsByZBlk_AllCets.r,
  #    and ASAMM_ACEs_hexD.r.
  #
  # 2. An equidistant conic map projection for Beaufort Sea study area is used:
  #          ECBproj6 <- CRS('+proj=eqdc +lat_1=69.9d
  #                                    +lat_2=71.6d
  #                                    +lat_0=70.75d
  #                                    +lon_0=-148.0d
  #                                    +x_0=0
  #                                    +y_0=0')
  #
  # 3. Files to import:
  #  Data
  #    am_all_1979_2019_v3_34_v0_28_v22_v3.Rdata
  #    CAPs_Sightings20181130.csv
  #    CAPs_Sightings_2019.csv
  #  Shapefiles
  #    BWASPblocks_ExclBarrierIs_ManyPts
  #  Functions:
  #    BeaufEffortByPolyInsxnLFcns.Rdata
  #    xtract_flightlines_SL_byBeaufVis_ReturnList_20181223.Rdata

  library(sp)
  library(rgeos)
  library(maptools)
  library(rgdal)
  library(fields)
  
  #Set character to designate range in years for filenames
    yr.fnam <- "0019"
    #yr.fnam <- "1219"
  
  #Input required functions
    load("Functions//BeaufEffortByPolyInsxnLFcns.Rdata")
    load("Functions//xtract_flightlines_SL_byBeaufVis_ReturnList_20181223.Rdata")

  #Input data
    load("Data//am_all_1979_2019_v3_34_v0_28_v22_v3.Rdata")
    
  #Input CAPs sighting data  
    caps.dat18 <- read.csv(file="Data//CAPs_Sightings20181130.csv")
    caps.dat19 <- read.csv(file="data//CAPs_Sightings_2019.csv")
    caps.dat <- rbind.data.frame(caps.dat18, caps.dat19)
    #CK
      dim(caps.dat18)
      dim(caps.dat19)
      dim(caps.dat)
      summary(caps.dat)
    
  #Create 2000-2019 dataframe
    am0019 <- am.all[am.all$Yr > 1999,]
    #CK
      summary(am0019$Yr)
      summary(am0019$Month)
      summary(am0019$SurvNam)
      rm(am.all)
    
  #Define map projection for Beaufort Sea study area.  Use Equidistant Conic.
    ECBproj6 <- CRS('+proj=eqdc +lat_1=69.9d
                              +lat_2=71.6d
                              +lat_0=70.75d
                              +lon_0=-148.0d
                              +x_0=0
                              +y_0=0')
  
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
         
      #Create a list for use in beauf.effort.by.poly
        hex.SPlist <- lapply(1:length(buff.hexPols), function(i){
          p.i <- SpatialPolygons(list(buff.hexPols@polygons[[i]]), proj4string=ECBproj6)
          return(p.i)
        })
        #CK
          length(hex.SPlist)
          
    #Function to do the following:
    # 1. subset data into time periods (years and months)
    # 2. create segments on transect and caps passing
    # 3. output .png files (if trax.out=TRUE) and shapefiles of segments (if trax.out=TRUE) 
    # 4. call beauf.effort.by.poly and insxn.L to summarize effort within the polygons
      debugg <- FALSE
      #debugg <- TRUE
      if(debugg == TRUE){
        yr <- 2019
        mos <- 9
        alldat <- am0019
        Prj <- ECBproj6
        Blk.list <- hex.SPlist
        region <- "hex_WBS"
        m <- 1
        trax.out <- TRUE
        caps <- "Tx"
      }
      #
      capstx.effort.by.tym.Blk <- function(yr, mos, alldat, Prj, Blk.list,
                                     region, trax.out, caps){
        #Create subset of required year
          yrdat <- alldat[which(alldat$Yr == yr),]
          #CK
            summary(yrdat$Yr)
        #Run through the following for each month
          moeffort <- lapply(1:length(mos), function(m){
            #Create monthly dataset
              modat <- yrdat[yrdat$Month == mos[m],]
              #CK
                summary(modat$Month)
            if(nrow(modat) > 0){ 
              #Create output filenames
                Fnam <- paste(caps,yr,"mo",mos[m],sep="")
                Fnam.blk.insxn <- paste("Output//Insxn//Insxn",region,Fnam,"HEX.Rdata",sep="")
                Fnam.blk.tp.L <- paste("Output//Insxn//InsxnTrakPolyLen",region,Fnam,"HEX.Rdata",sep="")
                Fnam.blk.p.L <- paste("Output//Insxn//InsxnPolyLen",region,Fnam,"HEX.Rdata",sep="")
              #Extract continuous transect segments as SpatialLines object.
                if(caps=="CAPs"){
                  if(length(which(modat$FltType == 7)) > 0){
                    motrax <- xtract.caps.SL(modat, Prj)
                  } else {
                    #na.caps <- NA
                    insxn.L.by.blk <- NA
                    #save(na.caps, file=Fnam.blk.insxn)
                    #save(na.caps, file=Fnam.blk.tp.L)
                    save(insxn.L.by.blk, file=Fnam.blk.p.L)
                    return(NA) #No caps effort
                  }  
                } else {
                  motrax <- xtract.tx.byBeaufVis.SL(modat, Prj, beauf.x=-1, vis.x=-1)                  
                }
                nadat <- data.frame(NA)
                traxPrj.SLDF <- SpatialLinesDataFrame(motrax[[3]], data=nadat, match.ID=F)
                traxLL.SLDF <- SpatialLinesDataFrame(motrax[[4]], data=nadat, match.ID=F)
              #Export trax to shapefile, save to .Rdata file, and plot
                if(trax.out == TRUE){
                  writeOGR(traxPrj.SLDF, "Output//Trax", paste(Fnam,"Prj",sep=""), driver="ESRI Shapefile", overwrite_layer = TRUE)
                  writeOGR(traxLL.SLDF, "Output//Trax", paste(Fnam,"LL",sep=""), driver="ESRI Shapefile", overwrite_layer = TRUE)
                  save(motrax, file=paste("Output//Trax//Trax",Fnam,".Rdata",sep=""))
                  png(file=paste("Output//Trax//Trax",Fnam,".png",sep=""),height=1000,width=1000,pointsize=24)
                    par(mfrow=c(2,1))
                    plot(motrax[[3]], col="cyan")      #start-to-end
                    plot(motrax[[5]], col="magenta")     #short segments
                  dev.off()
                }
              #Summarize effort by block and depth
                #Divide effort into polygons, compute length of transect effort
                #for each transect/polygon and each polygon in eff.smry
                  blk.eff.smry <- beauf.effort.by.poly(motrax, 3, Prj, Blk.list, Fnam.blk.insxn, debugg=FALSE)
                  blk.insxn.L <- insxn.L(blk.eff.smry, Fnam.blk.tp.L)
                  if(class(blk.insxn.L) == "matrix"){ 
                    insxn.L.by.blk <- colSums(blk.insxn.L)
                  } else { #Use this if there was only one flight
                    insxn.L.by.blk <- blk.insxn.L
                  }
                  save(insxn.L.by.blk, file=Fnam.blk.p.L)
                  rm(blk.eff.smry, blk.insxn.L, insxn.L.by.blk)
                #Return simple output
                  return(Fnam)
            } else {
              return(NULL)
            }  
          })
          return(moeffort)
      }
      
      mos <- 7:10
      all.mos <- c("july", "august", "september", "october")

      #Summarize transect effort
                
        for(yr.i in 2000:2019){ 
        
          hex.tx <- capstx.effort.by.tym.Blk(yr.i, mos, am0019, ECBproj6, 
                                           hex.SPlist, "hex_WBS", TRUE, "Tx")                                       
          #CK
          #  load("Output//Insxn//InsxnPolyLenhex_WBSTx2019mo7HEX.Rdata") #object insxn.L.by.blk
          #  insxn.L.by.blk
          #  rm(insxn.L.by.blk)
        }    

        #Summarize CAPs passing effort    

          for(yr.i in 2018:2019){ 
            
            hex.caps <- capstx.effort.by.tym.Blk(yr.i, mos, am0019, ECBproj6, 
                                             hex.SPlist, "hex_WBS", FALSE, "CAPs")                                       
            #CK
            #  load("Output//Insxn//InsxnPolyLenhex_WBSCAPs2019mo10HEX.Rdata") #object insxn.L.by.blk
            #  insxn.L.by.blk
            #  rm(insxn.L.by.blk)
            
          }  

    #Create dataframes and shapefiles with results

      #Run through months and years and output results
      
        #Tx   
      
          for(m in mos){
      
            m.tx.df.by.yr <- as.data.frame(sapply(2000:2019, function(yr.i, mo=m){
                
                #print(yr.i)
                #print(mo)
                
                try.load <- try(load(paste("Output//Insxn//InsxnPolyLenhex_WBSTx",yr.i,"mo",mo,"HEX.Rdata",sep="")),
                                silent=TRUE) #object insxn.L.by.blk
                if(class(try.load) == "try-error"){
                  insxn.L.by.blk <- rep(0, length(hex.SPlist))
                }
                return(insxn.L.by.blk)
                              
            }))
            tot <- rowSums(m.tx.df.by.yr)
            m.tx.df.by.yr <- cbind.data.frame(m.tx.df.by.yr, tot)
            names(m.tx.df.by.yr) <- c(2000:2019, "TOTAL")
            
            #Output dataframes as .Rdata and .csv files
              save(m.tx.df.by.yr, file=paste("Output//m",m,"txdfByYr.Rdata",sep=""))
              write.table(m.tx.df.by.yr, file=paste("Output//m",m,"txdfByYr.csv",sep=""), 
                          sep=",", col.names=TRUE, row.names=FALSE)  
              
            #Output Shapefiles
              spdf <- SpatialPolygonsDataFrame(buff.hexPols, data=m.tx.df.by.yr, match.ID=FALSE)
              writeOGR(spdf, "Output//Shapefiles", paste("m",m,"tx_effort_ByYr",sep=""),
                       driver="ESRI Shapefile", overwrite_layer = TRUE)
              
            #CK
              dim(m.tx.df.by.yr) #284 x 20
              colSums(m.tx.df.by.yr)
              
          }      
              
        #CAPs   
      
          for(m in mos){
      
            m.caps.df.by.yr <- as.data.frame(sapply(2018:2019, function(yr.i, mo=m){
                
                #print(yr.i)
                #print(mo)
                
                try.load <- try(load(paste("Output//Insxn//InsxnPolyLenhex_WBSCAPs",yr.i,"mo",mo,"HEX.Rdata",sep="")),
                                silent=TRUE) #object insxn.L.by.blk
                if(class(try.load) == "try-error"){
                  insxn.L.by.blk <- rep(0, length(hex.SPlist))
                }
                return(insxn.L.by.blk)
                              
            }))
            tot <- rowSums(m.caps.df.by.yr)
            m.caps.df.by.yr <- cbind.data.frame(m.caps.df.by.yr, tot)
            names(m.caps.df.by.yr) <- c(2018:2019, "TOTAL")
            
            #Output dataframes as .Rdata and .csv files
              save(m.caps.df.by.yr, file=paste("Output//m",m,"capsdfByYr.Rdata",sep=""))
              write.table(m.caps.df.by.yr, file=paste("Output//m",m,"capsdfByYr.csv",sep=""), 
                          sep=",", col.names=TRUE, row.names=FALSE)  
            
            #Output Shapefiles
              spdf <- SpatialPolygonsDataFrame(buff.hexPols, data=m.caps.df.by.yr, match.ID=FALSE)
              writeOGR(spdf, "Output//Shapefiles", paste("m",m,"caps_effort_ByYr",sep=""),
                       driver="ESRI Shapefile", overwrite_layer = TRUE)
              
            #CK
              dim(m.caps.df.by.yr) #284 x 20
              colSums(m.caps.df.by.yr)
              
          } 

  #Extract bowhead transect & caps sighting data: # individuals by cell for each month and year.
    debugg <- FALSE
    #debugg <- TRUE
    if(debugg == TRUE){
      sp.id <- "bowhead whale"
      yrbeg <- 2019
      yrend <- 2019
      mos <- 7:10
      #alldat <- am0019
      alldat <- caps.dat
      Prj <- ECBproj6
      Blk.SP <- buff.hexPols
      sp.region <- "test"
      m <- 2
      #ntry <- "caps"
      #ntry <- "s on transect"
      #clf <- TRUE
      #clf <- FALSE
    }  
    #  
    xtract.capstx.spp.nind.moblk <- function(sp.id, yrbeg, yrend, mos, alldat, Prj, 
                                       Blk.SP, sp.region, debugg=FALSE,
                                       ntry, clf){
      
      #This function knows how to output number of calves for transect and caps effort.
      
      spp.nind.moblkz <- lapply(1:length(mos), function(m){                                 
        #Create a sp.id sighting-only dataset for month m that omits 
        #repeat whales
          if(ntry != "caps"){
            if(yrend - yrbeg < 1){
              idx <- which((alldat$Species == sp.id & is.na(alldat$Species) == FALSE) &
                               alldat$Entry == ntry &
                               (alldat$Repeat != "yes" | is.na(alldat$Repeat) == TRUE) &
                               alldat$Yr == yrbeg &
                               (alldat$PrimObs == 1 & is.na(alldat$PrimObs) == FALSE) &
                               (alldat$Month == mos[m] & is.na(alldat$Month) == FALSE))            
            } else {
              idx <- which((alldat$Species == sp.id & is.na(alldat$Species) == FALSE) &
                               alldat$Entry == ntry &
                               (alldat$Repeat != "yes" | is.na(alldat$Repeat) == TRUE) &
                               (alldat$Yr >= yrbeg & alldat$Yr <= yrend) &
                               (alldat$PrimObs == 1 & is.na(alldat$PrimObs) == FALSE) &
                               (alldat$Month == mos[m] & is.na(alldat$Month) == FALSE))
            }
            dat.sp <- alldat[idx,]
            #Ck
              summary(dat.sp$Yr)
              summary(dat.sp$Species)
              summary(dat.sp$Entry)
              summary(dat.sp$Repeat)
              summary(dat.sp$Behavior)
              summary(dat.sp$Month)
          } else {
            idx <- which(alldat$new.Sp == sp.id &
                         (alldat$Yr >= yrbeg & alldat$Yr <= yrend) &
                         (alldat$Month == mos[m] & is.na(alldat$Month) == FALSE))
            dat.sp <- alldat[idx,]
            #Ck
              summary(dat.sp)
          }      
            
        if(nrow(dat.sp) > 0){   
          #Create SpatialPointsDataFrame for sightings and associated group size 
          #and output to shapefile  
          
          if(ntry != "caps"){
            
            if(clf == FALSE){
              gs <- dat.sp$FinalGrp
              gs.idx <- which(is.na(gs)==TRUE)
              gs[gs.idx] <- dat.sp$TotalNo[gs.idx]
              sp.SPtsDF <- SpatialPointsDataFrame(coords=cbind(dat.sp$ECnewlong,
                                                               dat.sp$ECnewlat),
                                                         data=data.frame(gs), proj=Prj)
            } else {
              gs <- dat.sp$CalfNo
              sp.SPtsDF <- SpatialPointsDataFrame(coords=cbind(dat.sp$ECnewlong,
                                                               dat.sp$ECnewlat),
                                                         data=data.frame(gs), proj=Prj)
            }
          } else {
            
            if(clf == FALSE){
              gs <- dat.sp$capsInd
              sp.SPtsDF.LL <- SpatialPointsDataFrame(coords=cbind(dat.sp$XWhale,
                                                               dat.sp$YWhale),
                                                         data=data.frame(gs), proj=CRS('+proj=longlat'))
              sp.SPtsDF <- spTransform(sp.SPtsDF.LL, Prj)
            } else {
              gs <- dat.sp$capsCalf
              sp.SPtsDF.LL <- SpatialPointsDataFrame(coords=cbind(dat.sp$XWhale,
                                                               dat.sp$YWhale),
                                                         data=data.frame(gs), proj=CRS('+proj=longlat'))
              sp.SPtsDF <- spTransform(sp.SPtsDF.LL, Prj)
            }  
          }            
          if(debugg==TRUE){    
              writeOGR(sp.SPtsDF, "Output//Shapefiles", paste("Sightings_With_GroupSize",yrbeg,"to",yrend,sp.region,
                                                mos[m],sep=""), driver="ESRI Shapefile", overwrite_layer = TRUE)
              #CK
                summary(gs) #Should be no NAs 
                png(paste("Output//Sights//SPtsInBlocks",yrbeg,"to",yrend,sp.region,
                                                mos[m],".png",sep=""),
                          height=1000,width=1000,bg="white")
                  plot(Blk.SP, lwd=1)
                  plot(sp.SPtsDF, pch=19, cex=0.5, col="purple", add=T)
                  plot(shor.SP.ec, col="turquoise", add=T)
                  plot(sp.SPtsDF, pch=19, cex=0.5, col="purple", add=T)
                dev.off()  
                png(paste("Output//Sights//SPtsInZ",yrbeg,"to",yrend,sp.region,
                                                mos[m],".png",sep=""),
                          height=1000,width=1000,bg="white")
                  plot(Z.SP, lwd=1)
                  plot(sp.SPtsDF, pch=19, cex=0.5, col="red", add=T)
                  plot(shor.SP.ec, col="green", add=T)
                  plot(Z.SP, lwd=1, add=TRUE)
                  plot(sp.SPtsDF, pch=19, cex=0.5, col="red", add=T)
                dev.off() 
          }
          #Extract number of sightings and number of individuals per poly
          
            #Set value of each sighting.  For non-caps sightings, each record counts
            #as one sighting.  For caps sightings, each record counts as capsN sightings.
              if(ntry != "caps"){
                n <- data.frame(rep(1, nrow(sp.SPtsDF@data)))
              } else {
                n <- data.frame(dat.sp$capsN)
              }  
              
              names(n) <- "n"
              gs1 <- SpatialPointsDataFrame(coords=sp.SPtsDF@coords, 
                                            data=n,
                                            proj4string=Prj)
          
            #By Block
              n.sp.in.blks <- over(Blk.SP, gs1, fn=sum)
              na.idx <- which(is.na(n.sp.in.blks) == TRUE)
              n.sp.in.blks[na.idx,1] <- 0
              #
              indssp.by.blks <- over(Blk.SP, sp.SPtsDF, fn=sum)
              na.idx <- which(is.na(indssp.by.blks) == TRUE)
              indssp.by.blks[na.idx,1] <- 0
          
              #Ck
                n.sp.in.blks
                indssp.by.blks
        } else {
                n.sp.in.blks <- data.frame(rep(0,length(Blk.SP@polygons)))
                indssp.by.blks <- data.frame(rep(0,length(Blk.SP@polygons)))
        }
        
        #Output
          out.list <- list("nBlks"=n.sp.in.blks,
                           "indBlks"=indssp.by.blks)   
          #When debugging guts of spp.nind.moblkz, stop here
          return(out.list)
      })    
    }
           
      #Project sighting coordinates
        idx <- which(am0019$YWhale > 0)
        newlat <- am0019$ArcLat
        newlat[idx] <- am0019$YWhale[idx]
        newlong <- am0019$ArcLong
        newlong[idx] <- am0019$XWhale[idx]
        xy.newLL.SPts <- SpatialPoints(cbind(newlong, newlat),
                                       proj=CRS('+proj=longlat'))
        xy.new.SPts <- spTransform(xy.newLL.SPts, ECBproj6)
        am0019 <- cbind.data.frame(am0019,"ECnewlong"=xy.new.SPts@coords[,1],
                                 "ECnewlat"=xy.new.SPts@coords[,2])

      yrbeg <- 2000
      yrend <- 2019
      
      caps.yrbeg <- 2018
      caps.yrend <- 2019
    
      #Bowheads
    
        #Limit to feeding and milling whales
    
          idx <- which(am0019$Behavior == "mill" | am0019$Behavior == "feed")
          dat <- am0019[idx,]
          
        #Transect  

          #Individual years
          
            Bm.nind.moblk <- sapply(yrbeg:yrend, function(y){
              #y <- 2019
              nind.Bm <- xtract.capstx.spp.nind.moblk("bowhead whale",y,y,mos,dat,ECBproj6, 
                                                     buff.hexPols,"hex_WBS",FALSE,
                                                     ntry="s on transect", clf=FALSE)
                     
              save(nind.Bm, file=paste("Output//Sights//nind_Bm_FeedMill",y,".Rdata",sep=""))
              return(y)
            })         
          
          #Pool years
          
            tot.nind.Bm.fm <- xtract.capstx.spp.nind.moblk("bowhead whale",2000,2019,mos,dat,ECBproj6, 
                                                     buff.hexPols,"hex_WBS",FALSE,
                                                     ntry="s on transect", clf=FALSE)
                     
            save(tot.nind.Bm.fm, 
                 file=paste("Output//Sights//tot",yr.fnam,"_nind_Bm_FeedMill.Rdata",sep=""))
            #CK 
              class(tot.nind.Bm.fm)
              str(tot.nind.Bm.fm)

            #Prep for output 
              
              tot.ind.Bm.fm.bymo <- rep(NA, length(buff.hexPols))
                
              for(i in 1:length(mos)){
                tot.ind.Bm.fm.bymo <- cbind.data.frame(tot.ind.Bm.fm.bymo, tot.nind.Bm.fm[[i]][[2]])
              } 
              
              tot.ind.Bm.fm.bymo <- tot.ind.Bm.fm.bymo[,-1] #remove column of NAs
              names(tot.ind.Bm.fm.bymo) <- all.mos[1:length(mos)]
              
              #Output Shapefiles
                spdf <- SpatialPolygonsDataFrame(buff.hexPols, data=tot.ind.Bm.fm.bymo, match.ID=FALSE)
                writeOGR(spdf, "Output//Shapefiles", paste("Bm_FeedMill_ByMo_",yr.fnam,"_pooled",sep=""),
                         driver="ESRI Shapefile", overwrite_layer = TRUE)

              #CK
                dim(tot.ind.Bm.fm.bymo)
                class(tot.ind.Bm.fm.bymo)
                names(tot.ind.Bm.fm.bymo)

        #CAPs  
                
          #Include sightings with capsPropFeedMill > 0  
            idx <- which(caps.dat$capsPropFeedMill > 0)
            dat <- caps.dat[idx,]
            summary(dat$capsInd)
            summary(dat$capsN)
            summary(as.factor(dat$capsPropFeedMill))
            
            #Scale capsN and capsInd by capsPropFeedMill for each sighting
              dat$capsInd <- dat$capsInd * dat$capsPropFeedMill
              dat$capsN <- dat$capsN * dat$capsPropFeedMill
              #Ck
                summary(as.factor(dat$capsPropFeedMill))
                summary(dat$capsInd)
                summary(dat$capsN)

          #Individual years
          
            Bm.nind.moblk.caps <- sapply(caps.yrbeg:caps.yrend, function(y){
              #y <- 2019
              nind.Bm <- xtract.capstx.spp.nind.moblk("bowhead whale",y,y,mos,dat,ECBproj6, 
                                                     buff.hexPols,"hex_WBS_caps",FALSE,
                                                     ntry="caps", clf=FALSE)
                     
              save(nind.Bm, file=paste("Output//Sights//nind_Bm_FeedMill_caps",y,".Rdata",sep=""))
              return(y)
            })         
          
          #Pool years
          
            tot.nind.Bm.fm.caps <- xtract.capstx.spp.nind.moblk("bowhead whale",2018,2019,mos,dat,ECBproj6, 
                                                     buff.hexPols,"hex_WBS_caps",FALSE,
                                                     ntry="caps", clf=FALSE)
                     
            save(tot.nind.Bm.fm.caps, 
                 file=paste("Output//Sights//tot",yr.fnam,"_nind_Bm_FeedMill_caps.Rdata",sep=""))
            #CK 
              class(tot.nind.Bm.fm.caps)
              str(tot.nind.Bm.fm.caps)

            #Prep for output 
              
              tot.ind.Bm.fm.bymo.caps <- rep(NA, length(buff.hexPols))
                
              for(i in 1:length(mos)){
                tot.ind.Bm.fm.bymo.caps <- cbind.data.frame(tot.ind.Bm.fm.bymo.caps, tot.nind.Bm.fm.caps[[i]][[2]])
              } 
              
              tot.ind.Bm.fm.bymo.caps <- tot.ind.Bm.fm.bymo.caps[,-1] #remove column of NAs
              names(tot.ind.Bm.fm.bymo.caps) <- all.mos[1:length(mos)]
              
              #Output Shapefiles
                spdf <- SpatialPolygonsDataFrame(buff.hexPols, data=tot.ind.Bm.fm.bymo.caps, match.ID=FALSE)
                writeOGR(spdf, "Output//Shapefiles", 
                         paste("Bm_FeedMill_caps_ByMo_",yr.fnam,"_pooled",sep=""),
                         driver="ESRI Shapefile", overwrite_layer = TRUE)

              #CK
                dim(tot.ind.Bm.fm.bymo.caps)
                class(tot.ind.Bm.fm.bymo.caps)
                names(tot.ind.Bm.fm.bymo.caps)
                
        #Limit to calves
    
          idx <- which(am0019$CalfNo >0 &
                         (am0019$Behavior != "dead" | is.na(am0019$Behavior)==TRUE))
          dat <- am0019[idx,]
          
          #Transect  
  
            #Individual years
            
              Bm.nind.moblk.calf <- sapply(yrbeg:yrend, function(y){
                #y <- 2019
                nind.Bm <- xtract.capstx.spp.nind.moblk("bowhead whale",y,y,mos,dat,ECBproj6, 
                                                       buff.hexPols,"hex_WBS_calf",FALSE,
                                                       ntry="s on transect", clf=TRUE)
                       
                save(nind.Bm, file=paste("Output//Sights//nind_Bm_Calf",y,".Rdata",sep=""))
                return(y)
              })         
            
            #Pool years
            
              tot.nind.Bm.calf <- xtract.capstx.spp.nind.moblk("bowhead whale",2000,2019,mos,dat,ECBproj6, 
                                                       buff.hexPols,"hex_WBS_calf",FALSE,
                                                       ntry="s on transect", clf=TRUE)
                       
              save(tot.nind.Bm.calf, 
                   file=paste("Output//Sights//tot",yr.fnam,"_nind_Bm_Calf.Rdata",sep=""))
              #CK 
                class(tot.nind.Bm.calf)
                str(tot.nind.Bm.calf)
  
              #Prep for output 
                
                tot.ind.Bm.calf.bymo <- rep(NA, length(buff.hexPols))
                  
                for(i in 1:length(mos)){
                  tot.ind.Bm.calf.bymo <- cbind.data.frame(tot.ind.Bm.calf.bymo, tot.nind.Bm.calf[[i]][[2]])
                } 
                
                tot.ind.Bm.calf.bymo <- tot.ind.Bm.calf.bymo[,-1] #remove column of NAs
                names(tot.ind.Bm.calf.bymo) <- all.mos[1:length(mos)]
                
                #Output Shapefiles
                  spdf <- SpatialPolygonsDataFrame(buff.hexPols, data=tot.ind.Bm.calf.bymo, match.ID=FALSE)
                  writeOGR(spdf, "Output//Shapefiles", 
                           paste("Bm_Calf_ByMo_",yr.fnam,"_pooled",sep=""),
                           driver="ESRI Shapefile", overwrite_layer = TRUE)
  
                #CK
                  dim(tot.ind.Bm.calf.bymo)
                  class(tot.ind.Bm.calf.bymo)
                  names(tot.ind.Bm.calf.bymo)
                  
          #CAPs 
                  
            idx <- which(caps.dat$capsCalf > 0)
            dat <- caps.dat[idx,]
            nrow(dat)

          #Individual years
          
            Bm.nind.moblk.calf.caps <- sapply(caps.yrbeg:caps.yrend, function(y){
              #y <- 2019
              nind.Bm <- xtract.capstx.spp.nind.moblk("bowhead whale",y,y,mos,dat,ECBproj6, 
                                                     buff.hexPols,"hex_WBS_calf_caps",FALSE,
                                                     ntry="caps", clf=TRUE)
                     
              save(nind.Bm, file=paste("Output//Sights//nind_Bm_Calf_caps",y,".Rdata",sep=""))
              return(y)
            })         
          
          #Pool years
          
            tot.nind.Bm.calf.caps <- xtract.capstx.spp.nind.moblk("bowhead whale",2018,2019,mos,dat,ECBproj6, 
                                                     buff.hexPols,"hex_WBS_calf_caps",FALSE,
                                                     ntry="caps", clf=TRUE)
                     
            save(tot.nind.Bm.calf.caps, 
                 file=paste("Output//Sights//tot",yr.fnam,"_nind_Bm_Calf_caps.Rdata",sep=""))
            #CK 
              class(tot.nind.Bm.calf.caps)
              str(tot.nind.Bm.calf.caps)

            #Prep for output 
              
              tot.ind.Bm.calf.bymo.caps <- rep(NA, length(buff.hexPols))
                
              for(i in 1:length(mos)){
                tot.ind.Bm.calf.bymo.caps <- cbind.data.frame(tot.ind.Bm.calf.bymo.caps, tot.nind.Bm.calf.caps[[i]][[2]])
              } 
              
              tot.ind.Bm.calf.bymo.caps <- tot.ind.Bm.calf.bymo.caps[,-1] #remove column of NAs
              names(tot.ind.Bm.calf.bymo.caps) <- all.mos[1:length(mos)]
              
              #Output Shapefiles
                spdf <- SpatialPolygonsDataFrame(buff.hexPols, data=tot.ind.Bm.calf.bymo.caps, match.ID=FALSE)
                writeOGR(spdf, "Output//Shapefiles", 
                         paste("Bm_Calf_caps_ByMo_",yr.fnam,"_pooled",sep=""),
                         driver="ESRI Shapefile", overwrite_layer = TRUE)

              #CK
                dim(tot.ind.Bm.calf.bymo.caps)
                class(tot.ind.Bm.calf.bymo.caps)
                names(tot.ind.Bm.calf.bymo.caps)

  #Compute encounter rates
                  
    #Feed/Mill              

      er.fm.bymo <- data.frame(sapply(1:length(mos), function(m){
        
        #Input effort summaries
          load(file=paste("Output//m",mos[m],"txdfByYr.Rdata",sep=""))
          #print(summary(m.tx.df.by.yr$TOTAL))

          load(file=paste("Output//m",mos[m],"capsdfByYr.Rdata",sep=""))
          #print(summary(m.caps.df.by.yr$TOTAL))
          idx <- which(is.na(m.caps.df.by.yr$TOTAL) == TRUE)
          m.caps.df.by.yr$TOTAL[idx] <- 0
          
        #Compute encounter rate of feeding and milling whales
          er.fm <- (tot.ind.Bm.fm.bymo[,m] + tot.ind.Bm.fm.bymo.caps[,m])/
           (m.tx.df.by.yr$TOTAL + m.caps.df.by.yr$TOTAL)
          idx <- which(is.na(er.fm))
          er.fm[idx] <- 0
          
          return(er.fm)
  
      }))         
      names(er.fm.bymo) <- all.mos[1:length(mos)]
      
      #Output Shapefiles
        spdf <- SpatialPolygonsDataFrame(buff.hexPols, data=er.fm.bymo, match.ID=FALSE)
        writeOGR(spdf, "Output//Shapefiles", 
                 paste("Bm_FeedMill_Rate_ByMo_",yr.fnam,"_pooled",sep=""),
                           driver="ESRI Shapefile", overwrite_layer = TRUE)
      #CK
        dim(er.fm.bymo)
        names(er.fm.bymo)
        summary(er.fm.bymo) #should be no NAs
                
    #Calves              

      er.calf.bymo <- data.frame(sapply(1:length(mos), function(m){
        
        #Input effort summaries
          load(file=paste("Output//m",mos[m],"txdfByYr.Rdata",sep=""))
          #print(summary(m.tx.df.by.yr$TOTAL))
        
          load(file=paste("Output//m",mos[m],"capsdfByYr.Rdata",sep=""))
          #print(summary(m.caps.df.by.yr$TOTAL))
          idx <- which(is.na(m.caps.df.by.yr$TOTAL) == TRUE)
          m.caps.df.by.yr$TOTAL[idx] <- 0
        
        #Compute encounter rate of feeding and milling whales
          er.calf <- (tot.ind.Bm.calf.bymo[,m] + tot.ind.Bm.calf.bymo.caps[,m])/
            (m.tx.df.by.yr$TOTAL + m.caps.df.by.yr$TOTAL)
          idx <- which(is.na(er.calf))
          er.calf[idx] <- 0
          
          return(er.calf)
  
      }))         
      names(er.calf.bymo) <- all.mos[1:length(mos)]
      
      #Output Shapefiles
        spdf <- SpatialPolygonsDataFrame(buff.hexPols, data=er.calf.bymo, match.ID=FALSE)
        writeOGR(spdf, "Output//Shapefiles", 
                 paste("Bm_Calf_Rate_ByMo_",yr.fnam,"_pooled",sep=""),
                           driver="ESRI Shapefile", overwrite_layer = TRUE)
      #CK
        dim(er.calf.bymo)
        names(er.calf.bymo)
        summary(er.calf.bymo) #should be no NAs
                   
                
                
                
                
              
           