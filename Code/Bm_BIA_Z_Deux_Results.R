#Script Bm_BIA_Z_Deux_Results.r...Megan C. Ferguson...24 October 2020

# NOTES:
#   0. This script is based on Bm_BIA_Results_15km.r, v. 1 August 2020.
#   1. This script requires input from Bm_BIA_Optimization_Z_Deux.r and
#      Bm_BIA_Optimization_C1_Z_Deux.r that are saved to Bm_BIA_Optim_C1.Rdata and
#      Bm_BIA_Optim.Rdata.
#   2. Also requires shapefiles from CamdenBayBehavior.r with encounter rates for 
#      tx + caps sightings and effort combined:
#       Bm_FeedMill_Rate_ByMo_0019_pooled 
#       Bm_Calf_Rate_ByMo_1219_pooled


  library(ggplot2)
  library(sp)
  library(rgeos)
  library(maptools)
  library(rgdal)
  library(dichromat)

  #Input files from Bm_BIA_Optimization_Z_Deux.r and Bm_BIA_Optimization_C1_Z_Deux.r,
  #and manipulate some factors
  
    load(file="Output_BIA_Z_Deux/OutLists/Bm_BIA_Optim_C1.Rdata")
    smry.df1 <- smry.df

    load(file="Output_BIA_Z_Deux/OutLists/Bm_BIA_Optim.Rdata")
    
    smry.df <- rbind.data.frame(smry.df1, smry.df)
    
    #Create factors for c and z
      smry.df$c.fact <- as.factor(smry.df$c)
      smry.df$Z <- as.factor(smry.df$z)
      
    #Create word labels for months
      smry.df$Month <- rep(NA, nrow(smry.df))
      smry.df$Month[which(smry.df$m == 7)] <- "July"
      smry.df$Month[which(smry.df$m == 8)] <- "August"
      smry.df$Month[which(smry.df$m == 9)] <- "September"
      smry.df$Month[which(smry.df$m == 10)] <- "October"
      smry.df$Month <- factor(smry.df$Month, ordered=TRUE, 
                              levels=c("July", "August", "September", "October"))
      #Ck
        summary(smry.df)

    #Compute % nonzero cells per month and per fm.calf category
      C1 <- read.csv("Output_BIA_Z_Deux//ClusterSets//C1.csv")
      n.x.i <- nrow(C1)
      
      FM.pct <- sapply(1:length(FM.gt0), function(i){
        pct.i <- length(FM.gt0[[i]])/n.x.i
        return(pct.i)
      })
      
      calf.pct <- sapply(1:length(calf.gt0), function(i){
        pct.i <- length(calf.gt0[[i]])/n.x.i
        return(pct.i)
      })
      #CK
        n.x.i #should be 146
        FM.pct
        calf.pct

    #Compute "total number" of whales per month and per fm.calf category based on 
    # ***observed encounter rates***  
      
      #Input encounter rate data  
        FM.spdf <- readOGR(dsn="Output0019/Shapefiles", 
                             layer="Bm_FeedMill_Rate_ByMo_0019_pooled")
        
        calf.spdf <- readOGR(dsn="Output1219/Shapefiles", 
                             layer="Bm_Calf_Rate_ByMo_1219_pooled")

      #Area of one cell in km^2
         a.i <- C1$a[1] #km^2
        
      #FM.ind computes total number by cell by month. FM.ind.by.mo pools across cells
      #for each month.

         Month <- c("July", "August", "September", "October")
                  
         FM.ind <- sapply(1:4, function(m.idx){
           FM.ind.i=FM.spdf@data[,m.idx]*a.i
           return(FM.ind.i)
         })
         FM.ind.by.mo <- colSums(FM.ind)
         FM.ind.by.mo.df <- cbind.data.frame("ind"=FM.ind.by.mo, "Month"=Month)
         FM.ind.by.mo.df$Month <- factor(FM.ind.by.mo.df$Month, ordered=TRUE, 
                                  levels=c("July", "August", "September", "October"))

         calf.ind <- sapply(1:4, function(m.idx){
           calf.ind.i=calf.spdf@data[,m.idx]*a.i
           return(calf.ind.i)
         })
         calf.ind.by.mo <- colSums(calf.ind)
         calf.ind.by.mo.df <- cbind.data.frame("ind"=calf.ind.by.mo, "Month"=Month)
         calf.ind.by.mo.df$Month <- factor(calf.ind.by.mo.df$Month, ordered=TRUE, 
                                  levels=c("July", "August", "September", "October"))
         #CK
           length(FM.spdf)
           dim(FM.ind) #should be nrows=num hex cells in spdf X ncols=num months
           FM.ind.by.mo
           FM.ind.by.mo.df
           
           length(calf.spdf)
           dim(calf.ind) #should be nrows=num hex cells in spdf X ncols=num months
           calf.ind.by.mo
           calf.ind.by.mo.df

  #Plot objVal ~ sum.xi, by c and z
    
    #FM
    #Note: Some combinations of c X z X m for FM have objVal == 0 due to small number of
    #      occupied cells.
      fm9 <- smry.df[which(smry.df$fm.calf == "FM" & smry.df$m == 9),]
      fm <- smry.df[which(smry.df$fm.calf == "FM"),]
  
      ggplot(fm9,aes(sum.xi,objVal,colour=Z,shape=c.fact))+geom_point()+
        geom_hline(yintercept=FM.ind.by.mo[3])
      
      #dev.new()
      ggplot(fm9,aes(sum.xi,objVal,colour=Z,size=c))+geom_point()
  
      #dev.new()
      ggplot(fm,aes(sum.xi,objVal,colour=Z,shape=c.fact))+geom_point()+facet_grid(Month~.)
      
      #dev.new()
      ggplot(fm,aes(sum.xi,objVal,colour=Z,shape=c.fact))+geom_point()+facet_grid(.~Month)
      
      #dev.new()
      ggplot(fm,aes(sum.xi,objVal,colour=Z,size=c))+geom_point()+facet_grid(Month~.)
      
      #dev.new() 
      ggplot(fm,aes(sum.xi,objVal,colour=Z,size=c))+geom_point()+facet_grid(.~Month)

      dev.new() #preferred figure
      ggplot(fm,aes(sum.xi,objVal,colour=Z,size=c))+geom_point()+
        facet_grid(.~Month)+
        geom_hline(data=FM.ind.by.mo.df, aes(yintercept=ind))+
        xlab("\nNumber of Cells")+
        ylab("Number of Feeding and Milling Whales\n")+
        scale_size(name="Cluster Size")
      ggsave("Output_BIA_Z_Deux//Figures//Bm_BIA_FM_facet_size.png", width=8, height=6)
        #Print out percent cells occupied, for reference. In the figure, the percents
        #approx. correspond to the location where the C2 point hits the horizontal line
          FM.pct
          
      dev.new() #preferred figure
      ggplot(fm,aes(sum.xi,objVal,colour=Z,shape=c.fact))+geom_point()+
        geom_hline(data=FM.ind.by.mo.df, aes(yintercept=ind))+
        xlab("\nNumber of Cells")+
        ylab("Number of Feeding and Milling Whales\n")+
        scale_shape_manual(name="Cluster Size", values=c(10, 16,17,15,8))+
        facet_grid(.~Month)
      ggsave("Output_BIA_Z_Deux//Figures//Bm_BIA_FM_facet_shape.png", width=8, height=6)

      #dev.new()
      ggplot(fm,aes(sum.xi,objVal,colour=z,size=c))+geom_point()+facet_grid(.~Month)
  
    #calf 
    #Note: Some combinations of c X z X m for calf have objVal == 0 due to small number of
    #      occupied cells.
      calf9 <- smry.df[which(smry.df$fm.calf == "calf" & smry.df$m == 9),]
      calf <- smry.df[which(smry.df$fm.calf == "calf"),]
      
      #dev.new()
      ggplot(calf9,aes(sum.xi,objVal,colour=Z,shape=c.fact))+geom_point()
  
      #dev.new()
      ggplot(calf,aes(sum.xi,objVal,colour=Z,shape=c.fact))+geom_point()+facet_grid(Month~.)
      
      #dev.new() 
      ggplot(calf,aes(sum.xi,objVal,colour=Z,size=c))+geom_point()+facet_grid(.~Month)
      
      dev.new() #preferred figure
      ggplot(calf,aes(sum.xi,objVal,colour=Z,size=c))+geom_point()+
        geom_hline(data=calf.ind.by.mo.df, aes(yintercept=ind))+
        xlab("\nNumber of Cells")+
        ylab("Number of Calves\n")+
        scale_size(name="Cluster Size")+
        facet_grid(.~Month)
      ggsave("Output_BIA_Z_Deux//Figures//Bm_BIA_calves_facet_size.png", width=8, height=6)
        #Print out percent cells occupied, for reference. In the figure, the percents
        #approx. correspond to the location where the C2 point hits the horizontal line
          calf.pct
          
      dev.new() #preferred figure
      ggplot(calf,aes(sum.xi,objVal,colour=Z,shape=c.fact))+geom_point()+
        geom_hline(data=calf.ind.by.mo.df, aes(yintercept=ind))+
        xlab("\nNumber of Cells")+
        ylab("Number of Calves\n")+
        scale_size(name="Cluster Size")+
        scale_shape_manual(name="Cluster Size", values=c(10, 16,17,15,8))+
        facet_grid(.~Month)
      ggsave("Output_BIA_Z_Deux//Figures//Bm_BIA_calves_facet_shape.png", width=8, height=6)
          
      #dev.new()
      ggplot(calf,aes(sum.xi,objVal,colour=z,size=c))+geom_point()+facet_grid(.~Month)
  
      graphics.off()    
      
  #Plot maps per mo X fm.calf with gradient colors corresponding to encounter rate values,
  #one map per Z X C. Show color only in x.i=1. Use * in all maps in cells with d.i>0.
      
     #Input Round 1 BIA shapefiles
       bia.LL.spdf <- readOGR(dsn="Shapefiles", 
                             layer="CetMap_BIA_WGS84")
       bia.spdf <- spTransform(bia.LL.spdf, ECBproj6)
       #CK
         png(file="Output_BIA_Z_Deux//Figures//CetMap_Round1_Feeding_BIAs.png",
             height=540, width=1000, pointsize=30)
           par(mai=c(0.5,0.5,0.75,0.5))
           plot(buff.hexPols) 
           text(x=0, y=200000, labels="CetMap Round 1 Feeding BIAs")
           plot(bia.spdf[13,], add=TRUE, col="cyan") #Sep-Oct feeding
           plot(bia.spdf[44,], add=TRUE, col="blue") #Aug-Oct feeding
           plot(buff.hexPols, add=TRUE) 
           plot(shor.SP.ec, col="gray", add=TRUE)
         dev.off()   
         
         png(file="Output_BIA_Z_Deux//Figures//CetMap_Round1_Calves_JulAug_BIAs.png",
             height=540, width=1000, pointsize=30)
           par(mai=c(0.5,0.5,0.75,0.5))
           plot(buff.hexPols) 
           text(x=0, y=200000, labels="CetMap Round 1 July-August Calf BIAs")
           plot(bia.spdf[12,], add=TRUE, col="orchid") #Jul-Aug Calves
           plot(buff.hexPols, add=TRUE) 
           plot(shor.SP.ec, col="gray", add=TRUE)
         dev.off()   

         png(file="Output_BIA_Z_Deux//Figures//CetMap_Round1_Calves_Sep_BIAs.png",
             height=540, width=1000, pointsize=30)
           par(mai=c(0.5,0.5,0.75,0.5))
           plot(buff.hexPols) 
           text(x=0, y=200000, labels="CetMap Round 1 September Calf BIAs")
           plot(bia.spdf[8,], add=TRUE, col="thistle") #sep Calves
           plot(buff.hexPols, add=TRUE) 
           plot(shor.SP.ec, col="gray", add=TRUE)
         dev.off()   
         
         png(file="Output_BIA_Z_Deux//Figures//CetMap_Round1_Calves_Oct_BIAs.png",
             height=540, width=1000, pointsize=30)
           par(mai=c(0.5,0.5,0.75,0.5))
           plot(buff.hexPols) 
           text(x=0, y=200000, labels="CetMap Round 1 October Calf BIAs")
           plot(bia.spdf[6,], add=TRUE, col="purple") #Oct Calves
           plot(buff.hexPols, add=TRUE) 
           plot(shor.SP.ec, col="gray", add=TRUE)
         dev.off()   
         
     #Set up color gradients 
       fm.colfunc <- colorRampPalette(c("paleturquoise", "darkblue"))
       #CK
        fm.colfunc(10) 
        plot(rep(1,10),col=fm.colfunc(10),pch=19,cex=3)
        plot(rep(1,5),col=fm.colfunc(5),pch=19,cex=3)
       
       calf.colfunc <- colorRampPalette(c("lavender", "darkviolet"))
       #CK
        calf.colfunc(10) 
        plot(rep(1,10),col=calf.colfunc(10),pch=19,cex=3)
        plot(rep(1,5),col=calf.colfunc(5),pch=19,cex=3)
       
     #Make plots
       
       fm.calf <- c("FM", "calf") 
       Z.vec <- seq(from=0.1, to=1.2, by=0.1)
       mos <- 7:10
       Months <- c("July", "August", "September", "October")
       Activity <- c("Feeding and Milling", "Calves")
       #n.brks <- 5
       
       debugg <- FALSE
       #debugg <- TRUE
       if(debugg == TRUE){
         m.idx <- 3
         z.idx <- 5
         j <- 1
         c.idx <- 3
         n.brks <- 5
         spdf <- FM.spdf
         gt0 <- FM.gt0
         col.grdt <- fm.colfunc(5) 
       }
       
       for(c.idx in 1:5){
         
         for(m.idx in 1:4){
           
           for(z.idx in 1:length(Z.vec)){
             
             if(z.idx > 10 & c.idx == 1){
               print("No plots produced for c1 with z > 1.0")
             } else {
       
               for(j in 1:2){
                 
                 if(j == 1){
                   spdf <- FM.spdf
                   gt0 <- FM.gt0
                   col.grdt <- fm.colfunc(5)  
                 }else{
                   spdf <- calf.spdf
                   gt0 <- calf.gt0
                   col.grdt <- calf.colfunc(5)  
                 }
         
                 #Input solution
                   load(file=paste("Output_BIA_Z_Deux//OutLists//C",c.idx,"_Z",Z.vec[z.idx],
                       "_m",mos[m.idx],"_",fm.calf[j],".Rdata",sep=""))
                   
                 #Create categories based on non-zero entries of FM.spdf
                   idx0 <- which(spdf@data[,m.idx] > 0)
                   #h.m <- hist(spdf@data[idx0,m.idx], breaks=n.brks, plot=FALSE)
                   #int <- findInterval(spdf@data[idx0,m.idx], h.m$breaks)
                   q.m <- quantile(spdf@data[idx0,m.idx], probs=seq(0,1,0.2))
                   int <- findInterval(spdf@data[idx0,m.idx], q.m, rightmost.closed=TRUE)
          
                 #Output figure
                   png(file=paste("Output_BIA_Z_Deux//Figures//C",c.idx,"_Z",Z.vec[z.idx],
                                             "_m",mos[m.idx],"_",fm.calf[j],"_grdt.png",sep=""),
                                             height=540, width=1000, pointsize=30)
                     par(mai=c(0.5,0.5,0.75,0.5))
                     plot(spdf)
                     #text(x=0, y=200000, labels=paste(Months[m.idx], Activity[j], "Cluster Size", c.idx,
                     #           "Z=",z.idx, sep=" "))
                     text(x=0, y=180000, labels=paste(Months[m.idx], Activity[j]))
                     for(i in 1:length(hex.SPlist)){
                       if(i %in% idx0){
                         idx <- which(idx0 == i)
                         plot(hex.SPlist[[i]], col=col.grdt[int[idx]], add=TRUE)  
                         #Leave cells with d.i=0 blank
                       }   
                     }
                     
                     #Turn unselected x.i=0 white
                       wht <- out.list$x.in.sol$x.i[which(out.list$x.in.sol$value == 0)]
                       plot(buff.hexPols[wht,], col="white", add=TRUE)
                       
                     #Bold all selected x.i   
                       bld <- out.list$x.in.sol$x.i[which(out.list$x.in.sol$value > 0)]
                       plot(buff.hexPols[bld,], lwd=3, add=TRUE)
                       
                     #Create SPDF and output
                       dat <- data.frame(rep(0, length(buff.hexPols)))
                       dat[bld,1] <- 1
                       spdf.10 <- SpatialPolygonsDataFrame(buff.hexPols, dat, match.ID=FALSE)
                       writeOGR(spdf.10, "Output_BIA_Z_Deux//Shapefiles", 
                                paste("C",c.idx,"_Z",Z.vec[z.idx],
                                             "_m",mos[m.idx],"_",fm.calf[j],sep=""),
                                driver="ESRI Shapefile", overwrite_layer = TRUE)
                       
                     plot(buff.hexPts[gt0[[m.idx]],], pch="*", col="gray", add=TRUE) #* in cells with d.i>0
                     plot(shor.SP.ec, col="gray", add=TRUE)
    
                     text(x=-250000, y=-50000, labels=paste("Cluster Size ", c.idx, sep=""))
                     text(x=-250000, y=-75000, labels=paste("Z=",Z.vec[z.idx], sep=""))
                   dev.off()
           
               } #end j loop    
              
             } #end if c==1 & z > 0.9      
                 
           } #end z loop       
                 
         } #end m loop
         
       } #end c loop   
         
       
      
      
      
     
      
      
      
                   
  