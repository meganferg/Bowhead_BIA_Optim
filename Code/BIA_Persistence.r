#Script BIA_Persistence.r...Megan C. Ferguson...25 May 2021

  #Notes
  #
  # 1. This script inputs objects created by CamdenBayBehavior.r (v. 25 April 2020) and
  #    CamdenBayBehavior_15km.r (v. 4 July 2020).
  #
  # 2. Info on GLMMs in R: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
  #
  # 3. For online help with ggplot2, see https://ggplot2-book.org/guides.html or
  #    Google
  #
  # 4. This script builds LMs and GLMMs only for the 25-km resolution analysis. The
  #    proportional analysis is conducted for both the 25 km and 15 km analyses.
  #
  # 5. For the 15km resolution, there were cells with sightings but no effort due to 
  #    distance at which sighting was detected. Sightings were associated with cells 
  #    based on X/Ywhale, but effort was determined based on Lat and Long. I created 
  #    get.n.km.yrs.omit0() to acommodate this situation, but it means that the plots 
  #    will show cells with sightings but no effort.
  #
  # 6. The example GLMM case study analysis uses Bm_BIA_Optimization_Z_Deux.r results
  #    saved to 
  #C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior
  #   \\Output_BIA_Z_Deux\\OutLists\\C#_Z0.#_m#_***.rdata
  #
  # 7. GLMM case studies are specified in JTC_best_cluster_Z.csv based on 
  #    JTC_best_cluster_Z.pptx
  #
  # 8. For converting between IDs in shapefiles and R spatial objects, remember
  #    FID numbering begins with 0, whereas the r spatial objects begin indexing with 1.
  #    ****Add 1 to FID to get R spatial object index.****
  #
  # 9. See test_wow_modl.conv.r for detailed notes on assessing and interpreting glmer
  #    model convergence and singularity issues.

  library(sp)
  library(rgeos)
  library(maptools)
  library(rgdal)
  library(fields)
  library(lme4)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(broom)
  library(ggplot2)
  library(maps)
  library(STRbook)
  library(glmmTMB)
  library(MASS)
  library(mgcv)
  library(tidyverse)
  library(RColorBrewer)
  library(extrafont)
  
  #font_import()
  loadfonts(device = "win")
  #windowsFonts()
  
  #Designate 25 km or 15 km analysis
    #km25 <- TRUE
    #km25 <- FALSE
  
  #Should c.ind values be rounded to integer values in order to create cluster GLMMs?
    #round.c.ind <- FALSE
    #round.c.ind <- TRUE
  
    if(round.c.ind == TRUE){
      rnd <- "RND"
    }else{
      rnd <- ""
    }
  
  #Create new debugg.txt file for this session
    write("", file="Debugg//debugg.txt")
  
  #Define months and all.yrs
    mos <- 7:10
    all.yrs <- 2000:2019
  
  #Input encounter rate shapefiles 
    if(km25 == TRUE){
      fm.spdf <- readOGR(dsn="C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Output0019\\Shapefiles", 
                             layer="Bm_FeedMill_Rate_ByMo_0019_pooled")
        
      calf.spdf <- readOGR(dsn="C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Output1219\\Shapefiles", 
                             layer="Bm_Calf_Rate_ByMo_1219_pooled")
      #Ck
        dim(fm.spdf@data)
        summary(fm.spdf@data)
        plot(fm.spdf, col=rainbow(n=length(fm.spdf)))
        
        dim(calf.spdf@data)
        summary(calf.spdf@data)
        plot(calf.spdf, col=rainbow(n=length(calf.spdf)))
    } else {
      fm.spdf <- readOGR(dsn="C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Output0019_15km\\Shapefiles", 
                             layer="Bm_FeedMill_Rate_ByMo_0019_pooled")
        
      calf.spdf <- readOGR(dsn="C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Output1219_15km\\Shapefiles", 
                             layer="Bm_Calf_Rate_ByMo_1219_pooled")
      #Ck
        dim(fm.spdf@data)
        summary(fm.spdf@data)
        plot(fm.spdf, col=rainbow(n=length(fm.spdf)))
        
        dim(calf.spdf@data)
        summary(calf.spdf@data)
        plot(calf.spdf, col=rainbow(n=length(calf.spdf)))
    }
        
    #Extract number of cells
          
      n.hex <- length(fm.spdf)
      
    #Create a list of SpatialPolygons for individual cells. (This step might not be 
    #needed.)
        
      prj <- fm.spdf@proj4string
      
      hex.SPlist <- lapply(1:length(fm.spdf), function(i){
        p.i <- SpatialPolygons(list(fm.spdf@polygons[[i]]), proj4string=prj)
        return(p.i)
      })
      n.hex <- length(hex.SPlist)
      #CK
        n.hex #284
        class(hex.SPlist[[1]]) #SpatialPolygons
        
    #Import shoreline as SpatialPolygons object from GSHHS library and re-project
      shor.xlim <- c((360-158),(360-139.25))
      shor.ylim <- c(69.3,73)
      if (!rgeosStatus()) gpclibPermit()
      shor.SP <- Rgshhs("..//Data//gshhs_i.b",xlim=shor.xlim,ylim=shor.ylim, level=1)
      shor.SP.ec <- spTransform(shor.SP$SP, prj)
      #CK
        plot(shor.SP.ec)
        class(shor.SP.ec) #SpatialPolygons
        shor.poly <- tidy(shor.SP.ec) #fortify with broom::tidy
        ggplot(shor.poly, aes(long, lat, group=group)) +
          geom_polygon()

  #Input and wrangle effort data 
  
    #Path to effort summaries
      
      if(km25 == TRUE){
        km0019.path <- "C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Output0019"
      } else {
        km0019.path <- "C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Output0019_15km"
      }  
      
    #Create a single df with all fields in long format.
      km.by.moyr <- as.data.frame(matrix(rep(NA, n.hex*length(mos)*length(all.yrs)*4 ), ncol = 4))
      names(km.by.moyr) <- c("hexID", "Month", "Year", "km")    
      #Ck
        dim(km.by.moyr)
      
      debugg <- FALSE
      #debugg <- TRUE
      if(debugg == TRUE){
        j <- 4
      }
         
      for(j in 1:length(mos)){

        #Input effort summaries and replace NAs with 0
            
          load(paste(km0019.path, "\\m", mos[j], "txdfByYr.Rdata", sep="")) #m.tx.df.by.yr
        
          load(paste(km0019.path, "\\m", mos[j], "capsdfByYr.Rdata", sep="")) #m.caps.df.by.yr
          for(k in 1:dim(m.caps.df.by.yr)[2]) 
            m.caps.df.by.yr[is.na(m.caps.df.by.yr[,k]),k] <- 0
          #
          if(debugg == TRUE){
            print(dim(m.tx.df.by.yr))
            print(dim(m.caps.df.by.yr))
            print(names(m.tx.df.by.yr))
            print(names(m.caps.df.by.yr))
            print(summary(m.tx.df.by.yr))
            print(summary(m.caps.df.by.yr))
          }

          #Sum tx and caps effort; exclude TOTAL column
            df.by.yr <- m.tx.df.by.yr[,1:20]
            df.by.yr[,19] <- df.by.yr[,19] + m.caps.df.by.yr[,1]
            df.by.yr[,20] <- df.by.yr[,20] + m.caps.df.by.yr[,2]

          #Convert to long format. For the gather() function, Year is the key and km is the value.  
            df.by.yr$hexID <- 1:n.hex
            df.by.yr$Month <- mos[j]
            df.by.yr.long <- gather(df.by.yr, Year, km, -hexID, -Month)
            df.by.yr.long$Year <- as.numeric(df.by.yr.long$Year)
            #Ck 
              if(debugg == TRUE){
                print(summary(df.by.yr.long))
                print(dim(df.by.yr.long))
              }  
            
          #Store in km.by.moyr
            start.idx <- (j-1)*length(all.yrs)*n.hex + 1
            end.idx <- j*length(all.yrs)*n.hex
            km.by.moyr[start.idx:end.idx,] <- df.by.yr.long          

      } 
      #CK
        summary(km.by.moyr) #should be no NA entries

  #Sighting summaries
        
    if(km25 == TRUE){
      fm.sight.path <- "C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Output0019\\Sights"
      calf.sight.path <- "C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Output1219\\Sights"
    } else {
      fm.sight.path <- "C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Output0019_15km\\Sights"
      calf.sight.path <- "C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Output1219_15km\\Sights"
    }          
        
    debugg <- FALSE
    #debugg <- TRUE
    if(debugg == TRUE){
        yrs <- 2000:2019
        fm.calf <- "FeedMill"
        in.path <- fm.sight.path
        #yrs <- 2012:2019
        #fm.calf <- "Calf"
        #in.path <- calf.sight.path
        i <- 1
    }  
      
    input.sighting.smry <- function(fm.calf, yrs, in.path){  
      
      Bm.by.moyr <- as.data.frame(matrix(rep(NA, n.hex*length(mos)*length(yrs)*5 ), ncol = 5))
      names(Bm.by.moyr) <- c("hexID", "n", "ind", "Month", "Year")      
      #Ck
        if(debugg == TRUE){
          print(dim(Bm.by.moyr))
          print(names(Bm.by.moyr))
        }
      
      for(i in 1:length(yrs)){
        
        #Tx  
        
          load(paste(in.path, "\\nind_Bm_", fm.calf, yrs[i], ".Rdata", sep="")) #nind.Bm
          #Ck
            if(debugg == TRUE){
              print(class(nind.Bm)) #list
              print(length(nind.Bm)) #4
              print(str(nind.Bm)) #One list per month: 7, 8, 9, 10
              print(class(nind.Bm[[3]]$nBlks))
              print(class(nind.Bm[[3]]$indBlks))
              print(row.names(nind.Bm[[3]]$nBlks))
            }  
          #Convert to single dataframe in long format
          
          for(j in 1:length(mos)){
            start.idx <- (i-1)*4*n.hex + (j-1)*n.hex + 1
            end.idx <- start.idx + n.hex - 1
            Bm.by.moyr$hexID <- 1:n.hex
            Bm.by.moyr$n[start.idx:end.idx] <- nind.Bm[[j]]$nBlks[,1]
            Bm.by.moyr$ind[start.idx:end.idx] <- nind.Bm[[j]]$indBlks[,1]
            Bm.by.moyr$Month[start.idx:end.idx] <- mos[j]
            Bm.by.moyr$Year[start.idx:end.idx] <- yrs[i]
          }
        
        rm(nind.Bm)
          if(yrs[i] > 2017){    
        
          #CAPs    
            load(paste(in.path, "\\nind_Bm_", fm.calf, "_caps", yrs[i], ".Rdata", sep="")) #nind.Bm
            #Sum tx and caps sightings 
          
            for(j in 1:length(mos)){
              start.idx <- (i-1)*4*n.hex + (j-1)*n.hex + 1
              end.idx <- start.idx + n.hex - 1
              Bm.by.moyr$n[start.idx:end.idx] <- Bm.by.moyr$n[start.idx:end.idx] + nind.Bm[[j]]$nBlks[,1]
              Bm.by.moyr$ind[start.idx:end.idx] <- Bm.by.moyr$ind[start.idx:end.idx] + nind.Bm[[j]]$indBlks[,1]
            }
          
        } #end CAPs data manipulation
            
      } #end year i loop    
      
      return(Bm.by.moyr)
          
    } #end function     
      
    fm.Bm <- input.sighting.smry(fm.calf="FeedMill", yrs=2000:2019, in.path=fm.sight.path)
    calf.Bm <- input.sighting.smry(fm.calf="Calf", yrs=2012:2019, in.path=calf.sight.path)
    #Ck
        summary(fm.Bm) #No NA entries
        dim(fm.Bm)
        summary(calf.Bm) #No NA entries
        dim(calf.Bm)
        
  #Merge sightings and effort into a single dataframe
     
    #FM
        
      fm.hex.dat <- left_join(fm.Bm, km.by.moyr, 
                                     by=c('Year', 'Month', 'hexID'))
      #Ck
        summary(fm.hex.dat) #No NA entries
        dim(fm.hex.dat)
        fm.hex.dat[which(fm.hex.dat$Year == 2010 &
                      fm.hex.dat$Month == 9 &
                      fm.hex.dat$hexID ==172),]
        fm.Bm[which(fm.Bm$Year == 2010 &
                      fm.Bm$Month == 9 &
                      fm.Bm$hexID ==172),]
        km.by.moyr[which(km.by.moyr$Year == 2010 &
                      km.by.moyr$Month == 9 &
                      km.by.moyr$hexID ==172),]
    
    #Calf        
      calf.hex.dat <- left_join(calf.Bm, 
                                filter(km.by.moyr, (Year > 2011)), 
                                by=c('Year', 'Month', 'hexID'))
      #Ck
        summary(calf.hex.dat)
        dim(calf.hex.dat)
        calf.hex.dat[which(calf.hex.dat$Year == 2012 &
                      calf.hex.dat$Month == 7 &
                      calf.hex.dat$hexID ==70),]
        calf.Bm[which(calf.Bm$Year == 2012 &
                      calf.Bm$Month == 7 &
                      calf.Bm$hexID ==70),]
        km.by.moyr[which(km.by.moyr$Year == 2012 &
                      km.by.moyr$Month == 7 &
                      km.by.moyr$hexID ==70),]
    
        #Compare to encounter rates in fm.spdf and calf.spdf
        
          j <- 4 #month index in mos vector (manually change)
          
          #FM
          
            #Sum effort across years within hex
              fm.km.mo <- filter(km.by.moyr, Month == mos[j] &
                                             Year >= 2000 &
                                             Year <= 2019) 
              tot.fm.km.mo <- group_by(fm.km.mo, hexID) %>%
                              summarise(km = sum(km, na.rm=TRUE))
              summary(tot.fm.km.mo)
              dim(tot.fm.km.mo)
              
              fm.km.mo2 <- filter(fm.hex.dat, (Month == mos[j])) 
              tot.fm.km.mo2 <- group_by(fm.km.mo2, hexID) %>%
                              summarise(km = sum(km, na.rm=TRUE))
              summary(tot.fm.km.mo2)
              dim(tot.fm.km.mo2)
              
            #Sum Bm inds across years within hex  
              fm.inds.mo <- filter(fm.Bm, (Month == mos[j])) 
              tot.fm.inds.mo <- group_by(fm.inds.mo, hexID) %>%
                                summarise(ind = sum(ind, na.rm=TRUE))
              summary(tot.fm.inds.mo)
              dim(tot.fm.inds.mo)
              
              fm.inds.mo2 <- filter(fm.hex.dat, (Month == mos[j])) 
              tot.fm.inds.mo2 <- group_by(fm.inds.mo2, hexID) %>%
                                summarise(ind = sum(ind, na.rm=TRUE))
              summary(tot.fm.inds.mo2)
              dim(tot.fm.inds.mo2)
             
            #Compute encounter rate   
              fm.er.mo <- tot.fm.inds.mo$ind / tot.fm.km.mo$km
              summary(fm.er.mo - fm.spdf@data[,j]) #Ck below that the NAs correspond to zeros in fm.spdf
              summary(fm.spdf@data[,j])
              idx <- which(is.na(fm.er.mo)==TRUE)
              summary(fm.spdf@data[idx,j]) #should all be zero
              
              fm.er.mo2 <- tot.fm.inds.mo2$ind / tot.fm.km.mo2$km
              idx <- which(tot.fm.km.mo2$km > 0.0)
              summary(fm.er.mo2[idx] - fm.er.mo[idx]) #should all be zero
            
          #Calf
          
            #Sum effort across years within hex
              calf.km.mo <- filter(km.by.moyr, Month == mos[j] &
                                             Year >= 2012 &
                                             Year <= 2019) 
              tot.calf.km.mo <- group_by(calf.km.mo, hexID) %>%
                              summarise(km = sum(km, na.rm=TRUE))
              summary(tot.calf.km.mo)
              dim(tot.calf.km.mo)
              
              calf.km.mo2 <- filter(calf.hex.dat, (Month == mos[j])) 
              tot.calf.km.mo2 <- group_by(calf.km.mo2, hexID) %>%
                              summarise(km = sum(km, na.rm=TRUE))
              summary(tot.calf.km.mo2)
              dim(tot.calf.km.mo2)
              
            #Sum Bm inds across years within hex  
              calf.inds.mo <- filter(calf.Bm, (Month == mos[j])) 
              tot.calf.inds.mo <- group_by(calf.inds.mo, hexID) %>%
                                summarise(ind = sum(ind, na.rm=TRUE))
              summary(tot.calf.inds.mo)
              dim(tot.calf.inds.mo)
              
              calf.inds.mo2 <- filter(calf.hex.dat, (Month == mos[j])) 
              tot.calf.inds.mo2 <- group_by(calf.inds.mo2, hexID) %>%
                                summarise(ind = sum(ind, na.rm=TRUE))
              summary(tot.calf.inds.mo2)
              dim(tot.calf.inds.mo2)
             
            #Compute encounter rate   
              calf.er.mo <- tot.calf.inds.mo$ind / tot.calf.km.mo$km
              summary(calf.er.mo - calf.spdf@data[,j]) #Ck below that the NAs correspond to zeros in calf.spdf
              summary(calf.spdf@data[,j])
              idx <- which(is.na(calf.er.mo)==TRUE)
              summary(calf.spdf@data[idx,j]) #should all be zero
              
              calf.er.mo2 <- tot.calf.inds.mo2$ind / tot.calf.km.mo2$km
              idx <- which(tot.calf.km.mo2$km > 0.0)
              summary(calf.er.mo2[idx] - calf.er.mo[idx]) #should all be zero
          
  #Build models for every cell X month. First try lm, just to get it working. Then build
  #random effects model. See Wikle et al. (2019) Lab exercise 3.3 (p. 127) for an example.
  #
  #Use nest() to build a nested data frame, which is a data frame containing one row for
  #each group. The "nested" property comes from the fact that we may have a data frame,
  #conventionally under the field name "data," for each group.
  #
  #To carry out the function on each record in the nested data frame, use the function purrr::map()
  #from the package purrr. Results can be assigned to another column within nested data frame 
  #using mutate(). Use the function tidy() from the package broom to extract the key information 
  #from the model and put it into a data frame.
  #
  #In summary, the steps carried out below are: take the data, group by hex cell, 
  #create a nested data frame, fit a model to each cell, and extract a data 
  #frame containing information on the linear fit by cell.
     
    #Define function for fitting simple linear model as an easy (but statistically 
    #stupid test of the code)
      fit.one.hex.lm <- function(data) #fcn to fit lm at a single pixel
                          mod <- lm(ind ~ 1 + Year, data = data)
      
    #GLMMs
      
      #Define function to examine overdisperion, downloaded from
      #https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html.
      #the usual procedure of calculating the sum of squared Pearson 
      #residuals and comparing it to the residual degrees of freedom 
      #should give at least a crude idea of overdispersion. The following 
      #attempt counts each variance or covariance parameter as one model 
      #degree of freedom and presents the sum of squared Pearson residuals, 
      #the ratio of (SSQ residuals/rdf), the residual df, and the p-value 
      #based on the (approximately!!) appropriate chi-sq distribution.
        overdisp_fun <- function(model) {
            rdf <- df.residual(model)
            rp <- residuals(model,type="pearson")
            Pearson.chisq <- sum(rp^2)
            prat <- Pearson.chisq/rdf
            pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
            c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
        }
        
      if(km25 == TRUE){ 
        
        #Try building example GLMMs and GLM
          
          #Fake data
  
            set.seed(101)  
            d <- data.frame(y=rpois(1000,lambda=3),x=runif(1000),
                            f=factor(sample(1:10,size=1000,replace=TRUE)))
            m1 <- glmer(y~x+(1|f),data=d,family=poisson)
            overdisp_fun(m1)
            
          #Real data (I wrote this code before I created fm.hex.dat and calf.hex.dat. 
          #Also, the examples here are based on the 25-km cell resolution.)
            
            #Summarize number of whales
          
              inds.mo <- filter(fm.Bm, (Month == 9)) 
              tot.inds.mo <- group_by(inds.mo, hexID) %>%
                                  summarise(ind = sum(ind, na.rm=TRUE))
              hexID.no0 <- tot.inds.mo$hexID[which(tot.inds.mo$ind > 0)]
              
            #Sum effort across years within hex
                
              fm.km.mo <- filter(km.by.moyr, Month == 9 &
                                               Year >= 2000 &
                                               Year <= 2019) 
              tot.fm.km.mo <- group_by(fm.km.mo, hexID) %>%
                                summarise(km = sum(km, na.rm=TRUE))
              summary(tot.fm.km.mo)
              dim(tot.fm.km.mo)
            
            #Try specific hexID. 
            #  Note: hexID 4 has zero sightings in SEP across all years. Trying
            #        to use glmer with hexID == 4 results in errors.
  
              inds.mo.hex <- filter(inds.mo, (hexID == 172))
              summary(inds.mo.hex)
              dim(inds.mo.hex)
              
              km.mo.hex <- filter(fm.km.mo, (hexID == 172))
              summary(km.mo.hex)
              dim(km.mo.hex)
              
              #Merge inds and km into a single dataframe for use with glmer
                hex.dat <- left_join(inds.mo.hex, km.mo.hex, 
                                     by=c('Year', 'Month', 'hexID'))
                summary(hex.dat)
                dim(hex.dat)
                hex.dat[which(hex.dat$Year == 2010),]
                inds.mo.hex[which(inds.mo.hex$Year == 2010),]
                km.mo.hex[which(km.mo.hex$Year == 2010),]
                
              #Remove years with zero effort
                
                hex.dat <- filter(hex.dat, (km > 0.0))
              
            #Try building a few glmms
                
              #Poisson. Based on results from overdisp_fun, looks like
              #Poisson provides a good fit.
              
                m2 <- glmer(ind ~ 1 + (1|Year) + offset(log(km)),
                                    data=hex.dat, family=poisson) 
                summary(m2)
                overdisp_fun(m2)
                
                m3 <- glmer(ind ~ 1 + (1|Year), offset=log(km),
                                    data=hex.dat, family=poisson) 
                overdisp_fun(m3)
            
                m4 <- glmer(ind ~ 1 + (1|Year),
                                    data=hex.dat, family=poisson) 
                overdisp_fun(m4)
  
              #Negative Binomial
              #From GLMM FAQ: "Negative binomial models in glmmTMB and lognormal-Poisson 
              #models in glmer (or MCMCglmm) are probably the best quick alternatives 
              #for overdispersed count data. 
                
                #m5 doesn't converge
                  m5 <- glmer.nb(ind ~ 1 + (1|Year) + offset(log(km)),
                                  data=hex.dat, verbose=TRUE) 
              
                #Negative binomial distribution: quadratic parameterization 
                #(Hardin & Hilbe 2007). V=mu*(1+mu/phi) = mu+mu^2/phi.
                  m6 <- glmmTMB(ind ~ 1 + (1|Year) + offset(log(km)),
                                      data=hex.dat, family=nbinom2) 
                  summary(m6)
          
                #Negative binomial distribution: linear parameterization 
                #(Hardin & Hilbe 2007). V=mu*(1+phi)  
                  m7 <- glmmTMB(ind ~ 1 + (1|Year) + offset(log(km)),
                                      data=hex.dat, family=nbinom1) 
                  summary(m7)
              
            #GAM
              
              m8 <- gam(ind ~ s(Year) + offset(log(km)),
                        data=hex.dat, link=log, 
                        family=nb(), method="REML")
              summary(m8)
              plot(m8)
              
            #October fm models. The issue here is non-integer number of individuals in one cell.
            #That causes warnings when glmer tries to fit a Poisson model.
              
              mo.dat <- filter(fm.hex.dat, (Month == 10)) 
              tot.inds.mo <- group_by(mo.dat, hexID) %>%
                              summarise(ind = sum(ind, na.rm=TRUE))
              hexID.no0.ind <- tot.inds.mo$hexID[which(tot.inds.mo$ind > 0)]
              
              test <- filter(fm.hex.dat, (hexID %in% hexID.no0.ind)) %>%  #extract non-zero cells
                             filter(Month == 10 & km > 0.0) #only use records with survey effort in mo
              unique(test$ind) #Some cells with 1.125 inds (non-integer).
              idx <- which(test$ind == 1.125)
              test$hexID[idx] #hexID 225
  
              dat <- filter(fm.hex.dat, (hexID %in% hexID.no0.ind)) %>%  #extract non-zero cells
                             filter(Month == 10 & km > 0.0 & hexID == 225) 
              summary(dat)
              dat
              m9 <- glmer(ind ~ 1 + (1|Year) + offset(log(km)),
                                  data=dat, family=poisson)
              isSingular(m9)
              m9@optinfo$conv$lme4$conv
              m9@optinfo$conv$lme4$messages
              length(m9@optinfo$warnings)
              m9
              
              #Try changing the ind = 1.125 to 1.0 and fit to Poisson GLMM.
              #-->This makes it singular.
                new.dat <- dat
                new.dat$ind
                new.dat$ind[16] <- 1
                new.dat$ind #all integer
              
                m9.1 <- glmer(ind ~ 1 + (1|Year) + offset(log(km)),
                                    data=new.dat, family=poisson)
                isSingular(m9.1)
                m9.1@optinfo$conv$lme4$conv
                m9.1@optinfo$conv$lme4$messages
                length(m9.1@optinfo$warnings)
                m9.1
              
              #From mgcv::ldTweedie helpfile: 
              # "A Tweedie random variable with 1<p<2 is a sum of N gamma random variables 
              #  where N has a Poisson distribution. The p=1 case is a generalization of a 
              #  Poisson distribution and is a discrete distribution supported on integer 
              #  multiples of the scale parameter. For 1<p<2 the distribution is supported 
              #  on the positive reals with a point mass at zero. p=2 is a gamma distribution. 
              #  As p gets very close to 1 the continuous distribution begins to converge on 
              #  the discretely supported limit at p=1."
              
              m10 <- gam(ind ~ s(Year) + offset(log(km)),
                        data=hex.dat, link=log, 
                        family=tw(), method="REML")
              summary(m10)
              plot(m10)
              
      }        

      #Define function for fitting random effects generalized linear mixed effects model
      #with Poisson distribution.
        fit.one.hex.glmm <- function(dat) #fcn to fit GLMM at a single pixel
                            mod <- glmer(ind ~ 1 + (1|Year) + offset(log(km)),
                                  data=dat, family=poisson)
        
      #Define function to extract convergence code from lme4 object
        get.conv.lme4 <- function(modl){
          conv <- modl@optinfo$conv$lme4$code
          if(is.null(conv)) conv <- 1
          return(conv)
        }
        if(km25 == TRUE){
          get.conv.lme4(m4)
          get.conv.lme4(m9)
        }  
        
      #Define function to extract number of lme4 messages (warnings) from lme4 object
        get.n.msg.lme4 <- function(modl){
          n.msg <- length(modl@optinfo$conv$lme4$messages)
          return(n.msg)
        }
        if(km25 == TRUE){
          get.n.msg.lme4(m9)
          get.n.msg.lme4(m4)
        }
        
      #Define function to extract number of optimizer warnings from lme4 object
        get.n.opt.lme4 <- function(modl){
          n.opt <- length(modl@optinfo$warnings)
          return(n.opt)
        }
        if(km25 == TRUE){
          get.n.opt.lme4(m9)
          get.n.opt.lme4(m4)
        }
        
      #Define function to extract number of years with sightings from data
        get.n.Bm.yrs <- function(data){
          n.yrs <- length(which(data$ind > 0))
          return(n.yrs)
        }
        
      #Define function to extract number of years with effort from data
        get.n.km.yrs <- function(data){
          n.yrs <- nrow(data)
          return(n.yrs)
        }
        
        get.n.km.yrs.omit0 <- function(data){
          no0.km <- filter(data, km > 0.000001)
          n.yrs <- nrow(no0.km)
          return(n.yrs)
        }

      #Create function to extract relevant glmerMod parameters. (No tidy method
      #for objects of class glmerMod.) This function is specific to the GLMM
      #formulation specified in fit.one.hex.glmm.
              
        get.params.lme4 <- function(modl){
          fixed.int <- summary(modl)$coefficients[1] #Fixed Effects Intercept
          se.fixed.int <- summary(modl)$coefficients[2] #SE of Fixed Effects Intercept
          Year.sd <- attributes(summary(modl)$varcor[[1]])$stddev #SD Year Rdm Effect
          resid.sd <- summary(modl)$sigma #residual error SD
          out.df <- cbind.data.frame("fixed.int"=fixed.int,
                                     "se.fixed.int"=se.fixed.int,
                       "Year.sd"=as.vector(Year.sd),
                       "resid.sd"=resid.sd)
          return(out.df)
        }
        #Ck
        if(km25 == TRUE){
          test <- get.params.lme4(m4)
          test
          dim(test) # 1 x 4
          names(test) #"fixed.int" "se.fixed.int" "Year.sd"   "resid.sd"
          
          get.params.lme4(m9)
        }  

    #For 25 km resolution, define function for building models and plotting
        
      if(km25 == TRUE){  
     
        debugg <- FALSE
        #debugg <- TRUE
        if(debugg == TRUE){
          #dat <- fm.Bm
          dat <- fm.hex.dat
          mo <- mos[3]
          #mod.fcn <- fit.one.hex.lm
          mod.fcn <- fit.one.hex.glmm
          LM <- FALSE
          fnam <- "test"
          spdf <- fm.spdf
          fm.calf <- "FeedMill"
          #fm.calf <- "Calf"
        }
  
        fit.and.plot <- function(dat, mo, mod.fcn, fnam, spdf, LM=FALSE, fm.calf){ 
        
          #Sum Bm inds across years within hex  
          
            mo.dat <- filter(dat, (Month == mo)) 
            tot.inds.mo <- group_by(mo.dat, hexID) %>%
                              summarise(ind = sum(ind, na.rm=TRUE))
            if(debugg == TRUE){
              print(summary(tot.inds.mo))
              print(dim(tot.inds.mo))
            }
            
          #Identify which hex cells have nonzero encounters (pooled across all years)
            hexID.no0.ind <- tot.inds.mo$hexID[which(tot.inds.mo$ind > 0)]
  
          #Build models
            
            if(LM == TRUE){
              #hex.modl <- filter(dat, (hexID %in% hexID.no0.ind)) %>%  #extract non-zero cells
              hex.modl <- dat %>%  #take the data
                             filter(Month == mo) %>%
                             group_by(hexID)  %>%    #group by cell
                             nest() %>%              #create a nested data frame
                             mutate(model = purrr::map(data, mod.fcn)) %>% #fit a model to each cell
                             mutate(model_df = purrr::map(model, tidy)) #extract a df w info on the linear fit by cell
              #Ck 
                if(debugg == TRUE)
                  print(hex.modl %>% head(3)) #examine only the first three rows
  
              #Extract model params for simple linear models
                
                modl.pars <- hex.modl %>% unnest(model_df)
                modl.pars$id <- as.character(modl.pars$hexID - 1) #To match the tidy id created below
                #CK
                  if(debugg == TRUE)
                    print(modl.pars %>% filter(hexID %in% hexID.no0.ind)) #Examine only results for non-zero cells
  
            } else {
              hex.modl <- filter(dat, (hexID %in% hexID.no0.ind)) %>%  #extract non-zero cells
                             filter(Month == mo & km > 0.0) %>% #only use records with survey effort in mo
                             group_by(hexID)  %>%    #group by cell
                             nest() %>%              #create a nested data frame
                             mutate(model = purrr::map(data, mod.fcn)) #fit a model to each cell
              
              if(debugg == TRUE){ 
                
                #What to do with models that fail to converge or are singular?
                #See Bolker's glmmFAQ on convergence and singularity for info.
    
                #optinfo$warnings doesn't save info on singularity or convergence issues:
                  for(i in 1:nrow(hex.modl)) print(hex.modl$model[[i]]@optinfo$warnings)
                 
                #Figure out how the output from the purrr::map() function corresponds to
                #the output from sequential calls to mod.fcn
                  test.list <- list()
                  for(x in 1:length(hexID.no0.ind)){
                    dat.x <- filter(dat, (hexID == hexID.no0.ind[x])) %>%
                             filter(Month == mo & km > 0.0)
                    #summary(dat.x)
                    test <- mod.fcn(dat.x)
                    test.list <- c(test.list, test)
                  }  
                  length(test.list)
                  length(hexID.no0.ind)  #same as length(test.list)
                
                #Identify singular models from the list
                  for(x in 1:length(test.list))
                    print(paste(x, isSingular(test.list[[x]]), sep="_"))
                
                #Note that hexID.no0.ind is not in the same order as hex.modl$hexID
                  hexID.no0.ind
                  hex.modl$hexID
                  sum(hexID.no0.ind != hex.modl$hexID)
                  #Put hex.modl in ascending hexID order
                    test <- hex.modl[order(hex.modl$hexID),]
                    test$hexID
                    sum(hexID.no0.ind != test$hexID)
                    
                #Look at convergence stuff for all models in the tbl
                  for(x in 1:nrow(test)){
                    print(paste(x,"hexID",hexID.no0.ind[x],sep="_"))
                    print(test$model[[x]]@optinfo$conv)
                  }  
                
                #Inspect the model for one cell in depth    
                  idx <- 39 #idx is NOT the hexID; this is the for extracting models from
                            #test.list and test. Corresponds to hexID = hexID.no0.ind[idx]
                  test.list[[idx]]
                  isSingular(test.list[[idx]]) #TRUE if singular
                  test$hexID[idx]
                  #test[idx,]
                  #test$data[idx]
                  #test$model[idx]
                  test$model[[idx]]
                  isSingular(test$model[[idx]]) #TRUE if singular
                  #test$model[[idx]]@optinfo
                  test$model[[idx]]@optinfo$conv
                  test$model[[idx]]@optinfo$conv$lme4$code #-1 if no convergence
                                                           #NULL if singular
                                                           #NULL if converged
                  is.null(test$model[[idx]]@optinfo$conv$lme4$code)
                  test$model[[idx]]@optinfo$warnings #not helpful
                  
              }  #end of debugging intermission
              
              #Put hex.modl in ascending hexID order
              
                sum(hexID.no0.ind != hex.modl$hexID) #Will be > 0
              
                hex.modl <- hex.modl[order(hex.modl$hexID),]
                #Ck
                  if(debugg == TRUE){
                    hex.modl$hexID
                    sum(hexID.no0.ind != hex.modl$hexID) #Zero
                  }  
                
              #Add columns for singularity, convergence, # lme4 warnings, and # optimization 
              #warnings. 
              #  sing: Singular = TRUE or FALSE. 
              #        From Bolker's glmmFAQ website:
              #           It is very common for overfitted mixed models to result 
              #           in singular fits. Technically, singularity means that some 
              #           of the variance-covariance Cholesky decomposition 
              #           parameters corresponding to diagonal elements of the 
              #           Cholesky factor are exactly zero, which is the edge of the 
              #           feasible space, or equivalently that the variance-covariance
              #           matrix has some zero eigenvalues (i.e. is positive 
              #           semidefinite rather than positive definite), or (almost 
              #           equivalently) that some of the variances are estimated as 
              #           zero or some of the correlations are estimated as +/-1. 
              #           This commonly occurs in two scenarios:
              #           1. small numbers of random-effect levels (e.g. <5).
              #           2. complex random-effects models, e.g. models of the form 
              #              (f|g) where f is a categorical variable with a relatively
              #              large number of levels, or models with several different 
              #              random-slopes terms.
              #  conv: -1 if no convergence; 1 if converged. Can be singular and
              #        still converge.
                
                hex.modl <- mutate(hex.modl, sing = unlist(purrr::map(model, isSingular))) %>%
                            mutate(conv = unlist(purrr::map(model, get.conv.lme4))) %>%
                            mutate(n.lme4.msg = unlist(purrr::map(model, get.n.msg.lme4))) %>%
                            mutate(n.opt.warn = unlist(purrr::map(model, get.n.opt.lme4)))
                #Ck 
                  if(debugg == TRUE){
                    print(class(hex.modl))
                    print(head(hex.modl))
                    print(summary(hex.modl$sing))
                    print(summary(hex.modl$conv))
                    print(summary(hex.modl$n.lme4.msg))
                    print(length(which(hex.modl$n.lme4.msg > 0)))
                    print(summary(hex.modl$n.opt.warn))
                    print(length(which(hex.modl$n.opt.warn > 0)))
                  }
                
              #Add columns for number of years with sightings, number of years with effort,
              #and proportion of years with sightings
                
                hex.modl <- mutate(hex.modl, n.Bm.yrs = unlist(purrr::map(data, get.n.Bm.yrs))) %>%
                            mutate(n.km.yrs = unlist(purrr::map(data, get.n.km.yrs))) %>% 
                            mutate(p.Bm.yrs = n.Bm.yrs/n.km.yrs)
                if(debugg == TRUE){
                  print(hex.modl$n.Bm.yrs)
                  print(hex.modl$n.km.yrs)
                  print(hex.modl$p.Bm.yrs)
                }
                
              #Extract model params for glmms
                
                if(debugg == TRUE){
                  hex.modl$model[[1]]
                  class(hex.modl$model[[1]])
                  summary(hex.modl$model[[1]])$coefficients[1] #Fixed Effects Intercept
                  attributes(summary(hex.modl$model[[1]])$varcor[[1]])$stddev #SD Year Rdm Effect
                  summary(hex.modl$model[[1]])$sigma #residual error SD
                  hex.modl$hexID[1]
                }  
                
                modl.pars <- purrr::map(hex.modl$model, get.params.lme4) %>% #extract params
                             do.call("rbind",.) %>% #convert list to data.frame
                             mutate(hexID = hex.modl$hexID) %>% #add hexID
                             mutate(id = as.character(hexID - 1)) #To match the tidy id created below
                #CK
                  if(debugg == TRUE){
                    print(class(modl.pars))
                    print(head(modl.pars, 3))
                    print(summary(modl.pars))
                    print(sum(modl.pars$hexID - hexID.no0.ind))
                  } 
            } #End of hex.modl creation for glmm  
  
            #Plot spatial maps.
            #  *For lm, plot Intercept, regression coeff assoc. with Year, and
            #   SE associated with Year.
            #  *For glmm, plot Fixed Intercept, SD for Random Effect Intercept for Year,
            #   and residual SD.
  
              #Prep data for plotting with ggplot2
                
                spdf@data$id <- as.character(1:n.hex - 1) #To match the tidy id created below     
                tidy.spdf <- tidy(spdf) #fortify spdf with broom::tidy
                modl.pars <- left_join(tidy.spdf, modl.pars, by='id') #merge spdf and modl.pars
                #CK
                  if(debugg == TRUE){
                    modl.pars %>% filter(hexID %in% hexID.no0.ind)
                    class(modl.pars)
                    names(modl.pars)
                  }  
                    
              #Plot      
                
                if(debugg == TRUE){  
                  test.plot <- ggplot(modl.pars, aes(x = long, y = lat, group=id)) + 
                                      geom_polygon()
                  test.plot
                }
                
                if(LM == TRUE) {#Plot simple linear model
                  
                  plot.int <- ggplot(filter(modl.pars, term == "(Intercept)")) +
                    geom_polygon(aes(long, lat, fill = estimate, group=id)) +
                    #col_scale() +
                    #fill_scale(name = "Intercept") +
                    labs(fill = "Intercept") +
                    theme_bw() + coord_fixed()
                  ggsave(filename = paste("Figures//",fnam,"_m", mo, "_Int.png",sep=""),
                         device = "png", 
                         width = 14, height = 5.5, units = "cm", 
                         plot = plot.int)
    
                  plot.yr <- ggplot(filter(modl.pars, term == "Year")) +
                    geom_polygon(aes(long, lat, fill = estimate, group=id)) +
                    #col_scale() +
                    #fill_scale(name = "Year") +
                    labs(fill = "Year") +
                    theme_bw() + coord_fixed()
                  ggsave(filename = paste("Figures//",fnam,"_m", mo, "_Year_Est.png",sep=""),
                         device = "png", 
                         width = 14, height = 5.5, units = "cm", 
                         plot = plot.yr)
                  
                  plot.yr.se <- ggplot(filter(modl.pars, term == "Year")) +
                    geom_polygon(aes(long, lat, fill = std.error, group=id)) +
                    #col_scale() +
                    #fill_scale(name = "SE(Year)") +
                    labs(fill = "SE(Year)") +
                    theme_bw() + coord_fixed()
                  ggsave(filename = paste("Figures//",fnam,"_m", mo, "_Year_SE.png",sep=""),
                         device = "png", 
                         width = 14, height = 5.5, units = "cm", 
                         plot = plot.yr.se)
                
                } else { #Plot GLMM
                  
                  plot.int <- ggplot(modl.pars) +
                    geom_polygon(aes(long, lat, fill = fixed.int, group=id)) +
                    #col_scale() +
                    #fill_scale(name = "Fixed Intercept") +
                    labs(fill = "Fixed Intercept") +
                    theme_bw() + coord_fixed()
                  ggsave(filename = paste("Figures//",fnam,"_m", mo, "_FixedInt.png",sep=""),
                         device = "png", 
                         width = 14, height = 5.5, units = "cm", 
                         plot = plot.int)
                  
                  plot.yr.sd <- ggplot(modl.pars) +
                    geom_polygon(aes(long, lat, fill = Year.sd, group=id)) +
                    #col_scale() +
                    #fill_scale(name = "SD Year Random Effect") +
                    labs(fill = "SD Year Random Effect") +
                    theme_bw() + coord_fixed()
                  ggsave(filename = paste("Figures//",fnam,"_m", mo, "_Year_SD_Rdm.png",sep=""),
                         device = "png", 
                         width = 14, height = 5.5, units = "cm", 
                         plot = plot.yr.sd)
                  
                  plot.resid.sd <- ggplot(modl.pars) +
                    geom_polygon(aes(long, lat, fill = resid.sd, group=id)) +
                    #col_scale() +
                    #fill_scale(name = "SD Residual Error") +
                    labs(fill = "SD Residual Error") +
                    theme_bw() + coord_fixed()
                  ggsave(filename = paste("Figures//",fnam,"_m", mo, "_Resid_SD.png",sep=""),
                         device = "png", 
                         width = 14, height = 5.5, units = "cm", 
                         plot = plot.resid.sd)
                  
                  #Plot number of Bm years, number of km years, and proportion of Bm years
                  
                    #Prep data for plotting with ggplot2
                     
                      hex.modl <- mutate(hex.modl, id = as.character(hexID - 1)) #To match the tidy id created below
                      hex.modl <- left_join(tidy.spdf, hex.modl, by='id') #merge spdf and hex.modl
                      hex.modl.noNA <- filter(hex.modl, !is.na(hexID)) #Exclude cells without Bm
                      
                    #Standardize plotting variables
                      
                      n.Bm.yrs.colors = c("1" = "salmon", "2" = "yellow3", 
                                          "3" = "mediumseagreen", "4" = "mediumturquoise",
                                          "5" = "mediumorchid")
      
                      my.palette <- colorRampPalette(brewer.pal(11, "Spectral"))
                      
                      if(fm.calf == "FeedMill"){
                        n.km.fill.scale <- scale_fill_gradientn(colours = my.palette(100), 
                                                                limits=c(1, 20)) #for fm
                      } else {  
                        n.km.fill.scale <- scale_fill_gradientn(colours = my.palette(100), 
                                                                limits=c(1, 8)) #for calves
                      }  
                      
                      p.Bm.fill.scale <- scale_fill_gradientn(colours = my.palette(100), 
                                                                limits=c(0, 1.0)) #probability
                      
                      #my.theme <- theme(plot.margin = margin(0, 0, 0, 0, "cm")) 
  
                      if(debugg == TRUE){ #Experiment with plotting
                        
                        #Include NA in legend
                        test.plot0 <- ggplot(hex.modl) +
                          geom_polygon(aes(long, lat, fill = factor(n.Bm.yrs), group=id)) +
                          labs(fill = "Number of\nBowhead Years") +
                          theme_bw() + coord_fixed()
                        test.plot0
                      
                        #Don't plot NA cells
                        test.plot1 <- ggplot(hex.modl.noNA) +
                          geom_polygon(aes(long, lat, fill = factor(n.Bm.yrs), group=id)) +
                          labs(fill = "Number of\nBowhead Years") +
                          theme_bw() + coord_fixed()
                        test.plot1
                        
                        #Don't plot NA cells
                        test.plot2 <- ggplot(hex.modl) +
                          geom_polygon(data = hex.modl.noNA, aes(long, lat, fill = factor(n.Bm.yrs), group=id)) +
                          labs(fill = "Number of\nBowhead Years") +
                          theme_bw() + coord_fixed()
                        test.plot2
                        
                        #Default fill for NA cells
                        test.plot3 <- ggplot(hex.modl) +
                          geom_polygon(aes(long, lat, fill = NA, group=id)) +
                          geom_polygon(data = hex.modl.noNA, aes(long, lat, fill = factor(n.Bm.yrs), group=id)) +
                          labs(fill = "Number of\nBowhead Years") +
                          theme_bw() + coord_fixed()
                        test.plot3
                        
                        #Black cell outlines underneath
                        test.plot4 <- ggplot(hex.modl) +
                          geom_polygon(aes(long, lat, group=id), fill = NA, colour = "black") +
                          geom_polygon(data = hex.modl.noNA, aes(long, lat, fill = factor(n.Bm.yrs), group=id)) +
                          labs(fill = "Number of\nBowhead Years") +
                          theme_bw() + coord_fixed()
                        test.plot4
                        
                        #Black cell outlines on top
                        test.plot5 <- ggplot(hex.modl) +
                          geom_polygon(data = hex.modl.noNA, aes(long, lat, fill = factor(n.Bm.yrs), group=id)) +
                          geom_polygon(aes(long, lat, group=id), fill = NA, colour = "black") +
                          labs(fill = "Number of\nBowhead Years") +
                          theme_bw() + coord_fixed()
                        test.plot5
                        
                        #Change axis labels to x and y 
                        test.plot6 <- ggplot(hex.modl) +
                          geom_polygon(data = hex.modl.noNA, aes(long, lat, fill = factor(n.Bm.yrs), group=id)) +
                          geom_polygon(aes(long, lat, group=id), fill = NA, colour = "black") +
                          labs(fill = "Number of\nBowhead Years") +
                          theme_bw() + coord_fixed() +
                          xlab("x") +
                          ylab("y")
                        test.plot6
                        
                        #Standardize number of years. Use n.Bm.yrs.colors defined above.
                        test.plot7 <- ggplot(hex.modl) +
                          geom_polygon(data = hex.modl.noNA, aes(long, lat, 
                                                                 fill = factor(n.Bm.yrs), 
                                                                 group=id)) +
                          scale_fill_manual(values = n.Bm.yrs.colors) +
                          geom_polygon(aes(long, lat, group=id), fill = NA, colour = "black") +
                          labs(fill = "Number of\nBowhead Years") +
                          theme_bw() + coord_fixed() +
                          xlab("x") +
                          ylab("y")
                        test.plot7
                        
                        #Standardize number of years. Use n.km.yrs.palette defined above.
                        test.plot8 <- ggplot(hex.modl) +
                          geom_polygon(data = hex.modl.noNA, aes(long, lat, 
                                                                 fill = n.km.yrs, 
                                                                 group = id)) + 
                          n.km.fill.scale +
                          geom_polygon(aes(long, lat, group=id), fill = NA, colour = "black") +
                          labs(fill = "Number of\nSurvey Years") +
                          theme_bw() + coord_fixed() +
                          xlab("x") +
                          ylab("y")
                        test.plot8
                        
                        #Narrow figure margins.
                        test.plot9 <- ggplot(hex.modl) +
                          geom_polygon(data = hex.modl.noNA, aes(long, lat, 
                                                                 fill = n.km.yrs, 
                                                                 group = id)) + 
                          n.km.fill.scale +
                          geom_polygon(aes(long, lat, group=id), fill = NA, colour = "black") +
                          labs(fill = "Number of\nSurvey Years") +
                          coord_fixed() +
                          xlab("x") +
                          ylab("y") 
                        ggsave(filename = paste("Figures//",fnam,"_m", mo, "_n_km_yrs.png",sep=""),
                               device = "png", 
                               width = 14, height = 5.5, units = "cm",
                               plot = test.plot9)
  
                      }
  
                    #Plot and save
                      
                      plot.n.Bm.yrs <- ggplot(hex.modl) +
                          geom_polygon(data = hex.modl.noNA, aes(long, lat, 
                                                                 fill = factor(n.Bm.yrs), 
                                                                 group=id)) +
                          scale_fill_manual(values = n.Bm.yrs.colors) +
                          geom_polygon(aes(long, lat, group=id), fill = NA, colour = "black") +
                          geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group)) +
                          labs(fill = "Number of\nBowhead Years") +
                          theme_bw() + coord_fixed() +
                          xlab("x") +
                          ylab("y")
                      ggsave(filename = paste("Figures//",fnam,"_m", mo, "_n_Bm_yrs.png",sep=""),
                             device = "png", 
                             width = 14, height = 5.5, units = "cm", 
                             plot = plot.n.Bm.yrs)
                                      
                      plot.n.km.yrs <- ggplot(hex.modl) +
                          geom_polygon(data = hex.modl.noNA, aes(long, lat, 
                                                                 fill = n.km.yrs, 
                                                                 group = id)) + 
                          n.km.fill.scale +
                          geom_polygon(aes(long, lat, group=id), fill = NA, colour = "black") +
                          geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group)) +
                          labs(fill = "Number of\nSurvey Years") +
                          theme_bw() + coord_fixed() +
                          xlab("x") +
                          ylab("y")
                      ggsave(filename = paste("Figures//",fnam,"_m", mo, "_n_km_yrs.png",sep=""),
                             device = "png", 
                             width = 14, height = 5.5, units = "cm", 
                             plot = plot.n.km.yrs)
                  
                      plot.p.Bm.yrs <- ggplot(hex.modl) +
                        geom_polygon(data = hex.modl.noNA, aes(long, lat, 
                                                               fill = p.Bm.yrs, 
                                                               group=id)) +
                        p.Bm.fill.scale +
                        geom_polygon(aes(long, lat, group=id), fill = NA, colour = "black") +
                        geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group)) +
                        labs(fill = "Proportion of\nBowhead Years") +
                        theme_bw() + coord_fixed() 
                      ggsave(filename = paste("Figures//",fnam,"_m", mo, "_p_Bm_yrs.png",sep=""),
                             device = "png", 
                             width = 14, height = 5.5, units = "cm", 
                             plot = plot.p.Bm.yrs)
                      
                }
                
            #Save modl.pars and hex.modl
              save(file = paste("Output//",fnam,"_m",mo,".Rdata", sep=""), modl.pars, hex.modl)
                
        }        
                     
        #Run fit.and.plot
          
          #FM
          
           #LM
          
             fit.and.plot(dat=fm.Bm, mo=7, mod.fcn=fit.one.hex.lm, fnam="lm_FM", 
                          spdf=fm.spdf, LM=TRUE, fm.calf="FeedMill") 
             fit.and.plot(dat=fm.Bm, mo=8, mod.fcn=fit.one.hex.lm, fnam="lm_FM", 
                          spdf=fm.spdf, LM=TRUE, fm.calf="FeedMill")
             fit.and.plot(dat=fm.Bm, mo=9, mod.fcn=fit.one.hex.lm, fnam="lm_FM", 
                          spdf=fm.spdf, LM=TRUE, fm.calf="FeedMill")
             fit.and.plot(dat=fm.Bm, mo=10, mod.fcn=fit.one.hex.lm, fnam="lm_FM", 
                          spdf=fm.spdf, LM=TRUE, fm.calf="FeedMill")
             
           #GLMM   
           
             fit.and.plot(dat=fm.hex.dat, mo=7, mod.fcn=fit.one.hex.glmm, fnam="glmm_FM", 
                          spdf=fm.spdf, LM=FALSE, fm.calf="FeedMill") 
             fit.and.plot(dat=fm.hex.dat, mo=8, mod.fcn=fit.one.hex.glmm, fnam="glmm_FM", 
                          spdf=fm.spdf, LM=FALSE, fm.calf="FeedMill")
             fit.and.plot(dat=fm.hex.dat, mo=9, mod.fcn=fit.one.hex.glmm, fnam="glmm_FM", 
                          spdf=fm.spdf, LM=FALSE, fm.calf="FeedMill")
             fit.and.plot(dat=fm.hex.dat, mo=10, mod.fcn=fit.one.hex.glmm, fnam="glmm_FM", 
                          spdf=fm.spdf, LM=FALSE, fm.calf="FeedMill")
              
          #Calf
             
           #LM   
          
             fit.and.plot(dat=calf.Bm, mo=7, mod.fcn=fit.one.hex.lm, fnam="lm_Calf", 
                          spdf=calf.spdf, LM=TRUE, fm.calf="Calf") 
             fit.and.plot(dat=calf.Bm, mo=8, mod.fcn=fit.one.hex.lm, fnam="lm_Calf", 
                          spdf=calf.spdf, LM=TRUE, fm.calf="Calf")
             fit.and.plot(dat=calf.Bm, mo=9, mod.fcn=fit.one.hex.lm, fnam="lm_Calf", 
                          spdf=calf.spdf, LM=TRUE, fm.calf="Calf")
             fit.and.plot(dat=calf.Bm, mo=10, mod.fcn=fit.one.hex.lm, fnam="lm_Calf", 
                          spdf=calf.spdf, LM=TRUE, fm.calf="Calf")
            
           #GLMM
             
             fit.and.plot(dat=calf.hex.dat, mo=7, mod.fcn=fit.one.hex.glmm, fnam="glmm_Calf", 
                          spdf=calf.spdf, LM=FALSE, fm.calf="Calf") 
             fit.and.plot(dat=calf.hex.dat, mo=8, mod.fcn=fit.one.hex.glmm, fnam="glmm_Calf", 
                          spdf=calf.spdf, LM=FALSE, fm.calf="Calf")
             fit.and.plot(dat=calf.hex.dat, mo=9, mod.fcn=fit.one.hex.glmm, fnam="glmm_Calf", 
                          spdf=calf.spdf, LM=FALSE, fm.calf="Calf")
             fit.and.plot(dat=calf.hex.dat, mo=10, mod.fcn=fit.one.hex.glmm, fnam="glmm_Calf", 
                          spdf=calf.spdf, LM=FALSE, fm.calf="Calf")
                  
        #Examine frequency of singularity, sample size, convergence, and warning issues
    
          debugg <- FALSE   
          #debugg <- TRUE
          if(debugg == TRUE){
            fnam <- "glmm_Calf_m"
            mo <- 9
          }
    
          hex.modl.smry <- function(fnam, mo, debugg){   
            
            load(paste("Output//",fnam,mo,".Rdata",sep=""))
            
            n.hex <- n_distinct(hex.modl$hexID, na.rm = TRUE) #Number of occupied cells
            
            no.conv <- filter(hex.modl, conv < 0) 
            n.no.conv <- n_distinct(no.conv$hexID) #number models that didn't converge
            
            bad.sing <- filter(hex.modl, sing == TRUE)
            n.sing <- n_distinct(bad.sing$hexID) #number singular models 
            
            lme4.msg <- filter(hex.modl, n.lme4.msg > 0) 
            n.lme4.msg <- n_distinct(lme4.msg$hexID) #number models with lme4 warnings
      
            opt.warn <- filter(hex.modl, n.opt.warn > 0) 
            n.opt.warn <- n_distinct(opt.warn$hexID) #number models with opt warnings
            
            Bm1 <- filter(hex.modl, n.Bm.yrs == 1)
            n.Bm1 <- n_distinct(Bm1$hexID) #number cells with only 1 yr occupancy
            
            all.gud.gt1 <- filter(hex.modl, (is.na(hexID) == FALSE & #cell included in model-building
                                         conv > 0 &              #model converged
                                         sing == FALSE &         #model not singular
                                         n.lme4.msg == 0 &       #no lme4 warnings
                                         n.opt.warn == 0 &       #no optimization warnings
                                         n.Bm.yrs > 1))          #>1 yr with Bm sightings
            n.gud.gt1 <- n_distinct(all.gud.gt1$hexID)
            
            all.gud.gt0 <- filter(hex.modl, (is.na(hexID) == FALSE & #cell included in model-building
                                         conv > 0 &              #model converged
                                         sing == FALSE &         #model not singular
                                         n.lme4.msg == 0 &       #no lme4 warnings
                                         n.opt.warn == 0))       #no optimization warnings
            n.gud.gt0 <- n_distinct(all.gud.gt0$hexID)
            
            out.df <- cbind.data.frame("Month"=mo,
                                       "n.hex"=n.hex,
                                       "n.no.conv"=n.no.conv,
                                       "n.sing"=n.sing,
                                       "n.lme4.msg"=n.lme4.msg,
                                       "n.opt.warn"=n.opt.warn,
                                       "n.Bm1"=n.Bm1,
                                       "p.Bm1"=n.Bm1/n.hex,
                                       "n.gud.gt1"=n.gud.gt1,
                                       "n.gud.gt0"=n.gud.gt0)
            
              #Ck
                if(debugg == TRUE){
                  print(n.hex)
                  print(n.no.conv)
                  print(n.sing)
                  print(n.lme4.msg)
                  print(n.opt.warn)
                  print(n.Bm1)
                  print(n.gud.gt1)
                  print(n.gud.gt0)
                  print(out.df)
                }  
            
                #Look for associations among hex.modl, sing, conv, n.lme4.msg, n.opt.warn,
                #n.Bm.yrs
                        
                  ck.df <- filter(hex.modl, (is.na(hexID) == FALSE) & #keep only cells with models
                                             order == 1) %>%          #keep only one row per cell
                           dplyr::select(hexID, sing, conv,  #extract relevant columns
                                         n.lme4.msg, n.opt.warn, n.Bm.yrs) %>%
                           mutate(sing.01 = as.integer(sing)) #convert TRUE/FALSE to 1/0
                  
                  test1 <- which(ck.df$n.lme4.msg > 0 & 
                        ck.df$conv == 1 &
                        ck.df$n.opt.warn == 0 &
                        ck.df$sing.01 == 0)
                  test1.string <- paste(fnam, mo, "n.lme4.msg > 0 and no other flags:", 
                                       length(test1), sep=" ")
                  write(test1.string, file="Debugg//debugg.txt", append = TRUE)
                  
                  test2 <- which(ck.df$n.lme4.msg == 0 & 
                        (ck.df$conv != 1 |
                        ck.df$n.opt.warn > 0 |
                        ck.df$sing.01 == 1))
                  test2.string <- paste(fnam, mo, "n.lme4.msg = 0 and other flags exist:", 
                                       length(test2), sep=" ")
                  write(test2.string, file="Debugg//debugg.txt", append = TRUE)
                  write("", file="Debugg//debugg.txt", append = TRUE)
                  
    
              return(out.df)
          }    
          #Ck
            test <- hex.modl.smry(fnam="glmm_FM_m", mo=7, debugg=FALSE)
          
          #FM summaries  
            
            fm.hex.modl.smry <- lapply(7:10, function(i){
              stuff <- hex.modl.smry(fnam="glmm_FM_m", mo=i, debugg=FALSE)
              return(stuff)
            })
            fm.hex.modl.smry.df <- do.call("rbind",fm.hex.modl.smry)
            write.csv(fm.hex.modl.smry.df, file="Output//fm_hex_modl_smry_df.csv", row.names=FALSE)
            #CK
              summary(fm.hex.modl.smry.df)
              
          #Calf summaries  
            
            calf.hex.modl.smry <- lapply(7:10, function(i){
              stuff <- hex.modl.smry(fnam="glmm_Calf_m", mo=i, debugg=FALSE)
              return(stuff)
            })
            calf.hex.modl.smry.df <- do.call("rbind",calf.hex.modl.smry)
            write.csv(calf.hex.modl.smry.df, file="Output//calf_hex_modl_smry_df.csv", row.names=FALSE)
            #CK
              summary(calf.hex.modl.smry.df)
                
      } #end km25 specific analyses      
        
        
        
        
        
        
        
        
        
        
        
            
    #For 15 km resolution, define function for building models and plotting
        
      if(km25 == FALSE){  
              
        debugg <- FALSE
        #debugg <- TRUE
        if(debugg == TRUE){
          #dat <- fm.Bm
          dat <- fm.hex.dat
          mo <- mos[2]
          fnam <- "test"
          spdf <- fm.spdf
          fm.calf <- "FeedMill"
          #fm.calf <- "Calf"
        }
  
        p.persistence <- function(dat, mo, fnam, spdf, fm.calf){ 
        
          #Sum Bm inds across years within hex  
          
            mo.dat <- filter(dat, (Month == mo)) 
            tot.inds.mo <- group_by(mo.dat, hexID) %>%
                              summarise(ind = sum(ind, na.rm=TRUE))
            if(debugg == TRUE){
              print(summary(tot.inds.mo))
              print(dim(tot.inds.mo))
            }
            
          #Identify which hex cells have nonzero encounters (pooled across all years).
            hexID.no0.ind <- tot.inds.mo$hexID[which(tot.inds.mo$ind > 0)]
            if(debugg == TRUE) print(length(hexID.no0.ind))
  
          #Wrangle data.
          #For August & October FM with 15km cell resolution, there were cells with 
          #sightings but no effort due to distance at which sighting was detected. 
          #Sightings were associated with cells based on X/Ywhale, but effort is 
          #determined based on Lat and Long. I created get.n.km.yrs.omit0() to 
          #acommodate this situation, but it means that the plots will show cells
          #with sightings but no effort.

              hex.dat <- filter(dat, (hexID %in% hexID.no0.ind)) %>%  #extract non-zero cells
                             #filter(Month == mo & km > 0.0) %>% #only use records with survey effort in mo
                             filter(Month == mo) %>% #only use records from mo
                             group_by(hexID)   %>% #group by cell
                             nest() #nest the df so that the dim changes to the # of no0 cells
              if(debugg == TRUE){
                print(dim(hex.dat))
                print(names(hex.dat))
              } 

              #Put hex.dat in ascending hexID order
              
                sum(hexID.no0.ind != hex.dat$hexID) #Will be > 0
              
                hex.dat <- hex.dat[order(hex.dat$hexID),]
                #Ck
                  if(debugg == TRUE){
                    hex.dat$hexID
                    sum(hexID.no0.ind != hex.dat$hexID) #Zero
                  }  
                
              #Add columns for number of years with sightings, number of years with effort,
              #and proportion of years with sightings
                
                hex.dat <- mutate(hex.dat, n.Bm.yrs = unlist(purrr::map(data, get.n.Bm.yrs))) %>%
                            mutate(n.km.yrs = unlist(purrr::map(data, get.n.km.yrs.omit0))) %>% 
                            mutate(p.Bm.yrs = n.Bm.yrs/n.km.yrs)
                if(debugg == TRUE){
                  print(hex.dat$n.Bm.yrs)
                  print(hex.dat$n.km.yrs)
                  print(hex.dat$p.Bm.yrs)
                }
                
          #Plot number of Bm years, number of km years, and proportion of Bm years
                  
            #Prep data for plotting with ggplot2
                
              spdf@data$id <- as.character(1:n.hex - 1) #To match the tidy id created below     
              tidy.spdf <- tidy(spdf) #fortify spdf with broom::tidy
                     
              hex.dat <- mutate(hex.dat, id = as.character(hexID - 1)) #To match the tidy id created below
              hex.dat <- left_join(tidy.spdf, hex.dat, by='id') #merge spdf and hex.dat
              hex.dat.noNA <- filter(hex.dat, !is.na(hexID)) #Exclude cells without Bm
                      
            #Standardize plotting variables
                      
              n.Bm.yrs.colors = c("1" = "salmon", "2" = "yellow3", 
                                  "3" = "mediumseagreen", "4" = "mediumturquoise",
                                  "5" = "mediumorchid")
      
              my.palette <- colorRampPalette(brewer.pal(11, "Spectral"))
                      
              if(fm.calf == "FeedMill"){
                n.km.fill.scale <- scale_fill_gradientn(colours = my.palette(100), 
                                                        limits=c(1, 20)) #for fm
              } else {  
                n.km.fill.scale <- scale_fill_gradientn(colours = my.palette(100), 
                                                        limits=c(1, 8)) #for calves
              }  
                      
              p.Bm.fill.scale <- scale_fill_gradientn(colours = my.palette(100), 
                                                        limits=c(0, 1.0)) #probability
                      
              #my.theme <- theme(plot.margin = margin(0, 0, 0, 0, "cm")) 
  
            #Plot and save
                      
              plot.n.Bm.yrs <- ggplot(hex.dat) +
                  geom_polygon(data = hex.dat.noNA, aes(long, lat, 
                                                         fill = factor(n.Bm.yrs), 
                                                         group=id)) +
                  scale_fill_manual(values = n.Bm.yrs.colors) +
                  geom_polygon(aes(long, lat, group=id), fill = NA, colour = "black") +
                  labs(fill = "Number of\nBowhead Years") +
                  theme_bw() + coord_fixed() +
                  xlab("x") +
                  ylab("y")
              ggsave(filename = paste("Figures//",fnam,"_m", mo, "_n_Bm_yrs.png",sep=""),
                     device = "png", 
                     width = 14, height = 5.5, units = "cm", 
                     plot = plot.n.Bm.yrs)
                              
              plot.n.km.yrs <- ggplot(hex.dat) +
                  geom_polygon(data = hex.dat.noNA, aes(long, lat, 
                                                         fill = n.km.yrs, 
                                                         group = id)) + 
                  n.km.fill.scale +
                  geom_polygon(aes(long, lat, group=id), fill = NA, colour = "black") +
                  labs(fill = "Number of\nSurvey Years") +
                  theme_bw() + coord_fixed() +
                  xlab("x") +
                  ylab("y")
              ggsave(filename = paste("Figures//",fnam,"_m", mo, "_n_km_yrs.png",sep=""),
                     device = "png", 
                     width = 14, height = 5.5, units = "cm", 
                     plot = plot.n.km.yrs)
          
              plot.p.Bm.yrs <- ggplot(hex.dat) +
                geom_polygon(data = hex.dat.noNA, aes(long, lat, 
                                                       fill = p.Bm.yrs, 
                                                       group=id)) +
                p.Bm.fill.scale +
                geom_polygon(aes(long, lat, group=id), fill = NA, colour = "black") +
                labs(fill = "Proportion of\nBowhead Years") +
                theme_bw() + coord_fixed() 
              ggsave(filename = paste("Figures//",fnam,"_m", mo, "_p_Bm_yrs.png",sep=""),
                     device = "png", 
                     width = 14, height = 5.5, units = "cm", 
                     plot = plot.p.Bm.yrs)
              
          #Save hex.dat
            save(file = paste("Output//",fnam,"_m",mo,".Rdata", sep=""), hex.dat)
                
        }        
        
        #Call p.persistence
          p.persistence(fm.hex.dat, mo=7, fnam="FM", spdf=fm.spdf, fm.calf="FeedMill")
          p.persistence(fm.hex.dat, mo=8, fnam="FM", spdf=fm.spdf, fm.calf="FeedMill")
          p.persistence(fm.hex.dat, mo=9, fnam="FM", spdf=fm.spdf, fm.calf="FeedMill")
          p.persistence(fm.hex.dat, mo=10, fnam="FM", spdf=fm.spdf, fm.calf="FeedMill")
          
          p.persistence(calf.hex.dat, mo=7, fnam="calf", spdf=calf.spdf, fm.calf="Calf")
          p.persistence(calf.hex.dat, mo=8, fnam="calf", spdf=calf.spdf, fm.calf="Calf")
          p.persistence(calf.hex.dat, mo=9, fnam="calf", spdf=calf.spdf, fm.calf="Calf")
          p.persistence(calf.hex.dat, mo=10, fnam="calf", spdf=calf.spdf, fm.calf="Calf")
        
      } #end 15-km-specific analyses    
         
        
        
        
        
        
        
        
        
        
        
  #Try running GLMM analysis on case studies defined in JTC_best_cluster_Z.csv. 
  #Use results from Bm_BIA_Optimization_Z_Deux.r.
        
    #Input information about case studies from JTC_best_cluster_Z.csv
      best <- read.csv("Notes//JTC_best_cluster_Z.csv")
      #CK
        summary(best)
        dim(best)
        best
      
    #Extract R object ID of cells considered in analysis
        
      load("C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Output_BIA_Z_Deux\\OutLists\\C2_Z0.7_m7_FM.rdata")
      all.xi <- out.list$x.in.sol$x.i
      #CK
        all.xi
      
    #Loop through scenarios in best and append x.i to best. 
    #Ignore warning about "row names were found from a short variable and
    #have been discarded. This is because df best has row names.
        
      best.id <- cbind.data.frame(best[1,], "x.i"=NA, "id"=NA, "cluster"=NA)
      best.id
        
      for (B in 1:nrow(best)){     
        
        #B <- 3    
  
        #Input cluster results 
          pth <- "C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Output_BIA_Z_Deux\\OutLists\\"
          load(paste(pth, "C", best$C[B], 
                          "_Z", best$Z[B], 
                          "_m", best$month[B], 
                          "_", best$fm.calf[B],
                          ".rdata", 
                     sep = "")) 
          #Ck
            names(out.list)
            out.list$x.in.sol
            
        #Identify clusters in solution    
          x <- as.data.frame(out.list$x.in.sol)
          x$id <- as.character(x$x.i - 1) #For consistency with tidy.spdf convention

        #Extract only cells in solution
          x.in <- filter(x, (value > 0)) 
          #CK
            summary(x.in) #value should all be >0
            
        #Merge x.in with info in best.id
          new.rows <- cbind.data.frame(best[B,], "x.i"=x.in$x.i, 
                                      "id"=x.in$id, "cluster"=NA)  
          best.id <- rbind.data.frame(best.id, new.rows)

      }
      
      #Remove initial dummy row and output to .csv
        best.id[1,]
        nrow(best.id)
        best.id <- best.id[-1,]
        nrow(best.id)
        summary(best.id)
        write.csv(best.id, file="Notes//best_id.csv")
        
      #Create shapefile with cluster boundaries for each month X activity  
        
        fmcalf <- c("FM", "calf")
      
        for(i in 1:2){ #fm = 1; calf = 2
          
          for(j in 7:10){ #months
            
            #Extract data for fm/calf in month j
              ij.dat <- filter(best.id, fm.calf == fmcalf[i] &
                                        month == j)
            
            #Create list of polygons corresponding to IDs in x.i  
              poly.list.ij <- lapply(1:nrow(ij.dat), function(k){
                return(fm.spdf@polygons[[ij.dat$x.i[k]]])
              })
              
            #Create SpatialPolygon object from polygon list
              p.ij <- SpatialPolygons(poly.list.ij, proj4string=prj)
            
            #Create spdf from sp object & output to shapefile
              spdf.ij <- SpatialPolygonsDataFrame(p.ij, data=ij.dat, match.ID = FALSE)
              
              fnam <- paste("clusters_",
                            ij.dat$fm.calf[1],
                            "_mo",ij.dat$month[1],
                            "_C",ij.dat$C[1],
                            "_Z",ij.dat$Z[1],
                            sep="")
              
              writeOGR(spdf.ij, dsn="Output//Shapefiles", layer=fnam,
                       driver="ESRI Shapefile", overwrite_layer = TRUE)
            
          }
          
        }  

      #Intermission to examine map of study area in Bm_BIA_Persistence.mxd and manually
      #assign cluster IDs to each cell in best.id. Save cluster assignments to 
      #best_id_cluster.csv and input below.
        
        best.id.cluster <- read.csv("Notes/best_id_cluster.csv")
        best.id.cluster$id <- as.character(best.id.cluster$id)
        names(best.id.cluster)[2] <- "Month"
        #CK
          summary(best.id.cluster)
        
      Months <- rep(c("July", "August", "September", "October"),2)   
            
      for (B in 1:nrow(best)){   
        
        #B <- 3    
        
        if(best$fm.calf[B] == "FM"){
          act <- "Feeding & Milling "
        } else {
          act <- "Calves "
        }
        
        title.B <- paste(Months[B],
                         " ",
                         act,
                         "(C",
                         best$C[B],
                         ", Z=",
                         best$Z[B],
                         ")",
                         sep = "")
        title.B
        
        if(B <= 4){
          B.hex.dat <- fm.hex.dat
          B.spdf <- fm.spdf
        } else {
          B.hex.dat <- calf.hex.dat
          B.spdf <- calf.spdf
        }
        
        #Extract info for month X fm.calf B
        
          x <- filter(best.id.cluster, 
                      Month == best$month[B] &
                      fm.calf == best$fm.calf[B])
          x$cluster <- as.factor(x$cluster)
          #CK
            summary(x)
            summary(as.factor(x$fm.calf))
            summary(x$cluster)
            
      #Merge cluster info with sighting and effort data
        B.hex.dat$id <- as.character(B.hex.dat$hexID - 1) #For consistency with tidy.spdf convention
        x <- left_join(x, B.hex.dat, by=c('id', 'Month'))
        #CK
          dim(x)
          names(x)
          head(x, 30)
            
      #Extract only data with effort and output for further reflection
        x <- filter(x, km > 0.0)
        x.fnam <- paste("Output//data_by_cell_C",
                                  best$C[B],
                                  "_Z",
                                  best$Z[B],
                                  "_m",
                                  best$month[B],
                                  "_",
                                  best$fm.calf[B],
                                  ".csv",
                                  sep="")
        write.csv(x, file = x.fnam, row.names = FALSE)
        #CK
          dim(x)
          summary(x$Month)
          summary(x$km)
          x
  
      #Sum number of whales per cluster per year. Round c.ind if round.c.ind == TRUE.
            
        if(round.c.ind == TRUE){
          tot.inds.cluster <- group_by(x, cluster, Year) %>%
                                       summarise(c.ind = round(sum(ind, na.rm=TRUE)))
        } else {
          tot.inds.cluster <- group_by(x, cluster, Year) %>%
                                       summarise(c.ind = sum(ind, na.rm=TRUE))
        }
        #Ck
          dim(tot.inds.cluster)
          names(tot.inds.cluster)
          summary(tot.inds.cluster)
          sum(x$ind)
          sum(tot.inds.cluster$c.ind)
          tot.inds.cluster
  
      #Sum effort per cluster per year
  
        tot.km.cluster <- group_by(x, cluster, Year) %>%
                                       summarise(c.km = sum(km, na.rm=TRUE))
        #Ck
          dim(tot.km.cluster)
          names(tot.km.cluster)
          summary(tot.km.cluster)
          sum(x$km)
          sum(tot.km.cluster$c.km)
          tot.km.cluster
          
      #Merge cluster data for effort and sightings into a single object
      #and output for further reflection
          
        wow <- left_join(tot.km.cluster, tot.inds.cluster, by=c('cluster', 'Year'))
        
        wow.fnam <- paste("Output//data_by_cluster_C",
                                  best$C[B],
                                  "_Z",
                                  best$Z[B],
                                  "_m",
                                  best$month[B],
                                  "_",
                                  best$fm.calf[B],
                                  "_",
                                  rnd,
                                  ".csv",
                                  sep="")
        write.csv(wow, file = wow.fnam, row.names = FALSE)
        #Ck
          names(wow)
          dim(wow)
          class(wow)
          wow
          
      #Build GLMMs by cluster with Year as random effect. I compared models built with
      #Year as a factor variable and as an integer, and the GLMMs ended up identical.
        
        #Define function for building GLMM        
          fit.one.cluster.glmm <- function(dat) #fcn to fit GLMM at a single pixel
                                    mod <- glmer(c.ind ~ 1 + (1|Year) + offset(log(c.km)),
                                                 data=dat, family=poisson)
          #CK
            test.dat <- filter(wow, cluster=="a")
            names(test.dat)
            test.glmm <- fit.one.cluster.glmm(test.dat)
            test.glmm
            
        #Apply function to each cluster in a nested df. 
  
          wow <- group_by(wow, cluster) %>%
                 nest() %>%             #create a nested data frame
                 mutate(model = purrr::map(data, fit.one.cluster.glmm)) #fit a model to each cluster
          #Ck  
            names(wow)
            class(wow)
            wow
            i <- 1
            wow$data[[i]]
            wow$model[[i]]
            isSingular(wow$model[[i]])
            
        #Extract model fitting and parameter estimates.
          
          #Add columns for singularity, convergence, # lme4 warnings, and # optimization 
          #warnings. See notes in fit.and.plot for further details.
  
            wow <- mutate(wow, sing = unlist(purrr::map(model, isSingular))) %>%
                   mutate(conv = unlist(purrr::map(model, get.conv.lme4))) %>%
                   mutate(n.lme4.msg = unlist(purrr::map(model, get.n.msg.lme4))) %>%
                   mutate(n.opt.warn = unlist(purrr::map(model, get.n.opt.lme4)))
           #Ck 
              class(wow)
              head(wow)
              summary(wow$sing)
              summary(wow$conv)
              summary(wow$n.lme4.msg)
              length(which(wow$n.lme4.msg > 0))
              summary(wow$n.opt.warn)
              length(which(wow$n.opt.warn > 0))
  
          #Add columns for number of years with sightings, number of years with effort,
          #proportion of years with sightings, total number of sightings (pooled across
          #years), and total effort (pooled across years) by cluster

            #Define cluster-based functions for extracting n.Bm.yrs, n.km.yrs, n.Bm,
            #and tot.km
            
              get.cluster.n.Bm.yrs <- function(data){
                                          n.yrs <- length(which(data$c.ind > 0))
                                          return(n.yrs)
                                      }
              
              get.cluster.n.km.yrs <- function(data){
                                        n.yrs <- nrow(data)
                                        return(n.yrs)
                                      }

              get.cluster.n.Bm <- function(data){
                                        n.Bm <- sum(data$c.ind)
                                        return(n.Bm)
                                      }
              
              get.cluster.tot.km <- function(data){
                                        tot.km <- sum(data$c.km) 
                                        return(tot.km)
                                      }

            #Apply functions          
                  
              wow <- mutate(wow, n.Bm.yrs = unlist(purrr::map(data, get.cluster.n.Bm.yrs))) %>%
                                mutate(n.Bm = unlist(purrr::map(data, get.cluster.n.Bm))) %>%
                                mutate(n.km.yrs = unlist(purrr::map(data, get.cluster.n.km.yrs))) %>% 
                                mutate(tot.km = unlist(purrr::map(data, get.cluster.tot.km))) %>%
                                mutate(p.Bm.yrs = n.Bm.yrs/n.km.yrs)
              #CK
                wow$n.Bm.yrs
                wow$n.Bm
                wow$n.km.yrs
                wow$tot.km
                wow$p.Bm.yrs
  
          #Extract model params for glmms
                  
            #Examine wow object structure
              
              i <- 1  
              wow$model[[i]]
              class(wow$model[[i]])
              summary(wow$model[[i]])$coefficients[1] #Fixed Effects Intercept
              summary(wow$model[[i]])$coefficients[2] #SE of Fixed Effects Intercept
              attributes(summary(wow$model[[i]])$varcor[[1]])$stddev #SD Year Rdm Effect
              summary(wow$model[[i]])$sigma #residual error SD
              wow$cluster[i]
   
            #Extract and output .Rdata file   
              
              wow.modl.pars <- purrr::map(wow$model, get.params.lme4) %>% #extract params
                         do.call("rbind",.) %>% #convert list to data.frame
                         mutate(cluster = wow$cluster) #add cluster
              
              dim(wow)
              wow <- left_join(wow, wow.modl.pars, by='cluster')
              
              wow.rdata.fnam <- paste("Output//wow_by_cluster_C",
                                      best$C[B],
                                      "_Z",
                                      best$Z[B],
                                      "_m",
                                      best$month[B],
                                      "_",
                                      best$fm.calf[B],
                                      "_",
                                      rnd,
                                      ".Rdata",
                                      sep="")
              save(wow, file=wow.rdata.fnam)
              #CK
                class(wow.modl.pars)
                wow.modl.pars
                summary(wow.modl.pars)
                
                dim(wow)
                summary(wow)
                
      #Plot results       
                
        #Add wow to x
              
          x <- left_join(x, wow, by='cluster')
          #Ck
            names(x)
                
        #Add id and fortify B.spdf, then left_join it to x. 
      
          B.spdf@data$id <- as.character(1:n.hex - 1) #To match the tidy id created below
          B.spdf <- tidy(B.spdf) #fortify spdf with broom::tidy
          x <- left_join(B.spdf, x, by='id') #merge spdf
          x.nona <- filter(x, is.na(cluster) == FALSE)
          #CK
            summary(B.spdf)
            summary(x.nona$cluster)
            summary(x$cluster)
    
            plot.test <- ggplot() +
                      geom_polygon(x, mapping = aes(long, lat, 
                                       fill = cluster, 
                                       group=id)) +
                      geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group)) +
                      theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
                            axis.text.x=element_blank(), axis.text.y=element_blank(),
                            axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + 
                      coord_fixed() +
                      xlab("x") +
                      ylab("y") + 
                      ggtitle(title.B)
            plot.test
            
        #Create and export shapefile with stats and parameter info
          
            #Create a df comprising unique hex IDs (x.i) and associated params & stats
              shp.df <- x.nona[,c("Month",
                                                        "fm.calf",
                                                        "C",
                                                        "Z",
                                                        "x.i",
                                                        "cluster",
                                                        "n.Bm.yrs",
                                                        "n.Bm",
                                                        "n.km.yrs",
                                                        "tot.km",
                                                        "p.Bm.yrs",
                                                        "fixed.int",
                                                        "se.fixed.int",
                                                        "Year.sd",
                                                        "resid.sd")]
              dim(shp.df)
              dim(x.nona)
              shp.df <- shp.df[!duplicated(shp.df$x.i),]
                #Ck
                  dim(shp.df)
                  names(shp.df)
                  summary(shp.df)
        
            #Create list of polygons corresponding to IDs in x.i  
              shp.poly.list <- lapply(1:nrow(shp.df), function(shp.i){
                return(fm.spdf@polygons[[shp.df$x.i[shp.i]]])
              })
                  
            #Create SpatialPolygon object from polygon list
              shp.SP <- SpatialPolygons(shp.poly.list, proj4string=prj)
                
            #Create spdf from sp object & output to shapefile
              shp.spdf <- SpatialPolygonsDataFrame(shp.SP, data=shp.df, match.ID = FALSE)
                  
              shp.fnam <- paste("C",
                                      best$C[B],
                                      "_Z",
                                      best$Z[B],
                                      "_m",
                                      best$month[B],
                                      "_",
                                      best$fm.calf[B],
                                      "_",rnd,
                                      "_results",
                                      sep="")
                  
              writeOGR(shp.spdf, dsn="Output//Shapefiles", layer=shp.fnam,
                           driver="ESRI Shapefile", overwrite_layer = TRUE)
              #Ck
                plot(shp.spdf)

        #Standardize plotting variables
                          
          n.Bm.yrs.colors = c("12" = "yellow3", 
                              "5" = "mediumseagreen", "4" = "mediumturquoise",
                              "3" = "mediumorchid")
          
          n.km.yrs.colors = c("20" = "salmon", 
                              "19" = "yellow3", "18" = "mediumturquoise")
          
          my.palette <- colorRampPalette(brewer.pal(11, "Spectral"))
                          
          n.km.fill.scale <- scale_fill_gradientn(colours = rev(my.palette(100)), limits=c(1, 20))
          
          p.Bm.fill.scale <- scale_fill_gradientn(colours = rev(my.palette(100)), limits=c(0, 1.0))
          
          my.theme <- theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
                            axis.text.x=element_blank(), axis.text.y=element_blank(),
                            axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                            panel.background = element_blank(), 
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            text=element_text(size=10,  family="Times New Roman"),
#                            legend.position = c(1, 1), 
#                            legend.justification = c(1, 1),
#                            legend.box.background = element_rect(
#                              fill = "white"
#                            ),
#                            plot.background = element_rect(
#                              colour = "black",
#                              size = 1
#                            ),
                            legend.background = element_rect(fill=NA, size=.5))
          
        #n.Bm.yrs
            
          plot.n.Bm.yrs <- ggplot(x) +
                              geom_polygon(data=x.nona, mapping = aes(long, lat, 
                                               fill = factor(n.Bm.yrs), 
                                               group=id)) +
                              geom_polygon(aes(long, lat, group=id), fill = NA, 
                                           colour = "black", size=0.2) +
                              geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group), fill = "gray",
                                   colour = "black", size=0.2) +
                              #scale_fill_manual(values = n.Bm.yrs.colors) +
                              labs(fill = "Number of\nBowhead Years") +
                              my.theme +
                              coord_fixed() +
#                              guides(fill = guide_legend(ncol = 2))
                              ggtitle(title.B)
          plot.n.Bm.yrs
          ggsave(filename = paste("Figures//C",
                                  best$C[B],
                                  "_Z",
                                  best$Z[B],
                                  "_m",
                                  best$month[B],
                                  "_",
                                  best$fm.calf[B],
                                  "_",rnd,
                                  "_n_Bm_yrs.png",
                                  sep=""),
                                  device = "png", 
                                  width = 12, height = 6, units = "cm", 
                                  plot = plot.n.Bm.yrs)
    
        #n.km.yrs
            
          plot.n.km.yrs <- ggplot(x) +
                              geom_polygon(data=x.nona, aes(long, lat, 
                                               fill = n.km.yrs, 
                                               group=id)) +
                              n.km.fill.scale +
                              geom_polygon(aes(long, lat, group=id), fill = NA, 
                                           colour = "black", size=0.2) +
                              geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group), fill = "gray",
                                   colour = "black", size=0.2) +
                              labs(fill = "Number of\nSurvey Years") +
                              my.theme + 
                              coord_fixed()
                              #ggtitle(title.B)
          plot.n.km.yrs
          ggsave(filename = paste("Figures//C",
                                  best$C[B],
                                  "_Z",
                                  best$Z[B],
                                  "_m",
                                  best$month[B],
                                  "_",
                                  best$fm.calf[B],
                                  "_",rnd,
                                  "_n_km_yrs_continuous.png",
                                  sep=""),
                                  device = "png", 
                                  width = 14, height = 5.5, units = "cm", 
                                  plot = plot.n.km.yrs)
            
          plot.n.km.yrs2 <- ggplot(x) +
                              geom_polygon(data=x.nona, aes(long, lat, 
                                               fill = factor(n.km.yrs), 
                                               group=id)) +
                              #scale_fill_manual(values = n.km.yrs.colors) +
                              geom_polygon(aes(long, lat, group=id), fill = NA, 
                                           colour = "black", size=0.2) +
                              geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group), fill = "gray",
                                   colour = "black", size=0.2) +
                              labs(fill = "Number of\nSurvey Years") +
                              my.theme + 
                              coord_fixed() +
                              guides(fill = guide_legend(ncol = 2))
                              #ggtitle(title.B)
          plot.n.km.yrs2
          ggsave(filename = paste("Figures//C",
                                  best$C[B],
                                  "_Z",
                                  best$Z[B],
                                  "_m",
                                  best$month[B],
                                  "_",
                                  best$fm.calf[B],
                                  "_",rnd,
                                  "_n_km_yrs_discrete.png",
                                  sep=""),
                                  device = "png", 
                                  width = 14, height = 5.5, units = "cm", 
                                  plot = plot.n.km.yrs2)
            
        #p.Bm.yrs
            
          plot.p.Bm.yrs <- ggplot(x) +
                              geom_polygon(data=x.nona, aes(long, lat, 
                                               fill = p.Bm.yrs, 
                                               group=id)) +
                              p.Bm.fill.scale +
                              geom_polygon(aes(long, lat, group=id), fill = NA, 
                                           colour = "black", size=0.2) +
                              geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group), fill = "gray",
                                   colour = "black", size=0.2) +
                              labs(fill = "Proportion of\nBowhead Years") +
                              my.theme + 
                              coord_fixed()
                              #ggtitle(title.B)
          plot.p.Bm.yrs
          ggsave(filename = paste("Figures//C",
                                  best$C[B],
                                  "_Z",
                                  best$Z[B],
                                  "_m",
                                  best$month[B],
                                  "_",
                                  best$fm.calf[B],
                                  "_",rnd,
                                  "_p_Bm_yrs_continuous.png",
                                  sep=""),
                                  device = "png", 
                                  width = 14, height = 5.5, units = "cm", 
                                  plot = plot.p.Bm.yrs)
            
        #fixed intercept
            
          plot.fixed.int <- ggplot(x) +
                              geom_polygon(data=x.nona, aes(long, lat, 
                                               fill = fixed.int, 
                                               group=id)) +
                              scale_fill_gradientn(colours = rev(my.palette(100)), 
                                                   limits=range(x.nona$fixed.int)) +
                              geom_polygon(aes(long, lat, group=id), fill = NA, 
                                           colour = "black", size=0.2) +
                              geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group), fill = "gray",
                                   colour = "black", size=0.2) +
                              labs(fill = "Fixed\nIntercept") +
                              my.theme + 
                              coord_fixed()
                              #ggtitle(title.B)
          plot.fixed.int
          ggsave(filename = paste("Figures//C",
                                  best$C[B],
                                  "_Z",
                                  best$Z[B],
                                  "_m",
                                  best$month[B],
                                  "_",
                                  best$fm.calf[B],
                                  "_",rnd,
                                  "_fixed_int.png",
                                  sep=""),
                                  device = "png", 
                                  width = 14, height = 5.5, units = "cm", 
                                  plot = plot.fixed.int)
          
        #SD(Year) Random Effect
            
          plot.sd.yr <- ggplot(x) +
                              geom_polygon(data=x.nona, aes(long, lat, 
                                               fill = Year.sd, 
                                               group=id)) +
                              scale_fill_gradientn(colours = rev(my.palette(100)), 
                                                   limits=range(x.nona$Year.sd)) +
                              geom_polygon(aes(long, lat, group=id), fill = NA, 
                                           colour = "black", size=0.2) +
                              geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group), fill = "gray",
                                   colour = "black", size=0.2) +
                              labs(fill = "SD(Year)\nRandom Effect") +
                              my.theme + 
                              coord_fixed()
                              #ggtitle(title.B)
          plot.sd.yr
          ggsave(filename = paste("Figures//C",
                                  best$C[B],
                                  "_Z",
                                  best$Z[B],
                                  "_m",
                                  best$month[B],
                                  "_",
                                  best$fm.calf[B],
                                  "_",rnd,
                                  "_sdYear.png",
                                  sep=""),
                                 device = "png", 
                                 width = 14, height = 5.5, units = "cm", 
                                 plot = plot.sd.yr)
            
        #Cluster locations
            
          plot.cluster <- ggplot(x) +
                              geom_polygon(data=x.nona, aes(long, lat, 
                                               fill = cluster, 
                                               group=id)) +
                              geom_polygon(aes(long, lat, group=id), fill = NA, 
                                           colour = "black", size=0.2) +
                              geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group), fill = "gray",
                                   colour = "black", size=0.2) +
                              labs(fill = "Cluster") +
                              my.theme + 
                              coord_fixed() +
                              guides(fill = guide_legend(ncol = 2))
                              #ggtitle(title.B)
          plot.cluster
          ggsave(filename = paste("Figures//C",
                                  best$C[B],
                                  "_Z",
                                  best$Z[B],
                                  "_m",
                                  best$month[B],
                                  "_",
                                  best$fm.calf[B],
                                  "_",rnd,
                                  "_Cluster.png",
                                  sep=""),
                                 device = "png", 
                                 width = 14, height = 5.5, units = "cm", 
                                 plot = plot.cluster)
   
        #n.Bm
          
          plot.n.Bm <- ggplot(x) +
                              geom_polygon(data=x.nona, mapping = aes(long, lat, 
                                               fill = factor(n.Bm), 
                                               group=id)) +
                              geom_polygon(aes(long, lat, group=id), fill = NA, 
                                           colour = "black", size=0.2) +
                              geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group), fill = "gray",
                                   colour = "black", size=0.2) +
                              #scale_fill_manual(values = n.Bm.yrs.colors) +
                              labs(fill = "Number of\nBowheads") +
                              my.theme + 
                              coord_fixed() +
                              guides(fill = guide_legend(ncol = 2))
                              #ggtitle(title.B)
          plot.n.Bm
          ggsave(filename = paste("Figures//C",
                                  best$C[B],
                                  "_Z",
                                  best$Z[B],
                                  "_m",
                                  best$month[B],
                                  "_",
                                  best$fm.calf[B],
                                  "_",rnd,
                                  "_n_Bm.png",
                                  sep=""),
                                  device = "png", 
                                  width = 14, height = 5.5, units = "cm", 
                                  plot = plot.n.Bm)

        #tot.km
          
          plot.tot.km <- ggplot(x) +
                              geom_polygon(data=x.nona, mapping = aes(long, lat, 
                                               fill = factor(as.integer(tot.km)), 
                                               group=id)) +
                              geom_polygon(aes(long, lat, group=id), fill = NA, 
                                           colour = "black", size=0.2) +
                              geom_polygon(shor.poly, 
                                   mapping = aes(long, lat, group=group), fill = "gray",
                                   colour = "black", size=0.2) +
                              #scale_fill_manual(values = n.Bm.yrs.colors) +
                              labs(fill = "km") +
                              my.theme + 
                              coord_fixed() +
                              guides(fill = guide_legend(ncol = 2))
                              #ggtitle(title.B)
          plot.tot.km
          ggsave(filename = paste("Figures//C",
                                  best$C[B],
                                  "_Z",
                                  best$Z[B],
                                  "_m",
                                  best$month[B],
                                  "_",
                                  best$fm.calf[B],
                                  "_",rnd,
                                  "_tot_km.png",
                                  sep=""),
                                  device = "png", 
                                  width = 14, height = 5.5, units = "cm", 
                                  plot = plot.tot.km)

    } #end looping through B scenarios in best for GLMM building and plotting
          
  #Input GLMM cluster model results. Evaluate convergence and singularity issues.
  #Extract inter-annual variability results. Remember that the C and Z values for
  #the selected FM and calf scenarios are saved to dataframe best.
            
    wow.path <- "C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\CamdenBayBehavior\\Persistence\\Output\\"        
    
    for (B in 1:nrow(best)){     
        
      #B <- 3    
  
      #Input cluster model results in tibble wow 
      
        load(paste(wow.path, "wow_by_cluster_C", best$C[B], 
                          "_Z", best$Z[B], 
                          "_m", best$month[B], 
                          "_", best$fm.calf[B],
                          "_", rnd,
                          ".Rdata", 
                     sep = "")) 
      
      #Save results to dataframe and print results
        wow.out.df <- as.data.frame(wow[,c(1,4:16)])
        wow.out.df
        out.fnam <- paste(wow.path, "wow_out_df_C", best$C[B], 
                          "_Z", best$Z[B], 
                          "_m", best$month[B], 
                          "_", best$fm.calf[B],
                          "_", rnd,
                          ".csv", 
                     sep = "")
        write.csv(wow.out.df, file = out.fnam)
    }  
    
    #Examination of wow.out.df shows that all of the FM models converged and none were singular.
    #The following diagnostics were found for the calf models:
    #  July: 1 singular model
    #  August: no convergence or singularity issues
    #  September: 2 singular and 1 non-convergence
    #  October: 1 singular and 1 non-convergence. 
    #
    #See test_wow_modl_conv.r for detailed notes on convergence and singularity issues. Based
    #on Bolker's GLMM FAQ website, I decided to accept that Year.sd = 0 for singular models.
    #
    #The only models that didn't converge were those with non-integer c.ind values due to CAPs.
    #To revise those models, I started by rounding c.ind so that all c.ind values were integers.
    #I rebuilt the model based on the new data. If the model converged, I accepted the final 
    #results without further inquiry. If the model still didn't converge, I tried using allFit 
    #determine whether different optimization methods produce similar results. See code below.
    #I copied the final parameter estimates to 
    #   wow_out_df_C3_Z0.7_m9_calf_all_converged.csv
    #   wow_out_df_C2_Z0.7_m10_calf_all_converged.csv
    
      #September Calves
        load(paste(wow.path, "wow_by_cluster_C3_Z0.7_m9_calf.Rdata", sep="" ))  

        #Examine data for model that didn't converge
          wow$data[[1]]
          
          #The 2019 data included CAPs, so c.ind was not an integer.
          #Try replacing non-integer c.ind with integer value by rounding
            new.dat <- wow$data[[1]]
            new.dat$c.ind[8] <- round(new.dat$c.ind[8])
            new.dat
            new.mod <- glmer(c.ind ~ 1 + (1|Year) + offset(log(c.km)),
                                         data=new.dat, family=poisson)
            summary(new.mod)
            summary(new.mod)$coefficients[1] #fixed.int
            summary(new.mod)$coefficients[2] #se.fixed.int
            attributes(summary(new.mod)$varcor[[1]])$stddev #Year.sd
            new.mod@optinfo$conv$lme4$code #-1 if no convergence
                                                           #NULL if singular
                                                           #NULL if converged
            new.mod@optinfo$conv

          #Try allFit to examine parameter estimates from multiple optimizers
            allFit.mods <- allFit(new.mod) 
            summary(allFit.mods)
            #Parameter estimates are all nearly identical. Use parameter estimates in new.mod.
            
      #October Calves
        load(paste(wow.path, "wow_by_cluster_C2_Z0.7_m10_calf.Rdata", sep="" ))  

        #Examine data for model that didn't converge
          wow$data[[5]]
          
          #The 2018 data included CAPs, so c.ind was not an integer.
          #Try replacing non-integer c.ind with integer value by rounding
            new.dat <- wow$data[[5]]
            new.dat$c.ind[7] <- round(new.dat$c.ind[7])
            new.dat
            new.mod <- glmer(c.ind ~ 1 + (1|Year) + offset(log(c.km)),
                                         data=new.dat, family=poisson)
            summary(new.mod)
            summary(new.mod)$coefficients[1] #fixed.int
            summary(new.mod)$coefficients[2] #se.fixed.int
            attributes(summary(new.mod)$varcor[[1]])$stddev #Year.sd
            new.mod@optinfo$conv$lme4$code #-1 if no convergence
                                                           #NULL if singular
                                                           #NULL if converged
            new.mod@optinfo$conv

          #Try allFit to examine parameter estimates from multiple optimizers
            allFit.mods <- allFit(new.mod) 
            summary(allFit.mods)
            #Parameter estimates are all nearly identical. Use parameter estimates in new.mod.
             
    
    
    

    
            
          
          
          
          
          
          
          
          