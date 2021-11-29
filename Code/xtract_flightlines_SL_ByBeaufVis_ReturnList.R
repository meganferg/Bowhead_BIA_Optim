#Script xtract_flightlines_SL_byBeaufVis_ReturnList.r...Megan C. Ferguson...16 November 2021
#
# NOTES:
#
#  1. This script was based on xtract_flightlines_SL_byBeauf_ReturnList.r, v. 20181223.  
#
#  2. This script uses data formatted by InputAerialMaster...r.
#
#  3. Should use xFltType instead of FltType because auto position
#     updates in AerialMaster are coded with FLtType = -1
#
#  4. In order for this code to work, must introduce the variables iBeauf and VisX.km,
#     defined as follows:
#
#     #Re-code cBeauf as integer.  
#        iBeauf <- rep(NA, nrow(am.all))
#        cBeauf.names <- names(summary(am.all$cBeauf))
#        iBeauf[which(am.all$cBeauf == cBeauf.names[1])] <- NA
#        iBeauf[which(am.all$cBeauf == cBeauf.names[2])] <- 0
#        iBeauf[which(am.all$cBeauf == cBeauf.names[3])] <- 1
#        iBeauf[which(am.all$cBeauf == cBeauf.names[4])] <- 2
#        iBeauf[which(am.all$cBeauf == cBeauf.names[5])] <- 3
#        iBeauf[which(am.all$cBeauf == cBeauf.names[6])] <- 4
#        iBeauf[which(am.all$cBeauf == cBeauf.names[7])] <- 5
#        iBeauf[which(am.all$cBeauf == cBeauf.names[8])] <- 6
#        iBeauf[which(am.all$cBeauf == cBeauf.names[9])] <- 7
#        iBeauf[which(am.all$cBeauf == cBeauf.names[10])] <- 8
#        iBeauf[which(am.all$cBeauf == cBeauf.names[11])] <- NA
#        iBeauf[which(am.all$cBeauf == cBeauf.names[12] |
#                      am.all$cBeauf == cBeauf.names[13] |
#                      am.all$cBeauf == cBeauf.names[14])] <- NA
#      
#        #Fill in non-deadhead auto-updates with a Beaufort value.
#          old.iBeauf <- iBeauf[1]  
#          for(i in 2:nrow(am.all)){
#            new.iBeauf <- iBeauf[i]
#            if(is.na(new.iBeauf) == FALSE | 
#               (am.all$Entry[i] == "deadhead" & is.na(am.all$Entry[i]) == FALSE)){
#              old.iBeauf <- new.iBeauf
#            } else {
#              iBeauf[i] <- old.iBeauf
#            }
#          }
#      
#     #Create VisX.km, as average numeric vis across both sides of aircraft
#
#          VisL.km <- rep(NA, nrow(am.all))
#          VisL.km[am.all$VisL == "0 km"] <- 0
#          VisL.km[am.all$VisL == "<1 km"] <- 0.5
#          VisL.km[am.all$VisL == "1-2 km"] <- 1.5
#          VisL.km[am.all$VisL == "2-3 km"] <- 2.5
#          VisL.km[am.all$VisL == "3-5 km"] <- 4
#          VisL.km[am.all$VisL == "5-10 km"] <- 7.5
#          VisL.km[am.all$VisL == "Unlimited" | am.all$VisL == "unlimited"] <- 20
#          
#          VisR.km <- rep(NA, nrow(am.all))
#          VisR.km[am.all$VisR == "0 km"] <- 0
#          VisR.km[am.all$VisR == "<1 km"] <- 0.5
#          VisR.km[am.all$VisR == "1-2 km"] <- 1.5
#          VisR.km[am.all$VisR == "2-3 km"] <- 2.5
#          VisR.km[am.all$VisR == "3-5 km"] <- 4
#          VisR.km[am.all$VisR == "5-10 km"] <- 7.5
#          VisR.km[am.all$VisR == "Unlimited" | am.all$VisR == "unlimited"] <- 20
#          
#          VisX.km <- (VisL.km + VisR.km)/2
#
#        #Fill in non-deadhead auto-updates with a VisX.km value.
#          old.VisX.km <- VisX.km[1]  
#          for(i in 2:nrow(am.all)){
#            new.VisX.km <- VisX.km[i]
#            if(is.na(new.VisX.km) == FALSE | 
#               (am.all$Entry[i] == "deadhead" & is.na(am.all$Entry[i]) == FALSE)){
#              old.VisX.km <- new.VisX.km
#            } else {
#              VisX.km[i] <- old.VisX.km
#            }
#          }
#
#  5. It is possible to have Beaufort=NA during non-deadhead effort.  In order
#     to extract all non-deadhead effort for entries with cBeauf=NA via the 
#     beauf.x variable, must specify beauf.x = NA.  Set beauf.x = -1 to 
#     extract all effort of a specific survey mode, regardless of cBeauf.
#
#  6. vis.x refers to the minimum vis (km) averaged on both sides of aircraft that is
#     acceptable for including in the segments.  Set vis.x = -1 to extract all effort of
#     a specific survey mode.
#
#  7. Function Definitions: 
#    a. xtract.tx.byBeaufVis.SL (previously called "xtract.trax.SL")
#    The function xtract.tx.SL inputs BWASP/COMIDA data and a proj4string for the
#    desired projection of transects that are output, and it outputs the following:
#      [[1]] tx.idx : vector of indices corresponding to transect data.
#      [[2]] tx.list : list of indices for individual transects from "tx.idx" vector.
#      [[3]] tx.SL.prj : projected transects represented as a single Line for
#                          each continuous segment of transect effort.  
#      [[4]] tx.SL.LL : lat/long transects represented as a single Line for
#                          each continuous segment of transect effort.  
#      [[5]] txx.SL.prj : projected transects represented as a single Line for
#                           every pair of points in a continuous segment
#                           of transect effort. 
#      [[6]] tx.bfvis.SL.prj : projected transects represented as a single Line for
#                          each continuous segment of transect effort satisfying both 
#                          beauf.x and vis.x. 
#      [[7]] tx.bfvis.SL.LL : lat/long transects represented as a single Line for
#                          each continuous segment of transect effort satisfying both 
#                          beauf.x and vis.x.
#      [[8]] bfvis.tx.list : vector of indices corrsponding to transect data within the 
#                            necessary beauf.x and vis.x parameters
#    b. xtract.ctx.byBeaufVis.SL
#    The function xtract.ctx.SL should be used only for years > 2008 because circling
#    events were not identified prior to 2009.  This function inputs BWASP/COMIDA 
#    data and a proj4string for the desired projection of segments that are output, 
#    and it outputs the following:
#      [[1]] ctx.idx : vector of indices corresponding to circling-on-transect data
#      [[2]] ctx.list : list of indices for individual circling-on-transect segments 
#                       from "ctx.idx" vector
#      [[3]] ctx.SL.prj : projected segments represented as a single Line for
#                          each continuous segment of effort
#      [[4]] ctx.SL.LL : lat/long segments represented as a single Line for
#                          each continuous segment of effort
#      [[5]] ctxx.SL.prj : projected segments represented as a single Line for
#                           every pair of points in a continuous segment
#                           of effort
#      [[6]] ctx.bfvis.SL.prj : projected segments represented as a single Line for
#                          each continuous segment of ctx effort satisfying both beauf.x
#                          and vis.x
#      [[7]] ctx.bfvis.SL.LL : lat/long segments represented as a single Line for
#                          each continuous segment of ctx effort satisfying both beauf.x
#                          and vis.x
#      [[8]] bfvis.ctx.list : vector of indices corrsponding to ctx data within the 
#                             necessary beauf.x and vis.x parameters 
#    c. xtract.srch.byBeaufVis.SL
#    The function xtract.srch.SL inputs BWASP/COMIDA data and a proj4string for the
#    desired projection of segments that are output, and it outputs the following:
#      [[1]] srch.idx : vector of indices corresponding to search and connect data
#      [[2]] srch.list : list of indices for individual search and connect segments 
#                       from "srch.idx" vector
#      [[3]] srch.SL.prj : projected segments represented as a single Line for
#                          each continuous segment of effort
#      [[4]] srch.SL.LL : lat/long segments represented as a single Line for
#                          each continuous segment of effort
#      [[5]] srchh.SL.prj : projected segments represented as a single Line for
#                           every pair of points in a continuous segment
#                           of effort
#      [[6]] srch.bfvis.SL.prj : projected segments represented as a single Line for
#                          each continuous segment of srch effort satisfying both beauf.x
#                          and vis.x
#      [[7]] srch.bfvis.SL.LL : lat/long segments represented as a single Line for
#                          each continuous segment of srch effort satisfying both beauf.x
#                          and vis.x
#      [[8]] bfvis.srch.list : vector of indices corrsponding to search data within the 
#                              necessary beauf.x and vis.x parameters
#    d. xtract.csrch.byBeaufVis.SL
#    The function xtract.csrch.SL should be used only for years > 2008 because circling
#    events were not identified prior to 2009.  This functioninputs BWASP/COMIDA data 
#    and a proj4string for the desired projection of segments that are output, and it
#    outputs the following:
#      [[1]] csrch.idx : vector of indices corresponding to circling-on-search data
#      [[2]] csrch.list : list of indices for individual circling-on-search segments 
#                       from "csrch.idx" vector
#      [[3]] csrch.SL.prj : projected segments represented as a single Line for
#                          each continuous segment of effort
#      [[4]] csrch.SL.LL : lat/long segments represented as a single Line for
#                          each continuous segment of effort
#      [[5]] csrchh.SL.prj : projected segments represented as a single Line for
#                           every pair of points in a continuous segment
#                           of effort
#      [[6]] csrch.bfvis.SL.prj : projected segments represented as a single Line for
#                          each continuous segment of csrch effort satisfying both beauf.x
#                          and vis.x
#      [[7]] csrch.bfvis.SL.LL : lat/long segments represented as a single Line for
#                          each continuous segment of csrch effort satisfying both beauf.x
#                          and vis.x
#      [[8]] bfvis.csrch.list : vector of indices corrsponding to csrch data within the 
#                               necessary beauf.x and vis.x parameters
#    e. xtract.dh.SL
#    The function xtract.dh.SL inputs BWASP/COMIDA data and a proj4string for the
#    desired projection of segments that are output, and it outputs the following:
#      [[1]] dh.idx : vector of indices corresponding to deadhead data
#      [[2]] dh.list : list of indices for individual deadhead segments 
#                       from "dh.idx" vector
#      [[3]] dh.SL.prj : projected segments represented as a single Line for
#                          each continuous segment of effort
#      [[4]] dh.SL.LL : lat/long segments represented as a single Line for
#                          each continuous segment of effort
#      [[5]] dhh.SL.prj : projected segments represented as a single Line for
#                           every pair of points in a continuous segment
#                           of effort
#    f. xtract.flt.SL
#    The function xtract.flt.SL inputs BWASP/COMIDA data and a proj4string for the
#    desired projection of segments that are output, and it outputs the following:
#      [[1]] flt.list : list of indices for individual flts 
#                       from dat
#      [[2]] flt.SL.prj : projected segments represented as a single Line for
#                          each continuous flt
#      [[3]] flt.SL.LL : lat/long segments represented as a single Line for
#                          each continuous flt
#      [[4]] fltt.SL.prj : projected segments represented as a single Line for
#                           every pair of points in a flt
#    g. digest.txx
#    The function digest.txx digests txx.SL.prj objects created using 
#    xtract.tx.SL into a list of SpatialLines comprised of a single Line defined 
#    by only two points.  This function was previously called "digest.traxx"
#
#    h. xtract.ffov.SL
#    The function xtract.ffov.SL inputs BWASP/COMIDA data and a proj4string for the
#    desired projection of segments that are output, and it outputs the following:
#      [[1]] ffov.idx : vector of indices corresponding to ffov data
#      [[2]] ffov.list : list of indices for individual ffov segments 
#                       from "ffov.idx" vector
#      [[3]] ffov.SL.prj : projected segments represented as a single Line for
#                          each continuous segment of effort
#      [[4]] ffov.SL.LL : lat/long segments represented as a single Line for
#                          each continuous segment of effort
#      [[5]] ffovh.SL.prj : projected segments represented as a single Line for
#                           every pair of points in a continuous segment
#                           of effort
#
#    i. xtract.ccaps.SL
#    The function xtract.ccaps.SL inputs BWASP/COMIDA data and a proj4string for the
#    desired projection of segments that are output, and it outputs the following:
#      [[1]] ccaps.idx : vector of indices corresponding to CAPs circling data
#      [[2]] ccaps.list : list of indices for individual CAPs circling segments 
#                       from "ccaps.idx" vector
#      [[3]] ccaps.SL.prj : projected segments represented as a single Line for
#                          each continuous segment of effort
#      [[4]] ccaps.SL.LL : lat/long segments represented as a single Line for
#                          each continuous segment of effort
#      [[5]] ccapsh.SL.prj : projected segments represented as a single Line for
#                           every pair of points in a continuous segment
#                           of effort
#
#    j. xtract.fgf.SL
#    The function xtract.fgf.SL inputs BWASP/COMIDA data and a proj4string for the
#    desired projection of segments that are output, and it outputs the following:
#      [[1]] fgf.idx : vector of indices corresponding to fgf data
#      [[2]] fgf.list : list of indices for individual deadhead segments 
#                       from "fgf.idx" vector
#      [[3]] fgf.SL.prj : projected segments represented as a single Line for
#                          each continuous segment of effort
#      [[4]] fgf.SL.LL : lat/long segments represented as a single Line for
#                          each continuous segment of effort
#      [[5]] fgfh.SL.prj : projected segments represented as a single Line for
#                           every pair of points in a continuous segment
#                           of effort
#
#    k. xtract.capsstrip.SL
#       **Assumes that all CAPs strip effort is immediately followed by CAPs effort.
#       **This fcn combines all continuous CAPs strip effort (regardless of 
#         direction of travel) into a single SL.
#       **The function xtract.capsstrip.SL inputs BWASP/COMIDA data and a proj4string for the
#         desired projection of segments that are output, and it outputs the following:
#         [[1]] capsstrip.idx : vector of indices corresponding to CAPs strip data
#         [[2]] capsstrip.list : list of indices for individual CAPs strip segments 
#                       from "capsstrip.idx" vector
#         [[3]] capsstrip.SL.prj : projected segments represented as a single Line for
#                          each continuous segment of effort
#         [[4]] capsstrip.SL.LL : lat/long segments represented as a single Line for
#                          each continuous segment of effort
#         [[5]] capsstriph.SL.prj : projected segments represented as a single Line for
#                           every pair of points in a continuous segment
#                           of effort
#
#    l. xtract.caps.SL
#       **Assumes that only CAPs strip or CAPs circling follows CAPs effort.
#       **The function xtract.caps.SL inputs BWASP/COMIDA data and a proj4string for the
#         desired projection of segments that are output, and it outputs the following:
#         [[1]] caps.idx : vector of indices corresponding to CAPs passing mode data
#         [[2]] caps.list : list of indices for individual CAPs strip segments 
#                       from "caps.idx" vector
#         [[3]] caps.SL.prj : projected segments represented as a single Line for
#                          each continuous segment of effort
#         [[4]] caps.SL.LL : lat/long segments represented as a single Line for
#                          each continuous segment of effort
#         [[5]] capsh.SL.prj : projected segments represented as a single Line for
#                           every pair of points in a continuous segment
#                           of effort
#
#  8. The byBeaufVis functions will return an object of length 5 if there is not corresponding
#     effort for the x.beauf or vis.x designated in the call to the function.  

  library(sp)
  library(rgdal)
  
  #For debugging.  Good test years are:
  # 2006: 1979-2006 end transect was FltType 3; no enttag
  # 2008: 1979-2008 connect, divert, and resume transect were used
  # 2009: resume and divert to circling started 
  # 2015: latest available survey effort
  # 2018: caps, ffov, and fgf protocols introduced
    debugg <- FALSE
    #debugg <- TRUE
    if(debugg == TRUE){
      #Input and manipulate data
        load("Data//am_all_7915_v3_21_v0_13_v05_ibcaoz.Rdata")
      
        #Create numerical VisL.km, VisR.km, and VisX.km variables
          VisL.km <- rep(NA, nrow(am.all))
          VisL.km[am.all$VisL == "0 km"] <- 0
          VisL.km[am.all$VisL == "<1 km"] <- 0.5
          VisL.km[am.all$VisL == "1-2 km"] <- 1.5
          VisL.km[am.all$VisL == "2-3 km"] <- 2.5
          VisL.km[am.all$VisL == "3-5 km"] <- 4
          VisL.km[am.all$VisL == "5-10 km"] <- 7.5
          VisL.km[am.all$VisL == "Unlimited" | am.all$VisL == "unlimited"] <- 20
          
          VisR.km <- rep(NA, nrow(am.all))
          VisR.km[am.all$VisR == "0 km"] <- 0
          VisR.km[am.all$VisR == "<1 km"] <- 0.5
          VisR.km[am.all$VisR == "1-2 km"] <- 1.5
          VisR.km[am.all$VisR == "2-3 km"] <- 2.5
          VisR.km[am.all$VisR == "3-5 km"] <- 4
          VisR.km[am.all$VisR == "5-10 km"] <- 7.5
          VisR.km[am.all$VisR == "Unlimited" | am.all$VisR == "unlimited"] <- 20
          
          VisX.km <- (VisL.km + VisR.km)/2
          #CK
            summary(VisX.km)
          
        #Fill in non-deadhead auto-updates with a VisX.km value.
          old.VisX.km <- VisX.km[1]  
          for(i in 2:nrow(am.all)){
            new.VisX.km <- VisX.km[i]
            if(is.na(new.VisX.km) == FALSE | 
               (am.all$Entry[i] == "deadhead" & is.na(am.all$Entry[i]) == FALSE)){
              old.VisX.km <- new.VisX.km
            } else {
              VisX.km[i] <- old.VisX.km
            }
          }
          summary(VisX.km)

          am.all <- cbind.data.frame(am.all, VisX.km)
          #CK
            names(am.all)
            
            summary(VisL.km)
            summary(VisL.km[am.all$VisL == "0 km"])
            summary(VisL.km[am.all$VisL == "<1 km"])
            summary(VisL.km[am.all$VisL == "1-2 km"])
            summary(VisL.km[am.all$VisL == "2-3 km"])
            summary(VisL.km[am.all$VisL == "3-5 km"])
            summary(VisL.km[am.all$VisL == "5-10 km"])
            summary(VisL.km[am.all$VisL == "Unlimited" | am.all$VisL == "unlimited"])
            summary(VisL.km[is.na(am.all$VisL) == "TRUE"])
      
            summary(VisR.km)
            summary(VisR.km[am.all$VisR == "0 km"])
            summary(VisR.km[am.all$VisR == "<1 km"])
            summary(VisR.km[am.all$VisR == "1-2 km"])
            summary(VisR.km[am.all$VisR == "2-3 km"])
            summary(VisR.km[am.all$VisR == "3-5 km"])
            summary(VisR.km[am.all$VisR == "5-10 km"])
            summary(VisR.km[am.all$VisR == "Unlimited" | am.all$VisR == "unlimited"])
            summary(VisR.km[is.na(am.all$VisR) == "TRUE"])
            
            summary(am.all$VisX.km)
            
      #Extract year
        #idx <- which(am.all$Yr <= 2015 & am.all$Yr >=2008)
        #idx <- which(am.all$Yr == 2006 |
        #             am.all$Yr == 2008 |
        #             am.all$Yr == 2009 |
        #             am.all$Yr == 2015)
        #idx <- which(am.all$Yr == 2015 & (am.all$FltNo >= 252
        #                                  & am.all$FltNo < 257))
        #idx <- which(am.all$Yr == 2015 & am.all$FltNo == 254)
        idx <- which(am.all$Yr == 2015)
        dat <- am.all[idx,]
      
      #Define map projection for study area.  Use Equidistant Conic (ECproj).
        prj <- CRS('+proj=eqdc +lat_1=60.5d
                              +lat_2=80.5d
                              +lat_0=70.5d
                              +lon_0=-154.5d
                              +x_0=0
                              +y_0=0')
      
      #Set Beaufort Sea State value
        #beauf.x <- 0
        #beauf.x <- 1
        #beauf.x <- 2
        #beauf.x <- 3
        #beauf.x <- 4
        #beauf.x <- 5
        #beauf.x <- 6
        #beauf.x <- 7
        #beauf.x <- 0:4
        #beauf.x <- 0:5
        #beauf.x <- c(NA,0:5)
        #beauf.x <- 0:7
        #beauf.x <- -1
        
      #Set vis.x value
        #vis.x <- 2.0
        #vis.x <- 1.5
        #vis.x <- -1
    }
  
  #Extract continuous transect segments as SpatialLines object
  xtract.tx.byBeaufVis.SL <- function(dat,prj,beauf.x=-1,vis.x=-1){
    #Create SpatialLines objects from transect effort covered during each flight.
    #Extract "transect" flight types and records corresponding to the end of
    #transects (in order to get the full length of transect), then identify
    #subsets of row indices corresponding to individual transects.  Extract
    #only portions of transect flown in Beaufort Sea State beauf.x and visibility
    #Vis.x.
        #Identify start transect, resume, resume transect, p on transect, 
        #s on transect, and auto updates (originally coded as Entry = "."; now NA) 
        #within transects using xFltType==2
          FltTyp2 <- which(dat$xFltType == 2 & 
                           is.na(dat$xFltType) == FALSE)
          #Ck
            summary(dat$xFltType[FltTyp2])   #Should all be 2
            summary(as.factor(as.vector(dat$Entry[FltTyp2])))  #NA correspond to 
                                                               #auto position updates,
                                                               #which were converted from
                                                               #"." to NA in InputAerialMaster...r
        #Identify divert 
          dvt <- which(dat$Entry == "divert")
          #CK
            summary(dat$xFltType[dvt]) 
            summary(as.factor(as.vector(dat$Entry[dvt])))   #Should all be "divert"
        #Identify divert to circling within transect  
          dctx <- which(dat$Entry == "divert to circling" &
                        dat$xFltType == 5)
          #CK
            summary(dat$xFltType[dctx])  #5
            summary(as.factor(as.vector(dat$Entry[dctx])))    #"divert to circling"
        #Identify end transect
          etx <- which(dat$Entry == "end transect")
          #CK
            summary(dat$xFltType[etx])  #3 or 4
            summary(as.factor(as.vector(dat$Entry[etx])))    #"end transect"
        #Create vector of indices corresponding to transect data  
          tx.idx <- sort(c(FltTyp2, etx))
          if(length(dvt) > 0){
            tx.idx <- sort(c(tx.idx, dvt))
          } 
          if(length(dctx) > 0){
            tx.idx <- sort(c(tx.idx, dctx))
          } 
          #Ck
            test <- unique(tx.idx)
            length(tx.idx)
            length(test)
            head(tx.idx, n=100)
            tail(tx.idx, n=100)

        #Extract list of indices for individual transect segments from "tx.idx" vector
          tx.end.idx <- etx
          
          if(length(dvt) > 0){
            tx.end.idx <- sort(c(tx.end.idx, dvt))
          } 
          if(length(dctx) > 0){
            tx.end.idx <- sort(c(tx.end.idx, dctx))
          } 
          
          tx.list <- lapply(1:length(tx.end.idx), function(i){
                       #Identify index of end of tx segment i within
                       #tx.idx
                        end.idx <- which(tx.idx == tx.end.idx[i])
                       #Identify index of start of tx segment i within
                       #tx.idx
                        if(i == 1) {
                          start.idx <- 1
                        } else {
                          start.idx <- which(tx.idx == tx.end.idx[i-1]) + 1
                        }
                       #Create series of all indices in tx segment i
                        t.idx <- start.idx:end.idx
                        return(t.idx)
                      })
            #CK
              class(tx.list)  #list
              length(tx.list) #num transect segments
              tx.list[[1]]    #should be sequential, increasing, no missing values
              tx.list[[length(tx.list)]]  #should be sequential, increasing, no missing values
        
          #Create segments satisfying beauf.x and vis.x parameters
            #Extract indices only for beauf.x and vis.x
              if(beauf.x[1] > -1 | is.na(beauf.x[1]) == TRUE | vis.x > -1){
                bfvis.tx.list <- lapply(1:length(tx.list), function(i){
                #bfvis.tx.list <- lapply(103:103, function(i){
                  tx.i.tf <- (dat$iBeauf[tx.idx[tx.list[[i]]]] %in% beauf.x) & 
                             (dat$VisX.km[tx.idx[tx.list[[i]]]] >= vis.x) 
                  
                  tx.i.segs <- rep(NA, length(tx.i.tf))
                  seg.idx <- 1
                  for(j in 1:length(tx.i.tf)){
                    if(is.na(tx.i.tf[j]) == FALSE & tx.i.tf[j] == TRUE){
                      tx.i.segs[j] <- seg.idx
                    } else {
                      seg.idx <- seg.idx + 1
                    }
                  }
                  
                  seg.id <- unique(tx.i.segs)
                  seg.id <- seg.id[is.na(seg.id)==FALSE]
                   
                  if(length(seg.id) > 0){ # >= 1 valid beauf.x and vis.x in tx.list[[i]]...
                    tx.i.seg.list <- lapply(1:length(seg.id), function(j){
                      j.idx <- which(tx.i.segs == seg.id[j])
                      seg.j <- tx.list[[i]][j.idx]
                      
                      if(seg.j[length(seg.j)] != tx.list[[i]][length(tx.list[[i]])]){
                        seg.j <- c(seg.j, tx.list[[i]][max(j.idx)+1] )
                      }
                      
                      #Return a point for "segments" that comprise only an 
                      #end transect event
                      if(length(seg.j) > 1){
                        return(seg.j)
                      } else {
                        #return(NA)
                        return(c(seg.j, seg.j)) #Duplicate a single point
                      }
                      
                    })
                  
                    return(list("tx.i.seg.list"=tx.i.seg.list, "n.segs"=length(tx.i.seg.list)))
                    
                  } else { #no valid beauf.x or vis.x in tx.list[[i]]...
                    return(NA)
                  }

                })
                                
              } else {
                bfvis.tx.list <- NA
              }
          
        #Create SpatialLines objects for transects two (no beauf.x or vis.x) or three 
        #(with beauf.x or vis.x) ways:
        #  1. As a single Line for each continuous segment of transect
        #     effort
        #  2. As a single Line for every pair of points in a continuous segment
        #     of transect effort
        #  3. As a single Line for every continuous beauf.x and vis.x segment of transect
        #     effort, if beauf.x or vis.x is specified
          #First, as a single long line
            x.flt <- 360 + dat$ArcLong
            y.flt <- dat$ArcLat
            xy.flt <- cbind(x.flt,y.flt)
            xy.flt.SPts <- SpatialPoints(xy.flt, proj4string=CRS('+proj=longlat'))
            xy.flt.SPts.prj <- spTransform(xy.flt.SPts, prj)
          
            tx.SL.prj <- SpatialLines(list(Lines(lapply(1:length(tx.list), function(i){
            #tx.SL.prj <- SpatialLines(list(Lines(lapply(1:1, function(i){
                                      #Extract coordinates of points on transect i
                                        xy <- xy.flt.SPts.prj@coords[tx.idx[tx.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="PRJ")), proj=prj)
          
            tx.SL.LL <- SpatialLines(list(Lines(lapply(1:length(tx.list), function(i){
            #tx.SL.LL <- SpatialLines(list(Lines(lapply(371:371, function(i){
                                      #Extract coordinates of points on transect i
                                        xy <- xy.flt.SPts@coords[tx.idx[tx.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="LL")), proj=CRS('+proj=longlat'))
          
          #Second, as multiple short lines
            txx.SL.prj <- SpatialLines(lapply(1:length(tx.list), function(i){
                              #Extract coordinates of points on transect i
                                xy <- xy.flt.SPts.prj@coords[tx.idx[tx.list[[i]]],]
                              #Create lines from pairs of coords
                                Lines.i <- Lines(lapply(1:(nrow(xy)-1), function(j){
                                             L <- Line(xy[j:(j+1),])
                                             return(L)
                                           }), ID=as.character(i))
                              }), proj=prj)

          if(beauf.x[1] > -1 | is.na(beauf.x[1]) == TRUE | vis.x > -1){        
            #Third, one line per stretch of beauf.x and vis.x transect
              bfvis.tx.list.len <- sapply(1:length(bfvis.tx.list), function(i){
                return(length(bfvis.tx.list[[i]]))
              })
              bfvis.tx.list.idx <- which(bfvis.tx.list.len > 1)
          
              if(length(bfvis.tx.list.idx) > 0){
                tx.bfvis.SL.prj <- SpatialLines(lapply(bfvis.tx.list.idx, function(i){
                #tx.bfvis.SL.prj <- SpatialLines(lapply(bfvis.tx.list.idx[1:100], function(i){  
                #tx.bfvis.SL.prj <- SpatialLines(lapply(563, function(i){                            
                                    Lines.i <- Lines(lapply(1:bfvis.tx.list[[i]]$n.segs, function(j){
                                      #Extract coordinates of points on beauf.x & vis.x seg j
                                        xy <- xy.flt.SPts.prj@coords[tx.idx[bfvis.tx.list[[i]]$tx.i.seg.list[[j]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                    }), ID=i)}), proj=prj)
            
                tx.bfvis.SL.LL <- SpatialLines(lapply(bfvis.tx.list.idx, function(i){
                #tx.bfvis.SL.LL <- SpatialLines(lapply(bfvis.tx.list.idx[1:100], function(i){  
                #tx.bfvis.SL.LL <- SpatialLines(lapply(563, function(i){                            
                                    Lines.i <- Lines(lapply(1:bfvis.tx.list[[i]]$n.segs, function(j){
                                      #Extract coordinates of points on beauf.x & vis.x seg j
                                        xy <- xy.flt.SPts@coords[tx.idx[bfvis.tx.list[[i]]$tx.i.seg.list[[j]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                    }), ID=i)}), proj=CRS('+proj=longlat')) 
              
                return.list <- list(tx.idx,tx.list,tx.SL.prj,tx.SL.LL,txx.SL.prj,
                          tx.bfvis.SL.prj, tx.bfvis.SL.LL, bfvis.tx.list)
              } else {
                return.list <- list(tx.idx,tx.list,tx.SL.prj,tx.SL.LL,txx.SL.prj)
              }
          } else {
            return.list <- list(tx.idx,tx.list,tx.SL.prj,tx.SL.LL,txx.SL.prj)
          } 
            
          return(return.list)  
  }
  
  
  
  
  
  
  
  
  #Extract continuous circling-on-transect segments as SpatialLines object
  xtract.ctx.byBeaufVis.SL <- function(dat,prj,beauf.x=-1,vis.x=-1){
    #Create SpatialLines objects from circling-on-transect effort covered 
    #during each flight.  Extract "circling - transect" flight type 5 and 
    #"resume" records corresponding to the return to transects 
    #(in order to get the full length of circling effort), then identify
    #subsets of row indices corresponding to individual circling sections.
        #Identify divert to circling (on transect), p on circling - transect,
        #s on circling - transect, and associated auto updates 
        #(originally coded as Entry = "."; now NA) using xFltType==5
          FltTyp5 <- which(dat$xFltType == 5 & 
                           is.na(dat$xFltType) == FALSE)
          #Ck
            summary(dat$xFltType[FltTyp5])   #Should all be 5
            summary(as.factor(as.vector(dat$Entry[FltTyp5])))  #NA correspond to 
                                                               #auto position updates,
                                                               #which were converted from
                                                               #"." to NA in InputAerialMaster...r
        
        #Identify resume to transect records
          rtx <- which(dat$Entry == "resume" &
                       dat$xFltType == 2)
          #CK
            summary(dat$xFltType[rtx])  #2
            summary(as.factor(as.vector(dat$Entry[rtx])))    #"resume"
        
        #Create vector of indices corresponding to ctx data 
          if(length(FltTyp5) > 0 & 
             length(rtx) > 0){
            ctx.idx <- sort(c(FltTyp5, rtx))
          } else {
            ctx.idx <- NA
          }
          #CK
            test <- unique(ctx.idx)
            length(test)
            length(ctx.idx)
            head(ctx.idx, n=20)
            tail(ctx.idx, n=20)
 
        #Extract list of indices for individual circling sections from "ctx.idx" vector
          ctx.list <- lapply(1:length(rtx), function(i){
                       #Identify index of end of ctx section i within
                       #ctx.idx
                        end.idx <- which(ctx.idx == rtx[i])
                       #Identify index of start of ctx section i within
                       #ctx.idx
                        if(i == 1) {
                          start.idx <- 1
                        } else {
                          start.idx <- which(ctx.idx == rtx[i-1]) + 1
                        }
                       #Create series of all indices in ctx section i
                        ct.idx <- start.idx:end.idx
                        return(ct.idx)
                      })
            #CK
              class(ctx.list)  #list
              length(ctx.list) #num circling-on-transect sections
              ctx.list[[1]]    #should be sequential, increasing, no missing values
              ctx.list[[length(ctx.list)]]  #should be sequential, increasing, no missing values

          #Create segments satisfying beauf.x and vis.x parameters
            #Extract indices only for beauf.x and vis.x
              if(beauf.x[1] > -1 | is.na(beauf.x[1]) == TRUE | vis.x > -1){
                bfvis.ctx.list <- lapply(1:length(ctx.list), function(i){
                  ctx.i.tf <- (dat$iBeauf[ctx.idx[ctx.list[[i]]]] %in% beauf.x) &
                              (dat$VisX.km[ctx.idx[ctx.list[[i]]]] >= vis.x)
                  
                  ctx.i.segs <- rep(NA, length(ctx.i.tf))
                  seg.idx <- 1
                  for(j in 1:length(ctx.i.tf)){
                    if(ctx.i.tf[j] == TRUE){
                      ctx.i.segs[j] <- seg.idx
                    } else {
                      seg.idx <- seg.idx + 1
                    }
                  }
                  
                  seg.id <- unique(ctx.i.segs)
                  seg.id <- seg.id[is.na(seg.id)==FALSE]
                   
                  if(length(seg.id) > 0){ # >= 1 valid beauf.x and vis.x in ctx.list[[i]]...
                    ctx.i.seg.list <- lapply(1:length(seg.id), function(j){
                      j.idx <- which(ctx.i.segs == seg.id[j])
                      seg.j <- ctx.list[[i]][j.idx]
                      
                      if(seg.j[length(seg.j)] != ctx.list[[i]][length(ctx.list[[i]])]){
                        seg.j <- c(seg.j, ctx.list[[i]][max(j.idx)+1] )
                      }
                      
                      #Return a point for "segments" that comprise only a final 
                      #event
                      if(length(seg.j) > 1){
                        return(seg.j)
                      } else {
                        #return(NA)
                        return(c(seg.j, seg.j)) #Duplicate a single point
                      }
                      
                    })
                    
                    return(list("ctx.i.seg.list"=ctx.i.seg.list, "n.segs"=length(ctx.i.seg.list)))
                    
                  } else { #no valid beauf.x or vis.x in ctx.list[[i]]...
                    return(NA)
                  }
                  
                })
                                
              } else {
                bfvis.ctx.list <- NA
              }
                  
        #Create SpatialLines objects for circling sections two (no beauf.x or vis.x) or three 
        #(with beauf.x or vis.x) ways:
        #  1. As a single Line for each continuous section
        #  2. As a single Line for every pair of points in a continuous section
        #  3. As a single Line for every continuous beauf.x and vis.x segment of ctx
        #     effort, if beauf.x or vis.x is specified
          #First, as a single long line
            x.flt <- 360 + dat$ArcLong
            y.flt <- dat$ArcLat
            xy.flt <- cbind(x.flt,y.flt)
            xy.flt.SPts <- SpatialPoints(xy.flt, proj4string=CRS('+proj=longlat'))
            xy.flt.SPts.prj <- spTransform(xy.flt.SPts, prj)
          
            ctx.SL.prj <- SpatialLines(list(Lines(lapply(1:length(ctx.list), function(i){
            #ctx.SL.prj <- SpatialLines(list(Lines(lapply(305:305, function(i){
                                      #Extract coordinates of points on section i
                                        xy <- xy.flt.SPts.prj@coords[ctx.idx[ctx.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="PRJ")), proj=prj)
          
            ctx.SL.LL <- SpatialLines(list(Lines(lapply(1:length(ctx.list), function(i){
            #ctx.SL.LL <- SpatialLines(list(Lines(lapply(371:371, function(i){
                                      #Extract coordinates of points on section i
                                        xy <- xy.flt.SPts@coords[ctx.idx[ctx.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="LL")), proj=CRS('+proj=longlat'))
          
          #Second, as multiple short lines
            ctxx.SL.prj <- SpatialLines(lapply(1:length(ctx.list), function(i){
                              #Extract coordinates of points on section i
                                xy <- xy.flt.SPts.prj@coords[ctx.idx[ctx.list[[i]]],]
                              #Create lines from pairs of coords
                                Lines.i <- Lines(lapply(1:(nrow(xy)-1), function(j){
                                             L <- Line(xy[j:(j+1),])
                                             return(L)
                                           }), ID=as.character(i))
                              }), proj=prj)
          
          if(beauf.x[1] > -1 | is.na(beauf.x[1]) == TRUE | vis.x > -1){        
            #Third, one line per stretch of beauf.x and vis.x ctx 
              bfvis.ctx.list.len <- sapply(1:length(bfvis.ctx.list), function(i){
                return(length(bfvis.ctx.list[[i]]))
              })
              bfvis.ctx.list.idx <- which(bfvis.ctx.list.len > 1)
          
              if(length(bfvis.ctx.list.idx) > 0){
                ctx.bfvis.SL.prj <- SpatialLines(lapply(bfvis.ctx.list.idx, function(i){
                #ctx.bfvis.SL.prj <- SpatialLines(lapply(bfvis.ctx.list.idx[1:100], function(i){  
                #ctx.bfvis.SL.prj <- SpatialLines(lapply(563, function(i){                            
                                    Lines.i <- Lines(lapply(1:bfvis.ctx.list[[i]]$n.segs, function(j){
                                      #Extract coordinates of points on beauf.x and vis.x seg j
                                        xy <- xy.flt.SPts.prj@coords[ctx.idx[bfvis.ctx.list[[i]]$ctx.i.seg.list[[j]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                    }), ID=i)}), proj=prj)
            
                ctx.bfvis.SL.LL <- SpatialLines(lapply(bfvis.ctx.list.idx, function(i){
                #ctx.bfvis.SL.LL <- SpatialLines(lapply(bfvis.ctx.list.idx[1:100], function(i){  
                #ctx.bfvis.SL.LL <- SpatialLines(lapply(563, function(i){                            
                                    Lines.i <- Lines(lapply(1:bfvis.ctx.list[[i]]$n.segs, function(j){
                                      #Extract coordinates of points on beauf.x and vis.x seg j
                                        xy <- xy.flt.SPts@coords[ctx.idx[bfvis.ctx.list[[i]]$ctx.i.seg.list[[j]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                    }), ID=i)}), proj=CRS('+proj=longlat')) 
              
                return.list <- list(ctx.idx,ctx.list,ctx.SL.prj,ctx.SL.LL,ctxx.SL.prj,
                          ctx.bfvis.SL.prj, ctx.bfvis.SL.LL, bfvis.ctx.list)
              } else {
                return.list <- list(ctx.idx,ctx.list,ctx.SL.prj,ctx.SL.LL,ctxx.SL.prj)
              }
          } else {
            return.list <- list(ctx.idx,ctx.list,ctx.SL.prj,ctx.SL.LL,ctxx.SL.prj)
          }  
            
          return(return.list)  
        
  }
  
  
  
  
  
  
  
  
  
  
  #Extract continuous circling-on-search segments as SpatialLines object
  xtract.csrch.byBeaufVis.SL <- function(dat,prj,beauf.x=-1,vis.x=-1){
    #Create SpatialLines objects from circling-on-search effort covered 
    #during each flight.  Extract "circling - search" flight type 6 and 
    #"resume" records corresponding to the return to search 
    #(in order to get the full length of circling effort), then identify
    #subsets of row indices corresponding to individual circling sections.
        #Identify divert to circling (on search), p on circling - search,
        #s on circling - search, and associated auto updates 
        #(originally coded as Entry = "."; now NA) using xFltType==6
          FltTyp6 <- which(dat$xFltType == 6 & 
                           is.na(dat$xFltType) == FALSE)
          #Ck
            summary(dat$xFltType[FltTyp6])   #Should all be 6
            summary(as.factor(as.vector(dat$Entry[FltTyp6])))  #NA correspond to 
                                                               #auto position updates,
                                                               #which were converted from
                                                               #"." to NA in InputAerialMaster...r
        
        #Identify resume to search records
          rsrch <- which(dat$Entry == "resume" &
                       dat$xFltType == 4)
          #CK
            summary(dat$xFltType[rsrch])  #4
            summary(as.factor(as.vector(dat$Entry[rsrch])))    #"resume"
        
        #Create vector of indices corresponding to csrch data 
          if(length(FltTyp6) > 0 & 
             length(rsrch) > 0){
            csrch.idx <- sort(c(FltTyp6, rsrch))
          } else {
            csrch.idx <- NA
          }
          #CK
            test <- unique(csrch.idx)
            length(test)
            length(csrch.idx)
            head(csrch.idx, n=20)
            tail(csrch.idx, n=20)
 
        #Extract list of indices for individual circling sections from "csrch.idx" vector
          csrch.list <- lapply(1:length(rsrch), function(i){
                       #Identify index of end of csrch section i within
                       #csrch.idx
                        end.idx <- which(csrch.idx == rsrch[i])
                       #Identify index of start of csrch section i within
                       #csrch.idx
                        if(i == 1) {
                          start.idx <- 1
                        } else {
                          start.idx <- which(csrch.idx == rsrch[i-1]) + 1
                        }
                       #Create series of all indices in csrch section i
                        cs.idx <- start.idx:end.idx
                        return(cs.idx)
                      })
            #CK
              class(csrch.list)  #list
              length(csrch.list) #num circling-on-search sections
              csrch.list[[1]]    #should be sequential, increasing, no missing values
              csrch.list[[length(csrch.list)]]  #should be sequential, increasing, no missing values

          #Create segments satisfying beauf.x and vis.x parameters
            #Extract indices only for beauf.x and vis.x
              if(beauf.x[1] > -1 | is.na(beauf.x[1]) == TRUE | vis.x > -1){
                bfvis.csrch.list <- lapply(1:length(csrch.list), function(i){
                  csrch.i.tf <- (dat$iBeauf[csrch.idx[csrch.list[[i]]]] %in% beauf.x) &
                                (dat$VisX.km[csrch.idx[csrch.list[[i]]]] >= vis.x)
                  
                  csrch.i.segs <- rep(NA, length(csrch.i.tf))
                  seg.idx <- 1
                  for(j in 1:length(csrch.i.tf)){
                    if(csrch.i.tf[j] == TRUE){
                      csrch.i.segs[j] <- seg.idx
                    } else {
                      seg.idx <- seg.idx + 1
                    }
                  }
                  
                  seg.id <- unique(csrch.i.segs)
                  seg.id <- seg.id[is.na(seg.id)==FALSE]
                   
                  if(length(seg.id) > 0){ # >= 1 valid beauf.x and vis.x in csrch.list[[i]]...
                    csrch.i.seg.list <- lapply(1:length(seg.id), function(j){
                      j.idx <- which(csrch.i.segs == seg.id[j])
                      seg.j <- csrch.list[[i]][j.idx]
                      
                      if(seg.j[length(seg.j)] != csrch.list[[i]][length(csrch.list[[i]])]){
                        seg.j <- c(seg.j, csrch.list[[i]][max(j.idx)+1] )
                      }
                      
                      #Return a point for "segments" that comprise only an 
                      #end transect event
                      if(length(seg.j) > 1){
                        return(seg.j)
                      } else {
                        #return(NA)
                        return(c(seg.j, seg.j)) #Duplicate a single point
                      }
                      
                    })
                    
                    return(list("csrch.i.seg.list"=csrch.i.seg.list, "n.segs"=length(csrch.i.seg.list)))
                    
                  } else { #no valid beauf.x or vis.x in csrch.list[[i]]...
                    return(NA)
                  }
                  
                })
                                
              } else {
                bfvis.csrch.list <- NA
              }
                
        #Create SpatialLines objects for circling sections two (no beauf.x or vis.x) or three 
        #(with beauf.x or vis.x) ways:
        #  1. As a single Line for each continuous section
        #  2. As a single Line for every pair of points in a continuous section
        #  3. As a single Line for every continuous beauf.x and vis.x segment of csrch
        #     effort, if beauf.x or vis.x is specified
          #First, as a single long line
            x.flt <- 360 + dat$ArcLong
            y.flt <- dat$ArcLat
            xy.flt <- cbind(x.flt,y.flt)
            xy.flt.SPts <- SpatialPoints(xy.flt, proj4string=CRS('+proj=longlat'))
            xy.flt.SPts.prj <- spTransform(xy.flt.SPts, prj)
          
            csrch.SL.prj <- SpatialLines(list(Lines(lapply(1:length(csrch.list), function(i){
            #csrch.SL.prj <- SpatialLines(list(Lines(lapply(305:305, function(i){
                                      #Extract coordinates of points on section i
                                        xy <- xy.flt.SPts.prj@coords[csrch.idx[csrch.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="PRJ")), proj=prj)
          
            csrch.SL.LL <- SpatialLines(list(Lines(lapply(1:length(csrch.list), function(i){
            #csrch.SL.LL <- SpatialLines(list(Lines(lapply(371:371, function(i){
                                      #Extract coordinates of points on section i
                                        xy <- xy.flt.SPts@coords[csrch.idx[csrch.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="LL")), proj=CRS('+proj=longlat'))
          
          #Second, as multiple short lines
            csrchh.SL.prj <- SpatialLines(lapply(1:length(csrch.list), function(i){
                              #Extract coordinates of points on section i
                                xy <- xy.flt.SPts.prj@coords[csrch.idx[csrch.list[[i]]],]
                              #Create lines from pairs of coords
                                Lines.i <- Lines(lapply(1:(nrow(xy)-1), function(j){
                                             L <- Line(xy[j:(j+1),])
                                             return(L)
                                           }), ID=as.character(i))
                              }), proj=prj)
        
          if(beauf.x[1] > -1 | is.na(beauf.x[1]) == TRUE | vis.x > -1){        
            #Third, one line per stretch of beauf.x 
              bfvis.csrch.list.len <- sapply(1:length(bfvis.csrch.list), function(i){
                return(length(bfvis.csrch.list[[i]]))
              })
              bfvis.csrch.list.idx <- which(bfvis.csrch.list.len > 1)
          
              if(length(bfvis.csrch.list.idx) > 0){
                csrch.bfvis.SL.prj <- SpatialLines(lapply(bfvis.csrch.list.idx, function(i){
                #csrch.bfvis.SL.prj <- SpatialLines(lapply(bfvis.csrch.list.idx[1:100], function(i){  
                #csrch.bfvis.SL.prj <- SpatialLines(lapply(563, function(i){                            
                                    Lines.i <- Lines(lapply(1:bfvis.csrch.list[[i]]$n.segs, function(j){
                                      #Extract coordinates of points on beauf.x and vis.x seg j
                                        xy <- xy.flt.SPts.prj@coords[csrch.idx[bfvis.csrch.list[[i]]$csrch.i.seg.list[[j]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                    }), ID=i)}), proj=prj)
            
                csrch.bfvis.SL.LL <- SpatialLines(lapply(bfvis.csrch.list.idx, function(i){
                #csrch.bfvis.SL.LL <- SpatialLines(lapply(bfvis.csrch.list.idx[1:100], function(i){  
                #csrch.bfvis.SL.LL <- SpatialLines(lapply(563, function(i){                            
                                    Lines.i <- Lines(lapply(1:bfvis.csrch.list[[i]]$n.segs, function(j){
                                      #Extract coordinates of points on beauf.x and vis.x seg j
                                        xy <- xy.flt.SPts@coords[csrch.idx[bfvis.csrch.list[[i]]$csrch.i.seg.list[[j]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                    }), ID=i)}), proj=CRS('+proj=longlat')) 
              
                return.list <- list(csrch.idx,csrch.list,csrch.SL.prj,csrch.SL.LL,csrchh.SL.prj,
                          csrch.bfvis.SL.prj, csrch.bfvis.SL.LL, bfvis.csrch.list)
              } else {
                return.list <- list(csrch.idx,csrch.list,csrch.SL.prj,csrch.SL.LL,csrchh.SL.prj)
              }
          } else {
            return.list <- list(csrch.idx,csrch.list,csrch.SL.prj,csrch.SL.LL,csrchh.SL.prj)
          }
            
          return(return.list)
        
  }

  
  
  
  
  
  
  
  
  
  
  
  
  #Extract continuous search segments as SpatialLines object
  xtract.srch.byBeaufVis.SL <- function(dat,prj,beauf.x=-1,vis.x=-1){
    #Create SpatialLines objects from search & connect effort covered during each 
    #flt.  Identify the following:
    #  a. "search" and "connect" flight types via xFltType == 3 or 4
    #      --> for "end transect", beware solo records  
    #  b. {"divert to circling" & "xFltType == 6"}
    #  c. "resume transect" always marks an end to search effort
    #  d. "start transect", "deadhead", "CAPs" (directly after "end transect"), 
    #     "FFOV", and "FGF" may follow search effort.  
    #Then, identify subsets of row indices corresponding to individual search 
    #segments.
      #Identify search and connect flight types with xFltType==3 and xFltType==4
        FltTyp3.4 <- which((dat$xFltType == 3 | dat$xFltType == 4) & 
                           is.na(dat$xFltType) == FALSE)
          #Ck
            summary(dat$xFltType[FltTyp3.4])   #Should all be 3 or 4
            summary(as.factor(as.vector(dat$Entry[FltTyp3.4])))  #NA correspond to 
                                                               #auto position updates,
                                                               #which were converted from
                                                               #"." to NA in InputAerialMaster...r
      #Identify divert to circling within search  
        dcsrch <- which(dat$Entry == "divert to circling" &
                      dat$xFltType == 6)
        #CK
          summary(dat$xFltType[dcsrch])  #6
          summary(as.factor(as.vector(dat$Entry[dcsrch])))    #"divert to circling"
      #Identify resume transect
        rtx <- which(dat$Entry == "resume transect")
        #CK
          summary(dat$xFltType[rtx])  #2
          summary(as.factor(as.vector(dat$Entry[rtx])))    #"resume transect"
      #Identify "start transect", "deadhead", "CAPs", "FGF", and "FFOV" events that mark 
      #the end of search
        start.ded.idx <- which(dat$Entry == "start transect" |
                               dat$Entry == "deadhead" | 
                               dat$Entry == "FGF" |
                               dat$Entry == "FFOV" | 
                               dat$Entry == "CAPs")
        length(start.ded.idx)
        start.ded.idx <- start.ded.idx[start.ded.idx != 1]
        length(start.ded.idx) #Should be at most 1 shorter than previous length
        prev.xFltType <- dat$xFltType[start.ded.idx - 1]
        end.srch.idx <- start.ded.idx[prev.xFltType == 4 | 
                                      prev.xFltType == 3]
        #Ck
          summary(as.factor(prev.xFltType)) #Should be 1, 3, 4, 8, 9, or 10 bc can go from 
                                            #CAPs circling (FltType 9) to start transect
          summary(as.factor(dat$xFltType[end.srch.idx - 1]))  #Should be 3 or 4
          summary(as.factor(dat$xFltType[end.srch.idx]))  #Should be 1, 2, 8, or 10
          summary(as.factor(as.vector(dat$Entry[end.srch.idx])))  #Should be 
                                                                  #"start transect" or         
                                                                  #"deadhead" or "FFOV"
                                                                  #or "FGF" or "CAPs"

        #Create vector of indices corresponding to search data
          srch.idx <- sort(c(FltTyp3.4, dcsrch, rtx, end.srch.idx))
          #Ck
            test <- unique(srch.idx)
            length(srch.idx)
            length(test)
            head(srch.idx, n=100)
            tail(srch.idx, n=100)
        
        #Extract list of indices for individual search segments from "srch.idx"
          srch.end.idx <- sort(c(rtx, dcsrch, end.srch.idx))
          
          srch.list <- lapply(1:length(srch.end.idx), function(i){
                         #Identify index of end of srch segment i within
                         #srch.idx
                          end.idx <- which(srch.idx == srch.end.idx[i])
                         #Identify index of start of srch segment i within
                         #srch.idx
                          if(i == 1) {
                            start.idx <- 1
                          } else {
                            start.idx <- which(srch.idx == srch.end.idx[i-1]) + 1
                          }
                         #Create series of all indices in srch segment i.  Duplicate
                         #points for single-point series in order to create a 
                         #SpatialLine object
                          if(start.idx != end.idx){
                            s.idx <- start.idx:end.idx
                          } else {
                            s.idx <- c(start.idx,end.idx) #duplicate a single point
                          }  
                          return(s.idx)
                        })
            #CK
              class(srch.list)  #list
              length(srch.list) #num srch segments
              srch.list[[1]]    #should be sequential, increasing, no missing values
              srch.list[[length(srch.list)]]  #should be sequential, increasing, no missing values
          
          #Create segments satisfying beauf.x and vis.x parameters
            #Extract indices only for beauf.x and vis.x.  Allow for vis.x=NA because deadhead effort
            #may be included.
              if(beauf.x[1] > -1 | is.na(beauf.x[1]) == TRUE | vis.x > -1){
                bfvis.srch.list <- lapply(1:length(srch.list), function(i){
                  srch.i.tf <- (dat$iBeauf[srch.idx[srch.list[[i]]]] %in% beauf.x) &
                               (dat$VisX.km[srch.idx[srch.list[[i]]]] >= vis.x | 
                                is.na(dat$VisX.km[srch.idx[srch.list[[i]]]]) == TRUE)
                  
                  srch.i.segs <- rep(NA, length(srch.i.tf))
                  seg.idx <- 1
                  for(j in 1:length(srch.i.tf)){
                    if(srch.i.tf[j] == TRUE){
                      srch.i.segs[j] <- seg.idx
                    } else {
                      seg.idx <- seg.idx + 1
                    }
                  }
                  
                  seg.id <- unique(srch.i.segs)
                  seg.id <- seg.id[is.na(seg.id)==FALSE]
                   
                  if(length(seg.id) > 0){ # >= 1 valid beauf.x and vis.x in srch.list[[i]]...
                    srch.i.seg.list <- lapply(1:length(seg.id), function(j){
                      j.idx <- which(srch.i.segs == seg.id[j])
                      seg.j <- srch.list[[i]][j.idx]
                      
                      if(seg.j[length(seg.j)] != srch.list[[i]][length(srch.list[[i]])]){
                        seg.j <- c(seg.j, srch.list[[i]][max(j.idx)+1] )
                      }
                      
                      #Return a point for "segments" that comprise only an 
                      #end transect event
                      if(length(seg.j) > 1){
                        return(seg.j)
                      } else {
                        #return(NA)
                        return(c(seg.j, seg.j)) #Duplicate a single point
                      }
                      
                    })
                    
                    return(list("srch.i.seg.list"=srch.i.seg.list, "n.segs"=length(srch.i.seg.list)))
                    
                  } else { #no valid beauf.x or vis.x in srch.list[[i]]...
                    return(NA)
                  }
                  
                })
                                
              } else {
                bfvis.srch.list <- NA
              }
      
        #Create SpatialLines objects for search two (no beauf.x or vis.x) or three 
        #(with beauf.x or vis.x) ways:
        #  1. As a single Line for each continuous segment of search
        #     effort
        #  2. As a single Line for every pair of points in a continuous segment
        #     of search effort
        #  3. As a single Line for every continuous beauf.x and vis.x segment of srch
        #     effort, if beauf.x or vis.x is specified
          #First, as a single long line
            x.flt <- 360 + dat$ArcLong
            y.flt <- dat$ArcLat
            xy.flt <- cbind(x.flt,y.flt)
            xy.flt.SPts <- SpatialPoints(xy.flt, proj4string=CRS('+proj=longlat'))
            xy.flt.SPts.prj <- spTransform(xy.flt.SPts, prj)
          
            srch.SL.prj <- SpatialLines(list(Lines(lapply(1:length(srch.list), function(i){
            #srch.SL.prj <- SpatialLines(list(Lines(lapply(1:1, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts.prj@coords[srch.idx[srch.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="PRJ")), proj=prj)
          
            srch.SL.LL <- SpatialLines(list(Lines(lapply(1:length(srch.list), function(i){
            #srch.SL.LL <- SpatialLines(list(Lines(lapply(371:371, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts@coords[srch.idx[srch.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="LL")), proj=CRS('+proj=longlat'))
          
          #Second, as multiple short lines
            srchh.SL.prj <- SpatialLines(lapply(1:length(srch.list), function(i){
                              #Extract coordinates of points on seg i
                                xy <- xy.flt.SPts.prj@coords[srch.idx[srch.list[[i]]],]
                              #Create lines from pairs of coords
                                Lines.i <- Lines(lapply(1:(nrow(xy)-1), function(j){
                                             L <- Line(xy[j:(j+1),])
                                             return(L)
                                           }), ID=as.character(i))
                              }), proj=prj)
          
          if(beauf.x[1] > -1 | is.na(beauf.x[1]) == TRUE | vis.x > -1){        
            #Third, one line per stretch of beauf.x transect
              bfvis.srch.list.len <- sapply(1:length(bfvis.srch.list), function(i){
                return(length(bfvis.srch.list[[i]]))
              })
              bfvis.srch.list.idx <- which(bfvis.srch.list.len > 1)
          
              if(length(bfvis.srch.list.idx) > 0){
                srch.bfvis.SL.prj <- SpatialLines(lapply(bfvis.srch.list.idx, function(i){
                #srch.bfvis.SL.prj <- SpatialLines(lapply(bfvis.srch.list.idx[1:100], function(i){  
                #srch.bfvis.SL.prj <- SpatialLines(lapply(563, function(i){                            
                                    Lines.i <- Lines(lapply(1:bfvis.srch.list[[i]]$n.segs, function(j){
                                      #Extract coordinates of points on beauf.x and vis.x seg j
                                        xy <- xy.flt.SPts.prj@coords[srch.idx[bfvis.srch.list[[i]]$srch.i.seg.list[[j]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                    }), ID=i)}), proj=prj)
            
                srch.bfvis.SL.LL <- SpatialLines(lapply(bfvis.srch.list.idx, function(i){
                #srch.bfvis.SL.LL <- SpatialLines(lapply(bfvis.srch.list.idx[1:100], function(i){  
                #srch.bfvis.SL.LL <- SpatialLines(lapply(563, function(i){                            
                                    Lines.i <- Lines(lapply(1:bfvis.srch.list[[i]]$n.segs, function(j){
                                      #Extract coordinates of points on beauf.x and vis.x seg j
                                        xy <- xy.flt.SPts@coords[srch.idx[bfvis.srch.list[[i]]$srch.i.seg.list[[j]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                    }), ID=i)}), proj=CRS('+proj=longlat')) 
              
                return.list <- list(srch.idx,srch.list,srch.SL.prj,srch.SL.LL,srchh.SL.prj,
                          srch.bfvis.SL.prj, srch.bfvis.SL.LL, bfvis.srch.list)
              } else {
                return.list <- list(srch.idx,srch.list,srch.SL.prj,srch.SL.LL,srchh.SL.prj)
              }
          } else {
            return.list <- list(srch.idx,srch.list,srch.SL.prj,srch.SL.LL,srchh.SL.prj)
          }  
            
          return(return.list)  
  }

  
  
  
  
  
  
  
  #Extract continuous deadhead segments as SpatialLines object
  xtract.dh.SL <- function(dat,prj){
    #Create SpatialLines objects from deadhead effort covered during each 
    #flt.  Identify the following:
    #  a. "deadhead" flight type via xFltType == 1
    #  b. "start transect", "search", and "p on connect" may follow 
    #      deadhead effort.  
    #Then, identify subsets of row indices corresponding to individual
    #deadhead segments.
      #Identify deadhead flight type with xFltType==1
        FltTyp1 <- which(dat$xFltType == 1 & 
                           is.na(dat$xFltType) == FALSE)
          #Ck
            summary(dat$xFltType[FltTyp1])   #Should all be 1
            summary(as.factor(as.vector(dat$Entry[FltTyp1])))  #NA correspond to 
                                                               #auto position updates,
                                                               #which were converted from
                                                               #"." to NA in InputAerialMaster...r
      #Identify "start transect" "search", "p on search", "p on connect", "s on search",
      #and "s on connect" events that mark the end of deadhead
        start.srch.idx <- which(dat$Entry == "start transect" |
                               dat$Entry == "search" |
                               dat$Entry == "p on connect" |
                               dat$Entry == "p on search" |
                               dat$Entry == "s on connect" |
                               dat$Entry == "s on search")
        length(start.srch.idx)
        prev.xFltType <- dat$xFltType[start.srch.idx - 1]
        end.dh.idx <- start.srch.idx[prev.xFltType == 1]
        length(end.dh.idx)
        #Ck
          summary(as.factor(prev.xFltType)) #Should be 1, 3, 4, 8, or 9
          summary(as.factor(dat$xFltType[end.dh.idx - 1]))  #Should be 1
          summary(as.factor(dat$xFltType[end.dh.idx]))  #Should be 2, 3, or 4 
          summary(as.factor(as.vector(dat$Entry[end.dh.idx])))  #Should be 
                                                                #"start transect",
                                                                #"search",
                                                                #or "p on connect" 

      #Identify end of flight entries by Entry == "deadhead" and FltNo.  Can't
      #rely on enttag because it was first used in 2007.
        dh.entry <- which(dat$Entry == "deadhead")
        old.FltNo <- dat$FltNo[dh.entry[1]] 
        
        eoflt.TF <- rep(FALSE, length(dh.entry))
        eoflt.TF[length(eoflt.TF)] <- TRUE
        
        for(i in 2:length(dh.entry)){
          new.FltNo <- dat$FltNo[dh.entry[i]]
          if(new.FltNo != old.FltNo){
            old.FltNo <- new.FltNo
            eoflt.TF[i-1] <- TRUE
          } 
        }

        eoflt <- dh.entry[eoflt.TF]
        #Ck
          summary(dat$FltType[eoflt])   #Should all be 1
          summary(dat$Enttag[eoflt])   #Should all be 5 or NA (Yr < 2007)
          summary(as.factor(as.vector(dat$Entry[eoflt]))) #deadhead only
        
      #Create vector of indices corresponding to dh data.  Note that eoflt
      #is contained within FltTyp1
          dh.idx <- sort(c(FltTyp1, end.dh.idx))
          #Ck
            test <- unique(dh.idx)
            length(dh.idx)
            length(test)
            head(dh.idx, n=100)
            tail(dh.idx, n=100)
        
      #Extract list of indices for individual dh segments from "dh.idx"
        dh.end.idx <- sort(c(end.dh.idx, eoflt))
        
        dh.list <- lapply(1:length(dh.end.idx), function(i){
                       #Identify index of end of dh segment i within
                       #dh.idx
                        end.idx <- which(dh.idx == dh.end.idx[i])
                       #Identify index of start of dh segment i within
                       #dh.idx
                        if(i == 1) {
                          start.idx <- 1
                        } else {
                          start.idx <- which(dh.idx == dh.end.idx[i-1]) + 1
                        }
                       #Create series of all indices in dh segment i.  Duplicate
                       #points for single-point series in order to create a 
                       #SpatialLine object
                        if(start.idx != end.idx){
                          d.idx <- start.idx:end.idx
                        } else {
                          d.idx <- c(start.idx,end.idx) #duplicate a single point
                        }  
                        return(d.idx)
                      })
          #CK
            class(dh.list)  #list
            length(dh.list) #num dh segments
            dh.list[[1]]    #should be sequential, increasing, no missing values
            dh.list[[length(dh.list)]]  #should be sequential, increasing, no missing values
        
      #Create SpatialLines objects for dh two ways:
      #  1. As a single Line for each continuous segment of dh
      #     effort
      #  2. As a single Line for every pair of points in a continuous segment
      #     of dh effort
          #First, as a single long line
            x.flt <- 360 + dat$ArcLong
            y.flt <- dat$ArcLat
            xy.flt <- cbind(x.flt,y.flt)
            xy.flt.SPts <- SpatialPoints(xy.flt, proj4string=CRS('+proj=longlat'))
            xy.flt.SPts.prj <- spTransform(xy.flt.SPts, prj)
          
            dh.SL.prj <- SpatialLines(list(Lines(lapply(1:length(dh.list), function(i){
            #dh.SL.prj <- SpatialLines(list(Lines(lapply(1:1, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts.prj@coords[dh.idx[dh.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="PRJ")), proj=prj)
          
            dh.SL.LL <- SpatialLines(list(Lines(lapply(1:length(dh.list), function(i){
            #dh.SL.LL <- SpatialLines(list(Lines(lapply(371:371, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts@coords[dh.idx[dh.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="LL")), proj=CRS('+proj=longlat'))
          
          #Second, as multiple short lines
            dhh.SL.prj <- SpatialLines(lapply(1:length(dh.list), function(i){
                              #Extract coordinates of points on seg i
                                xy <- xy.flt.SPts.prj@coords[dh.idx[dh.list[[i]]],]
                              #Create lines from pairs of coords
                                Lines.i <- Lines(lapply(1:(nrow(xy)-1), function(j){
                                             L <- Line(xy[j:(j+1),])
                                             return(L)
                                           }), ID=as.character(i))
                              }), proj=prj)
          
          return(list(dh.idx,dh.list,dh.SL.prj,dh.SL.LL,dhh.SL.prj))
  }
 
  
  
  
  
  
  
  
  
  #Extract continuous fights as SpatialLines object
  xtract.flt.SL <- function(dat,prj){
    #Create SpatialLines objects from each flight.
      #Identify end of flight entries by Entry == "deadhead" and FltNo.  Can't
      #rely on enttag because it was first used in 2007.
        dh.entry <- which(dat$Entry == "deadhead")
        old.FltNo <- dat$FltNo[dh.entry[1]] 
        
        eoflt.TF <- rep(FALSE, length(dh.entry))
        eoflt.TF[length(eoflt.TF)] <- TRUE
        
        for(i in 2:length(dh.entry)){
          new.FltNo <- dat$FltNo[dh.entry[i]]
          if(new.FltNo != old.FltNo){
            old.FltNo <- new.FltNo
            eoflt.TF[i-1] <- TRUE
          } 
        }

        eoflt <- dh.entry[eoflt.TF]
        #Ck
          summary(dat$FltType[eoflt])   #Should all be 1
          summary(dat$Enttag[eoflt])   #Should all be 5 or NA (Yr < 2007)
          summary(as.factor(as.vector(dat$Entry[eoflt]))) #deadhead only

      #Extract list of indices for individual flts from dat
        flt.list <- lapply(1:length(eoflt), function(i){
                     #Identify index of end of flt i 
                      end.idx <- eoflt[i]
                     #Identify index of start of flt i 
                      if(i == 1) {
                        start.idx <- 1
                      } else {
                        start.idx <- eoflt[i-1] + 1
                      }
                     #Create series of all indices in flt i
                      f.idx <- start.idx:end.idx
                      return(f.idx)
                    })
          #CK
            class(flt.list)  #list
            length(flt.list) #num flts
            flt.list[[1]]    #should be sequential, increasing, no missing values
            flt.list[[length(flt.list)]]  #should be sequential, increasing, no missing values
          
      #Create SpatialLines objects for flts two ways:
      #  1. As a single Line for each flt
      #  2. As a single Line for every pair of points in a flt
        #First, as a single long line
          x.flt <- 360 + dat$ArcLong
          y.flt <- dat$ArcLat
          xy.flt <- cbind(x.flt,y.flt)
          xy.flt.SPts <- SpatialPoints(xy.flt, proj4string=CRS('+proj=longlat'))
          xy.flt.SPts.prj <- spTransform(xy.flt.SPts, prj)
        
          flt.SL.prj <- SpatialLines(list(Lines(lapply(1:length(flt.list), function(i){
          #flt.SL.prj <- SpatialLines(list(Lines(lapply(1:1, function(i){
                                    #Extract coordinates of points on flt i
                                      xy <- xy.flt.SPts.prj@coords[flt.list[[i]],]
                                    #Create line from coords
                                      L <- Line(xy)
                                 }), ID="PRJ")), proj=prj)
        
          flt.SL.LL <- SpatialLines(list(Lines(lapply(1:length(flt.list), function(i){
          #flt.SL.LL <- SpatialLines(list(Lines(lapply(371:371, function(i){
                                    #Extract coordinates of points on flt i
                                      xy <- xy.flt.SPts@coords[flt.list[[i]],]
                                    #Create line from coords
                                      L <- Line(xy)
                                 }), ID="LL")), proj=CRS('+proj=longlat'))
        
        #Second, as multiple short lines
          fltt.SL.prj <- SpatialLines(lapply(1:length(flt.list), function(i){
                            #Extract coordinates of points on flt i
                              xy <- xy.flt.SPts.prj@coords[flt.list[[i]],]
                            #Create lines from pairs of coords
                              Lines.i <- Lines(lapply(1:(nrow(xy)-1), function(j){
                                           L <- Line(xy[j:(j+1),])
                                           return(L)
                                         }), ID=as.character(i))
                            }), proj=prj)
        
        return(list(flt.list,flt.SL.prj,flt.SL.LL,fltt.SL.prj))
  } 
 
  
  
  
  
  
  
  
  
  #Function to digest traxx.SL.prj into a list of SpatialLines comprised of 
  #a single Line defined by only two points.
    digest.txx <- function(traxx.SL, prj){
      dig <- list()
      dig.idx <- 0
      for(i in 1:length(traxx.SL@lines)){
        for(j in 1:length(traxx.SL@lines[[i]]@Lines)){
          SL <- SpatialLines(list(Lines(list(traxx.SL@lines[[i]]@Lines[[j]]),
                                        ID=paste(i,j,sep="_"))),
                             proj=prj)
          dig.idx <- dig.idx + 1                   
          dig[[dig.idx]] <- SL
        }
      }
      return(dig)
    }      
  
  
  
  
  
  


    
  #Extract continuous ffov segments as SpatialLines object
  xtract.ffov.SL <- function(dat,prj){
    #Create SpatialLines objects from ffov effort covered during each 
    #flt.  Identify the following:
    #  a. "ffov" flight type via xFltType == 8
    #  b. "start transect", "search", or "deadhead" may follow 
    #      ffov effort.  
    #Then, identify subsets of row indices corresponding to individual
    #ffov segments.
      #Identify ffov flight type with xFltType==8
        FltTyp8 <- which(dat$xFltType == 8 & 
                           is.na(dat$xFltType) == FALSE)
          #Ck
            summary(dat$xFltType[FltTyp8])   #Should all be 8
            summary(as.factor(as.vector(dat$Entry[FltTyp8])))  #NA correspond to 
                                                               #auto position updates,
                                                               #which were converted from
                                                               #"." to NA in InputAerialMaster...r
      #Identify "start transect" "search", and "deadhead" events that 
      #mark the end of ffov
        next.idx <- which(dat$Entry == "start transect" |
                               dat$Entry == "search" |
                               dat$Entry == "deadhead")
        length(next.idx)
        next.idx <- next.idx[next.idx != 1] #Omit first deadhead, o.w. things get ugly
        length(next.idx)
        prev.xFltType <- dat$xFltType[next.idx - 1]
        end.ffov.idx <- next.idx[prev.xFltType == 8]
        length(end.ffov.idx)
        #Ck
          summary(as.factor(prev.xFltType)) #Should be 1, 4, 8, 9, 10
          summary(as.factor(dat$xFltType[end.ffov.idx - 1]))  #Should be 8
          summary(as.factor(dat$xFltType[end.ffov.idx]))  #Should be 1, 2, or 4 
          summary(as.factor(as.vector(dat$Entry[end.ffov.idx])))  #Should be 
                                                                #"start transect",
                                                                #"search",
                                                                #or "deadhead" 

      #Create vector of indices corresponding to ffov data.  
          ffov.idx <- sort(c(FltTyp8, end.ffov.idx))
          #Ck
            test <- unique(ffov.idx)
            length(ffov.idx)
            length(test)
            head(ffov.idx, n=100)
            tail(ffov.idx, n=100)
        
      #Extract list of indices for individual ffov segments from "ffov.idx"
        ffov.list <- lapply(1:length(end.ffov.idx), function(i){
                       #Identify index of end of ffov segment i within
                       #ffov.idx
                        end.idx <- which(ffov.idx == end.ffov.idx[i])
                       #Identify index of start of ffov segment i within
                       #ffov.idx
                        if(i == 1) {
                          start.idx <- 1
                        } else {
                          start.idx <- which(ffov.idx == end.ffov.idx[i-1]) + 1
                        }
                       #Create series of all indices in ffov segment i.  Duplicate
                       #points for single-point series in order to create a 
                       #SpatialLine object
                        if(start.idx != end.idx){
                          d.idx <- start.idx:end.idx
                        } else {
                          d.idx <- c(start.idx,end.idx) #duplicate a single point
                        }  
                        return(d.idx)
                      })
          #CK
            class(ffov.list)  #list
            length(ffov.list) #num ffov segments
            ffov.list[[1]]    #should be sequential, increasing, no missing values
            ffov.list[[length(ffov.list)]]  #should be sequential, increasing, no missing values
        
      #Create SpatialLines objects for ffov two ways:
      #  1. As a single Line for each continuous segment of ffov
      #     effort
      #  2. As a single Line for every pair of points in a continuous segment
      #     of ffov effort
          #First, as a single long line
            x.flt <- 360 + dat$ArcLong
            y.flt <- dat$ArcLat
            xy.flt <- cbind(x.flt,y.flt)
            xy.flt.SPts <- SpatialPoints(xy.flt, proj4string=CRS('+proj=longlat'))
            xy.flt.SPts.prj <- spTransform(xy.flt.SPts, prj)
          
            ffov.SL.prj <- SpatialLines(list(Lines(lapply(1:length(ffov.list), function(i){
            #ffov.SL.prj <- SpatialLines(list(Lines(lapply(1:1, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts.prj@coords[ffov.idx[ffov.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="PRJ")), proj=prj)
          
            ffov.SL.LL <- SpatialLines(list(Lines(lapply(1:length(ffov.list), function(i){
            #ffov.SL.LL <- SpatialLines(list(Lines(lapply(371:371, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts@coords[ffov.idx[ffov.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="LL")), proj=CRS('+proj=longlat'))
          
          #Second, as multiple short lines
            ffovh.SL.prj <- SpatialLines(lapply(1:length(ffov.list), function(i){
                              #Extract coordinates of points on seg i
                                xy <- xy.flt.SPts.prj@coords[ffov.idx[ffov.list[[i]]],]
                              #Create lines from pairs of coords
                                Lines.i <- Lines(lapply(1:(nrow(xy)-1), function(j){
                                             L <- Line(xy[j:(j+1),])
                                             return(L)
                                           }), ID=as.character(i))
                              }), proj=prj)
          
          return(list(ffov.idx,ffov.list,ffov.SL.prj,ffov.SL.LL,ffovh.SL.prj))
  }
    
    
    
    
    
    
    
    
    
    
    
    
  #Extract continuous CAPs circling segments as SpatialLines object
  xtract.ccaps.SL <- function(dat,prj){
    #Create SpatialLines objects from ccaps effort covered during each 
    #flt.  Identify the following:
    #  a. "ccaps" flight type via xFltType == 9
    #  b. "start transect" or "deadhead" may follow ccaps effort.  
    #Then, identify subsets of row indices corresponding to individual
    #ccaps segments.
      #Identify ccaps flight type with xFltType==9
        FltTyp9 <- which(dat$xFltType == 9 & 
                           is.na(dat$xFltType) == FALSE)
          #Ck
            summary(dat$xFltType[FltTyp9])   #Should all be 9
            summary(as.factor(as.vector(dat$Entry[FltTyp9])))  #NA correspond to 
                                                               #auto position updates,
                                                               #which were converted from
                                                               #"." to NA in InputAerialMaster...r
      #Identify "start transect" and "deadhead" events that mark the end of ccaps
        next.idx <- which(dat$Entry == "start transect" |
                               dat$Entry == "deadhead")
        length(next.idx)
        next.idx <- next.idx[next.idx != 1] #Omit first deadhead, o.w. things get ugly
        length(next.idx)
        prev.xFltType <- dat$xFltType[next.idx - 1]
        end.ccaps.idx <- next.idx[prev.xFltType == 9]
        length(end.ccaps.idx)
        #Ck
          summary(as.factor(prev.xFltType)) #Should be 1, 4, 8, 9, 10
          summary(as.factor(dat$xFltType[end.ccaps.idx - 1]))  #Should be 9
          summary(as.factor(dat$xFltType[end.ccaps.idx]))  #Should be 1 or 2 
          summary(as.factor(as.vector(dat$Entry[end.ccaps.idx])))  #Should be 
                                                                #"start transect"
                                                                #or "deadhead" 

      #Create vector of indices corresponding to ccaps data.  
          ccaps.idx <- sort(c(FltTyp9, end.ccaps.idx))
          #Ck
            test <- unique(ccaps.idx)
            length(ccaps.idx)
            length(test)
            head(ccaps.idx, n=100)
            tail(ccaps.idx, n=100)
        
      #Extract list of indices for individual ccaps segments from "ccaps.idx"
        ccaps.list <- lapply(1:length(end.ccaps.idx), function(i){
                       #Identify index of end of ccaps segment i within
                       #ccaps.idx
                        end.idx <- which(ccaps.idx == end.ccaps.idx[i])
                       #Identify index of start of ccaps segment i within
                       #ccaps.idx
                        if(i == 1) {
                          start.idx <- 1
                        } else {
                          start.idx <- which(ccaps.idx == end.ccaps.idx[i-1]) + 1
                        }
                       #Create series of all indices in ccaps segment i.  Duplicate
                       #points for single-point series in order to create a 
                       #SpatialLine object
                        if(start.idx != end.idx){
                          d.idx <- start.idx:end.idx
                        } else {
                          d.idx <- c(start.idx,end.idx) #duplicate a single point
                        }  
                        return(d.idx)
                      })
          #CK
            class(ccaps.list)  #list
            length(ccaps.list) #num ccaps segments
            ccaps.list[[1]]    #should be sequential, increasing, no missing values
            ccaps.list[[length(ccaps.list)]]  #should be sequential, increasing, no missing values
        
      #Create SpatialLines objects for ccaps two ways:
      #  1. As a single Line for each continuous segment of ccaps
      #     effort
      #  2. As a single Line for every pair of points in a continuous segment
      #     of ccaps effort
          #First, as a single long line
            x.flt <- 360 + dat$ArcLong
            y.flt <- dat$ArcLat
            xy.flt <- cbind(x.flt,y.flt)
            xy.flt.SPts <- SpatialPoints(xy.flt, proj4string=CRS('+proj=longlat'))
            xy.flt.SPts.prj <- spTransform(xy.flt.SPts, prj)
          
            ccaps.SL.prj <- SpatialLines(list(Lines(lapply(1:length(ccaps.list), function(i){
            #ccaps.SL.prj <- SpatialLines(list(Lines(lapply(1:1, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts.prj@coords[ccaps.idx[ccaps.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="PRJ")), proj=prj)
          
            ccaps.SL.LL <- SpatialLines(list(Lines(lapply(1:length(ccaps.list), function(i){
            #ccaps.SL.LL <- SpatialLines(list(Lines(lapply(371:371, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts@coords[ccaps.idx[ccaps.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="LL")), proj=CRS('+proj=longlat'))
          
          #Second, as multiple short lines
            ccapsh.SL.prj <- SpatialLines(lapply(1:length(ccaps.list), function(i){
                              #Extract coordinates of points on seg i
                                xy <- xy.flt.SPts.prj@coords[ccaps.idx[ccaps.list[[i]]],]
                              #Create lines from pairs of coords
                                Lines.i <- Lines(lapply(1:(nrow(xy)-1), function(j){
                                             L <- Line(xy[j:(j+1),])
                                             return(L)
                                           }), ID=as.character(i))
                              }), proj=prj)
          
          return(list(ccaps.idx,ccaps.list,ccaps.SL.prj,ccaps.SL.LL,ccapsh.SL.prj))
  }
    
    
    
    
    
    
    
    
    
    
    
  #Extract continuous FGF segments as SpatialLines object
  xtract.fgf.SL <- function(dat,prj){
    #Create SpatialLines objects from fgf effort covered during each 
    #flt.  Identify the following:
    #  a. "fgf" flight type via xFltType == 10
    #  b. "deadhead" may follow fgf effort.  
    #Then, identify subsets of row indices corresponding to individual
    #fgf segments.
      #Identify fgf flight type with xFltType==10
        FltTyp10 <- which(dat$xFltType == 10 & 
                           is.na(dat$xFltType) == FALSE)
          #Ck
            summary(dat$xFltType[FltTyp10])   #Should all be 10
            summary(as.factor(as.vector(dat$Entry[FltTyp10])))  #NA correspond to 
                                                               #auto position updates,
                                                               #which were converted from
                                                               #"." to NA in InputAerialMaster...r
      #Identify "deadhead" events that mark the end of fgf
        next.idx <- which(dat$Entry == "deadhead")
        length(next.idx)
        next.idx <- next.idx[next.idx != 1] #Omit first deadhead, o.w. things get ugly
        length(next.idx)
        prev.xFltType <- dat$xFltType[next.idx - 1]
        end.fgf.idx <- next.idx[prev.xFltType == 10]
        length(end.fgf.idx)
        #Ck
          summary(as.factor(prev.xFltType)) #Should be 1, 4, 8, 9, 10
          summary(as.factor(dat$xFltType[end.fgf.idx - 1]))  #Should be 10
          summary(as.factor(dat$xFltType[end.fgf.idx]))  #Should be 1  
          summary(as.factor(as.vector(dat$Entry[end.fgf.idx])))  #Should be "deadhead" 

      #Create vector of indices corresponding to fgf data.  
          fgf.idx <- sort(c(FltTyp10, end.fgf.idx))
          #Ck
            test <- unique(fgf.idx)
            length(fgf.idx)
            length(test)
            head(fgf.idx, n=100)
            tail(fgf.idx, n=100)
        
      #Extract list of indices for individual fgf segments from "fgf.idx"
        fgf.list <- lapply(1:length(end.fgf.idx), function(i){
                       #Identify index of end of fgf segment i within
                       #fgf.idx
                        end.idx <- which(fgf.idx == end.fgf.idx[i])
                       #Identify index of start of fgf segment i within
                       #fgf.idx
                        if(i == 1) {
                          start.idx <- 1
                        } else {
                          start.idx <- which(fgf.idx == end.fgf.idx[i-1]) + 1
                        }
                       #Create series of all indices in fgf segment i.  Duplicate
                       #points for single-point series in order to create a 
                       #SpatialLine object
                        if(start.idx != end.idx){
                          d.idx <- start.idx:end.idx
                        } else {
                          d.idx <- c(start.idx,end.idx) #duplicate a single point
                        }  
                        return(d.idx)
                      })
          #CK
            class(fgf.list)  #list
            length(fgf.list) #num fgf segments
            fgf.list[[1]]    #should be sequential, increasing, no missing values
            fgf.list[[length(fgf.list)]]  #should be sequential, increasing, no missing values
        
      #Create SpatialLines objects for fgf two ways:
      #  1. As a single Line for each continuous segment of fgf
      #     effort
      #  2. As a single Line for every pair of points in a continuous segment
      #     of fgf effort
          #First, as a single long line
            x.flt <- 360 + dat$ArcLong
            y.flt <- dat$ArcLat
            xy.flt <- cbind(x.flt,y.flt)
            xy.flt.SPts <- SpatialPoints(xy.flt, proj4string=CRS('+proj=longlat'))
            xy.flt.SPts.prj <- spTransform(xy.flt.SPts, prj)
          
            fgf.SL.prj <- SpatialLines(list(Lines(lapply(1:length(fgf.list), function(i){
            #fgf.SL.prj <- SpatialLines(list(Lines(lapply(1:1, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts.prj@coords[fgf.idx[fgf.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="PRJ")), proj=prj)
          
            fgf.SL.LL <- SpatialLines(list(Lines(lapply(1:length(fgf.list), function(i){
            #fgf.SL.LL <- SpatialLines(list(Lines(lapply(371:371, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts@coords[fgf.idx[fgf.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="LL")), proj=CRS('+proj=longlat'))
          
          #Second, as multiple short lines
            fgfh.SL.prj <- SpatialLines(lapply(1:length(fgf.list), function(i){
                              #Extract coordinates of points on seg i
                                xy <- xy.flt.SPts.prj@coords[fgf.idx[fgf.list[[i]]],]
                              #Create lines from pairs of coords
                                Lines.i <- Lines(lapply(1:(nrow(xy)-1), function(j){
                                             L <- Line(xy[j:(j+1),])
                                             return(L)
                                           }), ID=as.character(i))
                              }), proj=prj)
          
          return(list(fgf.idx,fgf.list,fgf.SL.prj,fgf.SL.LL,fgfh.SL.prj))
  }
    
    
    
    
    
    
    
    

  
  
  #Extract continuous capsstrip segments as SpatialLines object
  xtract.capsstrip.SL <- function(dat,prj){
    #Create SpatialLines objects from capsstrip effort covered during each 
    #flt.  Identify the following:
    #  a. "CAPs strip" flight type via xFltType == 11
    #  b. Allow only for CAPs to follow CAPs strip effort.  
    #Then, identify subsets of row indices corresponding to continuous
    #capsstrip segments.
      #Identify capsstrip flight type with xFltType==11
        FltTyp11 <- which(dat$xFltType == 11 & 
                           is.na(dat$xFltType) == FALSE)
          #Ck
            summary(dat$xFltType[FltTyp11])   #Should all be 11
            summary(as.factor(as.vector(dat$Entry[FltTyp11])))  #NA correspond to 
                                                               #auto position updates,
                                                               #which were converted from
                                                               #"." to NA in InputAerialMaster...r
      #Identify "CAPs" events that mark the end of capsstrip
        next.idx <- which(dat$Entry == "CAPs")
        length(next.idx)
        prev.xFltType <- dat$xFltType[next.idx - 1]
        end.capsstrip.idx <- next.idx[prev.xFltType == 11]
        length(end.capsstrip.idx)
        #Ck
          summary(as.factor(prev.xFltType)) #Should be 4 or 11
          summary(as.factor(dat$xFltType[end.capsstrip.idx - 1]))  #Should be 11
          summary(as.factor(dat$xFltType[end.capsstrip.idx]))  #Should be 7  
          summary(as.factor(as.vector(dat$Entry[end.capsstrip.idx])))  #Should be "CAPs" 

      #Create vector of indices corresponding to capsstrip data.  
          capsstrip.idx <- sort(c(FltTyp11, end.capsstrip.idx))
          #Ck
            test <- unique(capsstrip.idx)
            length(capsstrip.idx)
            length(test)
            head(capsstrip.idx, n=100)
            tail(capsstrip.idx, n=100)
        
      #Extract list of indices for individual capsstrip segments from "capsstrip.idx"
        capsstrip.list <- lapply(1:length(end.capsstrip.idx), function(i){
                       #Identify index of end of capsstrip segment i within
                       #capsstrip.idx
                        end.idx <- which(capsstrip.idx == end.capsstrip.idx[i])
                       #Identify index of start of capsstrip segment i within
                       #capsstrip.idx
                        if(i == 1) {
                          start.idx <- 1
                        } else {
                          start.idx <- which(capsstrip.idx == end.capsstrip.idx[i-1]) + 1
                        }
                       #Create series of all indices in capsstrip segment i.  Duplicate
                       #points for single-point series in order to create a 
                       #SpatialLine object
                        if(start.idx != end.idx){
                          d.idx <- start.idx:end.idx
                        } else {
                          d.idx <- c(start.idx,end.idx) #duplicate a single point
                        }  
                        return(d.idx)
                      })
          #CK
            class(capsstrip.list)  #list
            length(capsstrip.list) #num capsstrip segments
            capsstrip.list[[1]]    #should be sequential, increasing, no missing values
            capsstrip.list[[length(capsstrip.list)]]  #should be sequential, increasing, no missing values
        
      #Create SpatialLines objects for capsstrip two ways:
      #  1. As a single Line for each continuous segment of capsstrip
      #     effort
      #  2. As a single Line for every pair of points in a continuous segment
      #     of capsstrip effort
          #First, as a single long line
            x.flt <- 360 + dat$ArcLong
            y.flt <- dat$ArcLat
            xy.flt <- cbind(x.flt,y.flt)
            xy.flt.SPts <- SpatialPoints(xy.flt, proj4string=CRS('+proj=longlat'))
            xy.flt.SPts.prj <- spTransform(xy.flt.SPts, prj)
          
            capsstrip.SL.prj <- SpatialLines(list(Lines(lapply(1:length(capsstrip.list), function(i){
            #capsstrip.SL.prj <- SpatialLines(list(Lines(lapply(1:1, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts.prj@coords[capsstrip.idx[capsstrip.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="PRJ")), proj=prj)
          
            capsstrip.SL.LL <- SpatialLines(list(Lines(lapply(1:length(capsstrip.list), function(i){
            #capsstrip.SL.LL <- SpatialLines(list(Lines(lapply(371:371, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts@coords[capsstrip.idx[capsstrip.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="LL")), proj=CRS('+proj=longlat'))
          
          #Second, as multiple short lines
            capsstriph.SL.prj <- SpatialLines(lapply(1:length(capsstrip.list), function(i){
                              #Extract coordinates of points on seg i
                                xy <- xy.flt.SPts.prj@coords[capsstrip.idx[capsstrip.list[[i]]],]
                              #Create lines from pairs of coords
                                Lines.i <- Lines(lapply(1:(nrow(xy)-1), function(j){
                                             L <- Line(xy[j:(j+1),])
                                             return(L)
                                           }), ID=as.character(i))
                              }), proj=prj)
          
          return(list(capsstrip.idx,capsstrip.list,capsstrip.SL.prj,capsstrip.SL.LL,capsstriph.SL.prj))
  }
  
  
  
  
  
  
  
  
  

  
  
  
  #Extract continuous capsstrip segments as SpatialLines object
  xtract.caps.SL <- function(dat,prj){
    #Create SpatialLines objects from caps effort covered during each 
    #flt.  Identify the following:
    #  a. "CAPs" flight type via xFltType == 7
    #  b. Allow only for CAPs strip and CAPs circling to follow CAPs effort.  
    #Then, identify subsets of row indices corresponding to continuous
    #caps segments.
      #Identify caps flight type with xFltType==7
        FltTyp7 <- which(dat$xFltType == 7 & 
                           is.na(dat$xFltType) == FALSE)
          #Ck
            summary(dat$xFltType[FltTyp7])   #Should all be 7
            summary(as.factor(as.vector(dat$Entry[FltTyp7])))  #NA correspond to 
                                                               #auto position updates,
                                                               #which were converted from
                                                               #"." to NA in InputAerialMaster...r
      #Identify "CAPs strip" and "CAPs circling" events that mark the end of caps
        next.idx <- which(dat$Entry == "CAPs strip" |
                          dat$Entry == "CAPs circling")
        length(next.idx)
        prev.xFltType <- dat$xFltType[next.idx - 1]
        end.caps.idx <- next.idx[prev.xFltType == 7]
        length(end.caps.idx)
        #Ck
          summary(as.factor(prev.xFltType)) #Should be 2, 4, 7, or 11
          summary(as.factor(dat$xFltType[end.caps.idx - 1]))  #Should be 7
          summary(as.factor(dat$xFltType[end.caps.idx]))  #Should be 9 or 11  
          summary(as.factor(as.vector(dat$Entry[end.caps.idx])))  #Should be "CAPs strip"
                                                                  #or "CAPs circling"

      #Create vector of indices corresponding to caps data.  
          caps.idx <- sort(c(FltTyp7, end.caps.idx))
          #Ck
            test <- unique(caps.idx)
            length(caps.idx)
            length(test)
            head(caps.idx, n=100)
            tail(caps.idx, n=100)
        
      #Extract list of indices for individual caps segments from "caps.idx"
        caps.list <- lapply(1:length(end.caps.idx), function(i){
                       #Identify index of end of caps segment i within
                       #caps.idx
                        end.idx <- which(caps.idx == end.caps.idx[i])
                       #Identify index of start of caps segment i within
                       #caps.idx
                        if(i == 1) {
                          start.idx <- 1
                        } else {
                          start.idx <- which(caps.idx == end.caps.idx[i-1]) + 1
                        }
                       #Create series of all indices in caps segment i.  Duplicate
                       #points for single-point series in order to create a 
                       #SpatialLine object
                        if(start.idx != end.idx){
                          d.idx <- start.idx:end.idx
                        } else {
                          d.idx <- c(start.idx,end.idx) #duplicate a single point
                        }  
                        return(d.idx)
                      })
          #CK
            class(caps.list)  #list
            length(caps.list) #num caps segments
            caps.list[[1]]    #should be sequential, increasing, no missing values
            caps.list[[length(caps.list)]]  #should be sequential, increasing, no missing values
        
      #Create SpatialLines objects for caps two ways:
      #  1. As a single Line for each continuous segment of caps
      #     effort
      #  2. As a single Line for every pair of points in a continuous segment
      #     of caps effort
          #First, as a single long line
            x.flt <- 360 + dat$ArcLong
            y.flt <- dat$ArcLat
            xy.flt <- cbind(x.flt,y.flt)
            xy.flt.SPts <- SpatialPoints(xy.flt, proj4string=CRS('+proj=longlat'))
            xy.flt.SPts.prj <- spTransform(xy.flt.SPts, prj)
          
            caps.SL.prj <- SpatialLines(list(Lines(lapply(1:length(caps.list), function(i){
            #caps.SL.prj <- SpatialLines(list(Lines(lapply(1:1, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts.prj@coords[caps.idx[caps.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="PRJ")), proj=prj)
          
            caps.SL.LL <- SpatialLines(list(Lines(lapply(1:length(caps.list), function(i){
            #caps.SL.LL <- SpatialLines(list(Lines(lapply(371:371, function(i){
                                      #Extract coordinates of points on seg i
                                        xy <- xy.flt.SPts@coords[caps.idx[caps.list[[i]]],]
                                      #Create line from coords
                                        L <- Line(xy)
                                   }), ID="LL")), proj=CRS('+proj=longlat'))
          
          #Second, as multiple short lines
            capsh.SL.prj <- SpatialLines(lapply(1:length(caps.list), function(i){
                              #Extract coordinates of points on seg i
                                xy <- xy.flt.SPts.prj@coords[caps.idx[caps.list[[i]]],]
                              #Create lines from pairs of coords
                                Lines.i <- Lines(lapply(1:(nrow(xy)-1), function(j){
                                             L <- Line(xy[j:(j+1),])
                                             return(L)
                                           }), ID=as.character(i))
                              }), proj=prj)
          
          return(list(caps.idx,caps.list,caps.SL.prj,caps.SL.LL,capsh.SL.prj))
  }
  
  
  
  
  
  
    
      
    
      
  #Output functions
    save(xtract.tx.byBeaufVis.SL, xtract.ctx.byBeaufVis.SL, xtract.csrch.byBeaufVis.SL,  
         xtract.srch.byBeaufVis.SL, xtract.dh.SL, xtract.flt.SL, digest.txx, 
         xtract.ffov.SL, xtract.ccaps.SL, xtract.fgf.SL, xtract.capsstrip.SL,
         xtract.caps.SL,
         file="xtract_flightlines_SL_byBeaufVis_ReturnList.Rdata")
  
  
  
  
  
  
  
  