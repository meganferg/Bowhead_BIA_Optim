#Script InputAerialMaster1979_2019_v3_34_v0_28_v22_v3.r...Megan C. Ferguson...25 March 2020

  setwd("C:\\Users\\megan.ferguson\\Work\\FergusonFiles\\ArcticMonkeys\\Analysis\\Bm2019")	

  library(RODBC)
  
  outfnam <- "Data//am_all_1979_2019_v3_34_v0_28_v22_v3.Rdata"         # .Rdata

  #Open connections to survey data and input data
    am.ch <- odbcConnectAccess("Data\\Aerial_Master_1979_2011_v3_34.MDB", readOnly=TRUE)
    am7911 <- sqlFetch(am.ch, "Output")
    odbcClose(am.ch)
    #
    am.ch <- odbcConnectAccess("Data\\Aerial_Master_2012_2014_v0_28.MDB", readOnly=TRUE)
    am1214 <- sqlFetch(am.ch, "Output")
    odbcClose(am.ch)
    #
    am.ch <- odbcConnectAccess("Data\\Aerial_Master_2015_2017_v22.mdb", readOnly=TRUE)
    am1517 <- sqlFetch(am.ch, "Output")
    odbcClose(am.ch)     
    #
    am.ch <- odbcConnectAccess("Data\\Aerial_Master_2018_2019_v3.mdb", readOnly=TRUE)
    am1819 <- sqlFetch(am.ch, "Output")
    odbcClose(am.ch)  

  #Manipulate data
    #omit Lat, Long, UTMZ6X and UTMZ6y
      am.all <- rbind.data.frame(am7911, am1214, am1517, am1819)[,c(2:4,7:43,46:72)]
      rm(am7911, am1214, am1517, am1819)
    
    #Remove un-Saved records
      nrow(am.all)
      Sav <- which(am.all$saved != 0)
      am.all <- am.all[Sav,]
      #CK
        nrow(am.all)
        summary(am.all$saved)

    #Ck for leading spaces and other errors.  
      summary(am.all$Entry)
      summary(am.all$Species)
      summary(am.all$Sightcue)
      summary(am.all$Habitat)
      summary(am.all$Behavior)
      summary(am.all$Sop)
      summary(am.all$Swimdir)
      summary(am.all$Response)
      summary(am.all$Repeat)
      summary(am.all$Observer)
      summary(am.all$Skycon)
      summary(am.all$VisImpLeft)
      summary(am.all$VisImpRight)
      summary(am.all$Vis_left)
      summary(am.all$Vis_right)
      summary(as.factor(am.all$Icepercent))
      summary(as.factor(am.all$Icepercent_l))
      summary(as.factor(am.all$Icepercent_r))
      summary(am.all$Icetype)
      summary(am.all$Seastate)
      summary(am.all$Aerial_Survey)
      summary(am.all$magvar_dir)
      summary(am.all$Transect_id)
      summary(am.all$DataRecorder)  
      summary(am.all$ObsLeft)
      summary(am.all$ObsRight)
      summary(as.factor(am.all$Pilot))
      summary(as.factor(am.all$Co_Pilot))
          
    #Convert "." and "n/a " to NA
      #for(i in 1:ncol(am.all)){
      for(i in c(1:2,4:67)){  #Doesn't like the class of GMT_Minus8_DateTime
        na <- which(am.all[,i] == "." |
                    am.all[,i] == "n/a")
        if(length(na) > 0) am.all[na,i] <- NA
      }
    
    #Extract components of date and time
      dattim <- as.vector(as.character(am.all$GMT_Minus8_DateTime))
      nchar.dattim <- nchar(dattim)
      dattim.spc <- regexpr(" ", dattim)
      GMT_Minus8_Date <- substr(dattim, start=1, stop=(dattim.spc-1))
      GMT_Minus8_Time <- substr(dattim, start=(dattim.spc+1), stop=nchar.dattim)
      am.all <- cbind.data.frame(am.all, GMT_Minus8_Date, GMT_Minus8_Time)
      #Format time
        am.Time.vec <- as.character(am.all$GMT_Minus8_Time)
        nchar.Time <- nchar(am.Time.vec)
        HOUR24 <- as.numeric(substr(am.Time.vec, start=1, stop=(nchar.Time-6)))
        MINUTE <- as.numeric(substr(am.Time.vec, start=(nchar.Time-4),
                                    stop=(nchar.Time-3)))
        SECOND <- as.numeric(substr(am.Time.vec, start=(nchar.Time-1),
                                    stop=nchar.Time))
        Tyme <- HOUR24 + MINUTE/60 + SECOND/(60*60)
        #Ck
          summary(MINUTE)
          summary(SECOND)
          summary(HOUR24)
          summary(Tyme)
      #Format date
        Dayt <- as.Date(am.all$GMT_Minus8_Date, "%Y-%m-%d")
        Yr <- as.integer(substr(Dayt, start=1, stop=4))
        Month <- as.integer(substr(Dayt, start=6, stop=7))
        am.all <- cbind(am.all,Dayt,Yr,Month,Tyme)
        #CK.
          #The following should be of class 'Date'.
            summary(am.all$Dayt)
            class(am.all$Dayt)
          #The following should be class 'factor'
            summary(am.all$GMT_Minus8_Date)
            class(am.all$GMT_Minus8_Date)
          #Tyme: The following should not be greater than 24
            summary(am.all$Tyme)
            class(am.all$Tyme)
          #Month
            summary(am.all$Month)
            class(am.all$Month)

    #Rename columns
      cnam <- c("Event","FltNo","cDatTim","ArcLat","ArcLong",
                "Alt","AirHead","Entry","Species","SightCue","Habitat",
                "Behavior","Size","TotalNo","FinalGrp","Low","Hi","CalfNo",
                "CalfCircl","Clino","SOP","SwimDir","SwimSpeed","Response",
                "NoReacted","Repeat","Observer","PrimObs","SkyCon","VisImpL",
                "VisImpR","VisL","VisR","IcePct","IcePctL", "IcePctR","IceType",
                "cBeauf","Blk","Depth","SurvNam","SwimDir.T","MagVar.gps",
                "MagVar.dir","AirHead.gps","Alt.gps","Cert","Grp","Saved","Notes",
                "FltType","T.ID","Famly","Enttag","assoc","XWhale","YWhale","OffshDist",
                "DataRec","ObsL","ObsR","Obs4","Pilot","CoPilot","Photo","LatTemp",
                "LongTemp","GMTMinus8Date","GMTMinus8Time","Dayt","Yr","Month",
                "Tyme")
      names(am.all) <- cnam  

    #Order
      o <- order(am.all$Yr, am.all$Dayt, am.all$FltNo, am.all$Tyme, am.all$Event)
      am.all <- am.all[o,]
      summary(am.all)
  
    #Create new FltType field that populates all automatic position update records
    #with the correct FltType code for the survey mode.
      xFltType <- am.all$FltType
      old.ft <- am.all$FltType[1]  #The following code will work only if this is a valid 
                                   #FltType code != -1.
      for(i in 2:nrow(am.all)){
        new.ft <- am.all$FltType[i]
        if(new.ft > 0){
          old.ft <- new.ft
        } else {
          xFltType[i] <- old.ft
        }
      }
      am.all <- cbind.data.frame(am.all[,1:51], xFltType, am.all[,52:73])
      #CK
        summary(as.factor(am.all$FltType))
        summary(as.factor(am.all$xFltType))
        idx <- which(am.all$FltType != am.all$xFltType)
        length(idx)
        summary(am.all$FltType[idx])  #Should = -1
        names(am.all)
        i <- 10000
        am.all[((idx[i]-1):(idx[i]+20)),51:52]
  
    #Convert IcePct fields to numeric
      am.all$IcePct <- as.numeric(as.vector(am.all$IcePct))
      am.all$IcePctL <- as.numeric(as.vector(am.all$IcePctL))
      am.all$IcePctR <- as.numeric(as.vector(am.all$IcePctR))
      #CK
        summary(am.all$IcePct)
        summary(am.all$IcePctL)
        summary(am.all$IcePctR)
  
    #Create integer-valued Beauf.  
        iBeauf <- rep(NA, nrow(am.all))
        cBeauf.names <- names(summary(am.all$cBeauf))
        iBeauf[which(am.all$cBeauf == cBeauf.names[1])] <- NA
        iBeauf[which(am.all$cBeauf == cBeauf.names[2])] <- 0
        iBeauf[which(am.all$cBeauf == cBeauf.names[3])] <- 1
        iBeauf[which(am.all$cBeauf == cBeauf.names[4])] <- 2
        iBeauf[which(am.all$cBeauf == cBeauf.names[5])] <- 3
        iBeauf[which(am.all$cBeauf == cBeauf.names[6])] <- 4
        iBeauf[which(am.all$cBeauf == cBeauf.names[7])] <- 5
        iBeauf[which(am.all$cBeauf == cBeauf.names[8])] <- 6
        iBeauf[which(am.all$cBeauf == cBeauf.names[9])] <- 7
        iBeauf[which(am.all$cBeauf == cBeauf.names[10])] <- 8
        iBeauf[which(am.all$cBeauf == cBeauf.names[11])] <- NA
        iBeauf[which(am.all$cBeauf == cBeauf.names[12] |
                      am.all$cBeauf == cBeauf.names[13] |
                      am.all$cBeauf == cBeauf.names[14])] <- NA
      
        #Fill in non-deadhead auto-updates with a Beaufort value.
          old.iBeauf <- iBeauf[1]  
          for(i in 2:nrow(am.all)){
            new.iBeauf <- iBeauf[i]
            if(is.na(new.iBeauf) == FALSE | 
               (am.all$Entry[i] == "deadhead" & is.na(am.all$Entry[i]) == FALSE)){
              old.iBeauf <- new.iBeauf
            } else {
              iBeauf[i] <- old.iBeauf
            }
          }
      
        am.all <- cbind.data.frame(am.all, iBeauf)              

  #Create numerical VisL.km, VisR.km, and VisX.km variables
    VisL.km <- rep(NA, nrow(am.all))
    VisL.km[am.all$VisL == "0 km"] <- 0
    VisL.km[am.all$VisL == "<1 km"] <- 1
    VisL.km[am.all$VisL == "1-2 km"] <- 2
    VisL.km[am.all$VisL == "2-3 km"] <- 3
    VisL.km[am.all$VisL == "3-5 km"] <- 5
    VisL.km[am.all$VisL == "5-10 km"] <- 10
    VisL.km[am.all$VisL == "Unlimited" | am.all$VisL == "unlimited"] <- 20
          
    VisR.km <- rep(NA, nrow(am.all))
    VisR.km[am.all$VisR == "0 km"] <- 0
    VisR.km[am.all$VisR == "<1 km"] <- 1
    VisR.km[am.all$VisR == "1-2 km"] <- 2
    VisR.km[am.all$VisR == "2-3 km"] <- 3
    VisR.km[am.all$VisR == "3-5 km"] <- 5
    VisR.km[am.all$VisR == "5-10 km"] <- 10
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
        
  #Characterize Vis.km for sightings by outputting only those values (L 
  #or R) associated with SOP of sighting.  SOP = Side of plane from which a 
  #sighting was made.
    sop.l <- which(am.all$SOP == "l")
    sop.r <- which(am.all$SOP == "r")
    VisSOP.km <- rep(NA, nrow(am.all))
    VisSOP.km[sop.l] <- VisL.km[sop.l]
    VisSOP.km[sop.r] <- VisR.km[sop.r] 
    am.all$VisSOP.km <- VisSOP.km
    #Ck
      summary(am.all$VisSOP.km) 
      summary(am.all$SOP) 
      summary(am.all$VisX.km) 
      length(sop.l)
      length(sop.r)
      length(sop.l) + length(sop.r)
      names(am.all)

  #Output
    save(am.all, file=outfnam)
    #write.csv(am.all, file=outfnam.csv)
    rm(am.all)