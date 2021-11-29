#Script CAPs_Smry.r...Megan C. Ferguson...30 November 2018

  #NOTES.  (See also CAPs_Smry_NotesAndMetadata_v3)
  #
  #CAPs Summary Table
  #
  #  1.	Only large whale species are used to compute the CAPs statistics.  The species and associated species codes are:
  #  a.	Ba = minke whale
  #  b.	Bm = bowhead whale
  #  c.	Bp = fin whale
  #  d.	Er = gray whale
  #  e.	Mn = humpback whale
  #  
  #  2.	The current code summarizes data collected during CAPs passing (FltType 7) and CAPs circling (FltType 9).  
  #     The current code does not know how to work with CAPs strip data (FltType 11), but this can easily be added 
  #      in the future.
  #  
  #  3.	The summary table has one row for each CAPs session.  A CAPs session is defined as all saved=1 records 
  #     in the Flight table beginning with Entry=CAPs and ending with the last record prior to the first Entry=deadhead, 
  #     search, or start transect later in the flight.  There may be more than one CAPs session per flight.
  #  
  #  4.	For each row of the CAPs summary table (i.e., each CAPs session), the sum of the Xx_Circ_Prop should add 
  #     to 1.0
  #  
  #  5.	Xx_Adj_calf should never be smaller than Xx_Pass_calf
  #  
  #  6.	Xx_Adj_ind should never be smaller than Xx_Pass_ind
  #  
  #  7.	Xx_Adj_n should never be smaller than Xx_Pass_n
  #  
  #  8.	Variable definitions: The variables defined below use "Xx" as a substitute for the 2-letter species code, 
  #     "n" = number of sightings, "ind" = number of individual whales, "Circ" = CAPs circling, "Pass" = CAPs passing, 
  #     "Add" = additional, and "Adj" = adjusted.
  #  
  #  FltNo: Flight Number
  #  StartDateTime: Date and time of the CAPs entry initiating the CAPs session
  #  StartEventNo: Event number corresponding to the CAPs entry initiating the CAPs session
  #  n_WhaleSp: Number of large whale species in the CAPs session.  Any large whale identified to species species 
  #             that was recorded during FltType 7 or 9 is counted.  This excludes Species = unidentified cetacean.
  #  Pass_n_unid: Number of unidentified cetacean sightings during CAPs passing 
  #  Xx_Circ_n: Number of sightings of species Xx sighted during CAPs circling
  #  Xx_Circ_ind: Number of whales of species Xx sighted during CAPs circling
  #  Xx_Circ_Prop: Proportion of sightings of species Xx during CAPs circling = (Xx_Circ_n)/(total # sightings of all 
  #             large whales identified to species during circling).  The denominator of this variable excludes 
  #             unidentified cetaceans.
  #  Xx_Circ_avggrp: Average group size (FinalGrp) of species Xx during CAPs circling 
  #                 = (Xx_Circ_ind)/(Xx_Circ_n)
  #  Xx_Circ_calf: Total number of calves of species Xx sighted during CAPs circling
  #  Xx_Circ_avgcalf: Average number of calves of species Xx per sighting during CAPs circling 
  #                 = (Xx_Circ_calf)/(Xx_Circ_n)
  #  Xx_Circ_Prop_FeedMill: Proportion of sightings of species Xx sighted during CAPs circling with behavior feed or mill
  #  Xx_Pass_n: Number of sightings identified as species Xx during CAPs passing
  #  Xx_Pass_ind: Number of whales identified as species Xx during CAPs passing, based on FinalGrp
  #  Xx_Pass_avggrp: Average size of groups (FinalGrp) identified as species Xx during CAPs passing 
  #                 = (Xx_Pass_ind)/(Xx_Pass_n)
  #  Xx_Pass_calf: Total number of calves identified as species Xx during CAPs passing
  #  Xx_Pass_Prop_FeedMill: Proportion of sightings of species Xx sighted during CAPs passing with behavior feed or mill
  #  Xx_Add_n: Additional number of CAPs passing sightings for species Xx, based on proportion of sightings 
  #            of species Xx during CAPs circling and the number of unid cetacean sightings during CAPs passing: 
  #            Xx_Add_n = Xx_Circ_Prop*Pass_n_unid
  #            If species Xx was not sighted during circling, Xx_Add_n will be zero.
  #  Xx_Add_ind: Additional number of whales for species Xx: 
  #            Xx_Add_ind=Xx_Add_n*max(Xx_Circ_avggrp, Xx_Pass_avggrp)
  #            In other words, Xx_Add_n is multiplied by the larger of Xx_Circ_avggrp or Xx_Pass_avggrp.  
  #            Xx_Add_ind will be zero if species Xs was not sighted during circling.
  #  Xx_Adj_n: Adjusted number of sightings of species Xx, after accounting for unid cetaceans: 
  #            Xx_Adj_n = Xx_Pass_n + Xx_Add_n.
  #  Xx_Adj_ind: Adjusted number of whales of species Xx: 
  #            Xx_Adj_ind = Xx_Pass_n* max(Xx_Circ_avggrp, Xx_Pass_avggrp) + Xx_Add_ind.
  #  Xx_Adj_calf: Adjusted number of calves during passing = max(Xx_Circ_avgcalf*Xx_Adj_n, Xx_Pass_calf).
  #
  #CAPs Sighting Table
  #
  #  1.	Relies on statistics computed for the CAPs Summary Table, defined above
  #  2.	This table includes a single row for every saved=1 large whale sighting identified to species during CAPs passing.  Every saved=1 unid cetacean sighting during CAPs passing is replicated n_WhaleSp times.
  #  3.	Variable definitions:
  #     a.	Event: Event number of sighting in Flight table
  #     b.  StartEvent: Event number corresponding to the CAPs session for the sighting
  #     c.	FltNo: Flight number
  #     d.  Month: Month of sighting
  #     e.	Yr: Year of sighting
  #     f.  Behavior: Behavior of original sighting
  #     g.  IcePctL and R: Ice percent on the L and R side of plane for original sighting
  #     h.  Blk: Survey block of original sighting
  #     i.  Depth: Depth of original sighting
  #     j.	XWhale: Longitude of sighting, from Flight table
  #     k.	YWhale: Latitude of sighting, from Flight table
  #     l.  OffshDist: Offshore distance of original sighting
  #     m.	Species: Species recorded for sighting in Flight table
  #     n.	new.Sp: New species identification assigned to this record, based on species identifications during CAPs 
  #         circling in this CAPs session
  #     o.	FinalGrp: FinalGrp of sighting, from Flight table
  #     p.	capsN: Number of sightings this record represents, after accouting for CAPs statistics.  capsN for 
  #         unid cetacean sightings equals Xx_Circ_Prop for the corresponding new.Sp.  capsN for sightings originally 
  #         identified to species will always be 1.
  #     q.	capsInd: Number of individuals this record represents, after accouting for CAPs statistics.  capsInd for 
  #         unid cetaceans equals Xx_Add_ind/Pass_n_unid.  capsInd for sightings originally identified to species 
  #         equals max(Xx_Pass_avggrp, Xx_Circ_avggrp).
  #     r.	capsCalf: Number of calves this record represents, after accounting for CAPs statistics.  capsCalf 
  #         for unid cetaceans and sightings originally identified to species equals Xx_Adj_calf/(Pass_n_unid+XX_Pass_n).
  #     s.  capsPropFeedMill: Proportional feeding/milling behavior, defined as follows:
  #           i. For CAPs passing sightings with behaviors recorded in the original survey data, capsPropFeedMill = 1
  #              if Behavior = feed or mill, and capsPropFeedMill = 0 for all other behaviors.
  #           ii. For CAPs passing sightings lacking behavior in the original survey data, 
  #              capsPropFeedMill = Xx_Circ_Prop_FeedMill from the CAPs session.
  #  4.	For a given CAPs session, the following should always be true:
  #     a.	The sum of all capsN values for species Xx should equal Xx_Adj_n in the CAPs Summary Table
  #     b.	The sum of all capsInd values for species Xx should equal Xx_Adj_ind in the CAPs Summary Table
  #     c.	The sum of all capsCalf values for species Xx should equal Xx_Adj_calf in the CAPs Summary Table

  #Input data
    load("Data//am_all_18v1.Rdata")
    
  #Format output into a single table 
    
    #Define row names for CAPs summary table
      spp <- c("Ba", "Bm", "Bp", "Er", "Mn")
    
      sp.var.names <- c("Circ_n", "Circ_ind", "Circ_Prop", "Circ_avggrp", "Circ_calf", "Circ_avgcalf", "Circ_Prop_FeedMill",
                        "Pass_n", "Pass_ind", "Pass_avggrp", "Pass_calf", "Pass_Prop_FeedMill",
                        "Add_n", "Add_ind",
                        "Adj_n", "Adj_ind", "Adj_calf")
      
      cnames <- sapply(1:length(spp), function(i){
        sp.i <- paste(spp[i], sp.var.names, sep="_")
      })
      cnames <- c("FltNo", "StartDateTime", "StartEventNo", "n_WhaleSp", "Pass_n_unid",
                  cnames[,1], cnames[,2], cnames[,3], cnames[,4], cnames[,5])
      cnames
      
  #Find flights with CAPs effort
    caps.FltNo <- unique(am.all$FltNo[which(am.all$Entry == "CAPs")])
    
    smry.df <- rbind.data.frame(rep(NA, length(cnames)))
    colnames(smry.df) <- cnames
    dim(smry.df)
    summary(smry.df)
    
    sight.df <- rbind.data.frame(rep(NA,20))
    names(sight.df) <- c("Event","StartEventNo","FltNo", "Month", "Yr", "Behavior",
                         "IcePctL", "IcePctR", "Blk", "Depth", "XWhale", "YWhale", "OffshDist",
                         "Species", "new.Sp", "FinalGrp", "capsN", "capsInd", "capsCalf", "capsPropFeedMill")

    for(f in 1:length(caps.FltNo)){

      debugg <- FALSE
      #debugg <- TRUE
  
      if(debugg == TRUE){
        f <- 4
        i <- 1
      }

      #Extract data for one CAPs flight
        idx <- which(am.all$FltNo == caps.FltNo[f])
        flt.dat <- am.all[idx,]
        #CK
          summary(flt.dat$FltNo)
        
      #Determine number of CAPs sessions 
        idx.caps <- which(flt.dat$Entry == "CAPs")
        n.caps <- length(idx.caps)
        #CK
          n.caps
        
      #Do the following for each CAPs session
        caps.smry <- lapply(1:n.caps, function(i){
          
          session <- flt.dat$Event[idx.caps[i]]
          start.date.time <- as.character(flt.dat$cDatTim[idx.caps[i]])
          #CK
            session
            start.date.time
          
          #Find end of this session and extract data only for this session
            idx.not.caps <- which(flt.dat$FltType != -1 &
                                  flt.dat$FltType != 7 &
                                  flt.dat$FltType != 9 &
                                  flt.dat$FltType != 11)
            test.idx <- idx.not.caps > idx.caps[i]
            end.idx <- which(test.idx == TRUE)[1]
    
            caps.dat <- flt.dat[idx.caps[i]:(idx.not.caps[end.idx] - 1),]
            caps.dat$Species <- as.vector(caps.dat$Species)
            #CK
              caps.dat$Event
            
          #Number of unid sightings during CAPs passing
            n.pass.unid <- length(which(caps.dat$Species == "unid cetacean" & 
                               caps.dat$FltType == 7))
            #Ck
              n.pass.unid
            
          #"whale" species IDs recorded during this session.  Do not include beluga sightings.
            session.spID <- unique(caps.dat$Species)
            test.whale <- grep("whale", session.spID)
            session.spID <- session.spID[test.whale]  #extract only "whale" IDs
            test.Dl <- grep("beluga", session.spID)
            if(length(test.Dl) > 0) session.spID <- session.spID[-test.Dl] #omit beluga sightings
            n.sp <- length(session.spID) #total number of whale species identified in this session
            #CK
              n.sp
              session.spID
              
          if(n.sp > 0){    
              
            #Compute statistics for each whale species sighted during caps circling.  Statistics computed:
            #Circ_Prop_, Circ_avggrp_, and Circ_avgcalf_.  Circ_Prop_ should be based on percentage
            #of sightings, not individuals, because this percentage will be used to adjust the 
            #caps passing data, which are encounter rate (n/L) data.  
                
              #Identify and count total number of "whale" (non-beluga) sightings during caps circling
                caps.circl.dat <- caps.dat[which(caps.dat$FltType == 9),]
                tot.circl.w <- sum(caps.circl.dat$Species %in% session.spID)
                #CK
                  tot.circl.w
                
              circ.stats.by.sp <- sapply(1:n.sp, function(w){
                
                #Extract sightings of species w on caps circling 
                  w.idx <- which(caps.circl.dat$Species == session.spID[w]) 
                  
                  if(length(w.idx) > 0){
                    
                    circ.n <- length(w.idx)
                    
                    circ.ind <- sum(caps.circl.dat$FinalGrp[w.idx])
                    
                    p.w <- circ.n/tot.circl.w #proportion of species w during caps circling
                    
                    avg.gs.w <- mean(caps.circl.dat$FinalGrp[w.idx]) #avg grp size of sp w during caps circling
                    
                    fm.idx <- which(caps.circl.dat$Behavior[w.idx] == "feed" |
                                    caps.circl.dat$Behavior[w.idx] == "mill")
                    circ.prop.feed.mill <- length(fm.idx)/length(w.idx)
                    
                    calf.w.idx <- which(caps.circl.dat$CalfNo[w.idx] > 0)
                    if(length(calf.w.idx)>0){
                      circ.calf <- sum(caps.circl.dat$CalfNo[w.idx[calf.w.idx]])
                      avg.calf.w <- circ.calf/circ.n
                    } else {
                      circ.calf <- 0
                      avg.calf.w <- 0
                    }
                  } else {
                    circ.n <- 0
                    circ.ind <- 0
                    p.w <- 0
                    avg.gs.w <- 0
                    circ.calf <- 0
                    avg.calf.w <- 0
                    circ.prop.feed.mill <- 0
                  }
                  return(c("Circ_n"=circ.n, "Circ_ind"=circ.ind, "Circ_Prop"=p.w, "Circ_avggrp"=avg.gs.w, 
                           "Circ_calf"=circ.calf, "Circ_avgcalf"=avg.calf.w, "Circ_Prop_FeedMill"=circ.prop.feed.mill))
                  
              })  
              #CK
                circ.stats.by.sp
                
            #Compute statistics for each whale species sighted during caps passing.  Statistics computed:
            #Pass_n_, Pass_ind_, Pass_avggrp_, Pass_calf_. 
              
              #Identify and count total number of "whale" (non-beluga) sightings during caps passing
                caps.pass.dat <- caps.dat[which(caps.dat$FltType == 7),]
  
              pass.stats.by.sp <- sapply(1:n.sp, function(w){
                
                #Extract sightings of species w on caps passing 
                  w.idx <- which(caps.pass.dat$Species == session.spID[w]) 
                  
                  if(length(w.idx) > 0){
                    n.w <- length(w.idx) #number of sightings of sp w during caps passing
                    
                    ind.w <- sum(caps.pass.dat$FinalGrp[w.idx]) #number of individuals of sp w during caps passing
                    
                    avg.gs.w <- mean(caps.pass.dat$FinalGrp[w.idx]) #avg grp size of sp w during caps passing
                    
                    fm.idx <- which(caps.pass.dat$Behavior[w.idx] == "feed" |
                                    caps.pass.dat$Behavior[w.idx] == "mill")
                    pass.prop.feed.mill <- length(fm.idx)/length(w.idx)
                    
                    calf.w.idx <- which(caps.pass.dat$CalfNo[w.idx] > 0)
                    if(length(calf.w.idx)>0){
                      n.calf.w <- sum(caps.pass.dat$CalfNo[w.idx[calf.w.idx]])
                    } else {
                      n.calf.w <- 0
                    }
                  } else {
                    n.w <- 0
                    ind.w <- 0
                    avg.gs.w <- 0
                    n.calf.w <- 0
                    pass.prop.feed.mill <- 0
                  }
                  return(c("Pass_n"=n.w, "Pass_ind"=ind.w, "Pass_avggrp"=avg.gs.w, "Pass_calf"=n.calf.w,
                           "Pass_Prop_FeedMill"=pass.prop.feed.mill))
                  
              })  
              #CK
                pass.stats.by.sp
            
            #Compute "add-ons" for each species based on statistics from caps circling.  Statistics include
            #Add_n_, Add_ind_, Adj_n_, Adj_ind_, and Adj_calf_.
              
                add.adj.stats.by.sp <- sapply(1:n.sp, function(w){
                  
                  #Additional number of caps passing sightings of species w, based on proportion of sightings 
                  #of species w during caps circling and number of unid cetaceans sighted during caps passing.
                  #If species w was not sighted during caps circling, the add-on value will be zero.
                    add.n <- as.vector(circ.stats.by.sp[3,w])*n.pass.unid #use as.vector() to remove object name
                    adj.n <- as.vector(pass.stats.by.sp[1,w]) + add.n
                    
                  #Adjust number of caps passing individuals of species w by the larger of the two values:
                  #  a) avg grp size of species w during caps circling
                  #  b) avg grp size of species w during caps passing
                  #Multiply add.n and pass.n by this "maximum" avg group size value.
                  #If species w was not sighted during caps circling, the add-on value will be 
                  #zero and the adjusted value will equal the passing mode value.
                    gs <- max(as.vector(circ.stats.by.sp[4,w]), as.vector(pass.stats.by.sp[3,w]))
                    add.ind <- add.n*gs
                    adj.ind <- as.vector(pass.stats.by.sp[1,w])*gs + add.ind
                    
                  #Adjusted number of caps passing calves of species w will be the larger of the two values:
                  # a) (avg.calf.w from circling)*adj.n
                  # b) Pass_calf
                    adj.calf <- max(as.vector(circ.stats.by.sp[6,w])*adj.n, as.vector(pass.stats.by.sp[4,w]))
                    
                  return(c("Add_n"=add.n, "Adj_n"=adj.n, "Add_ind"=add.ind, "Adj_ind"=adj.ind, "Adj_calf"=adj.calf))  
                })
                #CK
                  add.adj.stats.by.sp
                  
            #Return data for caps session i
              smry.df <- rbind.data.frame(circ.stats.by.sp, pass.stats.by.sp, add.adj.stats.by.sp)
              names(smry.df) <- session.spID
              
          } else {
            smry.df <- NA 
          }    

          #Generate detailed CAPs passing sighting table
              
            #Extract caps passing sightings of unids and large whales 
              caps.pass.dat <- caps.dat[which(caps.dat$FltType == 7),]
              lrgw.idx <- caps.pass.dat$Species %in% c("unid cetacean", session.spID)
              caps.pass.lrgw.dat <- caps.pass.dat[lrgw.idx,]
              #CK
                summary(as.factor(caps.pass.lrgw.dat$Species))
                nrow(caps.pass.lrgw.dat)
              
              unid.idx <- which(caps.pass.lrgw.dat$Species == "unid cetacean") #find unid cetacean sightings
              w.idx <- which(caps.pass.lrgw.dat$Species %in% session.spID)     #find ID'd large whale sightings
              
            if(n.pass.unid > 0 & n.sp > 0){  
            
                #Replicate unid cetacean sightings n.sp times
                  new.unid.idx <- rep(unid.idx, each=n.sp)
                  
                #Create new dataframe, sorted chronologically, that includes single rows for each 
                #identified cetacean sighting and n.sp number of rows for each unid cetacean sighting.  Column names
                #are:
                # Event, StartEventNo, FltNo, cDatTime, StartEventNo, Blk, Depth, XWhale, YWhale, OffshDist,
                # Species, new.Sp, FinalGrp, capsN, capsInd, capsCalf, capsPropFeedMill
                #
                # For unid cetacean sightings: 
                #   capsN = smry.df$Circ_Prop
                #   capsInd = smry.df$Add_ind/n.pass.unid
                #   capsCalf = smry.df$Adj_calf/(length(unid.idx)+length(w.idx))
                #   capsPropFeedMill = smry.df$Xx_Circ_Prop_FeedMill
                #
                # For identified cetacean sightings:
                #   capsN = 1
                #   capsInd = 1*max(smry.df$Pass_avggrp, smry.df$Circ_avggrp)
                #   capsCalf = 1*smry.df$Adj_calf/(length(unid.idx)+length(w.idx))
                #   capsPropFeedMill = smry.df$Xx_Circ_Prop_FeedMill if Behavior == NA,
                #                      1 if Behavior == "feed" or "mill" 
                #                      0 if Behavior != "feed" or "mill"
                  
                  df.cols <- c(2,3,78,77,15,38,39,42,43,62,63,64,12,18) 
                  
                  new.unid.df <- caps.pass.lrgw.dat[new.unid.idx,df.cols]
                  new.unid.df$new.Sp <- rep(session.spID, n.pass.unid)
                  
                  new.unid.df$capsPropFeedMill <- rep(as.vector(unlist(smry.df[7,])), n.pass.unid)
                  for(i in 1:nrow(new.unid.df)){
                    #Replace capsPropFeedMill for definitely feeding sightings with 1
                      idx <- which(new.unid.df$Behavior == "feed" | new.unid.df$Behavior == "mill")
                      new.unid.df$capsPropFeedMill[idx] <- 1

                    #Replace capsPropFeedMill for definitely NOT feeding sightings with 0
                      idx <- which(new.unid.df$Behavior != "feed" & new.unid.df$Behavior != "mill" &
                                   is.na(new.unid.df$Behavior) == FALSE)
                      new.unid.df$capsPropFeedMill[idx] <- 0
                  }
                  
                  unid.n <- as.vector(unlist(smry.df[3,])) 
                  new.unid.df$capsN <- rep(unid.n, n.pass.unid)
                  
                  unid.ind <- as.vector(unlist(smry.df[15,]))/n.pass.unid 
                  new.unid.df$capsInd <- rep(unid.ind, n.pass.unid)
                  
                  capsCalf <- as.vector(unlist(smry.df[17,]))/(length(unid.idx)+length(w.idx)) 
                  new.unid.df$capsCalf <- rep(capsCalf, n.pass.unid)
                  
                  caps.pass.id.dat <- caps.pass.lrgw.dat[w.idx,df.cols]
                  caps.pass.id.dat$new.Sp <- caps.pass.id.dat$Species
                  
                  caps.pass.id.dat$capsN <- 1
                  
                  id.ind <- sapply(1:length(w.idx), function(x){
                    sp.id <- which(names(smry.df) == caps.pass.id.dat$Species[x])
                    ind <- max(smry.df[10,sp.id], smry.df[4,sp.id])
                    return(ind)
                  })
                  caps.pass.id.dat$capsInd <- id.ind
                  
                  id.calf <- sapply(1:length(w.idx), function(x){
                    sp.id <- which(names(smry.df) == caps.pass.id.dat$Species[x])
                    calf <- capsCalf[sp.id]
                    return(calf)
                  })
                  caps.pass.id.dat$capsCalf <- id.calf
                  
                  id.capsPropFeedMill <-  sapply(1:length(w.idx), function(x){
                    if(is.na(caps.pass.id.dat$Behavior[x]) == TRUE){ #No behavior recorded for original sighting
                      sp.id <- which(names(smry.df) == caps.pass.id.dat$Species[x])
                      b <- smry.df[7,sp.id] #return Circ_Prop_FeedMill
                    } else{ #there was a behavior recorded for original sighting
                      if(caps.pass.id.dat$Behavior[x] == "feed" | caps.pass.id.dat$Behavior[x] == "mill"){
                        b <- 1
                      } else {
                        b <- 0
                      }
                    }
                    return(b)
                  })
                  caps.pass.id.dat$capsPropFeedMill <- id.capsPropFeedMill
                  
                  all.caps.pass.df <- rbind.data.frame(new.unid.df, caps.pass.id.dat)[, c(1:13,15,14,16:19)]
                  all.caps.pass.df$StartEventNo <- session
                  all.caps.pass.df <- all.caps.pass.df[,c(1,20,2:19)]
                  df.idx <- sort.int(all.caps.pass.df$Event, index.return=TRUE)
                  all.caps.pass.df <- all.caps.pass.df[df.idx$ix,]                  
                  #CK
                    names(all.caps.pass.df) #Event, StartEventNo, FltNo, Month, Yr, Behavior, 
                                            #IcePctL, IcePctR, Blk, Depth, XWhale, YWhale, OffshDist, 
                                             #Species, new.Sp, FinalGrp, capsN, capsInd, capsCalf, capsPropFeedMill
                    all.caps.pass.df$Event  #Should be in order
                    
                    nrow(all.caps.pass.df) - (n.sp*n.pass.unid) - length(w.idx) #0
                    
                    #The following check should be tweaked for each caps session
                      idx <- which(all.caps.pass.df$new.Sp == "fin whale")
                      sum(all.caps.pass.df$capsN[idx]) - smry.df[12,1]
                      sum(all.caps.pass.df$capsInd[idx]) - smry.df[14,1]
                      sum(all.caps.pass.df$capsCalf[idx]) - smry.df[15,1]
            } else if(n.sp > 0 & length(w.idx > 0)){ #Generate detailed CAPs passing sighting table when there are no unid 
                                             #cetaceans on passing but there are large whales on passing
              
              df.cols <- c(2,3,78,77,15,38,39,42,43,62,63,64,12,18) 
              
              all.caps.pass.df <- caps.pass.lrgw.dat[,df.cols]
              all.caps.pass.df$new.Sp <- all.caps.pass.df$Species
                  
              all.caps.pass.df$capsN <- 1
                  
              id.ind <- sapply(1:length(w.idx), function(x){
                sp.id <- which(names(smry.df) == all.caps.pass.df$Species[x])
                ind <- max(smry.df[10,sp.id], smry.df[4,sp.id])
                return(ind)
              })
              all.caps.pass.df$capsInd <- id.ind
              
              caps.calf <- as.vector(unlist(smry.df[17,]))/(length(unid.idx)+length(w.idx))
              id.calf <- sapply(1:length(w.idx), function(x){
                  sp.id <- which(names(smry.df) == all.caps.pass.df$Species[x])
                  calf <- caps.calf[sp.id]
                  return(calf)
                })
              all.caps.pass.df$capsCalf <- id.calf
              
              id.capsPropFeedMill <- sapply(1:length(w.idx), function(x){
                    if(is.na(all.caps.pass.df$Behavior[x]) == TRUE){ #No behavior recorded for original sighting
                      sp.id <- which(names(smry.df) == all.caps.pass.df$Species[x])
                      b <- smry.df[7,sp.id] #return Circ_Prop_FeedMill
                    } else{ #there was a behavior recorded for original sighting
                      if(all.caps.pass.df$Behavior[x] == "feed" | all.caps.pass.df$Behavior[x] == "mill"){
                        b <- 1
                      } else {
                        b <- 0
                      }
                    }
                    return(b)
              })
              all.caps.pass.df$capsPropFeedMill <- id.capsPropFeedMill
              
              all.caps.pass.df <- all.caps.pass.df[, c(1:13,15,14,16:19)]
              all.caps.pass.df$StartEventNo <- session
              all.caps.pass.df <- all.caps.pass.df[,c(1,20,2:19)]
              #Ck
                names(all.caps.pass.df) #Event, StartEventNo, FltNo, Month, Yr, Behavior,
                                        #ICePctL, IcePctR, Blk, Depth, XWhale, YWhale, OffshDist,  
                                         #Species, new.Sp, FinalGrp, capsN, capsInd, capsCalf, capsPropFeedMill
                all.caps.pass.df$Event  #Should be in order
                
                nrow(all.caps.pass.df) - (n.sp*n.pass.unid) - length(w.idx) #0
                
                #The following check should be tweaked for each caps session if want to debug
                  idx <- which(all.caps.pass.df$new.Sp == "bowhead whale")
                  sum(all.caps.pass.df$capsN[idx]) - smry.df[12,1]
                  sum(all.caps.pass.df$capsInd[idx]) - smry.df[14,1]
                  sum(all.caps.pass.df$capsCalf[idx]) - smry.df[15,1]
              
            } else {
              all.caps.pass.df <- NA
            } 

          return(list("FltNo"=caps.FltNo[f], "StartDateTime"=start.date.time, "StartEventNo"=session, 
                            "n_WhaleSp"=n.sp, "Pass_n_unid"=n.pass.unid, "Species_Smry"=smry.df,
                            "DetailedSightings"=all.caps.pass.df))
              
          #end code for caps session i  

        }) 
        
        save(caps.smry, file=paste("CapsSmryRdata//caps_smry",f,".Rdata",sep="")) #one .Rdata file per flight
        
        for(x in 1:length(caps.smry)){
          d <- nrow(smry.df) + 1
          smry.df <- rbind.data.frame(smry.df, c(rep(NA, 3), rep(0,87)))
          smry.df[d,1] <- caps.smry[[x]][[1]]
          smry.df[d,2] <- caps.smry[[x]][[2]]
          smry.df[d,3] <- caps.smry[[x]][[3]]
          smry.df[d,4] <- caps.smry[[x]][[4]]
          smry.df[d,5] <- caps.smry[[x]][[5]]
          
          if(caps.smry[[x]][[4]] > 0){
          
            for(w in 1:caps.smry[[x]][[4]]){
              
              if(colnames(caps.smry[[x]][[6]])[w] == "minke whale"){
                idx <- 1
              } else if(colnames(caps.smry[[x]][[6]])[w] == "bowhead whale"){
                idx <- 2
              } else if(colnames(caps.smry[[x]][[6]])[w] == "fin whale"){
                idx <- 3
              } else if(colnames(caps.smry[[x]][[6]])[w] == "gray whale"){
                idx <- 4
              } else if(colnames(caps.smry[[x]][[6]])[w] == "humpback whale"){
                idx <- 5
              }
              
              c.idx <- ((idx-1)*17)+6 
              smry.df[d,c.idx] <- caps.smry[[x]][[6]][1,w] #circ.n
              smry.df[d,c.idx+1] <- caps.smry[[x]][[6]][2,w] #circ.ind
              smry.df[d,c.idx+2] <- caps.smry[[x]][[6]][3,w] #circ.prop
              smry.df[d,c.idx+3] <- caps.smry[[x]][[6]][4,w] #circ.avggrp
              smry.df[d,c.idx+4] <- caps.smry[[x]][[6]][5,w] #circ.calf
              smry.df[d,c.idx+5] <- caps.smry[[x]][[6]][6,w] #circ.avgcalf
              smry.df[d,c.idx+6] <- caps.smry[[x]][[6]][7,w] #circ.prop.feed.mill
              smry.df[d,c.idx+7] <- caps.smry[[x]][[6]][8,w] #pass.n
              smry.df[d,c.idx+8] <- caps.smry[[x]][[6]][9,w] #pass.ind
              smry.df[d,c.idx+9] <- caps.smry[[x]][[6]][10,w] #pass.avggrp
              smry.df[d,c.idx+10] <- caps.smry[[x]][[6]][11,w] #pass.calf
              smry.df[d,c.idx+11] <- caps.smry[[x]][[6]][12,w] #pass.prop.feed.mill
              smry.df[d,c.idx+12] <- caps.smry[[x]][[6]][13,w] #add.n
              smry.df[d,c.idx+13] <- caps.smry[[x]][[6]][15,w] #add.ind
              smry.df[d,c.idx+14] <- caps.smry[[x]][[6]][14,w] #adj.n
              smry.df[d,c.idx+15] <- caps.smry[[x]][[6]][16,w] #adj.ind
              smry.df[d,c.idx+16] <- caps.smry[[x]][[6]][17,w] #adj.calf

            } #Finished with all species in caps.smry
            
            sight.df <- rbind.data.frame(sight.df, caps.smry[[x]][[7]])
          
          }

        } #Finished updating smry.df for this flight

        #end code for flight f   
    }  
    
    smry.df <- smry.df[-1,]
    write.csv(smry.df, file="Output//CAPs_Smry.csv")
    
    sight.df <- sight.df[-1,]
    write.csv(sight.df, file="Output//CAPs_Sightings.csv")
    

      
      
     
                
                
    
      
      
      
      
      
      
      
      