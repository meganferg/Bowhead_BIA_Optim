#Bm_BIA_Optimization_C1.r...Megan C. Ferguson...3 August 2020

#  NOTES
#  1. This script is based on Bm_BIA_Optimization_C1.r, but works only on problems with
#     "cluster size" = 1. It requires some objects created by Bm_BIA_Optimization_Z_Deux.r.

  library(stringr)
  library(sp)
  library(rgeos)
  library(maptools)
  library(rgdal)
  library(fields)

  #Create function to produce CPLEX formulations for LP with "cluster size" of only 1
      
    debugg <- FALSE
    #debugg <- TRUE
    if(debugg == TRUE){
      c1 <- C1
      c2 <- C2
      C.set <- C1
      Z <- 0.5
      mo <- 9
      FM.spdf <- readOGR(dsn="Output0019/Shapefiles", 
                           layer="Bm_FeedMill_Rate_ByMo_0019_pooled")
      d.i <- FM.spdf@data$september
      fpath <- "Output_BIA_Z_Deux/CPLEX_LP/"
    }
    
    write.c1.cpx.LP <- function(c1, c2, C.set, Z, d.i, fpath, FM.calf, mo){
      
      #C.set should be a dataframe whose only columns are the i.# columns that provide
      #cell indices. 
        card.j <- ncol(C.set)
        
      #Create constraint matrix
        n.x.i <- nrow(c1) #number of x.i variables

        nc <- n.x.i #number of variables
        nr <- 1 #number of constraints
      
        c.mtx <- matrix(data=rep(0, nr*nc), nrow=nr)
        dim(c.mtx)
        colnames(c.mtx) <- c(paste("x",c1$i.1,sep="_")) #parcel indices, x.i

        rownames(c.mtx) <- c("Area")
    
      #Coeffs for constraint 2: area constraint
        c.mtx[1,1:n.x.i] <- 1
              
      #RHS vector and sense 
        rhs <- rep("0",nrow(c.mtx))
        sense <- rep(NA,nrow(c.mtx))
        
        #constraint 2
          n.occupied <- length(which(d.i>0))
          rhs[1] <- as.character(n.occupied*Z) #proportion of occupied cells included in solution
          sense[1] <- "<="

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
   
  #Create LP files for different values of Z, C.set, months, and activity states     
      
      #Set variables to cycle through
        Z.vec <- seq(from=0.1, to=1.0, by=0.1)
        C.list <- list(C1[,1], C2[,1:2], C3, C4, C5)
        fm.calf <- c("FM", "calf")
        
      #Set filenames, paths, and text that are common to all cycles  
        cplex.folder <- "/Output_BIA_Z_Deux/CPLEX_LP/"
        
        wd <- getwd()
        bat.txt <- rep("X",7)
        bat.txt[1] <- "set timelimit 3600"
        bat.txt[4] <- "mipopt"
        bat.txt[5] <- "change problem fixed"
        bat.txt[6] <- "primopt"
        bat.fnam <- paste(wd,cplex.folder,"batch_c1.txt",sep="")

        mos <- 7:10
        
        write.table("", file = bat.fnam, append = FALSE, 
                    quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                    col.names = FALSE)
      
      debugg <- FALSE
      #debugg <- TRUE
      if(debugg <- TRUE){
        z.idx <- 10
        m.idx <- 3
      }
      
      c.idx <- 1
        
        for(z.idx in 1:length(Z.vec)){
          
          for(m.idx in 1:length(mos)){

            #Create LP files
            
              folder <- substr(cplex.folder, start=2, stop=nchar(cplex.folder))
            
              write.c1.cpx.LP(c1=C1, c2=C2, C.set=data.frame(C1[,1]), Z=Z.vec[z.idx], 
                           d.i=FM.spdf@data[,m.idx], fpath=folder, FM.calf="FM", mo=mos[m.idx])
              
              write.c1.cpx.LP(c1=C1, c2=C2, C.set=data.frame(C1[,1]), Z=Z.vec[z.idx], 
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
        

      
      
      
      
      
      
  #  1. Change all of the "/" in the batch file to "\"  
  #  2. The cplex path for entering into terminal:
  #    C:\Program Files\ibm\ILOG\CPLEX_Studio129\cplex\bin\x64_win64
  #  3. The CPLEX_LP path in the working directory: 
  #    C:\Users\megan.ferguson\Work\FergusonFiles\ArcticMonkeys\Analysis\CamdenBayBehavior\Output_BIA_Z_Deux\CPLEX_LP
  #  4. Run CPLEX from the terminal 
      
      
      
      
      
      
      
      
      
      

  #Input cplex solutions and create objects to store results
      
      Z.vec <- seq(from=0.1, to=1.0, by=0.1)
      
      FM.gt0 <- list("jul"=jul.FM.gt0,
                                   "aug"=aug.FM.gt0,
                                   "sep"=sep.FM.gt0,
                                   "oct"=oct.FM.gt0)
        
      calf.gt0 <- list("jul"=jul.calf.gt0,
                                   "aug"=aug.calf.gt0,
                                   "sep"=sep.calf.gt0,
                                   "oct"=oct.calf.gt0)
      
      debugg <- FALSE
      #debugg <- TRUE
      if(debugg == TRUE){  
        c.idx <- 2
        z.idx <- 1
        m.idx <- 2
        j <- 1
      }  
      
      smry.df <- cbind.data.frame("c"=rep(0.0,80),
                                  "z"=rep(0.0,80),
                                  "m"=rep(0.0,80),
                                  "fm.calf"=rep(NA,80),
                                  "objVal"=rep(0.0,80),
                                  "sum.xi"=rep(0.0,80),
                                  "binary.x"=rep(TRUE,80))
      smry.idx <- 0

      c.idx <- 1
        
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
        

      #Output stuff.
      
        save(smry.df,
             file="Output_BIA_Z_Deux//OutLists//Bm_BIA_Optim_C1.rdata")
        write.csv(smry.df, file="Output_BIA_Z_Deux//OutLists//smry_df_c1.csv")
      
      
          
      
  