
length2age <- function(curveSel = "von Bertalanffy", numCoh = 10, Linf = 10, kappa = 0.5, tZero = 0, lengthIn = 5, timeIn = 0, sqrtSigma = 1:8){
  if(curveSel == "von Bertalanffy"){
    temp <- Linf * (1 - exp(-kappa*(((1:numCoh)-1 - tZero + timeIn))))
  }else{
    temp <- Linf *  exp(-(1/kappa * exp(-kappa * ((1:numCoh)-1 - tZero + timeIn))))
  }
  postProbs = dnorm(lengthIn, temp, sqrtSigma)
  return(as.numeric(names(which.max(table(sample(1:numCoh, size = 50, prob = postProbs, replace = TRUE))))))
}

calcGomp <- function(elle, kappa, ageVector){
  return(elle *  exp(-(1/kappa * exp(-kappa * ageVector))))
}


calcVonBert <- function(elle, kappa, ageVector){
  return(elle * (1 - exp(-kappa * ageVector)))
}


# RecLFD <- function(MAT,RANGE,NH){
#   vecL <- matrix(0,2,length(RANGE))
#   colnames(vecL) <- as.character(RANGE)
#   rownames(vecL) <- c("Female","Male")
#   for(ii in 1:nrow(MAT)){
#     vecL[1,which(colnames(vecL)== as.character(MAT[ii,1]))]=vecL[1,which(colnames(vecL)== as.character(MAT[ii,1]))]+MAT[ii,2]
#     vecL[2,which(colnames(vecL)== as.character(MAT[ii,1]))]=vecL[2,which(colnames(vecL)== as.character(MAT[ii,1]))]+MAT[ii,3]
#   }
#   vecL <- vecL/NH
#   return(vecL)
# }


# plotRecLFD <- function(reclfd, perc = TRUE, besid = FALSE){
#   par(mar = c(1.5,2,3,0))
#   if(besid){
#     barplot(t(reclfd)/max(reclfd), col = c("skyblue3","pink3"), beside = TRUE, cex.axis = 0.8, cex.names = 0.5, font.main = 4, space = c(0,0.3))
#   }else{
#     if(perc == TRUE){
#       reclfd[1,] <- reclfd[1,]/max(reclfd[1,])
#       reclfd[2,] <- reclfd[2,]/max(reclfd[2,])
#     }
#     pre_mfrow <- par("mfrow")
#     par(mfrow = c(2, 1))
#     barplot(reclfd[1,], ylim = c(0,1), col = "pink3", main = "Female", cex.axis = 0.5, cex.names = 0.6, font.main = 4, las = 2)
#     barplot(reclfd[2,], ylim = c(0,1), col = "skyblue3", main = "Male", cex.axis = 0.5, cex.names = 0.6, font.main = 4)
#     par(mfrow = pre_mfrow)
#   }
# }


IntInvDis <- function(xdata, RefCell, IntCell,
                      Refmax, Refmin, ncells,
                      Grid, graph=F, logplot=T){
  xdataRef <- xdata[RefCell,]
  colnames(xdataRef) <- c("LON","LAT","Coh")
  xdata.gstat <- gstat(id="Coh",
                       formula = Coh~1,
                       locations = ~ LON + LAT,
                       data = xdataRef,
                       nmax = Refmax,
                       nmin = Refmin)
  xdataOut <- numeric(ncells)
  xdataOut[RefCell] <- xdataRef[,3]
  xdataInt <- xdata[IntCell,]
  colnames(xdataInt) <- c("LON","LAT","Coh")
  xdataOut[IntCell] <- predict(xdata.gstat, xdataInt)[,3]
  if(graph){
    if(logplot){
      gv <- log10(xdataOut+1)
    }else{
      gv <- xdataOut
    }
    #cols <- rev(heat.colors(10))
    #colvec <- seq(round(min(gv),1),round(max(gv),1),length=10)
    #plot(Grid6min, col= cols[findInterval(gv,colvec)])
    levelplot(gv~LON+LAT, cbind(xdata[,1:2],gv), aspect = "fill")
  }
  return(as.data.frame(cbind(xdata[,1:2],xdataOut)))
}


# FindCenters=function(vertexes,nvert){
#   ncenters=nrow(vertexes)/nvert
#   coords=matrix(0,ncenters,2)
#   colnames(coords)=c("LON","LAT")
#   for(i in 1:ncenters)	  coords[i,]=c(mean(vertexes[(i+4*(i-1)):(i+4*(i-1)+4),4]),
#                                       mean(vertexes[(i+4*(i-1)):(i+4*(i-1)+4),5]))
#   return(coords)
# }

# GenS <- function(Abbmat, num_cla, qqM, LCspe, yea, num_coh, MixtureP){
#   new_dim <- dim(Abbmat)
#   new_dim[2] <- num_cla
#   new_dim[3] <- 1
#   TempArray <- array(0,dim=new_dim)
#   for(sex in 1:2){
#     for(coh in 1:num_coh){
#       mmcoh <- MixtureP[[sex]]$Means[yea,coh]
#       sdcoh <- MixtureP[[sex]]$Sigmas[yea,coh]
#       for(ij in 1:dim(TempArray)[1]){
#         abbcoh <- Abbmat[ij,coh,yea,sex]
#         add <- floor((1/qqM)*abbcoh*dnorm(LCspe,mmcoh,sdcoh))
#         TempArray[ij,,1,sex] <- TempArray[ij,,1,sex]+add
#       }
#     }
#   }
#   return(TempArray)
# }


GenPop <- function(Abbmat, num_cla, LCspe, RA, qMM, num_ye, num_coh, MixtureP){
  new_dim <- dim(Abbmat)
  new_dim[2] <- num_cla
  TempArray <- array(0,dim=new_dim)
  for(yy in 1:length(num_ye)){
    for(sex in 1:2){
      for(coh in 1:num_coh){
        mmcoh <- MixtureP[[sex]]$Means[yy,coh]
        sdcoh <- MixtureP[[sex]]$Sigmas[yy,coh]
        for(ij in 1:dim(TempArray)[1]){
          abbcoh <- Abbmat[ij,coh,yy,sex]
          add <- floor(RA*(1/qMM)*abbcoh*dnorm(LCspe,mmcoh,sdcoh))
          TempArray[ij,,yy,sex] <- TempArray[ij,,yy,sex]+add
        }
      }
    }
  }
  return(TempArray)
}


LFDtoBcell <- function(LCspe, abbF, abbM, LWpar){
  aF <- LWpar[1,1]
  bF <- LWpar[1,2]
  aM <- LWpar[2,1]
  bM <- LWpar[2,2]
  WLC_F <- aF * LCspe^bF
  WLC_M <- aM * LCspe^bM
  Bcell <- matrix(rep(WLC_F,nrow(abbF)),nrow(abbF),ncol(abbF),byrow=T) * abbF +
    matrix(rep(WLC_M,nrow(abbM)),nrow(abbM),ncol(abbM),byrow=T) * abbM
  return(Bcell)
}


# GenOgiveSel <- function(lenCla, SelPar){
#   LCspe <- lenCla + (lenCla[2]-lenCla[1])/2
#   L50 <- SelPar[1]
#   L75 <- SelPar[2]
#   S1 <- L50 * log(3) /(L75 - L50)
#   S2 <- log(3) /(L75 - L50)
#   SL <- 1/(1 + exp(S1 - S2*LCspe))
#   return(SL)
# }


# getLogit <- function(Lit, X, thrB, ptrain = 80, ptest = 20){
#   Litb = 1*(Lit > thrB)
#   # wcol <- which(substr(colnames(X),1,2) %in% c("FG","MO","YE"))
#   XY <- cbind(X,Litb)
#   itrain <- sample(1:nrow(XY),floor(nrow(XY)*ptrain/100),replace = FALSE)
#   itest <- setdiff(1:nrow(XY),unique(itrain))
#
#   # wFG <- which(substr(colnames(XY),1,2) =="FG")
#   wFG <- c(3:(ncol(XY)-1))
#   FGsum <- apply(XY[,wFG],2,sum)
#   zeroFG <- which(FGsum==0)
#   if(length(zeroFG)>0) XY <- XY[,-wFG[zeroFG]]
#
#   logit_f <- glm(Litb ~. , family = "binomial", data = as.data.frame(XY[itrain,]))
#   predf <- 1*(predict(logit_f, newdata = as.data.frame(XY[itest,-ncol(XY)]), type = "response")>0.5)
#
#   #Result #1: % of correct fish/non fish prediction
#   confm <- 100*sum(diag(table(predf,Litb[itest])))/sum(table(predf,Litb[itest]))
#   cat("\nLogit preliminary performance = ", confm, "%", sep = "")
#   logit_f <- glm(Litb ~. , family="binomial", data = as.data.frame(XY))
#   LogitList <- vector(mode="list", length=3)
#   LogitList[[1]] <- confm
#   LogitList[[2]] <- logit_f
#   LogitList[[3]] <- zeroFG
#   names(LogitList) <- c("confm","logit_f","zeroFG")
#   return(LogitList)
# }


# FindFG <- function(grid_shp,
#                    grid_polyset,
#                    grid_centres,
#                    ncells,
#                    cells_data,
#                    numCuts = 50,
#                    minsize = 10,
#                    modeska="S",
#                    skater_method){
#   set.seed(123)
#   #Build the neighboorhod list
#   Grid.bh <- grid_shp[1]
#   bh.nb <- poly2nb(Grid.bh, queen=TRUE)
#   bh.mat <- cbind(rep(1:length(bh.nb),lapply(bh.nb,length)),unlist(bh.nb))
#   #Compute the basic objects for clustering
#   lcosts <- nbcosts(bh.nb,cells_data)
#   nb.w <- nb2listw(bh.nb, lcosts, style=modeska)
#   mst.bh <- mstree(nb.w, ini=1)
#   index <- 0
#   clu_matrix <- matrix(NA,ncells,length(numCuts))
#   #Perform the first CC (without removing spurious clusters)
#   for(nCuts in numCuts){
#     cat("Performing CC with ",nCuts," cuts.......")
#     index <- index +1
#     res1 <- skater(mst.bh[,1:2], cells_data, ncuts = nCuts, minsize,
#                    method=skater_method)
#     clu_matrix[,index] <- res1$groups
#     cat("Done","\n")
#   }
#   IndexS <- IndexCH <- numeric(ncol(clu_matrix))
#   for(i in 1:ncol(clu_matrix)){
#     IndexS <- silhouette(clu_matrix[,i],dist(cells_data,
#                                              method=skater_method))
#     IndexCH <-  get_CH(cells_data,clu_matrix[,i])
#   }
# }

getNNLS <- function(subX, subY, zeroFG){
  nnls_fit = nnls(as.matrix(subX), subY)
  betas <- nnls_fit$x
  rss <- nnls_fit$deviance
  tss <- sum(subY^2)
  s2 <- rss/(nrow(subX)-length(betas)-1)
  adjr2 <- 1 - (rss/(nrow(subX)-length(betas)-1))/(tss/(nrow(subX)-1))
  r2 <- 1 - (rss/tss)
  nnls_m <- vector(mode="list",length=7)
  nnls_m[[1]] <- nnls_fit
  nnls_m[[2]] <- betas
  nnls_m[[3]] <- s2
  nnls_m[[4]] <- zeroFG
  nnls_m[[5]] <- r2
  nnls_m[[6]] <- subY
  nnls_m[[7]] <- as.numeric(nnls_fit$fitted)
  names(nnls_m) <- c("model","betas","s2","FGno","r2","obs","fitted")
  return(nnls_m)
}

fillbetas <- function(bmat){
  bdf <- as.data.frame(bmat)
  if(ncol(bmat) > 50){
    fbmat <- matrix(NA, nrow = nrow(bmat), ncol = ncol(bmat))
    numBlock <- seq(1, ncol(bdf), by = 50)
    if(!(ncol(bdf) %in% numBlock)) numBlock <- c(numBlock, ncol(bdf))
    for(iter in 1:(length(numBlock)-1)){
      if(iter < (length(numBlock)-1)){
        selVect <- numBlock[iter]:(numBlock[iter+1]-1)
      }else{
        selVect <- numBlock[iter]:numBlock[iter+1]
      }
      tmp_bdf <- bdf[,selVect]
      ff <- paste(colnames(tmp_bdf),"+",sep="",collapse="")
      ff <- as.formula(paste("~",substr(ff,1,nchar(ff)-1)))
      fbmat[,selVect] <- as.matrix(mnimput(ff,tmp_bdf)$filled.dataset)
    }
  }else{
    ff <- paste(colnames(bdf),"+",sep="",collapse="")
    ff <- as.formula(paste("~",substr(ff,1,nchar(ff)-1)))
    fbmat <- as.matrix(mnimput(ff,bdf)$filled.dataset)
  }
  return(fbmat)
}


# weight2number <- function(x){
#   # setwd("~/Desktop")
#   # x <- read.table("/Users/Lomo/Documents/Uni/R/smart/data/Resource\ -\ Fishery/fishery_data_CampBiol.csv", h=TRUE, sep=";",dec=".")[,c("DATE","LCLASS","UNSEX")]
#   colnames(x) <- c("UTC","LClass","KG")
#
#   a <- mean(c(0.00002648, 0.00001532))
#   b <- mean(c(2.823, 2.942))
#   Lmean <- sort(unique(x$LClass))
#   Wmean <- a*(Lmean+0.5)^b
#   WK <- data.frame(Length=Lmean,Weight=Wmean)
#   N <- numeric(nrow(x))
#
#   # for(i in 1:nrow(x))
#   #   N[i] <- x[i,"KG"]/WK[which(WK$Length==x[i,"LClass"]),"Weight"]
#   # N <- round(N,0)
#   N <- round(x$KG/WK[x$LClass-min(x$LClass)+1,]$Weight)
#
#   # Ldata <- matrix(0,0,2)
#   # for(i in 1:nrow(x)){
#   #   Ni <- N[i]
#   #   ll <- rep(x[i,"LClass"],Ni)+runif(Ni,0,1)
#   #   tt <- rep(x[i,"UTC"],Ni)
#   #   Ldata <- rbind(Ldata,data.frame(tt,ll))
#   # }
#   # colnames(Ldata) <- c("UTC","Length(cm)")
#   Ldata <- data.frame(UTC = rep(x[,"UTC"],N),
#                       Length = rep(x[,"LClass"], N)+runif(sum(N),0,1))
#   return(Ldata)
# }

genFlatEffo = function(effoPatt){
  npp_star = sum(effoPatt)
  pj = (effoPatt/npp_star)
  if(length(which(is.na(pj)))>0) pj[which(is.na(pj))] <- 0
  EFG = sample(1:length(effoPatt), ceiling(npp_star), prob = pj, replace = TRUE)
  Ename = as.numeric(names(table(EFG)))
  Estar = numeric(length(effoPatt))
  Estar[Ename] = as.numeric(table(EFG))
  return(Estar)
}

genFlatEffoDen = function(effoPatt, targetDensity){
  npp_star = sum(effoPatt)
  pj = (effoPatt/npp_star)*(1/targetDensity)
  pj[pj == Inf] <- 0
  pj = pj/sum(pj, na.rm = TRUE)
  if(length(which(is.na(pj)))>0) pj[which(is.na(pj))] <- 0
  EFG = sample(1:length(effoPatt), ceiling(npp_star), prob= pj, replace = TRUE)
  Ename = as.numeric(names(table(EFG)))
  Estar = numeric(length(effoPatt))
  Estar[Ename] = as.numeric(table(EFG))
  return(Estar)
}

genBanEffo = function(effoPatt, set0){
  npp_star <- sum(effoPatt)
  REstar <- effoPatt
  REstar[set0] <- 0
  if(sum(REstar) == 0) REstar[setdiff(1:length(effoPatt),set0)] <- sum(effoPatt)/length(setdiff(1:length(effoPatt),set0))
  pj = REstar/sum(REstar, na.rm = TRUE)
  if(length(which(is.na(pj)))>0) pj[which(is.na(pj))] <- 0
  Ecell = sample(1:length(effoPatt), ceiling(npp_star), prob = pj, replace = TRUE)
  Ename = as.numeric(names(table(Ecell)))
  Estar = numeric(length(effoPatt))
  Estar[Ename] = as.numeric(table(Ecell))
  return(Estar)
}

genBanEffoDen = function(effoPatt, set0, targetDensity){
  npp_star <- sum(effoPatt)
  REstar <- effoPatt
  REstar[set0] <- 0
  if(sum(REstar) == 0) 
    REstar[setdiff(1:length(effoPatt),set0)] <- sum(effoPatt)/length(setdiff(1:length(effoPatt),set0))
  pj = REstar/sum(REstar)*(1/targetDensity)
  pj[pj == Inf] <- 0
  pj = pj/sum(pj, na.rm = TRUE)
  if(length(which(is.na(pj)))>0) pj[which(is.na(pj))] <- 0
  Ecell = sample(1:length(effoPatt), ceiling(npp_star), prob = pj, replace = TRUE)
  Ename = as.numeric(names(table(Ecell)))
  Estar = numeric(length(effoPatt))
  Estar[Ename] = as.numeric(table(Ecell))
  return(Estar)
}

getFleetRevenue = function(predProd, lwStat, priceVec){
  outProp <- apply(predProd, 1, function(x) apply(t(lwStat*t(x))*priceVec,2, sum, na.rm = TRUE))
  outProp <- t(outProp)
  return(outProp)
}

getFleetRevSeason = function(predProd, monthVec, lwStat, priceVec){
  outProp <- matrix(data = 0, nrow = nrow(predProd), ncol = ncol(predProd))
  tmpSeason <- data.frame(Month = 1:12,
                          Season = c("winter", "winter", "spring",
                                     "spring", "spring", "summer",
                                     "summer", "summer", "fall",
                                     "fall", "fall", "winter"))
  for(season in c("winter", "spring", "summer", "fall")){
    tmpOutProp <- apply(predProd[monthVec %in% tmpSeason$Month[tmpSeason$Season == season],], 1, function(x) apply(t(lwStat[[season]][,-1]*t(x))*priceVec,2, sum, na.rm = TRUE))
    outProp[monthVec %in% tmpSeason$Month[tmpSeason$Season == season],] <- t(tmpOutProp)
  }
  return(outProp)
}

TheFunk2 <- function(Pars,DoEst=T,SpeciesData,Nspecies,ParInit,Phases,PredationPars){
  
  ExtVal <- Extract(Pars,ParInit,SpeciesData,Nspecies,Phases)
  
  ParOut <- ExtVal$ParOut
  AltPIN <- ExtVal$AltPIN
  InitN <- ExtVal$InitN
  RecDev <- ExtVal$RecDev
  LogR0 <- ExtVal$LogR0
  SurvSel <- ExtVal$SurvSel
  Selex <- ExtVal$Selex
  Fvals <- ExtVal$Fvals
  InitF <- ExtVal$InitF
  Prior <- ExtVal$Prior
  
  # Projection the population model and compute the negative log-likelihood
  Outs <- PopModel(Nspecies,SpeciesData,InitN,RecDev,LogR0,Fvals,Selex,InitF,PredationPars,Nproj=0)
  OutLikelihood <- Like(Nspecies,SpeciesData,Outs,SurvSel,RecDev,InitN)
  
  # Trick to get things passes
  if (DoEst==T){
    return(OutLikelihood$TotalLike+Prior)  
  }else{
    Out2 <- NULL
    
    Out2$ParOut <- ParOut
    Out2$AltPIN <- AltPIN
    Out2$TotalLike <- OutLikelihood$TotalLike
    Out2$Prior <- Prior
    
    AmaxA <- OutLikelihood$AmaxA
    Out2$Amax <- NULL
    for (Ispec in 1:Nspecies) Out2$Amax <- c(Out2$Amax,SpeciesData[[Ispec]]$Amax)
    NlenA <- OutLikelihood$NlenA
    Out2$Nlen <- NULL
    for (Ispec in 1:Nspecies) Out2$Nlen <- c(Out2$Nlen,SpeciesData[[Ispec]]$Nlen)
    
    Out2$Nyear <- length(Outs$SSB) 
    Out2$Nspecies <- Nspecies
    
    Out2$CvCatch <- OutLikelihood$CvCatch
    Out2$CvIndex <- OutLikelihood$CvIndex
    Out2$SigmaR <- OutLikelihood$SigmaR
    Out2$EffFish <- OutLikelihood$EffFish
    
    Out2$N <- Outs$N/1000
    Out2$FAA <- Outs$FAA
    Out2$SSB <- Outs$SSB/1000
    
    Out2$ObsSAA <- array(0,dim=c(Nspecies,100,AmaxA))
    Out2$YSAA <- matrix(0,nrow=Nspecies,ncol=100)
    Out2$SSAA <- matrix(0,nrow=Nspecies,ncol=100)
    for (Ispec in 1:Nspecies){
      if (length(SpeciesData[[Ispec]]$SAA)>0){ 
        Amax <- SpeciesData[[Ispec]]$Amax
        Nsurv <- length(SpeciesData[[Ispec]]$SAA[,1])
        Out2$ObsSAA[Ispec,1:Nsurv,1:Amax] <- SpeciesData[[Ispec]]$SAA
        Out2$YSAA[Ispec,1:Nsurv] <- SpeciesData[[Ispec]]$YSAA
        Out2$SSAA[Ispec,1:Nsurv] <- SpeciesData[[Ispec]]$SSAA
      }
    }
    Out2$PredSAA <- OutLikelihood$PredSurvA
    
    Out2$ObsSAL <- array(0,dim=c(Nspecies,100,NlenA))
    Out2$YSAL <- matrix(0,nrow=Nspecies,ncol=100)
    Out2$SSAL <- matrix(0,nrow=Nspecies,ncol=100)
    for (Ispec in 1:Nspecies){
      if (length(SpeciesData[[Ispec]]$SAL)>0){ 
        Nlen <- SpeciesData[[Ispec]]$Nlen
        Nsurv <- length(SpeciesData[[Ispec]]$SAL[,1])
        Out2$ObsSAL[Ispec,1:Nsurv,1:Nlen] <- SpeciesData[[Ispec]]$SAL
        Out2$YSAL[Ispec,1:Nsurv] <- SpeciesData[[Ispec]]$YSAL
        Out2$SSAL[Ispec,1:Nsurv] <- SpeciesData[[Ispec]]$SSAL
      }  
    }
    Out2$PredSAL <- OutLikelihood$PredSurvL
    
    Out2$ObsCAA <- array(0,dim=c(Nspecies,100,AmaxA))
    for (Ispec in 1:Nspecies){
      if (length(SpeciesData[[Ispec]]$CAA)>0){ 
        Amax <- SpeciesData[[Ispec]]$Amax
        NyearCAA <- length(SpeciesData[[Ispec]]$CAA[,1])
        Out2$ObsCAA[Ispec,1:NyearCAA,1:Amax] <- SpeciesData[[Ispec]]$CAA
      }
    }
    Out2$PredCAA <- OutLikelihood$PredCAA
    
    Out2$ObsCAL <- array(0,dim=c(Nspecies,100,NlenA))
    for (Ispec in 1:Nspecies){
      if (length(SpeciesData[[Ispec]]$CAL)>0){ 
        Nlen <- SpeciesData[[Ispec]]$Nlen
        NyearCAL <- length(SpeciesData[[Ispec]]$CAL[,1])
        Out2$ObsCAL[Ispec,1:NyearCAL,1:Nlen] <- SpeciesData[[Ispec]]$CAL
      }
    }
    Out2$PredCAL <- OutLikelihood$PredCAL
    
    Out2$Selex <- Selex
    Out2$SurvSel <- SurvSel
    
    Out2$ObsSurvBio <- array(0,dim=c(Nspecies,10,Nyear))
    for (Ispec in 1:Nspecies)
      Out2$ObsSurvBio[Ispec,,] <-SpeciesData[[Ispec]]$SurvBio/1000
    Out2$PredSurvBio <- OutLikelihood$PredSurvBio/1000
    
    Out2$ObsCatch <- matrix(0,nrow=Nspecies,ncol=Nyear)
    for (Ispec in 1:Nspecies) Out2$ObsCatch[Ispec,] <- SpeciesData[[Ispec]]$Catch
    Out2$PredCatch <- Outs$PredCW
    
    return(Out2)
  } 
}  


Like <- function(Nspecies,SpeciesData,Outs,SurvSel,RecDev,InitN){
  
  # Extract data needed for likelihood calculation
  AmaxA <- Outs$AmaxA
  NlenA <- Outs$NlenA
  N <- Outs$N
  PCAA <- Outs$CAA
  PredCW <- Outs$PredCW
  
  # Declare model predictions
  PredSurvA <- array(0,dim=c(Nspecies,100,AmaxA))
  PredSurvL <- array(0,dim=c(Nspecies,100,NlenA))
  PredCAA <- array(0,dim=c(Nspecies,100,AmaxA))
  PredCAL <- array(0,dim=c(Nspecies,100,NlenA))
  PredSurvBio <- array(0,dim=c(Nspecies,10,100))
  SurveyQ <- matrix(0,nrow=Nspecies,ncol=10)
  
  # Pre-specified values
  CVIndex <- 0.3
  CVCatch <- 0.1
  SigmaR <- 0.6
  EffFish <- 100
  
  Like1 <- rep(0,Nspecies)
  Like2 <- rep(0,Nspecies)
  Like3 <- rep(0,Nspecies)
  Prob1 <- rep(0,Nspecies)
  
  for (Ispec in 1:Nspecies){
    Amax <- SpeciesData[[Ispec]]$Amax
    Nlen <- SpeciesData[[Ispec]]$Nlen
    YrA <- SpeciesData[[Ispec]]$Yr1
    
    # Survey (assumed to be absolute, but subject to selectivity)
    if (SpeciesData[[Ispec]]$NSAA > 0){
      for (Jsurv in 1:SpeciesData[[Ispec]]$Nsurvey){  
        Q1 <- 0
        Q2 <- 0
        for (II in 1:SpeciesData[[Ispec]]$NSAA){
          if (SpeciesData[[Ispec]]$SSAA[II]==Jsurv){
            # Convert from real years to model years  
            Ipnt <- SpeciesData[[Ispec]]$YSAA[II]-YrA+1
            Isurv <- SpeciesData[[Ispec]]$SSAA[II]
            for (Iage in 1:Amax){ 
              PredSurvA[Ispec,II,Iage] <- SurvSel[Ispec,Isurv,Iage]*N[Ispec,Ipnt,Iage]
              if (SpeciesData[[Ispec]]$SAA[II,Iage] > 0){ 
                Q1 <- Q1 + log(SpeciesData[[Ispec]]$SAA[II,Iage]/PredSurvA[Ispec,II,Iage])
                Q2 <- Q2 + 1
              }
            }
          }
        }
        SurveyQ[Ispec,Jsurv] <- exp(Q1/Q2)
        #cat(Jsurv,SurveyQ[Ispec,Jsurv],Q2,"\n")
        for (II in 1:SpeciesData[[Ispec]]$NSAA){
          if (SpeciesData[[Ispec]]$SSAA[II]==Jsurv){
            # Convert from real years to model years  
            Ipnt <- SpeciesData[[Ispec]]$YSAA[II]-YrA+1
            Isurv <- SpeciesData[[Ispec]]$SSAA[II]
            for (Iage in 1:Amax){ 
              PredSurvA[Ispec,II,Iage] <- SurveyQ[Ispec,Jsurv]*PredSurvA[Ispec,II,Iage]
              PredSurvBio[Ispec,Isurv,Ipnt] <- PredSurvBio[Ispec,Isurv,Ipnt] + PredSurvA[Ispec,II,Iage]*SpeciesData[[Ispec]]$WeightS[Iage]
              if (SpeciesData[[Ispec]]$SAA[II,Iage] > 0){ 
                Residual <- log(PredSurvA[Ispec,II,Iage]) - log(SpeciesData[[Ispec]]$SAA[II,Iage])
                Like1[Ispec] <- Like1[Ispec] + Residual^2/(2*CVIndex^2)
              }
            }
          }
        }
      }
    }
    
    # Survey catch-at-length
    if (SpeciesData[[Ispec]]$NSAL > 0){
      for (Jsurv in 1:SpeciesData[[Ispec]]$Nsurvey){  
        # Compute ML estimate of survey Q
        Q1 <- 0;Q2 <- 0
        for (II in 1:SpeciesData[[Ispec]]$NSAL){
          if (SpeciesData[[Ispec]]$SSAL[II]==Jsurv){
            # Convert from real years to model years  
            Ipnt <- SpeciesData[[Ispec]]$YSAL[II]-YrA+1
            Isurv <- SpeciesData[[Ispec]]$SSAL[II]
            for (Ilen in 1:Nlen){ 
              PredSurvL[Ispec,II,Ilen] <- sum(SpeciesData[[Ispec]]$ALK2[1:Amax,Ilen]*SurvSel[Ispec,Isurv,1:Amax]*N[Ispec,Ipnt,1:Amax])
              if (SpeciesData[[Ispec]]$SAL[II,Ilen] > 0){ 
                Q1 <- Q1 + log(SpeciesData[[Ispec]]$SAL[II,Ilen]/PredSurvL[Ispec,II,Ilen])
                Q2 <- Q2 + 1
              }
            }
          }
        }
        SurveyQ[Ispec,Jsurv] <- exp(Q1/Q2)
        #cat(Jsurv,SurveyQ[Ispec,Jsurv],Q2,"\n")
        for (II in 1:SpeciesData[[Ispec]]$NSAL){
          if (SpeciesData[[Ispec]]$SSAL[II]==Jsurv){
            # Convert from real years to model years  
            Ipnt <- SpeciesData[[Ispec]]$YSAL[II]-YrA+1
            Isurv <- SpeciesData[[Ispec]]$SSAL[II]
            for (Ilen in 1:Nlen){ 
              PredSurvL[Ispec,II,Ilen] <- SurveyQ[Ispec,Jsurv]*PredSurvL[Ispec,II,Ilen]
              PredSurvBio[Ispec,Isurv,Ipnt] <- PredSurvBio[Ispec,Isurv,Ipnt] + PredSurvL[Ispec,II,Ilen]*SpeciesData[[Ispec]]$WeightLen[Ilen]
              if (SpeciesData[[Ispec]]$SAL[II,Ilen] > 0){ 
                Residual <- log(PredSurvL[Ispec,II,Ilen]) - log(SpeciesData[[Ispec]]$SAL[II,Ilen])
                #cat(SpeciesData[[Ispec]]$YSAL[II],Isurv,Ilen,SpeciesData[[Ispec]]$SAL[II,Ilen],PredSurvL[Ispec,II,Ilen],"\n")
                Like1[Ispec] <- Like1[Ispec] + Residual^2/(2*CVIndex^2)
              }
            }
          }
        }
      }
    } 
    
    # Catch-at-age and -at-length
    if (SpeciesData[[Ispec]]$NCAA > 1){
      for (II in 1:SpeciesData[[Ispec]]$NCAA){
        # Convert from real years to model years  
        Ipnt <- SpeciesData[[Ispec]]$YCAA[II]-YrA+1
        PredCAA[Ispec,II,1:Amax] <- PCAA[Ispec,Ipnt,1:Amax]/(sum(PCAA[Ispec,Ipnt,1:Amax]))
        for (Iage in 1:Amax){
          if (SpeciesData[[Ispec]]$CAA[II,Iage] > 0){ 
            Residual <- SpeciesData[[Ispec]]$CAA[II,Iage]*log(PredCAA[Ispec,II,Iage]/SpeciesData[[Ispec]]$CAA[II,Iage])
            Like2[Ispec] <- Like2[Ispec] - EffFish*Residual
          }
        }
      }
    }
    if (SpeciesData[[Ispec]]$NCAL > 1){
      for (II in 1:SpeciesData[[Ispec]]$NCAL){
        # Convert from real years to model years  
        Ipnt <- SpeciesData[[Ispec]]$YCAL[II]-YrA+1
        for (Ilen in 1:Nlen)
          PredCAL[Ispec,II,Ilen] <- sum(SpeciesData[[Ispec]]$ALK2[,Ilen]*PCAA[Ispec,Ipnt,1:Amax])
        PredCAL[Ispec,II,] <- PredCAL[Ispec,II,]/(sum(PredCAL[Ispec,II,]))
        for (Ilen in 1:Nlen){
          if (SpeciesData[[Ispec]]$CAL[II,Ilen] > 0){ 
            Residual <- SpeciesData[[Ispec]]$CAL[II,Ilen]*log(PredCAL[Ispec,II,Ilen]/SpeciesData[[Ispec]]$CAL[II,Ilen])
            Like2[Ispec] <- Like2[Ispec] - EffFish*Residual
          } 
        }
      } 
    }
    # Total catch (applies to all years)
    for (II in 1:SpeciesData[[Ispec]]$Nyear){
      Residual <- log(PredCW[Ispec,II])-log(SpeciesData[[Ispec]]$Catch[II])
      Like3[Ispec] <- Like3[Ispec] + Residual^2/(2*CVCatch^2)
    }  
    
    # Penalty on rec_devs
    for (Iyear in 1:SpeciesData[[Ispec]]$Nyear) Prob1[Ispec] <- Prob1[Ispec] + RecDev[Ispec,Iyear]^2.0/(2.0*SigmaR^2)   
    for (Iyear in 1:SpeciesData[[Ispec]]$Amax) Prob1[Ispec] <- Prob1[Ispec] + InitN[Ispec,Iyear]^2.0/(2.0*SigmaR^2)   
  }  
  
  TotalLike <- sum(Like1)+sum(Like2)+sum(Like3)+sum(Prob1)
  #cat(Like1,Like2,Like3,Prob1,TotalLike,"\n")
  #if (TotalLike < 9800) AAA
  #AAAA
  
  Outs <- NULL
  Outs$AmaxA <- AmaxA
  Outs$NlenA <- NlenA
  Outs$PredSurvA <- PredSurvA
  Outs$PredSurvL <- PredSurvL
  Outs$PredCAA <- PredCAA
  Outs$PredCAL <- PredCAL
  Outs$TotalLike <- TotalLike
  Outs$PredSurvBio <- PredSurvBio
  Outs$SigmaR <- SigmaR
  Outs$CvCatch <- CVCatch
  Outs$CvIndex <- CVIndex
  Outs$EffFish <- EffFish
  return(Outs)
}


PopModel <- function(Nspecies,SpeciesData,InitN,RecDev,LogR0,Fvals,Selex,InitF,PredationPars,Nproj){
  
  Nyear <- SpeciesData[[1]]$Nyear
  Amax <- rep(0,Nspecies)
  for (Ispec in 1:Nspecies) Amax[Ispec] <- SpeciesData[[Ispec]]$Amax
  MaxA <- max(Amax)
  Nlen <- 0
  for (Ispec in 1:Nspecies) Nlen[Ispec] <- SpeciesData[[Ispec]]$Nlen
  NlenA <- max(Nlen)
  
  M <- matrix(0,nrow=Nspecies,ncol=MaxA)
  WeightS <- matrix(0,nrow=Nspecies,ncol=MaxA)
  WeightH <- matrix(0,nrow=Nspecies,ncol=MaxA)
  Mat <- matrix(0,nrow=Nspecies,ncol=MaxA)
  PropZBeforeMat <- rep(0,nrow=Nspecies)
  for (Ispec in 1:Nspecies){  
    AmaxA <- Amax[Ispec]
    M[Ispec,1:AmaxA] <- SpeciesData[[Ispec]]$M[1:AmaxA]
    WeightS[Ispec,1:AmaxA] <- SpeciesData[[Ispec]]$WeightS[1:AmaxA]
    WeightH[Ispec,1:AmaxA] <- SpeciesData[[Ispec]]$WeightH[1:AmaxA]
    Mat[Ispec,1:AmaxA] <- SpeciesData[[Ispec]]$Mat[1:AmaxA]
    PropZBeforeMat[Ispec] <- SpeciesData[[Ispec]]$PropZBeforeMat 
  }
  
  # Declare 
  N <- array(0,dim=c(Nspecies,Nyear+Nproj+1,MaxA))
  CAA <- array(0,dim=c(Nspecies,Nyear+Nproj,MaxA))
  FAA <- array(0,dim=c(Nspecies,Nyear+Nproj,MaxA))
  PredCW <- matrix(0,nrow=Nspecies,ncol=Nyear+Nproj)
  SSB <- matrix(0,nrow=Nspecies,ncol=Nyear+Nproj)
  Ntemp <- matrix(0,nrow=Nspecies,ncol=MaxA)
  Z <- matrix(0,nrow=Nspecies,ncol=MaxA)
  R0 <- exp(LogR0)
  # Predicted B0 by stock
  PredB0 <- rep(0,Nspecies)
  # Predicted biomass by stock
  PredB <- matrix(0,nrow=Nspecies,ncol=Nyear+Nproj)
  
  # Compute B0 by stock
  Neqn <- matrix(0,nrow=Nspecies,ncol=MaxA)
  for (Ispec in 1:Nspecies){  
    AmaxA <- Amax[Ispec]
    Neqn[Ispec,1] <- R0[Ispec]
    for (Iage in 2:AmaxA) Neqn[Ispec,Iage] <- Neqn[Ispec,Iage-1]*exp(-M[Ispec,Iage-1])
    Neqn[Ispec,AmaxA] <- Neqn[Ispec,AmaxA]/(1.0-exp(-M[Ispec,AmaxA]))
    for (Iage in 1:AmaxA) PredB0[Ispec]  <- sum(Neqn[Ispec,1:AmaxA]*WeightS[Ispec,1:AmaxA])
  }  
  
  # Predation parameters
  # This is hake-Chi
  HakeChi <- PredationPars$HakeSurv
  # This is anchovy and sardine omega
  OmegaMAnch <- PredationPars$PropMAnch
  OmegaMSard <- PredationPars$PropMSard
  # This is anchovy and sardine chi
  ChiAnch <- PredationPars$MdropAnch
  ChiSard <- PredationPars$MdropSard
  
  # transform the predation parameters
  HakeBeta <- (1-HakeChi)/(2*HakeChi-1)
  HakeAlpha <- HakeBeta+1
  AnchBeta <- (2*ChiAnch-1.0)/(1-ChiAnch)
  AnchAlpha <- AnchBeta+1
  SardBeta <- (2*ChiSard-1.0)/(1-ChiSard)
  SardAlpha <- SardBeta+1
  #cat(HakeAlpha,HakeBeta,AnchAlpha,AnchBeta,SardAlpha,SardBeta,"\n")
  
  # set up the N matrix
  for (Ispec in 1:Nspecies){  
    N[Ispec,1,1] <- R0[Ispec]
    for (Iage in 2:Amax[Ispec]) N[Ispec,1,Iage] <- N[Ispec,1,Iage-1]*exp(-M[Ispec,Iage-1])*exp(-InitF[Ispec])
    for (Iage in 1:Amax[Ispec]) N[Ispec,1,Iage] <- N[Ispec,1,Iage]*exp(InitN[Ispec,Iage])
  }  
  
  # Project forward 
  WithPred <- F
  for (Iyear in 1:(Nyear+Nproj)){
    # Compute pred biomass
    for (Ispec in 1:Nspecies){  
      AmaxA <- Amax[Ispec]
      for (Iage in 1:AmaxA) PredB[Ispec,Iyear]  <- sum(N[Ispec,Iyear,1:AmaxA]*WeightS[Ispec,1:AmaxA])
    }  
    
    # Somewhat hard-wired
    MTwo <- matrix(0,nrow=Nspecies,ncol=MaxA)
    if (WithPred==T){  
      # Too little prey reduces predator survival
      PreyHake <- (PredB[2,Iyear]+PredB[3,Iyear])/(PredB0[2]+PredB0[3])
      MTwo[1,] = -1*log(HakeAlpha*PreyHake/(HakeBeta+PreyHake))
      
      # Calculate the depletions      
      DeplHake <- PredB[1,Iyear]/PredB0[1]
      DeplAnch <- PredB[2,Iyear]/PredB0[2]
      DeplSard <- PredB[3,Iyear]/PredB0[3]
      
      # too many hake increases prey M (but under type 2)
      Func1 <- AnchAlpha*DeplHake/(AnchBeta+DeplAnch)
      for (Iage in 1:Amax[1])
        MTwo[2,Iage]  <- OmegaMAnch*M[2,Iage]*(Func1-1)
      Func1 <- SardAlpha*DeplHake/(SardBeta+DeplSard)
      for (Iage in 1:Amax[1])
        MTwo[3,Iage]  <- OmegaMSard*M[3,Iage]*(Func1-1)
    }
    
    # Compute F, Z, catch-at-age, and SSB; note that I have added the extra mortality
    for (Ispec in 1:Nspecies){
      for (Iage in 1:Amax[Ispec]){
        FAA[Ispec,Iyear,Iage] <- Selex[Ispec,Iage]*Fvals[Ispec,Iyear]
        Z[Ispec,Iage] <- M[Ispec,Iage]+FAA[Ispec,Iyear,Iage]+MTwo[Ispec,Iage]
      }
      for (Iage in 1:Amax[Ispec]) CAA[Ispec,Iyear,Iage] <- FAA[Ispec,Iyear,Iage]/Z[Ispec,Iage]*N[Ispec,Iyear,Iage]*(1.0-exp(-Z[Ispec,Iage]))
      
      PredCW[Ispec,Iyear] <- sum(WeightH[Ispec,]*CAA[Ispec,Iyear,])
      # Predict SSB during the year and remove Z
      SSB[Ispec,Iyear] <- sum(WeightS[Ispec,]*Mat[Ispec,]*exp(-PropZBeforeMat[Ispec]*Z[Ispec,])*N[Ispec,Iyear,])
    }  
    
    # update N
    for (Ispec in 1:Nspecies){
      AmaxA <- Amax[Ispec]
      Ntemp[Ispec,1:AmaxA] <- N[Ispec,Iyear,1:AmaxA]*exp(-Z[Ispec,1:AmaxA])
    }  
    
    # Update dynamics and add recruitment     
    for (Ispec in 1:Nspecies){  
      AmaxA <- Amax[Ispec]
      N[Ispec,Iyear+1,AmaxA] <- Ntemp[Ispec,AmaxA]+Ntemp[Ispec,AmaxA-1]
      for (Iage in 2:(AmaxA-1)) N[Ispec,Iyear+1,Iage] <- Ntemp[Ispec,Iage-1]
      N[Ispec,Iyear+1,1] <- R0[Ispec]*exp(RecDev[Ispec,Iyear])
    }  
  }  
  
  Outs <- NULL
  Outs$AmaxA <- AmaxA
  Outs$Amax <- Amax
  Outs$NlenA <- NlenA
  Outs$Nlen <- Nlen
  Outs$WeightS <- WeightS
  Outs$WeightH <- WeightH
  Outs$Mat <- Mat
  Outs$M <- M
  Outs$PropZBeforeMat <- PropZBeforeMat
  Outs$N <- N
  Outs$SSB <- SSB
  Outs$CAA <- CAA
  Outs$FAA <- FAA
  Outs$PredCW <- PredCW
  return(Outs)
}



Extract <- function(Pars,ParInt,SpeciesData,Nspecies,Phases,Nproj=0){
  
  AmaxP <- rep(0,Nspecies)
  
  InitN <- matrix(NA,nrow=Nspecies,ncol=100)
  RecDev <- matrix(0,nrow=Nspecies,ncol=Nyear+Nproj)
  LogR0 <- rep(0,Nspecies)
  InitF <- rep(0,Nspecies)
  SurvSel <- array(NA,dim=c(Nspecies,10,100))
  Selex <- matrix(0,nrow=Nspecies,ncol=100)
  Fvals <- matrix(0,nrow=Nspecies,ncol=Nyear+Nproj)
  Ipar <- 0
  Jpar <- 0
  
  Prior <- 0
  ParOut <- NULL
  ParInitN <- NULL
  ParRecDev <- NULL
  ParLogR0 <- NULL
  ParVecS <- NULL
  ParVecC <- NULL
  ParFvals <- NULL
  ParInitF <- NULL
  for (Ispec in 1:Nspecies){  
    Amax <- SpeciesData[[Ispec]]$Amax
    Nlen <- SpeciesData[[Ispec]]$Nlen
    Nyear <- SpeciesData[[Ispec]]$Nyear
    Nsurvey <- SpeciesData[[Ispec]]$Nsurvey
    VecS <- NULL    
    VecC <- NULL
    
    # Extract the parameters from "Pars"
    if (Phases[1]==T){
      InitN[Ispec,1:Amax] <- Pars[(Ipar+1):(Ipar+Amax)]
      Ipar <- Ipar + Amax 
    }else{
      InitN[Ispec,1:Amax] <- ParsInit[(Jpar+1):(Jpar+Amax)]
      Jpar <- Jpar + Amax 
    }
    
    if (Phases[2]==T){
      RecDev[Ispec,1:Nyear] <- Pars[(Ipar+1):(Nyear+Ipar)]
      Ipar <- Ipar + Nyear 
    }else{
      RecDev[Ispec,1:Nyear] <- ParsInit[(Jpar+1):(Nyear+Jpar)]
      Jpar <- Jpar + Nyear
    }
    if (Phases[3]==T){
      LogR0[Ispec] <- Pars[Ipar+1]
      Ipar <- Ipar + 1 
    }else{
      LogR0[Ispec] <- ParsInit[Jpar+1]
      Jpar <- Jpar + 1
    }
    
    if (SpeciesData[[Ispec]]$SelsurvType==1){  
      if (Phases[4]==T){
        SVec <- Pars[(Ipar+1):(Ipar+Nsurvey*(Amax-2))]
        Ipar <- Ipar + Nsurvey*(Amax-2) 
      }else{
        SVec <- ParsInit[(Jpar+1):(Jpar+Nsurvey*(Amax-2))]
        Jpar <- Jpar + Nsurvey*(Amax-2)
      }
      VecS <- c(VecS,SVec)
      for (Isurv in 1:Nsurvey){ 
        Offset1 <- (Isurv-1)*(Amax-2)+1; Offset2 <- Isurv*(Amax-2)
        SurvSel[Ispec,Isurv,1:Amax] <- c(1/(1+exp(SVec[Offset1:Offset2])),1,1)
      } 
    } 
    if (SpeciesData[[Ispec]]$SelsurvType==2){  
      if (Phases[4]==T){
        SVec <- Pars[(Ipar+1):(Ipar+Nsurvey*3)]
        Ipar <- Ipar + Nsurvey*3 
      }else{
        SVec <- ParsInit[(Jpar+1):(Jpar+Nsurvey*3)]
        Jpar <- Jpar + Nsurvey*3
      }
      VecS <- c(VecS,SVec)
      for (Isurv in 1:Nsurvey){ 
        Offset <- (Isurv-1)*3
        ModalAge <- exp(SVec[Offset+1]);
        Sig1 <-exp(SVec[Offset+2]);                      
        Sig2 <-exp(SVec[Offset+3]);                      
        Prior <- Prior + 0.01*Sig1*Sig1 + 0.01*Sig2*Sig2
        if (SVec[1] < -10) Prior <- Prior + 0.01*(10+SVec[1])^2
        for (Iage in 0:(Amax-1)){ 
          if (Iage < ModalAge){
            SurvSel[Ispec,Isurv,Iage+1] <- exp(-(Iage-ModalAge)^2/Sig1)
          }else{
            SurvSel[Ispec,Isurv,Iage+1] <- exp(-(Iage-ModalAge)^2/Sig2)
          }
        }  
        SurvSel[Ispec,Isurv,1:Amax] <- SurvSel[Ispec,Isurv,1:Amax] / max(SurvSel[Ispec,Isurv,1:Amax])
      }  
    } 
    if (SpeciesData[[Ispec]]$SelexType==1){  
      if (Phases[5]==T){ 
        SVec <- Pars[(Ipar+1):(Ipar+Amax-2)]
        Ipar <- Ipar + Amax-2 
      }else{
        SVec <- ParsInit[(Jpar+1):(Jpar+Amax-2)]
        Jpar <- Jpar + Amax-2
      }
      VecC <- c(VecC,SVec)
      Selex[Ispec,1:Amax] <- c(1/(1+exp(SVec)),1,1)
    } 
    if (SpeciesData[[Ispec]]$SelexType==2){  
      if (Phases[5]==T){
        SVec <- Pars[(Ipar+1):(Ipar+3)]
        Ipar <- Ipar +3 
      }else{ 
        SVec <- ParsInit[(Jpar+1):(Jpar+3)]
        Jpar <- Jpar +3 
      }
      VecC <- c(VecC,SVec)
      ModalAge <- exp(SVec[1]);
      Sig1 <-exp(SVec[2]);                      
      Sig2 <-exp(SVec[3]);                      
      Prior <- Prior + 0.01*Sig1*Sig1 + 0.01*Sig2*Sig2
      if (SVec[1] < -10) Prior <- Prior + 0.01*(10+SVec[1])^2
      for (Iage in 0:(Amax-1)){
        if (Iage < ModalAge){
          Selex[Ispec,Iage+1] <- exp(-(Iage-ModalAge)^2/Sig1)
        }else{
          Selex[Ispec,Iage+1] <- exp(-(Iage-ModalAge)^2/Sig2)
        }
      } 
      Selex[Ispec,1:Amax] <- Selex[Ispec,1:Amax] / max(Selex[Ispec,1:Amax])
    } 
    
    if (Phases[6]==T){
      Fvals[Ispec,1:Nyear] <- exp(Pars[(Ipar+1):(Ipar+Nyear)])
      Ipar <- Ipar + Nyear 
    }else{
      Fvals[Ispec,1:Nyear] <- exp(ParsInit[(Jpar+1):(Jpar+Nyear)])
      Jpar <- Jpar + Nyear
    }
    
    if (Phases[7]==T){
      InitF[Ispec] <- exp(Pars[Ipar+1]); Ipar <- Ipar + 1 
    }else{
      InitF[Ispec] <- exp(ParsInit[Jpar+1]); Jpar <- Jpar + 1 
    }
    ParOut <- c(ParOut,InitN[Ispec,1:Amax],RecDev[Ispec,1:Nyear],LogR0[Ispec],VecS,VecC,log(Fvals[Ispec,1:Nyear]),log(InitF[Ispec]))
    ParInitN <- c(ParInitN,InitN[Ispec,1:Amax])
    ParRecDev <- c(ParRecDev,RecDev[Ispec,1:Nyear])
    ParLogR0 <- c(ParLogR0,LogR0[Ispec])
    ParVecS <- c(ParVecS,VecS)
    ParVecC <- c(ParVecC,VecC)
    ParFvals <- c(ParFvals,log(Fvals[Ispec,1:Nyear]))
    ParInitF <- c(ParInitF,log(InitF[Ispec]))
  }
  
  Outs <- NULL
  Outs$ParOut <- ParOut
  Outs$AltPIN <- list(Dummy=0,ParlogR0=ParLogR0,ParVecC=ParVecC,ParVecS=ParVecS,ParInitN=ParInitN,ParRecDev=ParRecDev,ParFvals=ParFvals,ParInitF=ParInitF)
  Outs$InitN <- InitN
  Outs$RecDev <- RecDev
  Outs$LogR0 <- LogR0
  Outs$SurvSel <- SurvSel
  Outs$Selex <- Selex
  Outs$Fvals <- Fvals
  Outs$InitF <- InitF
  Outs$Prior <- Prior
  Outs$Nyear <- Nyear
  return(Outs)
}


DoFit <- function(Pars,TheFunk,FullMin=F,DoVarCo=F,...)
{
  # First call - always do this
  Res <- optim(Pars,TheFunk,hessian=F,control=list(maxit=10),DoEst=T,...)
  print(Res$value)
  #Res <- optim(Res$par,TheFunk,method = "BFGS",hessian=F,DoEst=T,...)
  #print(Res$value)
  Npar <- length(Res$par)
  cat("number of parameters =",Npar,"\n")
  print(Res)
  SSBEst <- TheFunk2(Res$par,DoEst=F,...)$SSB
  Nyear <- length(SSBEst)
  Res$VarCo <- matrix(0,ncol=Npar,nrow=Npar)
  Res$SSBSD <- rep(0,Nyear)
  
  # Hints: Set FullMin=T to apply the full estimates; DoVarCo=T to estimate the variances of the parameters and SSB
  Outputs <- TheFunk2(Res$par,DoEst=F,...)
  Outputs$par <- Res$par
  Outputs$VarCo <- Res$VarCo
  Outputs$SSBSD <- rep(0,Nyear)
  
  # print results to a file
  # print.out(Dirn,"Test.out",Outputs)
  
  # Now do a full minimization
  if (FullMin==T)
  {
    print("Doing full minimization")
    Res <- optim(Res$par,TheFunk,method = "BFGS",hessian=F,DoEst=T,...)
    print(Res$value)
    Best <- 10000
    while (abs(Res$value-Best)>0.01)
    {
      Best <- Res$value  
      Res <- optim(Res$par,TheFunk,hessian=F,method = "BFGS",DoEst=T,...)
      cat(Res$value,"BFGS","\n")
      Best <- Res$value  
      Res <- optim(Res$par,TheFunk,hessian=F,method = "CG",DoEst=T,...)
      cat(Res$value,"CG","\n")
    }  
    
    # print results to a test file
    print("Done convergence")
    Outputs <- TheFunk2(Res$par,DoEst=F,...)
    Outputs$par <- Res$par
    print(Res$par)
    Outputs$VarCo <- matrix(0,ncol=Npar,nrow=Npar)
    Outputs$SSBSD <- rep(0,Nyear)
    # print("Now printing")
    # print.out(Dirn,"Test2.out",Outputs)
    
    Res <- optim(Res$par,TheFunk,method = "BFGS",hessian=T,DoEst=T,...)
    Outputs <- TheFunk2(Res$par,DoEst=F,...)
    Outputs$par <- Res$par
    print(Res$par)
    Outputs$VarCo <- matrix(0,ncol=Npar,nrow=Npar)
    Outputs$SSBSD <- rep(0,Nyear)
    # print("Now printing")
    # print.out(Dirn,"Test3.out",Outputs)
    cat("Final Obj Function",Res$value,"\n")
    
    # This section computes the variance covariance matrix and hence the standard errors for SSB
    if (DoVarCo == T)
    {  
      print("Attempting to solve for the variance-covariance matrix")
      VarCo <- solve(Res$hessian)
      Res$VarCo <- VarCo
      
      SSBEst <- TheFunk2(Res$par,DoEst=F,...)$SSB
      Nyear <- length(SSBEst)
      print(SSBEst)
      
      # Set up the derivative matrix
      ParStore <- Res$par
      Deriv <- matrix(0,ncol=Nyear,nrow=Npar)
      for (II in 1:Npar)
      {
        # Numerical differentiation
        Res$par <- ParStore
        Res$par[II] <- ParStore[II]+0.001
        SSB1 <- TheFunk2(Res$par,DoEst=F,...)$SSB
        Res$par <- ParStore
        Res$par[II] <- ParStore[II]-0.001
        SSB2 <- TheFunk2(Res$par,DoEst=F,...)$SSB
        Deriv[II,] <- (SSB1-SSB2)/0.002
      }  
      
      # Use the delta method to get the variances for SSB    
      SSBSD <- rep(0,Nyear)
      for (Iyear in 1:Nyear)
      {
        for (II in 1:Npar)      
          for (JJ in 1:Npar)  
            SSBSD[Iyear] <- SSBSD[Iyear] + Deriv[II,Iyear]*Deriv[JJ,Iyear]*VarCo[II,JJ]
          SSBSD[Iyear] <- sqrt(SSBSD[Iyear])
      }
      Res$SSBSD <- SSBSD
    }  
    else
    {
      # Dummy matrix
      Res$VarCo <- matrix(0,ncol=Npar,nrow=Npar)
    }
  }
  print("Completed minimization")
  return(Res)
}  

