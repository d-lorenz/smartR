
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

###############

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
  EFG = sample(1:length(effoPatt), ceiling(npp_star), prob = pj, replace = TRUE)
  Ename = as.numeric(names(table(EFG)))
  Estar = numeric(length(effoPatt))
  Estar[Ename] = as.numeric(table(EFG))
  return(Estar)
}

genFlatEffoDen = function(effoPatt, targetDensity){
  npp_star = sum(effoPatt)
  pj = (effoPatt/npp_star)*(1/targetDensity)
  pj = pj/sum(pj,na.rm=T)
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
  pj = REstar/sum(REstar)
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
  pj = pj/sum(pj)
  Ecell = sample(1:length(effoPatt), ceiling(npp_star), prob = pj, replace = TRUE)
  Ename = as.numeric(names(table(Ecell)))
  Estar = numeric(length(effoPatt))
  Estar[Ename] = as.numeric(table(Ecell))
  return(Estar)
}

getPropWeig = function(tarWei, lwClass, nameFG, sizeClass, classPrice){
  if(tarWei == 0){
    outData <- data.frame(0)
    names(outData) <- paste0("RV_", ifelse(nchar(nameFG) == 2, nameFG, paste0("0", nameFG)))
  }else if(is.null(lwClass[[1]])){
    outData <- data.frame(NA)
    names(outData) <- paste0("RV_", ifelse(nchar(nameFG) == 2, nameFG, paste0("0", nameFG)))
  }else{
    # tmp_Revenue <- data.frame(avgLen = lwClass[[1]]$avgLen, propWei = tarWei*lwClass[[1]]$absAbb)
    # tmp_Revenue$SizeClass <- factor(findInterval(x = tmp_Revenue$avgLen, vec = sizeClass), levels = 2:length(sizeClass))
    # tmpWei <- merge(data.frame(SizeClass = levels(tmp_Revenue$SizeClass)), aggregate(formula = propWei ~ SizeClass, data = tmp_Revenue, FUN = sum), all.x = TRUE)
    outData <- data.frame(sum(lwClass[[1]]$propWei*classPrice*tarWei, na.rm = TRUE))
    names(outData) <- paste0("RV_", ifelse(nchar(nameFG) == 2, nameFG, paste0("0", nameFG)))
  }
  return(outData)
}

getPropWeiRow = function(prodRow, inLWclas, inNames, vecClass, vecPrice){
  tmp_out <- lapply(seq_along(prodRow), function(i, y) {getPropWeig(tarWei = prodRow[i],
                                                                    lwClass = inLWclas[substr(names(inLWclas), 4, nchar(names(inLWclas))) == i],
                                                                    nameFG = i,
                                                                    sizeClass = vecClass,
                                                                    classPrice = vecPrice)}, y = inNames)
  do.call(cbind, tmp_out)
}

getFleetRevenue = function(predProd, lwStat, priceVec){
  outProp <- apply(predProd, 1, function(x) apply(t(lwStat*t(x))*priceVec,2, sum))
  outProp <- do.call(rbind, outProp)
  return(outProp)
}