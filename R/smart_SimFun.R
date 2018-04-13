
## Get Production ####
getProduction = function(effoPatt, numFG, thrZero, logitMod, nnlsMod){
  Prod <- matrix(data = NA, nrow(effoPatt), ncol = numFG)
  lyears <- sort(as.numeric(as.character(unique(effoPatt$Year))))
  fgClms <- which(colnames(effoPatt) %in% as.character(seq(1, numFG)))
  datalog <- effoPatt
  datalog$MonthNum <- as.factor(datalog$MonthNum)
  datalog$Year <- as.factor(datalog$Year)
  if(logitMod$Name == "GLM"){
    infish <- which(predict(logitMod$Model, datalog, type = "response") > logitMod$Cut)
  }else{
    infish <- which(predict(logitMod$Model, datalog, type = "prob")[,2] > logitMod$Cut)
  }
  for(i in 1:length(infish)){
    idata <- as.numeric(effoPatt[infish[i], fgClms])
    iloa <- as.numeric(effoPatt[infish[i], "Loa"])
    iy <- which(lyears == effoPatt[infish[i], "Year"])
    im <- as.numeric(as.character(effoPatt[infish[i], "MonthNum"]))
    ib <- nnlsMod$bmat[which((nnlsMod$SceMat$YEAR == iy) & (nnlsMod$SceMat$MONTH == im)),]
    if(sum(ib*idata)>0){
      Prod[infish[i],] <- (ib * idata * iloa) + ((ib*idata)/sum(ib*idata))*thrZero
    }
  }
  Prod[is.na(Prod)] <- 0
  colnames(Prod) <- paste("PR_", as.character(seq(1, ncol(Prod))), sep = "")
  return(Prod)
}

## Get Spatial Index ####
getSpatialIndex = function(effortPattern, fgWeight){
  tmp_ei <- apply(data.frame(mapply(`*`, effortPattern[, 4:(length(fgWeight)+3)], fgWeight)), 1, sum)
  spatialIndex <- data.frame(effortPattern[,c(1:3,ncol(effortPattern))], EffInd = tmp_ei)
  spatialIndex <- aggregate(EffInd ~ I_NCEE + Year + Loa, spatialIndex, sum)
  return(spatialIndex)
}

## Get Spatial Cost ####
getSpatialCost = function(spatialIndex, spatialCostModel){
  predSpatCost = predict(spatialCostModel, spatialIndex)
  return(predSpatCost)
}

## Get Effort Index ####
getEffortIndex = function(daysAtSea){
  effortIndex <- aggregate(Freq ~ I_NCEE + Year + Loa + Kw, daysAtSea, sum)
  return(effortIndex)
}

## Get Effort Cost ####
getEffortCost = function(effortIndex, effortCostModel){
  predEffoCost <- predict(effortCostModel, effortIndex)
  return(predEffoCost)
}

## Get Production Index ####
getProductionIndex = function(productionPattern, numFG){
  tmp_Prod <- data.frame(Year = productionPattern$Year,
                         I_NCEE = productionPattern$I_NCEE,
                         MonthNum = productionPattern$MonthNum,
                         Production = apply(productionPattern[,(4+numFG):ncol(productionPattern)],1, sum))
  agg_ProdInd <- aggregate(Production ~ I_NCEE + Year, tmp_Prod, sum)
  return(agg_ProdInd)
}


## Get Production Cost ####
getProductionCost = function(productionIndex, productionCostModel){
  predProdCost = predict(productionCostModel, productionIndex)
  return(predProdCost)
}

## Get Total Cost ####
getTotalCost = function(spatialCost, effortCost, productionCost){
  tmp_out_costs <- merge(merge(effortCost, spatialCost), productionCost)
  out_costs <- tmp_out_costs[, c("I_NCEE", "Year", "Loa", "predEffoCost", "predSpatCost", "predProdCost")]
  out_costs$totCost <- out_costs$predEffoCost + out_costs$predSpatCost + out_costs$predProdCost
  return(out_costs)
}

## Get LWstat ####
getLWstat = function(mcmcOut){
  lwStat <- ddply(mcmcOut, .(rouWei, FG), summarise,
                  avgLen = mean(Length), sdLen = sd(Length), relAbb = length(Length))
  lwStat <- lwStat[-which(is.na(lwStat), arr.ind = TRUE)[,1],]
  return(lwStat)
}

## Get PreRevenue ####
getPreRevenue = function(lwStat, numFG){
  fgNames <- paste0("LW_", 1:numFG)
  preRevenue <- vector("list", length(fgNames))
  names(preRevenue) <- fgNames
  for(i in names(preRevenue)){
    preRevenue[[i]] <- lwStat[lwStat$FG == substr(i, 4, nchar(i)),]
    preRevenue[[i]]$absAbb <- preRevenue[[i]]$relAbb/sum(preRevenue[[i]]$relAbb)
  }
  return(preRevenue)
}

## Get Fleet Revenue ####
getPropWeig = function(tarWei, lwClass, nameFG, sizeClass, classPrice){
  if(tarWei == 0){
    outData <- data.frame(0)
    names(outData) <- paste0("RV_", ifelse(nchar(nameFG) == 2, nameFG, paste0("0", nameFG)))
  }else if(nrow(lwClass[[1]]) == 0){
    outData <- data.frame(NA)
    names(outData) <- paste0("RV_", ifelse(nchar(nameFG) == 2, nameFG, paste0("0", nameFG)))
  }else{
    tmp_Revenue <- data.frame(avgLen = lwClass[[1]]$avgLen, propWei = tarWei*lwClass[[1]]$absAbb)
    tmp_Revenue$SizeClass <- factor(findInterval(x = tmp_Revenue$avgLen, vec = sizeClass), levels = 2:length(sizeClass))
    tmpWei <- merge(data.frame(SizeClass = levels(tmp_Revenue$SizeClass)), aggregate(formula = propWei ~ SizeClass, data = tmp_Revenue, FUN = sum), all.x = TRUE)
    outData <- data.frame(sum(tmpWei$propWei*classPrice, na.rm = TRUE))
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

getFleetRevenue = function(predProd, lwStat, fgNames, classVec, priceVec){
  outProp <- apply(predProd, 1, function(x) getPropWeiRow(prodRow = x,
                                                          inLWclas = lwStat,
                                                          inNames = fgNames,
                                                          vecClass = classVec,
                                                          vecPrice = priceVec))
  outProp <- do.call(rbind, outProp)
  return(outProp)
}


## Get Cost Revenue Wrapper
getCostRevenue = function(effort_pattern, number_fg, threshold_zero, logit_model, nnls_model,
                          fg_weights, spatial_cost_model,
                          day_at_sea, effort_cost_model,
                          production_cost_model,
                          inMCMC, inPric){

  # CALL: Predict Production
  out_prod <- getProduction(effoPatt = effort_pattern,
                            numFG = number_fg,
                            thrZero = threshold_zero,
                            logitMod = logit_model,
                            nnlsMod = nnls_model)
  # CALL: Get Spatial Index
  out_spatInd <- getSpatialIndex(effortPattern = effort_pattern, fgWeight = fg_weights)
  # CALL: Get Spatial Cost
  out_spatial_cost <- getSpatialCost(spatialIndex = out_spatInd, spatialCostModel = spatial_cost_model)

  # CALL: Get Effort Index
  out_effoInd <- getEffortIndex(daysAtSea = day_at_sea)
  # CALL: Get Effort cost
  out_effort_cost <- getEffortCost(effortIndex = out_effoInd, effortCostModel = effort_cost_model)

  # CALL: Get Production Index
  tmp_newProd <- cbind(effort_pattern, out_prod)

  out_prod_ind <- getProductionIndex(productionPattern = tmp_newProd, numFG = number_fg)
  # CALL: Get Production Cost
  out_prod_cost <- getProductionCost(productionIndex = out_prod_ind, productionCostModel = production_cost_model)

  # CALL: Get Total Cost
  out_pred_spatial <- cbind(out_spatInd, predSpatCost = out_spatial_cost)
  out_pred_effort <- cbind(out_effoInd, predEffoCost = out_effort_cost)
  out_pred_prod <- cbind(out_prod_ind, predProdCost = out_prod_cost)

  out_cost <- getTotalCost(spatialCost = out_pred_spatial,
                           effortCost = out_pred_effort,
                           productionCost = out_pred_prod)

  # CALL: Get Length Weight Stat
  lenWeiStat <- getLWstat(mcmcOut = inMCMC)
  # CALL: Get Pre Revenue
  pre_Revenue <- getPreRevenue(lwStat = lenWeiStat, numFG = number_fg)

  # CALL: Get Fleet Revenue
  fgNamesProd <- substr(colnames(out_prod), 4, nchar(colnames(out_prod)))
  vecSize <- sort(unique(c(inPric$LowerBound, inPric$UpperBound)))

  outRevenue <- getFleetRevenue(predProd = out_prod,
                                lwStat = pre_Revenue,
                                fgNames = fgNamesProd,
                                classVec = vecSize,
                                priceVec = inPric$Price)

  outRevenue[is.na(outRevenue)] <- 0
  tmp_newRevenue <- cbind(effort_pattern, totRevenue = apply(outRevenue, 1, sum))
  agg_totRevenue <- aggregate(totRevenue ~ I_NCEE + Year, tmp_newRevenue, sum)
  tmp_out_CRdf <- merge(out_cost, agg_totRevenue)
  out_CRdf <- tmp_out_CRdf[, c("I_NCEE", "Year", "totCost", "totRevenue")]
  return(out_CRdf)
}
