#Creates stx1 as xts object containing variables as defined in v.com for current stock
#Requires res, ret, cmn, logadv.z (bin variable) to be previously computed for current stock

mk_vars_fun <- function(v.com, res, ret, cmnret, logadv.z, adjv, cmnpvars) {
  require(dplyr)
  tier1 <- 1:length(v.com)
  tier_v <- list(tier1)    #future requirement when variable creation has to be ordered
  
  stx1.data <- NULL #res[,substr(predict.ret,1,2)]
  oos1.data <- NULL
  for (tier_n in 1:length(tier_v)) {
    v_list <- tier_v[[tier_n]]
    for (v_n in v_list) {
      #print(paste(tier_n,v_n))
      vc <- v.com[[v_n]]
      #print(names(v.com)[v_n])
      if (all(vc$var != "vol")) {  # 'all' so that NULL values will evaluate to TRUE (NULL -> "ret")
        switch(vc$type,
               "res" = stx1.var1 <- res[,vc$retnam],
               "ret" = stx1.var1 <- ret[,vc$retnam],
               "cmn" = stx1.var1 <- cmnret[,vc$retnam],
               {print(paste(v_n,"Type not found"))
                 next}
        )
      }
      if (!is.null(vc$var)) {
        switch(vc$var,
               "ret" = {}, #no action, default
               "vol" = {
                 switch(vc$type,
                        "res" = {  #ratio of stx dolvol to cmn dolvol
                          vdf <- cbind(adjv[,"D"],cmnpvars[,"D"])
                          colnames(vdf) <- c("retD","cmnD")
                          stx1.var1 <- calc_volres(vdf)
                        },  
                        "ret" = {  #ratio of stx dolvol to stx adv
                          stx1.var1 <- calc_voladv(adjv[,"D"])  
                        },
                        "cmn" = {  #ratio of cmn dolvol to cmn adv
                          stx1.var1 <- calc_voladv(cmnpvars[,"D"])  
                        }
                 )
                 if (!is.null(vc$log)) stx1.var1 <- log(stx1.var1) #convert ratios to log
               },
               "vlt" = stx1.var1 <- calc_vlt(stx1.var1)
        )
      }
      if (!is.null(vc$cap)) {
        if (vc$cap > 0) stx1.var1 <- calc_cap(stx1.var1,vc$cap)
      }
      if (!is.null(vc$scale)) {
        switch(vc$scale,
               "zscore" = stx1.var1 <- calc_z(stx1.var1,ma=TRUE),
               "zscale" = stx1.var1 <- calc_z(stx1.var1,ma=FALSE),
               print("No scaling"))
      }
      if (!is.null(vc$bin_var)) {
        colnames(stx1.var1) <- names(v.com)[v_n]
        stx1.var1 <- calc_bin(stx1.var1,eval(as.name(vc$bin_var)),vc$b1,vc$b2)
      }
      if (!is.null(vc$decay)) {
        if (vc$decay > 0) stx1.var1 <- calc_decay(stx1.var1,d=vc$decay)
      }
      if (!is.null(vc$lag)) {
        if (vc$lag > 0) stx1.var1 <- calc_lag(stx1.var1,vc$lag)
      }
      if (ncol(stx1.var1) == 1) colnames(stx1.var1) <- names(v.com)[v_n]
      #if (verbose) print(colnames(stx1.var1))
      if (is.null(stx1.data)) {
        #stx1.data <- as.data.frame(index(stx1.var1))
        #stx1.data[,2] <- rep(nam, nrow(stx1.var1))
        #colnames(stx1.data) <- c("date","TICKER")
        stx1.data <- stx1.var1
      } else {
        stx1.data <- merge(stx1.data,stx1.var1)
      }
    }  
  }
  return(stx1.data)
}  

make_adj <- function(df,field,cnam,adjcnam) {
  adj_var <- df[,field] * df[,adjcnam] / df[,cnam]
  return(adj_var)
}

calc_adj <- function(df,nam) {
  adj_fields <- c(paste(nam,".Open",sep=""),paste(nam,".High",sep=""),paste(nam,".Low",sep=""))
  cnam <- paste(nam,".Close",sep="")
  adjcnam <- paste(nam,".Adjusted",sep="")
  adj_vars <- NULL
  for (i in adj_fields) {
    adj_var <- make_adj(df,i,cnam,adjcnam)
    adj_vars <- cbind(adj_vars,adj_var)
  }
  volnam <- paste(nam,".Volume",sep="")
  adj_vars <- cbind(adj_vars,df[,adjcnam],df[,volnam])
  colnames(adj_vars) <- c("O","H","L","C","V")
  #calc M
  adj_var <- sqrt(adj_vars[,"H"]*adj_vars[,"L"])
  adj_vars <- cbind(adj_vars,adj_var)
  colnames(adj_vars)[ncol(adj_vars)] <- "M"
  #calc D (dollars = M*V)
  adj_var <- adj_vars[,"V"]*adj_vars[,"M"]
  adj_vars <- cbind(adj_vars,adj_var)
  colnames(adj_vars)[ncol(adj_vars)] <- "D"
  return(adj_vars)
}

calc_lag <- function(df,n=1,rename=FALSE) {
  lag_vars <- stats::lag(df,n)
  if (rename) {
    if (n==1) {
      lag_str <- "Y"
    } 
    else {
      lag_str <- paste("Y",n,sep="")
    }
    colnames(lag_vars) <- paste(lag_str,colnames(lag_vars),sep="")
  }
  return(lag_vars)
}

calc_rets <- function(df) {
  ret_formulas = matrix(c(
    "CC","YC","C",
    "CO","YC","O",
    "OC","O","C",
    "MC","M","C",
    "LH","L","H"),
    nrow = 5,
    ncol = 3,
    byrow = TRUE)
  colnames(ret_formulas) <- c("name","sprice","eprice")
  ret_vars <- NULL
  for (i in 1:nrow(ret_formulas)) {
    ret_f <- ret_formulas[i,]
    ret_var <- log(df[,ret_f[3]]/df[,ret_f[2]])
    colnames(ret_var) <- ret_f[1]
    ret_vars <- cbind(ret_vars,ret_var)
  }
  ret_vars <- subset(ret_vars,!is.na(ret_vars[,substr(predict.ret,1,2)])) #remove observations where predict var is NA
  return(ret_vars)
}

calc_vol <- function(df,window=20) {
  df.adv <- xts(apply(df,2,runMean,n=window), index()) 
      df.vlt <- xts(apply(df,2,runSD,n=window), index(df))*sqrt(252)

}


calc_cap_pct <- function(df, low, high, lb = 0) {
  #only works for all data [lookback == 0], need to code longer lookbacks
  #print(paste(low,high))
  qs <- sapply(df,function(z) quantile(z,c(low,high),na.rm = TRUE))
  #print(qs)
  df.cap.val <- NULL
  for (i in 1:ncol(df)) {
    col.data <- as.xts(df[,i])
    col.data[col.data < qs[1,i]] <- qs[1,i]
    col.data[col.data > qs[2,i]] <- qs[2,i]
    df.cap.val <- cbind(df.cap.val,col.data)
  }
  return(df.cap.val)
}

calc_cap_val <- function(df, low, high) {
  df.cap.val <- df
  df.cap.val[df.cap.val < low] <- low
  df.cap.val[df.cap.val > high] <- high
  return(df.cap.val)
}


calc_cap <- function(df, cap = NA, pct = TRUE, low = NA, high = NA, lb = 0) {
  #check parameters
  if (is.na(cap) & (is.na(low) | is.na(high)) ) {
    print("Error: cap or low/high must be set")
    return(-1)
  }
  if (pct) {
    if (is.na(cap)) {
      if (low < -1 | low >= 1 | high <= -1 | high > 1) {
        print("Error: Low and High have to be pcts (otherwise set pct = FALSE)")
        return(-1)
      }
    }
    else {
      if (cap < 0 | cap > 1 ) {
        print ("Error: Cap has to be pcts (otherwise set pct = FALSE)")
        return(-1)
      }
    }
  } #pct
  #set low/high if needed  
  if (!is.na(cap) & pct) {
    low <- cap
    high <- 1-cap
  }  
  else {
    if (!is.na(cap) & !pct) {
    low <- -cap
    high <- cap
    }
  }
  if (pct) {
    df.cap <- calc_cap_pct(df,low,high,lb)
  }  
  else {
    df.cap <- calc_cap_val(df,low,high)
  }
  return(df.cap)  
}

calc_decay <- function(df,d,rename=FALSE) {
  require(forecast)
  df.decay <- NULL
  for (i in 1:ncol(df)) {
    #print(paste(i,colnames(df)[i]))
    col.data <- as.xts(df[,i])
    #replace NA with 0
    col.data[is.na(col.data)] <- 0
    col.ses <- ses(col.data,alpha=d)
    col.decay <- xts(fitted.values(col.ses),order.by = index(col.data))
    df.decay <- cbind(df.decay,col.decay)
  }
  if (rename) colnames(df.decay) <- paste(colnames(df),"d",100*d,sep="")
  else colnames(df.decay) <- colnames(df)

  return(df.decay)
}

calc_z <- function(df,ma = TRUE, window = 60, cap = 0) {
  #pct_cap on the st.dev. not yet supported

  #if required to not use future information (uses only historic information in window)
  #df.sd <-xts(apply(df,2,runSD,n=window), index(df))
  #if (ma) {
  #  df.mean <- xts(apply(df,2,runMean,n=window), index(df))
  #  df.zscore <- (df-df.mean)/df.sd
  #} else {
  #  df.zscore <- df/df.sd
  #}
  df.zscore <- scale(df,center=ma)
  return(df.zscore)
}

calc_vlt <- function(df,window=60) {
  df.vlt <- xts(apply(df,2,runSD,n=window), index(df))
  df.vlt <- lag(df.vlt,1) 
  df.vlt <- df.vlt*df.vlt #vlt = sd*sd
  colnames(df.vlt) <- paste(colnames(df.vlt),"vlt",sep="")
  return(df.vlt)
}

calc_vol <- function(df,window=20) {
  df.adv <- runMean(df,n=window,cumulative=FALSE)
  df.logadv <- log(df.adv)
  df.ratio <- df / df.adv
  df.vol <- cbind(df,df.adv,df.logadv,df.ratio)
  df.vol <- lag(df.vol,1)
  colnames(df.vol) <- c(paste("Y",colnames(df),sep=""),paste("AD",colnames(df),window,sep=""),
                        paste("AD",colnames(df),"log",sep=""),paste(colnames(df),"rat",sep=""))
  return(df.vol)
}

calc_bin <- function(var,bin_var,b1=-2.,b2=2) {
    v.temp <- na.exclude(merge(var,bin_var))
    x <- c(b1,b2)
    y <- c(1,0)
    vl <- v.temp[,names(var)]*approx(x,y,v.temp[,names(bin_var)],yleft=1,yright=0)$y
    y <- c(0,1)
    vh <- v.temp[,names(var)]*approx(x,y,v.temp[,names(bin_var)],yleft=0,yright=1)$y
    v.out <- cbind(vl,vh)
    naml <- paste(names(var),".",abs(trunc(10*b1)),abs(trunc(10*b2)),"l",sep="")
    namh <- paste(names(var),".",abs(trunc(10*b1)),abs(trunc(10*b2)),"h",sep="")
    colnames(v.out) <- c(naml,namh)
  return(v.out)
}

calc_volres <- function(df) {  #assumes df has two columns, retD / cmnD, daily dollar volumes of stx and cmn
  vol_ratio <- df[,"retD"]/df[,"cmnD"]
  return(vol_ratio)
}

calc_voladv <- function(df,window=20) { #assumes df contains single column of Volume
  df.adv <- runMean(df,n=window,cumulative=FALSE)
  vol_ratio <- df / df.adv
  return(vol_ratio)
}

