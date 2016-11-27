#Calculate Vlty and Next day returns
#create vars as defined in v.com, ordered by tier_v list
#stx.data <- NULL
#oos.data <- NULL

if (verbose) print("Setup v.com")
v1 <- NULL
v1$type <- "ret"
v1$var <- "ret"
v1$retnam <- substr(predict.ret,1,2)
v1$cap <- 0.01
v1$name <- predict.ret
v.com$CCret <- v1
names(v.com) <- predict.ret
v1 <- NULL
v1$type <- "ret"
v1$var <- "vlt"
v1$retnam <- "CC"
v1$lag <- 1
v1$name <- "CCvlt"
v.com$CCvlt <- v1

if (verbose) print("Calc cmn vars")
nam <- stx_list[1]
adjv <- calc_adj(get(nam),nam)
lagv <- calc_lag(adjv,rename=TRUE)
cmnpvars <- cbind(adjv,lagv)
cmnret <- calc_rets(cmnpvars)
cmnret.cap <- calc_cap(cmnret,0.01)

MU <- NULL
VLTY <- NULL
for (nam in stx.symbols) {
  #nam <- symbols[n]
  if (verbose) print(paste("Getting data for:",nam))
  
  #calculate returns (list of returns in calc_rets in calc_func.R)
  adjv <- calc_adj(get(nam),nam)
  lagv <- calc_lag(adjv,rename=TRUE)
  pvars <- cbind(adjv,lagv)
  ret <- calc_rets(pvars)
  res <- ret - cmnret  #create resid returns
  if (verbose) print("calc logadv.z")
  logadv.z <- runMean(adjv[,"D"],n=20,cumulative=FALSE)
  logadv.z <- log(logadv.z)
  logadv.z <- logadv.z - 18.5 #create logadv.z, lb=20, assumed 18.5 as mean
  names(logadv.z) <- "logadv.z"
  
  if (verbose) print ("mk_vars_fun")
  stx1.data <- mk_vars_fun(v.com, res, ret, cmnret, logadv.z, adjv, cmnpvars)
  
  if (verbose) print("postprocess data")
  if (is.null(MU)) {
    MU <- stx1.data[,"CCret"]
  } else {
    MU <- cbind(MU,stx1.data[,"CCret"])
  }
  if (is.null(VLTY)) {
    VLTY <- stx1.data[,"CCvlt"]
  } else {
    VLTY <- cbind(VLTY,stx1.data[,"CCvlt"])
  }
  
  #attach date and TICKER as key fields on variable data
  #if (verbose) print(colnames(stx1.data))
  #stx1.data.df <- as.data.frame(stx1.data)
  #stx2.data <- as.data.frame(index(stx1.data))
  #stx2.data[,2] <- rep(nam, nrow(stx1.data))
  #colnames(stx2.data) <- c("date","TICKER")
  #stx1.data.df <- bind_cols(stx2.data,stx1.data.df)
  #if (verbose) print(colnames(stx1.data.df))
  
  #stx1.data.df <- slice(stx1.data.df, -1:-90)
  #stx1.data.df[is.na(stx1.data.df)] <- 0
  #dp <- nrow(stx1.data.df)
  #if (dp > 252) {
  #  oos1.data.df <- slice(stx1.data.df,(dp-252):dp)
  #  stx1.data.df <- slice(stx1.data.df,-(dp-252):-dp)
  #} else {
  #  oos1.data.df <- stx1.data.df
  #  stx1.data.df <- NULL
  #}
  
  #stx.data <- bind_rows(stx.data,stx1.data.df)
  #oos.data <- bind_rows(oos.data,oos1.data.df)
  
}  
  
  
  