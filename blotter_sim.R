#make into function
#add in ordering logic
#pass in sim_start_date, sim_end_date

#sim Blotter
if (verbose) print ("Running Sim")
shares <- matrix(nrow=length(sim_date_index),ncol=length(stx.symbols))
#for ( s in stx.symbols) {
for (SimDate_i in 1:(length(sim_date_index))) {
  SimDate <- sim_date_index[SimDate_i]
  equity <- init_equity #equity <- getEndEq(account.st, SimDate)
  if (verbose) print(paste("DATE:",SimDate,"Equity:",equity))
  port.pos <- port_opt_lp(as.vector(MU[SimDate]),as.vector(VLTY[SimDate]),equity)
  shares[SimDate_i,] <- port.pos
  if (verbose) print(port.pos)
  #for (stk in 1:length(stx.symbols)) {
  #  stk.ticker <- stx.symbols[stk]
  #  Posn <- getPosQty(portfolio.st,Symbol=stk.ticker,Date=SimDate)
  #  if ((abs(port.pos[stk]) < lot_size) & (abs(Posn) < lot_size)) next
  #  stk.price_field <- paste(stk.ticker,"Adjusted",sep=".")
  #  stk.price <- as.numeric(get(stk.ticker)[SimDate,stk.price_field])
  #  if (abs(port.pos[stk]) < lot_size) {
  #    stk.qty <- 0
  #  } else {
  #    stk.qty <- port.pos[stk]/stk.price
  #  }
  #  stk.order_size <- stk.qty - Posn
  #  if (verbose) print(paste(stk.ticker,Posn,port.pos[stk],stk.qty,stk.price))
  #  addTxn(portfolio.st,Symbol=stk.ticker,TxnDate=SimDate,TxnPrice=stk.price,TxnQty=stk.order_size,TxnFees=0)
  #}
  #AdjPrice <- as.numeric(Ad(get(s)[i,]))
  #Posn <- getPosQty(portfolio.st,Symbol=s,Date=CurrentDate)
  #UnitSize <- as.numeric(trunc(equity/AdjPrice))
#    if (Posn == 0) {
#      if (AdjPrice < 0.8*start.price) {
#        addTxn(portfolio.st,Symbol=s,TxnDate=CurrentDate,TxnPrice=AdjPrice,TxnQty=UnitSize,TxnFees=0)
#      } else if (AdjPrice > 1.2*start.price) {
#        addTxn(portfolio.st,Symbol=s,TxnDate=CurrentDate,TxnPrice=AdjPrice,TxnQty=-UnitSize,TxnFees=0)
#      }
#    } else if (Posn > 0) {
#      if (AdjPrice > start.price) {
#        addTxn(portfolio.st,Symbol=s,TxnDate=CurrentDate,TxnPrice=AdjPrice,TxnQty=-Posn,TxnFees=0)
#      }
#    } else if (Posn < 0) {
#      if (AdjPrice < start.price) {
#        addTxn(portfolio.st,Symbol=s,TxnDate=CurrentDate,TxnPrice=AdjPrice,TxnQty=-Posn,TxnFees=0)
#      }
#    }
    #updatePortf(portfolio.st,Dates=SimDate)
    #updateAcct(account.st,Dates=SimDate)
    #updateEndEq(account.st,SimDate)
}