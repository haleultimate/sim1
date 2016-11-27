symbols <- c(
  "XLF", # Financial sector ETF
  "BRK-B",
  "JPM",
  "WFC",
  "BAC",
  "C",
  "USB",
  "GS",
  "AIG",
  "CB",
  "AXP",
  "MET",
  "MS",
  "BLK",
  "PNC",
  "BK",
  "SCHW",
  "CME",
  "COF",
  "MMC",
  "PRU",
  "TRV",
  "SPGI",
  "ICE",
  "BBT",
  "AON",
  "AFL",
  "STT",
  "ALL",
  "DFS",
  "STI",
  "PGR",
  "MTB",
  "HIG",
  "TROW",
  "AMP",
  "FITB",
  "NTRS",
  "PFG",
  "KEY",
  "IVZ",
  "BEN",
  "RF",
  "CINF",
  "L",
  "HBAN",
  "LNC",
  "XL",
  "AJG",
  "UNM",
  "CMA",
  "NDAQ",
  "AMG",
  "ETFC",
  "TMK",
  "ZION",
  "LUK",
  "AIZ",
  "LM"
)
stx_n <- c(2:length(symbols)) #c(2:length(symbols))  #c(5:10) #c(2:length(symbols))  #c(5:10) #
stx.symbols <- symbols[stx_n]      #list of stx to trade
stx <- length(stx_n)
#load mktdata
stx_list <- append(1,stx_n)
stx_list <- symbols[stx_list]      #cmn index + stx to trade

rm.list <- ls(all=TRUE)
keep.list <- c(stx_list,"stx","stx_list","stx.symbols","stx_n","stx_list.old")
isNameinKeep <- rm.list %in% keep.list
rm.list <- c(rm.list[!isNameinKeep],"keep.list","isNameinKeep","rm.list")
rm(list = rm.list)  #clear environment except for loaded stock data 

require(blotter)
.blotter <- new.env()
.instrument <- new.env()
require(lpSolveAPI)
require(quantmod)
require(dplyr)

#Init data_load vars
Sys.setenv(TZ = "UTC")
currency('USD')
stock(stx.symbols, 
      currency = "USD", 
      multiplier = 1)
adjustment <- TRUE
start_date <- "2004-01-01" #"2004-01-01"
end_date <- "2012-12-31"
if (!exists("stx_list.old")) {         #only load if stx_list has changed
  getSymbols(Symbols = stx_list, 
             src = "yahoo", 
             index.class = "POSIXct",
             from = start_date, 
             to = end_date, 
             adjust = adjustment)
  stx_list.old <- stx_list
} else if (!identical(stx_list,stx_list.old)) {
  isNameinStxold <- stx_list %in% stx_list.old
  stx_list.new <- stx_list[!isNameinStxold]
  getSymbols(Symbols = stx_list.new, 
             src = "yahoo", 
             index.class = "POSIXct",
             from = start_date, 
             to = end_date, 
             adjust = adjustment)
  rm(stx_list.new,isNameinStxold)
  stx_list.old <- stx_list
}

verbose <- FALSE
#run_type <- "add_vars"
#nloops <- 10
#insample.r2.threshold <- 0.02
predict.ret <- "CCret"  #must match first variable defined in v.com 

#corr.threshold <- 0.7
v.com <- NULL
#load(file="cores50.Rdata")
#if (verbose & exists("store.data")) print(store.data[length(store.data)])

sim_init_date <- "2011-12-30"
sim_start_date <- "2012-01-03"
sim_end_date <- "2012-02-01" #end_date
sim_date_range <- paste(sim_start_date,sim_end_date,sep="/") 
sim_date_index <- index(get(stx_list[1])[sim_date_range])

port_size <- init_equity <- 10000*stx
lot_size <- 100

#init portfolio and account
portfolio.st <- "Port.pos"
account.st <- "Acct.pos"

initPortf(name = portfolio.st,
          symbols = stx.symbols,
          initDate = sim_init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = sim_init_date,
         initEq = init_equity)


