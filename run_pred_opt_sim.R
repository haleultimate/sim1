#run pred_opt_sim
source("init_session.R")
source("calc_func.R")
source("port_opt.R")
source("mk_ret_vlt.R")
source("blotter_sim.R")

MU.matrix <- as.matrix(MU[sim_date_index])
MU_shares <- MU.matrix*shares
dayprofit <- rowSums(MU_shares)
totalprofit <- sum(dayprofit)