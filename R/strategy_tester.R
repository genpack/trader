# Header
# Filename:     strategy_tester.R
# Description:  Contains functions for testing and evaluating trading strategies for forex to be applied to an instance of Virtual.Trader class
# Author:       Nima Ramezani Taghiabadi
# Email :       N.RamezaniTaghiabadi@uws.edu.au
# Date:         20 May 2014
# Version:      3.0
# Changes from previous version:

# This version of strategy_tester is compatible with virtual_trader_ver_3 and strategies_ver_2

library(rapport)

# source("init.R")
#
# lib.set = c()
# lib.set = c(lib.set, paste(packages.path, "nima", "general_lib.R", sep = "/"))
# lib.set = c(lib.set, paste(packages.path, "nima", "artificial_intelligence", "business_intelligence", "trading", "virtual_trader.R", sep = "/"))
# lib.set = c(lib.set, paste(packages.path, "nima", "artificial_intelligence", "business_intelligence", "trading", "strategies.R", sep = "/"))
#
# for (lib in lib.set){source(lib)}


apply.strategy <- function(vt, strategy_name, time_number, parameters = default.parameters(vt, strategy_name), tpsl_ratio = NA){
  # This function, applies a given strategy on a virtual trading environment
  # A strategy, is a hyper position with given parameters taken at time_number
  # The output is interpreted as below:
  # output = 0: hyper position expired before hiting the hyper_tp or hyper_sl
  # output = 1: hyper position hit the hyper_tp
  # output = 2: hyper position hit the hyper_sl

  if (time_number>vt$number.of.intervals - 100){
    time_number=vt$number.of.intervals - 100
  }
  # Reseting the trade environment
  vt$reset()
  vt$goto(time_number)
  if (!is.na(tpsl_ratio)){
    parameters$hyper_tp = parameters$hyper_sl * tpsl_ratio
  }

  if (strategy_name == "sgl_ltd"){
    # Single Limited Strategy:
    # takes only one buy or sell position with tp or sl
    # in this strategy hyper_tp is the tp and hyper_sl is the sl of the position taken
    vt = sgl_ltd(vt, parameters)
  }

  else if (strategy_name == "mul_ltd"){
    # simply takes sequential buy or sell positions with defined tp and sl
    # waits until the previous position hits tp or sl and then takes the next position
    # This continues until one of hyper_tp or hyper_sl are hit or the strategy expires
    # This strategy does not have any prediction models.
    # All the positions are either buy or sell regardless of the technical or fundamental status of the market
    # Definition of Parameters:
    # parameters$dir     : if 1, buys are always taken, if -1 sells are taken
    # parameters$lot     : lots for taking, if positive buys and if negative, sells are taken
    # parameters$tp.pips : tp for the positions
    # parameters$sl.pips : sl for the positions

    vt = mul_ltd(vt, parameters)
  }

  else if (strategy_name == "prc_wll_lve"){
    prc_wll_lve(vt, parameters)
  }

  else if (strategy_name == "neg_lin"){
    neg_lin(vt, parameters)
  }

  else if (strategy_name == "pos_lin"){
    pos_lin(vt, parameters)
  }

  else if (strategy_name == "neg_lin_man"){
    neg_lin_man(vt, parameters)
  }

  else if (strategy_name == "smp_cmp"){
    smp_cmp(vt, parameters)
  }

  else if (strategy_name == "adv_cmp"){
    adv_cmp(vt, parameters)
  }

  else if (strategy_name == "paradise"){
    paradise(vt, parameters)
  }

  else if (strategy_name == "paradise_cb"){
    paradise_cb(vt, parameters)
  }

  else if (strategy_name == "pwl_auto"){
    pwl_auto(vt, parameters)
  }

  else if (strategy_name == "pwl_auto_cb"){
    pwl_auto_cb(vt, parameters)
  }

  else  {
    print("apply.strategy Error: Unknown strategy name")
    return()
  }

  # Extracting Results for Output:
  dur = vt$current.time.number - time_number
  if (dur<0){
    assert(FALSE,"Giret avordam")
  }
  leq = vt$equity()
  mnq = min(vt$history$equity)
  mxq = max(vt$history$equity)
  avq = mean(vt$history$equity)

  if (is.na(parameters$hyper_tp)) {tp_limit = 0} else {tp_limit = parameters$hyper_tp}
  if (is.na(parameters$hyper_sl)) {sl_limit = 0} else {sl_limit = parameters$hyper_sl}

  if (vt$equity() >= tp_limit) {rf = TRUE}
  else if (vt$equity() <= - sl_limit) {rf = FALSE}
  else {rf = NA}

  abs.lots = abs(vt$history$lots)

  mxl = max(abs.lots)
  avl = mean(abs.lots)

  output = list(duration = dur, last.equity = leq, min.equity = mnq, max.equity = mxq, mean.equity = avq, max.lot = mxl, mean.lot = avl, result.flag = rf)

  return(output)
}

generate.desired.test.data.frame <- function(num.time.intervals, num.parameter.sets, time_num_min, time_num_max, parameters_min, parameters_max, time.first = TRUE, replacement = TRUE){
  # If flag = TRUE  then  and each set is tested for n2 time intervals
  # If time.first = TRUE,
    # First n1 time intervals are randomly generated and
    # then  n2 parameter sets are generated to be tested for each time interval.
  # If time.first = FALSE,
    # First n1 parameter sets are generated randomly and
    # then  n2 time intervals are generated to be tested for parameter set.
  # Total number of experiments are n1*n2


  m = length(parameters_min)
  assert(m == length(parameters_max), "evaluate.strategy Error: parameters_max and parameters_min must have the same length")

  if (time.first){
    time.num = sample(time_num_min:time_num_max, size = num.time.intervals, replace = replacement)
    desir.table = c()
    for (i in time.num){
      param.table = as.data.frame(matrix(rep(0, num.parameter.sets*m), nrow = num.parameter.sets))
      colnames(param.table) = names(parameters_min)
      for (j in 1:m){
        if (is.numeric(parameters_min[[j]]) & is.numeric(parameters_max[[j]]))
          { param.table[,j] = runif(num.parameter.sets, min = parameters_min[[j]], max = parameters_max[[j]]) }
        else if ( is.na(parameters_min[[j]]) | is.na(parameters_max[[j]]) ) {param.table[,j] = parameters_min[[j]]}
        else if ( is.boolean(parameters_min[[j]]))
          { param.table[,j] = runif(num.parameter.sets, min = parameters_min[[j]], max = parameters_max[[j]]) > 0.5 }
        else {param.table[,j] = parameters_min[[j]]}
      }
      time.col    = rep(i, num.parameter.sets)
      desir.table = rbind(desir.table, cbind(time.col, param.table))
    }
  } else {

    param.table = matrix(rep(0, num.parameter.sets*m), nrow = num.parameter.sets)
    for (j in 1:m){
      param.table[,j] = runif(num.parameter.sets, min = parameters_min[[j]], max = parameters_max[[j]])
    }
    desir.table = c()
    for (i in sequence(num.parameter.sets)){
      time.col    = sample(time_num_min:time_num_max, size = num.time.intervals, replace = replacement)
      desir.table = rbind(desir.table, cbind(time.col, row.repeat(param.table[i, ], num.time.intervals)))
    }
  }
  colnames(desir.table) = c("time.num", names(parameters_min))
  rownames(desir.table) = sequence(dim(desir.table)[1])
  return (desir.table)
}

evaluate.strategy = function(vt, strategy_name, desired_test_table){
  # vt = prepare.environment(currency_pair, from_date, until_date, period = period, dt_format = dt_format)

  N = dim(desired_test_table)[1]
  m = dim(desired_test_table)[2]

  duration    = rep(0, N)
  last.equity = rep(0, N)
  min.equity  = rep(0, N)
  max.equity  = rep(0, N)
  mean.equity = rep(0, N)
  max.lot     = rep(0, N)
  mean.lot    = rep(0, N)
  ben.per.day = rep(0, N)
  rate = rep(0, N)
  result.flag = rep(0, N)

  for (i in sequence(N)){
    param = as.list(desired_test_table[i,2:m])
    res = apply.strategy(vt, strategy_name, time_number = desired_test_table[i ,1], parameters = param)
    # Results of applying the strategy
    duration[i]    = res$duration
    last.equity[i] = res$last.equity
    min.equity[i]  = res$min.equity
    max.equity[i]  = res$max.equity
    mean.equity[i] = res$mean.equity
    max.lot[i]     = res$max.lot
    mean.lot[i]    = res$mean.lot
    ben.per.day[i] = res$last.equity/res$duration
    rate[i] = ben.per.day[i]/(abs(min.equity[i]) + 1000*max.lot[i])
    result.flag[i] = res$result.flag
    cat("Experiment (",i,"): applied ", strategy_name ," strategy at: ", desired_test_table[i ,1], " with parameters: (TP = ", param$hyper_tp, " SL = ", param$hyper_sl, " Equity = ", last.equity[i], ")","\n")
  }

  output = data.frame(desired_test_table, duration, equity = last.equity, min_eq = min.equity, max_eq = max.equity, mean_eq = mean.equity, max_lot = max.lot, mean_lot = mean.lot, per_day = ben.per.day, rate = rate, success = result.flag)
  return(output)
}

summary = function(R, period = "D", remove_incomplete = FALSE){
  # Input argument R is a table(data.frame) containing the evaluation results
  # It can be the output of function "evaluate.strategy()"
  # Argument "period" specifies the period of the trading environment on which all the tests have been implemented.
  # Returns the annual rate of investment
  # If N is small, you will have erratic estimations of rate, N must be at least 100
  if (period == "D"){gain = 26100}
    else if (period == "H"){gain = 24*26100} else {assert(FALSE,"Unknown Period")}

  N       = dim(R)[1]
  sqrtN   = sqrt(N)
  ben     = mean(R$equity)
  ben.sd  = sd(R$equity)/sqrtN
  dur     = mean(R$duration)
  dur.sd  = sd(R$duration)/sqrtN
  inv.vec = 1000*R$max_lot+ abs(R$min_eq)
  inv     = mean(inv.vec)
  inv.sd  = sd(inv.vec)/sqrtN

  inv.dur.vec = inv.vec*R$duration

  # using Filler'r formula to find the %95 Confidence Interval of the ratio of y/x: (http://en.wikipedia.org/wiki/Fieller's_theorem)
  # based on paper:
  # WILLIAM P. DUNLAP and N. CLAYTON SILVER, "Confidence intervals and standard errors for
  # ratios of normal variables", Behavior Research Methods, Instruments, & Computers  1986, 18 (5), 469-47

  # Finding the mean and variance of multiplication of investment and duration
  Vx = dur*(inv.sd^2) + inv*(dur.sd^2) + 2*inv*dur*cov(inv.vec, R$duration)/N
  x  = mean(inv.dur.vec)
  x2 = x*x

  Vy = ben.sd^2
  y  = ben
  y2 = y*y

  y_x   = y/x
  y2_x2 = y2/x2

  cov_xy  = cov(R$equity, inv.dur.vec)/N
  cov_xy2 = cov_xy*cov_xy
  t  = 1.96
  t2 = t*t
  Q  = 1 - (t2*Vx/x2)
  C  = (y_x - t2*cov_xy/x2)/Q
  SE = sqrt(Vy -2*y_x*cov_xy + Vx*y2_x2 - (t2*Vx/x2)*(Vy - cov_xy2/Vx))/(x*Q)
  r.min = C - t*SE
  r.max = C + t*SE

  act.r = ben/(dur*inv)
  wcs.r = (ben - t*ben.sd)/((dur + t*dur.sd)*(inv + t*inv.sd))  # rate in worst case scenario

  n.succ = sum( R$success[!is.na(R$success)])
  n.fail = sum(!R$success[!is.na(R$success)])

  if (remove_incomplete){
    R = R[!is.na(R$success),]
    N = dim(R)[1]
  }

  positive = (R$equity >= 0)
  negative = (R$equity <  0)

  n.pos = sum(positive)
  n.neg = sum(negative)

  p   = (sum(positive) + 1)/(N+2)
  p   = p - 1.96*sqrt(p*(1-p)/N)
  if (p < 0) {p = 1/(N+2)}

  if (sum(positive) >= 1){
    B   = mean(R$equity[positive])
    ds  = mean(R$duration[positive])
    Is  = mean(R$max_lot[positive])*1000 + abs(mean(R$min_eq[positive]))
  } else {
    # Since we have no successful experiment, we estimate parameters of success
    B   = mean(R$hyper_tp)
    if (is.na(B)) {B = abs(mean(R$equity))}
    ds  = mean(R$duration)*B/abs(mean(R$equity))
    Is  = mean(R$max_lot)*1000 + abs(mean(R$min_eq))
  }

  if (sum(negative) >= 1){
    L   = - mean(R$equity[negative])
    df  = mean(R$duration[negative])
    If  = mean(R$max_lot[negative])*1000 + abs(mean(R$min_eq[negative]))
  }
  else{
    # Since we have no failed experiment, we estimate parameters of failure
    L = mean(R$hyper_sl)
    if (is.na(L)) {L = abs(mean(R$equity))}
    df  = mean(R$duration)*L/abs(mean(R$equity))
    If  = mean(R$max_lot)*1000 + abs(mean(R$min_eq))
  }

  E.b = B + L - L/p
  E.I = Is + If*(1-p)/p
  E.d = ds + df*(1-p)/p

  r_est = E.b/(E.I*E.d)

  out = list(n_succ = n.succ, n_fail = n.fail, n_pos = n.pos, n_neg = n.neg, prob_suc = p, odds_suc = p/(1-p), odds_ratio = p*B/(L*(1-p)), exp_ben = E.b, exp_inv = E.I, exp_dur = E.d, rate = gain*r_est, ben = ben, ben.sd = ben.sd, dur = dur, dur.sd = dur.sd, inv =inv, inv.sd = inv.sd, act_rate = gain*act.r, rate_min = gain*r.min, rate_max = gain*r.max, wcs_rate = gain*wcs.r)
  return(out)
}
