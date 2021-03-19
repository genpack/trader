# Header
# Filename:     strategies.R
# Description:  Contains various trading strategies for forex to be applied to an instance of Virtual.Trader class
# Author:       Nima Ramezani Taghiabadi
# Email :       N.RamezaniTaghiabadi@uws.edu.au
# Date:         20 May 2014
# Version:      2.0

# Changes from previous version:

# 1- Strategies register their label in all the positions they take, so now combined strategies can be written.
# 2- Programs changed to accommodate alterations in virtual_trader Version 3
# 3- Functions all.buys, all.sells, top.profitable.buys, top.profitable.sells transferred to virtual_trader version 3
# all.buys() changed name to buy.positions(), respectively for sell
# top.profitable.buys() changed name to peak.profitable.buys() and argument "inverted" changed name to "maximum"
# Respectively for top.profitable.sells()
# 4- Strategy neg_lin_fwd_pos_lin_inv() removed. It exists only in version 1.0

# This version is compatible by virtuaL_trader_ver_3

# source("init.R")
#
# lib.set = c()
# lib.set = c(lib.set, paste(packages.path, "nima", "general_lib.R", sep = "/"))
# lib.set = c(lib.set, paste(packages.path, "nima", "artificial_intelligence", "business_intelligence", "trading", "virtual_trader.R", sep = "/"))
# lib.set = c(lib.set, paste(packages.path, "nima", "artificial_intelligence", "data_mining", "time_series", "time_series_lib.R", sep = "/"))
#
# for (lib in lib.set){source(lib)}

#' @export
default.parameters <- function(strategy_name){
  if      (strategy_name == "prc_wll_lve"){
    pm = list(hyper_tp = 1000, hyper_sl=1000, hyper_ts = FALSE, expiry_tn = NA, max_dur = NA, lot = 0.1)
  }
  else if (strategy_name == "neg_lin"){
    pm = list(dir = 1,  hyper_tp = 10000, hyper_sl = 10000, hyper_ts = FALSE, expiry_tn = NA, max_dur = NA, lot = 0.1, tp.pips=100, gap.pips = 100, force = TRUE, tp_man=FALSE , pend_man=FALSE, n.above = NA, n.below = NA, label = "NLM")
  }
  else if (strategy_name == "pos_lin"){
    pm = list(dir = 1,  hyper_tp = 10000, hyper_sl = 10000, hyper_ts = FALSE, expiry_tn = NA, max_dur = NA, lot = 0.1, sl.pips=10, gap.pips = 100, force = TRUE, sl_man = TRUE , pend_man=FALSE, n.above = NA, n.below = NA, take_now = TRUE, label = "PLM")
  }
  else if (strategy_name == "smp_cmp"){
    # Parameter "lot" in this strategy is the starting lot
    # Parameter "tp.pips" in this strategy is the number of pips in which the whole loss is expected to be compensated
    pm = list(dir = 1 , hyper_tp = 10000, hyper_sl = 10000, hyper_ts = FALSE, expiry_tn = NA, max_dur = NA, lot = 0.1, tp.pips = 100)
  }
  else if (strategy_name == "neg_lin_man"){
    pm = list(dir = 1 , hyper_tp = 10000, hyper_sl = 10000, hyper_ts = FALSE, expiry_tn = NA, max_dur = NA, lot = 0.1, tp.pips=100, gap.pips= 100)
  }
  else if (strategy_name == "adv_cmp"){
    pm = default.parameters("neg_lin_man")
    pm = c(pm, maw = 24, cmp.pips = 100, take_excess = TRUE)
  }
  else if (strategy_name == "paradise"){
    pm = list(hyper_tp=1000, hyper_sl=1000, hyper_ts = FALSE, expiry_tn = NA, max_dur = NA, lot.fwd = 0.01, lot.inv=0.1, tp = 7.0, gap_fwd = 5.0, gap_inv.pips = 50, max_fwd=19, tp.pips = 20)
  }
  else if (strategy_name == "pwl_auto"){
    pm = list(hyper_tp=1000, hyper_sl=1000, hyper_ts = FALSE, expiry_tn = NA, max_dur = NA, lot=0.02, gap.pips = 50, hedging = FALSE)
  }
  else if (strategy_name == "balance.inv"){
    pm = list(hyper_tp=1000, hyper_sl=1000, hyper_ts = FALSE, expiry_tn = NA, max_dur = NA, lot=0.01, gap.pips = 50, sl.pips = 10)
  }
  else if (strategy_name == "balance"){
    pm = list(hyper_tp=1000, hyper_sl=1000, hyper_ts = FALSE, expiry_tn = NA, max_dur = NA, lot=0.01, gap.pips = 50, tp.pips = 10)
  }
  else {print("Error from default.parameters: Unknown strategy name")}
  return(pm)
}

#' @export
check.parameters <- function(vt, parameters, strategy_name){
  strategies.with.tp  = c("neg_lin", "sgl_ltd", "mul_ltd")
  strategies.with.sl  = c("sgl_ltd", "mul_ltd")
  strategies.with.dir = c(strategies.with.tp, "neg_lin_man")
  strategies.with.gap = c("neg_lin_man", "neg_lin", "paradise")



  valid = TRUE
  valid = valid & (parameters$lot <= 10000000) & (parameters$lot >= 0.000000000001)
  valid = valid & ((parameters$hyper_tp  >= 0)| is.na(parameters$hyper_tp)) & ((parameters$hyper_sl >= 0)| is.na(parameters$hyper_sl))
  valid = valid & (parameters$expiry_tn > vt$current.time.number) & (parameters$expiry_tn <= vt$number.of.intervals)

  if (strategy_name %in% strategies.with.dir){
    valid = valid & ((parameters$dir == 1) | (parameters$dir == -1) | (parameters$dir == 0))
  }
  if (strategy_name %in% strategies.with.tp){
    valid = valid & (abs(parameters$tp.pips) >= 10)
  }
  if (strategy_name %in% strategies.with.sl){
    valid = valid & (abs(parameters$sl.pips) >= 10)
  }
  return(valid)
}

#' @export
match.lot <- function(lots, desired_lot){
  # If we have a set of buy and sell positions with various lots, and we want the balance of this set
  # be a certain desired_lot. Which of them should be closed so that we don't add any extra position
  # or if we have to add, add an extra position with minimum absolute value of lot.
  # This function solves this problem. It gets the lots of the current positions (lots is a vector of lots)
  # and returns a vector indicating which of these positions must be closed. and how much extra lot must be taken.
  # The extra lot, if not zero, will appear as the last element of the output vector.

  ltbt  = floor.to.precision(desired_lot - sum(lots))  # ltbt: lots to be taken
  i_min = 1

  while ((ltbt != 0) & (i_min > 0)){
    i_min = 0

    for (i in sequence(length(lots))){
      ltbt_new = floor.to.precision(desired_lot - sum(lots[-i]))
      if (abs(ltbt_new) < abs(ltbt)){
        i_min   = i
        ltbt    = ltbt_new
      }
    }

    if (i_min > 0){
      lots[i_min] = 0
      ltbt        = floor.to.precision(desired_lot - sum(lots))
    }
  }

  if (ltbt == 0){return (lots)} else {return(c(lots, ltbt))}
}

#' @export
permit <- function(vt, pm){
  eq = vt$equity()
  if (pm$hyper_ts){SL = max(vt$history$equity) - pm$hyper_sl} else {SL = - pm$hyper_sl}
  exp.permit = (vt$current.time.number < pm$expiry_tn)
  tp.permit  = ((is.na(pm$hyper_tp)) | (eq <   pm$hyper_tp))
  sl.permit  = (is.na(SL) | (eq > SL))

  return(exp.permit & tp.permit & sl.permit)
}

milk <- function(vt, pos_list){
  # milking in Forex Trading is to close a position with positive profit and take a pending position instead
  for (i in pos_list){
    if ((vt$position$active[i])&(vt$position$profit[i] > 0)){
      if (vt$position$type[i] == 1){
        vt$take.buy.limit(price = vt$position$price[i], tp = vt$position$tp[i], sl = vt$position$sl[i], force = TRUE, setting = "at_price", lot = vt$position$lot[i])
      } else if (vt$position$type[i] == -1){
        vt$take.sell.limit(price = vt$position$price[i], tp = vt$position$tp[i], sl = vt$position$sl[i], force = TRUE, setting = "at_price", lot = vt$position$lot[i])
      }
      vt$close(i)
    }
  }
}

#' @export
ltd.decide <- function(vt, pm){
  # Decision making framework for ltd_sngl and ltd_mul strategies
  # Takes a position with pm$tp and pm$sl(limited) in a certain given direction if there is no existing position
  # Input 1: vt: Trading Environment to be decided
  # Input 2: pm: Trading Parameters
  if (vt$position.balance(label = "LTD") == 0){
    if (pm$dir == 1){
      vt$take.buy(lot = pm$lot, tp = pm$tp.pips, sl = pm$sl.pips, label = "LTD")
    }
    if (direction == -1){
      vt$take.sell(lot = pm$lot, tp = pm$tp.pips, sl = pm$sl.pips, label = "LTD")
    }
  }
  # Return modified trading environment
  return(vt)
}

#' @export
sgl_ltd <- function(vt, pm = default.parameters("sgl_ltd")){
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals}
  assert(check.parameters(vt, pm, "sgl_ltd"), "Error from sgl_ltd: invalid parameters")

  vt = ltd.decide(vt, pm)
  vt$jump.next(pm$expiry_tn)
}

#' @export
mul_ltd <- function(vt, pm = default.parameters("mul_ltd")){
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals}
  assert(check.parameters(vt, pm, "mul_ltd"), "Error from mul_ltd: invalid parameters")

  while (permit(vt, pm)){
    vt = ltd.decide(vt, pm)
    vt$jump.next(pm$expiry_tn)
    eq = vt$equity(labels = "LTD")
  }
}

#' @export
prc_wll_lve.start <- function(vt, pm){
  vt$take.buy(lot = pm$lot, label = "PWL")
}
prc_wll_lve.decide <- function(vt, pm, base){
  if ((vt$current.price > base) & (vt$position.balance() < 0)){
    vt$close.all(labels = "PWL")
    vt$take.buy(lot = pm$lot, label = "PWL")
  }

  if ((vt$current.price < base) & (vt$position.balance() > 0)){
    vt$close.all(labels = "PWL")
    vt$take.sell(lot = pm$lot, label = "PWL")
  }
}

#' @export
prc_wll_lve <- function(vt, pm = default.parameters("prc_wll_lve")){
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals}
  if (!is.na(pm$max_dur)){pm$expiry_tn = min(vt$number.of.intervals, vt$current.time.number+pm$max_dur)}
  assert(check.parameters(vt, pm, "prc_wll_lve"), "Error from prc_wll_lve: invalid parameters")

  prc_wll_lve.start(vt, pm)
  base_line = vt$current.price - 10*vt$pip

  while (permit(vt, pm)){
    prc_wll_lve.decide(vt, pm, base_line)
    vt$jump()
  }
}

#' @export
neg_lin.start <- function(vt, pm){
  if (pm$dir %in% c(1,-1)){
    vt$take.position.array(n = pm$n.above, direction = pm$dir, tp = (1-2*pm$tp_man)*pm$tp.pips, lot = pm$lot, gap = pm$gap.pips, manual = pm$pend_man, label  = pm$label)
    vt$take.position.array(n = pm$n.below, direction = pm$dir, tp = (1-2*pm$tp_man)*pm$tp.pips, lot = pm$lot, gap = pm$gap.pips, manual = pm$pend_man, label  = pm$label, above = FALSE, take_now = FALSE)
  }
  else {
    vt$take.position.array(n = pm$n.above, direction = -1, tp = (1-2*pm$tp_man)*pm$tp.pips, lot = pm$lot, gap = pm$gap.pips, manual = pm$pend_man, label  = pm$label)
    vt$take.position.array(n = pm$n.below, direction =  1, tp = (1-2*pm$tp_man)*pm$tp.pips, lot = pm$lot, gap = pm$gap.pips, manual = pm$pend_man, label  = pm$label, above = FALSE, take_now = FALSE)
  }
}

#' @export
neg_lin.decide <-function(vt, pm, s){
  spr = vt$spread*vt$pip
  tbl = function(inf_pos){
    vt$take.buy.limit(price  = vt$position$price[inf_pos] - spr, tp = (1-2*pm$tp_man)*pm$tp.pips, lot = pm$lot, force = pm$force, manual = pm$pend_man, label = pm$label)
  }
  tsl = function(inf_pos){
    vt$take.sell.limit(price = vt$position$price[inf_pos] + spr, tp = (1-2*pm$tp_man)*pm$tp.pips, lot = pm$lot, force = pm$force, manual = pm$pend_man, label = pm$label)
  }
  if (pm$tp_man) {ss = s$tp.man.died} else {ss = s$tp.died}
  for (inf.pos in ss){  # If the influenced position hit tp
    if (vt$position$label[inf.pos] == pm$label){
      if (pm$dir == 1){tbl(inf.pos)} else {tsl(inf.pos)}
    }
  }
}

#' @export
neg_lin <- function(vt, pm = default.parameters("neg_lin") ){
  # The strategy program starts here
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals}
  if (!is.na(pm$max_dur)){pm$expiry_tn = min(vt$number.of.intervals, vt$current.time.number+pm$max_dur)}
  assert(check.parameters(vt, pm, "neg_lin"), "Error from neg_lin: invalid parameters")

  neg_lin.start(vt, pm)
  s   = vt$jump.next(pm$expiry_tn)
  while (permit(vt, pm)){
    neg_lin.decide(vt, pm, s)
    s = vt$jump.next(pm$expiry_tn)
  }
}

#' @export
pos_lin.start <- function(vt, pm){
  if (pm$dir %in% c(1,-1)){
    vt$take.position.array(n = pm$n.above, direction = pm$dir, sl = (1-2*pm$sl_man)*pm$sl.pips, lot = pm$lot, gap = pm$gap.pips, manual = pm$pend_man, label  = pm$label, take_now = FALSE)
    vt$take.position.array(n = pm$n.below, direction = pm$dir, sl = (1-2*pm$sl_man)*pm$sl.pips, lot = pm$lot, gap = pm$gap.pips, manual = pm$pend_man, label  = pm$label, above = FALSE, take_now = pm$take_now)
  } else {
    vt$take.position.array(n = pm$n.above, direction =  1, sl = (1-2*pm$sl_man)*pm$sl.pips, lot = pm$lot, gap = pm$gap.pips, manual = pm$pend_man, label  = pm$label, take_now = FALSE)
    vt$take.position.array(n = pm$n.below, direction = -1, sl = (1-2*pm$sl_man)*pm$sl.pips, lot = pm$lot, gap = pm$gap.pips, manual = pm$pend_man, label  = pm$label, above = FALSE, take_now = pm$take_now)
  }
}

#' @export
pos_lin.decide <-function(vt, pm, s){
  spr = vt$spread*vt$pip
  tbs = function(inf_pos){
    vt$take.buy.stop(price  = vt$position$price[inf_pos] - spr, sl = (1-2*pm$sl_man)*pm$sl.pips, lot = pm$lot, force = pm$force, manual = pm$pend_man, label = pm$label)
  }
  tss = function(inf_pos){
    vt$take.sell.stop(price = vt$position$price[inf_pos] + spr, sl = (1-2*pm$sl_man)*pm$sl.pips, lot = pm$lot, force = pm$force, manual = pm$pend_man, label = pm$label)
  }
  if (pm$sl_man) {ss = s$sl.man.died} else {ss = s$sl.died}
  for (inf.pos in ss){  # If the influenced position hit sl
    if (vt$position$label[inf.pos] == pm$label){
      if      (pm$dir == -1){tss(inf.pos)}
      else if (pm$dir == 1) {tbs(inf.pos)}
      else{
        if (vt$current.price > vt$position$price[inf.pos] + spr){tss(inf.pos)} else {tbs(inf.pos)}
          }
    }
  }
}

#' @export
pos_lin <- function(vt, pm = default.parameters("pos_lin") ){
  # The strategy program starts here
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals}
  if (!is.na(pm$max_dur)){pm$expiry_tn = min(vt$number.of.intervals, vt$current.time.number+pm$max_dur)}

  assert(check.parameters(vt, pm, "pos_lin"), "Error from pos_lin: invalid parameters")

  pos_lin.start(vt, pm)
  s   = vt$jump.next(pm$expiry_tn)
  while (permit(vt, pm)){
    pos_lin.decide(vt, pm, s)
    s = vt$jump.next(pm$expiry_tn)
  }
}

#' @export
smp_cmp.start  <- function(vt, pm){
  if (pm$dir == 1) {
    vt$take.buy(lot = pm$lot, label = "SCP")
  } else {
    vt$take.sell(lot = pm$lot, label = "SCP")
  }
}

#' @export
smp_cmp.decide <- function(vt, pm){
  vt$close(which(vt$position$profit > 0), labels = "SCP")
  pr = vt$profit(labels = "SCP")
  if ( pr == 0) {
    required.lots = pm$lot
    if (pm$dir == 1) {vt$take.buy(lot = pm$lot, label = "SCP")} else {vt$take.sell(lot = pm$lot, label = "SCP")}
  } else if (pr < 0){
    required.lots = pm$dir*(pm$lot - 0.1*pr/pm$tp.pips)
  }
  lots.to.be.taken = required.lots - vt$position.balance(labels = "SCP")

  lots.to.be.taken = pm$dir*pm$lot*floor(pm$dir*lots.to.be.taken/pm$lot)

  if (lots.to.be.taken >= pm$lot){
    vt$take.buy(lot = lots.to.be.taken, label = "SCP")
  } else
    if (lots.to.be.taken <= - pm$lot){
      vt$take.sell(lot = - lots.to.be.taken, label = "SCP")
    }
}

#' @export
smp_cmp = function(vt, pm = default.parameters("smp_cmp")){
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals}
  if (!is.na(pm$max_dur)){pm$expiry_tn = min(vt$number.of.intervals, vt$current.time.number+pm$max_dur)}
  assert(check.parameters(vt, pm, "smp_cmp"), "Error from smp_cmp: invalid parameters")

  smp_cmp.start(vt, pm)
  while (permit(vt, pm)){
    smp_cmp.decide(vt, pm)
    vt$jump()
  }
}

#' @export
neg_lin_man.decide <- function(vt, pm){
  gap.dollars = 10*pm$gap.pips*pm$lot
  tp.dollars  = 10*pm$tp.pips*pm$lot

  vt$close(which(vt$position$profit > tp.dollars), labels = "NLM")
  if (vt$no.active.position(labels = "NLM")){
    vt$take.position(direction = pm$dir, lot = pm$lot, label = "NLM")
  }
  if (pm$dir == 1){
    top = vt$peak.profitable.buys(n  = 1, labels = "NLM")
  } else {
    top = vt$peak.profitable.sells(n = 1, labels = "NLM")
  }
  if (is.na(top)){
    print ("You should not see this!")}
  if ((vt$position$profit[top] < - gap.dollars)){
    vt$take.position(direction = pm$dir, lot = pm$lot, label = "NLM")
  }
}

#' @export
neg_lin_man <- function(vt, pm = default.parameters("neg_lin_man")){
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals}
  if (!is.na(pm$max_dur)){pm$expiry_tn = min(vt$number.of.intervals, vt$current.time.number+pm$max_dur)}
  assert(check.parameters(vt, pm, "neg_lin_man"), "Error from neg_lin_man: invalid parameters")

  while (permit(vt, pm)){
    neg_lin_man.decide(vt, pm)
    vt$jump()
  }
}

#' @export
adv_cmp <- function(vt, pm = default.parameters("adv_cmp")){
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals}
  if (!is.na(pm$max_dur)){pm$expiry_tn = min(vt$number.of.intervals, vt$current.time.number+pm$max_dur)}
  if (vt$current.time.number <= pm$maw) {vt$goto(pm$maw + 1)}

  assert(check.parameters(vt, pm, "neg_lin_man"), "Error from adv_cmp: invalid parameters")

  pf.open     = floor.to.precision(vt$data$open - moving.average(vt$data$open, pm$maw), 0.1*vt$pip)
  pf.high     = floor.to.precision(vt$data$high - moving.average(vt$data$open, pm$maw), 0.1*vt$pip)
  pf.low      = floor.to.precision(vt$data$low - moving.average(vt$data$open,  pm$maw), 0.1*vt$pip)
  pf.close    = floor.to.precision(vt$data$close - moving.average(vt$data$open,pm$maw), 0.1*vt$pip)

  data.f = data.frame(time = vt$data$time, open = pf.open, high = pf.high, low = pf.low, close = pf.low, volume = vt$data$volume)
  vt.f   = new("VIRTUAL.TRADER", data = data.f)
  vt.f$goto(vt$current.time.number)

  pfc = pm$dir*pm$cmp.pips*vt$pip # predicted future candle

  while (permit(vt, pm)){
    neg_lin_man.decide(vt.f, pm)
    u   = vt.f$position.balance()
    dp  = vt$current.price - vt$data$open[vt$current.time.number - 1]
    dpf = vt.f$current.price - vt.f$data$open[vt.f$current.time.number - 1]
    dm  = dpf - dp
    eqf = vt.f$equity()
    eq  = vt$equity()
    if ((pm$take_excess) & (eq > eqf)) {
      v = u
    } else {v = u*(1.0 - (dm/pfc)) + 0.1*vt$pip*(eqf - eq)/pfc}

    actv = vt$active.positions(labels = "ACP")
    lots = vt$position$lot[actv]*vt$position$type[actv]
    lots_new = match.lot(lots, v)
    n_l   = length(lots)
    n_l_n = length(lots_new)
    for (i in sequence(n_l)){
      if (lots_new[i] == 0){
        vt$close(actv[i], label = "ACP")
      }
    }
    if (n_l_n > n_l){
      vt$take.position(lot = abs(lots_new[n_l_n]), dir = sign(lots_new[n_l_n]), label = "ACP")
    }

    vt.f$jump()
    vt$jump()
  }
}

#' @export
paradise = function(vt, pm = default.parameters("paradise")){
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals-2}
  if (!is.na(pm$max_dur)){pm$expiry_tn = min(vt$number.of.intervals, vt$current.time.number+pm$max_dur)}
  N.nlm = 0
  # Decide for PWL:
  base    = round(vt$current.price/(vt$pip*pm$gap_inv.pips))*pm$gap_inv.pips*vt$pip
  # while (vt$equity() > - pm$hyper_sl & vt$equity() < pm$hyper_tp & N.nlm < pm$max_fwd & (vt$current.time.number < vt$number.of.intervals - 2)){
  while ((permit(vt, pm)) & (N.nlm < pm$max_fwd)){
    if (sum((vt$position$label == "PWL") & vt$position$active) == 0){
      # No Inverse Positions
      if (vt$current.price >= base){
        if (vt$current.price > base + pm$gap_inv.pips*vt$pip){
          # price has rised so high, we take a sell:
          while (base < vt$current.price + 10*vt$pip){
            base = base + pm$gap_inv.pips*vt$pip
          }
          vt$take.sell(lot = pm$lot.inv, label = "PWL", sl = base, setting = "at_price")
        } else{
          while (base > vt$current.price - 10*vt$pip){
            base = base - pm$gap_inv.pips*vt$pip
          }
          vt$take.buy(lot = pm$lot.inv, label = "PWL", sl = base, setting = "at_price")
        }
      } else{
        if (vt$current.price < base - pm$gap_inv.pips*vt$pip){
          # price has fallen too low, we take a buy:
          while (base > vt$current.price - 10*vt$pip){
            base = base - pm$gap_inv.pips*vt$pip
          }
          vt$take.buy(lot = pm$lot.inv, label = "PWL", sl = base, setting = "at_price")
        } else{
          while (base < vt$current.price + 10*vt$pip){
            base = base + pm$gap_inv.pips*vt$pip
          }
          vt$take.sell(lot = pm$lot.inv, label = "PWL", sl = base, setting = "at_price")
        }
      }
    }
    pwl.pos = which((vt$position$label == "PWL") & vt$position$active)
    assert(length(pwl.pos) == 1, "Something goes wrong!")

    nlm.pos = which((vt$position$label == "NLM") & vt$position$active)
    N.nlm   = length(nlm.pos) # Number of forward positions

    # Decide for NLM:
    if (N.nlm > 0){
      nlm.in.profit  = which((vt$position$label == "NLM") & (vt$position$profit > pm$tp) & (vt$position$active))
      nlm.buys       = which((vt$position$label == "NLM") & (vt$position$type ==  1 )  & (vt$position$active))
      nlm.sells      = which((vt$position$label == "NLM") & (vt$position$type == -1)   & (vt$position$active))
      vt$close(nlm.in.profit)

      if (vt$position$type[nlm.pos][1] == -1){
        top = vt$peak.profitable.sells(n  = 1, labels = "NLM")
        if ((is.na(top)) | (vt$position$profit[top] < - pm$gap_fwd)) {
          vt$take.sell(lot = pm$lot.fwd, label = "NLM")
        }
      }
      else{
        top = vt$peak.profitable.buys(n  = 1, labels = "NLM")
        if ((is.na(top)) | (vt$position$profit[top] < - pm$gap_fwd)) {
          vt$take.buy(lot = pm$lot.fwd, label = "NLM")
        }
      }
    }
    if (N.nlm == 0){
      # No forward positions
      if (vt$position$type[pwl.pos] == 1){
        vt$take.sell(lot = pm$lot.fwd, label = "NLM")
      } else{
        vt$take.buy(lot = pm$lot.fwd, label = "NLM")
      }
    }
    vt$jump()
  }
}

#' @export
paradise_cb = function(vt, pm = default.parameters("paradise")){
  # paradise constant base, the base does not change at all
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals-2}
  if (!is.na(pm$max_dur)){pm$expiry_tn = min(vt$number.of.intervals, vt$current.time.number+pm$max_dur)}
  N.nlm = 0
  # Decide for PWL:
  base    = round(vt$current.price/(vt$pip*pm$gap_inv.pips))*pm$gap_inv.pips*vt$pip
  # while (vt$equity() > - pm$hyper_sl & vt$equity() < pm$hyper_tp & N.nlm < pm$max_fwd & (vt$current.time.number < vt$number.of.intervals - 2)){
  while ((permit(vt, pm)) & (N.nlm < pm$max_fwd)){
    if (sum((vt$position$label == "PWL") & vt$position$active) == 0){
      # No Inverse Positions
      if (vt$current.price >= base){
          if (base <= vt$current.price - 10*vt$pip){
            vt$take.buy(lot = pm$lot.inv, label = "PWL", sl = base, setting = "at_price")
          }
      } else{
          if (base >= vt$current.price + 10*vt$pip){
            vt$take.sell(lot = pm$lot.inv, label = "PWL", sl = base, setting = "at_price")
          }
      }
    }
    pwl.pos = which((vt$position$label == "PWL") & vt$position$active)
    N.pwl   = length(pwl.pos) # Number of forward positions

    assert(N.pwl <= 1, "Something goes wrong!")

    nlm.pos = which((vt$position$label == "NLM") & vt$position$active)

    if (N.pwl == 0){
      vt$close(nlm.pos)
    }

    nlm.pos = which((vt$position$label == "NLM") & vt$position$active)

    N.nlm   = length(nlm.pos) # Number of forward positions

    # Decide for NLM:
    if ((N.nlm > 0) & (N.pwl > 0)){
        nlm.buys       = which((vt$position$label == "NLM") & (vt$position$type ==  1 )  & (vt$position$active))
        nlm.sells      = which((vt$position$label == "NLM") & (vt$position$type == -1)   & (vt$position$active))

        if (vt$position$type[pwl.pos] == 1){
          vt$close(nlm.buys)
          top = vt$peak.profitable.sells(n  = 1, labels = "NLM")
          if ((is.na(top)) | (vt$position$profit[top] < - pm$gap_fwd)) {
            vt$take.sell(lot = pm$lot.fwd, label = "NLM", tp = pm$tp.pips)
          }
        }
        else{
          vt$close(nlm.sells)
          top = vt$peak.profitable.buys(n  = 1, labels = "NLM")
          if ((is.na(top)) | (vt$position$profit[top] < - pm$gap_fwd)) {
            vt$take.buy(lot = pm$lot.fwd, label = "NLM", tp = pm$tp.pips)
          }
        }
    }
    if ((N.nlm == 0) & (N.pwl > 0)){
      # No forward positions
      if (vt$position$type[pwl.pos] == 1){
        vt$take.sell(lot = pm$lot.fwd, label = "NLM", tp = pm$tp.pips)
      } else{
        vt$take.buy(lot = pm$lot.fwd, label = "NLM", tp = pm$tp.pips)
      }
    }
    vt$jump()
  }
}


#' @export
pwl_auto = function(vt, pm = default.parameters("pwl_auto")){
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals-2}
  if (!is.na(pm$max_dur)){pm$expiry_tn = min(vt$number.of.intervals, vt$current.time.number+pm$max_dur)}
  # Decide for PWL:
  base    = round(vt$current.price/(vt$pip*pm$gap.pips))*pm$gap.pips*vt$pip
  # while (vt$equity() > - pm$hyper_sl & vt$equity() < pm$hyper_tp & N.nlm < pm$max_fwd & (vt$current.time.number < vt$number.of.intervals - 2)){
  while (permit(vt, pm)){
    if (sum((vt$position$label == "PWL") & vt$position$active) == 0){
      # No Inverse Positions
      if (vt$current.price >= base){
        if (vt$current.price > base + pm$gap.pips*vt$pip){
          # price has rised so high, we take a sell:
          while (base < vt$current.price + 10*vt$pip){
            base = base + pm$gap.pips*vt$pip
          }
          vt$take.sell(lot = pm$lot, label = "PWL", sl = base, setting = "at_price")
          if (pm$hedging){
            vt$take.buy(lot = pm$lot/2, label = "Hedge")
          }
        } else{
          while (base > vt$current.price - 10*vt$pip){
            base = base - pm$gap.pips*vt$pip
          }
          vt$take.buy(lot = pm$lot, label = "PWL", sl = base, setting = "at_price")
          if (pm$hedging){
            vt$take.sell(lot = pm$lot/2, label = "Hedge")
          }
        }
      } else{
        if (vt$current.price < base - pm$gap.pips*vt$pip){
          # price has fallen too low, we take a buy:
          while (base > vt$current.price - 10*vt$pip){
            base = base - pm$gap.pips*vt$pip
          }
          vt$take.buy(lot = pm$lot, label = "PWL", sl = base, setting = "at_price")
          if (pm$hedging){
            vt$take.sell(lot = pm$lot/2, label = "Hedge")
          }
        } else{
          while (base < vt$current.price + 10*vt$pip){
            base = base + pm$gap.pips*vt$pip
          }
          vt$take.sell(lot = pm$lot, label = "PWL", sl = base, setting = "at_price")
          if (pm$hedging){
            vt$take.buy(lot = pm$lot/2, label = "Hedge")
          }
        }
      }
    }
    pwl.pos = which((vt$position$label == "PWL") & vt$position$active)
    assert(length(pwl.pos) == 1, "Something goes wrong!")

    s = vt$jump()
    if ((length(s$sl.died) > 0) & (pm$hedging)){
      assert (length(s$sl.died) == 1)
      vt$close.all()
    }
  }
}

#' @export
pwl_auto_cb = function(vt, pm = default.parameters("pwl_auto")){
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals-2}
  if (!is.na(pm$max_dur)){pm$expiry_tn = min(vt$number.of.intervals, vt$current.time.number+pm$max_dur)}
  # Decide for PWL:
  base    = round(vt$current.price/(vt$pip*pm$gap.pips))*pm$gap.pips*vt$pip
  while (permit(vt, pm)){
    if (sum((vt$position$label == "PWL") & vt$position$active) == 0){
      # No Inverse Positions
      if (vt$current.price >= base){
          if (base <= vt$current.price - 10*vt$pip){
            vt$take.buy(lot = pm$lot, label = "PWL", sl = base, setting = "at_price")
            if (pm$hedging){
              vt$take.sell(lot = pm$lot/2, label = "Hedge")
            }
          }
      } else{
          if (base >= vt$current.price + 10*vt$pip){
            vt$take.sell(lot = pm$lot, label = "PWL", sl = base, setting = "at_price")
            if (pm$hedging){
              vt$take.buy(lot = pm$lot/2, label = "Hedge")
            }
          }
      }
    }
    pwl.pos = which((vt$position$label == "PWL") & vt$position$active)
    assert(length(pwl.pos) <= 1, "Something goes wrong!")

    s = vt$jump()
    if ((length(s$sl.died) > 0) & (pm$hedging)){
      assert (length(s$sl.died) == 1)
      vt$close.all()
    }
  }
}


#' @export
balance.inv = function(vt, pm = default.parameters("balance.inv")){

  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals-2}
  if (!is.na(pm$max_dur)){pm$expiry_tn = min(vt$number.of.intervals, vt$current.time.number+pm$max_dur)}

  while (permit(vt, pm)){
    if (vt$position.balance() == 0){
      vt$take.buy(lot = pm$lot, sl = pm$sl.pips)
      vt$take.sell(lot = pm$lot, sl = pm$sl.pips)
    } else {
      i = vt$position.peak.profit(maximum = FALSE)
      if (0.1*vt$position$profit[i]/vt$position$lot[i] > pm$gap.pips){
        if (vt$position.balance() > 0){
          vt$take.sell(lot = pm$lot, sl = pm$sl.pips)
        } else{
          vt$take.buy(lot = pm$lot, sl = pm$sl.pips)
        }
      }
    }
    s = vt$jump()
  }
}


#' @export
balance = function(vt, pm = default.parameters("balance")){

  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals-2}
  if (!is.na(pm$max_dur)){pm$expiry_tn = min(vt$number.of.intervals, vt$current.time.number+pm$max_dur)}

  while (permit(vt, pm)){
    if (vt$position.balance() == 0){
      vt$take.buy(lot = pm$lot, tp = pm$tp.pips)
      vt$take.sell(lot = pm$lot, tp = pm$tp.pips)
    } else {
      i = vt$position.peak.profit(maximum = TRUE)
      if (-0.1*vt$position$profit[i]/vt$position$lot[i] > pm$gap.pips){
        if (vt$position.balance() > 0){
          vt$take.sell(lot = pm$lot, tp = pm$tp.pips)
        } else{
          vt$take.buy(lot = pm$lot, tp = pm$tp.pips)
        }
      }
    }
    s = vt$jump()
  }
}
