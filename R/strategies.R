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

# Single Limited Strategy:
# takes only one buy or sell position with tp or sl
# in this strategy hyper_tp is the tp and hyper_sl is the sl of the position taken
#' @export
sgl_ltd <- function(vt, pm = default.parameters("sgl_ltd")){
  if (is.na(pm$expiry_tn)){pm$expiry_tn = vt$number.of.intervals}
  assert(check.parameters(vt, pm, "sgl_ltd"), "Error from sgl_ltd: invalid parameters")

  vt = ltd.decide(vt, pm)
  vt$jump.next(pm$expiry_tn)
}

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
