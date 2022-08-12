# strategy_tools

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

