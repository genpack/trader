# Header
# Filename:      strategies.R
# Description:   Contains various trading strategies for forex to be applied to an instance of Virtual.Trader class
# Author:        Nima Ramezani Taghiabadi
# Email :        N.RamezaniTaghiabadi@uws.edu.au
# Date_Started:  20 May 2014
# Date_Modified: 25 March 2021
# Version:       2.0

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


#' @exportClass STRATEGY
#' 
STRATEGY = setRefClass('STRATEGY',
  fields = c(objects = 'list', parameters = 'list', label = 'character'),
  methods = list(
    initialize = function(...){
      parameters <<- list(...)
    },
    
    start = function(vt){
      check.parameters(vt)
    },
    permit = function(vt){
      eq = vt$equity()
      if (parameters$hyper_ts){SL = max(vt$history$equity) - parameters$hyper_sl} else {SL = - parameters$hyper_sl}
      exp.permit = (vt$current.time.number < parameters$expiry_tn)
      tp.permit  = ((is.null(parameters$hyper_tp)) | (eq <   parameters$hyper_tp))
      sl.permit  = (is.null(SL) | (eq > SL))
      
      return(exp.permit & tp.permit & sl.permit)
    },
    
    check_parameters = function(vt){
      errmsg = "Invalid value for parameter '%s'."
      if(!is.null(parameters[['lot']])){assert(parameters[['lot']] > 0, sprintf(errmsg, 'lot'))}
      if(!is.null(parameters[['hyper_tp']])){assert((parameters$hyper_tp  >= 0) | is.null(parameters$hyper_tp), sprintf(errmsg, 'hyper_tp'))}
      if(!is.null(parameters[['hyper_sl']])){assert((parameters$hyper_sl  >= 0) | is.null(parameters$hyper_sl), sprintf(errmsg, 'hyper_sl'))}
      if (is.null(parameters$expiry_tn)){parameters$expiry_tn <<- vt$number.of.intervals}
      parameters$expiry_tn <<- min(parameters$expiry_tn, vt$number.of.intervals)
      parameters$expiry_tn <<- max(parameters$expiry_tn, vt$current.time.number)
      if (!is.null(parameters$dir)){assert((parameters$dir == 1) | (parameters$dir == -1) | (parameters$dir == 0), sprintf(errmsg, 'dir'))}
    }))


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
LIMITED = setRefClass(
  'LIMITED', contains = 'STRATEGY',
  methods = list(
    initialize = function(...){
      label <<- "LTD"
      
      callSuper(...)
      default_parameters <- list(lot = 0.1, tp.pips = 30, sl.pips = 30)
      parameters <<- parameters %>% c(default_parameters %>% list.remove(names(parameters)))
    },
    
    start = function(vt){
      check_parameters(vt)
      decide(vt)},
    
    # Decision making framework for ltd_sngl and ltd_mul strategies
    # Takes a position with parameters$tp and parameters$sl(limited) in a certain given direction if there is no existing position
    # Input 1: vt: Trading Environment to be decided
    # Input 2: pm: Trading Parameters
    # Return: None (input trading environment object vt is modified)
    decide = function(vt){
      if (vt$position.balance(label = label) == 0){
        if (parameters$dir == 1){
          vt$take.buy(lot = parameters$lot, tp = parameters$tp.pips, sl = parameters$sl.pips, label = "LTD")
        }
        if (direction == -1){
          vt$take.sell(lot = parameters$lot, tp = parameters$tp.pips, sl = parameters$sl.pips, label = "LTD")
        }
      }  
    },
    
    run = function(vt){
      while (permit(vt, pm)){
        vt = decide(vt, pm)
        vt$jump.next(parameters$expiry_tn)
      }
    }
      
  ))

# Single Limited Strategy:
# takes only one buy or sell position with tp or sl
# in this strategy hyper_tp is the tp and hyper_sl is the sl of the position taken
#' @export
LIMITED.SINGLE <- setRefClass(
  'LIMITED.SINGLE', contains = "LIMITED",
  methods = list(
    initialize = function(...){
      label <<- "LTD"

      callSuper(...)
      default_parameters <- list(lot = 0.1, tp.pips = 30, sl.pips = 30)
      parameters <<- parameters %>% c(default_parameters %>% list.remove(names(parameters)))
    },
    
    run = function(vt){
      start(vt)
      vt$jump.next(parameters$expiry_tn)
    }
))

#' @export
REDLINE <- setRefClass(
  'REDLINE', contains = "STRATEGY",
  methods = list(
    initialize = function(...){
      label <<- "RLN"
      
      callSuper(...)
      default_parameters <- list(hyper_tp = 1000, hyper_sl=1000, hyper_ts = FALSE, lot = 0.1)
      parameters <<- parameters %>% c(default_parameters %>% list.remove(names(parameters)))
    },
    
    check_parameters = function(vt){
      callSuper(vt)
      if (!is.null(parameters$max_dur)){parameters$expiry_tn <<- min(vt$number.of.intervals, vt$current.time.number+parameters$max_dur)}
    },
    
    start = function(vt){
      check_parameters(vt)
      vt$take.buy(lot = parameters$lot, label = "RLN")
      objects$redline <<- vt$current.price - 10*vt$pip
    },
    
    decide = function(vt){
      if ((vt$current.price > objects$redline) & (vt$position.balance() < 0)){
        vt$close.all(labels = "RLN")
        vt$take.buy(lot = parameters$lot, label = "RLN")
      }
      
      if ((vt$current.price < objects$redline) & (vt$position.balance() > 0)){
        vt$close.all(labels = "RLN")
        vt$take.sell(lot = parameters$lot, label = "RLN")
      }
    },
    
    run = function(vt){
      start(vt)
      while (permit(vt)){
        decide(vt)
        vt$jump()
      }
    }
  ))

#' @export
MILK <- setRefClass(
  'MILK', contains = "STRATEGY",
  methods = list(
    initialize = function(...){
      label <<- "MLK"
      
      callSuper(...)
      default_parameters <- list(dir = 1,  hyper_tp = 10000, hyper_sl = 10000, hyper_ts = FALSE, expiry_tn = NA, max_dur = NA, lot = 0.1, tp.pips=100, gap.pips = 100, force = TRUE, tp_man=FALSE , pend_man=FALSE, n.above = NA, n.below = NA)
      parameters <<- parameters %>% c(default_parameters %>% list.remove(names(parameters)))
    },
    
    start = function(vt){
      check_parameters(vt)
      
      if (parameters$dir %in% c(1,-1)){
        vt$take.position.array(n = parameters$n.above, direction = parameters$dir, tp = (1-2*parameters$tp_man)*parameters$tp.pips, lot = parameters$lot, gap = parameters$gap.pips, manual = parameters$pend_man, label  = label)
        vt$take.position.array(n = parameters$n.below, direction = parameters$dir, tp = (1-2*parameters$tp_man)*parameters$tp.pips, lot = parameters$lot, gap = parameters$gap.pips, manual = parameters$pend_man, label  = label, above = FALSE, take_now = FALSE)
      }
      else {
        vt$take.position.array(n = parameters$n.above, direction = -1, tp = (1-2*parameters$tp_man)*parameters$tp.pips, lot = parameters$lot, gap = parameters$gap.pips, manual = parameters$pend_man, label  = label)
        vt$take.position.array(n = parameters$n.below, direction =  1, tp = (1-2*parameters$tp_man)*parameters$tp.pips, lot = parameters$lot, gap = parameters$gap.pips, manual = parameters$pend_man, label  = label, above = FALSE, take_now = FALSE)
      }
    },
    
    decide = function(vt, jump_info){
      spr = vt$spread*vt$pip
      tbl = function(inf_pos){
        vt$take.buy.limit(price  = vt$position$price[inf_pos] - spr, tp = (1-2*parameters$tp_man)*parameters$tp.pips, lot = parameters$lot, force = parameters$force, manual = parameters$pend_man, label = label)
      }
      tsl = function(inf_pos){
        vt$take.sell.limit(price = vt$position$price[inf_pos] + spr, tp = (1-2*parameters$tp_man)*parameters$tp.pips, lot = parameters$lot, force = parameters$force, manual = parameters$pend_man, label = label)
      }
      if (parameters$tp_man) {ss = jump_info$tp.man.died} else {ss = jump_info$tp.died}
      for (inf.pos in ss){  # If the influenced position hit tp
        if (vt$position$label[inf.pos] == label){
          if (parameters$dir == 1){tbl(inf.pos)} else {tsl(inf.pos)}
        }
      }
    },
    
    run = function(vt){
      start(vt)
      info = vt$jump.next(parameters$expiry_tn)
      while (permit(vt)){
        decide(vt, info)
        info = vt$jump.next(parameters$expiry_tn)
      }
    }
  ))

# "Positive Linear" is another name for Hunter
#' @export
HUNTER <- setRefClass(
  'HUNTER', contains = "STRATEGY",
  methods = list(
    initialize = function(...){
      label <<- "HNT"
      callSuper(...)
      default_parameters <- list(dir = 1,  hyper_tp = 10000, hyper_sl = 10000, hyper_ts = FALSE, expiry_tn = NA, max_dur = NA, lot = 0.1, sl.pips=10, gap.pips = 100, force = TRUE, sl_man = TRUE , pend_man=FALSE, n.above = NA, n.below = NA, take_now = TRUE)
      parameters <<- parameters %>% c(default_parameters %>% list.remove(names(parameters)))
    },
    
    start = function(vt){
      check_parameters(vt)
      
      if (parameters$dir %in% c(1,-1)){
        vt$take.position.array(n = parameters$n.above, direction = parameters$dir, sl = (1-2*parameters$sl_man)*parameters$sl.pips, lot = parameters$lot, gap = parameters$gap.pips, manual = parameters$pend_man, label  = label, take_now = FALSE)
        vt$take.position.array(n = parameters$n.below, direction = parameters$dir, sl = (1-2*parameters$sl_man)*parameters$sl.pips, lot = parameters$lot, gap = parameters$gap.pips, manual = parameters$pend_man, label  = label, above = FALSE, take_now = parameters$take_now)
      } else {
        vt$take.position.array(n = parameters$n.above, direction =  1, sl = (1-2*parameters$sl_man)*parameters$sl.pips, lot = parameters$lot, gap = parameters$gap.pips, manual = parameters$pend_man, label  = label, take_now = FALSE)
        vt$take.position.array(n = parameters$n.below, direction = -1, sl = (1-2*parameters$sl_man)*parameters$sl.pips, lot = parameters$lot, gap = parameters$gap.pips, manual = parameters$pend_man, label  = label, above = FALSE, take_now = parameters$take_now)
      }
    },
    
    decide = function(vt, jump_info){
      spr = vt$spread*vt$pip
      tbs = function(inf_pos){
        vt$take.buy.stop(price  = vt$position$price[inf_pos] - spr, sl = (1-2*parameters$sl_man)*parameters$sl.pips, lot = parameters$lot, force = parameters$force, manual = parameters$pend_man, label = label)
      }
      tss = function(inf_pos){
        vt$take.sell.stop(price = vt$position$price[inf_pos] + spr, sl = (1-2*parameters$sl_man)*parameters$sl.pips, lot = parameters$lot, force = parameters$force, manual = parameters$pend_man, label = label)
      }
      if (parameters$sl_man) {ss = jump_info$sl.man.died} else {ss = jump_info$sl.died}
      for (inf.pos in ss){  # If the influenced position hit sl
        if (vt$position$label[inf.pos] == label){
          if      (parameters$dir == -1){tss(inf.pos)}
          else if (parameters$dir == 1) {tbs(inf.pos)}
          else{
            if (vt$current.price > vt$position$price[inf.pos] + spr){tss(inf.pos)} else {tbs(inf.pos)}
          }
        }
      }
    },
    
    run = function(vt){
      # The strategy program starts here
      callSuper(vt)

      start(vt)
      info   = vt$jump.next(parameters$expiry_tn)
      while (permit(vt)){
        decide(vt, info)
        info = vt$jump.next(parameters$expiry_tn)
      }
    }
))



