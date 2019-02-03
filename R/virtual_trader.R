# Header
# Filename:     virtual_trader.R
# Description:  Contains a class for trading on a currency-pair in Forex
# Author:       Nima Ramezani Taghiabadi
# Email :       N.RamezaniTaghiabadi@uws.edu.au
# Date:         20 May 2014
# Version:      3.0

# Changes from previous version:

# Positions have strategy labels. Various strategies can label their positions
# so that positions belonging to different strategies are separable and identifiable
# This enables to run multiple strategies on one trading environment
# A data.frame named strategy.data is added containing any information that strategies want to keep
# each row of the data frame can be used by one strategy
# in jump.next() you can manage stopping events. For example you can select to stop only if a tp is hit.
# The influenced events are returned in separate vectors so the output of jump.next() function will be a
# list of vectors. Each vector corresponds to an event type. For example hitting a buy limit
# has a vector that contains BL pending positions that have been activated.

# Notes:
# You may sell your product to companies like this:
# http://exponentialtrading.com.au/careers/

insert.position = function(pos, time_num_order, time_num_birth = time_num_order, time_num_death = NA, time_order, time_birth = time_order, time_death = NA, type = 1, active = TRUE, price, lot = 0.1, tp = 0, sl = 0, label = "UNNAMED"){
  # This function inserts a row to the end of position data frame (adds a position)
  p = data.frame(
    time.num.order = c(pos$time.num.order, time_num_order),
    time.num.birth = c(pos$time.num.birth, time_num_birth),
    time.num.death = c(pos$time.num.death, time_num_death),
    time.birth = c(pos$time.birth, time_birth),
    time.death = c(pos$time.death, time_death),
    label      = c(as.vector(pos$label), label),
    active     = c(pos$active, active),
    type       = c(pos$type,   type),
    price      = c(pos$price,  price),
    lot        = c(pos$lot,   lot),
    tp         = c(pos$tp,    tp),
    sl         = c(pos$sl,    sl),
    profit     = c(pos$profit, 0.0))
  return(p)
}


insert.history = function(his, time, lots, equity, balance){
  # This function inserts a row to the end of history data frame
  h   = data.frame(time        = c(his$time, time),
                   lots        = c(his$lots, lots),
                   equity      = c(his$equity, equity),
                   balance     = c(his$balance, balance))
  return(h)
}

combine.influenced.positions <- function(inf_pos){
  # Combines and returns a list of all influenced positions
  # The input of this function must be the output of goto(), jump() or jump.next() functions
  return(unique(c(inf_pos$tp.died, inf_pos$sl.died, inf_pos$bl.born, inf_pos$sl.born, inf_pos$bs.born, inf_pos$ss.born, inf_pos$tp.man.died, inf_pos$sl.man.died, inf_pos$bl.man.born, inf_pos$sl.man.born, inf_pos$bs.man.born, inf_pos$ss.man.born)))
}


# Creating a VIRTUAL.TRADER class
VIRTUAL.TRADER <- setRefClass("VIRTUAL.TRADER",
                              fields = list(
                                number.of.intervals = "numeric",
                                current.time.number = "numeric",
                                current.time  = "POSIXt",
                                current.price = "numeric",
                                spread   = "numeric",
                                pip     = "numeric",
                                position = "data.frame",
                                balance  = "numeric",
                                lots     = "numeric",
                                history  = "data.frame",
                                data     = "data.frame"),
                              methods = list(
                                initialize = function(data, spread = 3){
                                  number.of.intervals <<- dim(data)[1]
                                  current.time <<- data$time[1]
                                  current.price <<- data$open[1]
                                  spread <<- spread
                                  pip <<- 0.0001
                                  position <<- data.frame(time.num.order = c(), time.num.birth = c(), time.num.death = c(), time.order = c(), time.birth = c(), time.death = c(), label = c(), active = logical(0), type=c(), price=c(), lot=c(), tp = c(), sl = c(), profit=c())
                                  history <<- data.frame(time = c(0), lots = c(0.0), equity = c(0.0), balance = c(0.0))
                                  data <<- data
                                  balance <<- 0.0
                                  current.time.number <<- 1
                                },
                                reset = function(){
                                  # Resets the virtual trading environment:
                                  # Erases all the positions
                                  # Sets the balance, equity and profit to zero
                                  # goes to the first time interval
                                  keep_spr  = spread
                                  keep_pip = pip
                                  initialize(data)
                                  spread <<- keep_spr
                                  pip   <<- keep_pip
                                },
                                take.buy.limit = function(lot = 0.1, price, tp = 0, sl = 0, label = "UNNAMED", setting = "pips", manual = FALSE, force = FALSE){
                                  assert (lot > 0, "take.buy.limit Error: Negative value chained to argument lot")
                                  prmt = (price < current.price - 10*pip)
                                  if (force & !prmt) {price = current.price - 11*pip
                                  prmt  = TRUE}
                                  if (prmt){
                                    tp.pr = tp.price(price + spread*pip, tp, setting)
                                    sl.pr = sl.price(price + spread*pip, sl, setting)
                                    if (!(is.na(sl.pr) | is.na(tp.pr))){
                                      position <<- insert.position(position, time_num_order = current.time.number, time_num_birth = NA, time_order = current.time, time_birth = NA, type = 2*(manual + 1), label = label, active = FALSE, price = price, lot = lot, tp = tp.pr, sl = sl.pr)
                                    } else {print("take.buy.limit Error: Pending position is not set due to ill tp or sl values")}
                                  } else {print("take.buy.limit Error: Given price for Buy limit must be at least 10 pips below the current price")}
                                },
                                take.buy.stop = function(lot = 0.1, price, tp = 0, sl = 0, label = "UNNAMED", setting = "pips", manual = FALSE, force = FALSE){
                                  assert (lot > 0, "take.buy.stop Error: Negative value chained to argument lot")
                                  prmt = (price > current.price + 10*pip)
                                  if (force & !prmt) {price = current.price + 11*pip
                                  prmt  = TRUE}
                                  if (prmt){
                                    tp.pr = tp.price(price + spread*pip, tp, setting)
                                    sl.pr = sl.price(price + spread*pip, sl, setting)
                                    if (!(is.na(sl.pr) | is.na(tp.pr))){
                                      position <<- insert.position(position, time_num_order = current.time.number, time_num_birth = NA, time_order = current.time, time_birth = NA, type = 3+2*manual, label = label, active = FALSE, price = price, lot = lot, tp = tp.pr, sl = sl.pr)
                                    } else {print("take.buy.stop Error: Pending position is not set due to ill tp or sl values")}
                                  } else {print("take.buy.stop Error: Given price for Buy stop must be at least 10 pips over the current price")}
                                },
                                take.sell.limit = function(lot = 0.1, price, tp = 0, sl = 0, label = "UNNAMED", setting = "pips", manual = FALSE, force = FALSE){
                                  assert (lot > 0, "take.sell.limit Error: Negative value chained to argument lot")
                                  prmt = (price > current.price + 10*pip)
                                  if (force & !prmt) {price = current.price + 11*pip
                                  prmt  = TRUE}
                                  if (prmt){
                                    tp.pr = sl.price(price - spread*pip, tp, setting)
                                    sl.pr = tp.price(price - spread*pip, sl, setting)
                                    if (!(is.na(sl.pr) | is.na(tp.pr))){
                                      position <<- insert.position(position, time_num_order = current.time.number, time_num_birth = NA, time_order = current.time, time_birth = NA, type = -2*(manual+1), label = label, active = FALSE, price = price, lot = lot, tp = tp.pr, sl = sl.pr)
                                    } else {print("take.sell.limit Error: Pending position is not set due to ill tp or sl values")}
                                  } else {print("take.sell.limit Error: Given price for Sell limit must be at least 10 pips over the current price")}
                                },
                                take.sell.stop = function(lot = 0.1, price, tp = 0, sl = 0, label = "UNNAMED", setting = "pips", manual = FALSE, force = FALSE){
                                  assert (lot > 0, "take.sell.stop Error: Negative value chained to argument lot")
                                  prmt = (price < current.price - 10*pip)
                                  if (force & !prmt) {price = current.price - 11*pip
                                  prmt  = TRUE}
                                  if (prmt){
                                    tp.pr = sl.price(price - spread*pip, tp, setting)
                                    sl.pr = tp.price(price - spread*pip, sl, setting)
                                    if (!(is.na(sl.pr) | is.na(tp.pr))){
                                      position <<- insert.position(position, time_num_order = current.time.number, time_num_birth = NA, time_order = current.time, time_birth = NA, type = -3-2*manual, label = label, active = FALSE, price = price, lot = lot, tp = tp.pr, sl = sl.pr)
                                    } else {print("take.sell.stop Error: Pending position is not set due to ill tp or sl values")}
                                  } else {print("take.sell.stop Error: Given price for Sell stop must be at least 10 pips below the current price")}
                                },
                                tp.price = function(price = current.price, tp, settings, manual = FALSE){
                                  # Returns the take profit price for buy according to the settings
                                  # This function is used to set a tp for a buy position or a sl for a sell position
                                  # The output price is always set over the given price.

                                  # if settings = "points" / "pips"
                                  # take profit will be set as tp points/pips over the given price
                                  # stop loss   will be set as sl points/pips below the given price
                                  #
                                  # if settings = "at_price"
                                  # take profit will be set at the given price chained to argument tp.
                                  # In this case the value of argument price is only used to verify that given tp is at least 10 pips over the given price.
                                  # If the given price is not valid, (i.e.: stop loss is higher than the given price in a buy position)
                                  # , the function returns an error.
                                  # it is recommended not to use "at_price" for argument setting
                                  # Value 0 for arguments tp and sl mean there is no take profit or stop loss
                                  # if NA is returned, this means that the given arguments are not appropriate and the position will not be taken if this function returns NA
                                  tp.pr = 0
                                  manual = (tp < 0)
                                  tp = abs(tp)

                                  if (tp !=0){
                                    if (settings == "pips"){
                                      if (tp >= 10) {tp.pr = price + tp*pip}
                                      else {
                                        print("tp.price Error: tp or sl must be at least 10 pips.")
                                        tp.pr = NA
                                      }}
                                    else if (settings == "points"){
                                      if (tp >= 100) {
                                        tp.pr = price + 0.1*tp*pip}
                                      else{
                                        print("tp.price Error: tp or sl must be at least 100 points.")
                                        tp.pr = NA
                                      }}
                                    else if (settings == "at_price"){
                                      if (tp >= price + 10*pip){
                                        tp.pr = tp}
                                      else{
                                        print("tp.price Error: tp for buy or sl for sell must be at least 10 pips higher than the current price")
                                        tp.pr = NA
                                      }
                                    } else {
                                      print("tp.price Error: Given setting is not recognized.")
                                      tp.pr = NA
                                    }
                                  }
                                  return(tp.pr*(-2*manual+1))
                                },

                                sl.price = function(price = current.price, sl, settings){
                                  # This function is used to set a sl for a buy position or a tp for a sell position
                                  # Similar to tp.price but the output price is set below the given price.
                                  manual = (sl < 0)
                                  sl = abs(sl)
                                  sl.pr = 0
                                  if (sl !=0){
                                    if (settings == "pips"){
                                      if (sl >= 10) {
                                        sl.pr = price - sl*pip}
                                      else{
                                        print("sl.price Error: tp/sl must be at least 10 pips.")
                                        sl.pr = NA
                                      }}
                                    else if (settings == "points"){
                                      if (sl >= 100) {
                                        sl.pr = price - 0.1*sl*pip}
                                      else{print("sl.price Error: tp or sl must be at least 100 points.")
                                        sl.pr = NA
                                      }}
                                    else if (settings == "at_price"){
                                      if (sl <= price - 10*pip){
                                        sl.pr = sl}
                                      else {
                                        print("sl.price Error: sl for buy or tp for sell must be at least 10 pips lower than the current price")
                                        sl.pr = NA
                                      }
                                    } else {
                                      print("sl.price Error: Given setting is not recognized.")
                                      sl.pr = NA
                                    }}
                                  return(sl.pr*(-2*manual+1))},

                                take.buy = function(lot = 0.1, tp = 0, sl = 0, label = "UNNAMED", setting = "pips"){
                                  # takes a buy position at the current date-time with given lot, stop loss(sl) and take profit (tp)
                                  #
                                  assert (lot > 0, "take.buy Error: Negative value chained to argument lot")

                                  pr    = data$open[current.time.number] + spread*pip
                                  tp.pr = tp.price(pr, tp, setting = setting)
                                  sl.pr = sl.price(pr, sl, setting = setting)
                                  if (!(is.na(tp.pr) | is.na(sl.pr))){
                                    position <<- insert.position(position, time_num_order = current.time.number, time_order = current.time, label = label, price = pr, lot = lot, tp = tp.pr, sl = sl.pr)
                                    position.update(pos_number = dim(position)[1], pos_price= current.price)
                                  } else {print("take.buy Error: Position is not taken due to ill tp or sl values")}
                                },
                                take.sell = function(lot = 0.1, tp = 0, sl = 0, label = "UNNAMED", setting = "pips"){
                                  # takes a sell position at the current date-time with given lot, stop loss(sl) and take profit (tp)
                                  # settings for tp and sl is the same as function take.buy
                                  assert (lot > 0, "take.sell Error: Negative value chained to argument lot")
                                  pr    = data$open[current.time.number] - spread*pip
                                  tp.pr = sl.price(pr, tp, setting = setting)
                                  sl.pr = tp.price(pr, sl, setting = setting)
                                  if (!(is.na(tp.pr) | is.na(sl.pr))){
                                    position <<- insert.position(position, time_num_order = current.time.number, time_order = current.time, label = label, type = -1, price = pr, lot = lot, tp = tp.pr, sl = sl.pr)
                                    position.update(pos_number = dim(position)[1], pos_price= current.price)
                                  } else {print("take.sell Error: Position is not taken due to ill tp or sl values")}
                                },
                                take.position = function(direction = 1, lot = 0.1, tp = 0, sl = 0, label = "UNNAMED", setting = "pips"){
                                  # takes a position at the current date-time in the given direction with given lot, stop loss(sl) and take profit (tp)
                                  # if direction = 1, a buy is take., if direction = -1 a sell is taken
                                  # settings for tp and sl is the same as function take.buy and take.sell
                                  assert (lot > 0, "take.position Error: Negative value chained to argument lot")
                                  if (direction == 1){
                                    take.buy(lot = lot, tp = tp, sl = sl, label = label, setting = setting)
                                  } else if (direction == -1){
                                    take.sell(lot = lot, tp = tp, sl = sl, label = label, setting = setting)
                                  } else {
                                    print("take.position Error: Direction must be 1 or -1. No position taken.")
                                  }
                                },

                                take.position.array = function(n = NA, gap = 100, direction = 1, lot = 0.1, tp = 0, sl = 0, manual = FALSE, label = "UNNAMED", above = TRUE, take_now = TRUE){
                                  # takes n pending positions at the current date-time in the given direction
                                  # with given lot, stop loss(sl) and take profit (tp)
                                  # above or below the current price (depending on the value of argument "above")
                                  # if direction = 1, buystops/buylimits are taken, if direction = -1 sellstops/selllimits are taken
                                  # tp and sl are in pips, gap in pips is the distance between the positions
                                  # if n = NA, then all the price domain is filled with sequential pending positions
                                  # if argument "take_now" is TRUE, an active position in the current price is also taken with given parameters
                                  cp  = current.price
                                  gp  = gap*pip
                                  if (above){max_n = floor((max(data$high) - cp)/gp)} else {max_n = floor((cp  - min(data$low))/gp)}
                                  if (is.na(n)){
                                    N = max_n
                                  } else{
                                    N = min(n, max_n)
                                  }

                                  for (i in sequence(N)){
                                    if (above){
                                      pr = cp + i*gp
                                      if (direction == 1){
                                        take.buy.stop(price    = pr, tp = tp, sl = sl, lot = lot, manual = manual, label = label)
                                      } else {
                                        take.sell.limit(price  = pr, tp = tp, sl = sl, lot = lot, manual = manual, label = label)
                                      }
                                    } else {
                                      pr = cp - i*gp
                                      if (direction == - 1){
                                        take.sell.stop(price   = pr, tp = tp, sl = sl, lot = lot, manual = manual, label = label)
                                      } else {
                                        take.buy.limit(price   = pr, tp = tp, sl = sl, lot = lot, manual = manual, label = label)
                                      }
                                    }
                                  }
                                  if (take_now){take.position(direction=direction, tp = tp, sl = sl,  lot = lot, label  = label)}
                                },
                                position.update = function(pos_number, pos_price){
                                  if (position$active[pos_number]){
                                    position$profit[pos_number] <<- (pos_price - position$price[pos_number])*(10/pip)*position$type[pos_number]*position$lot[pos_number]
                                  }},
                                position.activate = function(pos_number, birth_time, birth_time_num){

                                  if (position$type[pos_number] %in% c(2,3)){ # Buy Limit or Buy Stop
                                    pr    = position$price[pos_number] + spread*pip
                                    tp.pr = tp.price(price = pr, tp = position$tp[pos_number], setting = "at_price")
                                    sl.pr = sl.price(price = pr, sl = position$sl[pos_number], setting = "at_price")}
                                  else if (position$type[pos_number] %in% c(-2,-3)){ # Sell Limit or Sell Stop
                                    pr    = position$price[pos_number] - spread*pip
                                    tp.pr = sl.price(price = pr, sl = position$tp[pos_number], setting = "at_price")
                                    sl.pr = tp.price(price = pr, tp = position$sl[pos_number], setting = "at_price")}
                                  # For manual pending positions
                                  else if (position$type[pos_number] %in% c(4,5, -4, -5)){   # if Manual pending
                                    flg = (position$type[pos_number] %in% c(4,5))  # flg is 1 if Manual Buy Limit or Buy Stop, flg is 0 if Manual Sell Limit or Sell Stop
                                    pr  = data$open[birth_time_num] + (2*flg-1)*spread*pip

                                    if      (position$tp[pos_number]> 0) {t.p =   pr + position$tp[pos_number] - position$price[pos_number]}
                                    else if (position$tp[pos_number]< 0) {t.p = - pr + position$tp[pos_number] + position$price[pos_number]}
                                    else { t.p = 0} # position$tp[pos_number]== 0

                                    if      (position$sl[pos_number]> 0) {s.l =   pr + position$sl[pos_number] - position$price[pos_number]}
                                    else if (position$sl[pos_number]< 0) {s.l = - pr + position$sl[pos_number] + position$price[pos_number]}
                                    else { s.l = 0}  # position$tp[pos_number]== 0

                                    if (flg){tp.pr = tp.price(price = pr, tp =  t.p, setting = "at_price")
                                    sl.pr = sl.price(price = pr, sl =  s.l, setting = "at_price")}
                                    else    {tp.pr = sl.price(price = pr, sl =  t.p, setting = "at_price")
                                    sl.pr = tp.price(price = pr, tp =  s.l, setting = "at_price")}}
                                  else{
                                    print("position.activate Error: The given order(pos_number) does not belong to a pending position")
                                    return()
                                  }

                                  #                   if      (position$type[pos_number] %in% c(2,3))  { pr = position$price[pos_number] + spread*pip} # Buy Limit or Buy Stop
                                  #                   else if (position$type[pos_number] %in% c(-2,-3)){ pr = position$price[pos_number] - spread*pip} # Sell Limit or Sell Stop
                                  #                   # For manual pending positions
                                  #                   else if (position$type[pos_number] %in% c(4,5))  { pr = data$open[birth_time_num] + spread*pip}  # Manual Buy Limit or Buy Stop
                                  #                   else if (position$type[pos_number] %in% c(-4,-5)){ pr = data$open[birth_time_num] - spread*pip}  # Manual Sell Limit or Sell Stop
                                  #                   else{
                                  #                     print("position.activate Error: The given order(pos_number) does not belong to a pending position")
                                  #                     return()
                                  #                   }

                                  position$type[pos_number] <<- sign(position$type[pos_number])
                                  if (!(is.na(tp.pr) | is.na(sl.pr))){
                                    position$tp[pos_number] <<- tp.pr
                                    position$sl[pos_number] <<- sl.pr

                                    position$active[pos_number] <<- TRUE
                                    position$time.birth[pos_number] <<- birth_time
                                    position$time.num.birth[pos_number] <<- birth_time_num
                                    position$price[pos_number] <<- pr
                                    position.update(pos_number, current.price)
                                  } else{print("position.activate Error: Position is not taken due to ill tp or sl values")}
                                },

                                set.tp = function(pos_number, tp, setting = "pips"){
                                  if (position$type[pos_number] == 1){
                                    tp.pr = tp.price(current.price, tp, setting)
                                    if (!is.na(tp.pr)){
                                      position$tp[pos_number] <<- tp.pr
                                    }
                                  }
                                  else if (position$type[pos_number] == - 1){
                                    tp.pr = sl.price(current.price, tp, setting)
                                    if (!is.na(tp.pr)){
                                      position$tp[pos_number] <<- tp.pr
                                    }
                                  }
                                  else if (position$type[pos_number] %in% c(2,3, 4,5)){
                                    tp.pr = tp.price(position$price[pos_number] + spread*pip, tp, setting)
                                    if (!is.na(tp.pr)){
                                      position$tp[pos_number] <<- tp.pr
                                    }
                                  }
                                  else if (position$type[pos_number] %in% c(-2,-3, -4, -5)){
                                    tp.pr = sl.price(position$price[pos_number] - spread*pip, tp, setting)
                                    if (!is.na(tp.pr)){
                                      position$tp[pos_number] <<- tp.pr
                                    }
                                  } else {print("set.tp Error: Position type unknown !")}
                                },
                                set.sl = function(pos_number, sl, setting = "pips"){
                                  if (position$type[pos_number] == 1){
                                    sl.pr = sl.price(current.price, sl, setting)
                                    if (!is.na(sl.pr)){
                                      position$sl[pos_number] <<- sl.pr
                                    }
                                  }
                                  else if (position$type[pos_number] == - 1){
                                    sl.pr = tp.price(current.price, sl, setting)
                                    if (!is.na(sl.pr)){
                                      position$sl[pos_number] <<- sl.pr
                                    }
                                  }
                                  else if (position$type[pos_number] %in% c(2,3,4,5)){
                                    sl.pr = sl.price(position$price[pos_number] + spread*pip, sl, setting)
                                    if (!is.na(sl.pr)){
                                      position$sl[pos_number] <<- sl.pr
                                    }
                                  }
                                  else if (position$type[pos_number] %in% c(-2,-3,-4,-5)){
                                    sl.pr = tp.price(position$price[pos_number] - spread*pip, sl, setting)
                                    if (!is.na(sl.pr)){
                                      position$sl[pos_number] <<- sl.pr
                                    }
                                  } else {print("set.tp Error: Position type unknown !")}
                                },
                                goto = function(time_number){
                                  # Takes you to the given time.number
                                  # The given time.number must be in the future. (More than the current time.number)
                                  # This function does not respect any pending orders, take profits, stop losses and trailing stops
                                  # Check if the given time.number is greater than the current
                                  influenced.positions = list(tp.died = c(), sl.died = c(), bl.born = c(), sl.born = c(), bs.born = c(), ss.born = c(), tp.man.died = c(), sl.man.died = c(), bl.man.born = c(), sl.man.born = c(), bs.man.born = c(), ss.man.born = c())
                                  if (time_number > number.of.intervals){
                                    print("goto Error: There is no data for the given time interval number.")
                                  } else  if (time_number > current.time.number){
                                    current.price <<- data$open[time_number]
                                    if (dim(position)[1] > 0){  # if there are any positions,
                                      # for active or pending positions:
                                      # live.positions = which((position$active) | (position$type > 1) | (position$type < -1))
                                      pp = pending.positions()

                                      for (i in pp){  # Among pending positions (inactive and live),
                                        if (position$type[i] %in% c(2,-3)){ # For Buy limit or Sell stop pending positions:
                                          j = first.touch.low(current.time.number,time_number, position$price[i])
                                          if (!is.na(j)) {
                                            if (position$type[i] == 2){influenced.positions$bl.born = c(influenced.positions$bl.born, i)}
                                            else {influenced.positions$ss.born = c(influenced.positions$ss.born, i)}
                                            position.activate(i, birth_time = data$time[j], birth_time_num = j) # Activate if Buy limit or Sell stop price is touched
                                          }}
                                        else if (position$type[i] %in% c(-2, 3)) {  # For Sell limit or Buy stop pending positions:
                                          j = first.touch.high(current.time.number,time_number, position$price[i])
                                          if (!is.na(j)) {
                                            if (position$type[i] == -2){influenced.positions$sl.born = c(influenced.positions$sl.born, i)}
                                            else {influenced.positions$bs.born = c(influenced.positions$bs.born, i)}
                                            position.activate(i, birth_time = data$time[j], birth_time_num = j) # Activate if Sell limit or Buy stop price is touched
                                          }}
                                        else if (position$type[i] %in% c(4,-5)){ # For manual Buy limit or Sell stop pending positions:
                                          j = first.touch.low.manual(current.time.number, time_number, position$price[i])
                                          if (!is.na(j)) {
                                            if (position$type[i] == 4){influenced.positions$bl.man.born = c(influenced.positions$bl.man.born, i)}
                                            else {influenced.positions$ss.man.born = c(influenced.positions$ss.man.born, i)}
                                            position.activate(i, birth_time = data$time[j], birth_time_num = j) # Activate if Buy limit or Sell stop price is touched
                                          }}
                                        else if (position$type[i] %in% c(-4,5)){  # For manual Sell limit or Buy stop pending positions:
                                          j = first.touch.high.manual(current.time.number,time_number, position$price[i])
                                          if (!is.na(j)) {
                                            if (position$type[i] == -4){influenced.positions$sl.man.born = c(influenced.positions$sl.man.born, i)}
                                            else {influenced.positions$bs.man.born = c(influenced.positions$bs.man.born, i)}
                                            position.activate(i, birth_time = data$time[j], birth_time_num = j) # Activate if Sell limit or Buy stop price is touched
                                          }}}

                                      ap = active.positions()

                                      for (i in ap){ # Among all active positions,
                                        if (position$sl[i] > 0){# For positions with sl:
                                          if (position$type[i] == 1){ # if Buy
                                            flg = data$open[position$time.num.order[i]] + spread*pip < position$price[i] # if flg is TRUE then it was a buy stop with sl which is recently activated
                                            j = first.touch.low(max(current.time.number,  position$time.num.birth[i]+flg), time_number, position$sl[i])}
                                          else {                      # if sell
                                            flg = data$open[position$time.num.order[i]] - spread*pip > position$price[i] # if flg is TRUE then it was a sell stop with sl which is recently activated
                                            j = first.touch.high(max(current.time.number, position$time.num.birth[i]+flg), time_number, position$sl[i])}
                                          if (!is.na(j)) {
                                            close.by(i, close_price = position$sl[i], death_time = data$time[j], death_time_num = j)  # Close at sl if sl is touched
                                            influenced.positions$sl.died = c(influenced.positions$sl.died, i)}
                                          else {position.update(i, current.price)}}

                                        else if (position$sl[i] < 0){# For positions with manual sl:
                                          if (position$type[i] == 1){ # if Buy
                                            j = first.touch.low.manual(max(current.time.number, position$time.num.birth[i]+1), time_number, - position$sl[i])}
                                          else {# if Sell
                                            j = first.touch.high.manual(max(current.time.number, position$time.num.birth[i]+1), time_number, - position$sl[i])}
                                          if (!is.na(j)) {
                                            close.by(i, close_price = data$open[j], death_time = data$time[j], death_time_num = j)  # Close at open price if sl is touched yesterday (the interval before)
                                            influenced.positions$sl.man.died = c(influenced.positions$sl.man.died, i)}
                                          else {position.update(i, current.price)}}
                                        else{  # if sl == 0
                                          if (position$tp[i] == 0){position.update(i, current.price)}}# If no tp or sl

                                        if (position$tp[i] > 0){ # For positions with tp:
                                          if (position$type[i] == 1){ # if Buy
                                            flg = data$open[position$time.num.order[i]] + spread*pip > position$price[i] # if flg is TRUE then it was a buy limit which is recently activated and has take profit
                                            j = first.touch.high(max(current.time.number, position$time.num.birth[i]+flg), time_number, position$tp[i])}
                                          else {                      # if sell
                                            flg = data$open[position$time.num.order[i]] - spread*pip < position$price[i] # if flg is TRUE then it was a sell limit which is recently activated
                                            j = first.touch.low(max(current.time.number,  position$time.num.birth[i]+flg),time_number, position$tp[i])}
                                          if (!is.na(j)) {
                                            close.by(i, close_price = position$tp[i], death_time = data$time[j], death_time_num = j) # Close at tp if tp is touched
                                            influenced.positions$tp.died = c(influenced.positions$tp.died, i)}
                                          else {position.update(i, current.price)}}

                                        else if (position$tp[i] < 0){ # For positions with manual tp:
                                          if (position$type[i] == 1){ # if Buy
                                            j = first.touch.high.manual(max(current.time.number, position$time.num.birth[i]+1), time_number, - position$tp[i])}
                                          else {                      # if sell
                                            j = first.touch.low.manual(max(current.time.number, position$time.num.birth[i]+1), time_number, - position$tp[i])}
                                          if (!is.na(j)) {
                                            close.by(i, close_price = data$open[j], death_time = data$time[j], death_time_num = j)
                                            influenced.positions$tp.man.died = c(influenced.positions$tp.man.died, i)}
                                          else {position.update(i, current.price)}}}}

                                    history <<- insert.history(history, time = current.time, lots = position.balance(), equity = equity(), balance = balance)
                                    current.time.number <<- time_number
                                    current.time <<- data$time[time_number]}
                                  else {print("goto Error: Nobody can return back to the past! Please specify a time in the future.")}
                                  return(influenced.positions)
                                },

                                jump = function(number.of.intervals = 1){
                                  # jumps to the next <<number.of.intervals>> interval
                                  goto(current.time.number + number.of.intervals)
                                },

                                close.by = function(order, close_price, death_time, death_time_num){
                                  # Input: order (A vector of integers containing the indices(orders) of the positions to be closed)
                                  # Closes the positions with the given order (index) at price close_price and time death_time
                                  if (position$active[order]){
                                    position.update(order, close_price)
                                    balance <<- balance + sum(position$profit[order])
                                    position$active[order] <<- FALSE  # The positions become inactive
                                    position$time.death[order] <<- death_time
                                    position$time.num.death[order] <<- death_time_num
                                  }
                                },

                                close = function(orders, labels = all_labels()){
                                  # Input: order (A vector of integers containing the indices(orders) of the positions to be closed)
                                  # Closes the positions with the given order (index)
                                  if (length(orders)>0){
                                    ord = orders[position$active[orders] & (position$label[orders] %in% labels)]
                                    balance <<- balance + sum(position$profit[ord])
                                    position$active[ord] <<- FALSE  # The positions become inactive
                                    position$time.death[ord] <<- current.time
                                    position$time.num.death[ord] <<- current.time.number
                                  }
                                },

                                close.all = function(labels = all_labels()){
                                  # Closes all active positions
                                  close(which(position$active), labels = labels)
                                },

                                # Accessor functions:
                                profit = function(labels = all_labels()) {
                                  indx = which(position$label %in% labels)
                                  return(sum(position$active[indx]*position$profit[indx]))
                                },
                                equity = function(labels = all_labels()) {
                                  eqt = sum(position$profit[position$label %in% labels])
                                  # assert(equal(balance + profit(), eqt), "Something Goes Wrong in Calculating Balance")
                                  return(eqt)
                                },

                                current.moving.average = function(ma_weight = 24){
                                  assert(current.time.number > ma_weight,"current.moving.average Error: Current Time Number is not greater than given ma_weight")
                                  return(mean(data$open[(current.time.number - ma_weight + 1):current.time.number]))
                                },

                                all_labels = function(){
                                  # returns the list of all position labels
                                  return(unique(position$label))
                                },

                                positive.positions = function(min_profit = 0, labels = all_labels()){
                                  # Returns the index of all active positions with a profit higher than min.profit
                                  return(which(position$active & (position$profit > min_profit) & (position$label %in% labels)))
                                },

                                position.peak.profit = function(maximum = TRUE, labels = all_labels()){
                                  # returns the index of the active position with maximum or minimum profit
                                  active.indices    = which(position$active & (position$label %in% labels))
                                  active.profits    = position$profit[active.indices]
                                  index.peak.profit = order(active.profits, decreasing = maximum)[1]
                                  return(active.indices[index.peak.profit])
                                },

                                peak.profitable.buys = function(n, maximum = TRUE, labels = all_labels()){
                                  # Returns the indexes of n maximum or minimum profitable buy positions
                                  # The output is an integer vector of n elements containing the
                                  # indexes of n buy positions with highest or lowest profit
                                  # if less then n buy positions exist, all buys are returned
                                  # if maximum = FALSE then the least profitable positions are returned
                                  all.buys   = which(position$active & (position$type == 1) & (position$label %in% labels))
                                  peak.buys  = all.buys[order(position$profit[all.buys], decreasing = maximum)[1: min(n, length(all.buys))]]
                                  return(peak.buys)
                                },

                                peak.profitable.sells = function(n, maximum = TRUE, labels = all_labels()){
                                  # Returns the indexes of n maximum or minimum profitable sell positions
                                  # The output is an integer vector of n elements containing the
                                  # indexes of n sell positions with highest or lowest profit
                                  # if less then n sell positions exist, all sells are returned
                                  # if maximum = FALSE then the least profitable positions are returned
                                  all.sells   = which(position$active & (position$type == -1) & (position$label %in% labels))
                                  peak.sells  = all.sells[order(position$profit[all.sells], decreasing = maximum)[1: min(n, length(all.sells))]]
                                  return(peak.sells)
                                },

                                buy.positions = function(labels = all_labels()){
                                  # Returns the indexes of all active buy positions
                                  return(which(position$active & (position$type == 1) & (position$label %in% labels)))
                                },

                                sell.positions = function(labels = all_labels()){
                                  # Returns the indexes of all active sell positions
                                  return(which(position$active & (position$type == - 1) & (position$label %in% labels)))
                                },

                                position.balance = function(labels = all_labels()){
                                  # returns the total balance of Buy and Sell positions
                                  if (dim(position)[1]==0) {
                                    return(0)
                                  } else {
                                    active.indices = which(position$active & (position$label %in% labels))
                                    active.lots   = position$type[active.indices]*position$lot[active.indices]
                                    return(sum(active.lots))}
                                },
                                no.active.position = function(labels = all_labels()){
                                  # returns TRUE if all the positions are inactive
                                  return(sum(position$active[position$label %in% labels]) == 0)
                                },
                                pending.positions = function(labels = all_labels()){
                                  labeled = position$label %in% labels
                                  type_ok = (position$type > 1) | (position$type < -1)
                                  pp = which(labeled & type_ok)
                                  return(pp)
                                },
                                active.positions = function(labels = all_labels()){
                                  ap = which(position$active & (position$label %in% labels))
                                  return(ap)
                                },

                                position.age = function(pos_number){
                                  # returns the age of given pos_number if all the positions are inactive
                                  return(current.time.number - position$time.num.birth[pos_number])
                                },

                                first.touch.high = function(from_time_number, until_time_number, touch_price){
                                  # starting from from_time upto the until_time
                                  # searches to see when the high price comes over the touch price
                                  # and returns the time number when the first touch occures
                                  i = from_time_number
                                  while ((data$high[i] < touch_price) & (i < until_time_number - 1)){i = i + 1}
                                  if (data$high[i] < touch_price){return(NA)} else {return(i)}
                                },
                                first.touch.low = function(from_time_number, until_time_number, touch_price){
                                  # starting from from_time upto the until_time
                                  # searches to see when the low price comes below the touch price
                                  # and returns the time number when the first touch occures
                                  i = from_time_number
                                  while ((data$low[i] > touch_price) & (i < until_time_number - 1)) {i = i + 1}
                                  if (data$low[i] > touch_price) {return(NA)} else {return(i)}
                                },

                                first.touch.high.manual = function(from_time_number, until_time_number, touch_price){
                                  # starting from from_time upto the until_time
                                  #     searches to see when the open price comes over the touch price
                                  #     and returns the time number when the first touch occures
                                  i = from_time_number
                                  while ((data$open[i] < touch_price) & (i < until_time_number)) {i = i + 1}
                                  if (data$open[i] < touch_price){return(NA)} else {return(i)}
                                },

                                first.touch.low.manual = function(from_time_number, until_time_number, touch_price){
                                  # starting from from_time upto the until_time
                                  #     searches to see when the open price comes below the touch price
                                  #     and returns the time number when the first touch occures
                                  i = from_time_number
                                  while ((data$open[i] > touch_price) & (i < until_time_number)) {i = i + 1}
                                  if (data$open[i] > touch_price) {return(NA)} else {return(i)}
                                },

                                jump.next = function(until_time_number = number.of.intervals, events = c("TP", "SL", "TP_MAN", "SL_MAN", "PEND", "PEND_MAN")){
                                  #jumps to the next event when the first sl/tp/bl/bs/sl/ss price is touched
                                  if (until_time_number > number.of.intervals) {
                                    print("goto.next Error: until_time_number can not be greater than the number of intervals")
                                    return()}
                                  if (dim(position)[1] == 0){
                                    goto(until_time_number)
                                    return()
                                  }
                                  if ("TP" %in% events){
                                    pos.with.tp        = which((position$active) & (position$tp > 0))
                                    tp.prices = position$tp[pos.with.tp]
                                  } else {tp.prices = c()}

                                  if ("TP_MAN" %in% events){
                                    pos.with.tp.manual = which((position$active) & (position$tp < 0))
                                    tp.manual.prices   = - position$tp[pos.with.tp.manual]
                                  } else {tp.manual.prices = c()}

                                  if ("SL" %in% events){
                                    pos.with.sl        = which((position$active) & (position$sl > 0))
                                    sl.prices = position$sl[pos.with.sl]
                                  } else {sl.prices = c()}

                                  if ("SL_MAN" %in% events){
                                    pos.with.sl.manual = which((position$active) & (position$sl < 0))
                                    sl.manual.prices   = - position$sl[pos.with.sl.manual]
                                  } else {sl.manual.prices = c()}

                                  if ("PEND" %in% events){
                                    pos.pend = which((position$type %in% c(2,-2,3,-3)))
                                    pos.pend.prices = position$price[pos.pend]
                                  } else {pos.pend.prices = c()}

                                  if ("PEND_MAN" %in% events){
                                    pos.pend.manual     = which((position$type %in% c(4,-4,5,-5)))
                                    pos.pend.man.prices = position$price[pos.pend.manual]
                                  } else {pos.pend.man.prices = c()}

                                  key.prices         = c(tp.prices, sl.prices, pos.pend.prices)
                                  key.prices.manual  = c(tp.manual.prices, sl.manual.prices, pos.pend.man.prices)

                                  over.current         = key.prices[which(key.prices > current.price)]
                                  below.current        = key.prices[which(key.prices < current.price)]
                                  over.current.manual  = key.prices.manual[which(key.prices.manual > current.price)]
                                  below.current.manual = key.prices.manual[which(key.prices.manual < current.price)]

                                  if (length(over.current) == 0){time.over = NA}
                                  else {time.over  = first.touch.high(current.time.number, until_time_number, min(over.current))}

                                  if (length(over.current.manual) == 0){time.over.manual = NA}
                                  else {time.over.manual  = first.touch.high.manual(current.time.number, until_time_number, min(over.current.manual))}

                                  if (length(below.current) == 0){time.below = NA}
                                  else {time.below = first.touch.low(current.time.number, until_time_number, max(below.current))}

                                  if (length(below.current.manual) == 0){time.below.manual = NA}
                                  else {time.below.manual = first.touch.low.manual(current.time.number, until_time_number, max(below.current.manual))}
                                  key.times = c()
                                  if (!is.na(time.over))         {key.times = c(key.times, time.over + 1)}
                                  if (!is.na(time.below))        {key.times = c(key.times, time.below + 1)}
                                  if (!is.na(time.over.manual))  {key.times = c(key.times, time.over.manual)}
                                  if (!is.na(time.below.manual)) {key.times = c(key.times, time.below.manual)}
                                  if (length(key.times) == 0) {return(goto(until_time_number))}
                                  else                        {return(goto(min(key.times)))}}
                              ))

# Generic Functions
print.VIRTUAL.TRADER = function(obj){
  s = paste("\n",
            "Current Time Number:", obj$current.time.number, "\n",
            "Current Equity     :", obj$equity(), "\n",
            "Current Balance    :", obj$balance, "\n",
            "Current lots       :", obj$position.balance(), "\n")
  cat(s)
}

plot.VIRTUAL.TRADER = function(obj, plot_balance = FALSE, plot_lots = FALSE){
  plot.new()
  n = 1
  if (plot_balance){n = n + 1}
  if (plot_lots){n = n + 1}
  if (n > 1){par(mfrow=c(n,1))}
  plot(obj$history$equity,  main="Equity",  type = "l")
  if (plot_balance){
    plot(obj$history$balance, main="Balance", type = "l")
  }
  if (plot_lots){
    plot(obj$history$lots, main="Lots", type = "l")
  }
}
