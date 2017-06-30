calcTradeDay = function(year) {
  # Trading day calculation
  # 
  # @param: year
  # @return: number of trading day and the date
  #
  firstDay = as.Date(paste(year, '-01-01', sep = ''))
  lastDay  = as.Date(paste(year, '-12-31', sep = ''))
  allDay   = seq.Date(from = firstDay, to = lastDay, by = 'day')
  numDay   = ifelse(year %% 4 == 0, 366, 365)
  weekends = sum(weekdays(allDay) %in% c('Saturday', 'Sunday'))
  weekday_ = weekdays(allDay) %notin% c('Saturday', 'Sunday')
  weekday_ = allDay[weekday_]
  tradeDay = numDay - weekends
  return(list(numOfTrDay = tradeDay, weekdays_  = weekday_))
}

year = 2000:2016
sapply(year, calcTradeDay)

?Negate  # functional programming function
'%notin%' = Negate('%in%')
