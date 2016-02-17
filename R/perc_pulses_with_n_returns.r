perc_pulses_with_n_returns = function(ReturnNumber, pulseID)
{
  dt = data.table(ReturnNumber, pulseID)
  pulses = dt[, list(nbReturn=max(ReturnNumber)), by=list(pulseID)]
  t = table(factor(pulses$nbReturn, levels=1:5))
  return(t)
}