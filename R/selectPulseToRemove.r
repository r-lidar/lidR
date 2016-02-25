.selectPulseToRemove = function(pulseID, n)
{
  p = unique(pulseID)

  if(n > length(p))
    return(rep(TRUE, length(pulseID)))

  selectedPulses = sample(p, n)
  selectedPulses = pulseID %in% selectedPulses

  return(selectedPulses)
}