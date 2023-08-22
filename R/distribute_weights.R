## function that distribute the weights given a vector of bete
## see equation (7) in the main manuscipt
distribute_weights = function(beta,Delta_S)
{
  return(exp(Delta_S*beta)/sum(exp(Delta_S*beta)))
}
