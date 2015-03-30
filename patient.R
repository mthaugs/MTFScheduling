patient <- function(type="acute",provider,team) {
      getType <- function() {type}
      getTeam <- function() {team}
      getProvider <- function() {provider}
      list(getType=getType,getTeam=getTeam,getProvider=getProvider)
}