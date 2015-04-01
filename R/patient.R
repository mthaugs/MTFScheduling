#' Patient Function
#'
#' This function allows you to create a Patient object.
#' @param type defines the type of appt requested. Defaults to acute.
#' @param provider defines the provider ID within the care team to which the patient is assigned
#' @param team defines the team ID of the team to which the patient is assigned
#' @keywords patient
#' @export
#' @examples
#' patient(type="routine", provider=2, team=1)
patient <- function(type="acute",provider,team) {
      getType <- function() {type}
      getTeam <- function() {team}
      getProvider <- function() {provider}
      list(getType=getType,getTeam=getTeam,getProvider=getProvider)
}