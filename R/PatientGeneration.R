#' PatientGeneration Function
#'
#' This function generates a daily list of patient requests for a simulation. The
#' list contains patient objects with defined characteristics and is returned 
#' when the function is called. 
#' @param lamdba the rate provided to the poisson random number generator
#' @param teams defines the number of teams in the clinic
#' @param provPerTeam defines the number of providers on each team
#' @param apptRatio is a named vector with correct element names with the proportion of each appt type. Ex: c(0.3,0.5,0.2)
#' @keywords PatientGeneration, patientList, patients
#' @export
#' @examples
#' PatientGeneration(lambda,teams,provPerTeam,apptRatio,seed=5)
#' 
PatientGeneration <- function(lambda,teams,provPerTeam,apptRatio,seed=5) {
      set.seed(seed)
      apptTypes <- names(apptRatio)
      PatientList <- vector("list",rpois(1,lambda))
      PatientList<- lapply (1 : length(PatientList), function(x) patient(sample(apptTypes,size=1,prob=apptRatio),sample(provPerTeam,1),sample(teams,1)))
      PatientList
}