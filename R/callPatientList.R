#' callPatientList Function
#'
#' This function generates a daily list of patient requests for a simulation. The
#' list contains patient objects with defined characteristics and is returned 
#' when the function is called. 
#' @param day is the day in the patient data frame from which to extract the patient requests
#' @keywords patientData is the data frame containing the patient data from which to extract demand
#' @export
#' @examples
#' callPatientList(day=1,patientData)
#' Data Structure:
#' 
#' |  day  |  type  |  team  |  provider  |
#'    1       acute      1         2
#'    1       routine    1         1...
callPatientList <- function(dayRequest,patientData) {
      requests <- subset(patientData,patientData$day == dayRequest)
      patientList <- vector("list",nrow(requests))
      for(i in 1:nrow(requests)) {
            patientList[[i]] <- patient(requests[i,2],requests[i,4],requests[i,3])
      }
      patientList
}