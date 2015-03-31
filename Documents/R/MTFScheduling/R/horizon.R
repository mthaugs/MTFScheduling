#' horizon Function
#'
#' Returns the number of schedule days of the access standard for the appt type
#' @param type provides the appointment type to base the horizon on.
#' @param day identifies the day in the schedule to count the horizon from.
#' @keywords horizon, accessStandard
#' @export
#' @examples
#' horizon(type="routine", day = 10)
#' 

#returns the number of schedule days of the access standard for the appt type
horizon <- function(type,day) {
      friday <- ((day %% 5)==0)
      
      if(type=="acute" && friday) {
            0
      } else if( type == "acute" && !friday) {
            1
      } else if(type=="routine") {
            5
      } else if(type=="well") {
            20
      } else {
            message("error in appt type provided")
      }
}