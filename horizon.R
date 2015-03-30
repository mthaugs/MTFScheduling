#returns the number of schedule days of the access standard for the appt type
horizon <- function(type) {
      if(type=="acute") {
            1
      } else if(type=="routine") {
            5
      } else if(type=="well") {
            20
      } else {
            message("error in appt type provided")
      }
}