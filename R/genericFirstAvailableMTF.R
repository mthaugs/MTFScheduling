#' genericFirstAvailableMTF Function
#'
#' This function schedules a patient into the first available appointment
#' beginning on the specified day. It moves through the hierarchy of the assigned
#' provider, to the assigned care team, and then to the full clinic if cross-booking
#' is authorized. The function returns a vector (list) with the position of the 
#' appt to book and whether access and continuity were met. The default is that 
#' access was not met since this function is called when an appt in the access 
#' standard is not available.
#' @param schedule provides the schedule (list type object) to search for an open appt
#' @param patient provides the patient class with the associated attributes
#' @param Day identifies the day in the schedule to begin searching for an appt
#' @param crossbook indicates if crossbooking to the full clinic is authorized. Default is TRUE.
#' @keywords firstAvailable, scheduling
#' @export
#' @examples
#' genericFirstAvailable(schedule,patient,Day=5,crossbook=TRUE)

#when an appointment within the access standard for the non-acute appt request
#cannot be made with the main scheduling algorithm, 

genericFirstAvailableMTF <- function(schedule,patient,Day,crossbook=TRUE) {
      
      #extract patient characteristics
      type = patient$getType()
      team = patient$getTeam()
      provider = patient$getProvider()
      
      #set function day to the Day provided
      day <- Day
      
      #dummy to indicate if appt was booked
      booked <- FALSE
      
      #trackers for continuity
      provPOP <- FALSE
      teamPOP <- FALSE
      
      #tracker for access -> set to true since this is used when the algorithms
      #cannot book within the standard
      POP <- TRUE
      
      #points to the correct schedule provider column for acute or non-acute
      if(type=="acute") {
            col <- 1
      } else if(type=="routine") {
            col <- 2
      } else if(type=="well") {
            col <- 3
      } else {
            message("error in appt type provided for schedule col")
      }
      
      while(booked!=TRUE) {
            if(schedule[[day]][[team]][provider,col]!=0) {
                  #only use non-acute appointments since acutes are not booked
                  #outside the standard
                  location <- c(day,team,provider,col)
                  booked <- TRUE
            }
            
            #exit loop if an appointment was found
            if(booked == TRUE) { 
                  break
            } else {
                  #check if an appt on the first day out of the std is available on
                  #the assigned care team
                  for(i in 1:(nrow(schedule[[day]][[team]])-1)) {
                        #create a randomized order to check providers on the team
                        #used to prevent bias when choosing another provider on team
                        providerTeam <- sample(nrow(schedule[[day]][[team]]),nrow(schedule[[day]][[team]]))
                        providerTeam <- setdiff(providerTeam,provider) #remove assigned PCM
                        if(schedule[[day]][[team]][providerTeam[i],col] != 0) {
                              location <- c(day,team,providerTeam[i],col)
                              booked <- TRUE
                              provPOP <- TRUE
                        } 
                  }
            }
            
            if(booked == TRUE && crossbook == TRUE) {
                  break
            } else {
                  for(j in 1:(length(schedule[[day]])-1)) {
                        Team <- sample(length(schedule[[day]]),length(schedule[[day]]))
                        Team <- setdiff(Team,team) #remove assigned team (done prior)
                        #create a randomized order to check providers on the team
                        #used to prevent bias when choosing another provider on team
                        for(i in 1:(nrow(schedule[[day]][[team]])-1)) {
                              providerTeam <- sample(nrow(schedule[[day]][[Team[j]]]),nrow(schedule[[day]][[Team[j]]]))
                              if(schedule[[day]][[Team[j]]][providerTeam[i],col]!=0) {
                                    location <- c(day,Team[j],providerTeam[i],col)
                                    booked <- TRUE
                                    teamPOP <- TRUE
                              } 
                        }
                  }
            }
            if(booked != TRUE) {
                  day <- day + 1
            }
            
      }
      returnFunc <- list(location, POP, provPOP, teamPOP)  
      returnFunc
}