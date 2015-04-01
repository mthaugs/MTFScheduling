#' MTFScheduling Function
#'
#' This function schedules a patient into the first available triaged appointment
#' beginning on the specified day . It moves through the hierarchy of the assigned
#' provider, to the assigned care team, and then to the full clinic if cross-booking
#' is authorized. The function returns a vector (list) with the position of the 
#' appt to book and whether access and continuity were met. 
#' @param schedule provides the schedule (list type object) to search for an open appt
#' @param patient provides the patient class with the associated attributes
#' @param Day identifies the day in the schedule to begin searching for an appt
#' @param crossbook indicates if crossbooking to the full clinic is authorized. Default is TRUE.
#' @keywords firstAvailable, scheduling, non-Acute, routine, well
#' @export
#' @examples
#' MTFScheduling(schedule,patient,Day=5,crossbook=TRUE)
#' 
#Algorithm to assign patient to the first available appt within the access std
#See README for algorithm flow chart
MTFScheduling <- function(schedule,patient,Day,crossbook=TRUE) {

      location <- c(0,0,0,0)
      #function returns a list with:
      #1. schedule
      #2. POP (if access was met)
      #3. provPOP (provider continuity)
      #4. teamPOP (team continuity)
      
      #set function day to the Day provided
      day<-Day
      
      #dummy to indicate if appt was booked
      booked <- FALSE
      
      #trackers for continuity
      provPOP <- FALSE
      teamPOP <- FALSE
      
      #trackers for access
      POP <- FALSE
      
      #extract patient characteristics
      type = patient$getType()
      team = patient$getTeam()
      provider = patient$getProvider()
      
      #find number of days in the appt access horizon by type
      Horizon = horizon(type,Day)
      
      #return the last schedule day in the schedule that meets the access std
      accessStandard = day+Horizon
      
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
      
      #iterates until appt is booked w/ scheduled provider or out of access std
      while(booked!=TRUE) {
            #determine if appt is available with assigned [provider,] on [[day]]
            if(schedule[[day]][[team]][provider,col]!=0) {
                  location <- c(day,team,provider,col) 
                  booked <- TRUE
                  #if acute & no acute appts available -> check appt pool for avail   
            } else {
                  day <- day + 1
            }
            
            #if the next[[day]] is outside the appt access std -> continue to 
            #check the team avail and note the loss of [provider,] continuity
            if(day > accessStandard) {
                  provPOP <- TRUE
                  break
            }
      }
      
      #reset day to check team availability
      day <- Day
      
      #iterates until appt is booked in team or out of access std
      while(booked!=TRUE) {
            #iterate through providers on team to try and find an appointment
            for(i in 1:(nrow(schedule[[day]][[team]])-1)) {
                  #create a randomized order to check providers on the team
                  #used to prevent bias when choosing another provider on team
                  #set.seed(i)
                  providerTeam <- sample(nrow(schedule[[day]][[team]]))
                  providerTeam <- setdiff(providerTeam,provider) #remove assigned PCM
                  #if a [provider,] on [[team]] available within the access std
                  #-> book with that provider
                  if(schedule[[day]][[team]][providerTeam[i],col] != 0) {
                        location <- c(day,team,providerTeam[i],col) 
                        booked <- TRUE
                        #check the full appt pool after checking acute appts when 
                        #requested appt is acute
                  } 
            }
            #check if an appt was found on [[day]] with a provider in [[team]]
            if(booked!=TRUE) {
                  day <- day + 1
            }
            
            #if an appt in [[team]] w/in access std not avail, pop to clinic if 
            #authorized
            if(day > accessStandard && crossbook==TRUE) {
                  teamPOP <- TRUE
                  break
                  #if out of access std and not allowed to crossbook -> take action 
                  #depending on if acute or non-acute
            } else if (day > accessStandard && crossbook==FALSE) {
                  if(type=="acute") {
                        booked <- TRUE
                        POP <- TRUE
                        teamPOP <- TRUE
                  }
                  break
            } 
      }
      
      #reset the day to check full clinic
      day <- Day
      
      #iterates until appt is booked in clinic or out of access std
      #iterate through providers in clinic to find an appt
      while(booked!=TRUE && crossbook == TRUE) {
            for(j in 1:(length(schedule[[day]])-1)) {
                  Team <- sample(length(schedule[[day]]))
                  Team <- setdiff(Team,team) #remove assigned team (done prior)
                  #create a randomized order to check providers on the team
                  #used to prevent bias when choosing another provider on team
                  for(i in 1:(nrow(schedule[[day]][[team]])-1)) {
                        providerTeam <- sample(nrow(schedule[[day]][[Team[j]]]))
                        if(schedule[[day]][[Team[j]]][providerTeam[i],col]!=0) {
                              location <- c(day,Team[j],providerTeam[i],col) 
                              booked <- TRUE
                        } 
                  }
            }
            
            #check if an appt was found on [[day]]
            if(booked!=TRUE) {
                  day <- day + 1
            }
            
            #if an appt w/in access std not avail, do appropriate action by type
            if (day > accessStandard) {
                  POP <- TRUE
                  if(type=="acute") {
                        booked <- TRUE
                  }
                  break
            } 
      }
      
      #check if appt was booked after checking. For non-acute appts that could 
      #not be booked within the access standard, use the genericFirstAvailable
      #algorithm to book the patient
      returnFunc <- list(location, POP, provPOP, teamPOP) 
      if(booked != TRUE) {
            returnFunc <- genericFirstAvailableMTF(schedule,patient,(accessStandard+1),crossbook)
      }
      
      returnFunc
      #function returns a list with:
      #1. schedule pointer
      #2. POP (if access was met)
      #3. provPOP (provider continuity)
      #4. teamPOP (team continuity)
}



