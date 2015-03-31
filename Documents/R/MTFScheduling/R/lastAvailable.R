#' lastAvailable Function
#'
#' This function schedules a patient into the last available appointment
#' beginning on the specified day through the appt's scheduling horizon. 
#' It moves through the hierarchy of the assigned provider, to the assigned care 
#' team, and then to the full clinic if cross-booking is authorized. 
#' The function returns a vector (list) with the position of the 
#' appt to book and whether access and continuity were met. 
#' @param schedule provides the schedule (list type object) to search for an open appt
#' @param patient provides the patient class with the associated attributes
#' @param Day identifies the day in the schedule to begin searching for an appt
#' @param crossbook indicates if crossbooking to the full clinic is authorized. Default is TRUE.
#' @keywords lastAvailable, scheduling, non-Acute, routine, well
#' @export
#' @examples
#' lastAvailable(schedule,patient,Day=5,crossbook=TRUE)
#' 

#Algorithm to assign patient to the last available appt within the access std
#This does not apply for acute appts -> acute always first available
#source firstAvailable function for acute appts in simulation library
#See README for algorithm flow chart
lastAvailable <- function(schedule,patient,Day,crossbook=TRUE) {
      location <- c(0,0,0,0)
      
      #function returns a list with:
      #1. schedule
      #2. POP (if access was met)
      #3. provPOP (provider continuity)
      #4. teamPOP (team continuity)
      
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
      accessStandard = Day+Horizon
      
      #set function day to the last day that can be booked
      day<-accessStandard
      
      #always use firstAvailable on acute appts
#       if(type == "acute") {
#             returnFunc <- firstAvailable(schedule,patient,Day,crossbook)
#             booked <- TRUE
#       }
      
      #iterates until appt is booked w/ scheduled provider or out of access std
      while(booked!=TRUE) {
            #determine if appt avail with assigned [provider,] on [[day]]
            if(schedule[[day]][[team]][provider,2]!=0) {
                  location <- c(day,team,provider,2)
                  booked <- TRUE
            #if no appts available, iterate one day closer
            } else {
                  day <- day - 1
            }
            #if no appts available within the total access std -> iterate
            #to check the provider team for availability
            if(day < Day) {
                  provPOP <- TRUE
                  break
            }
      }
      
      #reset the day
      day <- accessStandard
      
      #iterates until appt is booked in team or unavailable in access std
      while(booked!=TRUE) {
            #iterate through providers on team to try and find an appointment
            for(i in 1:(nrow(schedule[[day]][[team]])-1)) {
                  #create a randomized order to check providers on the team
                  #used to prevent bias when choosing another provider on team
                  set.seed(i)
                  providerTeam <- sample(nrow(schedule[[day]][[team]]))
                  providerTeam <- setdiff(providerTeam,provider) #remove assigned PCM
                  #if a [provider,] on [[team]] available within the access std
                  #-> book with that provider
                  
                  if(schedule[[day]][[team]][providerTeam[i],2]!=0) {
                        location <- c(day,team,providerTeam[i],2) 
                        booked <- TRUE
                  } 
            }
            #check if an appt was found on [[day]] with a provider in [[team]]
            if(booked!=TRUE) {
                  day <- day - 1
            }
            
            #if an appt in [[team]] w/in access std not avail, pop to clinic if 
            #authorized
            if(day < Day && crossbook==TRUE) {
                  teamPOP <- TRUE
                  break
                  #if out of access std and not allowed to crossbook -> take action 
            } else if (day < Day && crossbook==FALSE) {
                  break
            } 
      }
      
      #reset the day
      day <- accessStandard
      
      #iterates until appt is booked in clinic or out of access std
      #iterate through providers in clinic to find an appt
      while(booked!=TRUE && crossbook == TRUE) {
            for(j in 1:(length(schedule[[day]])-1)) {
                  Team <- sample(length(schedule[[day]]),length(schedule[[day]]))
                  Team <- setdiff(Team,team) #remove assigned team (done prior)
                  #create a randomized order to check providers on the team
                  #used to prevent bias when choosing another provider on team
                  for(i in 1:(nrow(schedule[[day]][[team]])-1)) {
                        providerTeam <- sample(nrow(schedule[[day]][[Team[j]]]),nrow(schedule[[day]][[Team[j]]]))
                        if(schedule[[day]][[Team[j]]][providerTeam[i],2]!=0) {
                              location <- c(day,Team[j],providerTeam[i],2) 
                              booked <- TRUE
                        } 
                  }
            }
            
            #check if an appt was found on [[day]]
            if(booked!=TRUE) {
                  day <- day - 1
            }
            
            #if an appt w/in access std not avail, do appropriate action by type
            if (day < Day) {
                  POP <- TRUE
                  break
            } 
      }
      
      #check if appt was booked -> if not use genericFirstAvailable algorithm 
      returnFunc <- list(location, POP, provPOP, teamPOP) 
      if(booked != TRUE) {
            returnFunc <- genericFirstAvailable(schedule,patient,(accessStandard+1),crossbook)
      }
      
      returnFunc
      #function returns a list with:
      #1. schedule pointer
      #2. POP (if access was met)
      #3. provPOP (provider continuity)
      #4. teamPOP (team continuity)
      
      }