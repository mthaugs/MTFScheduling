firstAvailBeta <- function(schedule,patient,Day,crossbook=TRUE) {
      #source("patient.R") #Patient has -> appt type, provider team, provider 
      #source("horizon.R") #returns the number of days in the appt type
      #source("genericFirstAvailable.R")
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
      Horizon = horizon(type)
      
      #return the last schedule day in the schedule that meets the access std
      accessStandard = day+Horizon
      
      #points to the correct schedule provider column for acute or non-acute
      if(type=="acute") {
            col <- 1
      } else if(type=="routine"||type=="well") {
            col <- 2
      } else { message("error in appt type provided for schedule col")}
      
      #iterates until appt is booked w/ scheduled provider or out of access std
      while(booked!=TRUE) {
            if(schedule[[day]][[team]][provider,col]!=0) {
                  location <- c(day,team,provider,col) 
                  booked <- TRUE
            } else if(type == "acute" && schedule[[day]][[team]][provider,2]!=0) {
                  location <- c(day,team,provider,2) 
                  booked <- TRUE
            } else {
                  day <- day + 1
            }
            if(day > accessStandard) {
                  provPOP <- TRUE
                  break
            }
      }
      
      #reset day to check team
      day <- Day
      
      #iterates until appt is booked in team or out of access std
      while(booked!=TRUE) {
            #iterate through providers on team to try and find an appointment
            for(i in 1:(nrow(schedule[[day]][[team]])-1)) {
                  #create a randomized order to check providers on the team
                  #used to prevent bias when choosing another provider on team
                  providerTeam <- sample(nrow(schedule[[day]][[team]]),nrow(schedule[[day]][[team]]))
                  providerTeam <- setdiff(providerTeam,provider) #remove assigned PCM
                  if(schedule[[day]][[team]][providerTeam[i],col] != 0) {
                        location <- c(day,team,providerTeam[i],col) 
                        booked <- TRUE
                  } else if(type == "acute" && schedule[[day]][[team]][providerTeam[i],2]!=0) {
                        location <- c(day,team,providerTeam[i],2) 
                        booked <- TRUE
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
            } else if (day > accessStandard && crossbook==FALSE && type == "acute") {
                  teamPOP <- TRUE
                  booked <- TRUE    #acute always referred out of the clinic to 
                  #stay within the access standard
                  POP <- TRUE
                  break
            } else if (day > accessStandard && crossbook==FALSE && type != "acute") {
                  teamPOP <- TRUE
                  break
            } 
      }
      
      #reset the day to check full clinic
      day <- Day
      
      #iterates until appt is booked in clinic or out of access std
      #iterate through providers in clinic to find an appt
      while(booked!=TRUE) {
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
                        } else if(type == "acute" && schedule[[day]][[Team[j]]][providerTeam[i],2]!=0) {
                              location <- c(day,Team[j],providerTeam[i],2) 
                              booked <- TRUE
                        }
                  }
            }
            
            #check if an appt was found on [[day]]
            if(booked!=TRUE) {
                  day <- day + 1
            }
            
            #if an appt w/in access std not avail, do appropriate action by type
            if(day > accessStandard && type == "acute") {
                  booked <- TRUE
                  POP <- TRUE
                  break
            } else if (day > accessStandard && type != "acute") {
                  POP <- TRUE
                  break
            } 
      }
      
      #check if appt was booked after checking. For non-acute appts that could 
      #not be booked within the access standard, use the genericFirstAvailable
      #algorithm to book the patient
      returnFunc <- list(location, POP, provPOP, teamPOP) 
      if(booked != TRUE) {
            genericFirstAvailable(schedule,patient,(accessStandard+1),crossbook)
      }
      returnFunc <- list(location, POP, provPOP, teamPOP)  
      returnFunc
      #function returns a list with:
      #1. schedule pointer
      #2. POP (if access was met)
      #3. provPOP (provider continuity)
      #4. teamPOP (team continuity)
}


