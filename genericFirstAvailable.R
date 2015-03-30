#when an appointment within the access standard for the non-acute appt request
#cannot be made with the main scheduling algorithm, 

genericFirstAvailable <- function(schedule,patient,Day,crossbook) {
      
      #extract patient characteristics
      type = patient$getType()
      team = patient$getTeam()
      provider = patient$getProvider()
      
      #set function day to the Day provided
      day<-Day
      
      #dummy to indicate if appt was booked
      booked <- FALSE
      
      #trackers for continuity
      provPOP <- FALSE
      teamPOP <- FALSE
      
      #tracker for access -> set to true since this is used when the algorithms
      #cannot book within the standard
      POP <- TRUE
      
      while(booked!=TRUE) {
            if(schedule[[day]][[team]][provider,2]!=0) {
                  #only use non-acute appointments since acutes are not booked
                  #outside the standard
                  schedule[[day]][[team]][provider,2] <- schedule[[day]][[team]][provider,2]-1
                  location <- c(day,team,provider,2)
                  booked <- TRUE
            }
            
            #exit loop if an appointment was found
            if(booked == TRUE) { 
                  break
            } else {
                  #check if an appt on the first day out of the std is available on
                  #the assigned care team
                  for(i in 1:(nrow(schedule[[day]][[team]]-1))) {
                        #create a randomized order to check providers on the team
                        #used to prevent bias when choosing another provider on team
                        providerTeam <- sample(nrow(schedule[[day]][[team]]),nrow(schedule[[day]][[team]]))
                        providerTeam <- setdiff(providerTeam,provider) #remove assigned PCM
                        if(schedule[[day]][[team]][providerTeam[i],2] != 0) {
                              location <- c(day,team,providerTeam[i],2)booked <- TRUE
                              provPOP <- TRUE
                        } 
                  }
            }
            
            if(booked == TRUE) {
                  break
            } else {
                  for(j in 1:(length(schedule[[day]])-1)) {
                        Team <- sample(length(schedule[[day]]),length(schedule[[day]]))
                        Team <- setdiff(Team,team) #remove assigned team (done prior)
                        #create a randomized order to check providers on the team
                        #used to prevent bias when choosing another provider on team
                        for(i in 1:(nrow(schedule[[day]][[team]])-1)) {
                              providerTeam <- sample(nrow(schedule[[day]][[Team[j]]]),nrow(schedule[[day]][[Team[j]]]))
                              if(schedule[[day]][[Team[j]]][providerTeam[i],2]!=0) {
                                    location <- c(day,Team[j],providerTeam[i],2)booked <- TRUE
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