#' evenFill Function
#'
#' This function schedules a patient into the day with the most availability. 
#' It moves through the hierarchy of the assigned
#' provider, to the assigned care team, and then to the full clinic if cross-booking
#' is authorized. The function returns a vector (list) with the position of the 
#' appt to book and whether access and continuity were met. 
#' @param schedule provides the schedule (list type object) to search for an open appt
#' @param patient provides the patient class with the associated attributes
#' @param Day identifies the day in the schedule to begin searching for an appt
#' @param crossbook indicates if crossbooking to the full clinic is authorized. Default is TRUE.
#' @keywords evenFill, scheduling, non-Acute, routine, well
#' @export
#' @examples
#' evenFill(schedule,patient,Day=5,crossbook=TRUE)
#' 
#Algorithm to assign patient to the day within the access std with greatest avail
#This does not apply for acute appts -> acute always first available
#source firstAvailable function for acute appts in simulation library
#See README for algorithm flow chart
evenFill <- function(schedule,patient,Day,crossbook=TRUE) {
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
      day<-Day
      
      #function to return the lowest filled day for the provider
      findLow <- function(provider, team, Day, accessStandard) {
            #build vector of days to check
            dayVector<-c(rep(0,Horizon))
            day<-Day
            #fill vect with the number of avail appts each day for [provider,]
            for(i in 1:Horizon) {
                  dayVector[i] <- schedule[[day]][[team]][provider,2]
                  day <- day + 1
            }
            #return a vector indicating the index order for lowest filled day
            checkVector <- sort(dayVector,decreasing = TRUE, index.return=TRUE)
            index <- checkVector$ix[1] + Day - 1 
            #index is position in set of available day that has greatest avail
            funcReturn <- c(index,schedule[[index]][[team]][provider,2])
            funcReturn
            #returns a vector with the day [index,] of the provider on the team 
            #and the number of [,appts] available on that day
      }
      
      #indicate the provider with the most open schedule on the team within the
      #access std
      findLowestProviderInTeam <- function(team, Day, accessStandard) {
            providerIndex <- 0
            apptsAvail <- 0
            for(i in 1:nrow(schedule[[Day]][[team]])) {
                  temp <- findLow(i, team, Day, accessStandard)
                  #if the temporary provider has more availability than the prev
                  #provider checked -> repoint to the better provider
                  if(temp[2]>apptsAvail) {
                        providerIndex <- i
                        dayIndex <- temp[1]
                        apptsAvail <- temp[2]
                  }
            }
            
            #if no appts available for any provider -> returns random provider
            #prevents systematic bias in other functions that call this one
            if(providerIndex <- 0) {
                  providerIndex <- sample(1:nrow(schedule[[Day]][[team]]),1)
            }
            funcReturn <- c(providerIndex,dayIndex,apptsAvail)
            funcReturn
            #returns a vector with the [provider index,] of the provider  
            #the [,day,] with greatest availability, and the [,apptsavail] on 
            #that day
      }
      
      #iterates until appt is booked w/ scheduled provider or out of access std
      
      #find the day with most availability and check if available
      dayIndex<- findLow(provider, team, Day, accessStandard)
      if(dayIndex[2]>0) {
            location <- c(dayIndex[1],team,provider,2)
            booked <- TRUE
            #if no appts available, check the team
      } else {
            provPOP <- TRUE
      }
      
      if(booked!=TRUE) {
            dayTeamIndex <- findLowestProviderInTeam(team, Day, accessStandard) 
            if(dayTeamIndex[3]>0) {
                  location <- c(dayTeamIndex[2],team,dayTeamIndex[1],2)
                  booked <- TRUE
            } else if (crossbook==TRUE) {
                  teamPOP <- TRUE     
            } 
      }
      
      if(booked!=TRUE && crossbook == TRUE) {
            #identify set of teams to check
            teamIndex <- c(1:length(schedule[[day]]))
            #care team already eliminated as possible option
            teamIndex <- setdiff(teamIndex,team)
            teamIndex <- sample(teamIndex)
            apptsAvail <- 0
            teamValue <- 0
            scheduleDay <- 0
            provIndex <- 0
            for(i in 1:length(teamIndex)) {
                  dayTeamIndex <- findLowestProviderInTeam(teamIndex[i], Day, accessStandard)
                  if(dayTeamIndex[3] > apptsAvail) {
                        apptsAvail <- dayTeamIndex[3]
                        teamValue <- teamIndex[i]
                        provIndex <- dayTeamIndex[1]
                        scheduleDay <- dayTeamIndex[2]
                  }
            }
            if(apptsAvail>0) {
                  booked <- TRUE
                  location <- c(scheduleDay,teamValue,provIndex,2)
            } else {
                  POP <- TRUE
            }
      }
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
      
      #returns a vector with the [provider index,] of the provider  
      #the [,day,] with greatest availability, and the [,apptsavail] on 
      #that day
      
}