
#Creates a new schedule with the number of days. Each day is populated with a list
#of provider teams and each provider team is populated with a matrix of providers
#and the number of available appointments.
#Schedule[[day]][[Team]][Provider][ApptType]
makeSchedule <- function(days=100, teams=1, provPerTeam=1, act=0, nonact=0, avail=1) {
      
      #Create a matrix that represents a provider team
      createTeam <- function(act,nonact) {
            acute <- c(rep(act,provPerTeam))
            nonacute <- c(rep(nonact,provPerTeam))
            Team <- cbind(acute, nonacute)
      }
      
      #Create a full day to add to the schedule
      createDay <- function(teams) {
            Day <- vector("list",teams)
            #cycle to create all providers teams
            for(i in 1:teams) { Day[[i]] <- createTeam(act,nonact) }
            Day
      }
      
      #create the schedule
      schedule <- vector("list",days)
      for(i in 1:days) {
            schedule[[i]] <- createDay(teams)
      }
      schedule
}

