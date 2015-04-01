#' makeMTFSchedule Function
#'
#' This function allows you to create a new MTF schedule to populate with the 
#' three triaged appt types (acute, routine, well).
#' @param days defines the number of days in the schedule. Defaults to 100 days
#' @param teams defines the number of teams in the clinic. Defaults to 1 team.
#' @param provPerTeam defines the number of providers on each team. Defaults to 1.
#' @param act defines the number of appts to reserve for acute patient requests. Defaults to 0.
#' @param routine defines the number of appts in the routine appt pool to book. Defaults to 0.
#' @param well defines the number of appts in the well appt pool to book. Defaults to 0.
#' @param avail defines the percentage of time providers are available in the schedule. Defaults to 1
#' @keywords schedule
#' @export
#' @examples
#' makeMTFSchedule(days=100, teams=1, provPerTeam=1, act=5, routine=13, well=1, avail=1)
#' 
#Creates a new MTF schedule with the # of days. Each day is populated with a list
#of provider teams and each provider team is populated with a matrix of providers
#and the number of available appointments. The primary difference compared to 
#the proposed algorithms is the use of three triage levels of appointments

#Schedule[[day]][[Team]][Provider][ApptType]
makeMTFSchedule <- function(days=100, teams=1, provPerTeam=1, act=0, routine=0, well=0, avail=1) {
      
      #Create a matrix that represents a provider team
      createTeam <- function(act,routine,well) {
            acute <- c(rep(act,provPerTeam))
            routine <- c(rep(routine,provPerTeam))
            well <- c(rep(well,provPerTeam))
            Team <- cbind(acute, routine, well)
      }
      
      #Create a full day to add to the schedule
      createDay <- function(teams) {
            Day <- vector("list",teams)
            #cycle to create all providers teams
            for(i in 1:teams) { Day[[i]] <- createTeam(act,routine, well) }
            Day
      }
      
      #create the schedule
      schedule <- vector("list",days)
      for(i in 1:days) {
            schedule[[i]] <- createDay(teams)
      }
      schedule
}

