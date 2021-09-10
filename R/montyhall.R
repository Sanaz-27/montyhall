

#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}



#' @title
#' CONTESTANT SELECTS A DOOR.
#'
#' @description
#' `select_door()` The contestant makes their first selection.
#'
#' @details
#' The contestant makes their first selection of a random door from
#' Doors 1-3, with 1/3 chance to choose a Car and 2/3 chance to
#' choose a goat.
#'
#' @param
#' no arguments are used by the function.
#'
#' @return
#' The function returns a 1/3 chance to choose a Car and 2/3 chance
#' to choose a goat.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' HOST OPENS GOAT DOOR.
#'
#' @description
#' `open_goat_door()` HOST OPENS GOAT DOOR.
#'
#' @details
#' The host will always open a door with a goat behind it,
#' but it can’t be a door the contestant has already selected.
#' So it must be a door that is not a car and not a current
#' contestant selection.
#' Note that if the contestant selects the car on the first guess
#' the host can open either door, but if the contestant selects
#' a goat the host only has one option.
#'
#' @param
#' if arguments are used by the function.
#'
#' @return
#' The function returns the opened door.
#'
#' @examples
#' open_goat_door()
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#' CHANGE DOORS.
#'
#' @description
#' `change_door ()` CHANGE DOORS.
#'
#' @details
#' The contestant is given the option to change from their
#' initial selection to the other door that is still closed.
#' The function will represent the game-playing strategy as
#' the argument stay=TRUE or stay=FALSE.
#'
#' @param
#' if arguments are used by the function.
#'
#' @return
#' The function returns the final pick of the door whether  to Stay
#' or Switch.
#'
#' @examples
#' change_door ()
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)

  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }

  return( final.pick )  # number between 1 and 3
}



#' @title
#' DETERMINE IF CONTESTANT HAS WON.
#'
#' @description
#' `determine_winner()` DETERMINE IF CONTESTANT HAS WON.
#'
#' @details
#' After the contestant choose his/her final pick whether to Stay
#' or Switch doors, the final picked door is open and the results
#' of determining if the contestant Wins or Loses.
#'
#' @param
#' if arguments are used by the function.
#'
#' @return
#' The function returns if the contestant WIN or LOSE.
#'
#' @examples
#' determine_winner()
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title
#' Simulations of the Game.
#'
#' @description
#' `play_game ()` Simulations of the Game.
#'
#' @details
#' This is a simulation of the game to go through all previous
#' steps in one export.
#'
#' @param
#' arguments are used by the function.
#'
#' @return
#' The function returns game results.
#'
#' @examples
#' play_game ()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Game Set-Up.
#'
#' @description
#' `play_n_games()` game set-up.
#'
#' @details
#' Return a data frame that contains results from one game, then
#' use a loop to build a simulation that plays the game 100 times,
#' and create a table to report the results.
#'
#' @param
#' for, table, round and print arguments are used by the function.
#'
#' @return
#' The function returns table to report the results.
#'
#' @examples
#' play_n_games()
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

  return( results.df )

}
