#' A quick way to play Mines.
#' 
#' This is an easier way to plays Mines.  Instead of 
#' using mines.custom to create the size of the board 
#' you want and to specify how many mines.  This allows 
#' you to use a few presets.
#' 
#' @aliases mines
#' @param size The size of the minefield.  Can be either 
#' "small" "medium" or "large". Only the first letter matters.
#' @author Dason Kurkiewicz \email{dasonk@@iastate.edu}
#' @export
mines <- function(size = "small"){
    size <- toupper(substr(size, 1, 1))
    if(size == "S"){
        n <- 9
        m <- 9
        n.mines <- 10
    }else if(size == "M"){
        n <- 16
        m <- 16
        n.mines <- 40
    }else if(size == "L"){
        n <- 16
        m <- 30
        n.mines <- 99
    }else{
        gmessage("Please only choose small, medium, or large for size")
        return()
    }
    mines_custom(n, m, n.mines)
}

