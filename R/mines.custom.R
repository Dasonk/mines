
#' Playing Mines
#' 
#' Allows you to set the size of the board and the number of mines to be played with.
#' A classic game that will provide entertainment to those in search of a challenge.
#' 
#' @param n.row The number of rows the minefield should contain
#' @param n.col The number of columns the minefield should contain
#' @param n.mines The number of mines in the minefield
#' 
#' @author Dason Kurkiewicz \email{dasonk@@iastate.edu}
#' @export
mines.custom <- function(n.row = 9, n.col = 9, n.mines = 16){
    options(guiToolkit = "RGtk2")
    
    ## CONSTANTS
    ##
    ## The size of each widget - we want all buttons
    ## and labels to have the same size
    SIZE <- c(25,25)
    ## What symbol do we use for the flags?
    FLAG <- "F"
    ## What is used for no flags
    NOFLAG <- ""
    ## What color should the flags be?
    FLAGCOLOR <- "red4"
    ## How often should the timer check for an update?
    ## I don't want it to be off by more than 1/5 of a second.
    TIMERINTERVAL <- 200
    ## How many seconds should the winning
    ## animation take?
    FANCYWINTIME <- 1.5
    ## How many seconds should be added for a hint?
    HINTPENALTY <- 10
    
    ## Various variables
    ##
    ## Keep track of if this is the first click
    first <- TRUE
    ## Keep track of if the game is active
    playing <- FALSE
    ## Keep track of starting time
    ## This gets reset right after the
    ## first click
    start <- Sys.time()
    ## Keep a count of how many flags are on the board
    flagcount <- 0
    
    
    
    ## Create Game Menu
    menulist <- list()
    menulist$Game$New$handler = function(h, ...){
        resethand(NULL)
    }
    menulist$Game$Hint$handler = function(h, ...){
        hinthand()
    }
    menulist$Game$sep = list(separator = TRUE)
    menulist$Game$Quit$handler = function(h, ...){
        dispose(win)
    }
    
    ## Create Size Menu
    menulist$Size$Small$handler = function(h, ...){
        newgame("small")
    }
    menulist$Size$Medium$handler = function(h, ...){
        newgame("medium")
    }
    menulist$Size$Large$handler = function(h, ...){
        newgame("large")
    }
    
    ## Create Help Menu
    menulist$Help$Help$handler <- function(h, ...){
        gmessage("No help yet.  Sucks to be you")
    }
    menulist$Help$sep <- list(separator = TRUE)
    menulist$Help$About$handler <- function(h, ...){
        gmessage("Written by: Dason Kurkiewicz\nVersion: 1.6.1\nLast updated: June 25, 2011", 
                 title = "About Rmines",
                 icon = "info")
    }
    
    
    ## Actually start the gui
    win <- gwindow("Mines", width = 200, height = 200)
    
    ## Add the menu
    menu <- gmenu(menulist, cont = win)
    
    
    biggroup <- ggroup(hor = TRUE, cont = win)
    addSpring(biggroup)
    smallgroup <- ggroup(hor = FALSE, cont = biggroup)
    topgroup <- ggroup(hor = TRUE, cont = smallgroup)
    addSpring(biggroup)
    
    ## gg is what actually contains the minefield
    gg <- gframe(horizontal = FALSE, spacing = 0)
    
    addSpring(smallgroup)
    
    ## Function for calculating
    ## how long (in seconds) it has been
    ## since the first click
    get.time <- function(round = 0){
        time <- as.numeric(difftime(Sys.time(), start, units = "secs"))
        displaytime <- round(time, digits = round)
        return(displaytime)
    }
    
    ## A function to generate a map with mines in it
    generate.map <- function(n, m, n.mines){
        map <- matrix(F, nrow = n, ncol = m)
        mines <- sample(1:(n*m), n.mines)
        map[mines] <- TRUE
        return(map)
    }
    
    ## The map of the minefield
    map <- generate.map(n.row, n.col, n.mines)
    
    ## Tells us which cells the player has explored
    playertable <- matrix(F, nrow =n.row, ncol = n.col)
    
    ## Let Inf represent a location that is a mine
    ## This function tells you how many mines are
    ## in the surrounding blocks.
    ## Assumes: Input is inside the map
    get.nums <- function(row, col){
        ## For this function could replace with n and m.
        #n.row <- dim(map)[1]
        #n.col <- dim(map)[2]
        
        ## Inf represents that location is a mine
        if(map[row, col]){
            return(Inf)
        }
        
        ro <- c(rep(row-1, 3), rep(row, 2), rep(row+1, 3))
        co <- c(col-1, col, col+1, col-1, col+1, col-1, col, col+1)
        check <- rep(TRUE, 8)
        mine <- rep(FALSE, 8)
        
        ## Find the spots outside of the map
        idx <- which(ro < 1 | ro > n.row | co < 1 | co > n.col)
        check[idx] <- FALSE
        
        ## Check to see which surrounding areas have mines
        for(i in 1:8){
            if(check[i]){
                mine[i] <- map[ro[i], co[i]]
            }
        }
        
        ## Return the number of mines in surrounding areas
        return(sum(mine))
    } ## end of get.nums #####################################
    
    ## Given an input (from get.nums) what color
    ## should we give a spot with input number of
    ## mines surrounding it.
    get.col <- function(input){
        ans <- "blue"
        if(input == 1){
            ans <- "blue"
        }else if(input == 2){
            ans <- "green"
        }else if(input == 3){
            ans <- "red"
        }else if(input == 4){
            ans <- "darkblue"
        }else if(input == 5){
            ans <- "darkred"
        }else if(input == 6){
            ans <- "cyan"
        }else if(input == 7){
            ans <- "purple"
        }else if(input == 8){
            ans <- "black"
        }else if(input == Inf){
            ans <- "red"
        }
        return(ans)
    }
    
    ## Translates a number into what we want
    ## the glabel to display
    get.text <- function(input){
        ans <- input
        if(input == Inf){
            ans <- "!"
        }else if(input == 0){
            ans <- ""
        }
        return(ans)
    }
    
    
    ## This function will block any handlers from
    ## receiving any input.
    blockhandlers <- function(){
        for(i in 1:n.row){
            for(j in 1:n.col){
                id1 <- as.numeric(tag(layout[[i]][[j]]$button, "id1"))
                id2 <- as.numeric(tag(layout[[i]][[j]]$button, "id2"))
                blockHandler(layout[[i]][[j]]$button, id1)
                blockHandler(layout[[i]][[j]]$button, id2)
            }
        }
    }
    
    ## Goes through and unblocks all handlers on the buttons
    unblockhandlers <- function(){
        for(i in 1:n.row){
            for(j in 1:n.col){
                id1 <- as.numeric(tag(layout[[i]][[j]]$button, "id1"))
                id2 <- as.numeric(tag(layout[[i]][[j]]$button, "id2"))
                unblockHandler(layout[[i]][[j]]$button, id1)
                unblockHandler(layout[[i]][[j]]$button, id2)
            }
        }
    }
    
    ## A function to be called when you lose the game
    gameover <- function(){
        if(playing){
            playing <<- FALSE
            ## If we want to display all the mines
            ## Takes a while on big boards
            ## for(i in 1:n.row){
            ##     for(j in 1:n.col){
            ##         if(map[i,j]){
            ##             guess(i, j)
            ##         }
            ##     }
            ## }
            losemessage <- "You lost - Don't explode yourself next time."
            newgameorquit(losemessage)
        }
        ## Stops the timer
        #playing <<- FALSE
        
        
        
        
    }
    
    ## Check if we won the game
    checkwin <- function(){
        val <- sum((map - (!playertable))^2)
        if(val == 0){
            gamewin()
        }
    }
    
    ## Called when the game is won.
    ## Stops the timer - displays a neat animation
    ## and gives the user a message.
    gamewin <- function(){
        ## Used to stop the timer
        playing <<- FALSE
        
        ## Figure out how long it took to win.
        endtime <- svalue(timerlabel)
        
        ## Create a fancy way of getting rid of the buttons.
        #grid <- expand.grid(1:n.row,1:n.col)
        #ord <- sample(1:(n.row*n.col))
        #slp <- FANCYWINTIME/(n.row*n.col)
        #for(idx in ord){
        #	i <- grid[idx, 1]
        #	j <- grid[idx, 2]
        #	visible(layout[[i]][[j]]$group) <- FALSE
        #	Sys.sleep(slp)
        #}
        
        ## Display a message and tell the user how long it took.
        winmessage <- paste("You Won!\n", endtime, "seconds.")
        newgameorquit(winmessage)
    }
    
    ## A function to handle what to do when
    ## a user makes a guess.
    ## i - the row they guessed
    ## j - the column they guessed
    guess <- function(i, j){
        
        ## Check if guess is in the table
        if(!(i > 0 & i <= n.row & j > 0 & j <= n.col)){
            return()
        }
        
        ## If the spot is a flag then don't check it.
        val <- svalue(layout[[i]][[j]]$button)
        if(val == FLAG){
            return()
        }
        
        ## If we've already been on this spot don't continue
        if(playertable[i,j]){
            return()
        }
        
        ## Mark that we explored this spot on the playertable
        playertable[i, j] <<- TRUE
        
        ## Should only be dealing with spots that are buttons now.
        
        ## Change the button to the label
        delete(layout[[i]][[j]]$group, layout[[i]][[j]]$button)
        add(layout[[i]][[j]]$group, layout[[i]][[j]]$label)
        
        num <- svalue(layout[[i]][[j]]$label)
        if(num == get.text(Inf)){
            ## We hit a mine - End the game
            gameover()
        }
        
        ## Check if we won
        checkwin()
        
        ## If the spot is blank we can automatically
        ## clear the 8 surrounding cells.
        if(num == get.text(0)){
            guess(i-1, j-1)
            guess(i-1, j  )
            guess(i-1, j+1)
            guess(i,   j-1)
            guess(i,   j+1)
            guess(i+1, j-1)
            guess(i+1, j  )
            guess(i+1, j+1)
        }
    } ## end of guess #####################################
    
    ##Left click handler for buttons
    buttonlchand <- function(h, ...){
        if(first){
            first <<- FALSE
            playing <<- TRUE
            start <<- Sys.time()
        }
        i <- tag(h$obj, "row")
        j <- tag(h$obj, "col")
        guess(i, j)
    } ## end of buttonlchand #############################
    
    ## Right click handler for buttons
    switchflag <- function(h, ...){
        val <- svalue(h$obj)
        i <- tag(h$obj, "row")
        j <- tag(h$obj, "col")
        id1 <- as.numeric(tag(h$obj, "id1"))
        id2 <- as.numeric(tag(h$obj, "id2"))
        if(val == FLAG){
            svalue(h$obj) <- NOFLAG
            flagcount <<- flagcount - 1
            updateflaglabel()
        }else{
            svalue(h$obj) <- FLAG
            font(h$obj) <- c(color = FLAGCOLOR)
            flagcount <<- flagcount + 1
            updateflaglabel()
        }
    } ## end of buttonrchand #######################
    
    ## Updates the flag label
    ## Called after we add or remove a flag.
    updateflaglabel <- function(){
        svalue(flaglabel) <- paste("Flags: ", flagcount, "/", n.mines, sep = "")
    }
    
    ## Updates the timer
    ## Should be used as an idle handler
    timerhandler <- function(h, ...){
        if(!playing){
            return()
        }
        svalue(h$obj) <- paste("Time:", get.time())
    }
    
    hinthand <- function(...){
        ## Add a penalty for getting a hint
        start <<- start - HINTPENALTY
        ## We only care about spots we haven't
        ## clicked on
        idx <- which(!playertable)
        cont <- TRUE
        tmp <- 0
        i <- 0
        j <- 0
        while(cont){
            ## Grab a random spot we haven't clicked on
            tmp <- sample(idx, 1)
            ## Figure out the row
            i <- ((tmp-1) %% n.row)+1
            ## Figure out the column
            j <- ((tmp-1) %/% n.col)+1
            ## Make sure it isn't a mine
            #if(!map[tmp]){
            if(svalue(layout[[i]][[j]]$label) != get.text(Inf)){
                cont <- FALSE
            }
        }
        
        
        buttonlchand(list(obj = layout[[i]][[j]]$button))
        #guess(i, j)
        
        ## Only click that spot - don't use guess because if we click
        ## an empty spot we don't want it to automatically expand...
        ## They can do that themselves since they got a hint...
        ## playertable[i, j] <<- TRUE
        ## delete(layout[[i]][[j]]$group, layout[[i]][[j]]$button)
        ## add(layout[[i]][[j]]$group, layout[[i]][[j]]$label)
        ## But it's possible somebody uses a hint for the last spot
        ## So check if they won or not...
        ## checkwin()
    }
    
    ## Resets the board
    resethand <- function(h, ...){
        first <<- TRUE
        playing <<- FALSE
        
        ## Set everything back to buttons
        ## and set buttons with flags to be cleared
        for(i in 1:n.row){
            for(j in 1:n.col){
                if(playertable[i,j]){
                    delete(layout[[i]][[j]]$group, layout[[i]][[j]]$label)
                    add(layout[[i]][[j]]$group, layout[[i]][[j]]$button)					
                }else{
                    val <- svalue(layout[[i]][[j]]$button)
                    if(val != NOFLAG){
                        switchflag(list(obj = layout[[i]][[j]]$button))
                    }
                }
                #visible(layout[[i]][[j]]$group) <- TRUE
            }
        }
        
        map <<- generate.map(n.row, n.col, n.mines)
        playertable <<- matrix(F, nrow = n.row, ncol = n.col)
        
        ## With a new map we need to change the labels to reflect that.
        for(i in 1:n.row){
            for(j in 1:n.col){
                num <- get.nums(i, j)
                svalue(layout[[i]][[j]]$label) <- get.text(num)
                font(layout[[i]][[j]]$label) <- c(color = get.col(num))	
            }
        }
        svalue(timerlabel) <- paste("Time: 0")
        
    }
    
    newgame <- function(size){
        alertgroup <- ggroup(hor = TRUE)
        infopic <- gimage("info", "stock", "button")
        add(alertgroup, infopic)
        message <- glabel("Destroying current window\nand creating new game")
        add(alertgroup, message)
        add(smallgroup, alertgroup)
        # Give it time to add or else the add won't
        # happen quick enough and the message won't appear.
        Sys.sleep(.1)
        # Create a new game
        mines(size)
        # Get rid of the old window...
        dispose(win)
    }
    
    newgameorquit <- function(message){
        blockhandlers()
        thiswin <- gwindow("New game?", width = 50, height = 50)
        gg <- ggroup(cont = thiswin, hor = TRUE)
        leftgroup <- ggroup(hor = F, cont = gg)
        rightgroup <- ggroup(hor = F, cont = gg, expand = TRUE)
        infoicon <- gimage("info", "stock", "dialog", cont = leftgroup)
        label <- glabel(message, cont = rightgroup)
        buttongroup <- ggroup(hor = T, cont = rightgroup)
        addSpring(buttongroup)
        
        newgamehand <- function(h, ...){
            resethand(NULL)
            unblockhandlers()
            dispose(thiswin)
        }
        newgamebut <- gbutton("New Game", cont = buttongroup, handler = newgamehand)
        
        quithand <- function(h, ...){
            dispose(win)
            dispose(thiswin)
        }
        quitbut <- gbutton("Quit", cont = buttongroup, handler = quithand)
        
        addSpring(buttongroup)
    }
    
    ## TODO: Add a label click handler to clear
    ## surrounding cells if the number of flags
    ## is met...
    
    
    flaglabel <- glabel()
    updateflaglabel()
    add(topgroup, flaglabel)
    addSpring(topgroup)
    
    resetbutton <- gbutton("Reset")
    addHandlerClicked(resetbutton, handler = resethand)
    add(topgroup, resetbutton)
    
    hintbutton <- gbutton("Hint")
    addHandlerClicked(hintbutton, handler = hinthand)
    add(topgroup, hintbutton)
    addSpring(topgroup)
    
    ## Stuff for the timer
    timerlabel <- glabel("Time: 0")
    addHandlerIdle(timerlabel, handler = timerhandler, interval = TIMERINTERVAL)
    add(topgroup, timerlabel)
    
    ## Groups that contains an entire row
    rowgroups <- list()
    
    ## Groups/buttons/labels for each spot
    layout <- list()
    
    for(i in 1:n.row){
        rowgroups[[i]] <- ggroup(cont = gg, spacing = 0)
        svalue(rowgroups[[i]]) <- 0
        layout[[i]] <- list()
    }
    
    for(i in 1:n.row){
        for(j in 1:n.col){
            ## For each "spot" have a list containing
            ## a group, a button, and a label
            layout[[i]][[j]] <- list()
            
            ## Make the group
            #layout[[i]][[j]]$group <- ggroup(spacing = 0, cont = rowgroups[[i]])
            layout[[i]][[j]]$group <- ggroup(spacing = 0)
            svalue(layout[[i]][[j]]$group) <- 0
            add(rowgroups[[i]], layout[[i]][[j]]$group)
            
            ## Making the button
            layout[[i]][[j]]$button <- gbutton("")
            id1 <- addHandlerClicked(layout[[i]][[j]]$button, handler = buttonlchand)
            id2 <- addHandlerRightclick(layout[[i]][[j]]$button, handler = switchflag)
            tag(layout[[i]][[j]]$button, "row") <- i
            tag(layout[[i]][[j]]$button, "col") <- j
            ## These are needed to block the handlers later
            tag(layout[[i]][[j]]$button, "id1") <- id1
            tag(layout[[i]][[j]]$button, "id2") <- id2
            size(layout[[i]][[j]]$button) <- SIZE
            
            num <- get.nums(i, j)
            layout[[i]][[j]]$label <- glabel(get.text(num))
            font(layout[[i]][[j]]$label) <- c(color = get.col(num))
            size(layout[[i]][[j]]$label) <- SIZE
            add(layout[[i]][[j]]$group, layout[[i]][[j]]$button)
        }
    }
    
    add(smallgroup, gg)
    
    ## For debugging - Also for cheating.
    ##print(map)
    
} 
