#'Basic Particle Swarm Optimization Solver
#' 
#' Finds a solution to a multimodal objective function using basic PSO.
#'
#'@name PSO
#'@param obj_function    the name of the function under which the objective function resides
#'@param n               the number of particles to use in the PSO algorithm
#'@param d               the number of dimensions (design variables) of the objective function
#'@param bounds          the bounds of the design space, as a list
#'@param contour         optional argument to create a 2-D contour plot fo design space.  **Does not work yet!!!
#'@param parallel        optional argument to state the number of cores to dedicate for parallel processing.  **Does not work yet.
#'
#'@return                 a list containing the solution and the position of the design variables at that solution
#'
#'@examples
#'## Define you objective function by name - Ackley's Path Function
#'obj_function = function(x) {
#'    f <<- f + 1
#'    return((4 - 2.1 * x[1]**2 + (x[1]**4) / 3) * x[1]**2 + x[1] * x[2] + (-4 + 4 * x[2]**2) * x[2]**2)
#'}
#'
#'## Define your bounds
#'bounds = list(c(-2.0, 2.0), c(-2.0, 2.0))
#'
#'## Call the function
#'p <- PSO(obj_function, n=15, d=2, bounds=bounds)
#'

require(R6)
#require(tidyverse)      Package required for ggplot2 contour plotting
#require("parallel")     Package for parallel processing

# Swarm Class (Collects Particles)
Swarm = R6Class("Swarm", public = list(
    swarm = list(),
    
    print = function() {
        print(self$swarm)
    },
    initialize = function(obj.f, n, d) {
        # Create the list of particles
        self$swarm <- replicate(Particle$new(obj.f, d), n=n)
      
        
        while(k < k.max) {
            # cat("\n")
            # cat("Global Best F (Before):        ", global.best.f.err, "\n")
            # cat("Global Best Position (Before): ", global.best.position, "\n")
            
            # Look at each particle in the swarm and find the best one.  Update global values
            # to reflect this new value
            for (i in seq_len(n)) {
                if ((self$swarm[[i]]$obj.f.err < global.best.f.err) || (global.best.f.err == -1)) {
                    global.best.f.err <<- self$swarm[[i]]$obj.f.err
                    global.best.position <<- self$swarm[[i]]$position.best
                }
            }
            
            # cat("\n")
            # cat("Global Best F (After):        ", global.best.f.err, "\n")
            # cat("Global Best Position (After): ", global.best.position, "\n")
            
            # Update swarm
            for (i in seq_len(n)) {
                # cat("\n")
                # cat("Initial Particle Velocity: ", self$swarm[[i]]$velocity, "\n")
                
                self$swarm[[i]]$update.velocity()
                self$swarm[[i]]$update.position()
                
                # cat("\n")
                # cat("Updated Particle Velocity: ", self$swarm[[i]]$velocity, "\n")
                # cat("Updated Particle Position: ", self$swarm[[i]]$position, "\n")
                
                self$swarm[[i]]$eval()
                
                # cat("Updated Objective Function: ", self$swarm[[i]]$obj.f.err, "\n")
            }
            
            
            # cat("\n") # Blank Line
            # self$print()

            # See if complete
            # Standard deviation of particle positions and objective functions
            obj.fun.list = c(rep(NA, n))

            for (i in seq_len(n)) {
                obj.fun.list[i] = self$swarm[[i]]$obj.f.err.best
            }

            obj.fun.list = obj.fun.list[!is.na(obj.fun.list)]
            
            
            # if (length(obj.fun.list) != 1) {
            #   std.dev = sd(obj.fun.list, na.rm=TRUE)
            # } else {
            #   std.dev = obj.fun.list
            # }
            std.dev = sd(obj.fun.list, na.rm=TRUE)

            # print(obj.fun.list)
            # print(std.dev)
            # print(conv)

            if (std.dev <= conv) {
                return()
            }
            
            k <<- k + 1  # Count iterations
        }
      
        invisible(self)
    })
)

# Particle Class
Particle = R6Class("Particle", list(
    position = c(NA),                    # The current position of the particle
    position.best = c(NA),               # The global best position of the particle
    velocity = c(NA),                    # The current velocity of the particle
    obj.f = NA,                          # The objective function to call
    obj.f.err = -1,                      # The last value of the objective function
    obj.f.err.best = -1,                 # The best value of the objective function obtained
    
    # Calls the objective function and updates the particles positions
    eval = function() {
        self$obj.f.err = self$obj.f(self$position)
        # print(self$obj.f.err)
        
        # Compare the new value with the historical best
        if ((self$obj.f.err < self$obj.f.err.best) || (self$obj.f.err.best == -1)) {
            self$position.best = self$position
            self$obj.f.err.best = self$obj.f.err
        }
    },
    
    # Function to update the velocity each iteration
    update.velocity = function() {
        c1 = 2          # Cognitive confidence interval
        c2 = 2          # Social confidence interval
        
        for (i in seq_len(dimensions)) {
            r1 = runif(1, 0, 1)  # Random number between 0 and 1
            r2 = runif(1, 0, 1)  # Random number between 0 and 1
            
            # cat("Global best inside update velocity: ", global.best.position, "\n")
            
            v.cognitive = c1 * r1 * (self$position.best[i] - self$position[i])
            v.social = c2 * r2 * (global.best.position[i] - self$position[i])   #***Possible error in global.best.position
            
            # cat("\n")
            # cat("Last velocity:        ", self$velocity[i], "\n")
            # cat("Social velocity:      ", v.social, "\n")
            # cat("Cognitive velocity:   ", v.cognitive, "\n")
            
            self$velocity[i] = self$velocity[i] + v.cognitive + v.social
            # cat("New velocity:         ", self$velocity[i], "\n")
            
            
            # Check on velocity bounds
            if (self$velocity[i] > (v.max.limit * bounds[[i]][2])) {
                self$velocity[i] = v.max.limit * bounds[[i]][2]
            }
            
            if (self$velocity[i] < (v.max.limit * bounds[[i]][1])) {
                self$velocity[i] = v.max.limit * bounds[[i]][1]
            }
        }
        
    },
    
    update.position = function() {
        for (i in seq_len(dimensions)) {
            # Update the position using the last position and current velocity
            self$position[i] = self$position[i] + self$velocity[i]
            
            # Check for bounds violation
            if (self$position[i] < bounds[[i]][1]) {
                self$position[i] = bounds[[i]][1]
            }
            
            if (self$position[i] > bounds[[i]][2]) {
                self$position[i] = bounds[[i]][2]
            }
        }
    },
    # Initializer of the Particle Class
    initialize = function(obj.f, d) {
        # Set initial position
        self$position = rep(runif(1, -2, 2), d)   # TODO - This needs to change to user-set
        self$position.best = self$position

        # Set the initial velocity
        #self$velocity = seq(1, d)
        self$velocity = rep(0, d)
        
        # Evaluate the objective function
        ##self$obj.f.err = obj.f(self$position)
        self$obj.f = obj.f
        self$eval()
        self$obj.f.err.best = self$obj.f.err
        
        
        ############## PRINT OUT INITIAL PARTICLE DATA
        # cat("Position:         ", self$position, "\n")
        # cat("Best position:    ", self$position.best, "\n")
        # cat("Velocity:         ", self$velocity, "\n")
        # cat("Obj F Error:      ", self$obj.f.err, "\n")
        # cat("Obj F Error Best: ", self$obj.f.err.best, "\n")
        
        invisible(self)
    })
)

PSO = function(obj.f, n, d, bounds, contour=FALSE) {
    f <<- 0                                         # Number of objective function evaluations
    dimensions <<- d                                # The number of design variables for this problem
    global.best.f.err <<- -1                        # The best objective function evaluation
    global.best.position <<- c(rep(0, d))           # The coordinates relating to the best evaluation      **** Why does this have to be a list?
    k <<- 1                                         # Iteration counter
    k.max <<- 1000                                  # Maximum iterations allowed - user should set this
    v.max.limit <<- 0.10                            # Limit on max velocity to prevent overshooting good targets
    conv <<- 0.001                                  # Convergence criterion
    std.dev <<- 1.0                                 # Initial standard deviation
    # bounds = list(c(-3.0, 3.0), c(-2.0, 2.0))     # *********************************** THIS NEEDS TO BE GENERIC 
    assign('bounds', bounds, globalenv())   
    
    # Create the Swarm
    swarm <- Swarm$new(obj.f, n, d)
    
    
    # If the user requested a contour plot, print it
    # if (contour == TRUE) {
    #     cat("Printing contour plot\n")
    #     
    #     # df <- data.frame()
    #     # df$x <- seq(-2, 2, 100)
    #     # df$y <- seq(-2, 2, 100)
    #     # df$z <- obj.f(list(df$x, df$y))
    #     # 
    #     # gg <- g
    # }
    
    return(list("position" = global.best.position, "solution" = global.best.f.err))
}

################################## MAIN FUNCTION CALL ###################################
# Basic PSO
#bounds = list(c(-3.0, 3.0), c(-2.0, 2.0))
#p <- PSO(obj_function, n=10, d=2, bounds=bounds)

# Ackley's Path Function
# bounds = list(c(-2.0, 2.0), c(-2.0, 2.0), c(-2.0, 2.0), c(-2.0, 2.0), c(-2.0, 2.0))
# print(bounds)


