##Calculating Parameters For Simple Linear Regression Using Particle Swarm Optimization

##Librarying
library(tidyverse)

##Store the data
data <- read_csv("data.csv")

##Variables and Parameters for PSO

num_par <- 50 #number of particles
num_iter <- 50 #number of iterations

particles <- tibble(b0 = c(0), b1 = c(0), ss = c(0), pbestb0 = c(0), pbestb1 = c(0), vec_moveb0 = c(0), vec_moveb1 = c(0), pbestss = c(0)) #creates 'particles', the tibble of all vectors representing the particles. Each row represents a vector. It's initialized with a meaningless zero just to create the columns
#b0 = intercept, b1 = slope, ss = error sum of squares, pbestb0 = best ever intercept for that particle, pbestb1 = best ever slope for that particle, vec_moveb0 = movement vector for that particle (b0 component), vec_moveb1 = movement vector for that particle (b1 component), pbestss = particle best sums of squares

sum_squares <- 0 #used to calculate error sums of squares for each particle. Initialized as a meaningless zero

global_best <- c(0,0,0) #the global best particle, initialized to zero
local_best <- c(0,0,0) #the local best particle, initialized to zero



w <- .7 #inertia weight, a fixed parameter between 0 and 1
c <- .7 #acceleration coefficient, a fixed parameter between 0 and 1

##Initializing the particles with random values

for (i in 1:num_par){ #for loop for number of particles
  
  ##Adds a vector to 'particles' with two values. The first represents B_0 and is generated from a uniform distribution between the minimum and maxium y values in the data. The second represents B_1 and is drawn from a uniform distribution between 0 and 1.
  particles[i, 1] <- runif(n = 1, min = min(data$y), max = max(data$y))
  particles[i, 2] <- runif(n = 1, min = 0, max = 1) 
  
  for (j in 1:nrow(data)){ #for each observation in the data
    
    model_estimate = particles[i, 1] + particles[i, 2]*data[j, 1] #calculate the model estimate: the intercept (b0) given in the particle, plus the slope given in the particle (b1) times the x value in the observation
    
    square_error = (data[j, 2] - model_estimate)^2 #calculate the square error for that observation: the correct y value - the model estimate y value, squared
    
    sum_squares = sum_squares + square_error #add this to the total sum of squares for this particle
  }
  
  particles[i, 3] <- sum_squares #store this sum of squares for the particle
  
  #store the pbestb0 and pbestb1 as the current location of the particle, as this is the first location explored by the particle, as well as the corresponding sum of squares
  particles[i, 4] <- particles[i, 1]
  particles[i, 5] <- particles[i, 2]
  particles[i, 8] <- particles[i, 3]
  
  #initialize each movement vector component to zero
  particles[i, 6] <- 1
  particles[i, 7] <- 1
  
  sum_squares = 0 #reset sums of squares to zero
}

##Finding the first local best
best_row = which.min(particles$ss) #get row number of best particle (that with lowest error sums of squares)
local_best <- tibble_row(particles[best_row,1], particles[best_row,2], particles[best_row,3]) #Set the new local best to the values of the best particle. Local best is a 1x3 tibble
global_best <- local_best #Because this is the first run, the global best will always be the same as the local best

#Run the swarm movement iterations
for (i in 1:num_iter) { #run for 'num_iter' iterations
  
  for (j in 1:num_par){ #for loop for number of particles
    
    #store vec_moveb0 for each particle as w*vec_moveb0 + c*(pbestb0 - b0) + c*(global_bestb0 - b0)
    particles[j, 6] <- w*particles[j, 6] + c*(particles[j, 4] - particles[j, 1]) + c*(global_best[1, 1] - particles[j, 1])
    
    #store vec_moveb1 for each particle as w*vec_moveb1 + c*(pbestb1 - b1) + c*(global_bestb1 - b1)
    particles[j, 7] <- w*particles[j, 7] + c*(particles[j, 5] - particles[j, 2])+ c*(global_best[1, 2] - particles[j, 2])
    
    #update the position of each particle to its current position plus the movement vector component
    particles[j, 1] <- particles[j, 1] + particles[j, 6]
    particles[j, 2] <- particles[j, 2] + particles[j, 7]
    
    #update sums of squares for the new particle locations
    for (k in 1:nrow(data)){ #for each observation in the data
      
      model_estimate = particles[j, 1] + particles[j, 2]*data[k, 1] #calculate the model estimate: the intercept (b0) given in the particle, plus the slope given in the particle (b1) times the x value in the observation
      
      square_error = (data[k, 2] - model_estimate)^2 #calculate the square error for that observation: the correct y value - the model estimate y value, squared
      
      sum_squares = sum_squares + square_error #add this to the total sum of squares for this particle
    }
    
    particles[j, 3] <- sum_squares #store this sum of squares for the particle 
    sum_squares = 0 #reinitialize sum_squares to 0
    
    #recalculate particle best
    if (particles[j, 8] > particles[j, 3]) { #if particle best ss > current ss
      particles[j, 4] <- particles[j, 1] #store pbestb0 as b0
      particles[j, 5] <- particles[j, 2] #store pbestb1 as b1
      particles[j, 8] <- particles[j, 3] #store pbestss as ss
    }
    
  }
  #recalculate local and global best
  best_row = which.min(particles$ss) #get row number of best particle (that with lowest error sums of squares)
  local_best <- tibble_row(particles[best_row,1], particles[best_row,2], particles[best_row,3]) #Set the new local best to the values of the   best particle. Local best is a 1x3 tibble
  
  if (local_best[,3] < global_best[,3]) { #if sums of squares for local best is lower than sums of squares for global best
    global_best <- local_best #then set global best to local best
  }
}

##After all iterations are complete particles should have converged on local minimum. We'll print the global best to output our results
print("PSO Calculated Model")
print(global_best) #print global best 

##Print the closed form model for comparison
print("Closed Form Model")
lm(data = data, y~x) #lm of y ~ x

