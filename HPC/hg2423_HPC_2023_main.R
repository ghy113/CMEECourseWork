# CMEE 2022 HPC exercises R code main pro forma
# You don't HAVE to use this but it will be very helpful.
# If you opt to write everything yourself from scratch please ensure you use
# EXACTLY the same function and parameter names and beware that you may lose
# marks if it doesn't work properly because of not using the pro-forma.

name <- "Hongyuan Guo"
preferred_name <- "Hongyuan"
email <- "hg2423@imperial.ac.uk"
username <- "hg2423"

                                                                                                                                                                                                                                                                                                                                                                                   
require(ggplot2)                                                                                                                                                                                                                                                                                                                                                                               
# Please remember *not* to clear the workspace here, or anywhere in this file.
# If you do, it'll wipe out your username information that you entered just
# above, and when you use this file as a 'toolbox' as intended it'll also wipe
# away everything you're doing outside of the toolbox.  For example, it would
# wipe away any automarking code that may be running and that would be annoying!

# Question 1
species_richness <- function(community){
  return(length(unique(community)))
}

# Question 2
init_community_max <- function(size){
  return(seq(size))
}

# Question 3
init_community_min <- function(size){
  return(rep(1, size))
}

# ==========    test 1, 2, 3
# print("Test 1, 2, 3")
# print(init_community_max(5))
# print(init_community_min(3))
# richness <- species_richness(init_community_max(5));
# print(richness)
# richness <- species_richness(init_community_min(5));
# print(richness)

# Question 4
choose_two <- function(max_value){
  numbers <- sample(max_value, 2)
  return(numbers)
}

# Question 5
neutral_step <- function(community){
  indices <- choose_two(length(community))
  community[indices[1]] <- community[indices[2]] # The first index dies, the second index reproduces and replaces the first index
  return(community)
}

# Question 6
neutral_generation <- function(community){
  steps <- floor(length(community) / 2)
  for (i in 1:steps) {
    community <- neutral_step(community)
  }
  return(community)
}

# Question 7
neutral_time_series <- function(community,duration)  {
  species_richness_values <- numeric(duration + 1)
  species_richness_values[1] <- species_richness(community)
  for (i in 1:duration) {
    community <- neutral_generation(community)
    species_richness_values[i + 1] <- species_richness(community)
  }
  return(species_richness_values)
}

# Question 8
question_8 <- function() {
  # The initial community is set to the maximum diversity of 100 individuals
  initial_community <- init_community_max(100)
  
  # Run the simulation for 200 generations
  time_series_data <- neutral_time_series(initial_community, 200)
  print(time_series_data)
  png(filename="question_8.png", width = 600, height = 400)
  # plot your graph here
  plot(time_series_data, type="l", main="Neutral Model Simulation Over 200 Generations",
       xlab="Generation", ylab="Species Richness")
  Sys.sleep(0.1)
  dev.off()
  # What state will the system always converge to if you wait long enough? Why is this?
  ans <- "With the increase of iteration time, the system always converges to the state of reduced species richness, 
  because new species cannot be produced in the system, and randomly selected species disappear, 
  a species can only disappear and reproduce, and eventually only one species will be left.";
  return(ans)
}
# Test 4, 5, 6, 7
# print(question_8())

# Question 9
neutral_step_speciation <- function(community,speciation_rate)  {
  # Choose two different individuals
  indices <- choose_two(length(community))
  
  # Decide whether speciation occurs
  if (runif(1) < speciation_rate) {
    # Assign a new species number
    new_species <- max(community) + 1
    community[indices[1]] <- new_species
  } else {
    # Replace the dead individual with the offspring of another
    community[indices[1]] <- community[indices[2]]
  }
  
  return(community)
}

# Question 10
neutral_generation_speciation <- function(community,speciation_rate)  {
  steps <- floor(length(community) / 2)
  for (i in 1:steps) {
    community <- neutral_step_speciation(community, speciation_rate)
  }
  return(community)
}

# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  species_richness_values <- numeric(duration + 1)
  species_richness_values[1] <- length(unique(community))
  
  for (i in 1:duration) {
    community <- neutral_generation_speciation(community, speciation_rate)
    species_richness_values[i + 1] <- length(unique(community))
  }
  
  return(species_richness_values)
}

# Question 12
question_12 <- function()  {
  # Set parameters
  speciation_rate <- 0.1
  community_size <- 100
  generations <- 200
  
  # Initialize communities
  community_max <- init_community_max(community_size)
  community_min <- init_community_min(community_size)
  
  # Run simulations
  time_series_max <- neutral_time_series_speciation(community_max, speciation_rate, generations)
  time_series_min <- neutral_time_series_speciation(community_min, speciation_rate, generations)
  
  
  png(filename="question_12.png", width = 600, height = 400)
  # plot your graph here
  plot(1:(generations + 1), time_series_max, type="l", col="blue", 
       xlab="Generation", ylab="Species Richness", 
       main="Neutral Theory Simulation with Speciation")
  lines(1:(generations + 1), time_series_min, type="l", col="red")
  legend("topright", legend=c("Max Diversity", "Min Diversity"), 
         col=c("blue", "red"), lty=1, cex=0.8)
  Sys.sleep(0.1)
  dev.off()
  # Explain what you found from this plot about the effect of initial conditions. Why does the neutral model simulation give you those particular results
  ans <- "I found that starting from the maximum species diversity, 
  the number of species first declines and then stabilizes; 
  starting from the single species diversity, 
  the number of species first increases and then stabilizes; 
  at the same time, the number of species after the final stabilization
   is almost the same regardless of the maximum species 
   diversity or the single species diversity, and always 
   fluctuates steadily within a certain range. Because the whole 
   community has both the creation of new species and the disappearance
    of old species, there is no discrimination in any community, 
    and eventually tends to be stable."
  return(ans)
}
# test 9, 10, 11, 12
# question_12()

# Question 13
species_abundance <- function(community)  {
  abundance <- table(community)
  res <- as.numeric(sort(abundance, decreasing = TRUE))
  return(res)
}

# Question 14
octaves <- function(abundance_vector) {
  new_vector <- floor(log(abundance_vector) / log(2) + 1)
  return(tabulate(new_vector))
}

# Question 15
sum_vect <- function(x, y) {
  #Prevent x, y from being c()
  if(length(x) == 0) x <- c(0)
  if(length(y) == 0) y <- c(0)
  len <- max(length(x), length(y))
  x_new <- numeric(len)  # Create a new vector of length len padded with 0s
  y_new <- numeric(len)  # Create a new vector of length len padded with 0s
  
  x_new[1:length(x)] <- x  # Copy the value of the original vector x to the new vector x_new
  y_new[1:length(y)] <- y  # Copy the value of the original vector x to the new vector y_new
  
  return(x_new + y_new)
}


# Question 16 
question_16 <- function() {
  speciation_rate <- 0.1
  community_size <- 100
  burn_in_generations <- 200
  simulation_duration <- 2000
  record_interval <- 20
  total_max <- c()
  total_min <- c()
  # Initialize two different communities
  community_max <- init_community_max(community_size)
  community_min <- init_community_min(community_size)
  
  # Perform burning period simulation
  for(i in 1:burn_in_generations){
    community_max = neutral_generation_speciation(community_max, speciation_rate)
    community_min = neutral_generation_speciation(community_min, speciation_rate)
  }
  octave_max = octaves(species_abundance(community_max))
  octave_min = octaves(species_abundance(community_min))
  total_max <- sum_vect(total_max, octave_max)
  total_min <- sum_vect(total_min, octave_min)
  # Continue to 2000 (2000, recorded every 20 times, a total of 100 times)
  for(i in 1:100){ 
    for(i in 1:record_interval){
      community_max = neutral_generation_speciation(community_max, speciation_rate)
      community_min = neutral_generation_speciation(community_min, speciation_rate)
    }
    octave_max = octaves(species_abundance(community_max))
    octave_min = octaves(species_abundance(community_min))
    total_max <- sum_vect(total_max, octave_max)
    total_min <- sum_vect(total_min, octave_min)
  }
  # Calculate average
  mean_max <- total_max / 101
  mean_min <- total_min / 101
  
  # x
  names(mean_max) <- c("1", "2~3", "4~7", "8~15", "16~31", "32~63")
  names(mean_min) <- c("1", "2~3", "4~7", "8~15", "16~31", "32~63")
  
  png(filename="question_16_min.png", width = 600, height = 400)
  barplot(mean_min, xlab = "Number of individuals per species", 
          ylab = "Species Richness",
          main = "Mean species abundance with minimum initial condition")
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="question_16_max.png", width = 600, height = 400)
  barplot(mean_max, xlab = "Number of individuals per species",
          ylab = "Species Richness",
          main = "Mean species abundance with maximum initial condition")
  Sys.sleep(0.1)
  dev.off()
  # Does the initial condition of the system matter? Why is this?
  
  return("Initial conditions are not important, because whether the initial species richness in the system is high or low, the final
  will become a state that matches the system, that is, as the simulation proceeds,
  The cumulative effect of random events gradually exceeds the influence of initial conditions,
  As a result, systems under different initial conditions may tend to have similar species diversity states over the long term.")
}

# test
# print(species_abundance(c(1,5,3,6,5,6,1,1)))
# print(sum_vect(c(1,3),c(1,0,5,2)))
# print(octaves(species_abundance(init_community_max(100))))
# print(octaves(species_abundance(init_community_min(100))))
# question_16()

# Question 17
neutral_cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name) {
  community <- init_community_min(size)
  generation <- 1
  start_time <- proc.time()[3]
  time_series <- c(1) #The initial species richness is 1
  abundance_list <- c(list(octaves(species_abundance(community))))
  while(proc.time()[3] - start_time < wall_time * 60){
    community <- neutral_generation_speciation(community, speciation_rate)
    # burn_in 
    if(generation <= burn_in_generations && generation %% interval_rich == 0){
      time_series <- c(time_series, species_richness(community))
    }
    # Richness recorded throughout the period
    if(generation %% interval_oct == 0){
      abundance_list <- c(abundance_list, list(octaves(species_abundance(community))))
    }
    generation <- generation + 1
  }
  total_time = (proc.time()[3] - start_time) / 60  # Record number of minutes
  save(time_series, abundance_list, community, total_time, speciation_rate,
       size, wall_time, interval_rich, interval_oct, burn_in_generations,
       file=output_file_name)
}

# Questions 18 and 19 involve writing code elsewhere to run your simulations on
# the cluster

# Question 20 
process_neutral_cluster_results <- function() {
  cluster_500 <- 0
  cluster_1000 <- 0
  cluster_2500 <- 0
  cluster_5000 <- 0
  
  total_500 <- c()
  total_1000 <- c()
  total_2500 <- c()
  total_5000 <- c()
  
  for(i in 1:100){
    load(file = paste0("neutral_cluster_", i, ".rda"))
    
    if(i <= 25){size = 500} else if
    (i <= 50){size = 1000} else if
    (i <= 75){size = 2500} else if
    (i <= 100){size = 5000}
    
    burn_in = 80
    
    total <- c()
    for(j in burn_in:length(abundance_list)){
      total <- sum_vect(total, abundance_list[[j]])
    }
    
    assign(paste0("cluster_", size), (get(paste0("cluster_", size)) + (length(abundance_list) - burn_in + 1)))
    assign(paste0("total_", size), sum_vect(get(paste0("total_", size)), total))
  }
  
  mean_500 <- total_500/cluster_500
  mean_1000 <- total_1000/cluster_1000
  mean_2500 <- total_2500/cluster_2500
  mean_5000 <- total_5000/cluster_5000
  combined_results <- list(mean_500, mean_1000, mean_2500, mean_5000) #create your list output here to return
  
  save(combined_results, file = "Combined_results.rda")
  # save results to an .rda file
}

plot_neutral_cluster_results <- function(){
  
  # load combined_results from your rda file
  load("combined_results.rda")
  
  png(filename="plot_neutral_cluster_results.png", width = 600, height = 400)
  
  par(mfrow = c(2, 2))
  size_list <- c('500', '1000', '2500', '5000')
  for (i in 1:4) {
    data <- combined_results[[i]]
    max_height <- max(data) * 1.3
    barplot(data, 
            main = paste("Octave Class Distribution Size ", size_list[i]), 
            xlab = "Abundance Range with octave classes", 
            ylab = "Species Count",
            ylim = c(0, max_height))
  }
  
  Sys.sleep(0.1)
  dev.off()
  
  return(combined_results)
}
# test
process_neutral_cluster_results()
plot_neutral_cluster_results()
# test ==========================================
# neutral_cluster_run(speciation_rate=0.1, size=100, wall_time=0.1, 
# interval_rich=1, interval_oct=0.1, burn_in_generations=200, 
# output_file_name="my_test_file_1.rda")


#===================================================================

#      Section Two: Stochastic demographic population model

#====================================================================

# Question 21 
state_initialise_adult <- function(num_stages,initial_size){
  state <- rep(0, num_stages)  # Create a vector of length num_stages in which all elements are initialized to 0
  state[num_stages] <- initial_size  # Set the value of the last vector 
  return(state)
}
# print(state_initialise_adult(3, 4))
# Question 22
state_initialise_spread <- function(num_stages,initial_size){
  base_count <- floor(initial_size / num_stages) # average number
  state <- rep(base_count, num_stages)  #
  remainder <- initial_size %% num_stages
  
  # Allocate the remainder starting from the youngest stage
  for (i in 1:remainder) {
    state[i] <- state[i] + 1
  }
  
  return(state)
}
# print(state_initialise_spread(num_stages=3, initial_size=8))
# Question 23
deterministic_step <- function(state, projection_matrix){
  return(projection_matrix %*% state)  # Multiplying matrices and vectors
}

# Question 24
deterministic_simulation <- function(initial_state, projection_matrix, simulation_length){
  population_size <- vector("numeric", simulation_length + 1)  # Create a vector to store population sizes
  population_size[1] <- sum(initial_state)  # Initial population size
  
  current_state <- initial_state  # Start with the initial state
  
  for (step in 1:simulation_length) {
    current_state <- deterministic_step(current_state, projection_matrix)  # Apply the deterministic model
    population_size[step + 1] <- sum(current_state)  # Update population size for this step
  }
  
  return(population_size)  
}

# Question 25
question_25 <- function(){
  
  # Define the projection matrix
  growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
                            0.5, 0.4, 0.0, 0.0,
                            0.0, 0.4, 0.7, 0.0,
                            0.0, 0.0, 0.25, 0.4),
                          nrow=4, ncol=4, byrow=TRUE)
  
  reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0),
                                nrow=4, ncol=4, byrow=TRUE)
  
  projection_matrix = reproduction_matrix + growth_matrix
  
  # Define simulation parameters
  simulation_length <- 24
  
  # Initial conditions
  initial_state_adults <- state_initialise_adult(4, 100)  # All adults
  initial_state_spread <- state_initialise_spread(4, 100)  # Evenly spread
  
  # Run simulations
  pop_size_adults <- deterministic_simulation(initial_state_adults, projection_matrix, simulation_length)
  pop_size_spread <- deterministic_simulation(initial_state_spread, projection_matrix, simulation_length)
  
  # Plotting
  png(filename="question_25.png", width = 600, height = 400)
  plot(0:simulation_length, pop_size_adults, type="o", col="blue", xlab="Time Step", ylab="Population Size", main="Population Size Over Time")
  lines(0:simulation_length, pop_size_spread, type="o", col="red")
  legend("topright", legend=c("All Adults", "Evenly Spread"), col=c("blue", "red"), lty=1)
  dev.off()
  # Explain how the initial distribution of a population at different life stages affects the initial and final growth of the population
  return("The initial distribution of the population across different life stages significantly impacts the trajectory of population growth. With all individuals starting in the adult stage, population growth initially lags due to the lack of younger stages contributing to reproduction. However, as these adults reproduce, the population can grow rapidly. In contrast, an even spread across stages leads to a more immediate increase in population size, as individuals in all stages contribute to growth. Eventually, both scenarios may converge to similar growth patterns, but the initial distribution shapes the early dynamics of population growth.")
}
# question_25()
# Question 26
multinomial <- function(pool,probs) {
  # Ensure the probabilities sum to 1
  if (sum(probs) < 1) {
    #Add death to the end
    probs <- c(probs, 1 - sum(probs))
  }
  
  # Draw from the multinomial distribution
  result <- rmultinom(n = 1, size = pool, prob = probs)
  
  # Return the result as a vector
  return(result[-length(result)])  #anything but death
}

# print("multinomial")
# print(multinomial(5, c(0.1, 0.2, 0.3, 0.1)))

# # Question 27
survival_maturation <- function(state,growth_matrix) {
  new_state <- rep(0, length(state))  # Initialize the new state with zeros
  
  for (i in 1:length(state)) {
    # Individuals currently in life stage i
    current_individuals <- state[i]
    
    # Growth probabilities for stage i (including implicit death if sum < 1)
    growth_probs <- growth_matrix[, i]
    
    # Apply the multinomial function to determine the fate of individuals
    transitions <- multinomial(current_individuals, growth_probs)
    
    # Add results to new_state, considering transitions to other life stages
    for (j in 1:length(state)) {
      new_state[j] <- new_state[j] + transitions[j]
    }
  }
  
  return(new_state)
}


# growth_matrix_1 <- matrix(c(1.0, 0.0, 0.0, 0.0,
#                             0, 1.0, 0.0, 0.0,
#                             0.0, 0, 1.0, 0.0,
#                             0.0, 0.0, 0, 1.0),
#                           nrow=4, ncol=4, byrow=TRUE)
# print(survival_maturation(c(1, 2, 3, 4), growth_matrix_1))


# Question 28
random_draw <- function(probability_distribution) {
  # Create a vector of possible values (1 to length of probability_distribution)
  values <- seq_along(probability_distribution)
  # Draw one value based on the provided probabilities
  drawn_value <- sample(values, size = 1, prob = probability_distribution)
  return(drawn_value)
}

# print(random_draw(c(0, 0, 0, 1)))
# print(random_draw(c(0,0.25,0.75)))

# Question 29
stochastic_recruitment <- function(reproduction_matrix, clutch_distribution){
  #Reproduction matrix, clutch size distribution
  # Get the recruitment rate in the reproduction matrix (usually the upper right element)
  recruitment_rate <- reproduction_matrix[1, ncol(reproduction_matrix)]
  
  # Calculate expected (average) clutch size based on clutch size distribution
  # Weighted average nest size = sum(each nest size * corresponding probability)
  expected_clutch_size <- sum(seq_along(clutch_distribution) * clutch_distribution)
  
  # Calculate recruitment probability
  recruitment_probability <- recruitment_rate / expected_clutch_size
  
  return(recruitment_probability)
  
}

# Question 30
offspring_calc <- function(state, clutch_distribution, recruitment_probability){
  # Determine the number of adult individuals in state
  # Assume that the adult individual is in the last element of the state vector
  num_adults <- state[length(state)]
  
  # Use the binomial distribution to generate the number of adults recruited (i.e. the number of adults that lay eggs)
  num_recruits <- rbinom(n = 1, size = num_adults, prob = recruitment_probability)
  
  # Initialize the total number of descendants
  total_offspring <- 0
  
  # For each egg-laying adult, the clutch egg size was extracted
  for (i in 1:num_recruits) {
    clutch_size <- random_draw(clutch_distribution)
    total_offspring <- total_offspring + clutch_size
  }
  
  # Ensures that if no adults lay eggs, the total number of offspring is 0
  total_offspring <- ifelse(num_recruits > 0, total_offspring, 0)
  
  return(total_offspring)
}

# Question 31
stochastic_step <- function(state,growth_matrix,reproduction_matrix,clutch_distribution,recruitment_probability){
  # Application survival and maturation processes
  new_state <- survival_maturation(state, growth_matrix)
  
  # Calculate the number of offspring
  offspring_count <- offspring_calc(state, clutch_distribution, recruitment_probability)
  
  # Add descendants to the first lifecycle stage of the new state
  new_state[1] <- new_state[1] + offspring_count
  
  return(new_state)
}

# Question 32
stochastic_simulation <- function(initial_state,growth_matrix,reproduction_matrix,clutch_distribution,simulation_length){
  # Initialize population size time series vector
  population_size <- numeric(simulation_length + 1)
  population_size[1] <- sum(initial_state)
  
  # Calculate individual recruitment probability
  recruitment_probability <- stochastic_recruitment(reproduction_matrix, clutch_distribution)
  
  # Set current status
  current_state <- initial_state
  
  # Apply stochastic_step in a loop
  for (step in 1:simulation_length) {
    # Apply random steps
    current_state <- stochastic_step(current_state, growth_matrix, reproduction_matrix, clutch_distribution, recruitment_probability)
    
    # Update population size
    population_size[step + 1] <- sum(current_state)
    
    # If the population size drops to 0, terminate the loop early
    if (population_size[step + 1] == 0) {
      population_size[(step + 2):(simulation_length + 1)] <- 0
      break
    }
  }
  
  return(population_size)
}

# Question 33
question_33 <- function(){
  clutch_distribution <- c(0.06,0.08,0.13,0.15,0.16,0.18,0.15,0.06,0.03)
  # Define the projection matrix
  growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
                            0.5, 0.4, 0.0, 0.0,
                            0.0, 0.4, 0.7, 0.0,
                            0.0, 0.0, 0.25, 0.4),
                          nrow=4, ncol=4, byrow=TRUE)
  
  reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0),
                                nrow=4, ncol=4, byrow=TRUE)
  # Define simulation parameters
  simulation_length <- 24
  
  # Initial conditions
  initial_state_adults <- state_initialise_adult(4, 100)  # All adults
  initial_state_spread <- state_initialise_spread(4, 100)  # Evenly spread
  
  # Run simulations
  
  pop_size_adults <- stochastic_simulation(initial_state_adults,growth_matrix,reproduction_matrix,clutch_distribution,simulation_length)
  pop_size_spread <- stochastic_simulation(initial_state_spread,growth_matrix,reproduction_matrix,clutch_distribution,simulation_length)
  
  # Plotting
  png(filename="question_33.png", width = 600, height = 400)
  plot(0:simulation_length, pop_size_adults, type="o", col="blue", xlab="Time Step", ylab="Population Size", main="Population Size Over Time")
  lines(0:simulation_length, pop_size_spread, type="o", col="red")
  legend("topright", legend=c("All Adults", "Evenly Spread"), col=c("blue", "red"), lty=1)
  dev.off()
  # How does the smoothness of the earlier deterministic simulations compare 
  # with these stochastic simulations? Explain why this is the case
  return("Deterministic simulations have smoother curves than stochastic simulations, while stochastic simulations are more fluctuating and messy because stochastic simulations
  incorporates randomness that can lead to unpredictable fluctuations in population size.")
}
# question_33()
# Questions 34 and 35 involve writing code elsewhere to run your simulations on the cluster

# Question 36
question_36 <- function(){
  # Used to store the number of extinctions under 4 conditions
  extinct_counts <- rep(0, 4)
  for(i in 1:100){
    if(i <= 25) condition = 1
    else if(i <= 50) condition = 2
    else if(i <= 75) condition = 3
    else if(i <= 100) condition = 4
    datapath = paste("stochastic_", i, ".rda", sep="")
    load(datapath)
    # For a certain seed, there are 150 simulations, and each simulation has 120 steps.
    for(j in 1:150){
      a <- simu_results[j]
      final_num <- a[[1]][length(a[[1]])]  #Final step value for each simulation
      if(final_num == 0){
        extinct_counts[condition] <- extinct_counts[condition] + 1
      }
    }
  }
  extinct_rate <- extinct_counts / rep(25 * 150, 4)
  print(extinct_counts)
  print(extinct_rate)
  names(extinct_rate) = c("Large 100 adults", "Small 10 adults", "Large 100 spread", "Small 10 spread")
  png(filename="question_36.png", width = 600, height = 400)
  # plot your graph here
  barplot(extinct_rate,
          xlab = "Initial Conditions", ylab = "Proportion of Extinction", ylim = c(0, max(extinct_rate) + 0.01),
          main = "Extinction Rate")
  Sys.sleep(0.1)
  dev.off()
  # Which population was most likely to go extinct? Explain why this is the case.
  return("Populations with initial conditions of a small population of 10 individuals spread across the life stages are most likely to become extinct, followed by
   a small population of 10 adults. First of all, for a population with a small population size, compared with a population with a large population size, it is easier to reduce the number to 0 due to random reasons, so that
   The population is extinct. Secondly, the reproduction of the population is mainly concentrated in the final stage, which means that the greater the number in the final stage, the greater the population's ability to resist randomness, and the initial condition is spread across the life
   Stages have fewer individuals in the final stages of the population, making them more susceptible to extinction.")
}

# Question 37
question_37 <- function(){
  # calculate mean population size at each time step across all simulations on stocastic model
  total_100_spread <- rep(0, 121)
  total_10_spread <- rep(0, 121)
  for(i in 51:100) { # "Large 100 spread", "Small 10 spread"
    # load data
    file_name <- paste("stochastic_", i, ".rda", sep="")
    load(file_name)
    for(t in 1:150){
      if(i <= 75){
        total_100_spread <- total_100_spread + simu_results[t][[1]]
      }
      else{
        total_10_spread <- total_10_spread + simu_results[t][[1]]
      }
    }
  }
  # changing demographic trends
  mean_100_spread <- total_100_spread / (25 * 150)
  mean_10_spread <- total_10_spread / (25 * 150)
  
  # calculate population size time series produced by the deterministic model
  large_initial_state <- state_initialise_spread(4, 100)
  small_initial_state <- state_initialise_spread(4, 10)
  growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
                            0.5, 0.4, 0.0, 0.0,
                            0.0, 0.4, 0.7, 0.0,
                            0.0, 0.0, 0.25, 0.4), nrow=4, ncol=4, byrow=T)
  reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0), nrow=4, ncol=4, byrow=T)
  projection_matrix <- reproduction_matrix + growth_matrix
  
  deterministic_large <- deterministic_simulation(large_initial_state, projection_matrix, 120)
  deterministic_small <- deterministic_simulation(small_initial_state, projection_matrix, 120)
  
  spread_10_deviation = mean_10_spread / deterministic_small
  spread_100_deviation = mean_100_spread / deterministic_large
  
  png(filename="question_37.png", width = 600, height = 400)
  y_min <- min(c(spread_10_deviation, spread_100_deviation))
  y_max <- max(c(spread_10_deviation, spread_100_deviation))
  
  plot(1:121, spread_10_deviation, type = "l", col = "blue", ylim = c(y_min - 0.01, y_max + 0.01),
       main = "Deviation of Stochastic Model from Deterministic Model", xlab = "Time Steps", ylab = "Deviation")
  
  lines(seq(1, 121), spread_100_deviation, type = "l", col = "black")
  abline(h=1, lty=2, col="red")
  legend("topright", legend=c("Initial 10 population spread ", "Initial 100 population spread"), col=c("blue", "black"), lty=1)
  Sys.sleep(0.1)
  dev.off()
  # For which initial condition is it more appropriate to approximate the 
  # ‘average’ behaviour of this stochastic system with a deterministic model? Explain why.
  return("For an initial condition of 100 individuals spread across the life stages is more appropriate to approximate the
   ‘average’ behavior of this stochastic system with a deterministic model because of random fluctuations between individuals in a larger population
   It is small relative to the population, so the average effect reduces the impact of randomness on the overall dynamics.
  This means that when the number of individuals is large, random events have relatively little impact on the overall population, so deterministic models can better predict the average behavior of the population.")
}


# Challenge questions - these are optional, substantially harder, and a maximum
# of 14% is available for doing them. 

# Challenge question A
Challenge_A <- function() {
  require(ggplot2)
  # initialising
  count <- 1 # count for the number of loops
  richness_count <- 0 # count for the number of records of species richness
  speciation_rate <- 0.1
  mean_richness_max <- 100
  mean_richness_min <- 0
  
  community_max <- init_community_max(100)
  community_min <- init_community_min(100)
  sum_richness_max <- species_richness(community_max)
  sum_richness_min <- species_richness(community_min)
  
  standard_deviation <- 1
  
  upper_max <- vector()
  upper_min <- vector()
  lower_max <- vector()
  lower_min <- vector()
  
  # deriving the mean species richness
  for (i in 1:2200){
    # update the current community for each loop
    community_max <- neutral_generation_speciation(community_max,0.1)
    community_min <- neutral_generation_speciation(community_min,0.1)
    
    # record the mean species richness after the burn-n period
    if (count >= 200 && count <= 2200) {
      # record every 20 generations
      if ((count - 200) %% 20 == 0){
        richness_count <- richness_count + 1
        # calculate the current richness
        richness_max <- species_richness(community_max)
        richness_min <- species_richness(community_min)
        # calculate the sum and the mean
        sum_richness_max <- sum(sum_richness_max, richness_max)
        sum_richness_min <- sum(sum_richness_min, richness_min)
        mean_richness_max[richness_count] <- sum_richness_max / richness_count
        mean_richness_min[richness_count] <- sum_richness_min / richness_count
        # Calculating the lower and upper bound of the 97.2% confidence interval
        error <- qnorm(0.972) * standard_deviation / richness_count
        upper_max <- c(upper_max, mean_richness_max[richness_count] + error)
        upper_min <- c(upper_min, mean_richness_min[richness_count] + error)
        lower_max <- c(lower_max, mean_richness_min[richness_count] - error)
        lower_min <- c(lower_min, mean_richness_min[richness_count] - error)
      }
    }
    count <- count + 1
  }
  
  # combining the results to a dataframe
  df_max <- data.frame(mean_richness_max, upper_max, lower_max)
  df_min <- data.frame(mean_richness_min, upper_min, lower_min)
  
  # plotting the graph
  png(filename="Challenge_A_min.png", width = 600, height = 400)
  # plot your graph here
  print(ggplot(data = df_min, aes(x = seq(1, 101), y = mean_richness_min)) +
          geom_point() +
          geom_errorbar(aes(ymin = lower_min, ymax = upper_min)) +
          theme_bw() +
          labs(x = "Time", y = "Mean species richness", 
               title = "Mean species richness against time"))
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="Challenge_A_max.png", width = 600, height = 400)
  # plot your graph here
  print(ggplot(data = df_max, aes(x = 1:101, y = mean_richness_max)) +
          geom_point() +
          geom_errorbar(aes(ymin = lower_max, ymax = upper_max)) +
          theme_bw() +
          labs(x = "Time", y = "Mean species richness", 
               title = "Mean species richness against time"))
  Sys.sleep(0.1)
  dev.off()
  
  return("needs around 25 turns to reach
  the equilibrium, which is 25 * 20(record interval) + 200(burn-in period) = 700
  generations.")
}
# Challenge_A()

# Challenge question B
Challenge_B <- function() {
  # load required package for plotting
  
  
  # initialising
  community <- vector()
  averaged_df <- data.frame()
  
  for (i in 1:100){
    # initialise for each loop
    richness <- vector()
    averaged_richness <- vector()
    
    # computing the initial community with size 100
    community <- sample(x = 1:100, size = i, replace = FALSE)
    richness[1] <- i
    averaged_richness[1] <- i
    
    # generating the whole range averaged time series for 200 generations
    for (j in 1:200){
      # getting the current community
      if (length(community) > 1){
        community <- neutral_generation_speciation(community,
                                                   speciation_rate = 0.1)
      }
      # calculate the current species richness
      richness[j+1] <- species_richness(community)
      # calculate the averaged species richness for each generation
      averaged_richness[j+1] <- sum(richness) / (j+1)
    }
    
    # creating a dataframe of averaged time series for each initial community
    averaged_each <- data.frame(rep(i, 201), seq(1, 201, by = 1), 
                                averaged_richness)
    names(averaged_each) <- c("initial_richness", "generations", 
                              "average_richness")
    averaged_df <- rbind(averaged_df, averaged_each)
  }
  
  png(filename="Challenge_B.png", width = 600, height = 400)
  # plot your graph here
  as.factor(averaged_df$initial_richness)
  print(ggplot(averaged_df, aes(x = generations, y = average_richness)) + 
          geom_line(data = averaged_df, aes(x = generations, y = average_richness,
                                            group = initial_richness, col = initial_richness)) +
          theme_bw() +
          labs(x = "Time (number of generations)", y = "Averaged richness",
               title = "Averaged time series for a whole range of different intial species richness"))
  Sys.sleep(0.1)
  dev.off()
  
}
# Challenge_B()
# Challenge question C
Challenge_C <- function() {
  simulation <- 1
  sizes <- c(500, 1000, 2500, 5000)
  
  means <- list()
  
  for (size in sizes) {
    
    # running total of richness for each community size
    series_total <- c()
    
    # for each simulation of each community size (25 each)
    for (i in 1:25) {
      
      load(paste("neutral_cluster_", simulation, ".rda", sep=""))
      
      series_total <- sum_vect(series_total, time_series)
      
      simulation <- simulation + 1
    }
    mean_Rch_series <- series_total / 25
    means <- c(means, list(mean_Rch_series))
  }
  png(filename="Challenge_C.png", width = 600, height = 400)
  par(mfrow = c(2,2), font.main = 12)
  # plot your graph here
  Sys.sleep(0.1)
  plot(means[[1]], type = "l", main = "Community Size = 500", xlab = "generations", ylab = "Species Richness")
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = T)
  plot(means[[2]], type = "l", main = "Community Size = 1000", xlab = "generations", ylab = "Species Richness")
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = T)
  plot(means[[3]], type = "l", main = "Community Size = 2500", xlab = "generations", ylab = "Species Richness")
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = T)
  plot(means[[4]], type = "l", main = "Community Size = 5000", xlab = "generations", ylab = "Species Richness")
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = T)
  title("Mean Species Richness for 25 Simulations of Neutral Model for Various Community Sizes", outer = T, line = -1)
  dev.off()
  
}
# Challenge_C()

coalescence <- function(J, v) { # runs coalesence simulation for given community size and speciation rate
  lineages <- c(rep(1, J))
  abundances <- c() 
  N <- J 
  theta <- v * ((J-1)/(1-v)) 
  while(N > 1){ 
    j <- ceiling(runif(1, 0, N)) 
    randnum <- runif(1, 0, 1) 
    if (randnum < (theta / (theta + N - 1))){ 
      abundances <- c(abundances, lineages[j]) 
    } else { 
      repeat{ 
        i <- ceiling(runif(1, 0, N)) 
        if (i != j){ 
          break 
        }
      }
      lineages[i] <- lineages[i] + lineages[j] 
    }
    lineages <- lineages[-j] 
    N <- N - 1 
  }
  
  return(c(abundances, lineages)) 
}
# Challenge question D
Challenge_D <- function() {
  my_speciation_rate = 0.004131
  start_time <- proc.time()[3] # start time
  Oct500 <- list() # store octaves
  Oct1000 <- list()
  Oct2500 <- list()
  Oct5000 <- list()
  
  for (i in 1:200){ 
    Oct500[[i]] <- octaves(sort(coalescence(500, my_speciation_rate), decreasing = T))
  }
  for (i in 1:200){
    Oct1000[[i]] <- octaves(sort(coalescence(1000, my_speciation_rate), decreasing = T))
  }
  for (i in 1:200){
    Oct2500[[i]] <- octaves(sort(coalescence(2500, my_speciation_rate), decreasing = T))
  }
  for (i in 1:200){
    Oct5000[[i]] <- octaves(sort(coalescence(5000, my_speciation_rate), decreasing = T))
  }
  
  plotOct500 <- Oct500[[i]] # creates vector to plot
  for (i in 1:(length(Oct500)-1)){
    plotOct500 <- sum_vect(plotOct500, Oct500[[i + 1]])
  }
  
  plotOct1000 <- Oct1000[[i]]
  for (i in 1:(length(Oct500)-1)){
    plotOct1000 <- sum_vect(plotOct1000, Oct1000[[i + 1]])
  }
  
  plotOct2500 <- Oct2500[[i]]
  for (i in 1:(length(Oct2500)-1)){
    plotOct2500 <- sum_vect(plotOct2500, Oct2500[[i + 1]])
  }
  
  plotOct5000 <- Oct5000[[i]]
  for (i in 1:(length(Oct5000)-1)){
    plotOct5000 <- sum_vect(plotOct5000, Oct5000[[i + 1]])
  }
  
  plotOct500 <- plotOct500/length(Oct500) # finds mean octaves
  plotOct1000 <- plotOct1000/length(Oct1000)
  plotOct2500 <- plotOct2500/length(Oct2500)
  plotOct5000 <- plotOct5000/length(Oct5000)
  
  png(filename="Challenge_D.png", width = 600, height = 400)
  par(mfrow = c(2,2), las = 2)
  # plot your graph here
  Sys.sleep(0.1)
  barplot(plotOct500, main = "size = 500", ylab = "Number of species", xlab = "Octave Classes")
  barplot(plotOct1000, main = "size = 1000", ylab = "Number of species", xlab = "Octave Classes")
  barplot(plotOct2500, main = "size = 2500", ylab = "Number of species", xlab = "Octave Classes")
  barplot(plotOct5000, main = "size = 5000", ylab = "Number of species", xlab = "Octave Classes")
  dev.off()
  # How many CPU hours were used on the coalescence simulation and how many on the cluster 
  # to do an equivalent set of simulations? Why were the coalescence simulations so much faster?
  spend_time <-  as.numeric(proc.time()[3] - start_time)
  ans <- "seconds were taken for  Coalescence simulations (2500 repeats), 
  but an analogous set of simulations on the cluster required a total of 11.5 hours 
  to run (11.5 hrs for each of 100 simulations). 
  Only those lineages that are present in the final community are simulated, 
  which makes the coalescence simulations so much faster. However, 
  the conclusions from the two approaches are comparable and with no obvious differences."
  res = paste(spend_time,ans, sep=" ")
  return(res)
}
# Challenge_D()
# Challenge question E
sto_simu_mean <- function(initial_state, projection_matrix, clutch_distribution, simulation_length) {
  # Define function
  mean_stage_cal <- function(stage) {
    # Calculate the mean
    weighted_mean <- sum(1:length(stage) * stage) / sum(stage)
    
    return(weighted_mean)
  }
  
  mean_stage <- c()
  mean_stage[1] <- mean_stage_cal(initial_state) # Adding the initial state to the stage_mean vector
  
  # Calculate recruitment probability
  recruitment_probability <- stochastic_recruitment(projection_matrix, clutch_distribution) 
  
  for (i in 1:simulation_length) {
    # Random sampling
    new_state <- stochastic_step(initial_state, projection_matrix, clutch_distribution, recruitment_probability)
    initial_state <- new_state
    mean_stage[i+1] <- mean_stage_cal(new_state) 
    
  }
  return(mean_stage)
}




calculate_mean_life <- function(stage){
  return (sum(1:length(stage) * stage) / sum(stage))
}
# print(calculate_mean_life(c(10, 12, 12, 10)))

stochastic_mean_simulation <- function(initial_state,growth_matrix,reproduction_matrix,clutch_distribution,simulation_length){
  # Initialize population size time series vector
  mean_stage <- c()
  mean_stage[1] <- calculate_mean_life(initial_state)
  
  # Calculate individual recruitment probability
  recruitment_probability <- stochastic_recruitment(reproduction_matrix, clutch_distribution)
  
  # Set current status
  current_state <- initial_state
  
  # Apply stochastic_step in a loop
  for (step in 1:simulation_length) {
    # Apply random steps
    current_state <- stochastic_step(current_state, growth_matrix, reproduction_matrix, clutch_distribution, recruitment_probability)
    
    # Update population size
    mean_stage[step + 1] <- calculate_mean_life(current_state)
  }
  
  return(mean_stage)
}

Challenge_E <- function(){
  clutch_distribution <- c(0.06,0.08,0.13,0.15,0.16,0.18,0.15,0.06,0.03)
  # Define the projection matrix
  growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
                            0.5, 0.4, 0.0, 0.0,
                            0.0, 0.4, 0.7, 0.0,
                            0.0, 0.0, 0.25, 0.4),
                          nrow=4, ncol=4, byrow=TRUE)
  
  reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0),
                                nrow=4, ncol=4, byrow=TRUE)
  # Define simulation parameters
  simulation_length <- 24
  
  # Initial conditions
  initial_state_adults <- state_initialise_adult(4, 100)  # All adults
  initial_state_spread <- state_initialise_spread(4, 100)  # Evenly spread
  
  # Run simulations
  
  mean_stage_adults <- stochastic_mean_simulation(initial_state_adults,growth_matrix,reproduction_matrix,clutch_distribution,simulation_length)
  mean_stage_spread <- stochastic_mean_simulation(initial_state_spread,growth_matrix,reproduction_matrix,clutch_distribution,simulation_length)
  
  # Plotting
  png(filename="Challenge_E.png", width = 600, height = 400)
  plot(0:simulation_length, mean_stage_adults, type="o", col="blue", xlab="Time Step", ylab="Mean Life", main="Mean Life Over Time")
  lines(0:simulation_length, mean_stage_spread, type="o", col="red")
  legend("topright", legend=c("All Adults", "Evenly Spread"), col=c("blue", "red"), lty=1)
  dev.off()
  # How do the two simulations differ in terms of the initial behaviour of the graph of 
  # mean life stage against time? Explain why we observe these dynamics
  return("The average life stage of 'all adults' decreases rapidly as the simulation progresses because stage 4 adults will produce new individuals, but there will be no new stage 4 adults.
  The timely replenishment of individuals leads to a rapid decline in the overall average life span. In experiments with evenly distributed individuals as the initial condition, the third stage in the initial population will also grow into the fourth stage, and the fourth stage
  will die, and the overall average lifespan will decrease, but no initial conditions are apparent for all adults in the population.")
}


# Challenge question F
Challenge_F <- function(){
  
  simulation_number <- c()
  initial_condition <- c()
  time_step <- c()
  population_size <- c()
  
  population_size_df <- data.frame(simulation_number, initial_condition, time_step, population_size, 
                                   stringsAsFactors = FALSE)
  population_size_df1 <- data.frame(simulation_number, initial_condition, time_step, population_size, 
                                    stringsAsFactors = FALSE)
  population_size_df2 <- data.frame(simulation_number, initial_condition, time_step, population_size, 
                                    stringsAsFactors = FALSE)
  population_size_df3 <- data.frame(simulation_number, initial_condition, time_step, population_size, 
                                    stringsAsFactors = FALSE)
  population_size_df4 <- data.frame(simulation_number, initial_condition, time_step, population_size, 
                                    stringsAsFactors = FALSE)
  
  # Generate all data frames
  for(i in 1:100){
    load(paste("stochastic_", i, ".rda", sep = "")) 
    results <- simu_results
    # Calculate each survival condition separately
    if(i <= 25){
      for(j in 1:length(results)){
        population_size_df[j,1] <- i  # Fill simulation_number
        population_size_df[j,2] <- "small adult"  # Fill initial_condition
        population_size_df[j,3] <- j-1  # Fill time_step
        population_size_df[j,4] <- results[j] # Fill population_size
      }
      population_size_df1 <- rbind(population_size_df1,population_size_df)
      
    }
    
    else if (i <= 50){
      for(j in 1:length(results)){
        population_size_df[j,1] <- i  # Fill simulation_number
        population_size_df[j,2] <- "large adult"  # FIll initial_condition
        population_size_df[j,3] <- j-1  # Fill time_step
        population_size_df[j,4] <- results[j] # Fill population_size
      }
      population_size_df2 <- rbind(population_size_df2,population_size_df)
    }
    
    else if (i <= 75){
      for(j in 1:length(results)){
        population_size_df[j,1] <- i  # Fill simulation_number
        population_size_df[j,2] <- "small evenly spread"  # Fill initial_condition
        population_size_df[j,3] <- j-1  # Fill time_step
        population_size_df[j,4] <- results[j] # Fill population_size
      }
      population_size_df3 <- rbind(population_size_df3,population_size_df)
    }
    
    else{
      for(j in 1:length(results)){
        population_size_df[j,1] <- i  # Fill simulation_number
        population_size_df[j,2] <- "large evenly spread "  # Fill initial_condition
        population_size_df[j,3] <- j-1  # Fill time_step
        population_size_df[j,4] <- results[j] # Fill population_size
      }
      population_size_df4 <- rbind(population_size_df4,population_size_df)
    }
  }
  # Aggregate the results of all four simulations into population_size_df
  population_size_df <- data.frame()
  population_size_df <- rbind(population_size_df1,population_size_df2,population_size_df3,population_size_df4)
  colnames(population_size_df) <- c("simulation_number", "initial_condition", "time_step", "population_size")
  
  # Plot
  library(ggplot2)
  ggplot(data = population_size_df, aes(x = time_step, y = population_size, group = simulation_number, 
                                        colour = initial_condition)) +
    geom_line(alpha = 0.1) +
    xlab("Time step") +
    ylab("Population size") +
    ggtitle("Population size time series for all simulations")
  
  
  ggsave("Challenge_F.png")
  
}
# Challenge_F()