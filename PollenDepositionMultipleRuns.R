# Parameters
n_plants <- 100   # Number of plants
m_flowers <- 12  # Number of flowers per plant
P_0 <- 0.5         # Initial probability of pollen deposition from the most recent deposition
lambda <- 0.5    # Pollen mixing coefficient
P_plant <- 1   # Probability that pollen is deposited on a flower (Species A)

# Number of simulation runs
n_runs <- 1000

# Initialize matrices to store results
results <- matrix(0, n_runs, 2)  # Columns for Same and Other proportions

# Function to calculate probability based on number of previous depositions (k)
calculate_probability <- function(k, P_0, lambda) {
  return (P_0 + (1 - P_0) * (1 - exp(-lambda * k)))
}

# Loop for multiple runs
for (run in 1:n_runs) {
  # Initialize vectors to store results for this run
  pollen_source <- rep(NA, n_plants * m_flowers)  # Tracks where pollen came from (same plant or different plant)
  deposition_history <- list()  # List to track actual pollen depositions on the bee's body
  deposition_indexes <- list()   # List to track flower indices of depositions on the bee's body
  
  # Simulate bee visiting flowers
  for (plant in 1:n_plants) {
    
    # Each plant gets its own list of valid pollen depositions
    deposition_history[[plant]] <- character()  
    deposition_indexes[[plant]] <- integer(0)   # Initialize vector for deposition indexes
    
    # Initialize a count for valid depositions before the current flower
    valid_depositions_before <- 0
    
    for (flower in 1:m_flowers) {
      
      # Flower index to fill in pollen_source array
      flower_index <- (plant - 1) * m_flowers + flower
      
      # Check if pollen is deposited on the bee's body
      pollen_deposited <- runif(1) < P_plant  # Adjust this for species A or B
      if (pollen_deposited) {
        deposition_history[[plant]] <- c(deposition_history[[plant]], "Same")
        deposition_indexes[[plant]] <- c(deposition_indexes[[plant]], flower_index)  # Record deposition index
      }
      
      # For the first flower of the plant, pollen comes from another plant
      if (flower == 1) {
        pollen_source[flower_index] <- "Other"
      } else {
        # If no valid depositions, pollen comes from another plant
        if (valid_depositions_before == 0) {
          pollen_source[flower_index] <- "Other"
        } else {
          # Calculate the probability of pollen coming from the same plant based on valid depositions
          current_prob_same <- calculate_probability(valid_depositions_before - 1, P_0, lambda)
          prob_other_plant <- 1 - current_prob_same
          
          # Determine pollen source for stigma based on the random draw
          if (runif(1) < prob_other_plant) {
            pollen_source[flower_index] <- "Other"
          } else {
            pollen_source[flower_index] <- "Same"
          }
        }
      }
      
      # Update valid depositions count only after processing the current flower
      if (pollen_deposited) {
        valid_depositions_before <- valid_depositions_before + 1
      }
    }
  }
  
  # Summarize results for this run
  results[run, 1] <- mean(pollen_source == "Same")  # Proportion of same plant
  results[run, 2] <- mean(pollen_source == "Other")  # Proportion of other plants
}

# Calculate average proportions across all runs
average_proportions <- colMeans(results)

# Display the average results
cat("Average proportion of pollen from same plant: ", average_proportions[1], "\n")
cat("Average proportion of pollen from other plants: ", average_proportions[2], "\n")

# Save plot to a file
jpeg("average_pollen_deposition.jpg", width = 500, height = 400)

# Convert proportions to percentages for plotting
average_proportions_percent <- average_proportions * 100

# Extend y-axis range slightly above the largest bar
y_max <- max(average_proportions_percent) * 1.1  # Add 10% buffer above the highest bar

# Plot with narrower bars and y-axis as percentages
barplot(
  average_proportions_percent, 
  main = "Vrsta A", 
  names.arg = c("Geitonogamne oprašitve", "Ksenogamne oprašitve"), 
  col = c("lightblue", "salmon"), 
  ylab = "Delež oprašitev znotraj populacije (%)",
  width = 0.1,  # Adjust the bar width for narrower bars
  ylim = c(0, y_max)  # Set the y-axis limit
)

# Add parameter values below the title
mtext(
  paste("Parametri: n =",n_plants, 
        ", m =", m_flowers, 
        ", P_0 =", P_0, 
        ", lambda =", lambda, 
        ", P_p =", P_plant), 
  side = 3, line = 0.5, cex = 0.8
)

# Close the device to save the file
dev.off()


