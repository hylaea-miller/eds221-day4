# adds up the number of birds and dogs

  
# Defined function
birddog_sum <-  function(bird, dog) {
    pets <- bird + dog
    return(pets)
  }
  
# use it!
total_pets <- birddog_sum(bird = 2, dog = 5)
total_pets <- birddog_sum(2, 5) # same as above

# create a function to double values

double_it <-  function(x) {
  print(2 * x)
}

  double_it(98479832)


  

 
  animal_age <- function(animal, age) {
    if (animal == "dog") {
      print(age * 7)
    } else if (animal == "goat") {
      print(age * 4.7)
    } 
  }
  
  # Try an 8 year old dog.
  animal_age(animal = "dog", age = 8)
  
  # Try a cow & see what is returned. What happens? Consider.
  animal_age(animal = "cow", age = 12)
  
  
  # write an updated version of the animal age function with error messages
  
animal_age_stop <- function(animal, age){
  
    if (!animal %in% c("dog", "goat")) {
      stop("Oops! Animal must be a dog or goat.")
    }
}

if (is.numeric(age) == FALSE) {
  stop("The age must be a number.")
}

if (age <= 0 | age > 50) {
  warning("Are you sure about you animal age?")
}
    
animal_age_stop("dog", 10)


  # Functions meet for loops

# all the dataframes in the function are called df ----> argument df

df_means <- function(df) {
  for (i in 1:ncol(df)) {
    if(is.numeric(df[[i]])) {
      column_name <- colnames(df[i])
      col_mean <- mean(df[[i]], na.rm = TRUE)
      
      print(paste("The mean value of", column_name, "is", round(col_mean,2)))
    }  
   }
  }
    
df_means(df = palmerpenguins::penguins)


# Logistic growth example

#Logistic growth equation

logistic_growth <- function(N0, K, r, time) {
  Nt <- K / (1 + ((K - N0) / N0) * exp(-r * time))
  print(Nt)
}

# check for one set of values 
logistic_growth(N0 = 100, K = 6000, r = 0.27, time = 40)

# working on example just dealing with time
time_vec <- seq(from = 0, to = 35, by = 0.1)

# Apply the logistic growth function to that vector of times (& store):
pop_35 <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec)

# Combining time steps and population size into a dataframe

pop_time_35 <- data.frame(time_vec, pop_35)

library(tidyverse)
# plot it
ggplot(data = pop_time_35, aes (x = time_vec, y = pop_35)) + 
  geom_line(size = 0.5)

# Alternative, with an internal for loop

# Pre-allocate storage for output vector
pop_35_vec <-  vector(mode = "numeric", length = length(time_vec))

# for loop for stepping through time steps

for(i in seq_along(time_vec)) {
  population <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec[i])
  pop_35_vec[i] <- population }

# now, building to estimating across growth rates
r_seq <- seq(from = 0.20, to = 0.40, by = 0.01)

#create a MATRIX to store the outputs in:
out_matrix <-  matrix(nrow = length(time_vec), ncol = length(r_seq))



for (j in seq_along(r_seq)) { 
  for (i in seq_along(time_vec)) { 
    pop <- logistic_growth(N0 = 100, K = 6000, r = r_seq[j], time = time_vec[i])
    out_matrix[i, j] <- pop 
  } 
}

# Data wrangling to plot

# adding time as a variable
out_df <- data.frame(out_matrix, time = time_vec)

# update column names for growth rates
colnames(out_df) <- c(paste0("gr_", r_seq), "time")

# pivot longer to make it tidy
out_df_long <- out_df %>%
  pivot_longer(cols = -time,
               names_to = "growth_rate",
               values_to = "population")

# plot it
ggplot(data = out_df_long, aes(time, population)) +
  geom_line(aes(color = growth_rate)) +
  theme_minimal()
