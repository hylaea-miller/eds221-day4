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


  
  # write a function with conditional
  # Example converting animal ages
 
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


  # Try a dog that is "yellow" years old:
  # animal_age(animal = "dog", age = "yellow") 
 