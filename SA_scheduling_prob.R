# Macierz czasow procesowania
processing_times <- matrix(sample(1:4, size=6, replace = TRUE), ncol = 2)

# Funkcja kosztu (celu), zwracajaca czas cyklu 
objective_function <- function(job_order, processing_times) {
  n_jobs <- nrow(processing_times)
  n_machines <- ncol(processing_times)
  
  completion_times <- matrix(0, nrow = n_jobs, ncol = n_machines)
  
  for (i in 1:n_jobs) {
    job <- job_order[i]
    
    if(i==1){
      # Przypisanie pierwszego zadania z kolejki do pierwszej maszyny
      completion_times[i, 1] <- processing_times[job, 1]
      # Przypisanie pierwszego zadania z kolejki do drugiej maszyny
      completion_times[i, 2] <- completion_times[i, 1] + processing_times[job, 2]
    } else{
      # Przypisanie kolejnego zadania z kolejki do pierwszej maszyny
      completion_times[i, 1] <- processing_times[job, 1] + completion_times[i-1, 1]
      # Przypisanie kolejnego zadania z kolejki do drugiej maszyny
      if(completion_times[i, 1]>completion_times[i, 2]){
        completion_times[i, 2] <- completion_times[i, 1] + processing_times[job, 2]
      } else{
        completion_times[i, 2] <- completion_times[i-1, 2] + processing_times[job, 2]
      }
        
    }
   
  }
  
  # Policzenie czasu cyklu
  total_completion_time <- completion_times[n_jobs,2]
  
  return(total_completion_time)
}

# Definicja pierwszego rozwiazania (losowy wybor zadan)
initial_solution <- sample(nrow(processing_times))

# Okreslenie parametrow do algorytmu SA
initial_temperature <- 100
final_temperature <- 0.1
cooling_factor <- 0.95

# Okreslenie maksymalnej liczby iteracji
iterations <- 100

# Definicja funkcji akceptacji
acceptance_probability <- function(delta, temperature) {
  if (delta < 0) {
    return(1)
  } else {
    return(exp(-delta / temperature))
  }
}

# Implementacja algorytmu SA
simulated_annealing <- function(processing_times, initial_solution, initial_temperature, final_temperature, cooling_factor, iterations) {
  current_solution <- initial_solution
  best_solution <- initial_solution
  current_temperature <- initial_temperature
  
  while (current_temperature > final_temperature) {
    for (i in 1:iterations) {
      # Wybor rozwiazania kandydata
      neighbor_solution <- sample(nrow(processing_times))
      
      # Wartosci funkcji celu w rozwiazaniu obecnym i kandydacie
      current_value <- objective_function(current_solution, processing_times)
      neighbor_value <- objective_function(neighbor_solution, processing_times)
      
      # Roznica wartosci funkcji celu
      delta <- neighbor_value - current_value
      
      # Warunek przejscia do gorszego rozwiazania
      acceptance_prob <- acceptance_probability(delta, current_temperature)
      if (runif(1) < acceptance_prob) {
        current_solution <- neighbor_solution
      }
      
      # Warunek aktualizacji najlepszego rozwiazania
      if (neighbor_value < objective_function(best_solution, processing_times)) {
        best_solution <- neighbor_solution
      }
    }
    
    # Zmiena temperatury w kolejnych iteracjach
    current_temperature <- current_temperature * cooling_factor
  }
  
  return(best_solution)
}

# Policzenie algorytmu SA dla danych wejsciowych
optimized_solution <- simulated_annealing(processing_times, initial_solution, initial_temperature, final_temperature, cooling_factor, iterations)

# Zwrocenie najlepszego rozwiazania i wartosci funkcji celu
print(processing_times)
print(optimized_solution)
print(objective_function(optimized_solution, processing_times))
