getBMI <- function(weight, height){
  # check input values
  if(!is.numeric(weight) || !is.numeric(height)){
    stop("Both weight and height must be numeric.")
  }
  if(weight <= 0 || height <= 0){
    stop("Weight and height must be positive.")
  }
  # convert height to meters
  height <- height / 100

  BMI <- round(weight / (height^2), 1)

  if(BMI < 18.5){
    status <- "You are in the underweight range."
  }
  if(BMI >= 18.5 && BMI < 25){
    status <- "You are within the normal range."
  }
  if(BMI >= 25 && BMI < 30){
    status <- "You are in the overweight range."
  }
  if(BMI >= 30 && BMI < 40){
    status <- "You are in the obese range."
  }
  if(BMI >= 40){
    status <- "You are in the severely obese range."
  }

  result <- list(status, BMI)
  return(result)
}
