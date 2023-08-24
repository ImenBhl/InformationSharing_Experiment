library(ggplot2)
library(scales)

squash_axis <- function(from, to, factor) { 
  # A transformation function that squashes the range of [from, to] by factor on a given axis 
  
  # Args:
  #   from: left end of the axis
  #   to: right end of the axis
  #   factor: the compression factor of the range [from, to]
  #
  # Returns:
  #   A transformation called "squash_axis", which is capsulated by trans_new() function
  
  trans <- function(x) { 
    
    # get indices for the relevant regions
    isq <- x > from & x < to & !is.na(x)
    ito <- x >= to & !is.na(x)
    
    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)
    
    return(x)
  }
  
  inv <- function(x) {
    
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor & !is.na(x)
    ito <- x >= from + (to - from)/factor & !is.na(x)
    
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))
    
    return(x)
  }
  
  # return the transformation
  return(trans_new("squash_axis", trans, inv))
}


library(ggplot2)
library(scales)

squash_axis2 <- function(from, to, factor) { 
  # A transformation function that squashes the range of [from, to] by factor on a given axis 
  
  # Args:
  #   from: left end of the axis
  #   to: right end of the axis
  #   factor: the compression factor of the range [from, to]
  #
  # Returns:
  #   A transformation called "squash_axis", which is capsulated by trans_new() function
  
  trans <- function(x) { 
    
    # get indices for the relevant regions
    isq <- abs(x) > abs(from) & abs(x) < abs(to) & !is.na(x)
    ito <- abs(x) >= abs(to) & !is.na(x)
    
    # apply transformation
    if(x>=0 & !is.na(x)){
      
      x[isq] <- from + (x[isq] - from)/factor
      x[ito] <- from + (to - from)/factor + (x[ito] - to)
    }else if(x<0 & !is.na(x)){
      
      x[isq] <- -(from + (abs(x[isq]) - from)/factor)
      x[ito] <- -(from + (to - from)/factor + (abs(x[ito]) - to))
    }
    
    
    
    return(x)
  }
  
  inv <- function(x) {
    
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor & !is.na(x)
    ito <- x >= from + (to - from)/factor & !is.na(x)
    
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))
    
    return(x)
  }
  
  # return the transformation
  return(trans_new("squash_axis", trans, inv))
}