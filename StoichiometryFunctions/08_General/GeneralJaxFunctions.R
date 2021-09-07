




# LaTeX 08: General
GeneralJax <- function(input_table = NULL){
  
  
  my_names <- colnames(input_table)
  dt_coef3 <- my_names == "Coef3"
  orden_names <- c(1:length(my_names))
  my_pos1 <- 2
  my_pos2 <- orden_names[dt_coef3] - 1
  my_pos3 <- my_pos2 + 1
  my_pos4 <- ncol(input_table)
  
  arrow_column <- rep(" \\rightarrow ", nrow(input_table))

  my_table_mod <- cbind(input_table[ ,c(my_pos1:my_pos2)], arrow_column, 
                        input_table[, c(my_pos3:my_pos4)])
  
  my_vector <- c()
  for (k in 1:nrow(my_table_mod)) my_vector[k] <-  paste0(my_table_mod[k,], collapse ="")
  
  
  for (k in 1:length(my_vector)){     
  
    my_vector[k] <- gsub("[]]", "", my_vector[k])
    my_vector[k] <- gsub("[[]", "_", my_vector[k])
    my_vector[k] <- gsub("_1", "", my_vector[k])
    my_vector[k] <- gsub("[*]", "", my_vector[k])
    my_vector[k] <- gsub("1", "", my_vector[k])
    my_vector[k] <- gsub("[']", "", my_vector[k])
  
    my_vector[k] <- paste0("$$", my_vector[k], "$$", collapse ="")
  
  }  
  
  my_exit <- cbind(input_table[,1], my_vector)
  colnames(my_exit) <- c("Step", "Jax")
  
  
  # Return
  return(my_exit)
  
  
}


GeneralJax2 <- function(input_table = NULL){
  
  
  my_names <- colnames(input_table)
  dt_coef3 <- my_names == "Coef3"
  orden_names <- c(1:length(my_names))
  my_pos1 <- 2
  my_pos2 <- orden_names[dt_coef3] - 1
  my_pos3 <- my_pos2 + 1
  my_pos4 <- ncol(input_table)
  
  arrow_column <- rep("symbol('\\256')", nrow(input_table))
  
  my_table_mod <- cbind(input_table[ ,c(my_pos1:my_pos2)], arrow_column, 
                        input_table[, c(my_pos3:my_pos4)])
  
  my_vector <- c()
  for (k in 1:nrow(my_table_mod)) my_vector[k] <-  paste0(my_table_mod[k,], collapse ="*")
  
  my_vector <- paste0("expression(", my_vector, ")")
  
  
  # for (k in 1:length(my_vector)){
  # 
  #   my_vector[k] <- gsub("phantom()", "", my_vector[k])
  #   my_vector[k] <- gsub("phantom(1)", "", my_vector[k])
  #   my_vector[k] <- gsub("[(1)]", "", my_vector[k])
  #   my_vector[k] <- gsub("[**]", "", my_vector[k])
  #   my_vector[k] <- gsub("[*]", "", my_vector[k])
  #   my_vector[k] <- gsub("1", "", my_vector[k])
  #  my_vector[k] <- gsub("[']", "", my_vector[k])
  # 
  #   my_vector[k] <- paste0("$$", my_vector[k], "$$", collapse ="")
  # 
  # }
  
  my_exit <- cbind(input_table[,1], my_vector)
  colnames(my_exit) <- c("Step", "Jax2")
  
  
  # Return
  return(my_exit)
  
  
}


# GeneralJax(input_table = input_table )