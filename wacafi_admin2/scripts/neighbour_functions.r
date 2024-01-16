#' Add list of neighbors to SF object
#'
#' @param sf_object An object of class 'SF'
#' @param unique_id_col A unique column to serve as an identifier
#' @return String column with all neighbors
#' @examples
#' example_sf <- nbrs_list(sf_object = example_sf, unique_id_col = "country")
#' example_sf <- nbrs_list(example_sf, "country")
#'
#' @rdname nbrs_list
#' @export
nbrs_list <- function(sf_object, unique_id_col){ #
  requireNamespace("spdep", quietly = TRUE)
  
  df_1 <- {sf_object} # creating copy of sf_object, will be used from here
  df_1 <- df_1[,c({unique_id_col},"geometry")]
  df_1 <- unique(df_1)
  final_df_1 <- data.frame()
  
  for (i in unique(df_1)){
    temp_df <- df_1
    temp_df$unique_id <-  eval(parse(text=paste0("temp_df$",{unique_id_col},sep="")))
    rownames(temp_df) <- temp_df$unique_id
    
    temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
    
    temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
    temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
    
    neighbor_list_df <- data.frame() # creating empty shell that will later gain the data from the for loop
    
    for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
      df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
      df_2$nbr_id <- df_2[,1] # giving column a usable name
      df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
      neighbor_list_df <- as.data.frame(rbind(neighbor_list_df,df_2)) # binding to empty data.frame made on line 16
    }
    neighbor_list_df <- as.data.frame(neighbor_list_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
    
    neighbor_list_df$unique_id <- temp_names_df$unique_id[match(neighbor_list_df$id, temp_names_df$new_temp_id)]
    
    neighbor_list_df$nbr_id<-temp_names_df$unique_id[match(neighbor_list_df$nbr_id, temp_names_df$new_temp_id)]
    
    neighbor_list_df <- aggregate(neighbor_list_df$nbr_id, by=list(unique_id=neighbor_list_df$unique_id), FUN=toString)
    names(neighbor_list_df)[names(neighbor_list_df)=="x"] <- "all_nbrs"
    
    final_df_1 <- rbind(final_df_1, neighbor_list_df)
  }
  final_df_1 <- merge({sf_object}, final_df_1, by.x=c(unique_id_col), by.y=c("unique_id"))
  final_df_1<-unique(final_df_1)
  return(final_df_1)
  
}

#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________

#' Add sum of neighbors values to SF object
#'
#' @param sf_object An object of class 'SF'
#' @param unique_id_col A string identifying a unique column to serve as an identifier
#' @param variable A string identifying a numeric variable
#' @param date_col (Optional) A string identifying a variable for date, use if data is over multiple time periods.
#'
#' @return Column string of neighbors
#'
#' @examples
#' panel_example_sf <- nbrs_sum(sf_object = panel_example_sf, unique_id_col = "country", variable = "gdp", date_col="year")
#' panel_example_sf <- nbrs_sum(panel_example_sf, "country", "gdp", "year")

#' cross_section_example_sf <- nbrs_sum(sf_object = cross_section_example_sf, unique_id_col = "country", variable = "gdp")
#' cross_section_example_sf <- nbrs_sum(cross_section_example_sf, "country", "gdp")
#' @rdname nbrs_sum
#' @export
nbrs_sum <- function(sf_object, unique_id_col, variable, date_col){ #
  requireNamespace("spdep", quietly = TRUE)
  if(missing(date_col)){
    df_1 <- {sf_object} # creating copy of sf_object, will be used from here
    
    df_1$nbr_value <- eval(parse(text=paste0("df_1$",{variable},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    temp_df <- df_1
    temp_df$unique_id <-  eval(parse(text=paste("temp_df$",{unique_id_col},sep="")))
    rownames(temp_df) <- temp_df$unique_id
    
    temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
    
    temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
    temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
    
    nbr_sums_df <- data.frame() # creating empty shell that will later gain the data from the for loop
    
    for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
      df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
      df_2$nbr_id <- df_2[,1] # giving column a usable name
      df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
      nbr_sums_df <- as.data.frame(rbind(nbr_sums_df,df_2)) # binding to empty data.frame made on line 16
    }
    nbr_sums_df <- as.data.frame(nbr_sums_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
    
    nbr_sums_df$unique_id <- temp_names_df$unique_id[match(nbr_sums_df$id, temp_names_df$new_temp_id)]
    
    nbr_sums_df$nbr_id<-temp_names_df$unique_id[match(nbr_sums_df$nbr_id, temp_names_df$new_temp_id)]
    
    nbr_sums_df$nbr_value<-temp_df$nbr_value[match(nbr_sums_df$nbr_id, temp_df$unique_id )]
    
    nbr_sums_df <- aggregate(nbr_sums_df$nbr_value, by=list(unique_id=nbr_sums_df$unique_id), FUN=sum)
    names(nbr_sums_df)[names(nbr_sums_df)=="x"] <- paste0("nbrs_",{variable},"_sum")
    
    final_df_1 <- merge({sf_object},nbr_sums_df, by.x=c(unique_id_col),by.y=c("unique_id"))
    return(final_df_1)
  }
  else{
    df_1 <- {sf_object} # creating copy of sf_object, will be used from here
    df_1$nbr_value <- eval(parse(text=paste("df_1$",{variable},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    df_1$date <- eval(parse(text=paste("df_1$",{date_col},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    final_df_1 <- data.frame()
    
    for (i in unique(df_1$date)){
      temp_df <- subset(df_1, date == i)
      temp_df$unique_id <-  eval(parse(text=paste("temp_df$",{unique_id_col},sep="")))
      rownames(temp_df) <- temp_df$unique_id
      
      temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
      
      temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
      temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
      
      nbr_sums_df <- data.frame() # creating empty shell that will later gain the data from the for loop
      
      for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
        df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
        df_2$nbr_id <- df_2[,1] # giving column a usable name
        df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
        nbr_sums_df <- as.data.frame(rbind(nbr_sums_df,df_2)) # binding to empty data.frame made on line 16
      }
      nbr_sums_df <- as.data.frame(nbr_sums_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
      
      nbr_sums_df$unique_id <- temp_names_df$unique_id[match(nbr_sums_df$id, temp_names_df$new_temp_id)]
      
      nbr_sums_df$nbr_id<-temp_names_df$unique_id[match(nbr_sums_df$nbr_id, temp_names_df$new_temp_id)]
      
      nbr_sums_df$nbr_value<-temp_df$nbr_value[match(nbr_sums_df$nbr_id, temp_df$unique_id )]
      
      nbr_sums_df <- aggregate(nbr_sums_df$nbr_value, by=list(unique_id=nbr_sums_df$unique_id), FUN=sum)
      names(nbr_sums_df)[names(nbr_sums_df)=="x"] <- paste0("nbrs_",{variable},"_sum")
      
      nbr_sums_df$date <- i
      final_df_1 <- rbind(final_df_1, nbr_sums_df)
      print(paste("Calculating sum", variable, "of neighbors for each observation for date:", i))
      
    }
    final_df_1 <- merge({sf_object},final_df_1, by.x=c(unique_id_col,date_col),by.y=c("unique_id","date"))
    return(final_df_1)
  }
}

#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________

#' Add mean of neighbors values to SF object
#'
#' @param sf_object An object of class 'SF'
#' @param unique_id_col A string identifying a unique column to serve as an identifier
#' @param variable A string identifying a numeric variable
#' @param date_col (Optional) A string identifying a variable for date, use if data is over multiple time periods.
#'
#' @return Column string of neighbors
#'
#' @examples
#' panel_example_sf <- nbrs_mean(sf_object = panel_example_sf, unique_id_col = "country", variable = "gdp", date_col="year")
#' panel_example_sf <- nbrs_mean(panel_example_sf, "country", "gdp", "year")

#' cross_section_example_sf <- nbrs_mean(sf_object = cross_section_example_sf, unique_id_col = "country", variable = "gdp")
#' cross_section_example_sf <- nbrs_mean(cross_section_example_sf, "country", "gdp")
#'
#' @rdname nbrs_mean
#' @export
nbrs_mean <- function(sf_object, unique_id_col, date_col, variable){ #
  requireNamespace("spdep", quietly = TRUE)
  if(missing(date_col)){
    df_1 <- {sf_object} # creating copy of sf_object, will be used from here
    
    df_1$nbr_value <- eval(parse(text=paste0("df_1$",{variable},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    temp_df <- df_1
    temp_df$unique_id <-  eval(parse(text=paste("temp_df$",{unique_id_col},sep="")))
    rownames(temp_df) <- temp_df$unique_id
    
    temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
    
    temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
    temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
    
    nbrs_mean_df <- data.frame() # creating empty shell that will later gain the data from the for loop
    
    for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
      df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
      df_2$nbr_id <- df_2[,1] # giving column a usable name
      df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
      nbrs_mean_df <- as.data.frame(rbind(nbrs_mean_df,df_2)) # binding to empty data.frame made on line 16
    }
    nbrs_mean_df <- as.data.frame(nbrs_mean_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
    
    nbrs_mean_df$unique_id <- temp_names_df$unique_id[match(nbrs_mean_df$id, temp_names_df$new_temp_id)]
    
    nbrs_mean_df$nbr_id<-temp_names_df$unique_id[match(nbrs_mean_df$nbr_id, temp_names_df$new_temp_id)]
    
    nbrs_mean_df$nbr_value<-temp_df$nbr_value[match(nbrs_mean_df$nbr_id, temp_df$unique_id )]
    
    nbrs_mean_df <- aggregate(nbrs_mean_df$nbr_value, by=list(unique_id=nbrs_mean_df$unique_id), FUN=mean)
    names(nbrs_mean_df)[names(nbrs_mean_df)=="x"] <- paste0("nbrs_",{variable},"_mean")
    
    final_df_1 <- merge({sf_object},nbrs_mean_df, by.x=c(unique_id_col),by.y=c("unique_id"))
    return(final_df_1)
  }
  else{
    df_1 <- {sf_object} # creating copy of sf_object, will be used from here
    df_1$nbr_value <- eval(parse(text=paste("df_1$",{variable},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    df_1$date <- eval(parse(text=paste("df_1$",{date_col},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    final_df_1 <- data.frame()
    
    for (i in unique(df_1$date)){
      temp_df <- subset(df_1, date == i)
      temp_df$unique_id <-  eval(parse(text=paste("temp_df$",{unique_id_col},sep="")))
      rownames(temp_df) <- temp_df$unique_id
      
      temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
      
      temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
      temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
      
      nbrs_mean_df <- data.frame() # creating empty shell that will later gain the data from the for loop
      
      for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
        df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
        df_2$nbr_id <- df_2[,1] # giving column a usable name
        df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
        nbrs_mean_df <- as.data.frame(rbind(nbrs_mean_df,df_2)) # binding to empty data.frame made on line 16
      }
      nbrs_mean_df <- as.data.frame(nbrs_mean_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
      
      nbrs_mean_df$unique_id <- temp_names_df$unique_id[match(nbrs_mean_df$id, temp_names_df$new_temp_id)]
      
      nbrs_mean_df$nbr_id<-temp_names_df$unique_id[match(nbrs_mean_df$nbr_id, temp_names_df$new_temp_id)]
      
      nbrs_mean_df$nbr_value<-temp_df$nbr_value[match(nbrs_mean_df$nbr_id, temp_df$unique_id )]
      
      nbrs_mean_df <- aggregate(nbrs_mean_df$nbr_value, by=list(unique_id=nbrs_mean_df$unique_id), FUN=mean)
      names(nbrs_mean_df)[names(nbrs_mean_df)=="x"] <- paste0("nbrs_",{variable},"_mean")
      
      nbrs_mean_df$date <- i
      final_df_1 <- rbind(final_df_1, nbrs_mean_df)
      print(paste("Calculating mean", variable, "of neighbors for each observation for date:", i))
    }
    final_df_1 <- merge({sf_object}, final_df_1, by.x=c(unique_id_col,date_col),by.y=c("unique_id","date"))
    return(final_df_1)
  }
}


#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________

#' Add standard deviation of neighbors values to SF object
#'
#' @param sf_object An object of class 'SF'
#' @param unique_id_col A string identifying a unique column to serve as an identifier
#' @param variable A string identifying a numeric variable
#' @param date_col (Optional) A string identifying a variable for date, use if data is over multiple time periods.
#'
#' @return Column string of neighbors
#'
#' @examples
#' panel_example_sf <- nbrs_stdev(sf_object = panel_example_sf, unique_id_col = "country", variable = "gdp", date_col="year")
#' panel_example_sf <- nbrs_stdev(panel_example_sf, "country", "gdp", "year")

#' cross_section_example_sf <- nbrs_stdev(sf_object = cross_section_example_sf, unique_id_col = "country", variable = "gdp")
#' cross_section_example_sf <- nbrs_stdev(cross_section_example_sf, "country", "gdp")
#'
#' @rdname nbrs_stdev
#' @export
nbrs_stdev <- function(sf_object, unique_id_col, date_col, variable){ #
  requireNamespace("spdep", quietly = TRUE)
  if(missing(date_col)){
    df_1 <- {sf_object} # creating copy of sf_object, will be used from here
    
    df_1$nbr_value <- eval(parse(text=paste0("df_1$",{variable},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    temp_df <- df_1
    temp_df$unique_id <-  eval(parse(text=paste("temp_df$",{unique_id_col},sep="")))
    rownames(temp_df) <- temp_df$unique_id
    
    temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
    
    temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
    temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
    
    nbrs_stdev_df <- data.frame() # creating empty shell that will later gain the data from the for loop
    
    for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
      df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
      df_2$nbr_id <- df_2[,1] # giving column a usable name
      df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
      nbrs_stdev_df <- as.data.frame(rbind(nbrs_stdev_df,df_2)) # binding to empty data.frame made on line 16
    }
    nbrs_stdev_df <- as.data.frame(nbrs_stdev_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
    
    nbrs_stdev_df$unique_id <- temp_names_df$unique_id[match(nbrs_stdev_df$id, temp_names_df$new_temp_id)]
    
    nbrs_stdev_df$nbr_id<-temp_names_df$unique_id[match(nbrs_stdev_df$nbr_id, temp_names_df$new_temp_id)]
    
    nbrs_stdev_df$nbr_value<-temp_df$nbr_value[match(nbrs_stdev_df$nbr_id, temp_df$unique_id )]
    
    nbrs_stdev_df <- aggregate(nbrs_stdev_df$nbr_value, by=list(unique_id=nbrs_stdev_df$unique_id), FUN=sd)
    names(nbrs_stdev_df)[names(nbrs_stdev_df)=="x"] <- paste0("nbrs_",{variable},"_stdev")
    
    final_df_1 <- merge({sf_object},nbrs_stdev_df, by.x=c(unique_id_col),by.y=c("unique_id"))
    return(final_df_1)
  }
  else{
    df_1 <- {sf_object} # creating copy of sf_object, will be used from here
    df_1$nbr_value <- eval(parse(text=paste("df_1$",{variable},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    df_1$date <- eval(parse(text=paste("df_1$",{date_col},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    final_df_1 <- data.frame()
    
    for (i in unique(df_1$date)){
      temp_df <- subset(df_1, date == i)
      temp_df$unique_id <-  eval(parse(text=paste("temp_df$",{unique_id_col},sep="")))
      rownames(temp_df) <- temp_df$unique_id
      
      temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
      
      temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
      temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
      
      nbrs_stdev_df <- data.frame() # creating empty shell that will later gain the data from the for loop
      
      for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
        df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
        df_2$nbr_id <- df_2[,1] # giving column a usable name
        df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
        nbrs_stdev_df <- as.data.frame(rbind(nbrs_stdev_df,df_2)) # binding to empty data.frame made on line 16
      }
      nbrs_stdev_df <- as.data.frame(nbrs_stdev_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
      
      nbrs_stdev_df$unique_id <- temp_names_df$unique_id[match(nbrs_stdev_df$id, temp_names_df$new_temp_id)]
      
      nbrs_stdev_df$nbr_id<-temp_names_df$unique_id[match(nbrs_stdev_df$nbr_id, temp_names_df$new_temp_id)]
      
      nbrs_stdev_df$nbr_value<-temp_df$nbr_value[match(nbrs_stdev_df$nbr_id, temp_df$unique_id )]
      
      nbrs_stdev_df <- aggregate(nbrs_stdev_df$nbr_value, by=list(unique_id=nbrs_stdev_df$unique_id), FUN=sd)
      names(nbrs_stdev_df)[names(nbrs_stdev_df)=="x"] <- paste0("nbrs_",{variable},"_stdev")
      
      nbrs_stdev_df$date <- i
      final_df_1 <- rbind(final_df_1, nbrs_stdev_df)
      print(paste("Calculating", variable, "standard deviation of neighbors for each observation for date:", i))
    }
    final_df_1 <- merge({sf_object}, final_df_1, by.x=c(unique_id_col,date_col),by.y=c("unique_id","date"))
    return(final_df_1)
  }
}


#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________

#' Add median of neighbors values to SF object
#'
#' @param sf_object An object of class 'SF'
#' @param unique_id_col A string identifying a unique column to serve as an identifier
#' @param variable A string identifying a numeric variable
#' @param date_col (Optional) A string identifying a variable for date, use if data is over multiple time periods.
#'
#' @return Column string of neighbors
#'
#' @examples
#' panel_example_sf <- nbrs_median(sf_object = panel_example_sf, unique_id_col = "country", variable = "gdp", date_col="year")
#' panel_example_sf <- nbrs_median(panel_example_sf, "country", "gdp", "year")

#' cross_section_example_sf <- nbrs_median(sf_object = cross_section_example_sf, unique_id_col = "country", variable = "gdp")
#' cross_section_example_sf <- nbrs_median(cross_section_example_sf, "country", "gdp")
#'
#' @rdname nbrs_median
#' @export
nbrs_median <- function(sf_object, unique_id_col, date_col, variable){ #
  requireNamespace("spdep", quietly = TRUE)
  if(missing(date_col)){
    df_1 <- {sf_object} # creating copy of sf_object, will be used from here
    
    df_1$nbr_value <- eval(parse(text=paste0("df_1$",{variable},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    temp_df <- df_1
    temp_df$unique_id <-  eval(parse(text=paste("temp_df$",{unique_id_col},sep="")))
    rownames(temp_df) <- temp_df$unique_id
    
    temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
    
    temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
    temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
    
    nbrs_median_df <- data.frame() # creating empty shell that will later gain the data from the for loop
    
    for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
      df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
      df_2$nbr_id <- df_2[,1] # giving column a usable name
      df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
      nbrs_median_df <- as.data.frame(rbind(nbrs_median_df,df_2)) # binding to empty data.frame made on line 16
    }
    nbrs_median_df <- as.data.frame(nbrs_median_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
    
    nbrs_median_df$unique_id <- temp_names_df$unique_id[match(nbrs_median_df$id, temp_names_df$new_temp_id)]
    
    nbrs_median_df$nbr_id<-temp_names_df$unique_id[match(nbrs_median_df$nbr_id, temp_names_df$new_temp_id)]
    
    nbrs_median_df$nbr_value<-temp_df$nbr_value[match(nbrs_median_df$nbr_id, temp_df$unique_id )]
    
    nbrs_median_df <- aggregate(nbrs_median_df$nbr_value, by=list(unique_id=nbrs_median_df$unique_id), FUN=median)
    names(nbrs_median_df)[names(nbrs_median_df)=="x"] <- paste0("nbrs_",{variable},"_median")
    
    final_df_1 <- merge({sf_object},nbrs_median_df, by.x=c(unique_id_col),by.y=c("unique_id"))
    return(final_df_1)
  }
  else{
    df_1 <- {sf_object} # creating copy of sf_object, will be used from here
    df_1$nbr_value <- eval(parse(text=paste("df_1$",{variable},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    df_1$date <- eval(parse(text=paste("df_1$",{date_col},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    final_df_1 <- data.frame()
    
    for (i in unique(df_1$date)){
      temp_df <- subset(df_1, date == i)
      temp_df$unique_id <-  eval(parse(text=paste("temp_df$",{unique_id_col},sep="")))
      rownames(temp_df) <- temp_df$unique_id
      
      temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
      
      temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
      temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
      
      nbrs_median_df <- data.frame() # creating empty shell that will later gain the data from the for loop
      
      for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
        df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
        df_2$nbr_id <- df_2[,1] # giving column a usable name
        df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
        nbrs_median_df <- as.data.frame(rbind(nbrs_median_df,df_2)) # binding to empty data.frame made on line 16
      }
      nbrs_median_df <- as.data.frame(nbrs_median_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
      
      nbrs_median_df$unique_id <- temp_names_df$unique_id[match(nbrs_median_df$id, temp_names_df$new_temp_id)]
      
      nbrs_median_df$nbr_id<-temp_names_df$unique_id[match(nbrs_median_df$nbr_id, temp_names_df$new_temp_id)]
      
      nbrs_median_df$nbr_value<-temp_df$nbr_value[match(nbrs_median_df$nbr_id, temp_df$unique_id )]
      
      nbrs_median_df <- aggregate(nbrs_median_df$nbr_value, by=list(unique_id=nbrs_median_df$unique_id), FUN=median)
      names(nbrs_median_df)[names(nbrs_median_df)=="x"] <- paste0("nbrs_",{variable},"_median")
      
      nbrs_median_df$date <- i
      final_df_1 <- rbind(final_df_1, nbrs_median_df)
      print(paste("Retrieving median", variable, "of neighbors for each observation for date:", i))
    }
    final_df_1 <- merge({sf_object}, final_df_1, by.x=c(unique_id_col,date_col),by.y=c("unique_id","date"))
    return(final_df_1)
  }
}


#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________

#' Add maximum observation of neighbors values to SF object
#'
#' @param sf_object An object of class 'SF'
#' @param unique_id_col A string identifying a unique column to serve as an identifier
#' @param variable A string identifying a numeric variable
#' @param date_col (Optional) A string identifying a variable for date, use if data is over multiple time periods.
#'
#' @return Column string of neighbors
#'
#' @examples
#' panel_example_sf <- nbrs_max(sf_object = panel_example_sf, unique_id_col = "country", variable = "gdp", date_col="year")
#' panel_example_sf <- nbrs_max(panel_example_sf, "country", "gdp", "year")

#' cross_section_example_sf <- nbrs_max(sf_object = cross_section_example_sf, unique_id_col = "country", variable = "gdp")
#' cross_section_example_sf <- nbrs_max(cross_section_example_sf, "country", "gdp")
#' @rdname nbrs_max
#' @export
nbrs_max <- function(sf_object, unique_id_col, date_col, variable){ #
  requireNamespace("spdep", quietly = TRUE)
  if(missing(date_col)){
    df_1 <- {sf_object} # creating copy of sf_object, will be used from here
    
    df_1$nbr_value <- eval(parse(text=paste0("df_1$",{variable},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    temp_df <- df_1
    temp_df$unique_id <-  eval(parse(text=paste("temp_df$",{unique_id_col},sep="")))
    rownames(temp_df) <- temp_df$unique_id
    
    temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
    
    temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
    temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
    
    nbrs_max_df <- data.frame() # creating empty shell that will later gain the data from the for loop
    
    for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
      df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
      df_2$nbr_id <- df_2[,1] # giving column a usable name
      df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
      nbrs_max_df <- as.data.frame(rbind(nbrs_max_df,df_2)) # binding to empty data.frame made on line 16
    }
    nbrs_max_df <- as.data.frame(nbrs_max_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
    
    nbrs_max_df$unique_id <- temp_names_df$unique_id[match(nbrs_max_df$id, temp_names_df$new_temp_id)]
    
    nbrs_max_df$nbr_id<-temp_names_df$unique_id[match(nbrs_max_df$nbr_id, temp_names_df$new_temp_id)]
    
    nbrs_max_df$nbr_value<-temp_df$nbr_value[match(nbrs_max_df$nbr_id, temp_df$unique_id )]
    
    nbrs_max_df <- aggregate(nbrs_max_df$nbr_value, by=list(unique_id=nbrs_max_df$unique_id), FUN=max)
    names(nbrs_max_df)[names(nbrs_max_df)=="x"] <- paste0("nbrs_",{variable},"_max")
    
    final_df_1 <- merge({sf_object},nbrs_max_df, by.x=c(unique_id_col),by.y=c("unique_id"))
    return(final_df_1)
  }
  else{
    df_1 <- {sf_object} # creating copy of sf_object, will be used from here
    df_1$nbr_value <- eval(parse(text=paste("df_1$",{variable},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    df_1$date <- eval(parse(text=paste("df_1$",{date_col},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    final_df_1 <- data.frame()
    
    for (i in unique(df_1$date)){
      temp_df <- subset(df_1, date == i)
      temp_df$unique_id <-  eval(parse(text=paste("temp_df$",{unique_id_col},sep="")))
      rownames(temp_df) <- temp_df$unique_id
      
      temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
      
      temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
      temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
      
      nbrs_max_df <- data.frame() # creating empty shell that will later gain the data from the for loop
      
      for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
        df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
        df_2$nbr_id <- df_2[,1] # giving column a usable name
        df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
        nbrs_max_df <- as.data.frame(rbind(nbrs_max_df,df_2)) # binding to empty data.frame made on line 16
      }
      nbrs_max_df <- as.data.frame(nbrs_max_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
      
      nbrs_max_df$unique_id <- temp_names_df$unique_id[match(nbrs_max_df$id, temp_names_df$new_temp_id)]
      
      nbrs_max_df$nbr_id<-temp_names_df$unique_id[match(nbrs_max_df$nbr_id, temp_names_df$new_temp_id)]
      
      nbrs_max_df$nbr_value<-temp_df$nbr_value[match(nbrs_max_df$nbr_id, temp_df$unique_id )]
      
      nbrs_max_df <- aggregate(nbrs_max_df$nbr_value, by=list(unique_id=nbrs_max_df$unique_id), FUN=max)
      names(nbrs_max_df)[names(nbrs_max_df)=="x"] <- paste0("nbrs_",{variable},"_max")
      
      nbrs_max_df$date <- i
      final_df_1 <- rbind(final_df_1, nbrs_max_df)
      print(paste("Retrieving maximum", variable, "of neighbors for each observation for date:", i))
    }
    final_df_1 <- merge({sf_object}, final_df_1, by.x=c(unique_id_col,date_col),by.y=c("unique_id","date"))
    return(final_df_1)
  }
}


#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________

#' Add minimum observation of neighbors values to SF object
#'
#' @param sf_object An object of class 'SF'
#' @param unique_id_col A string identifying a unique column to serve as an identifier
#' @param variable A string identifying a numeric variable
#' @param date_col (Optional) A string identifying a variable for date, use if data is over multiple time periods.
#'
#' @return Column string of neighbors
#'
#' @examples
#' panel_example_sf <- nbrs_min(sf_object = panel_example_sf, unique_id_col = "country", variable = "gdp", date_col="year")
#' panel_example_sf <- nbrs_min(panel_example_sf, "country", "gdp", "year")

#' cross_section_example_sf <- nbrs_min(sf_object = cross_section_example_sf, unique_id_col = "country", variable = "gdp")
#' cross_section_example_sf <- nbrs_min(cross_section_example_sf, "country", "gdp")
#' @rdname nbrs_min

#' @export
nbrs_min <- function(sf_object, unique_id_col, date_col, variable){ #
  requireNamespace("spdep", quietly = TRUE)
  if(missing(date_col)){
    df_1 <- {sf_object} # creating copy of sf_object, will be used from here
    
    df_1$nbr_value <- eval(parse(text=paste0("df_1$",{variable},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    temp_df <- df_1
    temp_df$unique_id <-  eval(parse(text=paste("temp_df$",{unique_id_col},sep="")))
    rownames(temp_df) <- temp_df$unique_id
    
    temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
    
    temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
    temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
    
    nbrs_min_df <- data.frame() # creating empty shell that will later gain the data from the for loop
    
    for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
      df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
      df_2$nbr_id <- df_2[,1] # giving column a usable name
      df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
      nbrs_min_df <- as.data.frame(rbind(nbrs_min_df,df_2)) # binding to empty data.frame made on line 16
    }
    nbrs_min_df <- as.data.frame(nbrs_min_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
    
    nbrs_min_df$unique_id <- temp_names_df$unique_id[match(nbrs_min_df$id, temp_names_df$new_temp_id)]
    
    nbrs_min_df$nbr_id<-temp_names_df$unique_id[match(nbrs_min_df$nbr_id, temp_names_df$new_temp_id)]
    
    nbrs_min_df$nbr_value<-temp_df$nbr_value[match(nbrs_min_df$nbr_id, temp_df$unique_id )]
    
    nbrs_min_df <- aggregate(nbrs_min_df$nbr_value, by=list(unique_id=nbrs_min_df$unique_id), FUN=min)
    names(nbrs_min_df)[names(nbrs_min_df)=="x"] <- paste0("nbrs_",{variable},"_min")
    
    final_df_1 <- merge({sf_object},nbrs_min_df, by.x=c(unique_id_col),by.y=c("unique_id"))
    return(final_df_1)
  }
  else{
    df_1 <- {sf_object} # creating copy of sf_object, will be used from here
    df_1$nbr_value <- eval(parse(text=paste("df_1$",{variable},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    df_1$date <- eval(parse(text=paste("df_1$",{date_col},sep=""))) # creating new column 'nbr_value' equaled to the variable input, 'nbr_value' is used from here on out
    
    final_df_1 <- data.frame()
    
    for (i in unique(df_1$date)){
      temp_df <- subset(df_1, date == i)
      temp_df$unique_id <-  eval(parse(text=paste("temp_df$",{unique_id_col},sep="")))
      rownames(temp_df) <- temp_df$unique_id
      
      temp_sf_nb_object <- spdep::poly2nb(temp_df) # getting list of neighbors based on queen's contiguity
      
      temp_names_df <- as.data.frame(col.names="unique_id",list(c(attr(temp_sf_nb_object,"region.id")))) # retrieving data frame of each polygon's unique id
      temp_names_df$new_temp_id <- as.numeric(rownames(temp_names_df)) # assigning new temporary id number to be used in for loop and in match() function later
      
      nbrs_min_df <- data.frame() # creating empty shell that will later gain the data from the for loop
      
      for (j in 1:length(temp_names_df$new_temp_id)){ # here is why
        df_2 <- as.data.frame(temp_sf_nb_object[[j]]) # retrieving each polygon's neighbors using nb[[i]]
        df_2$nbr_id <- df_2[,1] # giving column a usable name
        df_2$id <- j # retaining the "i" to serve as identifier when merging all rows
        nbrs_min_df <- as.data.frame(rbind(nbrs_min_df,df_2)) # binding to empty data.frame made on line 16
      }
      nbrs_min_df <- as.data.frame(nbrs_min_df[,-1]) # removing unusable name, hence why 'nbr_id' was created
      
      nbrs_min_df$unique_id <- temp_names_df$unique_id[match(nbrs_min_df$id, temp_names_df$new_temp_id)]
      
      nbrs_min_df$nbr_id<-temp_names_df$unique_id[match(nbrs_min_df$nbr_id, temp_names_df$new_temp_id)]
      
      nbrs_min_df$nbr_value<-temp_df$nbr_value[match(nbrs_min_df$nbr_id, temp_df$unique_id )]
      
      nbrs_min_df <- aggregate(nbrs_min_df$nbr_value, by=list(unique_id=nbrs_min_df$unique_id), FUN=min)
      names(nbrs_min_df)[names(nbrs_min_df)=="x"] <- paste0("nbrs_",{variable},"_min")
      
      nbrs_min_df$date <- i
      final_df_1 <- rbind(final_df_1, nbrs_min_df)
      print(paste("Retrieving minimum", variable, "of neighbors for each observation for date:", i))
    }
    final_df_1 <- merge({sf_object}, final_df_1, by.x=c(unique_id_col,date_col),by.y=c("unique_id","date"))
    return(final_df_1)
  }
}

#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________________