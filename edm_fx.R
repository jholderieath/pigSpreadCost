# Generate weights ------------------
#' Generate market weights
#'
#' Generates weights for types of consumers.
#'
#' @param crops A character vector of crops.
#' @param markets A character vector of consumer markets.
#'
#' @return A character vector of weights.
#'  @export
#'
gen_mkt_weights <- function(crops,markets) {
  tmp <- vector()
  tick <- 0
  for (c in crops) {
    for (m in markets) {
      tick <- tick + 1
      tmp[tick] <- paste('w',m,c,sep = "_")
    }
  }
  return(tmp)
}

#' Generate Market Weights and Put them in a Tibble
#'
#' Uses `gen_mkt_weights` to create a tibble for storing the market weights.
#'
#' @param crops A character vector of crops.
#' @param markets A character vector of consumer markets.
#'
#' @return Returns a tibble of the market weights.
#'  @export
#'
gmw <- function(crops,markets) {
  mkt_weight <- purrr::map_dfc(markets,gen_mkt_weights,crops)
  names(mkt_weight) <- markets
  return(mkt_weight)
}

#' Title
#'
#' @param crops
#' @param suppliers
#'
#' @return
#'  @export
#'
#' @examples
gen_sup_weights <- function(crops,suppliers){
  tmp <- vector()
  tick <- 0
  s <- suppliers
  for (c in crops) {
    for (s in suppliers) {
      tick <- tick + 1
      tmp[tick] <- paste('ss',s,c,sep = "_")
    }
  }
  return(tmp)
}

#' Title
#'
#' @param crops
#' @param suppliers
#'
#' @return
#'  @export
#'
#' @examples
gsw <- function(crops,suppliers) {
  sup_weight <- purrr::map_dfc(suppliers,gen_sup_weights,crops)
  names(sup_weight) <- suppliers
  return(sup_weight)
}

# Generate Elasticities-------------------------------
#' Title
#'
#' @param crops
#' @param markets
#' @param crossprice
#'
#' @return
#'  @export
#'
#' @examples
gen_mkt_elast <- function(crops,markets,crossprice) {
  tmp <- vector()
  tick <- 0
  for (i in crops) {
    for (n in markets) {
      tick <- tick + 1
      tmp[tick] <- paste('e',n,i,crossprice,sep = "_")
    }
    
  }
  return(tmp)
}

#' Title
#'
#' @param crops
#' @param markets
#' @param crossprice
#'
#' @return
#'  @export
#'
#' @examples
gme <- function(crops,markets,crossprice) {
  me <- list()
  tick <- 0
  for (i in markets) {
    for (j in crops) {
      for (k in crossprice) {
        tick <- tick + 1
        me[tick] <- gen_mkt_elast(crops = j,markets = i,crossprice = k)
      }
    }
  }
  return(me)
}

#' Title
#'
#' @param crops
#' @param suppliers
#' @param crossprice
#'
#' @return
#'  @export
#'
#' @examples
gen_sup_elast <- function(crops,suppliers,crossprice){
  tmp <- vector()
  tick <- 0
  for (i in crops) {
    for (n in suppliers) {
      tick <- tick + 1
      tmp[tick] <- paste('e',n,crossprice,i,sep = "_")
    }
  }
  return(tmp)
}

#' Title
#'
#' @param crops
#' @param suppliers
#' @param crossprice
#'
#' @return
#' @export
#'
#' @examples
gse <- function(crops,suppliers,crossprice) {
  me <- list()
  tick <- 0
  for (i in suppliers) {
    for (j in crops) {
      for (k in crossprice) {
        tick <- tick + 1
        me[tick] <- gen_sup_elast(crops = j,suppliers = i,crossprice = k)
      }
    }
  }
  return(me)
}



# Generate Price Changes------------------------------
#' Title
#'
#' @param crops
#'
#' @return
#' @export
#'
#' @examples
gen_price_chg <- function(crops){
  tmp <- vector()
  tick <- 0
  for (i in crops) {
    tick <- tick + 1
    tmp[tick] <- paste('EP',i,sep = "_")
  }
  return(tmp)
}
# Generate shocks
#' Title
#'
#' @param crops
#' @param place
#' @param side
#'
#' @return
#' @export
#'
#' @examples
gen_shock <- function(crops, place, side){
  tmp <- vector()
  tick <- 0
  for (i in crops) {
    for (j in place) {
      tick <- tick + 1
      s <- paste0("E",side)
      tmp[tick] <- paste(s,j,i,sep = "_")
    }
    
  }
  return(tmp)
}

# create VARIABLES
#' Title
#'
#' @param crops
#' @param markets
#' @param suppliers
#'
#' @return
#' @export
#'
#' @examples
gen_var <- function(crops,markets,suppliers) {
  
  #generate variables
  mkt_weight <- as.list(gen_mkt_weights(crops = crops, markets = markets))
  sup_weight <- as.list(gen_sup_weights(crops = crops, suppliers = suppliers))
  me <- gme(crops = crops,markets = markets,crossprice = crops)
  se <- gse(crops = crops,suppliers =  suppliers,crossprice = crops)
  mwl <- unlist(mkt_weight)
  swl <- unlist(sup_weight)
  
  # set variables in sympy
  variables <- c(mkt_weight,sup_weight,me,se)
}
#gen equations
#' Title
#'
#' @param variables
#' @param crops
#' @param markets
#' @param suppliers
#' @param EC
#' @param EB
#'
#' @return
#' @export
#'
#' @examples
gen_eq <- function(variables,crops,markets,suppliers,EC,EB) {
  tick <- 0
  temp <- list()
  demand <- list()
  dem_eqns <- list()
  # create portions of the demand equation
  for (m in markets) {
    for (c in crops) {
      tick = tick + 1
      regex_tmp <- paste("e",m,c,sep = "_")
      pe_tmp <- stringr::str_subset(variables,regex_tmp)
      
      regex_tmp <- paste("EC",m,c,sep = "_")
      ec_tmp <- stringr::str_subset(EC,regex_tmp)
      
      eq <- paste(pe_tmp,EP,sep=' * ') %>%
        paste(collapse = " + ")
      temp[tick] <- paste0('(',eq," + ",ec_tmp,')')
      regex_tmp <- paste("w",m,c,sep = "_")
      demand[tick] <- paste(regex_tmp,temp[tick],sep = ' * ')
    }
  }
  # put portions of demand equations together
  tick <- 0
  for (c in crops) {
    tick <- tick + 1
    regex_tmp <- paste("w_(export|domestic)",c,sep = "_")
    dem_elements <- stringr::str_subset(demand,regex_tmp)
    dem_eqns[tick] <- paste(dem_elements, collapse = " + ")
  }
  #create portions of the supply equation
  tick <- 0
  temp <- list()
  supply <- list()
  for (s in suppliers) {
    for (c in crops) {
      tick = tick + 1
      regex_tmp <- paste("e",s,c,sep = "_")
      pe_tmp <- stringr::str_subset(variables,regex_tmp)
      
      regex_tmp <- paste("EB",s,c,sep = "_")
      eb_tmp <- stringr::str_subset(EB,regex_tmp)
      
      eq <- paste(pe_tmp,EP,sep=' * ') %>%
        paste(collapse = " + ")
      temp[tick] <- paste0('(',eq," + ",eb_tmp,')')
      regex_tmp <- paste("ss",s,c,sep = "_")
      supply[tick] <- paste(regex_tmp,temp[tick],sep = ' * ')
    }
  }
  #put portions of supply equations together
  tick <- 0
  sup_eqns <- list()
  for (c in crops) {
    tick <- tick + 1
    regex_tmp <- paste("ss_(AOS|FRS|imports)",c,sep = "_")
    sup_elements <- stringr::str_subset(supply,regex_tmp)
    sup_eqns[tick] <- paste(sup_elements, collapse = " + ")
  }
  #combine into a system of equations
  eqns <- pmap_chr(list(dem_eqns,sup_eqns),paste, sep = " , ")
  eqns2 <- paste0("Eq(",eqns,")")
  eqns3 <- map2_chr(crops,eqns2,paste, sep = " = ")
  eqns4 <- paste0("sympy('",eqns3,"')")
  return(eqns4)
}

#' Title
#'
#' @param eqns
#'
#' @return
#' @export
#'
#' @examples
eqns_to_sympy <- function(eqns){
  for (i in length(eqns)) {
    eval(parse(text = eqns), envir=.GlobalEnv)
  }
}
#eval(parse(text = eqns4))
#----------------------------------
#' Title
#'
#' @param string
#'
#' @return
#' @export
#'
#' @examples
eval_parse <- function(string) {
  eval(parse(text = string), envir=.GlobalEnv)
}


#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
val_look_up <- function(data) {
  temp <-   paste0(
    str_extract(v$name,'\\b[^EP]{0,2}_'),
    str_extract(v$name,'(domestic|export|imports)'),"['",
    str_extract(v$name,'(corn|soy|wheat|rice|peanut){1}'),"','",
    str_extract(v$name,'(corn|soy|wheat|rice|peanut)\\b'),"']"
  )
  to_dbl <-  purrr::map_dbl(temp, eval_parse)
  return(to_dbl)
}


#' Title
#'
#' @param variables 
#'
#' @return
#' @export
#'
#' @examples
ssLookup <- function(x,y,data) {
  result <- data %>%
    filter(GEOID == y)%>%
    pull(x)
  unlist(result[[1]])
}
  

    
  



#' Title
#'
#' @param name
#' @param value
#'
#' @return
#' @export
#'
#' @examples
v_to_parse <- function(name,value){
  temp <- paste0("(",name,",",value,")")
  txt <- paste(temp, collapse = ", ")
}

#' Title
#'
#' @param c
#' @param t
#'
#' @return
#' @export
#'
#' @examples
eval_parse_crops <- function(c, t){
  txt <- paste0("sympy('",c," = ",c, ".subs([",t,"])')")
}


#' Title
#'
#' @param name
#' @param value
#'
#' @return
#' @export
#'
#' @examples
to_parse_shocks <- function(name,value){
  temp <- paste0("(",name,",",value,")")
  txt <- paste(temp, collapse = ", ")
}

#' Title
#'
#' @param c
#' @param t
#'
#' @return
#' @export
#'
#' @examples
parse_shocks <- function(c,t){
  txt <- paste0("sympy('",c," = ",c, ".subs([",t,"])')")
}
