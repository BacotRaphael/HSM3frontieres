## Mode ============
# Caculates and returns the mode of a column. Outputs "NC" or NA if several modes. 
aok_mode <-function(x) {
  if(all(is.na(x))){
    return("")
  }else{
    x <- x[x %!in% c("")]
    if(length(x) == 0){
      return("")
    }else{
      x = table(x)
      modes = sum(x == max(x))
      if(modes == 1){
        return(names(which(x == max(x))))
      }else{
        return("NC")
      }
    }
  }
}


## True ============
# Returns "1" over "0".
aok_true <-function(x) {
  if(all(is.na(x))){return("")}
  else{
    x <- x[x %!in% c("")]
    if(length(x) == 0){return("")}
    else{
      x = table(x)
      modes = sum(x == max(x))
      if(modes == 1){return(names(which(x == max(x))))}
      else if(modes > 1){
        if("1" %in% names(which(x == max(x)))){"1"} else {return("NC")}
      }
      else {return("NC")}
    }
  }
}

## Yes ============
# Returns "Yes" over "No".
aok_yes <-function(x) {
  if(all(is.na(x))){return("")}
  else{
    x <- x[x %!in% c("")]
    if(length(x) == 0){return("")}
    else{
      x = table(x)
      modes = sum(x == max(x))
      if(modes == 1){return(names(which(x == max(x))))}
      else if(modes > 1){
        if("oui" %in% names(which(x == max(x)))){"oui"} else {return("NC")}
      }
      else {return("NC")}
    }
  }
} 
# Returns "Yes" over "No". Returns mode depending if KI number exceeds 3.
aok_yes_modal<- function(x) {
  if (length(x)<=3){
    if ("oui" %in% x) {
      return("oui")} 
    else if("non" %in% x) {
      return("non")} 
    else {
      return("NC")}
  }
  else {
    x <- x[x != ""]
    ux <- unique(x)
    n <- length(ux)
    frequencies <- tabulate(match(x, ux))
    modes <- frequencies == max(frequencies)
    nmodes <- sum(modes)
    nmodes <- ifelse(nmodes==n, 0L, nmodes)
    if (nmodes == 0) { #No mode or the same answer among the KI.
      if (n == 1){ #The same answer among the KI.
        return(ux)}
      else{ #No mode among different answers. As possible answers are "Yes", "No, and "Don't Know", we decided to return "Yes".
        return("oui")}} 
    else if (nmodes != 1) {
      if(TRUE %in% (which(ux=="oui")==which(modes==TRUE)) == TRUE){ #This is to check between mutiples modes if one is "Yes". 
        return("oui")}
      else {#Otherwise it means there is two modes that are "No" and "Don't know", and we return "NC".
        return("NC")}
    }
    else {
      return(ux[which(modes)])}
  }
} 

## No ============
# Returns "No" over "Yes".
aok_no <-function(x) {
  if(all(is.na(x))){return("")}
  else{
    x <- x[x %!in% c("")]
    if(length(x) == 0){return("")}
    else{
      x = table(x)
      modes = sum(x == max(x))
      if(modes == 1){return(names(which(x == max(x))))}
      else if(modes > 1){
        if("non" %in% names(which(x == max(x)))){"non"} else {return("NC")}
      }
      else {return("NC")}
    }
  }
}

# Returns "No" over "Yes". Returns mode depending if KI number exceeds 3.
aok_no_modal<- function(x) {
  if (length(x)<=3){
    if ("non" %in% x) {
      return("non")} 
    else if("oui" %in% x) {
      return("oui")} 
    else {
      return("NC")}
  }
  else {
    x <- x[x != ""]
    ux <- unique(x)
    n <- length(ux)
    frequencies <- tabulate(match(x, ux))
    modes <- frequencies == max(frequencies)
    nmodes <- sum(modes)
    nmodes <- ifelse(nmodes==n, 0L, nmodes)
    if (nmodes == 0) { 
      if (n == 1){ 
        return(ux)}
      else{ 
        return("non")}} 
    else if (nmodes != 1) {
      if(TRUE %in% (which(ux=="non")==which(modes==TRUE)) == TRUE){ 
        return("non")}
      else {
        return("NC")}
    }
    else {
      return(ux[which(modes)])}
  }
} 

## Recent ============
# Returns the "most recent" value among a set of possibilities.
aok_recent <- function(x) {
  if(all(is.na(x))){return("")}
  else{
    x <- x[x %!in% c("")]
    if(length(x) == 0){return("")}
    else{
      x = factor(x, levels = c("1_mois",
                               "3_mois",
                               "4_6_mois",
                               "7_12_mois",
                               "1_an",
                               "sans_reponse",
                               "nsp"), ordered = T)
      y = c("1_mois",
            "3_mois",
            "4_6_mois",
            "7_12_mois",
            "1_an")
      x = table(x)
      modes = sum(x == max(x))
      if(modes == 1){names(x[x == max(x)])}
      else if(modes > 1 & sum(x) <= 2){
        if(any(y %in% names(x[x == max(x)]))){names(x[x == max(x)])[1]} else {return("NC")}
      }
      else {return("NC")}
    }
  }
}

# Small ============
#Returns the "smallest" value among a set of possibilities.
aok_small <- function(x) {
  if ("Moins que 1"%in% x) {
    return("Moins que 1")}
  else if("1" %in% x) {
    return("1")}
  else if("2" %in% x) {
    return("2")}
  else if("3" %in% x) {
    return("3")}
  else if("3+" %in% x) {
    return("3+")}
  else if("Pas de r?ponse / ne souhaite pas r?pondre" %in% x) {
    return("Pas de r?ponse / ne souhaite pas r?pondre")}
  else if("Je ne sais pas" %in% x) {
    return("Je ne sais pas")}
  else {
    return("NC")}
}


## Long ============
# Returns the "longest" value among a set of possibilities
aok_longest <- function(x) {
  if ("journee_entiere"%in% x) {
    return("journee_entiere")} 
  else if("demi_journee" %in% x) {
    return("demi_journee")} 
  else if("1_heure_1_demi_journee" %in% x) {
    return("1_heure_1_demi_journee")} 
  else if("30_min_1_heure" %in% x) {
    return("30_min_1_heure")} 
  else if("moins_30_min" %in% x) {
    return("moins_30_min")} 
  else if("sans_reponse" %in% x) {
    return("sans_reponse")} 
  else if("nsp" %in% x) {
    return("nsp")} 
  else {
    return("NC")}
}

# Returns the "longest" value among a set of possibilities. Returns the mode if KI number exceeds 3.
aok_longest_modal <- function(x) {
  if (length(x)<=3){
    if ("journee_entiere"%in% x) {
      return("journee_entiere")} 
    else if("demi_journee" %in% x) {
      return("demi_journee")} 
    else if("1_heure_1_demi_journee" %in% x) {
      return("1_heure_1_demi_journee")} 
    else if("30_min_1_heure" %in% x) {
      return("30_min_1_heure")} 
    else if("moins_30_min" %in% x) {
      return("moins_30_min")} 
    else if("sans_reponse" %in% x) {
      return("sans_reponse")} 
    else if("nsp" %in% x) {
      return("nsp")} 
    else {
      return("NC")}}
  else {
    x <- x[x != ""]
    ux <- unique(x)
    n <- length(ux)
    frequencies <- tabulate(match(x, ux))
    modes <- frequencies == max(frequencies)
    nmodes <- sum(modes)
    nmodes <- ifelse(nmodes==n, 0L, nmodes)
    if (nmodes == 0) { #No mode or the same answer among the KI.
      if (n == 1){ #The same answer among the KI.
        return(ux)}
      #No mode among different answers (All answers are equal).
      else if(TRUE %in% (which(ux=="journee_entiere")==which(modes==TRUE))){
        return("journee_entiere")}
      else if(TRUE %in% (which(ux=="demi_journee")==which(modes==TRUE))){
        return("demi_journee")}
      else if(TRUE %in% (which(ux=="1_heure_1_demi_journee")==which(modes==TRUE))){
        return("1_heure_1_demi_journee")}
      else if(TRUE %in% (which(ux=="30_min_1_heure")==which(modes==TRUE))){
        return("30_min_1_heure")}
      else if(TRUE %in% (which(ux=="moins_30_min")==which(modes==TRUE))){
        return("moins_30_min")}
      else {
        return("NC")}
    }
    else if (nmodes != 1) { #If there is 2 modes or more.
      if(TRUE %in% (which(ux=="journee_entiere")==which(modes==TRUE))){
        return("journee_entiere")}
      else if(TRUE %in% (which(ux=="demi_journee")==which(modes==TRUE))){
        return("demi_journee")}
      else if(TRUE %in% (which(ux=="1_heure_1_demi_journee")==which(modes==TRUE))){
        return("1_heure_1_demi_journee")}
      else if(TRUE %in% (which(ux=="30_min_1_heure")==which(modes==TRUE))){
        return("30_min_1_heure")}
      else if(TRUE %in% (which(ux=="moins_30_min")==which(modes==TRUE))){
        return("moins_30_min")}
      else {
        return("NC")}
    }
    else {#If there is 1 mode.
      return(ux[which(modes)])}
  }
}


## Frequent ============
# Returns the "most frequent" value among a set of possibilities.
aok_frequency <- function(x) {
  if ("chaque_semaine"%in% x) {
    return("chaque_semaine")}
  else if("chaque_2_semaines" %in% x) {
    return("chaque_2_semaines")}
  else if("chaque_mois" %in% x) {
    return("chaque_mois")}
  else if("chaque_2_3_mois" %in% x) {
    return("chaque_2_3_mois")}
  else if("urgence" %in% x) {
    return("urgence")}
  else if("si_necessaire" %in% x) {
    return("si_necessaire")}
  else if("sans_reponse" %in% x) {
    return("sans_reponse")}
  else if("nsp" %in% x) {
    return("nsp")}
  else {
    return("NC")}
}

## Tension ============
# Returns the "tension" value among a set of possibilities.
aok_tension <- function(x) {
  if ("fragile"%in% x ){
    return("fragile")}
  else {
    if(all(is.na(x))){return("")}
    else{
      x <- x[x %!in% c("")]
      if(length(x) == 0){return("")}
      else{
        x = table(x)
        modes = sum(x == max(x))
        if(modes == 1){return(names(which(x == max(x))))}
        else {return("NC")}
      }
    }
  }
}

## All ============
## Function to pick all the answers 
aok_all <- function(x) {
  x <- x[which(x!=""&!is.na(x))]
  x <- paste(x, collapse = '-')
  return(x)
}

settlement_agg<-function(db,by,champs,fct){
  if(length(champs)>0){
    db %>%
      group_by_at(by) %>%
      summarize_at(all_of(champs), ~fct(.)) %>%
      ungroup()
  } else{db[!duplicated(db[,by]),][,by]}
}

mean_agg<-function(db,first_lev,second_lev,groupSel){
  db[["agg_col"]]<-paste0(db[[first_lev]],"_",db[[second_lev]])
  res<- db %>% group_by(agg_col) %>% select(all_of(groupSel)) %>% summarise_all(~list(mean(.,na.rm=T)))
  return(res)
}