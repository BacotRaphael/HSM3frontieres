
re_create_sm <- function(df, survey, separator="."){
  multiple_choices <- filter(survey, str_detect(type, "(\\bselect_multiple\\b)"))$name
  for (m in 1:length(multiple_choices)) {
    choice_char_count <- nchar(multiple_choices[m]) + 2
    choices <- df %>% select(starts_with(paste0(multiple_choices[m], separator)))
    for (c in 1:length(choices)) {
      choice_name <- substring(names(choices[c]),choice_char_count)
      for (r in 1:nrow(choices)) {
        if (!is.na(choices[r,c]) & as.numeric(choices[[c]][r]) == 1 & choices[[c]][r] != "NC") {
          temp <- str_detect(df[[multiple_choices[m]]][r], choice_name)
          if(!is.na(temp) & !temp){
            df[r, multiple_choices[m]] <- paste(df[[multiple_choices[m]]][r], choice_name, sep = " ")
            df[r, multiple_choices[m]] <- gsub("NC ","",df[r, multiple_choices[m]])
          }
        }
      }
    }
  }
  return(df)
}

sm_label_toxml <- function(df, survey, separator="."){
  names(survey)<-gsub(":.*","",names(survey))
  multiple_choices <- filter(survey, str_detect(type, "(\\bselect_multiple\\b)"))$name
  for (m in 1:length(multiple_choices)) {
    choice_char_count <- nchar(multiple_choices[m]) + 2
    choices <- df %>% select(starts_with(paste0(multiple_choices[m], separator)))
    df[[multiple_choices[m]]]<-""
    for (c in 1:length(choices)) {
      choice_name <- substring(names(choices[c]),choice_char_count)
      for (r in 1:nrow(choices)) {
        if (!is.na(choices[r,c]) & as.numeric(choices[[c]][r]) == 1 & choices[[c]][r] != "NC") {
          if(df[[multiple_choices[m]]][r]==""){
            df[r, multiple_choices[m]] <- choice_name
            df[r, multiple_choices[m]] <- gsub("NC ","",df[r, multiple_choices[m]])
          } else{
            df[r, multiple_choices[m]] <- paste(df[[multiple_choices[m]]][r], choice_name, sep = " ")
            df[r, multiple_choices[m]] <- gsub("NC ","",df[r, multiple_choices[m]])
          }
        }
      }
    }
  }
  return(df)
}


sl_correction<-function(db,sl_definition,survey,sl_name="name",sl_condition="condition"){
  multiple_choices <- filter(survey, str_detect(type, "(\\bselect_multiple\\b)"))$name
  for (i in 1:nrow(sl_definition)) {
    qname<-sl_definition[[sl_name]][i]
    if(qname%in%names(db)){
      db[[qname]]<-ifelse(eval(parse(text=sl_definition[[sl_condition]][[i]]),envir = db),db[[qname]],"SL")
      if(qname%in%multiple_choices){
        sm<-names(db)[str_detect(names(db),paste0(qname,"."))]
        for(j in 1:length(sm)){
          db[[sm[j]]]<-ifelse(db[[qname]]=="SL","SL",db[[sm[j]]])
        }
      }
    }
  }
  return(db)
}

from_xml_tolabel<-function(db,choices,survey){
  multiple_choices <- filter(survey, str_detect(type, "(\\bselect_multiple\\b)"))$name
  chr_names<-db %>% select_if(~ !(all(is.na(.x)))) %>% select_if(~ is.character(.)) %>% names
  chr_names<-chr_names[!str_detect(chr_names,paste(paste0(multiple_choices,"."),collapse = "|"))]
  names(choices)<-gsub(":.*","",names(choices))
  names(survey)<-gsub(":.*","",names(survey))
  choice_labels <- choices[["label"]]
  survey_labels <- survey[["label"]]
  
  for (i in 1: length(chr_names)){
    if(chr_names[i]%in%multiple_choices){
      split_sm<-str_split(db[[chr_names[i]]]," ")
      db[[chr_names[i]]]<-lapply(split_sm, function(x)match(x, choices[["name"]])) %>% 
        lapply(.,function(x){ifelse(is.na(x),x,choice_labels[x])}) %>% lapply(., function(x)paste(x,collapse = " ")) %>% unlist
    } else{
      var_label <- match(db[[chr_names[i]]], choices[["name"]])
      db[[chr_names[i]]]<-ifelse(is.na(var_label),db[[chr_names[i]]],choice_labels[var_label])
    }
  }
  names(db)<-gsub(".*[.]","",names(db))
  label_indices<-match(names(db),survey[["name"]])
  names(db)<-ifelse(is.na(label_indices)|is.na(survey_labels[label_indices]),names(db),survey_labels[label_indices])
  choices_indices<-match(names(db),choices[["name"]])
  names(db)<-ifelse(is.na(choices_indices),names(db),choice_labels[choices_indices])
  return(db)
}

from_label_toxml<-function(db,choices,survey){
  names(choices)<-gsub(":.*","",names(choices))
  names(survey)<-gsub(":.*","",names(survey))
  multiple_choices <- filter(survey, str_detect(type, "(\\bselect_multiple\\b)"))$label
  chr_names<-db %>% select_if(~ !(all(is.na(.x)))) %>% select_if(~ is.character(.)) %>% names
  choice_xml <- choices[["name"]]
  survey_xml <- survey[["name"]]
  for (i in 1:length(chr_names)){
    if(chr_names[i]%!in%multiple_choices){
      var_xml <- match(db[[chr_names[i]]], choices[["label"]])
      db[[chr_names[i]]]<-ifelse(is.na(var_xml),db[[chr_names[i]]],choice_xml[var_xml])
    }
  }
  for (i in 1:length(names(db))){
    if(names(db)[i]%in%(multiple_choices)){
      choices_sm<-names(db)[grepl(paste0(names(db)[i],"."),names(db),fixed = T)]
      split_sm<-str_split(choices_sm,"[.]",n = 2)
      names(db)[which(names(db)%in%choices_sm)]<-lapply(split_sm, function(x)match(x, choices[["label"]])) %>% 
        lapply(.,function(x){ifelse(is.na(x),x,choice_xml[x])}) %>% lapply(., function(x)paste(x,collapse = ".")) %>% unlist %>%
      sub("NA",survey_xml[match(names(db)[i], survey[["label"]])],.)
      names(db)[i]<-survey_xml[match(names(db)[i], survey[["label"]])]
    } else{
      var_label <- match(names(db)[i], survey[["label"]])
      names(db)[i]<-ifelse(is.na(var_label),names(db)[i],survey_xml[var_label])
    }
  }
  return(db)
}
