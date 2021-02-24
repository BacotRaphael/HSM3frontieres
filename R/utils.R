latin_to_utf8<-function(x, from="latin1", to="UTF-8"){Encoding(x) <- from;iconv(x, from, to,sub='')}

remove_blank_headings<-function(data){data[,names(data)!=""]}
remove_vars<-function(data,vars){data[,names(data) %!in%vars]}

`%!in%` = Negate(`%in%`)

humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

rec_missing<-function(x,missings=c(NULL,'NULL','N/A','n/a',999,998,888,' ','(vide)','d/m','','NA','na',""," ")) {
  x[x %in% missings] <- NA
  return(x)
}

rec_missing_all<-function(data){lapply(data,rec_missing) %>% bind_cols}

cleanheaders<-function(data,slashtodot){
  if(slashtodot){
    names(data)<-gsub("^X_","",names(data))
    names(data)<-gsub("^_","",names(data))
    names(data)<-gsub("\\/",".",names(data)) 
  } else {
    names(data)<-gsub("^X_","",names(data))
    names(data)<-gsub("^_","",names(data))
  }
  return(data)}

prepdata<-function(data,slashtodot){data %>% cleanheaders(.,slashtodot) %>% rec_missing_all %>% remove_blank_headings %>% type_convert}

ch<-as.character
chr<-as.character

label_clog<- function(clog,survey,choices){
  
  names(choices)<-gsub(":.*","",names(choices))
  names(survey)<-gsub(":.*","",names(survey))
  choices_label <- choices[["label"]]
  survey_label <- survey[["label"]]
  question.name_label <- match(clog[["question.name"]], survey[["name"]])
  old.value_label <- match(clog[["old.value"]], choices[["name"]])
  parent.other.question_label <- match(clog[["parent.other.question"]], survey[["name"]])
  
  labeled_clog <- clog %>%
    mutate(question.name_label = ifelse(is.na(question.name_label),question.name,survey_label[question.name_label]),
           old.value_label = ifelse(is.na(old.value_label),old.value,choices_label[old.value_label]),
           parent.other.question_label = ifelse(is.na(parent.other.question_label),parent.other.question,survey_label[parent.other.question_label])
           )
  
  vars<-c("today","base","enumerator","uuid","question.name","question.name_label","old.value","old.value_label","new.value","parent.other.question","parent.other.question_label","parent.other.answer")
  labeled_clog<-labeled_clog %>% select(all_of(vars),everything())
  
  return(labeled_clog)
}

load_file <- function(name, path) {
  ext <- tools::file_ext(name)
  switch(ext,
         csv=read.csv(path,stringsAsFactors = F),
         xlsx=readxl::read_excel(path,1,col_types = "text"),
         xls=readxl::read_excel(path,1,col_types = "text"),
         validate("Invalid file; Please upload a .csv .xlsx or .xls file")
  )
}

pulluuid<-function(data,logiquetest){data$uuid[which(logiquetest)]}

clean <- function(x) {
  x <- tolower(x)
  x <- gsub("  "," ",x)
  x <- gsub("  "," ",x)
  #supprime l'espace en debut
  Nettoyage <- x[substr(x, 0, 1)==" "]
  x <- replace(x,substr(x, 0, 1)==" ",substr(Nettoyage, 2, nchar(Nettoyage)))
  #supprime l'espace en fin
  Nettoyage <- x[substr(x, nchar(x), nchar(x)+1)==" "]
  x <- replace(x,substr(x, nchar(x), nchar(x)+1)==" ", substr(Nettoyage, 1, nchar(Nettoyage)-1))
  # x <- gsub("?|?|?","a",x)
  # x <- gsub("?|?|?|?","e",x)
  # x <- gsub("?|?","i",x)
  # x <- gsub("?|?","o",x)
  # x <- gsub("?|?|?","u",x)
  # x <- gsub(" |-|'","_",x)
  return (x)
}