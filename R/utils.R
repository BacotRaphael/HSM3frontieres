latin_to_utf8<-function(x, from="latin1", to="UTF-8"){Encoding(x) <- from;iconv(x, from, to,sub='')}

remove_blank_headings<-function(data){data[,names(data)!=""]}
remove_vars<-function(data,vars){data[,names(data) %!in%vars]}

`%!in%` = Negate(`%in%`)

humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

rec_missing<-function(x,missings=c(NULL,'NULL','N/A','n/a',999,998,888,' ','(vide)','d/m','','NA','na')) {
  x[x %in% missings] <- NA
  return(x)
}

rec_missing_all<-function(data){lapply(data,rec_missing) %>% bind_cols}

cleanheaders<-function(data){names(data)<-gsub("^X_","",names(data));names(data)<-gsub("^_","",names(data));names(data)<-gsub("\\/",".",names(data));return(data)}

prepdata<-function(data){data %>% cleanheaders %>% rec_missing_all %>% remove_blank_headings %>% type_convert}

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
  other.text.var_label <- match(clog[["other.text.var"]], choices[["name"]])
  
  labeled_clog <- clog %>%
    mutate(question.name_label = ifelse(is.na(question.name_label),question.name,survey_label[question.name_label]),
           old.value_label = ifelse(is.na(old.value_label),old.value,choices_label[old.value_label]),
           parent.other.question_label = ifelse(is.na(parent.other.question_label),parent.other.question,survey_label[parent.other.question_label]),
           other.text.var_label = ifelse(is.na(other.text.var_label),other.text.var,choices_label[other.text.var_label]))
  
  vars<-c("today","base","enumerator","uuid","question.name","question.name_label","old.value","old.value_label","new.value","parent.other.question","parent.other.question_label","parent.other.answer","other.text.var","other.text.var_label")
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

makeslog<-function(data,logbook,checkid="empty",index,question.name,explanation,parent.other.question="NULL",parent.other.answer="NULL",new.value="NULL",action="check"){
  if(length(index)>0){
    if(question.name=="all"){oldval<-"-"}else{oldval<-as.character(data[[question.name]][data$uuid%in%index])}
    newlog<-data.frame(
      today=as.character(data$today[data$uuid%in%index]),
      base=data$base[data$uuid%in%index],
      enumerator=data$global_enum_id[data$uuid%in%index],
      uuid= index,
      question.name = question.name,
      old.value=oldval,
      new.value=new.value,
      probleme = explanation,
      parent.other.question=ifelse(parent.other.question=="NULL","NULL",as.character(data[[parent.other.question]][data$uuid%in%index])),
      parent.other.answer=ifelse(parent.other.answer=="NULL","NULL",as.character(data[[parent.other.answer]][data$uuid%in%index])),
      checkid= checkid,
      action=action)
    bind_rows(logbook,newlog)
  } else{
    logbook
  }
}