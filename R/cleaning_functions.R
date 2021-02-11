calc_duration<-function(df){
  df[["start"]]<-lubridate::ymd_hms(df[["start"]])
  df[["end"]]<-lubridate::ymd_hms(df[["end"]])
  difftime(as.POSIXct(df[["end"]]),as.POSIXct(df[["start"]]),units = "mins")
}

end_tonext_duration <- function(df, pointer, unit="mins"){
  difftime(as.POSIXct(lubridate::ymd_hms(df[["end"]][pointer])), as.POSIXct(lubridate::ymd_hms(df[["start"]][pointer+1])), units = unit)}
start_tonext_duration <- function(df, pointer, unit="mins"){
  difftime(as.POSIXct(lubridate::ymd_hms(df[["start"]][pointer+1])), as.POSIXct(lubridate::ymd_hms(df[["start"]][pointer])), units = unit)}

survey_tonext_uuid <- function(df, pointer, sep="-/-"){paste0(df[["uuid"]][pointer],sep,df[["uuid"]][pointer+1])}

survey_tonext_loop <- function(df){
  i <-1
  j<-1
  
  cleantemplate<-data.frame(
    today=c(),
    base= c(),
    enumerator=c(),
    uuid=c(),
    question.name=c(),
    old.value=c(),
    probleme=c(),
    checkid=c(),
    action=c()
  )
  end_tonext<-cleantemplate
  while(i<nrow(df)) {
    diff <- end_tonext_duration(df, i)
    if(between(diff, -5, 0)|diff >0){
      out<-data.frame(
        today=as.character(df[["today"]][i]),
        base= df[["base"]][i],
        enumerator= df[["global_enum_id"]][i],
        uuid= survey_tonext_uuid(df,i),
        question.name = "start",
        old.value = as.character(round(diff)),
        probleme = ifelse(diff>0,"l'enquete a commence avant la fin de l'enquete precedente","temps entre la fin de l'enquete et le debut de l'enquete suivante court"),
        checkid= "check01",
        action= "check"
      )
      end_tonext<-rbind(end_tonext,out)
    }
    i <- i+1
  }
  start_tonext<-cleantemplate
  while(j<nrow(df)) {
    diff <- start_tonext_duration(df, j)
    if(diff<20){
      ds<-data.frame(
        today=as.character(df[["today"]][j]),
        base= df[["base"]][j],
        enumerator= df[["global_enum_id"]][j],
        uuid= survey_tonext_uuid(df,j),
        question.name = "start",
        old.value = as.character(round(diff)),
        probleme = "le temps entre le debut de 2 enquetes successives est moins de 20min",
        checkid= "check01",
        action= "check"
      )
      start_tonext<-rbind(start_tonext,ds)
    }
    j <- j+1
  }
  
  return(rbind(end_tonext,start_tonext))
}

survey_tonext_check <- function(df){
  list_byenum <- df %>% split(.$global_enum_id) %>% purrr::map(~.[order(.$start),])
  res<-purrr::map(list_byenum, survey_tonext_loop) 
  res<-res[sapply(res,function(x) dim(x)[1]) > 0] %>% bind_rows()
  }

split_multiple_choice<-function(hh,questions,choices,sep="."){
  
  questions$type %>% ch %>% strsplit(.," ") %>% do.call(rbind,.)-> tosplit
  questions$choices <- ifelse(tosplit[,1]==tosplit[,2],NA,tosplit[,2])
  names(choices)<-paste0("ch_",names(choices))
  questionnaires<-merge(questions,choices,by.x="choices",by.y="ch_list_name",all=T)
  
  
  splitsmult<-function(hh,questionnaires,varname,sep=sep){
    chlist<-questionnaires$ch_name[which(questionnaires$name %in% varname)]
    binarysmult<-lapply(chlist,
                        function(x,hh,varname,sep){
                          filt<-grep(paste0("^",x," ","|"," ",x,"$","|","^",x,"$","|"," ",x," "),hh[[varname]])
                          hh[[paste0(varname,sep,x)]]<-c()
                          hh[[paste0(varname,sep,x)]][is.na(hh[[varname]])|hh[[varname]]=="NA"]<-NA
                          hh[[paste0(varname,sep,x)]][!is.na(hh[[varname]])&hh[[varname]]!="NA"]<-0
                          hh[[paste0(varname,sep,x)]][filt]<-1
                          return(hh[[paste0(varname,sep,x)]])
                        },hh=hh,varname=varname,sep=sep) %>% bind_cols
    names(binarysmult)<-paste(varname,chlist,sep=sep)
    return(binarysmult)
  }
  
  varname=questionnaires$name[grep("select_multiple",questionnaires$type)] %>% unique
  varname<-varname[varname%in%names(hh)]
  
  lapply(varname,splitsmult,hh=hh,questionnaires=questionnaires,sep=sep) %>% bind_cols -> splitteddata
  
  for (j in names(splitteddata)){
    hh[[j]]<-splitteddata[[j]]
  }
  return(hh)
}

other_check<-function(dbs,survey){
  
  dbs<-dbs %>% as.data.frame(stringAsFActors=F) %>% type_convert()
  date_u<-humanTime()
  
  cleantemplate<-data.frame(
    today=c(),
    base= c(),
    enumerator= c(),
    uuid= c(),
    question.name = c(),
    old.value = c(),
    new.value = c(),
    parent.other.question = c(),
    parent.other.answer=c(),
    other.text.var = c(),
    probleme = c(),
    checkid= c(),
    action=c(),
    checkid=c()
  )
  
  # names(dbs)<-tolower(names(dbs))
  
  oth<-which((stringr::str_detect(names(dbs),"_autre")|stringr::str_detect(names(dbs),"_autre$"))&( sapply(dbs,class)=="factor"|sapply(dbs,class)=="character"))
  outother<-cleantemplate
  
  for (k in oth){
    
    oth_qname<-survey[["name"]][which(survey[["name"]]==names(dbs)[k])-1]
    indexoth<-which(!is.na(dbs[[names(dbs)[k]]]))
    
    if(sum(!is.na(dbs[[names(dbs)[k]]]))>0){
      ds<-data.frame(
        today=as.character(dbs[["today"]][indexoth]),
        base=dbs[["base"]][indexoth],
        enumerator=dbs[["global_enum_id"]][indexoth],
        uuid=dbs[["uuid"]][indexoth],
        question.name=rep(names(dbs)[k],length(indexoth)),
        old.value=dbs[indexoth,k],
        parent.other.question=rep(oth_qname,length(indexoth)),
        parent.other.answer=dbs[[oth_qname]][indexoth],
        other.text.var=rep("NULL",length(indexoth)),
        probleme=rep("others: to check if could be recoded",length(indexoth)),
        action="check",
        checkid="others"
      )
      
      ds<-ds[!is.na(ds$question.name)&!is.na(ds$old.value),]
      outother<-rbind(outother,ds)
    }
  }
  return(outother)
}

impl_clean<-function(data,uuid,dclean,uuid_log,qmname,newval,oldval,action,othermain,othertextvar){
  for (k in 1:nrow(dclean))
  {
    Taction<-dclean[[action]][k]
    x1<-as.character(dclean[[uuid_log]][k])
    if(any(data[[uuid]]==x1)){
      if(!is.na(Taction)&Taction!="note"&Taction!="nothing"&Taction!="check"){
        if(Taction=="remove"){
          data<-data[which(!data[[uuid]]%in%dclean[[uuid_log]][k]),]
        } else if(Taction=="recode_all"){
          data[[dclean[[qmname]][k]]][data[[dclean[[qmname]][k]]]==dclean[[oldval]][k]]<-dclean[[newval]][k]
        } else if(Taction=="recode" ){
          X<-as.character(dclean[[uuid_log]][k])
          Y<-as.character(dclean[[othermain]][k])
          val<-dclean[[othertextvar]][k]
          data[[Y]]<-as.character(data[[Y]])
          # data[which(data[[uuid]]==X),which(names(data)==Y)]<-as.character(val)
          data[[Y]][which(data[[uuid]]==X)]<-as.character(val)
          
        } else if(Taction=="recode_sm"){
          X<-as.character(dclean[[uuid_log]][k])
          Y<-as.character(dclean[[othermain]][k])
          val<-dclean[[othertextvar]][k]
          data[[Y]]<-as.character(data[[Y]])
          data[which(data[[uuid]]==X),which(names(data)==Y)]<-gsub("autre",as.character(val),data[which(data[[uuid]]==X),which(names(data)==Y)])
        } else if(Taction=="append_sm"){
          X<-as.character(dclean[[uuid_log]][k])
          Y<-as.character(dclean[[othermain]][k])
          val<-dclean[[othertextvar]][k]
          if(data[which(data[[uuid]]==X),which(names(data)==Y)]=="NA"|is.na(data[which(data[[uuid]]==X),which(names(data)==Y)])){
            data[which(data[[uuid]]==X),which(names(data)==Y)]<-as.character(val)
          } else {
            data[which(data[[uuid]]==X),which(names(data)==Y)]<-paste0(data[which(data[[uuid]]==X),which(names(data)==Y)]," ",as.character(val))
          }
        } else if(Taction=="change") {
          X<-as.character(dclean[[uuid_log]][k])
          Y<-as.character(dclean[[qmname]][k])
          val<-dclean[[newval]][k]
          data[[Y]]<-as.character(data[[Y]])
          data[[Y]][which(data[[uuid]]==X)]<-as.character(val)
        }
        #     
        # if(!is.na(dclean[[variabletoclean]][k])){
        #   Ytoclean<-dclean[[variabletoclean]][k]
        #   valtoclean<-dclean[[choicestoclean]][k]
        #   data[,which(names(data)==Ytoclean)]<-as.character(data[,which(names(data)==Ytoclean)])
        #   data[which(data[[uuid]]==X),which(names(data)==Ytoclean)]<-as.character(valtoclean)
        # }
      }
    }
  }
  return(data)
}

cleaning_data <- function(db, clog, questions, choices){
  clean<-db %>% impl_clean("uuid",clog,"uuid","question.name","new.value","old.value","action","parent.other.question","other.text.var")
  clean<- rec_missing_all(clean)
  clean<-clean %>% type_convert()
  clean<-clean %>% split_multiple_choice(questions,choices)
  return(clean)
  
}
