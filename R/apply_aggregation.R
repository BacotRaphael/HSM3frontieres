
aggregation<-function(db,survey,choices,choiceslabel,surveylabel,champs_synthese,groupGeo,recode_sl,label){
  
# db[["info_localite_final"]]<-latin_to_utf8(db[["info_localite_final"]])
# db[["info_localite_final"]] <- clean(db[["info_localite_final"]])
  db[,groupGeo]<- purrr::map_df(db[,groupGeo],clean_pcode)
  
template_data <- db[0,]
template_data<-data.frame(lapply(template_data, as.character), stringsAsFactors=FALSE)


champs_aok_all <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_all",champs_synthese[["name"]],NA))
champs_aok_longest <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_longest",champs_synthese[["name"]],NA))
champs_aok_mode <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_mode",champs_synthese[["name"]],NA))
champs_aok_no <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_no",champs_synthese[["name"]],NA))
champs_aok_no_modal <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_no_modal",champs_synthese[["name"]],NA))
champs_aok_recent <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_recent",champs_synthese[["name"]],NA))
champs_aok_tension <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_tension",champs_synthese[["name"]],NA))
champs_aok_true <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_true",champs_synthese[["name"]],NA))
champs_aok_yes <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_yes",champs_synthese[["name"]],NA))
champs_aok_yes_modal <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_yes_modal",champs_synthese[["name"]],NA))
champs_aok_frequency  <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_frequency",champs_synthese[["name"]],NA))
champs_aok_longest_modal <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_longest_modal",champs_synthese[["name"]],NA))
champs_aok_small <- na.omit(ifelse(champs_synthese[["fonction"]]=="aok_small",champs_synthese[["name"]],NA))


settlement_all <- settlement_agg(db,groupGeo,champs_aok_all,aok_all)
settlement_longest <- settlement_agg(db,groupGeo,champs_aok_longest,aok_longest)
settlement_mode <- settlement_agg(db,groupGeo,champs_aok_mode,aok_mode)
settlement_no <- settlement_agg(db,groupGeo,champs_aok_no,aok_no)
settlement_no_modal <- settlement_agg(db,groupGeo,champs_aok_no_modal,aok_no_modal)
settlement_recent <- settlement_agg(db,groupGeo,champs_aok_recent,aok_recent)
settlement_tension <- settlement_agg(db,groupGeo,champs_aok_tension,aok_tension)
settlement_true <- settlement_agg(db,groupGeo,champs_aok_true,aok_true)
settlement_yes <- settlement_agg(db,groupGeo,champs_aok_yes,aok_yes)
settlement_yes_modal <- settlement_agg(db,groupGeo,champs_aok_yes_modal,aok_yes_modal)
settlement_frequency <- settlement_agg(db,groupGeo,champs_aok_frequency,aok_frequency)
settlement_longest_modal <- settlement_agg(db,groupGeo,champs_aok_longest_modal,aok_longest_modal)
settlement_small <- settlement_agg(db,groupGeo,champs_aok_small,aok_small)

settlement<- settlement_all %>%
  left_join(settlement_longest,by = groupGeo)%>%
  left_join(settlement_mode,by = groupGeo) %>%
  left_join(settlement_no,by = groupGeo) %>%
  left_join(settlement_no_modal,by = groupGeo) %>%
  left_join(settlement_recent,by = groupGeo) %>%
  left_join(settlement_tension,by = groupGeo) %>%
  left_join(settlement_true,by = groupGeo) %>%
  left_join(settlement_yes,by = groupGeo) %>%
  left_join(settlement_yes_modal,by = groupGeo)%>%
  left_join(settlement_frequency,by=groupGeo) %>%
  left_join(settlement_longest_modal,by=groupGeo)%>%
  left_join(settlement_small,by=groupGeo) %>% 
  left_join(db %>% count(!!! rlang::syms(groupGeo),name = "B_ki_coverage"),by = groupGeo)

settlement<-bind_rows(template_data,settlement) %>%prepdata(.,T)


parent_created <- re_create_sm(settlement, survey, separator = ".")
parent_created_sl<-sl_correction(parent_created,recode_sl,survey)
parent_created_sl$dep_com_loc<-paste(parent_created_sl$admin2,parent_created_sl$admin3,parent_created_sl$info_localite_final,sep = "_")
parent_created_sl<-parent_created_sl %>% select(B_ki_coverage,dep_com_loc,everything())

if(label=="oui") {
  parent_created_sl<-from_xml_tolabel(parent_created_sl,choices,survey,choiceslabel,surveylabel)}

return(parent_created_sl)
}