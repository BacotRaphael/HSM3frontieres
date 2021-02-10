apply_checks<-function(db){
  logbook<-data.frame(
    today=character(),
    base= character(),
    enumerator= character(),
    uuid= character(),
    question.name = character(),
    old.value = character(),
    new.value = character(),
    if.other.text.entry = character(),
    other.text.var = character(),
    probleme = character(),
    checkid= character(),
    action=character()
  )
  
sm_autre <- names(db)[grep(".[.]autre", names(db))]
sm_nsp <- names(db)[grep(".[.](nsp|ne_sait_pas|je_ne_sais_pas)", names(db))]
sm_dwta <- names(db)[grep(".[.](prefere_pas|ne_souhaite_pas_repondre|je_prefere_ne_pas_repondre|sans_reponse)", names(db))]

# new variables
data<-db
data$difftime<-calc_duration(data)

##Transversal (nsp)
data$nb_nsp <-apply(data,1,function(x) {grep("nsp|ne_sait_pas|je_ne_sais_pas",x) %>%  unlist %>% length}) +apply(data[sm_nsp],1,sum,na.rm=T)
index<-pulluuid(data,data$nb_nsp>3)
logbook<- makeslog(data,logbook,"id02",index, "nb_nsp", "Nombre important de reponses nsp par enqueteur")

##Transversal (sans_reponse)
data$nb_dwta <-apply(data,1,function(x) {grep("prefere_pas|sans_reponse",x) %>%  unlist %>% length}) +apply(data[sm_dwta],1,sum,na.rm=T)
index<-pulluuid(data,data$nb_dwta>3)
logbook<- makeslog(data,logbook,"id03",index, "nb_dwta", "Nombre important de reponses sans_reponse par enqueteur")

##Transversal (autre)
data$nb_autre <-apply(data,1,function(x) {grep("autre",x) %>%  unlist %>% length}) + apply(data[sm_autre],1,sum,na.rm=T)
index<-pulluuid(data,data$nb_autre>3)
logbook<- makeslog(data,logbook,"id04",index, "nb_autre", "Nombre important de reponses autre par enqueteur")

# consentement suppression
index<-pulluuid(data,data$consensus_note=="non")
logbook<- makeslog(data,logbook,"id05",index,"consensus_note","pas de consentement pour enquete",new.value = "remove")

##duree d'enquete ne depasse pas quinze minte
index<-pulluuid(data,data$difftime< 20)
logbook<- makeslog(data,logbook,"id06",index,"difftime","enquete plus courte que 20min")

##duree d'enquete ne depasse pas vingt minutes
# index<-pulluuid(data,data$difftime> 15 & data$difftime<=20)
# logbook<- makeslog(data,logbook,"id07",index,"difftime","enquete plus courte que 20min")

##duree d'enquete depasse 110mins
index<-pulluuid(data,data$difftime> 110)
logbook<- makeslog(data,logbook,"id08",index,"difftime","enquete plus long que 110min")

##manque de pluie dans une saison seche
index<-pulluuid(data, composr::sm_selected(data$activite_agricole_perturbe,any = c("manque_pluie")))
logbook<-makeslog(data,logbook,"id09",index,"activite_agricole_perturbe","manque de pluie dans une saison seche et agriculture contre saison",NA)

## pas d'acces a des services d'edu /vacances scolaire
index<-pulluuid(data, data$edu_raisons=="vacances")
logbook<-makeslog(data,logbook,"id10",index,"edu_raisons","s'agit-il bien des vacances ? Est-ce que y a des localites dans lesquelles jusqu'ici les eleves sont en vacances?")

index<-pulluuid(data, data$abris_destruction_raison=="inondations")
logbook<-makeslog(data,logbook,"id11",index,"abris_destruction_raison","S'agit-il d'inondation causee par la crue d'un cours d'eau ou les eaux de pluies, au cours des 30 derniers jours?")

temp<-data %>% group_by(deviceid) %>% summarise(n=n_distinct(global_enum_id)) %>% mutate(check=n>1)
index<-pulluuid(data, data$deviceid%in% temp$deviceid[which(temp$check==T)])
logbook<-makeslog(data,logbook,"id12",index,"global_enum_id","Meme identifiant de smartephone mais differents numeros d'enqueteurs","deviceid")

index<-pulluuid(data, data$ic_age>50&data$profession_ic=="etudiant")
logbook<-makeslog(data,logbook,"id13",index,"ic_age","Un informateur cle qui a plus de 50 avec comme profil etudiant")

index<-pulluuid(data, composr::sm_selected(data$groupes_presents,any = c("refugie"))&data$refugies_source_admin0=="niger")
logbook<-makeslog(data,logbook,"id14",index,"groupes_presents","Pays d'origine des réfugiés Niger")

index<-pulluuid(data, composr::sm_selected(data$groupes_presents,none = c("non_deplaces","retourne","rapatrie")))
logbook<-makeslog(data,logbook,"id15",index,"groupes_presents","Groupe de population present dans la localite uniquement PDI et / ou Réfugie")

temp<-c("acces_sante","acces_edu","acces_marche","acces_eau")
index<-pulluuid(data, grepl(paste(temp,collapse = "|"),data$pdi_raison))
logbook<-makeslog(data,logbook,"id16",index,"pdi_raison","Raison de deplacement des populations qui peut ne pas avoir un caractere force, qui peut etre liee a une saison ou un besoin specifique")

index<-pulluuid(data, grepl(paste(temp,collapse = "|"),data$refugies_raison))
logbook<-makeslog(data,logbook,"id17",index,"refugies_raison","Raison de deplacement des populations qui peut ne pas avoir un caractere force, qui peut etre liee à une saison ou un besoin specifique")

index<-pulluuid(data, composr::sm_selected(data$pas_nourriture_raison, any = c("pas_marche"))&data$marche_maintenant=="oui")
logbook<-makeslog(data,logbook,"id18",index,"pas_nourriture_raison","presence d'un marche fonctionnel mais raison non acces a suffisamment de nourriture est : Le marche ne fonctionne pas")

index<-pulluuid(data, composr::sm_selected(data$pas_nourriture_raison, any = c("marche_limite"))&data$marche_maintenant=="non")
logbook<-makeslog(data,logbook,"id19",index,"pas_nourriture_raison","Pas de marche fonctionnel a distance de marche mais raison non acces a suffisamment de nourriture est : Le marche existe mais la disponibilite des produits est limitee")

permanenet<-c("permanent","concession")
temp<-c("paille","abri_transition","abri_fortune","abri_urgence","rhu","batiment_public","ecole","batiment_abandonne","construction","aucun")
index<-pulluuid(data, data$pdi_abris_type1%in%permanenet&data$cl_abris_type1%in%temp)
logbook<-makeslog(data,logbook,"id20",index,"pdi_abris_type1","Maison construite en dur mais le type d'abri des populations non deplacees est un autre type")

index<-pulluuid(data, data$rf_abris_type1%in%permanenet&data$cl_abris_type1%in%temp)
logbook<-makeslog(data,logbook,"id20",index,"rf_abris_type1","Maison construite en dur mais le type d'abri des populations non deplacees est un autre type")

index<-pulluuid(data, data$marche_maintenant=="non"&data$besoin_bna=="aucun")
logbook<-makeslog(data,logbook,"id21",index,"besoin_bna","Tous les articles non alimentaires cites sont disponibles mais pas de marche a distance de marche dans la localite")

index<-pulluuid(data, data$marche_maintenant=="oui"&data$savon_difficultes=="marches_non_fonctionnels")
logbook<-makeslog(data,logbook,"id22",index,"savon_difficultes","Le marche ne fonctionne pas alors que la reponse a la question concernant la presence d'un marche fontionnel est : oui")

index<-pulluuid(data, data$info_source=="appel_telephone"&data$tel_mobile=="non")
logbook<-makeslog(data,logbook,"id23",index,"tel_mobile","Pour la majorite de la population la pricipale source d'information est : telephone alors qu'a la question concernant l'acces aux telephones la reponse est non")

index<-pulluuid(data, data$info_source=="facebook"&data$tel_mobile=="non")
logbook<-makeslog(data,logbook,"id23",index,"tel_mobile","Pour la majorite de la population la pricipale source d'information est :Medias sociaux (Facebook, Whatsapp, etc.) / internet alors qu'a la question concernant l'acces aux telephones la reponse est non")

index<-pulluuid(data, data$info_source=="internet"&data$tel_mobile=="non")
logbook<-makeslog(data,logbook,"id23",index,"tel_mobile","Pour la majorite de la population la pricipale source d'information est :internet alors qu'a la question concernant l'acces aux telephones la reponse est non")

index<-pulluuid(data, data$info_source=="telephone_sat"&data$tel_mobile=="non")
logbook<-makeslog(data,logbook,"id23",index,"tel_mobile","Pour la majorite de la population la pricipale source d'information est :internet alors qu'a la question concernant l'acces aux telephones la reponse est non")

index<-pulluuid(data, data$reseau_mobile=="oui"&data$info_obstacle1=="reseau_mobile_limite")
logbook<-makeslog(data,logbook,"id24",index,"reseau_mobile","Reponse reseau_mobile_limite alors que la reponse à l question de savoir si le reseau telephonique mobile stable etait disponible est oui")

index<-pulluuid(data, data$reseau_mobile=="oui"&data$info_obstacle1=="pas_reseau_mobile")
logbook<-makeslog(data,logbook,"id24",index,"reseau_mobile","Reponse pas_reseau_mobile alors que la reponse à l question de savoir si le reseau telephonique mobile stable etait disponible est oui")

index<-pulluuid(data, data$source_eau=="robinet_maison"&data$eau_maintenant_distance!="moins_30_min")
logbook<-makeslog(data,logbook,"id24",index,"eau_maintenant_distance","Source eau est robinet de la maison mais la distance a la source d'eau est > 30min")

return(logbook)
}