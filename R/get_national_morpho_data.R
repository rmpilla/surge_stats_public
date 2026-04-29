# Pull the National-Scale Morphology Predictions from Jeff Hollister
# retrieved file from https://raw.githubusercontent.com/USEPA/surge_morpho/refs/heads/main/data/surge_stats_national_morpho_metrics.csv. 

get_national_morpho_data <- function(national_morpho_file) {
 
  nm2<-read.csv(national_morpho_file)
  
  globathy<-nm2%>%
    mutate(source=as.factor(source), variables=as.factor(variable))%>%
    filter(source =="globathy")%>%
    filter(variable%in% c("max_depth","mean_depth","surface_area"))%>%
    select(lake_id,value,variable)
  
  glomd<-filter(globathy,variable=="max_depth")%>%
    select(lake_id,value)
  
  nhd<-nm2%>%
    filter(source=="nhdplus")%>%
    filter(variable %in% c("max_depth","mean_depth","surface_area"))%>%
    #filter out lake 69 since it's maxdepth in nhdplus is 0
    filter(lake_id!=69)%>%
    select(lake_id, value,variable)
  
  nhd_maxdepth<-filter(nhd,variable=="max_depth")%>%
    select(lake_id,value)
  
  lagos<-nm2%>%
    filter(source=="lagos")%>%
    filter(variable %in% c("max_depth","mean_depth","surface_area"))%>%
    select(lake_id,value,variable)
  
  lagos_maxdepth<-filter(lagos,variable=="max_depth")%>%
    select(lake_id,value)
  
  lake17<-nm2%>%
    filter(source=="nla17 index")%>%
    filter(lake_id==17)%>%
    filter(variable == "max_depth")%>%
    select(lake_id,value)
  
  lake10<-nm2%>%
    filter(source=="nla22 index")%>%
    filter(lake_id==10)%>%
    filter(variable == "max_depth")%>%
    select(lake_id,value)
  
  #do an antijoin to get nhd max depth values for lakes not in globathy
  nhd_join<-anti_join(nhd_maxdepth,glomd,by="lake_id")

  lagos_join<-anti_join(lagos_maxdepth,glomd,by="lake_id")
  lagos_join2<-anti_join(lagos_join,nhd_join,by="lake_id")
  
  #now join rows together, only missing 1 and 1 with an off value
  depths<-rbind(glomd,nhd_join,lagos_join2,lake10,lake17)
  colnames(depths)<-c("lake_id","max_depth_global")
  #colnames(depths)<-c("lake_id","max_depth_global","mean_depth_global")
  
  #look at how many surface area estimates we have in nhd
  sanhd<-nhd%>%
    filter(variable=="surface_area")
  
  #fill in the rest with hydrolakes
  sanlmd<-nm2%>%
    filter(source=="hydrolakes")%>%
    filter(variable=="surface_area")
  
  sanlmdu<-anti_join(sanlmd,sanhd,by="lake_id")%>%
    select(lake_id,value,variable)
  
  surface_area<-rbind(sanhd,sanlmdu)%>%
    select(lake_id,value)
  colnames(surface_area)<-c("lake_id","surface_area_global")
  
  national_morpho_data=left_join(depths,surface_area)
  
  #now add mean depths
  meandep<-filter(globathy,variable=="mean_depth")
  
  #now calculate mean depths for lakes outside globathy & calculate littoral fraction
  national_morpho_data<-left_join(national_morpho_data,meandep,by="lake_id")%>%
    mutate(mean_depth_global=ifelse(!is.na(value),value,max_depth_global/3))%>%
    mutate(
      q_bathy_shape = max_depth_global / mean_depth_global - 1,
      littoral_fraction_global = case_when(
        max_depth_global <= 2.5 ~ 100,
        TRUE ~ (1 - (1 - 2.5 / max_depth_global)^q_bathy_shape) * 100
      ))%>%
    select(lake_id,max_depth_global,mean_depth_global,surface_area_global,littoral_fraction_global)
  
  return(national_morpho_data) 
}
