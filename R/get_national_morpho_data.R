# Pull the National-Scale Morphology Predictions from Jeff Hollister
# retrieved file from https://github.com/USEPA/surge_morpho/blob/main/data/surge_res_morpho_all.csv

get_national_morpho_data <- function(national_morpho_file, lake_scale_data) {
 
  nm<-read.csv(national_morpho_file)
  
  glomd<-nm%>%
    mutate(source=as.factor(source), variables=as.factor(variables))%>%
    filter(source =="globathy")%>%
    filter(variables=="dmax_use")%>%
    select(lake_id,values)
  
  comids<-lake_scale_data%>%
    filter(name=="nhdplus_comid")
  
  nhd_maxdepth<-nm%>%
    filter(source=="nhdplus")%>%
    filter(join_id %in% comids$value)%>%
    filter(variables %in% c("maxdepth"))%>%
    #filter out lake 69 since it's maxdepth in nhdplus is 0
    filter(lake_id!=69)%>%
    group_by(lake_id)%>%
    summarise(values=max(values))%>%
    select(lake_id,values)
  
  lagos_maxdepth<-nm%>%
    filter(source=="lagos")%>%
    filter(variables %in% c("maxdepth"))%>%
    group_by(lake_id)%>%
    summarise(values=max(values))%>%
    select(lake_id,values)
  
  #pull maxdepth from nla17 for blue lake
  lake17<-nm%>%
    filter(source=="nla17 index")%>%
    filter(lake_id==17)%>%
    filter(variables == "maximum measured depth")%>%
    select(lake_id,values)
  
  #pull maxdepth from nla22 for lake 10
  lake10<-nm%>%
    filter(source=="nla22 index")%>%
    filter(lake_id==10)%>%
    filter(variables == "maximum measured depth")%>%
    select(lake_id,values)
  
  #do an antijoin to get nhd max depth values for lakes not in globathy
  nhd_join<-anti_join(nhd_maxdepth,glomd,by="lake_id")%>%
    #filter out negative value
    filter(lake_id != 16)
  lagos_join<-anti_join(lagos_maxdepth,glomd,by="lake_id")
  lagos_join2<-anti_join(lagos_join,nhd_join,by="lake_id")
  
  #now join rows together, only missing 1 and 1 with an off value
  depths<-rbind(glomd,nhd_join,lagos_join2,lake10,lake17)%>%
    mutate(mean_depth_global=values/3)
  colnames(depths)<-c("lake_id","max_depth_global","mean_depth_global")
  
  #look at how many surface area estimates we have in nhd
  sanhd<-nm%>%
    filter(source=="nhdplus")%>%
    filter(join_id %in% comids$value)%>%
    filter(variables=="lakearea")%>%
    group_by(lake_id)%>%
    summarise(surface_area_global=sum(values))
  
  sanlmd<-nm%>%
    filter(source=="nlmd")%>%
    filter(variables=="surfacearea")%>%
    group_by(lake_id)%>%
    summarise(surface_area_global=sum(values))
  sanlmdu<-anti_join(sanlmd,sanhd,by="lake_id")
  
  #Puerto Rico reservoir is the only one without an nhd surface area... let's just use the surge surface area for that one
  lake1000<-nm%>%
    filter(lake_id==1000)%>%
    filter(source=="surge_morpho")%>%
    filter(variables=="surfacearea")%>%
    select(lake_id,values)
  colnames(lake1000)<-c("lake_id","surface_area_global")
  
  surface_area<-rbind(sanhd,lake1000,sanlmdu)
  
  national_morpho_data=left_join(depths,surface_area)
  
  return(national_morpho_data) 
}