## Calculate signal to noise ratio following Olson and Hawkins 2013

get_signal_to_noise <- function(emission_rate_points_data) {
  
  among_var<- emission_rate_points_data %>%
    #filter(lake_id %in% c(147,148,250,281))%>%
    filter(visit==1)%>%
    #group_by(lake_id)%>%
    summarise(diff_var=var(ch4_diffusion,na.rm=TRUE),ebu_var=var(ch4_ebullition,na.rm=TRUE),
                            tot_var=var(ch4_total,na.rm=TRUE))
  summary(among_var)
  
  
within_var<- emission_rate_points_data %>%
    filter(lake_id %in% c(147,148,250,281))%>%
    mutate(id=paste(lake_id,site_id))%>%
    group_by(id)%>%
    summarise(diff_var=var(ch4_diffusion,na.rm=TRUE),ebu_var=var(ch4_ebullition,na.rm=TRUE),
              tot_var=var(ch4_total,na.rm=TRUE))
  
  summary(within_var)
  
pathway<-c("Total","Diffusive","Ebullitive")
var_sites<-c(mean(among_var$tot_var,na.rm=TRUE),mean(among_var$diff_var,na.rm=TRUE),mean(among_var$ebu_var,na.rm=TRUE))
var_reps<-c(mean(within_var$tot_var,na.rm=TRUE),mean(within_var$diff_var,na.rm=TRUE),mean(within_var$ebu_var,na.rm=TRUE))

s_n<-data.frame(pathway,var_sites,var_reps)
s_n$SN<-var_sites/var_reps
s_n$maxr2<-s_n$SN/(s_n$SN+1)
  
  return(s_n) 
}