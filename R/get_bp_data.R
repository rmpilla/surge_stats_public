## Developing indices for barometric pressure for both diffusive and ebullitive emissions 

get_bp_data <- function(bp_data_file,emission_points_file) {
  
 dt<-read_csv(emission_points_file)%>%
   mutate(ID=paste(lake_id,site_id,visit))%>%
    select(ID, chamb_deply_date_time, trap_deply_date_time)
  
 bp<-read_csv(bp_data_file) %>%
  mutate(ID=paste(lake_id,site_id,visit))
  
 bp_join<-left_join(bp,dt,by="ID")  

 bp_chamber_min<- bp_join %>%
  mutate(chamb_deply_round=round_date(chamb_deply_date_time,"hour")) %>%
  filter(date_time<= chamb_deply_round)%>%
  group_by(ID)%>%
  summarise(min_bp_diff=min(barometric_pressure),
            chg_bp_diff=max(barometric_pressure)-min(barometric_pressure),
            lake_id=lake_id[1], site_id=site_id[1], visit=visit[1], ID=ID[1])

 #It appears we might not have the funnel data specifically in some cases
 bp_funnel_min<- bp_join %>%
  mutate(trap_deply_round=round_date(trap_deply_date_time,"hour")-hours(3)) %>%
  filter(date_time>= trap_deply_round) %>%
  group_by(ID)%>%
  summarise(min_bp_ebu=min(barometric_pressure),
         chg_bp_ebu=max(barometric_pressure)-min(barometric_pressure),
         lake_id=lake_id[1], site_id=site_id[1], visit=visit[1], ID=ID[1])
  
bp_data<-full_join(bp_chamber_min,bp_funnel_min)
  
  #tar_load_everything()
  
  
  return(bp_data)
  
}
