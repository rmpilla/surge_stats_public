## Look at Trophic Status Grouping and Run Collinearity Matrix

# library(reshape2) migrate to dplyr pivot functions
library(corrplot)

chemistry<-read.csv(file="C:/Users/bdeemer/Environmental Protection Agency (EPA)/SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents/communications/manuscript/data_paper/7_site_data.csv")%>%
  filter(name %in% c("chla_sonde","no2_3","no3","tp","tn","op","doc","turbidity","chla_lab"))


lagos_wq<-read.csv(file="C:/Users/bdeemer/Environmental Protection Agency (EPA)/SuRGE Survey of Reservoir Greenhouse gas Emissions - Documents/communications/manuscript/data_paper/8_lake_scale.csv")%>%
  filter(name %in% c("chl_predicted_sample_season_visit1","doc_predicted_sample_season_visit1","doc_predicted_sample_season_visit2",
                     "chl_predicted_sample_season_visit2","chl_predicted_sample_month_visit1","chl_predicted_sample_month_visit2",
                     "doc_predicted_sample_month_visit1","doc_predicted_sample_month_visit2"))

#Make the datafiles wide and join them
chemistryw<- dcast(chemistry, lake_id+site_id+visit+sample_depth ~ name, value.var= "value")%>%
  mutate(no3=ifelse(!is.na(no3),no3,no2_3))%>%
  select(-no2_3)

#need to widen the sample depth columns
shallow<-chemistryw %>%
  filter(sample_depth=="shallow")%>%
  select(-sample_depth)

deep<-chemistryw %>%
  filter(sample_depth=="deep")%>%
  rename_with(~paste0(.,"deep"))%>%
  mutate(lake_id=lake_iddeep, site_id=site_iddeep,visit=visitdeep)%>%
  select(-sample_depthdeep,-lake_iddeep,-site_iddeep,-visitdeep)

#look at merge predictors script to see if this is the same
chemistrywd<-left_join(shallow,deep)%>%  
  group_by(lake_id|visit) %>%
  fill(doc, .direction="downup")%>%
  fill(no3, .direction="downup")%>%
  fill(op, .direction="downup")%>%
  fill(tn, .direction="downup")%>%
  fill(tp, .direction="downup")%>%
  fill(chla_lab, .direction="downup")%>%
  fill(docdeep, .direction="downup")%>%
  fill(no3deep, .direction="downup")%>%
  fill(opdeep, .direction="downup")%>%
  fill(tndeep, .direction="downup")%>%
  fill(tpdeep, .direction="downup")%>%
  select(-chla_labdeep)%>%
  ungroup()

lagos_wqw<- dcast(lagos_wq,lake_id ~ name, value.var="value" )

lagos_visit1<-lagos_wqw %>%
  select(lake_id, chl_predicted_sample_month_visit1, chl_predicted_sample_season_visit1,
         doc_predicted_sample_month_visit1,doc_predicted_sample_season_visit1)%>%
  mutate(visit=1)

lagos_visit2<-lagos_wqw %>%
  select(lake_id, chl_predicted_sample_month_visit2,chl_predicted_sample_season_visit2,
         doc_predicted_sample_month_visit2,doc_predicted_sample_season_visit2)%>%
  mutate(visit=2)%>%
  filter(!is.na(chl_predicted_sample_month_visit2))

colnames(lagos_visit1)<-c("lake_id","chl_predicted_sample_month","chl_predicted_sample_season",
                          "doc_predicted_sample_month","doc_predicted_sample_season","visit")
colnames(lagos_visit2)<-c("lake_id","chl_predicted_sample_month","chl_predicted_sample_season",
                          "doc_predicted_sample_month","doc_predicted_sample_season","visit")
lagos_wqwv<-rbind(lagos_visit1,lagos_visit2)%>%
  mutate(chl_predicted_sample_month=as.numeric(chl_predicted_sample_month),
         chl_predicted_sample_season=as.numeric(chl_predicted_sample_season),
         doc_predicted_sample_month=as.numeric(doc_predicted_sample_month),
         doc_predicted_sample_season=as.numeric(doc_predicted_sample_season))

trophic<-left_join(chemistrywd,lagos_wqwv)

trop<-trophic %>%
  select(-lake_id,-site_id,-visit)%>%
  filter(complete.cases(.))

c<-cor(trop)
corrplot(c,method="number")

#TP shallow
#TN Shallow
#DOC deep (or shallow?)
#could use turbidity in field-based model (but exclude w/ lagos models)


#prefer total over dissolved nutrients? op is correlated with tp both shallow and deep, no3 and tn correlated in surface
#prefer turbidity  over lagos chl? or vice versa?

#how to deal with deep vs. shallow?-- correlated at >0.5 for no3, op, tp, doc, chl_sonde, turbidity
#maybe keep shallow nutrients/chlorophyll and deep doc? need to decide on turbidity

#setup competing models (one with lab and one with lagos)
