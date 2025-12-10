# CODE BELOW READS LAGOS-US LIMNO FROM EDI,
# AND FILTERS TO MATCHES WITH SURGE LAKES
# CODE BELOW IS MOSTLY COMMENTED OUT TO AVOID
# LOADING LARGE LAGOS DATA SET. I RAN ONCE,
# THEN SAVED THE FILTERED OBJECT AND LOAD BELOW.
# CODE SAVED FOR REPRODUCIBILITY.

get_lagos_chem <- function(lagos_chem_file) {
# # CODE BELOW TAKEN FROM EDI
# # https://doi.org/10.6073/pasta/2c58f5a50ab813919f99cc1f265f271c
# 
# # Package ID: edi.1439.5 Cataloging System:https://pasta.edirepository.org.
# # Data set title: LAGOS-US LIMNO: Data module of surface water chemistry from 1975-2021 for lakes in the       conterminous U.S..
# # Data set creator:  Arnab Shuvo - University of Wisconsin-Madison 
# # Data set creator:  Noah Lottig - University of Wisconsin-Madison 
# # Data set creator:  Katherine Webster - Michigan State University 
# # Data set creator:  Austin Delany - University of Wisconsin-Madison 
# # Data set creator:  Kait Reinl - University of Wisconsin-Madison Division of Extension 
# # Data set creator:  Corinna Gries - University of Wisconsin-Madison 
# # Data set creator:  Nicole Smith - Michigan State University 
# # Data set creator:  Autumn Poisson - Michigan State University 
# # Data set creator:  Ian McCullough - Michigan State University 
# # Data set creator:  Sarah Collins - University of Wyoming 
# # Data set creator:  Katelyn King - University of Michigan 
# # Data set creator:  Ellie Phillips - Michigan State University 
# # Data set creator:  Kendra Cheruvelil - Michigan State University 
# # Data set creator:  Patricia Soranno - Michigan State University 
# # Contact:  Patricia Soranno -  Michigan State University  - soranno@msu.edu
# # Stylesheet v1.12 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 
# #
# #install package tidyverse if not already installed
# if(!require(tidyverse)){ install.packages("tidyverse") }  
# library("tidyverse")
# 
# #set UserAgent
# options(HTTPUserAgent="EDI_CodeGen") 
# infile1 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/1439/5/f4b1df4f4544ba0d015690c923bf9868") 
# infile1 <-sub("^https","http",infile1)
# # This creates a tibble named: dt1 
# dt1 <-read_delim(infile1  
#                  ,delim=","   
#                  ,skip=1 
#                  ,quote='"'  
#                  , col_names=c( 
#                    "eventid_epi",   
#                    "lagoslakeid",   
#                    "sample_date",   
#                    "sample_id",   
#                    "parameter_id",   
#                    "parameter_name",   
#                    "parameter_value",   
#                    "parameter_detectionlimit_value",   
#                    "censorcode",   
#                    "sample_depth_m",   
#                    "sample_depth_flag",   
#                    "cluster_id",   
#                    "cluster_lat_dd",   
#                    "cluster_lon_dd",   
#                    "source_id",   
#                    "source_sample_siteid"   ), 
#                  col_types=list(
#                    col_number() , 
#                    col_number() ,  
#                    col_character(),  
#                    col_character(), 
#                    col_number() ,  
#                    col_character(), 
#                    col_number() , 
#                    col_number() ,  
#                    col_character(), 
#                    col_number() ,  
#                    col_character(),  
#                    col_character(), 
#                    col_number() , 
#                    col_number() ,  
#                    col_character(),  
#                    col_character()), 
#                  na=c(" ",".","NA","")  )
# 
# 
# # Convert Missing Values to NA for individual vectors 
# dt1$eventid_epi <- ifelse((trimws(as.character(dt1$eventid_epi))==trimws("N")),NA,dt1$eventid_epi)               
# suppressWarnings(dt1$eventid_epi <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt1$eventid_epi))==as.character(as.numeric("N"))),NA,dt1$eventid_epi))
# dt1$lagoslakeid <- ifelse((trimws(as.character(dt1$lagoslakeid))==trimws("N")),NA,dt1$lagoslakeid)               
# suppressWarnings(dt1$lagoslakeid <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt1$lagoslakeid))==as.character(as.numeric("N"))),NA,dt1$lagoslakeid))
# dt1$sample_date <- ifelse((trimws(as.character(dt1$sample_date))==trimws("N")),NA,dt1$sample_date)               
# suppressWarnings(dt1$sample_date <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt1$sample_date))==as.character(as.numeric("N"))),NA,dt1$sample_date))
# dt1$sample_id <- ifelse((trimws(as.character(dt1$sample_id))==trimws("N")),NA,dt1$sample_id)               
# suppressWarnings(dt1$sample_id <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt1$sample_id))==as.character(as.numeric("N"))),NA,dt1$sample_id))
# dt1$parameter_id <- ifelse((trimws(as.character(dt1$parameter_id))==trimws("N")),NA,dt1$parameter_id)               
# suppressWarnings(dt1$parameter_id <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt1$parameter_id))==as.character(as.numeric("N"))),NA,dt1$parameter_id))
# dt1$parameter_name <- ifelse((trimws(as.character(dt1$parameter_name))==trimws("N")),NA,dt1$parameter_name)               
# suppressWarnings(dt1$parameter_name <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt1$parameter_name))==as.character(as.numeric("N"))),NA,dt1$parameter_name))
# dt1$parameter_value <- ifelse((trimws(as.character(dt1$parameter_value))==trimws("NA")),NA,dt1$parameter_value)               
# suppressWarnings(dt1$parameter_value <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$parameter_value))==as.character(as.numeric("NA"))),NA,dt1$parameter_value))
# dt1$parameter_detectionlimit_value <- ifelse((trimws(as.character(dt1$parameter_detectionlimit_value))==trimws("NA")),NA,dt1$parameter_detectionlimit_value)               
# suppressWarnings(dt1$parameter_detectionlimit_value <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$parameter_detectionlimit_value))==as.character(as.numeric("NA"))),NA,dt1$parameter_detectionlimit_value))
# dt1$censorcode <- ifelse((trimws(as.character(dt1$censorcode))==trimws("N")),NA,dt1$censorcode)               
# suppressWarnings(dt1$censorcode <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt1$censorcode))==as.character(as.numeric("N"))),NA,dt1$censorcode))
# dt1$sample_depth_m <- ifelse((trimws(as.character(dt1$sample_depth_m))==trimws("N")),NA,dt1$sample_depth_m)               
# suppressWarnings(dt1$sample_depth_m <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt1$sample_depth_m))==as.character(as.numeric("N"))),NA,dt1$sample_depth_m))
# dt1$sample_depth_flag <- ifelse((trimws(as.character(dt1$sample_depth_flag))==trimws("N")),NA,dt1$sample_depth_flag)               
# suppressWarnings(dt1$sample_depth_flag <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt1$sample_depth_flag))==as.character(as.numeric("N"))),NA,dt1$sample_depth_flag))
# dt1$cluster_id <- ifelse((trimws(as.character(dt1$cluster_id))==trimws("Y")),NA,dt1$cluster_id)               
# suppressWarnings(dt1$cluster_id <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt1$cluster_id))==as.character(as.numeric("Y"))),NA,dt1$cluster_id))
# dt1$cluster_lat_dd <- ifelse((trimws(as.character(dt1$cluster_lat_dd))==trimws("NA")),NA,dt1$cluster_lat_dd)               
# suppressWarnings(dt1$cluster_lat_dd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$cluster_lat_dd))==as.character(as.numeric("NA"))),NA,dt1$cluster_lat_dd))
# dt1$cluster_lon_dd <- ifelse((trimws(as.character(dt1$cluster_lon_dd))==trimws("NA")),NA,dt1$cluster_lon_dd)               
# suppressWarnings(dt1$cluster_lon_dd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$cluster_lon_dd))==as.character(as.numeric("NA"))),NA,dt1$cluster_lon_dd))
# dt1$source_id <- ifelse((trimws(as.character(dt1$source_id))==trimws("N")),NA,dt1$source_id)               
# suppressWarnings(dt1$source_id <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt1$source_id))==as.character(as.numeric("N"))),NA,dt1$source_id))
# dt1$source_sample_siteid <- ifelse((trimws(as.character(dt1$source_sample_siteid))==trimws("N")),NA,dt1$source_sample_siteid)               
# suppressWarnings(dt1$source_sample_siteid <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt1$source_sample_siteid))==as.character(as.numeric("N"))),NA,dt1$source_sample_siteid))
# 
# 
# # Observed issues when reading the data. An empty list is good!
# problems(dt1) 
# # Here is the structure of the input data tibble: 
# glimpse(dt1) 
# # And some statistical summaries of the data 
# summary(dt1) 
# # Get more details on character variables
# 
# summary(as.factor(dt1$sample_date)) 
# summary(as.factor(dt1$sample_id)) 
# summary(as.factor(dt1$parameter_name)) 
# summary(as.factor(dt1$censorcode)) 
# summary(as.factor(dt1$sample_depth_flag)) 
# summary(as.factor(dt1$cluster_id)) 
# summary(as.factor(dt1$source_id)) 
# summary(as.factor(dt1$source_sample_siteid)) 
# infile2 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/1439/5/fdc599cf0501e13423e19860a503fff3") 
# infile2 <-sub("^https","http",infile2)
# # This creates a tibble named: dt2 
# dt2 <-read_delim(infile2  
#                  ,delim=","   
#                  ,skip=1 
#                  ,quote='"'  
#                  , col_names=c( 
#                    "lagoslakeid",   
#                    "sample_id",   
#                    "parameter_id",   
#                    "source_id",   
#                    "source_name",   
#                    "source_activityorg_name",   
#                    "source_sample_siteid",   
#                    "source_activityid",   
#                    "source_comments",   
#                    "source_parameter_name",   
#                    "source_parameter_value",   
#                    "source_parameter_unit",   
#                    "source_value_qualifiercode",   
#                    "parameter_conversionfactor",   
#                    "source_detectionlimit_value",   
#                    "source_detectionlimit_unit",   
#                    "parameter_detectionlimit_conversionfactor",   
#                    "source_detectionlimit_condition",   
#                    "source_detectionlimit_type",   
#                    "source_labmethod_usgspcode",   
#                    "source_labmethod_description",   
#                    "source_labmethod_id",   
#                    "source_labmethod_name",   
#                    "source_labmethod_qualifier",   
#                    "source_samplesite_lat_dd",   
#                    "source_samplesite_lon_dd"   ), 
#                  col_types=list(
#                    col_number() ,  
#                    col_character(), 
#                    col_number() ,  
#                    col_character(),  
#                    col_character(),  
#                    col_character(),  
#                    col_character(),  
#                    col_character(),  
#                    col_character(),  
#                    col_character(),  
#                    col_character(),  
#                    col_character(),  
#                    col_character(), 
#                    col_number() , 
#                    col_number() ,  
#                    col_character(), 
#                    col_number() ,  
#                    col_character(),  
#                    col_character(), 
#                    col_number() ,  
#                    col_character(),  
#                    col_character(),  
#                    col_character(),  
#                    col_character(), 
#                    col_number() , 
#                    col_number() ), 
#                  na=c(" ",".","NA","")  )
# 
# 
# # Convert Missing Values to NA for individual vectors 
# dt2$lagoslakeid <- ifelse((trimws(as.character(dt2$lagoslakeid))==trimws("N")),NA,dt2$lagoslakeid)               
# suppressWarnings(dt2$lagoslakeid <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt2$lagoslakeid))==as.character(as.numeric("N"))),NA,dt2$lagoslakeid))
# dt2$sample_id <- ifelse((trimws(as.character(dt2$sample_id))==trimws("N")),NA,dt2$sample_id)               
# suppressWarnings(dt2$sample_id <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt2$sample_id))==as.character(as.numeric("N"))),NA,dt2$sample_id))
# dt2$parameter_id <- ifelse((trimws(as.character(dt2$parameter_id))==trimws("N")),NA,dt2$parameter_id)               
# suppressWarnings(dt2$parameter_id <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt2$parameter_id))==as.character(as.numeric("N"))),NA,dt2$parameter_id))
# dt2$source_id <- ifelse((trimws(as.character(dt2$source_id))==trimws("N")),NA,dt2$source_id)               
# suppressWarnings(dt2$source_id <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt2$source_id))==as.character(as.numeric("N"))),NA,dt2$source_id))
# dt2$source_name <- ifelse((trimws(as.character(dt2$source_name))==trimws("N")),NA,dt2$source_name)               
# suppressWarnings(dt2$source_name <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt2$source_name))==as.character(as.numeric("N"))),NA,dt2$source_name))
# dt2$source_activityorg_name <- ifelse((trimws(as.character(dt2$source_activityorg_name))==trimws("Y")),NA,dt2$source_activityorg_name)               
# suppressWarnings(dt2$source_activityorg_name <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt2$source_activityorg_name))==as.character(as.numeric("Y"))),NA,dt2$source_activityorg_name))
# dt2$source_sample_siteid <- ifelse((trimws(as.character(dt2$source_sample_siteid))==trimws("N")),NA,dt2$source_sample_siteid)               
# suppressWarnings(dt2$source_sample_siteid <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt2$source_sample_siteid))==as.character(as.numeric("N"))),NA,dt2$source_sample_siteid))
# dt2$source_activityid <- ifelse((trimws(as.character(dt2$source_activityid))==trimws("Y")),NA,dt2$source_activityid)               
# suppressWarnings(dt2$source_activityid <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt2$source_activityid))==as.character(as.numeric("Y"))),NA,dt2$source_activityid))
# dt2$source_comments <- ifelse((trimws(as.character(dt2$source_comments))==trimws("N")),NA,dt2$source_comments)               
# suppressWarnings(dt2$source_comments <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt2$source_comments))==as.character(as.numeric("N"))),NA,dt2$source_comments))
# dt2$source_parameter_name <- ifelse((trimws(as.character(dt2$source_parameter_name))==trimws("N")),NA,dt2$source_parameter_name)               
# suppressWarnings(dt2$source_parameter_name <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt2$source_parameter_name))==as.character(as.numeric("N"))),NA,dt2$source_parameter_name))
# dt2$source_parameter_value <- ifelse((trimws(as.character(dt2$source_parameter_value))==trimws("Y")),NA,dt2$source_parameter_value)               
# suppressWarnings(dt2$source_parameter_value <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt2$source_parameter_value))==as.character(as.numeric("Y"))),NA,dt2$source_parameter_value))
# dt2$source_parameter_unit <- ifelse((trimws(as.character(dt2$source_parameter_unit))==trimws("Y")),NA,dt2$source_parameter_unit)               
# suppressWarnings(dt2$source_parameter_unit <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt2$source_parameter_unit))==as.character(as.numeric("Y"))),NA,dt2$source_parameter_unit))
# dt2$source_value_qualifiercode <- ifelse((trimws(as.character(dt2$source_value_qualifiercode))==trimws("Y")),NA,dt2$source_value_qualifiercode)               
# suppressWarnings(dt2$source_value_qualifiercode <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt2$source_value_qualifiercode))==as.character(as.numeric("Y"))),NA,dt2$source_value_qualifiercode))
# dt2$parameter_conversionfactor <- ifelse((trimws(as.character(dt2$parameter_conversionfactor))==trimws("NA")),NA,dt2$parameter_conversionfactor)               
# suppressWarnings(dt2$parameter_conversionfactor <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$parameter_conversionfactor))==as.character(as.numeric("NA"))),NA,dt2$parameter_conversionfactor))
# dt2$source_detectionlimit_value <- ifelse((trimws(as.character(dt2$source_detectionlimit_value))==trimws("NA")),NA,dt2$source_detectionlimit_value)               
# suppressWarnings(dt2$source_detectionlimit_value <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$source_detectionlimit_value))==as.character(as.numeric("NA"))),NA,dt2$source_detectionlimit_value))
# dt2$source_detectionlimit_unit <- ifelse((trimws(as.character(dt2$source_detectionlimit_unit))==trimws("Y")),NA,dt2$source_detectionlimit_unit)               
# suppressWarnings(dt2$source_detectionlimit_unit <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt2$source_detectionlimit_unit))==as.character(as.numeric("Y"))),NA,dt2$source_detectionlimit_unit))
# dt2$parameter_detectionlimit_conversionfactor <- ifelse((trimws(as.character(dt2$parameter_detectionlimit_conversionfactor))==trimws("NA")),NA,dt2$parameter_detectionlimit_conversionfactor)               
# suppressWarnings(dt2$parameter_detectionlimit_conversionfactor <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$parameter_detectionlimit_conversionfactor))==as.character(as.numeric("NA"))),NA,dt2$parameter_detectionlimit_conversionfactor))
# dt2$source_detectionlimit_condition <- ifelse((trimws(as.character(dt2$source_detectionlimit_condition))==trimws("Y")),NA,dt2$source_detectionlimit_condition)               
# suppressWarnings(dt2$source_detectionlimit_condition <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt2$source_detectionlimit_condition))==as.character(as.numeric("Y"))),NA,dt2$source_detectionlimit_condition))
# dt2$source_detectionlimit_type <- ifelse((trimws(as.character(dt2$source_detectionlimit_type))==trimws("Y")),NA,dt2$source_detectionlimit_type)               
# suppressWarnings(dt2$source_detectionlimit_type <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt2$source_detectionlimit_type))==as.character(as.numeric("Y"))),NA,dt2$source_detectionlimit_type))
# dt2$source_labmethod_usgspcode <- ifelse((trimws(as.character(dt2$source_labmethod_usgspcode))==trimws("NA")),NA,dt2$source_labmethod_usgspcode)               
# suppressWarnings(dt2$source_labmethod_usgspcode <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$source_labmethod_usgspcode))==as.character(as.numeric("NA"))),NA,dt2$source_labmethod_usgspcode))
# dt2$source_labmethod_description <- ifelse((trimws(as.character(dt2$source_labmethod_description))==trimws("Y")),NA,dt2$source_labmethod_description)               
# suppressWarnings(dt2$source_labmethod_description <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt2$source_labmethod_description))==as.character(as.numeric("Y"))),NA,dt2$source_labmethod_description))
# dt2$source_labmethod_id <- ifelse((trimws(as.character(dt2$source_labmethod_id))==trimws("Y")),NA,dt2$source_labmethod_id)               
# suppressWarnings(dt2$source_labmethod_id <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt2$source_labmethod_id))==as.character(as.numeric("Y"))),NA,dt2$source_labmethod_id))
# dt2$source_labmethod_name <- ifelse((trimws(as.character(dt2$source_labmethod_name))==trimws("Y")),NA,dt2$source_labmethod_name)               
# suppressWarnings(dt2$source_labmethod_name <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt2$source_labmethod_name))==as.character(as.numeric("Y"))),NA,dt2$source_labmethod_name))
# dt2$source_labmethod_qualifier <- ifelse((trimws(as.character(dt2$source_labmethod_qualifier))==trimws("NA")),NA,dt2$source_labmethod_qualifier)               
# suppressWarnings(dt2$source_labmethod_qualifier <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$source_labmethod_qualifier))==as.character(as.numeric("NA"))),NA,dt2$source_labmethod_qualifier))
# dt2$source_samplesite_lat_dd <- ifelse((trimws(as.character(dt2$source_samplesite_lat_dd))==trimws("NA")),NA,dt2$source_samplesite_lat_dd)               
# suppressWarnings(dt2$source_samplesite_lat_dd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$source_samplesite_lat_dd))==as.character(as.numeric("NA"))),NA,dt2$source_samplesite_lat_dd))
# dt2$source_samplesite_lon_dd <- ifelse((trimws(as.character(dt2$source_samplesite_lon_dd))==trimws("NA")),NA,dt2$source_samplesite_lon_dd)               
# suppressWarnings(dt2$source_samplesite_lon_dd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$source_samplesite_lon_dd))==as.character(as.numeric("NA"))),NA,dt2$source_samplesite_lon_dd))
# 
# 
# # Observed issues when reading the data. An empty list is good!
# problems(dt2) 
# # Here is the structure of the input data tibble: 
# glimpse(dt2) 
# # And some statistical summaries of the data 
# summary(dt2) 
# # Get more details on character variables
# 
# summary(as.factor(dt2$sample_id)) 
# summary(as.factor(dt2$source_id)) 
# summary(as.factor(dt2$source_name)) 
# summary(as.factor(dt2$source_activityorg_name)) 
# summary(as.factor(dt2$source_sample_siteid)) 
# summary(as.factor(dt2$source_activityid)) 
# summary(as.factor(dt2$source_comments)) 
# summary(as.factor(dt2$source_parameter_name)) 
# summary(as.factor(dt2$source_parameter_value)) 
# summary(as.factor(dt2$source_parameter_unit)) 
# summary(as.factor(dt2$source_value_qualifiercode)) 
# summary(as.factor(dt2$source_detectionlimit_unit)) 
# summary(as.factor(dt2$source_detectionlimit_condition)) 
# summary(as.factor(dt2$source_detectionlimit_type)) 
# summary(as.factor(dt2$source_labmethod_description)) 
# summary(as.factor(dt2$source_labmethod_id)) 
# summary(as.factor(dt2$source_labmethod_name)) 
# summary(as.factor(dt2$source_labmethod_qualifier)) 
# infile3 <- trimws("https://pasta.lternet.edu/package/data/eml/edi/1439/5/824b259acddce3ebf5cb63aa5484127e") 
# infile3 <-sub("^https","http",infile3)
# # This creates a tibble named: dt3 
# dt3 <-read_delim(infile3  
#                  ,delim=","   
#                  ,skip=1 
#                  ,quote='"'  
#                  , col_names=c( 
#                    "lagoslakeid",   
#                    "cluster_id",   
#                    "cluster_lat_dd",   
#                    "cluster_lon_dd",   
#                    "source_sample_siteid",   
#                    "source_samplesite_lat_dd",   
#                    "source_samplesite_lon_dd",   
#                    "lake_waterarea_ha",   
#                    "lake_states",   
#                    "lake_namelagos",   
#                    "cluster_meandistance_m",   
#                    "cluster_maxdistance_m",   
#                    "cluster_sitedistance_m"   ), 
#                  col_types=list(
#                    col_number() ,  
#                    col_character(), 
#                    col_number() , 
#                    col_number() ,  
#                    col_character(), 
#                    col_number() , 
#                    col_number() , 
#                    col_number() ,  
#                    col_character(),  
#                    col_character(), 
#                    col_number() , 
#                    col_number() , 
#                    col_number() ), 
#                  na=c(" ",".","NA","")  )
# 
# 
# # Convert Missing Values to NA for individual vectors 
# dt3$lagoslakeid <- ifelse((trimws(as.character(dt3$lagoslakeid))==trimws("N")),NA,dt3$lagoslakeid)               
# suppressWarnings(dt3$lagoslakeid <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt3$lagoslakeid))==as.character(as.numeric("N"))),NA,dt3$lagoslakeid))
# dt3$cluster_id <- ifelse((trimws(as.character(dt3$cluster_id))==trimws("N")),NA,dt3$cluster_id)               
# suppressWarnings(dt3$cluster_id <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt3$cluster_id))==as.character(as.numeric("N"))),NA,dt3$cluster_id))
# dt3$cluster_lat_dd <- ifelse((trimws(as.character(dt3$cluster_lat_dd))==trimws("NA")),NA,dt3$cluster_lat_dd)               
# suppressWarnings(dt3$cluster_lat_dd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$cluster_lat_dd))==as.character(as.numeric("NA"))),NA,dt3$cluster_lat_dd))
# dt3$cluster_lon_dd <- ifelse((trimws(as.character(dt3$cluster_lon_dd))==trimws("NA")),NA,dt3$cluster_lon_dd)               
# suppressWarnings(dt3$cluster_lon_dd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$cluster_lon_dd))==as.character(as.numeric("NA"))),NA,dt3$cluster_lon_dd))
# dt3$source_sample_siteid <- ifelse((trimws(as.character(dt3$source_sample_siteid))==trimws("N")),NA,dt3$source_sample_siteid)               
# suppressWarnings(dt3$source_sample_siteid <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt3$source_sample_siteid))==as.character(as.numeric("N"))),NA,dt3$source_sample_siteid))
# dt3$source_samplesite_lat_dd <- ifelse((trimws(as.character(dt3$source_samplesite_lat_dd))==trimws("NA")),NA,dt3$source_samplesite_lat_dd)               
# suppressWarnings(dt3$source_samplesite_lat_dd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$source_samplesite_lat_dd))==as.character(as.numeric("NA"))),NA,dt3$source_samplesite_lat_dd))
# dt3$source_samplesite_lon_dd <- ifelse((trimws(as.character(dt3$source_samplesite_lon_dd))==trimws("NA")),NA,dt3$source_samplesite_lon_dd)               
# suppressWarnings(dt3$source_samplesite_lon_dd <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$source_samplesite_lon_dd))==as.character(as.numeric("NA"))),NA,dt3$source_samplesite_lon_dd))
# dt3$lake_waterarea_ha <- ifelse((trimws(as.character(dt3$lake_waterarea_ha))==trimws("N")),NA,dt3$lake_waterarea_ha)               
# suppressWarnings(dt3$lake_waterarea_ha <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt3$lake_waterarea_ha))==as.character(as.numeric("N"))),NA,dt3$lake_waterarea_ha))
# dt3$lake_states <- ifelse((trimws(as.character(dt3$lake_states))==trimws("N")),NA,dt3$lake_states)               
# suppressWarnings(dt3$lake_states <- ifelse(!is.na(as.numeric("N")) & (trimws(as.character(dt3$lake_states))==as.character(as.numeric("N"))),NA,dt3$lake_states))
# dt3$lake_namelagos <- ifelse((trimws(as.character(dt3$lake_namelagos))==trimws("Y")),NA,dt3$lake_namelagos)               
# suppressWarnings(dt3$lake_namelagos <- ifelse(!is.na(as.numeric("Y")) & (trimws(as.character(dt3$lake_namelagos))==as.character(as.numeric("Y"))),NA,dt3$lake_namelagos))
# dt3$cluster_meandistance_m <- ifelse((trimws(as.character(dt3$cluster_meandistance_m))==trimws("NA")),NA,dt3$cluster_meandistance_m)               
# suppressWarnings(dt3$cluster_meandistance_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$cluster_meandistance_m))==as.character(as.numeric("NA"))),NA,dt3$cluster_meandistance_m))
# dt3$cluster_maxdistance_m <- ifelse((trimws(as.character(dt3$cluster_maxdistance_m))==trimws("NA")),NA,dt3$cluster_maxdistance_m)               
# suppressWarnings(dt3$cluster_maxdistance_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$cluster_maxdistance_m))==as.character(as.numeric("NA"))),NA,dt3$cluster_maxdistance_m))
# dt3$cluster_sitedistance_m <- ifelse((trimws(as.character(dt3$cluster_sitedistance_m))==trimws("NA")),NA,dt3$cluster_sitedistance_m)               
# suppressWarnings(dt3$cluster_sitedistance_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$cluster_sitedistance_m))==as.character(as.numeric("NA"))),NA,dt3$cluster_sitedistance_m))
# 
# 
# # Observed issues when reading the data. An empty list is good!
# problems(dt3) 
# # Here is the structure of the input data tibble: 
# glimpse(dt3) 
# # And some statistical summaries of the data 
# summary(dt3) 
# # Get more details on character variables
# 
# summary(as.factor(dt3$cluster_id)) 
# summary(as.factor(dt3$source_sample_siteid)) 
# summary(as.factor(dt3$lake_states)) 
# summary(as.factor(dt3$lake_namelagos))


# # Subset and format chemistry data file
# lagos_chem <- dt1 %>%
#   mutate(
#     sample_date = as.Date(sample_date, format = "%Y-%m-%d"),
#   ) %>%
#   filter(
#     # only SuRGE lakes
#     lagoslakeid %in% dat_raw$lagoslakeid,
#     # only surface samples
#     sample_depth_m <= 0.5,
#     # recent samples
#     sample_date >= as.Date("2006-01-01"),
#     # parameters of interest
#     parameter_name %in% c(
#       "chla_ugl", "so4_mgl", "no2no3n_ugl", "tp_ugl",
#       "tn_ugl", "nh4n_ugl", "doc_mgl", "toc_mgl", "ph",
#       "spcond_uscm"
#     )
#   ) %>%
#   select(
#     lagoslakeid, sample_date, parameter_name, parameter_value
#   ) %>%
#   mutate(
#     # force consistency with dat_raw units conventions
#     parameter_units = case_when(
#       parameter_name == "chla_ugl" ~ "ug_l",
#       parameter_name == "so4_mgl" ~ "mg_l",
#       parameter_name == "tp_ugl" ~ "ug_p_l",
#       parameter_name %in% c("tn_ugl", "no2no3n_ugl", "nh4n_ugl") ~ "ug_n_l",
#       parameter_name %in% c("doc_mgl", "toc_mgl") ~ "mg_c_l",
#       parameter_name == "ph" ~ "ph",
#       parameter_name == "spcond_uscm" ~ "us_cm",
#       TRUE ~ "FLY YOU FOOLS"
#     ),
#     # omit units from parameter name (e.g. tn_ugl -> tn)
#     parameter_name = str_extract(parameter_name, "^[^_]+")
#   ) %>%
#   mutate(
#     # rename some parameters to be consistent with dat_raw conventions
#     parameter_name = case_when(
#       parameter_name == "nh4n" ~ "nh4",
#       parameter_name == "no2no3n" ~ "no2_3",
#       parameter_name == "spcond" ~ "sp_cond",
#       TRUE ~ parameter_name
#     )
#   ) %>%
#   pivot_wider(
#     names_from = parameter_name,
#     values_from = c(parameter_value, parameter_units),
#     names_glue = "lagos_{parameter_name}_{.value}_shallow"
#   ) %>%
#   # dat_raw convention is to have "units" at the end of column names
#   # (e.g., chla_shallow_units). So add "units" to end and remove from middle
#   rename_with(~ paste0(., "_units"), contains("units")) %>% # add to end
#   rename_with(~ str_remove(., "parameter_units_")) %>% # remove from middle
#   # remove "parameter_value" from column names
#   rename_with(~ str_remove(., "parameter_value_"))
# 
# 
# saveRDS(lagos_chem, "input_data/lagos_chem.rds")


lagos_chem <- readRDS(lagos_chem_file) # "input_data/lagos_chem.rds" 

return(lagos_chem)

}