rm(list=ls())
library(dplyr)
library(readxl)

#################################################################
# The file used in this data set is produced from the QGIS file #
# baseline_annual.                                              #
#################################################################


data <- read.csv("inputs/final_dataset_r_witharea.csv")

#data_old%>%
#  dplyr::summarize(totalarea = sum(area_km2))

data%>%
  dplyr::summarize(totalarea = sum(area)*0.000001)

data <- filter(data, gid_0 %in% c("COL", "PAN", "VEN", "BRB", "TTO", "BRA", "GUY", "SUR", "GUF", "BOL", "PER", "ECU", "URY", 
                                  "ARG", "PRY", "CHL", "MEX", "GTM", "BLZ", "HND", "NIC", "CRI", "BHS", "JAM", "DOM", "HTI", "SLV") )


variables <-read.csv("inputs/water_risk_baseline_variables.csv") 

#   Rename variables to match WRI (shapefiles truncate variable names)
colnames(data) <- variables$Field.in.Source

colnames<-colnames(data) 
#names <- c("NAME_0","NAME_1","NAME_2","NAME_3","NAME_4")
labels<-colnames[grepl("_label",colnames)]
#score<-colnames[grepl("_score",colnames)]

data<-select(data, 1:9,name_2, all_of(labels),pop_sum, land_area_km2)

data<- rename(data, "iso3" = "gid_0")
data$pop_sum<-round(data$pop_sum, 0)
data<-data%>%
  mutate(land_area_km2 = land_area_km2*0.000001)


for(i in 11:65){
  data[i][data[i]==""]<-"No data"
  data[i][data[i]=="NoData"]<-"No data"
}


# test<-data[data$land_area_km2>1,]
# test%>%
#   dplyr::summarize(totalarea = sum(land_area_km2))

    poptotals<-data%>%
      group_by(iso3)%>%
      dplyr::summarize(total_pop = sum(pop_sum))
    bws<-data%>%
      group_by(iso3, bws_label)%>%
      dplyr::summarize(pop = sum(pop_sum))%>%
      left_join(poptotals, by=c("iso3")) %>%
      mutate(pop_pct = (pop/total_pop)*100)
    
    pop_regiontotals<-data%>%
      dplyr::summarize(total_pop = sum(pop_sum))
    bws_region <- data%>%
      group_by(bws_label)%>%
      dplyr::summarize(pop = sum(pop_sum))%>%
      merge(pop_regiontotals) %>%
      mutate(pop_pct = (pop/total_pop)*100)
    area_total<-data%>%
      dplyr::summarize(area_total = sum(land_area_km2))
    area_region <- data%>%
      group_by(bws_label)%>%
      dplyr::summarize(area = sum(land_area_km2))%>%
      merge(area_total) %>%
      mutate(area_pct = (area/area_total)*100)
    
    
    pop_regiontotals<-data%>%
      dplyr::summarize(total_pop = sum(pop_sum))
    totrisk_region <- data%>%
      group_by(w_awr_def_tot_label)%>%
      dplyr::summarize(pop = sum(pop_sum))%>%
      merge(pop_regiontotals) %>%
      mutate(pop_pct = (pop/total_pop)*100)

    # write.csv(bws, "bws_pop_pct.csv")
    # 
    # 
    # 
    # bws_pop_pct<-left_join(bws, poptotals, by=c("iso3"))
                   
    # name_0<-as.data.frame(unique(data$name_0))
    # bws_label<-as.data.frame(unique(data$bws_label))
    # w_awr_def_qan_label<-as.data.frame(unique(data$w_awr_def_qan_label))
    # w_awr_def_qal_label<-as.data.frame(unique(data$w_awr_def_qal_label))
    # w_awr_def_rrr_label<-as.data.frame(unique(data$w_awr_def_rrr_label))
    # w_awr_def_tot_label <- as.data.frame(unique(data$w_awr_def_tot_label))
    # 
    # list<-list("name_0"=name_0, 
    #            "bws_label"=bws_label,
    #            "w_awr_def_qan_label"=w_awr_def_qan_label,
    #            "w_awr_def_qal_label"=w_awr_def_qal_label,
    #            "w_awr_def_rrr_label"=w_awr_def_rrr_label,
    #            "w_awr_def_tot_label"=w_awr_def_tot_label)
    # library(writexl)
    # write_xlsx(list, "translations.xlsx")

fields<- list("name_0", "bws_label","w_awr_def_qan_label","w_awr_def_qal_label","w_awr_def_rrr_label","w_awr_def_tot_label")
for(f in 1:length(fields)){
  trans<-read_xlsx("inputs/translations.xlsx", sheet = as.character(fields[f]))
  if(f==1){
    df<-left_join(data, trans)
    }
  if(f>1){
    df<-left_join(df, trans)
    }  
}

df<-rename(df,"area_km2"  = "land_area_km2")


#Remove scientific notation
options(scipen = 100)                                                                                                                                                                                                        

write.csv(df, "final/population_ws_baseline.csv", row.names = F)



